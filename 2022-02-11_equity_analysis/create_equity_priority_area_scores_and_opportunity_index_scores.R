install.packages("tidytransit")

library(tidyverse)
library(sf)
library(tidycensus)
library(tidytransit)
library(here)

# NOTE! you will need your own census API key for this to work
# This website has the info you need: https://walker-data.com/tidycensus/articles/basic-usage.html

###############################
## CENSUS DATA PARAMETERS #####
###############################
acs_year <- 2019
crs <- 2926
service_change <- 213

#####################################
## LOAD BLOCK GROUPS ################
#####################################

#download the block group file from the KC GIS Open data portal. 
#It's more accurate than the one available through the Census API
#update the location of the file to be wherever you stored it on your computer

block_groups <- st_read(paste0(here( "data" ,"KC_block_groups", #call to the here() command looks for a shapefile in a directory called "data/KC_block_groups"
                                     "2010_Census_Block_Groups_for_King_County_-_Conflated_to_Parcels_-_Major_Waterbodies_Erased___blkgrp10_shore_area.shp")),
                        stringsAsFactors = F) %>%
  janitor::clean_names() %>%
  select(geo_id_grp) %>%
  rename(geoid= geo_id_grp) %>%
  st_transform(crs)

#this is another way to get block groups from the data.gov portal. 
#The boundaries are a little innacurate though when mapped with KC GIS data

# block_groups <- tigris::block_groups("WA", "King", cb = T) %>%
#   janitor::clean_names() %>%
#   select(-name) %>%
#   st_transform(crs)
############################
## GET ACS DATA ############
############################

# this command creates a table of census variables that is searchable in R. Provided for convenience and 
# for reference. Not used in further calculations. 

variables <-  tidycensus::load_variables(2019, "acs5")

# this is the main data pull from the ACS

acs <- get_acs(geography = "block group",  
                              variables = c(pop = "B01001_001",
                                            housing = "C16002_001",
                                            pov1="C17002_001"     ,
                                            pov2= "C17002_008" ,
                                            foreign_born =  "B99051_005"  ,
                                            people_of_color1 ="B03002_001",
                                            people_of_color2=   "B03002_003",
                                            lep1= "C16002_004"    ,
                                            lep2= "C16002_007"    ,
                                            lep3= "C16002_010"    ,
                                            lep4= "C16002_013"    ,
                                            disability1= "B23024_003"   ,
                                            disability2= "B23024_018" ),
                              state= "WA",
                              county = "King",
                              year= acs_year,
                              geometry = F) %>%
                janitor::clean_names() %>%
  #create grouping variable
                 mutate( mf_variable = case_when( grepl( "lep", variable) ~ "lep",
                                                  grepl( "pov", variable) ~ "pov",
                                                  grepl( "foreign_born", variable) ~ "foreign_born",
                                                  grepl( "people", variable) ~ "poc",
                                                  grepl( "disab", variable) ~ "disability",
                                                  grepl("pop", variable) ~ "population",
                                                  grepl("housing", variable) ~ "housing_units",
                                                  FALSE ~ "other"))  %>%
                 group_by(mf_variable, geoid) %>%
                 mutate(sum_moe = moe_sum(moe, estimate)) 

#spatial join to connect block group polygons to data based on GEOID

acs <- block_groups %>% 
  left_join(acs)

#############################################
##### calculate block group need scores #####
#############################################

block_group_need_scores <- acs %>%
  st_drop_geometry() %>%
  ungroup()%>%
  select(-c(moe, mf_variable )) %>%
  pivot_wider(names_from = variable, values_from = c(estimate, sum_moe)) %>%
  rowwise() %>%
  
  mutate(population_200_pct_below_poverty_line = estimate_pov1 - estimate_pov2,
         people_of_color= estimate_people_of_color1 - estimate_people_of_color2,
         limited_english_households = estimate_lep1 + estimate_lep2 + estimate_lep3 + estimate_lep4 ,
         people_with_disability = estimate_disability1 + estimate_disability2) %>%
  
  rename(foreign_born = estimate_foreign_born,
         people_of_color_moe = sum_moe_people_of_color1,
         foreign_born_moe = sum_moe_foreign_born,
         limited_english_households_moe = sum_moe_lep1,
         population_200_pct_below_poverty_line_moe = sum_moe_pov1,
         people_with_disability_moe = sum_moe_disability1) %>%
  
  select(geoid, name, population_200_pct_below_poverty_line, foreign_born, people_of_color,
         limited_english_households,
         people_with_disability, population_200_pct_below_poverty_line_moe, people_of_color_moe,
         foreign_born_moe, limited_english_households_moe, people_with_disability_moe) %>%
  
  ungroup()%>%
  #ntile creates even breaks across the dataset, splitting all block groups into 5 categories. 1 is low, 5 is high
  mutate(pov_quintile = ntile(population_200_pct_below_poverty_line, 5),
         poc_quintile = ntile(people_of_color, 5),
         foreign_born_quintile = ntile(foreign_born, 5),
         lep_quintile = ntile(limited_english_households, 5),
         disability_quintile = ntile(people_with_disability, 5)) %>%
  select(geoid, name, pov_quintile, poc_quintile, foreign_born_quintile,
         lep_quintile, disability_quintile) %>%
  
  # Apply weighting scheme to reflect KC's priorities of leading with race/income & to control
  # for high margin of error on other variables
  mutate(race_score = poc_quintile*.4,
         pov_score = pov_quintile*.3,
         lep_score = lep_quintile*.10,
         disability_score = disability_quintile *.10,
         foreign_born_score = foreign_born_quintile*.10)%>%
  #create final score--what is referred to as the Equity Priority Area Score
  mutate(final_score = round( (race_score + pov_score + lep_score + 
                                    disability_score + foreign_born_score), 0), 
         acs_year = acs_year)


block_group_need_scores_geo <- block_groups %>%
  left_join(block_group_need_scores, by = "geoid")  


mapview::mapview(block_group_need_scores_geo, zcol = "final_score")

# save data in a folder called "output". Feel free to change
write_csv(block_group_need_scores, here::here( "output", paste0("equity_priority_area_scores_", acs_year, ".csv")))


write_sf(block_group_need_scores_geo, here::here( "output", paste0("equity_priority_area_scores_", acs_year, ".shp")))

####################################
### STOP AND ROUTE EQUITY DATA #####
####################################

#you will need to download a GTFS feed as a zipped folder and store it in a folder. 
#You can update the file path to match where you store your GTFS feed. 
#you can get the GTFS from transitfeeds.com, but note that there are biweekly GTFS feeds on the website. Check with
# metro/other transit partners for correct GTFS to use. 

# This code figures out stop to route associations
service_id_count <- gtfs$trips %>% 
  left_join(gtfs$calendar) %>% 
  mutate(day_type = case_when(saturday == 1 ~ 1, 
                              sunday == 1 ~ 2, 
                              TRUE ~ 0)) %>% 
  group_by(service_id, day_type) %>% 
 summarise(count_trip = n())
  
associate_stops_by_route <- function(service_change){
#service_change <- "211"
  gtfs <- read_gtfs(here::here( "data", paste0(service_change , "_gtfs.zip"))) #looking for a zipped folder with the GTFS in the "data" folder
#join stops to trips to routes to get the stop/route associations. 
#Then, make the data a spatial object
stops_by_route <- gtfs$stops %>% 
  left_join(gtfs$stop_times) %>% 
  select(stop_id, stop_name, trip_id, stop_lat, stop_lon) %>% 
  left_join(gtfs$trips) %>% 
  distinct(stop_id,  stop_name, route_id, stop_lat, stop_lon, service_id) %>% 
  left_join(gtfs$calendar) %>%

  filter(start_date > lubridate::as_date("2021-10-01")) %>%
  select(stop_id,  stop_name, route_id,  stop_lat, stop_lon) %>% 

  left_join(gtfs$routes) %>% 
  select(stop_id,  stop_name, route_id, route_short_name, route_desc, stop_lat, stop_lon) %>% 
  distinct(stop_id, route_short_name, .keep_all = TRUE) %>% 
 #manually move two stops on Vashon Island that are in a water block group. 
   mutate(stop_lat = case_when(stop_id %in% c("46769", 
                                   "46766" ) ~ 47.508329, 
                              TRUE ~ stop_lat), 
         stop_lon =  case_when(stop_id %in% c("46769", 
                                              "46766" ) ~ -122.463845, 
                               TRUE ~ stop_lon)) %>% 

  st_as_sf(., coords= c( "stop_lon", "stop_lat"), crs = 4326) %>%
  # convert to WA State Plane North
  st_transform(crs = 2926 ) %>% 
 #fix route naming issues. May need to be updated over time. 
  mutate(service_change_num = service_change, 
        route_short_name = case_when( route_short_name== "A" ~ "671", 
                                        route_short_name == "B" ~ "672", 
                                        route_short_name ==  "C" ~ "673", 
                                        route_short_name ==  "D" ~ "674", 
                                        route_short_name ==  "E" ~ "675", 
                                        route_short_name == "F" ~ "676", 
                                        route_short_name == "A Line" ~ "671", 
                                        route_short_name == "B Line" ~ "672", 
                                        route_short_name ==  "C Line" ~ "673", 
                                        route_short_name ==  "D Line" ~ "674", 
                                        route_short_name ==  "E Line" ~ "675", 
                                        route_short_name == "F Line" ~ "676", 
                                        route_short_name == "Link light rail" ~ "599", 
                                        route_short_name == "First Hill Streetcar" ~ "98", 
                                        route_short_name == "South Lake Union Streetcar" ~ "97", 
                                        route_short_name== "Duvall-Monroe Shuttle" ~ "628", 
                                        route_short_name == "Trailhead Direct Mt. Si" ~ "634",
                                        route_short_name == "Trailhead Direct Mailbox Peak" ~ "636",
                                        route_short_name == "Trailhead Direct Issaquah Alps" ~ "637",
                                        route_short_name == "Trailhead Direct Cougar Mt." ~ "639", 
                                      TRUE ~ route_short_name
         )) 
}
  

stops_by_route_list <- map(list( 213), associate_stops_by_route)

stops_by_route<-  st_as_sf(bind_rows(stops_by_route_list), fill = TRUE, sf_column_name = "geometry") %>% 
  #In 193, link light rail didn't have a route number. This is a precaution
  mutate(route_short_name = replace_na(route_short_name, 599)) 

mapview::mapview(stops_by_route)



block_group_need_scores_geo <- block_group_need_scores_geo %>% 
  ungroup() %>% 
  st_transform(2926)

names(block_group_need_scores_geo$geometry) = NULL

#determine the spatial location of stops relative to block groups. 
#stops will inherit the equity priority area score of the block group they are located within. 

stops_equity_scores_by_route <- st_join(stops_by_route, block_group_need_scores_geo, join = st_within) %>% 
  janitor::clean_names() %>% 
  drop_na(geoid)




mapview::mapview(block_group_need_scores_geo["geoid"]) +
  mapview::mapview(stops_equity_scores_by_route["stop_id"])

stops_equity_scores_no_geo <- stops_equity_scores_by_route %>% 
  st_drop_geometry()

write_csv(stops_equity_scores_no_geo, here("output", paste0("stop_equity_scores_", service_change, ".csv")))



percent_stops_in_equity_bg <- stops_equity_scores_by_route %>%
  group_by( route_short_name, service_change_num) %>% 
  # includes sound transit and DART services
  summarise(total_stop_count = n(), 
            mean_stop_score = mean(final_score, na.rm = TRUE), 
            equity_stops_count = sum(final_score %in% c(5)), 
            non_equity_stops_count = sum(final_score %in% c(1:4) | is.na(final_score))) %>%
  rowwise()%>%
  ungroup()%>%
  mutate(percent_stops_in_priority_bg = round(equity_stops_count/total_stop_count, 2)) %>% 
  group_by(service_change_num) %>% 
  mutate(
         route_opportunity_index_score = ntile(percent_stops_in_priority_bg, 5)) 

percent_stops_in_equity_bg_no_geo <- percent_stops_in_equity_bg %>% 
 st_drop_geometry()

#export route level data

  write_csv(percent_stops_in_equity_bg_no_geo, here("output", "route_opportunity_index_scores_213.csv"), append = F)
  
# save data as an .RDS as a backup 
save(percent_stops_in_equity_bg_no_geo, block_group_need_scores, file = here::here( "output", "route_and_block_group_equity_data.RDS"))

