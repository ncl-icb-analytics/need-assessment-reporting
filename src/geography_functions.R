## Functions to make georgaphy things quicker and  slicker

get_north_west_london_code <- function() {
  
  msoa_population <- read.csv("C:/nwlicb/data/lookups/Population_Statistics%2C_England_(MSOA_level).csv") %>% 
    janitor::clean_names() 
  
  lsoa_icb <-  readxl::read_excel("C:/nwlicb/data/lookups/LSOA11_LOC22_ICB22_LAD22_EN_LU.xlsx") %>% 
    janitor::clean_names()
  
  ward_lsoa <- read.csv("C:/nwlicb/data/lookups/lsoa_21_ward_22_lad_22.csv") %>% 
    janitor::clean_names()
  
  nwl_geography_codes <- read.csv("C:/nwlicb/data/lookups/lsoa_2011_lsoa_2021_msoa_lad_lookup_for_nwl.csv") %>% 
    dplyr::left_join(msoa_population %>% distinct(msoa11cd, all_ages), by="msoa11cd") %>% 
    dplyr::left_join(lsoa_icb %>%  select(lsoa11cd, icb22cd, icb22cdh, icb22nm),  by=c("lsoa11cd"="lsoa11cd")) %>% 
    dplyr::left_join(ward_lsoa %>% select(lsoa21cd, wd22cd, wd22nm), by=c("lsoa21cd"="lsoa21cd"))

  
  lad_codes_21 <- unique(nwl_geography_codes$lad22cd)
  msoa_codes_21 <-  unique(nwl_geography_codes$msoa21cd)
  msoa_codes_11 <-  unique(nwl_geography_codes$msoa11cd)
  lsoa_codes_11 <-  unique(nwl_geography_codes$lsoa11cd)
  lsoa_codes_21 <-  unique(nwl_geography_codes$lsoa21cd)
  wd_codes_22 <-  unique(nwl_geography_codes$wd22cd)
  
  return(list(nwl_all_codes=nwl_geography_codes,
              lad_codes_21=lad_codes_21, 
              msoa_codes_21=msoa_codes_21, 
              msoa_codes_11=msoa_codes_11, 
              lsoa_codes_11=lsoa_codes_11, 
              lsoa_codes_21=lsoa_codes_21,
              wd_codes_22=wd_codes_22))
}

get_geography_codes <- function() {
  
  #msoa_population <- read.csv("C:/nwlicb/data/lookups/Population_Statistics%2C_England_(MSOA_level).csv") %>% 
   # janitor::clean_names() 
  
  lsoa_icb <-  readxl::read_excel("C:/nwlicb/data/lookups/LSOA11_LOC22_ICB22_LAD22_EN_LU.xlsx") %>% 
    janitor::clean_names()
  
  geography_codes <- read.csv("C:/nwlicb/data/lookups/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2).csv") %>% 
    janitor::clean_names() %>% 
    #dplyr::left_join(msoa_population %>% distinct(msoa11cd, all_ages), by="msoa11cd") %>% 
    dplyr::left_join(lsoa_icb %>%  select(lsoa11cd, icb22cd, icb22cdh, icb22nm),  by=c("lsoa11cd"="lsoa11cd"))
  
  return(list(geography_codes=geography_codes))
}

ingest_shape_date <- function(geography, filter_codes, all_codes) {
  
  if (geography=="lsoa") {
    shape <- sf::st_read("C:/nwlicb/data/geographies/LSOA_2021_EW_BGC.shp",  quiet = TRUE) %>% 
      janitor::clean_names() %>% 
      dplyr::left_join(all_codes, by=c("lsoa21cd", "lsoa21nm")) %>% 
      dplyr::filter(lsoa21cd %in% filter_codes)
  }
  
  if (geography=="lsoa_11") {
    shape <- sf::st_read("C:/nwlicb/data/geographies/LSOA_2011_EW_BFC_V3.shp",  quiet = TRUE) %>% 
      janitor::clean_names() %>% 
      dplyr::left_join(all_codes, by=c("lsoa11cd", "lsoa11nm")) %>% 
      dplyr::filter(lsoa11cd %in% filter_codes)
  }
  
  
  if (geography=="msoa") {
    shape <- sf::st_read("C:/nwlicb/data/geographies/infuse_msoa_lyr_2011.shp",  quiet = TRUE) %>% 
      janitor::clean_names() %>% 
      dplyr::rename(msoa11cd=geo_code) %>% 
      dplyr::left_join(all_codes, by=c("msoa11cd")) %>% 
      dplyr::filter(msoa11cd %in% filter_codes)
  }
  
  if (geography=="lad") {
  shape <- sf::st_read("C:/nwlicb/data/geographies/Local_Authority_Districts__May_2020__Boundaries_UK_BFE.shp",  quiet = TRUE) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(lad20cd %in% filter_codes)
  }
  
  if (geography=="ward") {
    shape <- sf::st_read("C:/nwlicb/data/geographies/WD_DEC_2022_UK_BSC.shp",  quiet = TRUE) %>% 
      janitor::clean_names() %>% 
      dplyr::filter(wd22cd %in% filter_codes)
  }
  
  return(shape)
}

get_tube_stops_data <- function() {
  
  tube_stops <- jsonlite::fromJSON(txt="https://api.tfl.gov.uk/StopPoint/Mode/tube", flatten=TRUE)$stopPoints
  
  tube_stops_filtered <- tube_stops %>% 
    dplyr::filter(stopType=="NaptanMetroStation")
  
  tube_lines <- dplyr::bind_rows(tube_stops_filtered$lineGroup)  %>% 
    tidyr::unnest(lineIdentifier) %>% 
    dplyr::filter(!stringr::str_detect(lineIdentifier, "\\d")) 
  
  tube_stops_unique <- tube_stops_filtered %>% 
    dplyr::select(stationNaptan, commonName, lat, lon) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
    dplyr::left_join(tube_lines,
                     by=c("stationNaptan"="stationAtcoCode"))
  
  return(tube_stops_unique)
  
}

get_deprivation_data <-  function() {
  
  imd <- read.csv("C:/nhs-nwl-icb-health-equity/src/data/File_1_-_IMD2019_Index_of_Multiple_Deprivation.csv") %>% 
    janitor::clean_names()
}  
