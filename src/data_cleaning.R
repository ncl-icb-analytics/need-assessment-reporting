
# # script to clean detailed ethnicities

clean_detailed_ethnicities <- function(data, ethnic_category, ethnic_description) {
  output <- data %>% 
    dplyr::mutate(ethnicity_description=stringr::str_to_lower({{ethnic_description}}),
                  ethnic_category=stringr::str_to_lower({{ethnic_category}})) %>% 
    dplyr::mutate(ethnicity_detailed = dplyr::case_when(
      grepl("african",ethnicity_description) & ethnic_category=="black or black british" ~ "african - black",
      grepl("african",ethnicity_description) & ethnic_category=="mixed" ~ "african - mixed",
      grepl("caribbean", ethnicity_description) & ethnic_category=="black or black british" ~ "caribbean - black",
      grepl("caribbean", ethnicity_description) & ethnic_category=="mixed" ~ "caribbean - mixed",
      grepl("pakistani", ethnicity_description) ~ "pakistani",
      grepl("indian", ethnicity_description) ~ "indian",
      grepl("bangladeshi", ethnicity_description) ~ "bangladeshi",
      grepl("chinese", ethnicity_description) ~ "chinese",
      grepl("other asian", ethnicity_description) &  ethnic_category=="asian or asian british" ~ "other asian",
      grepl("asian", ethnicity_description) &  ethnic_category=="mixed" ~ "asian - mixed",
      grepl("black", ethnicity_description) &  ethnic_category=="mixed" ~ "black - mixed",
      grepl("irish", ethnicity_description) ~ "irish",
      ethnic_category=="white" & (grepl("white", ethnicity_description) |  grepl("british", ethnicity_description))  ~ "white",
      grepl("any other mixed", ethnicity_description) ~ "other mixed",
      grepl("any other ethnic", ethnicity_description) ~ "other ethnic group",
      grepl("any other black", ethnicity_description) ~ "other black",
      TRUE ~ NA
    ))
  
  return(output)
} 

age_band_life_course_model <- function(data, age_col) {
  
  output <- data %>% 
    dplyr::filter(!is.na({{age_col}})) %>% 
    dplyr::mutate(age_bands=dplyr::case_when(
      {{age_col}} < 18 ~ "children",
      {{age_col}} >= 18 & {{age_col}} < 65 ~ "adults",
      {{age_col}} >= 65 ~ "older adults",
      TRUE ~ NA))
  
  return(output)

}

age_band_5y <- function(data, age_col) {
  
  output <- data %>% 
    dplyr::filter(!is.na({{age_col}})) %>% 
    dplyr::mutate(age_band=dplyr::case_when(
      {{age_col}} < 5 ~ "0-4",
      {{age_col}} >= 5 & {{age_col}} < 10 ~ "5-9",
      {{age_col}} >= 10 & {{age_col}} < 15 ~ "10-14",
      {{age_col}} >= 15 & {{age_col}} < 20 ~ "15-19",
      {{age_col}} >= 20 & {{age_col}} < 25 ~ "20-24",
      {{age_col}} >= 25 & {{age_col}} < 30 ~ "25-29",
      {{age_col}} >= 30 & {{age_col}} < 35 ~ "30-34",
      {{age_col}} >= 35 & {{age_col}} < 40 ~ "35-39",
      {{age_col}} >= 40 & {{age_col}} < 45 ~ "40-44",
      {{age_col}} >= 45 & {{age_col}} < 50 ~ "45-49",
      {{age_col}} >= 50 & {{age_col}} < 55 ~ "50-54",
      {{age_col}} >= 55 & {{age_col}} < 60 ~ "55-59",
      {{age_col}} >= 60 & {{age_col}} < 65 ~ "60-64",
      {{age_col}} >= 65 & {{age_col}} < 70 ~ "65-69",
      {{age_col}} >= 70 & {{age_col}} < 75 ~ "70-74",
      {{age_col}} >= 75 & {{age_col}} < 80 ~ "75-79",
      {{age_col}} >= 80 & {{age_col}} < 85 ~ "80-84",
      {{age_col}} >= 85 & {{age_col}} < 90 ~ "85-89",
      {{age_col}} >= 90 ~"90+",
      TRUE ~ NA)) %>% 
    dplyr::mutate(age_band=factor(age_band, levels =c( "0-4", "5-9" , "10-14", "15-19" , "20-24" , "25-29", "30-34", "35-39", "40-44", "45-49",  "50-54", "55-59" , "60-64", "65-69", "70-74" , "75-79", "80-84","85-89", "90+" )))
  
  return(output)

}

## Arguments
# Data : A data frame
# Decile : numeric [1-10]
make_quintiles <- function(data, decile)  {
  output <- data %>% 
    dplyr::mutate(decile_num = as.numeric({{decile}}),
                  quintile=dplyr::case_when(
                    decile_num > 0 & decile_num <=2 ~ 1,
                    decile_num > 2 & decile_num <=4 ~ 2,
                    decile_num > 4 & decile_num <=6 ~ 3,
                    decile_num > 6 & decile_num <=8 ~ 4,
                    decile_num > 8 & decile_num <=10 ~ 5,
                    TRUE ~ NA
                  )) %>% 
    dplyr::select(-decile_num)
  
  
  return(output)
}



