### Script to calculate inequality measure for disease areas
# TODO: add better docs -  Roxygen 
# Setup -------------------------------------------------------------------
# Packages
library(pacman)
pacman::p_load("dplyr", "janitor", "ggplot2")
# Setup scripts
#source("C:/nhs-nwl-icb-health-equity/src/data_cleaning.R")
source("C:/nhs-nwl-icb-health-equity/src/plot_theme.R")


# Functions ---------------------------------------------------------------

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

## Arguments:
# Data : A data frame on a person level needs to have at least 1 group and age band with condition occurrence in a binary format (0,1)
# group : a column header in the data frame to group by
# age_col : character column in data frame representing age categories 
# age_min : minimum age of the to filter the data frame by
# age_max : maximum age of the to filter the data frame by
calculate_prevalence <- function(data, group, date_name, metric, metric_value, age_col=age_band, population, standard_ages=NULL) {
  
  quo_age_col <- rlang::quo_text(enquo(age_col)) # converts column name to string
  
  data_sum <- data %>% 
    dplyr::group_by({{date_name}}, {{group}}, {{metric}}, {{age_col}}) %>% 
    dplyr::summarise(case_count=sum({{metric_value}}),
                     total=sum({{population}}))
    
    if(is.null(standard_ages)) {
        output <- data_sum  %>% 
            dplyr::group_by({{date_name}}, {{group}}, {{metric}}) %>%
            PHEindicatormethods::phe_proportion(x=case_count, n=total, confidence = 0.95, multiplier=100) %>% 
            dplyr::group_by({{date_name}}, {{metric}}) %>%
            dplyr::mutate(is_ref=ifelse(value==min(value), "Ref", "")) %>% 
            dplyr::ungroup()
    
    }
    
    else {
        output <- data_sum  %>% 
            dplyr::group_by({{date_name}}, {{group}}, {{metric}}, {{age_col}})  %>% 
            PHEindicatormethods::phe_rate(x=case_count, n=total, confidence = 0.95, multiplier=1) %>% 
            dplyr::left_join(standard_ages, by=quo_age_col) %>% 
            dplyr::mutate(age_standarised_count=value*obs_value) %>% 
            dplyr::group_by({{date_name}}, {{group}}, {{metric}}) %>%
            dplyr::summarise(case_count=sum(age_standarised_count),
                         total=sum(obs_value)) %>%
            dplyr::group_by({{date_name}}, {{group}}, {{metric}}) %>%
            PHEindicatormethods::phe_proportion(x=case_count, n=total, confidence = 0.95, multiplier=100) %>% 
            dplyr::group_by({{date_name}}, {{metric}}) %>%
            dplyr::mutate(is_ref=ifelse(value==min(value), "Ref", "")) %>% 
            dplyr::ungroup()
        }
  
  return(output)  
                     }

## Arguments:
# Data : A data frame on a person level containing date, person, count of occurrences, age and at least 1 group
# date: date columun
# group : a column header in the data frame to group by
# metric_value: numeric - number of occurrences of the metric 
# age_col : character column in data frame representing age categories 
# age_min : minimum age of the to filter the data frame by
# age_max : maximum age of the to filter the data frame by
calculate_rates <- function(data, date_name, group, metric, metric_value, population, age_col, standard_ages=NULL, multiplier) {
  
  quo_age_col <- rlang::quo_text(enquo(age_col)) # converts column name to string


  data_sum <- data %>% 
    dplyr::group_by({{date_name}}, {{metric}}, {{group}}, {{age_col}}) %>% 
    dplyr::summarise(case_count=sum({{metric_value}}),
                     total=sum({{population}}))  %>% 
     dplyr::ungroup()  
    
    if(is.null(standard_ages)) {
        output <- data_sum  %>% 
            dplyr::group_by({{date_name}}, {{metric}}, {{group}}) %>% 
            PHEindicatormethods::phe_rate(x=case_count, n=total, confidence = 0.95, multiplier=multiplier) %>% 
            dplyr::group_by({{date_name}}, {{metric}}) %>%
            dplyr::mutate(is_ref=ifelse(value==min(value, na.rm=TRUE), "Ref", "")) %>% 
            dplyr::ungroup()
        }
    else {
        output <- data_sum  %>% 
            dplyr::left_join(standard_ages, by=quo_age_col) %>% 
            dplyr::group_by({{date_name}}, {{metric}}, {{group}}) %>% 
            PHEindicatormethods::phe_dsr(x=case_count, n=total, type="standard", stdpop = obs_value, 
                                         stdpoptype = "field",confidence = 0.95, multiplier=multiplier) %>% 
            dplyr::group_by({{date_name}}, {{metric}}) %>%
            dplyr::rename(total=total_pop, case_count=total_count)  %>% 
            dplyr::mutate(is_ref=ifelse(value==min(value, na.rm=TRUE), "Ref", "")) %>% 
            dplyr::ungroup()
    }
  

 return(output)
}

# #Calculate significance (chi_square)

calculate_significance_compared_to_ref <- function(data, group, identifiers){
  #group_column <- quo_name(group)

  ref= data %>%
    dplyr::filter(is_ref=="Ref") %>%
    dplyr::select(group) %>%
    toString()

  column_identifiers <- data %>%
    dplyr::select((identifiers)) %>%
    dplyr::distinct()

  data_formatted <- data %>%
    dplyr::mutate(no=total-case_count) %>%
    dplyr::rename(yes=case_count) %>%
    dplyr::select(group, yes, no) %>%
    tidyr::pivot_longer(cols = c(yes, no), names_to = "occurance", values_to = "count") %>% 
    dplyr::rename(grouper=group)

  contingency_table <- xtabs(count ~ grouper  + occurance, data=data_formatted)#
  #contingency_table <- xtabs(count ~ ethnic_category_mother + occurance, data=data_formatted)

  results <- rstatix::pairwise_prop_test(contingency_table) %>%
    dplyr::filter(group1==ref | group2==ref) %>%
    dplyr::mutate(ref_column=ref,
                  join_category=ifelse(group1==ref, group2, group1)) %>%
    dplyr::select(-c(group1, group2)) %>%
     dplyr::bind_cols(column_identifiers)
  
  return(results)
}

calculate_equity_index <- function(data, metric, groups=NULL, significant=FALSE) {
  
  if (significant==TRUE) {
    
    data <- data %>% 
      dplyr::filter(p.adj.signif!="ns")
  }

  
  output <- data %>% 
    dplyr::group_by({{metric}}, {{groups}}) %>% 
    dplyr::mutate(diffs=value-min(value, na.rm=TRUE)) %>% 
    dplyr::group_by({{metric}}, {{groups}}) %>% 
    dplyr::summarise(MAD_mean=sum(diffs, na.rm=TRUE)/n(),
                     RMAD_mean=MAD_mean/(max(uppercl, na.rm=TRUE)))
}

plot_indexes <- function(data, group_name){
  
  mdbw <- data %>% 
    ggplot(aes(x=MAD_mean, y=forcats::fct_reorder(gsub("_", " ",metric), MAD_mean))) +
    geom_point() +
    theme_nwl() +
    scale_y_discrete(label=scales::wrap_format(50)) +
    labs(x="Mean distance from minimum value", y="Condition", title=paste0("Mean difference from minimum value group by ", group_name)) 
  
  idisw <- data %>% 
    ggplot(aes(x=RMAD_mean, y=forcats::fct_reorder(gsub("_", " ",metric), RMAD_mean))) +
    geom_point() +
    theme_nwl() +
    scale_y_discrete(label=scales::wrap_format(50)) +
    labs(x="Relative distance from minimum value", y="Condition", title=paste0("Relative difference from minimum value group by ", group_name))
  
  joint_plot <- ggpubr::ggarrange(mdbw, idisw, ncol=1)
  
  return(joint_plot)
}

plot_indexes_over_time <- function(data, date_name, group_name, CIs=TRUE){
  
  mdbw <- data %>% 
    ggplot(aes(x={{date_name}}, y=MAD_mean, color=gsub("_", " ",metric))) +
    geom_point() +
    geom_line()+ 
    theme_nwl() +
    labs(x="Date",
         y="Mean distance from minimum value",
         color="Metric",
         title=paste0("Mean difference from minimum value group by ", group_name)) +
    scale_color_discrete(label=scales::wrap_format(50))
  
  idisw <- data %>% 
    ggplot(aes(x={{date_name}}, y=RMAD_mean , color=gsub("_", " ",metric))) +
    geom_point() +
    geom_line() +
    theme_nwl() +
    labs(x="Date",
         y="Relative distance from minimum value",
         title=paste0("Relative difference from minimum value group by ", group_name),
         color="Metric") +
    scale_color_discrete(label=scales::wrap_format(50)) 
     
  if (CIs==TRUE){
    mdbw <- mdbw +
      geom_ribbon(aes(ymin=mdbw_lower, ymax=mdbw_upper, fill=gsub("_", " ",metric)), alpha=0.4) +
      labs(fill="Metric") +
      scale_fill_discrete(label=scales::wrap_format(50)) 
      
    
    idisw <- idisw +
      geom_ribbon(aes(ymin=idisw_lower, ymax=idisw_upper, fill=gsub("_", " ",metric)), alpha=0.4) +
      labs(fill="Metric") +
      scale_fill_discrete(label=scales::wrap_format(50)) 
      
  }
  
  joint_plot <- ggpubr::ggarrange(mdbw, idisw, ncol=1, common.legend = TRUE, legend="right")
  print(joint_plot)
  
  return(joint_plot)

}


plot_metric_specific_index <- function(data, index, column_name, group_name, ltc, metric, metric_measure, highlight_sig=TRUE) {
  
  index_values <- index %>% 
    dplyr::filter({{metric}}==ltc)
  
  col_colors <- c("****"="#41B6E6", "Ref"="#005EB8", "***"="#41B6E6", "ns"="#768692","**"="#41B6E6", "*"="#41B6E6")
  
  plot <- data %>%
    filter({{metric}}==ltc) %>% 
    dplyr::mutate(p.adj.signif=ifelse(is.na(p.adj.signif), "Ref", p.adj.signif)) %>% 
    ggplot(aes(x={{column_name}}, y=value)) +
    geom_col(aes(fill=p.adj.signif)) +
    geom_label(data= data %>% 
                 dplyr::filter(is_ref=="Ref" & metric==ltc),
               aes(x={{column_name}}, y=value/2, label=paste0("AAD: ", signif(index_values$MAD_mean,3), "\nRAAD: ", signif(index_values$RMAD_mean,3)))) +
    geom_errorbar(aes(ymin=lowercl , ymax=uppercl), width=0.4)+
    scale_fill_manual(values=col_colors) +
    theme_nwl() +
    labs(x=group_name,
         y=stringr::str_wrap(metric_measure, 40),
         title=paste0(group_name, " - ", gsub("_", " ", ltc))) +
    theme(legend.position = "none")
  
  return(plot)

}
