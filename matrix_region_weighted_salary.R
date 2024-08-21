rm(list=ls())
options(scipen=999)

library(readxl)
library(dplyr)
library(magrittr)
library(scales)
library(ggplot2)
library(readODS)
library(writexl)
library(stringr)


### MATRIX

## FILTERS FOR: 
#North East
#North West
#Yorkshire and The Humber
#East Midlands
#West Midlands
#East
#London
#South East
#South West
#Wales
#Scotland
#Northern Ireland


#### INITIALISING ####
setwd("C:/Users/christy.bridgeman/Documents/Working Dissertation/Data")

completedata <- read_excel("modeldata.xlsx", sheet = 1)
completedata <- data.frame(completedata)

completedata$ln_Total_Admin_Intensity <- as.numeric(completedata$ln_Total_Admin_Intensity)
completedata$ln_Admin_Staff_Intensity <- as.numeric(completedata$ln_Admin_Staff_Intensity)
completedata$ln_Admin_Running_Cost_Intensity <- as.numeric(completedata$ln_Admin_Running_Cost_Intensity)
completedata$Shared_Services_Satisfaction_Index <- as.numeric(completedata$Shared_Services_Satisfaction_Index)
completedata$HR_q1_Concentration <- as.numeric(completedata$HR_q1_Concentration)
completedata$Finance_q1_Concentration <- as.numeric(completedata$Finance_q1_Concentration)
completedata$ln_Region_Weighted_Average_Salary <- as.numeric(completedata$ln_Region_Weighted_Average_Salary)
completedata$ln_Seniority_Weighted_Average_Salary <- as.numeric(completedata$ln_Seniority_Weighted_Average_Salary)
completedata$FTE.Total <- as.numeric(completedata$FTE.Total)
completedata$admin_total <- as.numeric(completedata$admin_total)
completedata$prog_total <- as.numeric(completedata$prog_total)
completedata$admin_staff <- as.numeric(completedata$admin_staff)
completedata$prog_staff <- as.numeric(completedata$prog_staff)
completedata$admin_running_costs <- as.numeric(completedata$admin_running_costs)
completedata$Num <- as.numeric(completedata$Num)


#### ORGANISATIONS VARIABLES

beis_orgs <- c('Department for Business, Energy and Industrial Strategy (excl. agencies)',
               'Advisory Conciliation and Arbitration Service',
               'Companies House',
               'Competition and Markets Authority',
               'Insolvency Service',
               'Land Registry',
               'Met Office',
               'UK Intellectual Property Office',
               'UK Space Agency', 'Department for Business, Energy and Industrial Strategy'
)

dit_orgs <- c('Department for International Trade')

dcms_orgs <- c('Department for Culture Media and Sport', 'Department for Digital, Culture, Media and Sport')

health_orgs <- c('Department of Health (excl. agencies)',
                 'Medicines and Healthcare Products Regulatory Agency',
                 'Public Health England', 'Department of Health and Social Care (excl. agencies)', 'Departmental of Health and Social Care'
)

ago_orgs <- c('Attorney General\'s Office',
              'Crown Prosecution Service',
              'Crown Prosecution Service Inspectorate',
              'Government Legal Department',
              'Serious Fraud Office', 'Attorney General\'s Departments'
)

dfe_orgs <- c('Department for Education',
              'Education Funding Agency',
              'Standards and Testing Agency',
              'The National College for Teaching and Leadership',
              'Skills Funding Agency', 'Education and Skills Funding Agency', 'Institute for Apprenticeships and Technical Education', 'Teaching Regulation Agency'
)

hmt_orgs <- c('HM Treasury',
              'Debt Management Office',
              'Government Internal Audit Agency',
              'Office for Budget Responsibility', 'National Infrastructure Commission'
)

co_orgs <- c('Cabinet Office (excl. agencies)',
             'Crown Commercial Service',
             'Government in Parliament', 'Government Property Agency', 'Cabinet Office'
             
)



regions_list <- c('North East',
                  'North West',
                  'Yorkshire and The Humber',
                  'East Midlands',
                  'West Midlands',
                  'East',
                  'London',
                  'South East',
                  'South West',
                  'Wales',
                  'Scotland',
                  'Northern Ireland',
                  'East of England'
)


matrix_overalls <- c(
  'Attorney General\'s Departments overall', 'Department for Business, Energy and Industrial Strategy overall', 'Cabinet Office overall', 'Department for Culture, Media and Sport overall', 'HM Treasury overall', 'Department for International Trade overall', 'Department for Education overall', 'Department of Health and Social Care overall', 'Department for Digital, Culture, Media and Sport overall', 'Department for Business and Trade overall', 'Department for Energy Security and Net Zero overall', 'Department for Science, Innovation and Technology overall'
)

#### FUNCTIONS


css_cleaner <- function(css) {
  new_col_names <- css[5, ] 
  css <- css[-(1:7), ]
  colnames(css) <- new_col_names 
  colnames(css)[1] <- 'Organisation'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  css <- css %>% 
    mutate_at(
      vars(`North East`, `North West`, `Yorkshire and The Humber`, `East Midlands`, `West Midlands`, `East`, `London`, `South East`, `South West`, `Wales`, `Scotland`, `Northern Ireland`), as.numeric
    )
  return(css)
}

css_cleaner_v2 <- function(css) {
  new_col_names <- css[4, ] 
  css <- css[-(1:6), ]
  colnames(css) <- new_col_names 
  colnames(css)[1] <- 'Organisation'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  css <- css %>% 
    mutate_at(
      vars(`North East`, `North West`, `Yorkshire and The Humber`, `East Midlands`, `West Midlands`, `East`, `London`, `South East`, `South West`, `Wales`, `Scotland`, `Northern Ireland`), as.numeric
    )
  return(css)
}




css_cleaner_v3 <- function(css) {
  new_col_names <- css[4, ] 
  css <- css[-(1:6), ]
  colnames(css) <- new_col_names 
  colnames(css)[1] <- 'Organisation'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  css <- css %>% 
    mutate_at(
      vars(`North East`, `North West`, `Yorkshire and The Humber`, `East Midlands`, `West Midlands`, `East of England`, `London`, `South East`, `South West`, `Wales`, `Scotland`, `Northern Ireland`), as.numeric
    )
  return(css)
}


css_cleaner_v4 <- function(css) {
  new_col_names <- css[5, ] 
  css <- css[-(1:6), ]
  colnames(css) <- new_col_names 
  colnames(css)[2] <- 'Organisation'
  colnames(css)[3] <- 'North East'
  colnames(css)[4] <- 'North West'
  colnames(css)[5] <- 'Yorkshire and The Humber'
  colnames(css)[6] <- 'East Midlands'
  colnames(css)[7] <- 'West Midlands'
  colnames(css)[8] <- 'East of England'
  colnames(css)[9] <- 'London'
  colnames(css)[10] <- 'South East'
  colnames(css)[11] <- 'South West'
  colnames(css)[12] <- 'Wales'
  colnames(css)[13] <- 'Scotland'
  colnames(css)[14] <- 'Northern Ireland'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  css <- css %>% 
    mutate_at(
      vars(`North East`, `North West`, `Yorkshire and The Humber`, `East Midlands`, `West Midlands`, `East of England`, `London`, `South East`, `South West`, `Wales`, `Scotland`, `Northern Ireland`), as.numeric
    )
  return(css)
}



filter_matrix <- function(css) {
  css <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% beis_orgs | Organisation %in% dit_orgs | Organisation %in% dcms_orgs | Organisation %in% health_orgs | Organisation %in% ago_orgs | Organisation %in% dfe_orgs | Organisation %in% hmt_orgs | Organisation %in% co_orgs)
}

filter_matrix_v2 <- function(css) {
  css <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% matrix_overalls)
}


find_region_totals_and_ratios <- function(css) {
  north_east_fte <<- css %>% 
    summarise(north_east_fte = sum(`North East`, na.rm = TRUE)) %>% 
    pull(north_east_fte)
  north_east_fte_ratio <<- north_east_fte / total_fte_for_observation
  
  north_west_fte <<- css %>% 
    summarise(north_west_fte = sum(`North West`, na.rm = TRUE)) %>% 
    pull(north_west_fte)
  north_west_fte_ratio <<- north_west_fte / total_fte_for_observation
  
  yorkshire_fte <<- css %>% 
    summarise(yorkshire_fte = sum(`Yorkshire and The Humber`, na.rm = TRUE)) %>% 
    pull(yorkshire_fte)
  yorkshire_fte_ratio <<- yorkshire_fte / total_fte_for_observation
  
  east_midlands_fte <<- css %>% 
    summarise(east_midlands_fte = sum(`East Midlands`, na.rm = TRUE)) %>% 
    pull(east_midlands_fte)
  east_midlands_fte_ratio <<- east_midlands_fte / total_fte_for_observation
  
  east_fte <<- css %>% 
    summarise(east_fte = sum(`East`, na.rm = TRUE)) %>% 
    pull(east_fte)
  east_fte_ratio <<- east_fte / total_fte_for_observation
  
  london_fte <<- css %>% 
    summarise(london_fte = sum(`London`, na.rm = TRUE)) %>% 
    pull(london_fte)
  london_fte_ratio <<- london_fte / total_fte_for_observation
  
  south_east_fte <<- css %>% 
    summarise(south_east_fte = sum(`South East`, na.rm = TRUE)) %>% 
    pull(south_east_fte)
  south_east_fte_ratio <<- south_east_fte / total_fte_for_observation
  
  south_west_fte <<- css %>% 
    summarise(south_west_fte = sum(`South West`, na.rm = TRUE)) %>% 
    pull(south_west_fte)
  south_west_fte_ratio <<- south_west_fte / total_fte_for_observation
  
  wales_fte <<- css %>% 
    summarise(wales_fte = sum(`Wales`, na.rm = TRUE)) %>% 
    pull(wales_fte)
  wales_fte_ratio <<- wales_fte / total_fte_for_observation
  
  scotland_fte <<- css %>% 
    summarise(scotland_fte = sum(`Scotland`, na.rm = TRUE)) %>% 
    pull(scotland_fte)
  scotland_fte_ratio <<- scotland_fte / total_fte_for_observation
  
  northern_ireland_fte <<- css %>% 
    summarise(northern_ireland_fte = sum(`Northern Ireland`, na.rm = TRUE)) %>% 
    pull(northern_ireland_fte)
  northern_ireland_fte_ratio <<- northern_ireland_fte / total_fte_for_observation
  
  return(css)
}

find_region_totals_and_ratios_v2 <- function(css) {
  north_east_fte <<- css %>% 
    summarise(north_east_fte = sum(`North East`, na.rm = TRUE)) %>% 
    pull(north_east_fte)
  north_east_fte_ratio <<- north_east_fte / total_fte_for_observation
  
  north_west_fte <<- css %>% 
    summarise(north_west_fte = sum(`North West`, na.rm = TRUE)) %>% 
    pull(north_west_fte)
  north_west_fte_ratio <<- north_west_fte / total_fte_for_observation
  
  yorkshire_fte <<- css %>% 
    summarise(yorkshire_fte = sum(`Yorkshire and The Humber`, na.rm = TRUE)) %>% 
    pull(yorkshire_fte)
  yorkshire_fte_ratio <<- yorkshire_fte / total_fte_for_observation
  
  east_midlands_fte <<- css %>% 
    summarise(east_midlands_fte = sum(`East Midlands`, na.rm = TRUE)) %>% 
    pull(east_midlands_fte)
  east_midlands_fte_ratio <<- east_midlands_fte / total_fte_for_observation
  
  east_fte <<- css %>% 
    summarise(east_fte = sum(`East of England`, na.rm = TRUE)) %>% 
    pull(east_fte)
  east_fte_ratio <<- east_fte / total_fte_for_observation
  
  london_fte <<- css %>% 
    summarise(london_fte = sum(`London`, na.rm = TRUE)) %>% 
    pull(london_fte)
  london_fte_ratio <<- london_fte / total_fte_for_observation
  
  south_east_fte <<- css %>% 
    summarise(south_east_fte = sum(`South East`, na.rm = TRUE)) %>% 
    pull(south_east_fte)
  south_east_fte_ratio <<- south_east_fte / total_fte_for_observation
  
  south_west_fte <<- css %>% 
    summarise(south_west_fte = sum(`South West`, na.rm = TRUE)) %>% 
    pull(south_west_fte)
  south_west_fte_ratio <<- south_west_fte / total_fte_for_observation
  
  wales_fte <<- css %>% 
    summarise(wales_fte = sum(`Wales`, na.rm = TRUE)) %>% 
    pull(wales_fte)
  wales_fte_ratio <<- wales_fte / total_fte_for_observation
  
  scotland_fte <<- css %>% 
    summarise(scotland_fte = sum(`Scotland`, na.rm = TRUE)) %>% 
    pull(scotland_fte)
  scotland_fte_ratio <<- scotland_fte / total_fte_for_observation
  
  northern_ireland_fte <<- css %>% 
    summarise(northern_ireland_fte = sum(`Northern Ireland`, na.rm = TRUE)) %>% 
    pull(northern_ireland_fte)
  northern_ireland_fte_ratio <<- northern_ireland_fte / total_fte_for_observation
  
  return(css)
}



find_region_totals_and_ratios_v2 <- function(css) {
  north_east_fte <<- css %>% 
    summarise(north_east_fte = sum(`North East`, na.rm = TRUE)) %>% 
    pull(north_east_fte)
  north_east_fte_ratio <<- north_east_fte / total_fte_for_observation
  
  north_west_fte <<- css %>% 
    summarise(north_west_fte = sum(`North West`, na.rm = TRUE)) %>% 
    pull(north_west_fte)
  north_west_fte_ratio <<- north_west_fte / total_fte_for_observation
  
  yorkshire_fte <<- css %>% 
    summarise(yorkshire_fte = sum(`Yorkshire and The Humber`, na.rm = TRUE)) %>% 
    pull(yorkshire_fte)
  yorkshire_fte_ratio <<- yorkshire_fte / total_fte_for_observation
  
  east_midlands_fte <<- css %>% 
    summarise(east_midlands_fte = sum(`East Midlands`, na.rm = TRUE)) %>% 
    pull(east_midlands_fte)
  east_midlands_fte_ratio <<- east_midlands_fte / total_fte_for_observation
  
  east_fte <<- css %>% 
    summarise(east_fte = sum(`East of England`, na.rm = TRUE)) %>% 
    pull(east_fte)
  east_fte_ratio <<- east_fte / total_fte_for_observation
  
  london_fte <<- css %>% 
    summarise(london_fte = sum(`London`, na.rm = TRUE)) %>% 
    pull(london_fte)
  london_fte_ratio <<- london_fte / total_fte_for_observation
  
  south_east_fte <<- css %>% 
    summarise(south_east_fte = sum(`South East`, na.rm = TRUE)) %>% 
    pull(south_east_fte)
  south_east_fte_ratio <<- south_east_fte / total_fte_for_observation
  
  south_west_fte <<- css %>% 
    summarise(south_west_fte = sum(`South West`, na.rm = TRUE)) %>% 
    pull(south_west_fte)
  south_west_fte_ratio <<- south_west_fte / total_fte_for_observation
  
  wales_fte <<- css %>% 
    summarise(wales_fte = sum(`Wales`, na.rm = TRUE)) %>% 
    pull(wales_fte)
  wales_fte_ratio <<- wales_fte / total_fte_for_observation
  
  scotland_fte <<- css %>% 
    summarise(scotland_fte = sum(`Scotland`, na.rm = TRUE)) %>% 
    pull(scotland_fte)
  scotland_fte_ratio <<- scotland_fte / total_fte_for_observation
  
  northern_ireland_fte <<- css %>% 
    summarise(northern_ireland_fte = sum(`Northern Ireland`, na.rm = TRUE)) %>% 
    pull(northern_ireland_fte)
  northern_ireland_fte_ratio <<- northern_ireland_fte / total_fte_for_observation
  
  return(css)
}




pull_regional_earnings <- function(css) {
  colnames(css)[11] <- 'Median_Earnings'
  colnames(css)[2] <- 'Region'
  css <- css[-(1:7), ]
  
  css <- css %>% 
    select(everything()) %>% 
    filter(Region %in% regions_list)
  
  north_east_salary <<- css %>% 
    filter(Region=='North East') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  north_east_salary <<- north_east_salary * north_east_fte_ratio

  
  north_west_salary <<- css %>% 
    filter(Region=='North West') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  north_west_salary <<- north_west_salary * north_west_fte_ratio
  
  
  yorkshire_salary <<- css %>% 
    filter(Region=='Yorkshire and The Humber' | Region=='Yorkshire') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  yorkshire_salary <<- yorkshire_salary * yorkshire_fte_ratio
  
  east_midlands_salary <<- css %>% 
    filter(Region=='East Midlands') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  east_midlands_salary <<- east_midlands_salary * east_midlands_fte_ratio
  
  east_salary <<- css %>% 
    filter(Region=='East' | Region=='East of England') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  east_salary <<- east_salary * east_fte_ratio
  
  london_salary <<- css %>% 
    filter(Region=='London') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  london_salary <<- london_salary * london_fte_ratio
  
  
  south_east_salary <<- css %>% 
    filter(Region=='South East') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  south_east_salary <<- south_east_salary * south_east_fte_ratio
  
  south_west_salary <<- css %>% 
    filter(Region=='South West') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  south_west_salary <<- south_west_salary * south_west_fte_ratio
  
  wales_salary <<- css %>% 
    filter(Region=='Wales') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  wales_salary <<- wales_salary * wales_fte_ratio
  
  scotland_salary <<- css %>% 
    filter(Region=='Scotland') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  scotland_salary <<- scotland_salary * scotland_fte_ratio
  
  
  
  
  northern_ireland_salary <<- css %>% 
    filter(Region=='Northern Ireland') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  northern_ireland_salary <<- northern_ireland_salary * northern_ireland_fte_ratio
  
  
  
  region_weighted_salary <<- log(mean(north_east_salary + north_west_salary + yorkshire_salary + east_midlands_salary + east_salary + london_salary + south_east_salary + south_west_salary + wales_salary + scotland_salary + northern_ireland_salary))
  
  

  return(region_weighted_salary)
}

pull_regional_earnings_v2 <- function(css) {
  colnames(css)[10] <- 'Median_Earnings'
  colnames(css)[1] <- 'Region'
  css <- css[-(1:7), ]
  
  css <- css %>% 
    select(everything()) %>% 
    filter(Region %in% regions_list)
  
  north_east_salary <<- css %>% 
    filter(Region=='North East') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  north_east_salary <<- north_east_salary * north_east_fte_ratio
  
  
  north_west_salary <<- css %>% 
    filter(Region=='North West') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  north_west_salary <<- north_west_salary * north_west_fte_ratio
  
  
  yorkshire_salary <<- css %>% 
    filter(Region=='Yorkshire and The Humber' | Region=='Yorkshire') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  yorkshire_salary <<- yorkshire_salary * yorkshire_fte_ratio
  
  east_midlands_salary <<- css %>% 
    filter(Region=='East Midlands') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  east_midlands_salary <<- east_midlands_salary * east_midlands_fte_ratio
  
  east_salary <<- css %>% 
    filter(Region=='East' | Region=='East of England') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  east_salary <<- east_salary * east_fte_ratio
  
  london_salary <<- css %>% 
    filter(Region=='London') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  london_salary <<- london_salary * london_fte_ratio
  
  
  south_east_salary <<- css %>% 
    filter(Region=='South East') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  south_east_salary <<- south_east_salary * south_east_fte_ratio
  
  south_west_salary <<- css %>% 
    filter(Region=='South West') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  south_west_salary <<- south_west_salary * south_west_fte_ratio
  
  wales_salary <<- css %>% 
    filter(Region=='Wales') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  wales_salary <<- wales_salary * wales_fte_ratio
  
  scotland_salary <<- css %>% 
    filter(Region=='Scotland') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  scotland_salary <<- scotland_salary * scotland_fte_ratio
  
  
  
  
  northern_ireland_salary <<- css %>% 
    filter(Region=='Northern Ireland') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  northern_ireland_salary <<- northern_ireland_salary * northern_ireland_fte_ratio
  
  
  
  region_weighted_salary <<- log(mean(north_east_salary + north_west_salary + yorkshire_salary + east_midlands_salary + east_salary + london_salary + south_east_salary + south_west_salary + wales_salary + scotland_salary + northern_ireland_salary))
  
  
  
  return(region_weighted_salary)
}






pull_regional_earnings_v3 <- function(css) {
  colnames(css)[8] <- 'Median_Earnings'
  colnames(css)[1] <- 'Region'
  css <- css[-(1:4), ]
  
  css <- css %>% 
    select(everything()) %>% 
    filter(Region %in% regions_list)
  
  north_east_salary <<- css %>% 
    filter(Region=='North East') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  north_east_salary <<- north_east_salary * north_east_fte_ratio
  
  
  north_west_salary <<- css %>% 
    filter(Region=='North West') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  north_west_salary <<- north_west_salary * north_west_fte_ratio
  
  
  yorkshire_salary <<- css %>% 
    filter(Region=='Yorkshire and The Humber' | Region=='Yorkshire') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  yorkshire_salary <<- yorkshire_salary * yorkshire_fte_ratio
  
  east_midlands_salary <<- css %>% 
    filter(Region=='East Midlands') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  east_midlands_salary <<- east_midlands_salary * east_midlands_fte_ratio
  
  east_salary <<- css %>% 
    filter(Region=='East of England') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  east_salary <<- east_salary * east_fte_ratio
  
  london_salary <<- css %>% 
    filter(Region=='London') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  london_salary <<- london_salary * london_fte_ratio
  
  
  south_east_salary <<- css %>% 
    filter(Region=='South East') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  south_east_salary <<- south_east_salary * south_east_fte_ratio
  
  south_west_salary <<- css %>% 
    filter(Region=='South West') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  south_west_salary <<- south_west_salary * south_west_fte_ratio
  
  wales_salary <<- css %>% 
    filter(Region=='Wales') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  wales_salary <<- wales_salary * wales_fte_ratio
  
  scotland_salary <<- css %>% 
    filter(Region=='Scotland') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  scotland_salary <<- scotland_salary * scotland_fte_ratio
  
  
  
  
  northern_ireland_salary <<- css %>% 
    filter(Region=='Northern Ireland') %>% 
    pull(Median_Earnings) %>% 
    as.numeric()
  northern_ireland_salary <<- northern_ireland_salary * northern_ireland_fte_ratio
  
  
  
  region_weighted_salary <<- log(mean(north_east_salary + north_west_salary + yorkshire_salary + east_midlands_salary + east_salary + london_salary + south_east_salary + south_west_salary + wales_salary + scotland_salary + northern_ireland_salary))
  
  
  
  return(region_weighted_salary)
}






### IMPORTING FTE NUMBERS, ONLY NEED TO DO THIS ONCE
matrix_fte <- read_excel("results_matrix_fte_1.xlsx", sheet = 1)




#### FINDING REGION AND EARNINGS NUMBERS EACH YEAR, MUST IMPORT NEW CSS EVERY TIME


### 1 of 8 ###

observation <- 1 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- matrix_fte$FTE.Total[observation]

css2017 <- read_excel("CSS/css2017.xls", sheet = 14) # always double check the pages these are in
css2017_earnings <- read_excel("CSS/css2017.xls", sheet = 28)

css <- css_cleaner(css2017)
css <- filter_matrix(css)
css <- find_region_totals_and_ratios(css)

region_weighted_average <- pull_regional_earnings(css2017_earnings)

sum(north_east_fte_ratio, north_west_fte_ratio, yorkshire_fte_ratio, east_midlands_fte_ratio, east_fte_ratio, london_fte_ratio, south_east_fte_ratio, south_west_fte_ratio, wales_fte_ratio,  scotland_fte_ratio, northern_ireland_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported locations and overseas staff. ]

print(region_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary = ifelse(Cluster=='Matrix' & Num==observation, region_weighted_average, ln_Region_Weighted_Average_Salary))


### 2 of 8 ###

observation <- 5 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- matrix_fte$FTE.Total[observation]

css2018 <- read_excel("CSS/css2018_v2.xlsx", sheet = 14) # always double check the pages these are in; double check if sheet is transposed
css2018_earnings <- read_excel("CSS/css2018_v2.xlsx", sheet = 28)

css <- css_cleaner(css2018) 
css <- filter_matrix(css)
css <- find_region_totals_and_ratios(css)

region_weighted_average <- pull_regional_earnings(css2018_earnings)

sum(north_east_fte_ratio, north_west_fte_ratio, yorkshire_fte_ratio, east_midlands_fte_ratio, east_fte_ratio, london_fte_ratio, south_east_fte_ratio, south_west_fte_ratio, wales_fte_ratio,  scotland_fte_ratio, northern_ireland_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported locations and overseas staff. ]

print(region_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary = ifelse(Cluster=='Matrix' & Num==observation, region_weighted_average, ln_Region_Weighted_Average_Salary))

### 3 of 8 ###

observation <- 9 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- matrix_fte$FTE.Total[observation]

css2019 <- read_excel("CSS/css2019.xlsx", sheet = 14) # always double check the pages these are in; double check if sheet is transposed
css2019_earnings <- read_excel("CSS/css2019.xlsx", sheet = 28)

css <- css_cleaner_v2(css2019) # need to use v2 as table is in a different format
css <- filter_matrix(css) 
css <- find_region_totals_and_ratios(css)

region_weighted_average <- pull_regional_earnings_v2(css2019_earnings) # change this to v2 as median earnings column changed

sum(north_east_fte_ratio, north_west_fte_ratio, yorkshire_fte_ratio, east_midlands_fte_ratio, east_fte_ratio, london_fte_ratio, south_east_fte_ratio, south_west_fte_ratio, wales_fte_ratio,  scotland_fte_ratio, northern_ireland_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported locations and overseas staff. ]

print(region_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary = ifelse(Cluster=='Matrix' & Num==observation, region_weighted_average, ln_Region_Weighted_Average_Salary))


### 4 of 8 ###

observation <- 13 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- matrix_fte$FTE.Total[observation]

css2020 <- read_excel("CSS/css2020.xlsx", sheet = 14) # always double check the pages these are in; double check if sheet is transposed
css2020_earnings <- read_excel("CSS/css2020.xlsx", sheet = 28)

css <- css_cleaner_v3(css2020) # need to use v2 as east is now 'east of england'
css <- filter_matrix(css) 
css <- find_region_totals_and_ratios_v2(css)

region_weighted_average <- pull_regional_earnings_v2(css2020_earnings) # change this to v2 as median earnings column changed

sum(north_east_fte_ratio, north_west_fte_ratio, yorkshire_fte_ratio, east_midlands_fte_ratio, east_fte_ratio, london_fte_ratio, south_east_fte_ratio, south_west_fte_ratio, wales_fte_ratio,  scotland_fte_ratio, northern_ireland_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported locations and overseas staff. ]

print(region_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary = ifelse(Cluster=='Matrix' & Num==observation, region_weighted_average, ln_Region_Weighted_Average_Salary))


### 5 of 8 ###

observation <- 17 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- matrix_fte$FTE.Total[observation]

css2021 <- read_excel("CSS/css2021.xlsx", sheet = 14) # always double check the pages these are in; double check if sheet is transposed
css2021_earnings <- read_excel("CSS/css2021.xlsx", sheet = 28)

css <- css_cleaner_v3(css2021) # need to use v2 as east is now 'east of england'
css <- filter_matrix(css) 
css <- find_region_totals_and_ratios_v2(css)

region_weighted_average <- pull_regional_earnings_v2(css2021_earnings) # change this to v2 as median earnings column changed

sum(north_east_fte_ratio, north_west_fte_ratio, yorkshire_fte_ratio, east_midlands_fte_ratio, east_fte_ratio, london_fte_ratio, south_east_fte_ratio, south_west_fte_ratio, wales_fte_ratio,  scotland_fte_ratio, northern_ireland_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported locations and overseas staff. ]

print(region_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary = ifelse(Cluster=='Matrix' & Num==observation, region_weighted_average, ln_Region_Weighted_Average_Salary))

### 6 of 8 ###

observation <- 21 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- matrix_fte$FTE.Total[observation]

css2022 <- read_ods("CSS/css2022.ods", sheet = 17) # always double check the pages these are in; double check if sheet is transposed
css2022_earnings <- read_ods("CSS/css2022.ods", sheet = 31)

css <- css_cleaner_v4(css2022) 
css <- filter_matrix_v2(css) 
css <- find_region_totals_and_ratios_v2(css)

region_weighted_average <- pull_regional_earnings_v3(css2022_earnings) # change this to v2 as median earnings column changed

sum(north_east_fte_ratio, north_west_fte_ratio, yorkshire_fte_ratio, east_midlands_fte_ratio, east_fte_ratio, london_fte_ratio, south_east_fte_ratio, south_west_fte_ratio, wales_fte_ratio,  scotland_fte_ratio, northern_ireland_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported locations and overseas staff. ]

print(region_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary = ifelse(Cluster=='Matrix' & Num==observation, region_weighted_average, ln_Region_Weighted_Average_Salary))



### 7 of 8 ###

observation <- 25 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- matrix_fte$FTE.Total[observation]

css2023 <- read_ods("CSS/css2023.ods", sheet = 18) # always double check the pages these are in; double check if sheet is transposed
css2023_earnings <- read_ods("CSS/css2023.ods", sheet = 32)

css <- css_cleaner_v4(css2023) 
css <- filter_matrix_v2(css) 
css <- find_region_totals_and_ratios_v2(css)

region_weighted_average <- pull_regional_earnings_v3(css2023_earnings) # change this to v2 as median earnings column changed

sum(north_east_fte_ratio, north_west_fte_ratio, yorkshire_fte_ratio, east_midlands_fte_ratio, east_fte_ratio, london_fte_ratio, south_east_fte_ratio, south_west_fte_ratio, wales_fte_ratio,  scotland_fte_ratio, northern_ireland_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported locations and overseas staff. ]

print(region_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary = ifelse(Cluster=='Matrix' & Num==observation, region_weighted_average, ln_Region_Weighted_Average_Salary))



### 8 of 8 ###

observation <- 29 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- matrix_fte$FTE.Total[observation]

css2024 <- read_ods("CSS/css2024.ods", sheet = 17) # always double check the pages these are in; double check if sheet is transposed
css2024_earnings <- read_ods("CSS/css2024.ods", sheet = 31)

css <- css_cleaner_v4(css2024) 
css <- filter_matrix_v2(css) 
css <- find_region_totals_and_ratios_v2(css)

region_weighted_average <- pull_regional_earnings_v3(css2024_earnings) # change this to v2 as median earnings column changed

sum(north_east_fte_ratio, north_west_fte_ratio, yorkshire_fte_ratio, east_midlands_fte_ratio, east_fte_ratio, london_fte_ratio, south_east_fte_ratio, south_west_fte_ratio, wales_fte_ratio,  scotland_fte_ratio, northern_ireland_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported locations and overseas staff. ]

print(region_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary = ifelse(Cluster=='Matrix' & Num==observation, region_weighted_average, ln_Region_Weighted_Average_Salary))





###### NATURAL CUBIC SPLINE INTERPOLATION



x <- c(1:29)
y <- completedata$ln_Region_Weighted_Average_Salary[1:29]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "Region Weights Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


matrix_indices <- which(completedata$Cluster == 'Matrix')

completedata <- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary_ncsi = NA)

completedata <- completedata %>%
  mutate(ln_Region_Weighted_Average_Salary_ncsi = ifelse(Cluster == 'Matrix', NA, ln_Region_Weighted_Average_Salary_ncsi))

completedata$ln_Region_Weighted_Average_Salary_ncsi[matrix_indices] <- spline_values





# WRITE TO DATAFRAME

write_xlsx(completedata, "outputs/results_matrix_region_weights.xlsx")

