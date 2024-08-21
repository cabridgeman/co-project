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


### MATRIX ORGANISATIONS
# Department for Business, Energy, and Industrial Strategy (2016-2023) (and child agencies)
# Department for Energy Security and Net Zero (2023-), Department for Science, Innovation and Technology (2023-) (and child agencies)

# Department for International Trade (2016-2023)  (and child agencies)
# Department for Business and Trade (2023-)  (and child agencies)

# Department for Digital, Culture, Media and Sport (2017-2023) 
# Department for Culture, Media and Sport (2023-) (and child agencies)

# Department of Health (-2018) (and child agencies)
# Department of Health and Social Care (2018-) (and child agencies)

# Attorney General’s Office (and child agencies)

# Department for Education (and child agencies)

# HM Treasury (and child agencies)

# Cabinet Office  (and child agencies)



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


#### LISTS OF DEPARTMENTS AND CHILD AGENCIES ####

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

matrix_depts_overalls <- c(
  'Attorney General\'s Departments overall', 'Department for Business, Energy and Industrial Strategy overall', 'Cabinet Office overall', 'Department for Culture, Media and Sport overall', 'HM Treasury overall', 'Department for International Trade overall', 'Department for Education overall', 'Department of Health and Social Care overall', 'Department for Digital, Culture, Media and Sport overall', 'Department for Business and Trade overall', 'Department for Energy Security and Net Zero overall', 'Department for Science, Innovation and Technology overall'
)

beis_overalls <- c('Department for Business, Energy and Industrial Strategy overall', 'Department for Energy Security and Net Zero overall', 'Department for Science, Innovation and Technology overall') # for v2's

dit_overalls <- c('Department for International Trade overall', 'Department for Business and Trade overall')

dcms_overalls <- c('Department for Culture, Media and Sport overall', 'Department for Digital, Culture, Media and Sport overall')

health_overalls <- c('Department of Health and Social Care overall')

ago_overalls <- c('Attorney General\'s Departments overall')

dfe_overalls <- c('Department for Education overall')

hmt_overalls <- c('HM Treasury overall')

co_overalls <-c ('Cabinet Office overall')


##### FUNCTIONS #####

css_cleaner <- function(css) {
  new_col_names <- css[4, ] 
  css <- css[-(1:7), ]
  colnames(css) <- new_col_names 
  colnames(css)[1] <- 'Organisation'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  return(css)
}

css_cleaner_v2 <- function(css) { # this version of CSS includes a total for departments.
  css <- css[-(1:5), ]
  colnames(css)[2] <- 'Organisation' # total is in this column; do not include column 1 as that adds dept TOTAL /and/ child agency TOTALS
  colnames(css)[9] <- 'Finance'
  colnames(css)[11] <- 'Human Resources'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  return(css)
}

css_cols <- function(css) {
  css <- css %>% 
    select(Organisation, `Finance`, `Human Resources`)
  return(css)
}

css_to_num <- function(css) {
  css <- css %>% 
    mutate(
      Finance = as.numeric(Finance),
      `Human Resources` = as.numeric(`Human Resources`)
    )
  return(css)
}

matrix_filter <- function(css) {
  css <- css %>% # filtering to all matrix departments and child agencies
    select(everything()) %>% 
    filter(Organisation %in% beis_orgs | Organisation %in% dit_orgs | Organisation %in% dcms_orgs | Organisation %in% health_orgs | Organisation %in% ago_orgs | Organisation %in% dfe_orgs | Organisation %in% hmt_orgs | Organisation %in% co_orgs)
}

matrix_filter_v2 <- function(css) {
  css <- css %>% # filtering to all matrix departments (overalls)
    select(everything()) %>% 
    filter(Organisation %in% matrix_depts_overalls) # for last entry, we need to group ESNZ + DSIT together
}

find_total_HR <- function(css) {
  total_hr <<- css %>% 
    summarise(total_hr = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total_hr)
  return(css)
}

find_total_Finance <- function(css) {
  total_f <<- css %>% 
    summarise(total_f = sum(Finance, na.rm = TRUE)) %>% 
    pull(total_f)
  return(css)
}

find_beis_HR_ratio <- function(css) {
  beis_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% beis_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  beis_hr_ratio <<- beis_hr_num / total_hr
  return(css)
}

find_beis_HR_ratio_v2 <- function(css) {
  beis_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% beis_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  beis_hr_ratio <<- beis_hr_num / total_hr
  return(css)
}

find_beis_Finance_ratio <- function(css) {
  beis_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% beis_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  beis_finance_ratio <<- beis_finance_num / total_f
  return(css)
}

find_beis_Finance_ratio_v2 <- function(css) {
  beis_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% beis_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  beis_finance_ratio <<- beis_finance_num / total_f
  return(css)
}

find_dit_HR_ratio <- function(css) {
  dit_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dit_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dit_hr_ratio <<- dit_hr_num / total_hr
  return(css)
}

find_dit_HR_ratio_v2 <- function(css) {
  dit_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dit_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dit_hr_ratio <<- dit_hr_num / total_hr
  return(css)
}

find_dit_Finance_ratio <- function(css) {
  dit_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dit_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dit_finance_ratio <<- dit_finance_num / total_f
  return(css)
}

find_dit_Finance_ratio_v2 <- function(css) {
  dit_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dit_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dit_finance_ratio <<- dit_finance_num / total_f
  return(css)
}

find_dcms_HR_ratio <- function(css) {
  dcms_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dcms_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dcms_hr_ratio <<- dcms_hr_num / total_hr
  return(css)
}

find_dcms_HR_ratio_v2 <- function(css) {
  dcms_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dcms_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dcms_hr_ratio <<- dcms_hr_num / total_hr
  return(css)
}

find_dcms_Finance_ratio <- function(css) {
  dcms_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dcms_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dcms_finance_ratio <<- dcms_finance_num / total_f
  return(css)
}

find_dcms_Finance_ratio_v2 <- function(css) {
  dcms_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dcms_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dcms_finance_ratio <<- dcms_finance_num / total_f
  return(css)
}

find_health_HR_ratio <- function(css) {
  health_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% health_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  health_hr_ratio <<- health_hr_num / total_hr
  return(css)
}

find_health_HR_ratio_v2 <- function(css) {
  health_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% health_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  health_hr_ratio <<- health_hr_num / total_hr
  return(css)
}

find_health_Finance_ratio <- function(css) {
  health_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% health_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  health_finance_ratio <<- health_finance_num / total_f
  return(css)
}

find_health_Finance_ratio_v2 <- function(css) {
  health_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% health_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  health_finance_ratio <<- health_finance_num / total_f
  return(css)
}

find_ago_HR_ratio <- function(css) {
  ago_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% ago_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  ago_hr_ratio <<- ago_hr_num / total_hr
  return(css)
}

find_ago_HR_ratio_v2 <- function(css) {
  ago_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% ago_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  ago_hr_ratio <<- ago_hr_num / total_hr
  return(css)
}

find_ago_Finance_ratio <- function(css) {
  ago_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% ago_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  ago_finance_ratio <<- ago_finance_num / total_f
  return(css)
}

find_ago_Finance_ratio_v2 <- function(css) {
  ago_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% ago_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  ago_finance_ratio <<- ago_finance_num / total_f
  return(css)
}

find_dfe_HR_ratio <- function(css) {
  dfe_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dfe_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dfe_hr_ratio <<- dfe_hr_num / total_hr
  return(css)
}

find_dfe_HR_ratio_v2 <- function(css) {
  dfe_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dfe_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dfe_hr_ratio <<- dfe_hr_num / total_hr
  return(css)
}

find_dfe_Finance_ratio <- function(css) {
  dfe_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dfe_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dfe_finance_ratio <<- dfe_finance_num / total_f
  return(css)
}


find_dfe_Finance_ratio_v2 <- function(css) {
  dfe_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dfe_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dfe_finance_ratio <<- dfe_finance_num / total_f
  return(css)
}

find_hmt_HR_ratio <- function(css) {
  hmt_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmt_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  hmt_hr_ratio <<- hmt_hr_num / total_hr
  return(css)
}

find_hmt_HR_ratio_v2 <- function(css) {
  hmt_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmt_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  hmt_hr_ratio <<- hmt_hr_num / total_hr
  return(css)
}

find_hmt_Finance_ratio <- function(css) {
  hmt_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmt_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  hmt_finance_ratio <<- hmt_finance_num / total_f
  return(css)
}

find_hmt_Finance_ratio_v2 <- function(css) {
  hmt_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmt_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  hmt_finance_ratio <<- hmt_finance_num / total_f
  return(css)
}

find_co_HR_ratio <- function(css) {
  co_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% co_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  co_hr_ratio <<- co_hr_num / total_hr
  return(css)
}

find_co_HR_ratio_v2 <- function(css) {
  co_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% co_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  co_hr_ratio <<- co_hr_num / total_hr
  return(css)
}

find_co_Finance_ratio <- function(css) {
  co_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% co_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  co_finance_ratio <<- co_finance_num / total_f
  return(css)
}

find_co_Finance_ratio_v2 <- function(css) {
  co_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% co_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  co_finance_ratio <<- co_finance_num / total_f
  return(css)
}




### 1 of 8 ###

Observation <- 1 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2017 <- read_excel("CSS/css2017.xls", sheet = 10)

matrix_2017 <- css2017 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  matrix_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_beis_HR_ratio() %>% 
  find_beis_Finance_ratio() %>% 
  find_dit_HR_ratio() %>% 
  find_dit_Finance_ratio() %>% 
  find_dcms_HR_ratio() %>% 
  find_dcms_Finance_ratio() %>% 
  find_health_HR_ratio() %>% 
  find_health_Finance_ratio() %>% 
  find_ago_HR_ratio() %>% 
  find_ago_Finance_ratio() %>% 
  find_dfe_HR_ratio() %>% 
  find_dfe_Finance_ratio() %>% 
  find_hmt_HR_ratio() %>% 
  find_hmt_Finance_ratio() %>% 
  find_co_HR_ratio() %>% 
  find_co_Finance_ratio() 

HR_ratios <- array(1:8)
HR_ratios[1] <- beis_hr_ratio
HR_ratios[2] <- dit_hr_ratio
HR_ratios[3] <- dcms_hr_ratio
HR_ratios[4] <- health_hr_ratio
HR_ratios[5] <- ago_hr_ratio
HR_ratios[6] <- dfe_hr_ratio
HR_ratios[7] <- hmt_hr_ratio
HR_ratios[8] <- co_hr_ratio

f_ratios <- array(1:8)
f_ratios[1] <- beis_finance_ratio
f_ratios[2] <- dit_finance_ratio
f_ratios[3] <- dcms_finance_ratio
f_ratios[4] <- health_finance_ratio
f_ratios[5] <- ago_finance_ratio
f_ratios[6] <- dfe_finance_ratio
f_ratios[7] <- hmt_finance_ratio
f_ratios[8] <- co_finance_ratio

summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))


### 2 of 8 ###

Observation <- 5 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2018 <- read_excel("CSS/css2018_v2.xlsx", sheet = 10) # double check which page has info!

matrix_2018 <- css2018 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  matrix_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_beis_HR_ratio() %>% 
  find_beis_Finance_ratio() %>% 
  find_dit_HR_ratio() %>% 
  find_dit_Finance_ratio() %>% 
  find_dcms_HR_ratio() %>% 
  find_dcms_Finance_ratio() %>% 
  find_health_HR_ratio() %>% 
  find_health_Finance_ratio() %>% 
  find_ago_HR_ratio() %>% 
  find_ago_Finance_ratio() %>% 
  find_dfe_HR_ratio() %>% 
  find_dfe_Finance_ratio() %>% 
  find_hmt_HR_ratio() %>% 
  find_hmt_Finance_ratio() %>% 
  find_co_HR_ratio() %>% 
  find_co_Finance_ratio() 

HR_ratios <- array(1:8)
HR_ratios[1] <- beis_hr_ratio
HR_ratios[2] <- dit_hr_ratio
HR_ratios[3] <- dcms_hr_ratio
HR_ratios[4] <- health_hr_ratio
HR_ratios[5] <- ago_hr_ratio
HR_ratios[6] <- dfe_hr_ratio
HR_ratios[7] <- hmt_hr_ratio
HR_ratios[8] <- co_hr_ratio

f_ratios <- array(1:8)
f_ratios[1] <- beis_finance_ratio
f_ratios[2] <- dit_finance_ratio
f_ratios[3] <- dcms_finance_ratio
f_ratios[4] <- health_finance_ratio
f_ratios[5] <- ago_finance_ratio
f_ratios[6] <- dfe_finance_ratio
f_ratios[7] <- hmt_finance_ratio
f_ratios[8] <- co_finance_ratio

summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))



### 3 of 8 ###

Observation <- 9 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2019 <- read_excel("CSS/css2019.xlsx", sheet = 10) # double check which page has info!

matrix_2019 <- css2019 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  matrix_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_beis_HR_ratio() %>% 
  find_beis_Finance_ratio() %>% 
  find_dit_HR_ratio() %>% 
  find_dit_Finance_ratio() %>% 
  find_dcms_HR_ratio() %>% 
  find_dcms_Finance_ratio() %>% 
  find_health_HR_ratio() %>% 
  find_health_Finance_ratio() %>% 
  find_ago_HR_ratio() %>% 
  find_ago_Finance_ratio() %>% 
  find_dfe_HR_ratio() %>% 
  find_dfe_Finance_ratio() %>% 
  find_hmt_HR_ratio() %>% 
  find_hmt_Finance_ratio() %>% 
  find_co_HR_ratio() %>% 
  find_co_Finance_ratio() 

HR_ratios <- array(1:8)
HR_ratios[1] <- beis_hr_ratio
HR_ratios[2] <- dit_hr_ratio
HR_ratios[3] <- dcms_hr_ratio
HR_ratios[4] <- health_hr_ratio
HR_ratios[5] <- ago_hr_ratio
HR_ratios[6] <- dfe_hr_ratio
HR_ratios[7] <- hmt_hr_ratio
HR_ratios[8] <- co_hr_ratio

f_ratios <- array(1:8)
f_ratios[1] <- beis_finance_ratio
f_ratios[2] <- dit_finance_ratio
f_ratios[3] <- dcms_finance_ratio
f_ratios[4] <- health_finance_ratio
f_ratios[5] <- ago_finance_ratio
f_ratios[6] <- dfe_finance_ratio
f_ratios[7] <- hmt_finance_ratio
f_ratios[8] <- co_finance_ratio

summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))



### 4 of 8 ###

Observation <- 13 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2020 <- read_excel("CSS/css2020.xlsx", sheet = 10) # double check which page has info!

matrix_2020 <- css2020 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  matrix_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_beis_HR_ratio() %>% 
  find_beis_Finance_ratio() %>% 
  find_dit_HR_ratio() %>% 
  find_dit_Finance_ratio() %>% 
  find_dcms_HR_ratio() %>% 
  find_dcms_Finance_ratio() %>% 
  find_health_HR_ratio() %>% 
  find_health_Finance_ratio() %>% 
  find_ago_HR_ratio() %>% 
  find_ago_Finance_ratio() %>% 
  find_dfe_HR_ratio() %>% 
  find_dfe_Finance_ratio() %>% 
  find_hmt_HR_ratio() %>% 
  find_hmt_Finance_ratio() %>% 
  find_co_HR_ratio() %>% 
  find_co_Finance_ratio() 

HR_ratios <- array(1:8)
HR_ratios[1] <- beis_hr_ratio
HR_ratios[2] <- dit_hr_ratio
HR_ratios[3] <- dcms_hr_ratio
HR_ratios[4] <- health_hr_ratio
HR_ratios[5] <- ago_hr_ratio
HR_ratios[6] <- dfe_hr_ratio
HR_ratios[7] <- hmt_hr_ratio
HR_ratios[8] <- co_hr_ratio

f_ratios <- array(1:8)
f_ratios[1] <- beis_finance_ratio
f_ratios[2] <- dit_finance_ratio
f_ratios[3] <- dcms_finance_ratio
f_ratios[4] <- health_finance_ratio
f_ratios[5] <- ago_finance_ratio
f_ratios[6] <- dfe_finance_ratio
f_ratios[7] <- hmt_finance_ratio
f_ratios[8] <- co_finance_ratio

summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))

### 5 of 8 ###

Observation <- 17 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2021 <- read_excel("CSS/css2021.xlsx", sheet = 10) # double check which page has info!

matrix_2021 <- css2021 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  matrix_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_beis_HR_ratio() %>% 
  find_beis_Finance_ratio() %>% 
  find_dit_HR_ratio() %>% 
  find_dit_Finance_ratio() %>% 
  find_dcms_HR_ratio() %>% 
  find_dcms_Finance_ratio() %>% 
  find_health_HR_ratio() %>% 
  find_health_Finance_ratio() %>% 
  find_ago_HR_ratio() %>% 
  find_ago_Finance_ratio() %>% 
  find_dfe_HR_ratio() %>% 
  find_dfe_Finance_ratio() %>% 
  find_hmt_HR_ratio() %>% 
  find_hmt_Finance_ratio() %>% 
  find_co_HR_ratio() %>% 
  find_co_Finance_ratio() 

HR_ratios <- array(1:8)
HR_ratios[1] <- beis_hr_ratio
HR_ratios[2] <- dit_hr_ratio
HR_ratios[3] <- dcms_hr_ratio
HR_ratios[4] <- health_hr_ratio
HR_ratios[5] <- ago_hr_ratio
HR_ratios[6] <- dfe_hr_ratio
HR_ratios[7] <- hmt_hr_ratio
HR_ratios[8] <- co_hr_ratio

f_ratios <- array(1:8)
f_ratios[1] <- beis_finance_ratio
f_ratios[2] <- dit_finance_ratio
f_ratios[3] <- dcms_finance_ratio
f_ratios[4] <- health_finance_ratio
f_ratios[5] <- ago_finance_ratio
f_ratios[6] <- dfe_finance_ratio
f_ratios[7] <- hmt_finance_ratio
f_ratios[8] <- co_finance_ratio

summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))



### 6 of 8 ###

Observation <- 21 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2022 <- read_ods("CSS/css2022.ods", sheet = 13) # double check which page has info!

matrix_2022 <- css2022 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% ## CSS layout schema has changed! Need to use v2 here. 
  css_cols() %>% 
  css_to_num() %>% 
  matrix_filter_v2() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_beis_HR_ratio_v2() %>% 
  find_beis_Finance_ratio_v2() %>% 
  find_dit_HR_ratio_v2() %>% 
  find_dit_Finance_ratio_v2() %>% 
  find_dcms_HR_ratio_v2() %>% 
  find_dcms_Finance_ratio_v2() %>% 
  find_health_HR_ratio_v2() %>% 
  find_health_Finance_ratio_v2() %>% 
  find_ago_HR_ratio_v2() %>% 
  find_ago_Finance_ratio_v2() %>% 
  find_dfe_HR_ratio_v2() %>% 
  find_dfe_Finance_ratio_v2() %>% 
  find_hmt_HR_ratio_v2() %>% 
  find_hmt_Finance_ratio_v2() %>% 
  find_co_HR_ratio_v2() %>% 
  find_co_Finance_ratio_v2() 

HR_ratios <- array(1:8)
HR_ratios[1] <- beis_hr_ratio
HR_ratios[2] <- dit_hr_ratio
HR_ratios[3] <- dcms_hr_ratio
HR_ratios[4] <- health_hr_ratio
HR_ratios[5] <- ago_hr_ratio
HR_ratios[6] <- dfe_hr_ratio
HR_ratios[7] <- hmt_hr_ratio
HR_ratios[8] <- co_hr_ratio

f_ratios <- array(1:8)
f_ratios[1] <- beis_finance_ratio
f_ratios[2] <- dit_finance_ratio
f_ratios[3] <- dcms_finance_ratio
f_ratios[4] <- health_finance_ratio
f_ratios[5] <- ago_finance_ratio
f_ratios[6] <- dfe_finance_ratio
f_ratios[7] <- hmt_finance_ratio
f_ratios[8] <- co_finance_ratio

summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))

### 7 of 8 ###

Observation <- 25 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2023 <- read_ods("CSS/css2023.ods", sheet = 14) # double check which page has info!

matrix_2023 <- css2023 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% ## CSS layout schema has changed! Need to use v2 here. 
  css_cols() %>% # CSS did not update dept for ESNZ etc. in this edition; doesn't need to be included.
  css_to_num() %>% 
  matrix_filter_v2() %>% #v2 needed
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_beis_HR_ratio_v2() %>% 
  find_beis_Finance_ratio_v2() %>% 
  find_dit_HR_ratio_v2() %>% 
  find_dit_Finance_ratio_v2() %>% 
  find_dcms_HR_ratio_v2() %>% 
  find_dcms_Finance_ratio_v2() %>% 
  find_health_HR_ratio_v2() %>% 
  find_health_Finance_ratio_v2() %>% 
  find_ago_HR_ratio_v2() %>% 
  find_ago_Finance_ratio_v2() %>% 
  find_dfe_HR_ratio_v2() %>% 
  find_dfe_Finance_ratio_v2() %>% 
  find_hmt_HR_ratio_v2() %>% 
  find_hmt_Finance_ratio_v2() %>% 
  find_co_HR_ratio_v2() %>% 
  find_co_Finance_ratio_v2() 

HR_ratios <- array(1:8)
HR_ratios[1] <- beis_hr_ratio
HR_ratios[2] <- dit_hr_ratio
HR_ratios[3] <- dcms_hr_ratio
HR_ratios[4] <- health_hr_ratio
HR_ratios[5] <- ago_hr_ratio
HR_ratios[6] <- dfe_hr_ratio
HR_ratios[7] <- hmt_hr_ratio
HR_ratios[8] <- co_hr_ratio

f_ratios <- array(1:8)
f_ratios[1] <- beis_finance_ratio
f_ratios[2] <- dit_finance_ratio
f_ratios[3] <- dcms_finance_ratio
f_ratios[4] <- health_finance_ratio
f_ratios[5] <- ago_finance_ratio
f_ratios[6] <- dfe_finance_ratio
f_ratios[7] <- hmt_finance_ratio
f_ratios[8] <- co_finance_ratio

summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))

### 8 of 8 ###

Observation <- 29 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2024 <- read_ods("CSS/css2024.ods", sheet = 13) # double check which page has info!

matrix_2024 <- css2024 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% ## CSS layout schema has changed! Need to use v2 here. 
  css_cols() %>% # CSS did not update dept for ESNZ etc. in this edition; doesn't need to be included.
  css_to_num() %>% 
  matrix_filter_v2() %>% #v2 needed
  find_total_HR() %>% # whilst new matrix_2024 will show an extra department, in the ratios calculations they are grouped. 
  find_total_Finance() %>% 
  find_beis_HR_ratio_v2() %>% 
  find_beis_Finance_ratio_v2() %>% 
  find_dit_HR_ratio_v2() %>% 
  find_dit_Finance_ratio_v2() %>% 
  find_dcms_HR_ratio_v2() %>% 
  find_dcms_Finance_ratio_v2() %>% 
  find_health_HR_ratio_v2() %>% 
  find_health_Finance_ratio_v2() %>% 
  find_ago_HR_ratio_v2() %>% 
  find_ago_Finance_ratio_v2() %>% 
  find_dfe_HR_ratio_v2() %>% 
  find_dfe_Finance_ratio_v2() %>% 
  find_hmt_HR_ratio_v2() %>% 
  find_hmt_Finance_ratio_v2() %>% 
  find_co_HR_ratio_v2() %>% 
  find_co_Finance_ratio_v2() 

HR_ratios <- array(1:8)
HR_ratios[1] <- beis_hr_ratio
HR_ratios[2] <- dit_hr_ratio
HR_ratios[3] <- dcms_hr_ratio
HR_ratios[4] <- health_hr_ratio
HR_ratios[5] <- ago_hr_ratio
HR_ratios[6] <- dfe_hr_ratio
HR_ratios[7] <- hmt_hr_ratio
HR_ratios[8] <- co_hr_ratio

f_ratios <- array(1:8)
f_ratios[1] <- beis_finance_ratio
f_ratios[2] <- dit_finance_ratio
f_ratios[3] <- dcms_finance_ratio
f_ratios[4] <- health_finance_ratio
f_ratios[5] <- ago_finance_ratio
f_ratios[6] <- dfe_finance_ratio
f_ratios[7] <- hmt_finance_ratio
f_ratios[8] <- co_finance_ratio

summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Matrix' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))



### NATURAL CUBIC SPLINE INTERPOLATION

# Adding new HR data

x <- c(1:29)
y <- completedata$HR_q1_Concentration[1:29]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "HR Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


matrix_indices <- which(completedata$Cluster == 'Matrix')

completedata <- completedata %>%
  mutate(HR_q1_concentration_ncsi = NA)

completedata <- completedata %>%
  mutate(HR_q1_concentration_ncsi = ifelse(Cluster == 'Matrix', NA, HR_q1_concentration_ncsi))

completedata$HR_q1_concentration_ncsi[matrix_indices] <- spline_values


# Adding new finance data


x <- c(1:29)
y <- completedata$Finance_q1_Concentration[1:29]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "Finance Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


matrix_indices <- which(completedata$Cluster == 'Matrix')

completedata <- completedata %>%
  mutate(Finance_q1_concentration_ncsi = NA)

completedata <- completedata %>%
  mutate(Finance_q1_concentration_ncsi = ifelse(Cluster == 'Matrix', NA, Finance_q1_concentration_ncsi))

completedata$Finance_q1_concentration_ncsi[matrix_indices] <- spline_values




# WRITE TO DATAFRAME

write_xlsx(completedata, "outputs/results_matrix_prof_ratios.xlsx")


