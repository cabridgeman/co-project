rm(list=ls())
options(scipen=999)

library(readxl)
library(dplyr)
library(magrittr)
library(scales)
library(ggplot2)
library(readODS)
library(writexl)

## SYNERGY: 
# DEFRA
# DWP
# HO
# MoJ 

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

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0


#### FUNCTIONS ####
all_rdel <- function(oscar) {
  oscar %>%
    select('Segment Department Long Name', 'Organisation', 'Control Budget Code', 'Control Budget Detail Code', 'Economic Budget Code', 'PESA Economic Group Code', 'Version', 'Quarter', 'Year', 'Amount', 'Sub Segment Long Name') %>%
    filter(`Control Budget Code` == 'DEL' & `Economic Budget Code` == 'RESOURCE' & `Version` == 'OUTTURN')
}

all_rdel_v2 <- function(oscar) {
  oscar %>%
    select('Segment Department Long Name', 'Organisation', 'Control Budget Code', 'Control Budget Detail Code', 'Economic Budget Code', 'PESA Economic Group Code', 'Version', 'Quarter', 'Year', 'Amount', 'Sub Segment Long Name') %>%
    filter(`Control Budget Code` == 'DEL' & `Economic Budget Code` == 'RESOURCE' & `Version` == 'RETURN12_APR')
}

all_rdel_v3 <- function(oscar) {
  oscar %>%
    select('Department', 'Organisation Name', 'Control Budget', 'Control Budget Detail', 'Economic Budget', 'PESA Economic Group Code', 'Version Snapshot', 'Quarter', 'Year', 'Amount', 'Sub-segment Name') %>%
    filter(`Control Budget` == 'DEL' & `Economic Budget` == 'RESOURCE' & `Version Snapshot` == 'MFO_202122_R13_v1')
}

all_rdel_v4 <- function(oscar) {
  oscar %>%
    select('Segment Department Long Name', 'Organisation', 'Control Budget Code', 'Control Budget Detail Code', 'Economic Budget Code', 'PESA Economic Group Code', 'Version', 'Quarter', 'Year', 'Amount', 'Sub Segment Long Name') %>%
    filter(`Control Budget Code` == 'TOTAL DEL' & `Economic Budget Code` == 'RESOURCE' & `Version` == 'MFO_2022-23_R13_v1')
}

all_rdel_v5 <- function(oscar) {
  oscar %>%
    select('Segment Department Long Name', 'Organisation', 'Control Budget Code', 'Control Budget Detail Code', 'Economic Budget Code', 'PESA Economic Group Code', 'Version', 'Quarter', 'Year', 'Amount', 'Sub Segment Long Name') %>%
    filter(`Control Budget Code` == 'TOTAL DEL' & `Economic Budget Code` == 'RESOURCE' & `Version` == 'MFO_2023-24_R13_v1')
}

only_synergy <- function(oscar) {
  oscar %>%
    select(everything()) %>%
    filter(`Segment Department Long Name` == 'Department for Environment, Food and Rural Affairs' | `Segment Department Long Name` == 'Department for Work and Pensions' | `Segment Department Long Name` ==  'Home Office'  | `Segment Department Long Name` ==  'Ministry of Justice')
}

only_synergy_v2 <- function(oscar) {
  oscar %>%
    select(everything()) %>%
    filter(`Department` == 'Department for Environment, Food and Rural Affairs' | `Department` == 'Department for Work and Pensions' | `Department` ==  'Home Office'  | `Department` ==  'Ministry of Justice')
}

all_admin_rdel <- function(oscar) {
  oscar_temp <- oscar %>%
    select(everything()) %>%
    filter(`Control Budget Detail Code` == 'DEL ADMIN')
  all_admin_rdel_total <<- find_total(oscar_temp)
  return(oscar) # need to return dataframe again at end because R functions always return last variable, which here is not the dataframe.
}

all_admin_rdel_v2 <- function(oscar) {
  oscar_temp <- oscar %>%
    select(everything()) %>%
    filter(`Control Budget Detail` == 'DEL ADMIN') 
  all_admin_rdel_total <<- find_total(oscar_temp)
  return(oscar) # need to return dataframe again at end because R functions always return last variable, which here is not the dataframe.
}


all_prog_rdel <- function(oscar) {
  oscar_temp <- oscar %>%
    select(everything()) %>%
    filter(`Control Budget Detail Code` == 'DEL PROG')
  all_prog_rdel_total <<- find_total(oscar_temp)
  return(oscar)
}

all_prog_rdel_v2 <- function(oscar) {
  oscar_temp <- oscar %>%
    select(everything()) %>%
    filter(`Control Budget Detail` == 'DEL PROG')
  all_prog_rdel_total <<- find_total(oscar_temp)
  return(oscar)
}



all_admin_staff_costs_only <- function(oscar) {
  oscar_temp <- oscar %>%
    select(everything()) %>%
    filter(`Control Budget Detail Code` == 'DEL ADMIN' & `PESA Economic Group Code` == 'STAFF COSTS')
  all_admin_staff_total <<- find_total(oscar_temp)
  return(oscar)
}

all_admin_staff_costs_only_v2 <- function(oscar) {
  oscar_temp <- oscar %>%
    select(everything()) %>%
    filter(`Control Budget Detail` == 'DEL ADMIN' & `PESA Economic Group Code` == 'STAFF COSTS')
  all_admin_staff_total <<- find_total(oscar_temp)
  return(oscar)
}


all_prog_staff_costs_only <- function(oscar) {
  oscar_temp <- oscar %>%
    select(everything()) %>%
    filter(`Control Budget Detail Code` == 'DEL PROG' & `PESA Economic Group Code` == 'STAFF COSTS')
  all_prog_staff_total <<- find_total(oscar_temp)
  return(oscar)
}

all_prog_staff_costs_only_v2 <- function(oscar) {
  oscar_temp <- oscar %>%
    select(everything()) %>%
    filter(`Control Budget Detail` == 'DEL PROG' & `PESA Economic Group Code` == 'STAFF COSTS')
  all_prog_staff_total <<- find_total(oscar_temp)
  return(oscar)
}


specify_quarter <- function(oscar, qtr, year) {
  oscar %>%
    select(everything()) %>%
    filter(grepl(qtr, `Quarter`) & `Year` == !!year)
}


find_total <- function(oscar_temp) {
  total <- oscar_temp %>%
    filter(`Amount` > 0) %>% 
    summarise(total = sum(`Amount`, na.rm = TRUE)) %>%
    pull(total) 
}

calculate_intensity_logs <- function(oscar, Num) {
  
  completedata <<- completedata %>% 
    mutate(admin_running_costs = ifelse(Cluster=='Synergy' & Num==Observation, round((all_admin_rdel_total - all_admin_staff_total)), admin_running_costs))
  
  completedata <<- completedata %>% 
    mutate(admin_total = ifelse(Cluster=='Synergy' & Num==Observation, round(all_admin_rdel_total), admin_total))
  
  completedata <<- completedata %>% 
    mutate(prog_total = ifelse(Cluster=='Synergy' & Num==Observation, round(all_prog_rdel_total), prog_total))
  
  completedata <<- completedata %>% 
    mutate(admin_staff = ifelse(Cluster=='Synergy' & Num==Observation, round(all_admin_staff_total), admin_staff))
  
  completedata <<- completedata %>% 
    mutate(prog_staff = ifelse(Cluster=='Synergy' & Num==Observation, round(all_prog_staff_total), prog_staff))
  
  running_costs <<- log(((all_admin_rdel_total - all_admin_staff_total) / (all_admin_rdel_total + all_prog_rdel_total)) * 100)
  
  completedata <<- completedata %>%
    mutate(ln_Admin_Running_Cost_Intensity = ifelse(Cluster=='Synergy' & Num==Observation, running_costs, ln_Admin_Running_Cost_Intensity))
  
  admin_staff_costs <<- log((all_admin_staff_total / (all_admin_staff_total + all_prog_staff_total)) * 100)
  
  completedata <<- completedata %>%
    mutate(ln_Admin_Staff_Intensity = ifelse(Cluster=='Synergy' & Num==Observation, admin_staff_costs, ln_Admin_Staff_Intensity))
  
  total_admin_intensity <<- log((all_admin_rdel_total / (all_admin_rdel_total + all_prog_rdel_total)) * 100)
  
  completedata <<- completedata %>%
    mutate(ln_Total_Admin_Intensity = ifelse(Cluster=='Synergy' & Num==Observation, total_admin_intensity, ln_Total_Admin_Intensity))
  
  print(oscar_version)
  print(paste('Natural log of admin running cost intensity:', running_costs))
  print(paste('Natural log of admin staff cost intensity:', admin_staff_costs))
  print(paste('Natural log of total admin intensity:', total_admin_intensity))
  
  return(oscar)
}


#### PIPELINES #### 

### 1 of 29 ###
oscar2017b <- read_excel("OSCAR/oscar2017b.xlsx", sheet = 2) # Initial load

## checks ## (only need to do this once per '20xxb' dataframe, as 20xxb's have 4 quarters)
colnames(oscar2017b) # Ensure column names are same as used in standard functions
glimpse(oscar2017b) # Ensure structure of Segment Department Long Name, Control Budget Code, Control Budget Detail Code, Economic Budget Code, Version, PESA Economic Group Code, Quarter, Year are same as in standard functions
unique(oscar2017b$`Organisation`) # Ensure naming scheme matches standard functions; have any other departments been added to cluster this period?

## transformation ##
Observation <- 1
pipeline_version <- '2017b'
pipeline_quarter <- 'Qtr4'
pipeline_year <- '2016-17'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2017b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 2 of 29 ###
oscar2018b <- read_excel("OSCAR/oscar2018b.xlsx", sheet = 2) # Initial load

## checks ## (only need to do this once per '20xxb' dataframe, as 20xxb's have 4 quarters)
colnames(oscar2018b) 
glimpse(oscar2018b) 
unique(oscar2018b$`Organisation`) 

## transformation ##
Observation <- 2
pipeline_version <- '2018b'
pipeline_quarter <- 'Qtr1'
pipeline_year <- '2017-18'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2018b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 3 of 29 ###

## transformation ##
Observation <- 3
pipeline_version <- '2018b'
pipeline_quarter <- 'Qtr2'
pipeline_year <- '2017-18'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2018b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0


### 4 of 29 ###

## transformation ##
Observation <- 4
pipeline_version <- '2018b'
pipeline_quarter <- 'Qtr3'
pipeline_year <- '2017-18'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2018b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 5 of 29 ###

## transformation ##
Observation <- 5
pipeline_version <- '2018b'
pipeline_quarter <- 'Qtr4'
pipeline_year <- '2017-18'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2018b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 6 of 29 ###
oscar2019b <- read_ods("OSCAR/oscar2019b.ods", sheet = 2) # Initial load

## checks ## (only need to do this once per '20xxb' dataframe, as 20xxb's have 4 quarters)
colnames(oscar2019b) 
glimpse(oscar2019b) 
unique(oscar2019b$`Organisation`) 

## transformation ##
Observation <- 6
pipeline_version <- '2019b'
pipeline_quarter <- 'Qtr1'
pipeline_year <- '2018-19'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2019b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 7 of 29 ###

## transformation ##
Observation <- 7
pipeline_version <- '2019b'
pipeline_quarter <- 'Qtr2'
pipeline_year <- '2018-19'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2019b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 8 of 29 ###

## transformation ##
Observation <- 8
pipeline_version <- '2019b'
pipeline_quarter <- 'Qtr3'
pipeline_year <- '2018-19'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2019b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 9 of 29 ###

## transformation ##
Observation <- 9
pipeline_version <- '2019b'
pipeline_quarter <- 'Qtr4'
pipeline_year <- '2018-19'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2019b %>%
  all_rdel() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 10 of 29 ###
oscar2020b <- read_ods("OSCAR/oscar2020b.ods", sheet = 2) # Initial load

## checks ## (only need to do this once per '20xxb' dataframe, as 20xxb's have 4 quarters)
colnames(oscar2020b) 
glimpse(oscar2020b) # version format changed, use all_rdel_v2
unique(oscar2020b$`Organisation`) 

## transformation ##
Observation <- 10
pipeline_version <- '2020b'
pipeline_quarter <- 'Qtr1'
pipeline_year <- '2019-20'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2020b %>%
  all_rdel_v2() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 11 of 29 ###

## transformation ##
Observation <- 11
pipeline_version <- '2020b'
pipeline_quarter <- 'Qtr2'
pipeline_year <- '2019-20'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2020b %>%
  all_rdel_v2() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 12 of 29 ###

## transformation ##
Observation <- 12
pipeline_version <- '2020b'
pipeline_quarter <- 'Qtr3'
pipeline_year <- '2019-20'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2020b %>%
  all_rdel_v2() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 13 of 29 ###

## transformation ##
Observation <- 13
pipeline_version <- '2020b'
pipeline_quarter <- 'Qtr4'
pipeline_year <- '2019-20'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2020b %>%
  all_rdel_v2() %>% 
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 14 of 29 ###
oscar2021b <- read_ods("OSCAR/oscar2021b.ods", sheet = 2) # Initial load

## checks ## (only need to do this once per '20xxb' dataframe, as 20xxb's have 4 quarters)
colnames(oscar2021b) 
glimpse(oscar2021b) # version changed back, use original rdel
unique(oscar2021b$`Organisation`) 

## transformation ##
Observation <- 14
pipeline_version <- '2021b'
pipeline_quarter <- 'Qtr1'
pipeline_year <- '2020-21'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2021b %>%
  all_rdel() %>% # version revert format
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 15 of 29 ###

## transformation ##
Observation <- 15
pipeline_version <- '2021b'
pipeline_quarter <- 'Qtr2'
pipeline_year <- '2020-21'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2021b %>%
  all_rdel() %>% # version revert format
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 16 of 29 ###

## transformation ##
Observation <- 16
pipeline_version <- '2021b'
pipeline_quarter <- 'Qtr3'
pipeline_year <- '2020-21'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2021b %>%
  all_rdel() %>% # version revert format
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 17 of 29 ###

## transformation ##
Observation <- 17
pipeline_version <- '2021b'
pipeline_quarter <- 'Qtr4'
pipeline_year <- '2020-21'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2021b %>%
  all_rdel() %>% # version revert format
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>% 
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0


### 18 of 29 ###
oscar2022b <- read_ods("OSCAR/oscar2022b.ods", sheet = 2) # Initial load

## checks ## (only need to do this once per '20xxb' dataframe, as 20xxb's have 4 quarters)
colnames(oscar2022b) 
glimpse(oscar2022b) 
unique(oscar2022b$`Organisation Name`) 

## transformation ##
Observation <- 18
pipeline_version <- '2022b'
pipeline_quarter <- 'Qtr1'
pipeline_year <- '2021-22'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2022b %>%
  all_rdel_v3() %>% # version revert format
  only_synergy_v2() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel_v2() %>%  # %>% # need to use v2 of all these, as 'control budget detail' has updated.
  all_prog_rdel_v2() %>% 
  all_admin_staff_costs_only_v2() %>% 
  all_prog_staff_costs_only_v2() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 19 of 29 ###

## transformation ##
Observation <- 19
pipeline_version <- '2022b'
pipeline_quarter <- 'Qtr2'
pipeline_year <- '2021-22'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2022b %>%
  all_rdel_v3() %>% # version revert format
  only_synergy_v2() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel_v2() %>%  # %>% # need to use v2 of all these, as 'control budget detail' has updated.
  all_prog_rdel_v2() %>% 
  all_admin_staff_costs_only_v2() %>% 
  all_prog_staff_costs_only_v2() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 20 of 29 ###

## transformation ##
Observation <- 20
pipeline_version <- '2022b'
pipeline_quarter <- 'Qtr3'
pipeline_year <- '2021-22'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2022b %>%
  all_rdel_v3() %>% # version revert format
  only_synergy_v2() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel_v2() %>%  # %>% # need to use v2 of all these, as 'control budget detail' has updated.
  all_prog_rdel_v2() %>% 
  all_admin_staff_costs_only_v2() %>% 
  all_prog_staff_costs_only_v2() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 21 of 29 ###

## transformation ##
Observation <- 21
pipeline_version <- '2022b'
pipeline_quarter <- 'Qtr4'
pipeline_year <- '2021-22'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2022b %>%
  all_rdel_v3() %>% # version revert format
  only_synergy_v2() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel_v2() %>%  # %>% # need to use v2 of all these, as 'control budget detail' has updated.
  all_prog_rdel_v2() %>% 
  all_admin_staff_costs_only_v2() %>% 
  all_prog_staff_costs_only_v2() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 22 of 29 ###
oscar2023b <- read_ods("OSCAR/oscar2023b.ods", sheet = 2) # Initial load

## checks ## (only need to do this once per '20xxb' dataframe, as 20xxb's have 4 quarters)
colnames(oscar2023b) 
glimpse(oscar2023b) 
unique(oscar2023b$`Organisation`) 

## transformation ##
Observation <- 22
pipeline_version <- '2023b'
pipeline_quarter <- 'Qtr1'
pipeline_year <- '2022-23'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2023b %>%
  all_rdel_v4() %>% # updated to v4, del is now 'total del', and version updated.
  only_synergy() %>% # can revert back to initial filter/function
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>%  # no suffixes here
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 23 of 29 ###

## transformation ##
Observation <- 23
pipeline_version <- '2023b'
pipeline_quarter <- 'Qtr2'
pipeline_year <- '2022-23'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2023b %>%
  all_rdel_v4() %>% # updated to v4, del is now 'total del', and version updated.
  only_synergy() %>% # can revert back to initial filter/function
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>%  # no suffixes here
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 24 of 29 ###

## transformation ##
Observation <- 24
pipeline_version <- '2023b'
pipeline_quarter <- 'Qtr3'
pipeline_year <- '2022-23'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2023b %>%
  all_rdel_v4() %>% # updated to v4, del is now 'total del', and version updated.
  only_synergy() %>% # can revert back to initial filter/function
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>%  # no suffixes here
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 25 of 29 ###

## transformation ##
Observation <- 25
pipeline_version <- '2023b'
pipeline_quarter <- 'Qtr4'
pipeline_year <- '2022-23'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2023b %>%
  all_rdel_v4() %>% # updated to v4, del is now 'total del', and version updated.
  only_synergy() %>% # can revert back to initial filter/function
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>%  # no suffixes here
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 26 of 29 ###
oscar2024b <- read_ods("OSCAR/oscar2024b.ods", sheet = 2) # Initial load

## checks ## (only need to do this once per '20xxb' dataframe, as 20xxb's have 4 quarters)
colnames(oscar2024b) 
glimpse(oscar2024b) 
unique(oscar2024b$`Organisation`) 

## transformation ##
Observation <- 26
pipeline_version <- '2024b'
pipeline_quarter <- 'Qtr1'
pipeline_year <- '2023-24'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2024b %>%
  all_rdel_v5() %>% # updated to v5 for new version code
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>%  
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0


### 27 of 29 ###

## transformation ##
Observation <- 27
pipeline_version <- '2024b'
pipeline_quarter <- 'Qtr2'
pipeline_year <- '2023-24'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2024b %>%
  all_rdel_v5() %>% # updated to v5 for new version code
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>%  
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 28 of 29 ###

## transformation ##
Observation <- 28
pipeline_version <- '2024b'
pipeline_quarter <- 'Qtr3'
pipeline_year <- '2023-24'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2024b %>%
  all_rdel_v5() %>% # updated to v5 for new version code
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>%  
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0

### 29 of 29 ###

## transformation ##
Observation <- 29
pipeline_version <- '2024b'
pipeline_quarter <- 'Qtr4'
pipeline_year <- '2023-24'

oscar_version <- paste0('oscar', pipeline_version)

temp_oscar <- oscar2024b %>%
  all_rdel_v5() %>% # updated to v5 for new version code
  only_synergy() %>% 
  specify_quarter(., pipeline_quarter, pipeline_year) %>% 
  all_admin_rdel() %>%  
  all_prog_rdel() %>% 
  all_admin_staff_costs_only() %>% 
  all_prog_staff_costs_only() 

temp_oscar <- temp_oscar %>% 
  calculate_intensity_logs(., Observation)

running_costs <- 0
admin_staff_costs <- 0
total_admin_intensity <- 0
temp_oscar <- 0
Observation <- 0


#### PIPELINES DONE ####



write_xlsx(completedata, "outputs/results_synergy_admin_intensities.xlsx")

