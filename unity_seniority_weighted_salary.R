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


### unity

## FILTERS FOR: 
# SENIOR CIVIL SERVANT, G6/7, SENIOR HIGHER EXECUTIVE OFFICER, EXECUTIVE OFFICER, ADMINISTRATIVE OFFICERS AND ASSISTANTS


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

hmrc_orgs <- c('HM Revenue and Customs',
               'Valuation Office Agency', 'HM Revenue and Customs (excl. agencies)')


dft_orgs <- c('Department for Transport (excl. agencies)',
              'Driver and Vehicle Licensing Agency',
              'Driver and Vehicle Standards Agency',
              'Maritime and Coastguard Agency',
              'Vehicle Certification Agency'
)

hclg_orgs <- c('Department for Communities and Local Government (excl. agencies)',
               'Planning Inspectorate',
               'Queen Elizabeth II Centre',
               'Ministry of Housing, Communities and Local Government (excl. agencies)'
)

seniority_list <- c('Senior Civil Service Level',
                    'Grades 6 and 7',
                    'Senior and Higher Executive Officers',
                    'Executive Officers',
                    'Administrative Officers and Assistants'
                    
)


unity_overalls <- c('HM Revenue and Customs overall', 'Department for Transport overall', 'Department for Levelling Up, Housing and Communities overall')



#### FUNCTIONS


css_cleaner <- function(css) {
  new_col_names <- css[4, ] 
  css <- css[-(1:7), ]
  colnames(css) <- new_col_names 
  colnames(css)[1] <- 'Organisation'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  print(colnames(css))
  
  css <- css %>%
    select(where(~ any(!is.na(.))))
  
  css <- css %>% 
    mutate_at(
      vars(`Senior Civil Service Level`, `Grades 6 and 7`, `Senior and Higher Executive Officers`, `Executive Officers`, `Administrative Officers and Assistants`), as.numeric
    )
  
  return(css)
}



css_cleaner_v2 <- function(css) {
  new_col_names <- css[6, ] 
  css <- css[-(1:6), ]
  colnames(css) <- new_col_names 
  colnames(css)[2] <- 'Organisation'
  colnames(css)[3] <- 'Senior Civil Service Level'
  colnames(css)[4] <- 'Grades 6 and 7'
  colnames(css)[5] <- 'Senior and Higher Executive Officers'
  colnames(css)[6] <- 'Executive Officers'
  colnames(css)[7] <- 'Administrative Officers and Assistants'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  print(colnames(css))
  
  css <- css %>%
    select(where(~ any(!is.na(.))))
  
  css <- css %>% 
    mutate_at(
      vars(`Senior Civil Service Level`, `Grades 6 and 7`, `Senior and Higher Executive Officers`, `Executive Officers`, `Administrative Officers and Assistants`), as.numeric
    )
  
  return(css)
}



filter_unity <- function(css) {
  css <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmrc_orgs | Organisation %in% dft_orgs | Organisation %in% hclg_orgs)
}

filter_unity_v2 <- function(css) {
  css <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% unity_overalls)
}


find_seniority_totals_and_ratios <- function(css) {
  
  scs_fte <<- css %>% 
    summarise(scs_fte = sum(`Senior Civil Service Level`, na.rm = TRUE)) %>% 
    pull(scs_fte)
  scs_fte_ratio <<- scs_fte / total_fte_for_observation
  
  g67_fte <<- css %>% 
    summarise(g67_fte = sum(`Grades 6 and 7`, na.rm = TRUE)) %>% 
    pull(g67_fte)
  g67_fte_ratio <<- g67_fte / total_fte_for_observation
  
  sheo_fte <<- css %>% 
    summarise(sheo_fte = sum(`Senior and Higher Executive Officers`, na.rm = TRUE)) %>% 
    pull(sheo_fte)
  sheo_fte_ratio <<- sheo_fte / total_fte_for_observation
  
  eo_fte <<- css %>% 
    summarise(eo_fte = sum(`Executive Officers`, na.rm = TRUE)) %>% 
    pull(eo_fte)
  eo_fte_ratio <<- eo_fte / total_fte_for_observation
  
  ao_fte <<- css %>% 
    summarise(ao_fte = sum(`Administrative Officers and Assistants`, na.rm = TRUE)) %>% 
    pull(ao_fte)
  ao_fte_ratio <<- ao_fte / total_fte_for_observation
}



pull_seniority_earnings <- function(css) {
  new_col_names <- css[4, ]
  css <- css[-(1:7), ]
  colnames(css) <- new_col_names 
  
  scs_median_salary <<- css %>% 
    select(`Senior Civil Service Level`) %>% 
    filter(css[1]=='All employees') %>% 
    as.numeric()
  scs_salary <<- scs_median_salary * scs_fte_ratio
  
  g67_median_salary <<- css %>% 
    select(`Grades 6 and 7`) %>% 
    filter(css[1]=='All employees') %>% 
    as.numeric()
  g67_salary <<- g67_median_salary * g67_fte_ratio
  
  sheo_median_salary <<- css %>% 
    select(`Senior and Higher Executive Officers`) %>% 
    filter(css[1]=='All employees') %>% 
    as.numeric()
  sheo_salary <<- sheo_median_salary * sheo_fte_ratio
  
  eo_median_salary <<- css %>% 
    select(`Executive Officers`) %>% 
    filter(css[1]=='All employees') %>% 
    as.numeric()
  eo_salary <<- eo_median_salary * eo_fte_ratio
  
  
  ao_median_salary <<- css %>% 
    select(`Administrative Officers and Assistants`) %>% 
    filter(css[1]=='All employees') %>% 
    as.numeric()
  ao_salary <<- ao_median_salary * ao_fte_ratio
  
  
  
  seniority_weighted_salary <<- log(mean(scs_salary + g67_salary + sheo_salary + eo_salary + ao_salary))
  
  
  
  return(seniority_weighted_salary)
  
  
}




pull_seniority_earnings_v2 <- function(css) {
  new_col_names <- css[6, ]
  css <- css[-(1:5), ]
  colnames(css) <- new_col_names 
  
  colnames(css)[3] <- 'Senior Civil Service Level'
  colnames(css)[4] <- 'Grades 6 and 7'
  colnames(css)[5] <- 'Senior and Higher Executive Officers'
  colnames(css)[6] <- 'Executive Officers'
  colnames(css)[7] <- 'Administrative Officers and Assistants'
  
  scs_median_salary <<- css %>% 
    select(`Senior Civil Service Level`) %>% 
    filter(css[1]=='Overall Civil Service') %>% 
    as.numeric()
  scs_salary <<- scs_median_salary * scs_fte_ratio
  
  g67_median_salary <<- css %>% 
    select(`Grades 6 and 7`) %>% 
    filter(css[1]=='Overall Civil Service') %>% 
    as.numeric()
  g67_salary <<- g67_median_salary * g67_fte_ratio
  
  sheo_median_salary <<- css %>% 
    select(`Senior and Higher Executive Officers`) %>% 
    filter(css[1]=='Overall Civil Service') %>% 
    as.numeric()
  sheo_salary <<- sheo_median_salary * sheo_fte_ratio
  
  eo_median_salary <<- css %>% 
    select(`Executive Officers`) %>% 
    filter(css[1]=='Overall Civil Service') %>% 
    as.numeric()
  eo_salary <<- eo_median_salary * eo_fte_ratio
  
  
  ao_median_salary <<- css %>% 
    select(`Administrative Officers and Assistants`) %>% 
    filter(css[1]=='Overall Civil Service') %>% 
    as.numeric()
  ao_salary <<- ao_median_salary * ao_fte_ratio
  
  
  
  seniority_weighted_salary <<- log(mean(scs_salary + g67_salary + sheo_salary + eo_salary + ao_salary))
  
  
  
  return(seniority_weighted_salary)
  
  
}









### IMPORTING FTE NUMBERS, ONLY NEED TO DO THIS ONCE
unity_fte <- read_excel("results_unity_fte_1.xlsx", sheet = 1)
unity_fte <- unity_fte %>% 
  filter(Cluster=='Unity')



#### FINDING SENIORITY AND EARNINGS NUMBERS EACH YEAR, MUST IMPORT NEW CSS EVERY TIME


### 1 of 8 ###

observation <- 1 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- unity_fte$FTE.Total[observation]

css2017 <- read_excel("CSS/css2017.xls", sheet = 23) # always double check the pages these are in
css2017_earnings <- read_excel("CSS/css2017.xls", sheet = 27)

css <- css_cleaner(css2017)
css <- filter_unity(css)
css <- find_seniority_totals_and_ratios(css)

seniority_weighted_average <- pull_seniority_earnings(css2017_earnings)

sum(scs_fte_ratio + g67_fte_ratio + sheo_fte_ratio + eo_fte_ratio + ao_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported grade level staff. ]

print(seniority_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary = ifelse(Cluster=='Unity' & Num==observation, seniority_weighted_average, ln_Seniority_Weighted_Average_Salary))


### 2 of 8 ###

observation <- 5 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- unity_fte$FTE.Total[observation]

css2018 <- read_excel("CSS/css2018_v2.xlsx", sheet = 23) # always double check the pages these are in
css2018_earnings <- read_excel("CSS/css2018_v2.xlsx", sheet = 27)

css <- css_cleaner(css2018)
css <- filter_unity(css)
css <- find_seniority_totals_and_ratios(css)

seniority_weighted_average <- pull_seniority_earnings(css2018_earnings)

sum(scs_fte_ratio + g67_fte_ratio + sheo_fte_ratio + eo_fte_ratio + ao_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported grade level staff. ]

print(seniority_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary = ifelse(Cluster=='Unity' & Num==observation, seniority_weighted_average, ln_Seniority_Weighted_Average_Salary))


### 3 of 8 ###

observation <- 9 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- unity_fte$FTE.Total[observation]

css2019 <- read_excel("CSS/css2019.xlsx", sheet = 23) # always double check the pages these are in
css2019_earnings <- read_excel("CSS/css2019.xlsx", sheet = 27)

css <- css_cleaner(css2019)
css <- filter_unity(css)
css <- find_seniority_totals_and_ratios(css)

seniority_weighted_average <- pull_seniority_earnings(css2018_earnings)

sum(scs_fte_ratio + g67_fte_ratio + sheo_fte_ratio + eo_fte_ratio + ao_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported grade level staff. ]

print(seniority_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary = ifelse(Cluster=='Unity' & Num==observation, seniority_weighted_average, ln_Seniority_Weighted_Average_Salary))

### 4 of 8 ###

observation <- 13 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- unity_fte$FTE.Total[observation]

css2020 <- read_excel("CSS/css2020.xlsx", sheet = 23) # always double check the pages these are in
css2020_earnings <- read_excel("CSS/css2020.xlsx", sheet = 27)

css <- css_cleaner(css2020)
css <- filter_unity(css)
css <- find_seniority_totals_and_ratios(css)

seniority_weighted_average <- pull_seniority_earnings(css2020_earnings)

sum(scs_fte_ratio + g67_fte_ratio + sheo_fte_ratio + eo_fte_ratio + ao_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported grade level staff. ]

print(seniority_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary = ifelse(Cluster=='Unity' & Num==observation, seniority_weighted_average, ln_Seniority_Weighted_Average_Salary))

### 5 of 8 ###

observation <- 17 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- unity_fte$FTE.Total[observation]

css2021 <- read_excel("CSS/css2021.xlsx", sheet = 23) # always double check the pages these are in
css2021_earnings <- read_excel("CSS/css2021.xlsx", sheet = 27)

css <- css_cleaner(css2021)
css <- filter_unity(css)
css <- find_seniority_totals_and_ratios(css)

seniority_weighted_average <- pull_seniority_earnings(css2021_earnings)

sum(scs_fte_ratio + g67_fte_ratio + sheo_fte_ratio + eo_fte_ratio + ao_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported grade level staff. ]

print(seniority_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary = ifelse(Cluster=='Unity' & Num==observation, seniority_weighted_average, ln_Seniority_Weighted_Average_Salary))


### 6 of 8 ###

observation <- 21 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- unity_fte$FTE.Total[observation]

css2022 <- read_ods("CSS/css2022.ods", sheet = 26) # always double check the pages these are in
css2022_earnings <- read_ods("CSS/css2022.ods", sheet = 30)

css <- css_cleaner_v2(css2022)
css <- filter_unity_v2(css)
css <- find_seniority_totals_and_ratios(css)

seniority_weighted_average <- pull_seniority_earnings_v2(css2022_earnings)

sum(scs_fte_ratio + g67_fte_ratio + sheo_fte_ratio + eo_fte_ratio + ao_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported grade level staff. ]

print(seniority_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary = ifelse(Cluster=='Unity' & Num==observation, seniority_weighted_average, ln_Seniority_Weighted_Average_Salary))

### 7 of 8 ###

observation <- 25 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- unity_fte$FTE.Total[observation]

css2023 <- read_ods("CSS/css2023.ods", sheet = 27) # always double check the pages these are in
css2023_earnings <- read_ods("CSS/css2023.ods", sheet = 31)

css <- css_cleaner_v2(css2023)
css <- filter_unity_v2(css)
css <- find_seniority_totals_and_ratios(css)

seniority_weighted_average <- pull_seniority_earnings_v2(css2023_earnings)

sum(scs_fte_ratio + g67_fte_ratio + sheo_fte_ratio + eo_fte_ratio + ao_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported grade level staff. ]

print(seniority_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary = ifelse(Cluster=='Unity' & Num==observation, seniority_weighted_average, ln_Seniority_Weighted_Average_Salary))

### 8 of 8 ###

observation <- 29 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

total_fte_for_observation <- unity_fte$FTE.Total[observation]

css2024 <- read_ods("CSS/css2024.ods", sheet = 26) # always double check the pages these are in
css2024_earnings <- read_ods("CSS/css2024.ods", sheet = 30)

css <- css_cleaner_v2(css2024)
css <- filter_unity_v2(css)
css <- find_seniority_totals_and_ratios(css)

seniority_weighted_average <- pull_seniority_earnings_v2(css2024_earnings)

sum(scs_fte_ratio + g67_fte_ratio + sheo_fte_ratio + eo_fte_ratio + ao_fte_ratio) # this should be close to 1 [it likely won't be exactly 1 as the total FTE number also includes unreported grade level staff. ]

print(seniority_weighted_average)

completedata <<- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary = ifelse(Cluster=='Unity' & Num==observation, seniority_weighted_average, ln_Seniority_Weighted_Average_Salary))





###### NATURAL CUBIC SPLINE INTERPOLATION



x <- c(1:29)
y <- completedata$ln_Seniority_Weighted_Average_Salary[61:89]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "Seniority Weights Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


unity_indices <- which(completedata$Cluster == 'Unity')

completedata <- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary_ncsi = NA)

completedata <- completedata %>%
  mutate(ln_Seniority_Weighted_Average_Salary_ncsi = ifelse(Cluster == 'Unity', NA, ln_Seniority_Weighted_Average_Salary_ncsi))

completedata$ln_Seniority_Weighted_Average_Salary_ncsi[unity_indices] <- spline_values





# WRITE TO DATAFRAME

write_xlsx(completedata, "outputs/results_unity_seniority_weights.xlsx")


