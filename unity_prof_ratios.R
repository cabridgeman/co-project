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

## SYNERGY ORGANISATIONS
# Department for Environment, Food and Rural Affairs (and child agencies)

# Department for Work and Pensions (where available) (always including the Health and Safety Executive too)

# The Health and Safety Executive (included /every year/)

# Home Office (and child agencies)

# Ministry of Justice (and child agencies)



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

hmrc_overalls <- c('HM Revenue and Customs overall')

dft_overalls <- c('Department for Transport overall')

hclg_overalls <- c('Department for Levelling Up, Housing and Communities overall')

unity_depts_overalls <- c('HM Revenue and Customs overall', 'Department for Transport overall', 'Department for Levelling Up, Housing and Communities overall')




##### FUNCTIONS #####

css_cleaner <- function(css) {
  new_col_names <- css[4, ] 
  css <- css[-(1:7), ]
  colnames(css) <- new_col_names 
  colnames(css)[1] <- 'Organisation'
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  # css$Organisation <- gsub(",", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  # css$Organisation <- gsub("[[:punct:]]", "", css$Organisation) # more trouble than it is worth with this dataset.
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

unity_filter <- function(css) {
  css <- css %>% # filtering to all synergy departments and child agencies
    select(everything()) %>% 
    filter(Organisation %in% hmrc_orgs | Organisation %in% dft_orgs | Organisation %in% hclg_orgs)
}

unity_filter_v2 <- function(css) {
  css <- css %>% # filtering to all synergy departments and child agencies
    select(everything()) %>% 
    filter(Organisation %in% unity_depts_overalls)
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




find_hmrc_HR_ratio <- function(css) {
  hmrc_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmrc_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  hmrc_hr_ratio <<- hmrc_hr_num / total_hr
  return(css)
}

find_hmrc_HR_ratio_v2 <- function(css) {
  hmrc_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmrc_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  hmrc_hr_ratio <<- hmrc_hr_num / total_hr
  return(css)
}


find_hmrc_Finance_ratio <- function(css) {
  hmrc_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmrc_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  hmrc_finance_ratio <<- hmrc_finance_num / total_f
  print(hmrc_finance_ratio)
  return(css)
}


find_hmrc_Finance_ratio_v2 <- function(css) {
  hmrc_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hmrc_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  hmrc_finance_ratio <<- hmrc_finance_num / total_f
  print(hmrc_finance_ratio)
  return(css)
}







find_dft_HR_ratio <- function(css) {
  dft_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dft_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dft_hr_ratio <<- dft_hr_num / total_hr
  return(css)
}

find_dft_HR_ratio_v2 <- function(css) {
  dft_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dft_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dft_hr_ratio <<- dft_hr_num / total_hr
  return(css)
}


find_dft_Finance_ratio <- function(css) {
  dft_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dft_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dft_finance_ratio <<- dft_finance_num / total_f
  print(dft_finance_ratio)
  return(css)
}


find_dft_Finance_ratio_v2 <- function(css) {
  dft_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dft_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dft_finance_ratio <<- dft_finance_num / total_f
  print(dft_finance_ratio)
  return(css)
}





find_hclg_HR_ratio <- function(css) {
  hclg_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hclg_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  hclg_hr_ratio <<- hclg_hr_num / total_hr
  return(css)
}

find_hclg_HR_ratio_v2 <- function(css) {
  hclg_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hclg_overalls) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  hclg_hr_ratio <<- hclg_hr_num / total_hr
  return(css)
}


find_hclg_Finance_ratio <- function(css) {
  hclg_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hclg_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  hclg_finance_ratio <<- hclg_finance_num / total_f
  print(hclg_finance_ratio)
  return(css)
}


find_hclg_Finance_ratio_v2 <- function(css) {
  hclg_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% hclg_overalls) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  hclg_finance_ratio <<- hclg_finance_num / total_f
  print(hclg_finance_ratio)
  return(css)
}












### 1 of 8 ###

Observation <- 1 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2017 <- read_excel("CSS/css2017.xls", sheet = 10)

unity_2017 <- css2017 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  unity_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_hmrc_HR_ratio() %>% 
  find_hmrc_Finance_ratio() %>% 
  find_dft_HR_ratio() %>% 
  find_dft_Finance_ratio() %>% 
  find_hclg_HR_ratio() %>% 
  find_hclg_Finance_ratio() 



HR_ratios <- array(1:3)
HR_ratios[1] <- hmrc_hr_ratio
HR_ratios[2] <- dft_hr_ratio
HR_ratios[3] <- hclg_hr_ratio



f_ratios <- array(1:3)
f_ratios[1] <- hmrc_finance_ratio
f_ratios[2] <- dft_finance_ratio
f_ratios[3] <- hclg_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))


### 2 of 8 ###

Observation <- 5 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2018 <- read_excel("CSS/css2018_v2.xlsx", sheet = 10)

unity_2018 <- css2018 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  unity_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_hmrc_HR_ratio() %>% 
  find_hmrc_Finance_ratio() %>% 
  find_dft_HR_ratio() %>% 
  find_dft_Finance_ratio() %>% 
  find_hclg_HR_ratio() %>% 
  find_hclg_Finance_ratio() 



HR_ratios <- array(1:3)
HR_ratios[1] <- hmrc_hr_ratio
HR_ratios[2] <- dft_hr_ratio
HR_ratios[3] <- hclg_hr_ratio



f_ratios <- array(1:3)
f_ratios[1] <- hmrc_finance_ratio
f_ratios[2] <- dft_finance_ratio
f_ratios[3] <- hclg_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))


### 3 of 8 ###

Observation <- 9 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2019 <- read_excel("CSS/css2019.xlsx", sheet = 10) # double check which page has info!

unity_2019 <- css2019 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  unity_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_hmrc_HR_ratio() %>% 
  find_hmrc_Finance_ratio() %>% 
  find_dft_HR_ratio() %>% 
  find_dft_Finance_ratio() %>% 
  find_hclg_HR_ratio() %>% 
  find_hclg_Finance_ratio() 



HR_ratios <- array(1:3)
HR_ratios[1] <- hmrc_hr_ratio
HR_ratios[2] <- dft_hr_ratio
HR_ratios[3] <- hclg_hr_ratio



f_ratios <- array(1:3)
f_ratios[1] <- hmrc_finance_ratio
f_ratios[2] <- dft_finance_ratio
f_ratios[3] <- hclg_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))


### 4 of 8 ###

Observation <- 13 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2020 <- read_excel("CSS/css2020.xlsx", sheet = 10) # double check which page has info!

unity_2020 <- css2020 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  unity_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_hmrc_HR_ratio() %>% 
  find_hmrc_Finance_ratio() %>% 
  find_dft_HR_ratio() %>% 
  find_dft_Finance_ratio() %>% 
  find_hclg_HR_ratio() %>% 
  find_hclg_Finance_ratio() 



HR_ratios <- array(1:3)
HR_ratios[1] <- hmrc_hr_ratio
HR_ratios[2] <- dft_hr_ratio
HR_ratios[3] <- hclg_hr_ratio



f_ratios <- array(1:3)
f_ratios[1] <- hmrc_finance_ratio
f_ratios[2] <- dft_finance_ratio
f_ratios[3] <- hclg_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))



### 5 of 8 ###

Observation <- 17 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2021 <- read_excel("CSS/css2021.xlsx", sheet = 10) # double check which page has info!

unity_2021 <- css2021 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  unity_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_hmrc_HR_ratio() %>% 
  find_hmrc_Finance_ratio() %>% 
  find_dft_HR_ratio() %>% 
  find_dft_Finance_ratio() %>% 
  find_hclg_HR_ratio() %>% 
  find_hclg_Finance_ratio() 



HR_ratios <- array(1:3)
HR_ratios[1] <- hmrc_hr_ratio
HR_ratios[2] <- dft_hr_ratio
HR_ratios[3] <- hclg_hr_ratio



f_ratios <- array(1:3)
f_ratios[1] <- hmrc_finance_ratio
f_ratios[2] <- dft_finance_ratio
f_ratios[3] <- hclg_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))






### 6 of 8 ###

Observation <- 21 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2022 <- read_ods("CSS/css2022.ods", sheet = 13) # double check which page has info!

unity_2022 <- css2022 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  unity_filter_v2() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_hmrc_HR_ratio_v2() %>% 
  find_hmrc_Finance_ratio_v2() %>% 
  find_dft_HR_ratio_v2() %>% 
  find_dft_Finance_ratio_v2() %>% 
  find_hclg_HR_ratio_v2() %>% 
  find_hclg_Finance_ratio_v2() 



HR_ratios <- array(1:3)
HR_ratios[1] <- hmrc_hr_ratio
HR_ratios[2] <- dft_hr_ratio
HR_ratios[3] <- hclg_hr_ratio



f_ratios <- array(1:3)
f_ratios[1] <- hmrc_finance_ratio
f_ratios[2] <- dft_finance_ratio
f_ratios[3] <- hclg_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))


### 7 of 8 ###

Observation <- 25 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2023 <- read_ods("CSS/css2023.ods", sheet = 14) # double check which page has info!

unity_2023 <- css2023 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  unity_filter_v2() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_hmrc_HR_ratio_v2() %>% 
  find_hmrc_Finance_ratio_v2() %>% 
  find_dft_HR_ratio_v2() %>% 
  find_dft_Finance_ratio_v2() %>% 
  find_hclg_HR_ratio_v2() %>% 
  find_hclg_Finance_ratio_v2() 



HR_ratios <- array(1:3)
HR_ratios[1] <- hmrc_hr_ratio
HR_ratios[2] <- dft_hr_ratio
HR_ratios[3] <- hclg_hr_ratio



f_ratios <- array(1:3)
f_ratios[1] <- hmrc_finance_ratio
f_ratios[2] <- dft_finance_ratio
f_ratios[3] <- hclg_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))

### 8 of 8 ###

Observation <- 29 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2024 <- read_ods("CSS/css2024.ods", sheet = 13) # double check which page has info!

unity_2024 <- css2024 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  unity_filter_v2() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_hmrc_HR_ratio_v2() %>% 
  find_hmrc_Finance_ratio_v2() %>% 
  find_dft_HR_ratio_v2() %>% 
  find_dft_Finance_ratio_v2() %>% 
  find_hclg_HR_ratio_v2() %>% 
  find_hclg_Finance_ratio_v2() 



HR_ratios <- array(1:3)
HR_ratios[1] <- hmrc_hr_ratio
HR_ratios[2] <- dft_hr_ratio
HR_ratios[3] <- hclg_hr_ratio



f_ratios <- array(1:3)
f_ratios[1] <- hmrc_finance_ratio
f_ratios[2] <- dft_finance_ratio
f_ratios[3] <- hclg_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Unity' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))




### NATURAL CUBIC SPLINE INTERPOLATION

# Adding new HR data

x <- c(1:29)
y <- completedata$HR_q1_Concentration[61:89]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "HR Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


unity_indices <- which(completedata$Cluster == 'Unity')

completedata <- completedata %>%
  mutate(HR_q1_concentration_ncsi = NA)

completedata <- completedata %>%
  mutate(HR_q1_concentration_ncsi = ifelse(Cluster == 'Unity', NA, HR_q1_concentration_ncsi))

completedata$HR_q1_concentration_ncsi[unity_indices] <- spline_values


# Adding new finance data


x <- c(1:29)
y <- completedata$Finance_q1_Concentration[61:89]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "Finance Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


unity_indices <- which(completedata$Cluster == 'Unity')

completedata <- completedata %>%
  mutate(Finance_q1_concentration_ncsi = NA)

completedata <- completedata %>%
  mutate(Finance_q1_concentration_ncsi = ifelse(Cluster == 'Unity', NA, Finance_q1_concentration_ncsi))

completedata$Finance_q1_concentration_ncsi[unity_indices] <- spline_values






# WRITE TO DATAFRAME

write_xlsx(completedata, "outputs/results_unity_prof_ratios.xlsx")

