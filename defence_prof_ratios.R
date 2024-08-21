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

## DEFENCE



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

mod_orgs <- c('Ministry of Defence', 'Ministry of Defence (excl. agencies)')

dstl_orgs <- c('Defence Science and Technology Laboratory')

deqsu_orgs <- c('Defence Equipment and Support')

ukho_orgs <- c('UK Hydrographic Office')


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

defence_filter <- function(css) {
  css <- css %>% # filtering to all synergy departments and child agencies
    select(everything()) %>% 
    filter(Organisation %in% deqsu_orgs | Organisation %in% dstl_orgs | Organisation %in% mod_orgs | Organisation %in% ukho_orgs)
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





find_deqsu_HR_ratio <- function(css) {
  deqsu_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% deqsu_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  deqsu_hr_ratio <<- deqsu_hr_num / total_hr
  return(css)
}


find_deqsu_Finance_ratio <- function(css) {
  deqsu_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% deqsu_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  deqsu_finance_ratio <<- deqsu_finance_num / total_f
  print(deqsu_finance_ratio)
  return(css)
}


find_dstl_HR_ratio <- function(css) {
  dstl_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dstl_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  dstl_hr_ratio <<- dstl_hr_num / total_hr
  return(css)
}


find_dstl_Finance_ratio <- function(css) {
  dstl_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% dstl_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  dstl_finance_ratio <<- dstl_finance_num / total_f
  print(dstl_finance_ratio)
  return(css)
}




find_mod_HR_ratio <- function(css) {
  mod_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% mod_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  mod_hr_ratio <<- mod_hr_num / total_hr
  return(css)
}


find_mod_Finance_ratio <- function(css) {
  mod_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% mod_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  mod_finance_ratio <<- mod_finance_num / total_f
  print(mod_finance_ratio)
  return(css)
}





find_ukho_HR_ratio <- function(css) {
  ukho_hr_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% ukho_orgs) %>% 
    summarise(total = sum(`Human Resources`, na.rm = TRUE)) %>% 
    pull(total)
  
  ukho_hr_ratio <<- ukho_hr_num / total_hr
  return(css)
}


find_ukho_Finance_ratio <- function(css) {
  ukho_finance_num <- css %>% 
    select(everything()) %>% 
    filter(Organisation %in% ukho_orgs) %>% 
    summarise(total = sum(Finance, na.rm = TRUE)) %>% 
    pull(total)
  
  ukho_finance_ratio <<- ukho_finance_num / total_f
  print(ukho_finance_ratio)
  return(css)
}








### 1 of 8 ###

Observation <- 1 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2017 <- read_excel("CSS/css2017.xls", sheet = 10)

defence_2017 <- css2017 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  defence_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_deqsu_HR_ratio() %>% 
  find_deqsu_Finance_ratio() %>% 
  find_dstl_HR_ratio() %>% 
  find_dstl_Finance_ratio() %>% 
  find_mod_HR_ratio() %>% 
  find_mod_Finance_ratio() %>% 
  find_ukho_HR_ratio() %>% 
  find_ukho_Finance_ratio() 



HR_ratios <- array(1:4)
HR_ratios[1] <- deqsu_hr_ratio
HR_ratios[2] <- dstl_hr_ratio
HR_ratios[3] <- mod_hr_ratio
HR_ratios[4] <- ukho_hr_ratio



f_ratios <- array(1:4)
f_ratios[1] <- deqsu_finance_ratio
f_ratios[2] <- dstl_finance_ratio
f_ratios[3] <- mod_finance_ratio
f_ratios[4] <- ukho_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))




### 2 of 8 ###

Observation <- 5 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2018 <- read_excel("CSS/css2018_v2.xlsx", sheet = 10)

defence_2018 <- css2018 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  defence_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_deqsu_HR_ratio() %>% 
  find_deqsu_Finance_ratio() %>% 
  find_dstl_HR_ratio() %>% 
  find_dstl_Finance_ratio() %>% 
  find_mod_HR_ratio() %>% 
  find_mod_Finance_ratio() %>% 
  find_ukho_HR_ratio() %>% 
  find_ukho_Finance_ratio() 



HR_ratios <- array(1:4)
HR_ratios[1] <- deqsu_hr_ratio
HR_ratios[2] <- dstl_hr_ratio
HR_ratios[3] <- mod_hr_ratio
HR_ratios[4] <- ukho_hr_ratio



f_ratios <- array(1:4)
f_ratios[1] <- deqsu_finance_ratio
f_ratios[2] <- dstl_finance_ratio
f_ratios[3] <- mod_finance_ratio
f_ratios[4] <- ukho_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))



### 3 of 8 ###

Observation <- 9 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2019 <- read_excel("CSS/css2019.xlsx", sheet = 10) # double check which page has info!

defence_2019 <- css2019 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  defence_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_deqsu_HR_ratio() %>% 
  find_deqsu_Finance_ratio() %>% 
  find_dstl_HR_ratio() %>% 
  find_dstl_Finance_ratio() %>% 
  find_mod_HR_ratio() %>% 
  find_mod_Finance_ratio() %>% 
  find_ukho_HR_ratio() %>% 
  find_ukho_Finance_ratio() 



HR_ratios <- array(1:4)
HR_ratios[1] <- deqsu_hr_ratio
HR_ratios[2] <- dstl_hr_ratio
HR_ratios[3] <- mod_hr_ratio
HR_ratios[4] <- ukho_hr_ratio



f_ratios <- array(1:4)
f_ratios[1] <- deqsu_finance_ratio
f_ratios[2] <- dstl_finance_ratio
f_ratios[3] <- mod_finance_ratio
f_ratios[4] <- ukho_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))




### 4 of 8 ###

Observation <- 13 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2020 <- read_excel("CSS/css2020.xlsx", sheet = 10) # double check which page has info!

defence_2020 <- css2020 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  defence_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_deqsu_HR_ratio() %>% 
  find_deqsu_Finance_ratio() %>% 
  find_dstl_HR_ratio() %>% 
  find_dstl_Finance_ratio() %>% 
  find_mod_HR_ratio() %>% 
  find_mod_Finance_ratio() %>% 
  find_ukho_HR_ratio() %>% 
  find_ukho_Finance_ratio() 



HR_ratios <- array(1:4)
HR_ratios[1] <- deqsu_hr_ratio
HR_ratios[2] <- dstl_hr_ratio
HR_ratios[3] <- mod_hr_ratio
HR_ratios[4] <- ukho_hr_ratio



f_ratios <- array(1:4)
f_ratios[1] <- deqsu_finance_ratio
f_ratios[2] <- dstl_finance_ratio
f_ratios[3] <- mod_finance_ratio
f_ratios[4] <- ukho_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))


### 5 of 8 ###

Observation <- 17 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2021 <- read_excel("CSS/css2021.xlsx", sheet = 10) # double check which page has info!

defence_2021 <- css2021 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  defence_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_deqsu_HR_ratio() %>% 
  find_deqsu_Finance_ratio() %>% 
  find_dstl_HR_ratio() %>% 
  find_dstl_Finance_ratio() %>% 
  find_mod_HR_ratio() %>% 
  find_mod_Finance_ratio() %>% 
  find_ukho_HR_ratio() %>% 
  find_ukho_Finance_ratio() 



HR_ratios <- array(1:4)
HR_ratios[1] <- deqsu_hr_ratio
HR_ratios[2] <- dstl_hr_ratio
HR_ratios[3] <- mod_hr_ratio
HR_ratios[4] <- ukho_hr_ratio



f_ratios <- array(1:4)
f_ratios[1] <- deqsu_finance_ratio
f_ratios[2] <- dstl_finance_ratio
f_ratios[3] <- mod_finance_ratio
f_ratios[4] <- ukho_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))



### 6 of 8 ###

Observation <- 21 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2022 <- read_ods("CSS/css2022.ods", sheet = 13) # double check which page has info!

defence_2022 <- css2022 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  defence_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_deqsu_HR_ratio() %>% 
  find_deqsu_Finance_ratio() %>% 
  find_dstl_HR_ratio() %>% 
  find_dstl_Finance_ratio() %>% 
  find_mod_HR_ratio() %>% 
  find_mod_Finance_ratio() %>% 
  find_ukho_HR_ratio() %>% 
  find_ukho_Finance_ratio() 



HR_ratios <- array(1:4)
HR_ratios[1] <- deqsu_hr_ratio
HR_ratios[2] <- dstl_hr_ratio
HR_ratios[3] <- mod_hr_ratio
HR_ratios[4] <- ukho_hr_ratio



f_ratios <- array(1:4)
f_ratios[1] <- deqsu_finance_ratio
f_ratios[2] <- dstl_finance_ratio
f_ratios[3] <- mod_finance_ratio
f_ratios[4] <- ukho_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))


### 7 of 8 ###

Observation <- 25 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2023 <- read_ods("CSS/css2023.ods", sheet = 14) # double check which page has info!

defence_2023 <- css2023 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  defence_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_deqsu_HR_ratio() %>% 
  find_deqsu_Finance_ratio() %>% 
  find_dstl_HR_ratio() %>% 
  find_dstl_Finance_ratio() %>% 
  find_mod_HR_ratio() %>% 
  find_mod_Finance_ratio() %>% 
  find_ukho_HR_ratio() %>% 
  find_ukho_Finance_ratio() 



HR_ratios <- array(1:4)
HR_ratios[1] <- deqsu_hr_ratio
HR_ratios[2] <- dstl_hr_ratio
HR_ratios[3] <- mod_hr_ratio
HR_ratios[4] <- ukho_hr_ratio



f_ratios <- array(1:4)
f_ratios[1] <- deqsu_finance_ratio
f_ratios[2] <- dstl_finance_ratio
f_ratios[3] <- mod_finance_ratio
f_ratios[4] <- ukho_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))




### 8 of 8 ###

Observation <- 29 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2024 <- read_ods("CSS/css2024.ods", sheet = 13) # double check which page has info!

defence_2024 <- css2024 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols() %>% 
  css_to_num() %>% 
  defence_filter() %>% 
  find_total_HR() %>% 
  find_total_Finance() %>% 
  find_deqsu_HR_ratio() %>% 
  find_deqsu_Finance_ratio() %>% 
  find_dstl_HR_ratio() %>% 
  find_dstl_Finance_ratio() %>% 
  find_mod_HR_ratio() %>% 
  find_mod_Finance_ratio() %>% 
  find_ukho_HR_ratio() %>% 
  find_ukho_Finance_ratio() 



HR_ratios <- array(1:4)
HR_ratios[1] <- deqsu_hr_ratio
HR_ratios[2] <- dstl_hr_ratio
HR_ratios[3] <- mod_hr_ratio
HR_ratios[4] <- ukho_hr_ratio



f_ratios <- array(1:4)
f_ratios[1] <- deqsu_finance_ratio
f_ratios[2] <- dstl_finance_ratio
f_ratios[3] <- mod_finance_ratio
f_ratios[4] <- ukho_finance_ratio



summary(HR_ratios)
print(HR_ratios)
sum(HR_ratios)
summary(f_ratios)
print(f_ratios)
sum(f_ratios)# double check array = 100%

completedata <<- completedata %>%
  mutate(HR_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(HR_ratios, probs = 0.25), HR_q1_Concentration))

completedata <<- completedata %>%
  mutate(Finance_q1_Concentration = ifelse(Cluster=='Defence' & Num==Observation, quantile(f_ratios, probs = 0.25), Finance_q1_Concentration))




### NATURAL CUBIC SPLINE INTERPOLATION

# Adding new HR data

x <- c(1:29)
y <- completedata$HR_q1_Concentration[121:149]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "HR Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


defence_indices <- which(completedata$Cluster == 'Defence')

completedata <- completedata %>%
  mutate(HR_q1_concentration_ncsi = NA)

completedata <- completedata %>%
  mutate(HR_q1_concentration_ncsi = ifelse(Cluster == 'Defence', NA, HR_q1_concentration_ncsi))

completedata$HR_q1_concentration_ncsi[defence_indices] <- spline_values


# Adding new finance data


x <- c(1:29)
y <- completedata$Finance_q1_Concentration[121:149]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "Finance Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


defence_indices <- which(completedata$Cluster == 'Defence')

completedata <- completedata %>%
  mutate(Finance_q1_concentration_ncsi = NA)

completedata <- completedata %>%
  mutate(Finance_q1_concentration_ncsi = ifelse(Cluster == 'Defence', NA, Finance_q1_concentration_ncsi))

completedata$Finance_q1_concentration_ncsi[defence_indices] <- spline_values


# WRITE TO DATAFRAME

write_xlsx(completedata, "outputs/results_defence_prof_ratios.xlsx")


