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


### defence ORGANISATIONS
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

mod_orgs <- c('Ministry of Defence', 'Ministry of Defence (excl. agencies)')

dstl_orgs <- c('Defence Science and Technology Laboratory')

deqsu_orgs <- c('Defence Equipment and Support')

ukho_orgs <- c('UK Hydrographic Office')





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
  #new_col_name <- css[6, 35]
  css <- css[-(1:5), ]
  colnames(css)[2] <- 'Organisation' # total is in this column; do not include column 1 as that adds dept TOTAL /and/ child agency TOTALS
  colnames(css)[9] <- 'Finance'
  colnames(css)[11] <- 'Human Resources'
  colnames(css)[35] <- 'Total full-time equivalent (FTE) of all civil servants'
  
  css$Organisation <- gsub("[0-9]", "", css$Organisation)
  css$Organisation <- trimws(css$Organisation)
  return(css)
}

css_cols_all_employees <- function(css) {
  css <- css %>% 
    select(Organisation, `All employees`)
  return(css)
}


css_cols_all_civilservants <- function(css) {
  css <- css %>% 
    select(Organisation, `Total full-time equivalent (FTE) of all civil servants`)
  return(css)
}

css_to_num_all_emlpoyees <- function(css) {
  css <- css %>% 
    mutate(
      `All employees` = as.numeric(`All employees`)
    )
  return(css)
}

css_to_num_all_civilservants <- function(css) {
  css <- css %>% 
    mutate(
      `Total full-time equivalent (FTE) of all civil servants` = as.numeric(`Total full-time equivalent (FTE) of all civil servants`)
    )
  return(css)
}

css_cols_total <- function(css) {
  css <- css %>% 
    select(Organisation, `Total`)
  return(css)
}

css_to_num_total <- function(css) {
  css <- css %>% 
    mutate(
      `Total` = as.numeric(`Total`)
    )
  return(css)
}

defence_filter <- function(css) {
  css <- css %>% # filtering to all defence departments and child agencies
    select(everything()) %>% 
    filter(Organisation %in% deqsu_orgs | Organisation %in% dstl_orgs | Organisation %in% mod_orgs | Organisation %in% ukho_orgs)
}

defence_filter_v2 <- function(css) {
  css <- css %>% # filtering to all defence departments (overalls)
    select(everything()) %>% 
    filter(Organisation %in% defence_depts_overalls) # for last entry, we need to group ESNZ + DSIT together
}

find_total_FTE_all_employees <- function(css) {
  total_fte <<- css %>% 
    summarise(total_fte = sum(`All employees`, na.rm = TRUE)) %>% 
    pull(total_fte)
  return(css)
}

find_total_FTE_total <- function(css) {
  total_fte <<- css %>% 
    summarise(total_fte = sum(`Total`, na.rm = TRUE)) %>% 
    pull(total_fte)
  return(css)
}





find_total_FTE_civilservants <- function(css) {
  total_fte <<- css %>% 
    summarise(total_fte = sum(`Total full-time equivalent (FTE) of all civil servants`, na.rm = TRUE)) %>% 
    pull(total_fte)
  return(css)
}




### FTE Count Variable

defence_FTE <- array(1:8)


### 1 of 8 ###

Observation <- 1 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2017 <- read_excel("CSS/css2017.xls", sheet = 10)

defence_2017 <- css2017 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols_total() %>% 
  css_to_num_total() %>% 
  defence_filter() %>% 
  find_total_FTE_total()


print(total_fte)

completedata <<- completedata %>%
  mutate(FTE.Total = ifelse(Cluster=='Defence' & Num==Observation, total_fte, FTE.Total))


### 2 of 8 ###

Observation <- 5 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2018 <- read_excel("CSS/css2018_v2.xlsx", sheet = 10) # double check which page has info!

defence_2018 <- css2018 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols_total() %>% 
  css_to_num_total() %>% 
  defence_filter() %>% 
  find_total_FTE_total()


print(total_fte)

completedata <<- completedata %>%
  mutate(FTE.Total = ifelse(Cluster=='Defence' & Num==Observation, total_fte, FTE.Total))


### 3 of 8 ###

Observation <- 9 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2019 <- read_excel("CSS/css2019.xlsx", sheet = 10) # double check which page has info!

defence_2019 <- css2019 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols_total() %>% 
  css_to_num_total() %>% 
  defence_filter() %>% 
  find_total_FTE_total()


print(total_fte)

completedata <<- completedata %>%
  mutate(FTE.Total = ifelse(Cluster=='Defence' & Num==Observation, total_fte, FTE.Total))


### 4 of 8 ###

Observation <- 13 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2020 <- read_excel("CSS/css2020.xlsx", sheet = 10) # double check which page has info!

defence_2020 <- css2020 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols_total() %>% 
  css_to_num_total() %>% 
  defence_filter() %>% 
  find_total_FTE_total()


print(total_fte)

completedata <<- completedata %>%
  mutate(FTE.Total = ifelse(Cluster=='Defence' & Num==Observation, total_fte, FTE.Total))


### 5 of 8 ###

Observation <- 17 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2021 <- read_excel("CSS/css2021.xlsx", sheet = 10) # double check which page has info!

defence_2021 <- css2021 %>% # NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner() %>% 
  css_cols_all_employees() %>% 
  css_to_num_all_emlpoyees() %>% 
  defence_filter() %>% 
  find_total_FTE_all_employees()


print(total_fte)

completedata <<- completedata %>%
  mutate(FTE.Total = ifelse(Cluster=='Defence' & Num==Observation, total_fte, FTE.Total))


### 6 of 8 ###

Observation <- 21 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2022 <- read_ods("CSS/css2022.ods", sheet = 13) # double check which page has info!

defence_2022 <- css2022 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols_all_civilservants() %>% 
  css_to_num_all_civilservants() %>% 
  defence_filter() %>% 
  find_total_FTE_civilservants()


print(total_fte)

completedata <<- completedata %>%
  mutate(FTE.Total = ifelse(Cluster=='Defence' & Num==Observation, total_fte, FTE.Total))



### 7 of 8 ###

Observation <- 25 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2023 <- read_ods("CSS/css2023.ods", sheet = 14) # double check which page has info!

defence_2023 <- css2023 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols_all_civilservants() %>% 
  css_to_num_all_civilservants() %>% 
  defence_filter() %>% 
  find_total_FTE_civilservants()


print(total_fte)

completedata <<- completedata %>%
  mutate(FTE.Total = ifelse(Cluster=='Defence' & Num==Observation, total_fte, FTE.Total))


### 8 of 8 ###

Observation <- 29 # (1, 5, 9, 13, 17, 21, 25, 29) # always update this

# NOTES: Check organisation name updates, dataframe layout updates, check schema for which depts to include (have more been added? some dissolved?)

css2024 <- read_ods("CSS/css2024.ods", sheet = 13) # double check which page has info!

defence_2024 <- css2024 %>%# NAs added by coercion are fine, not an error: it is removing characters in numbers column, and NAs aren't added anyway.
  css_cleaner_v2() %>% 
  css_cols_all_civilservants() %>% 
  css_to_num_all_civilservants() %>% 
  defence_filter() %>% 
  find_total_FTE_civilservants()


print(total_fte)

completedata <<- completedata %>%
  mutate(FTE.Total = ifelse(Cluster=='Defence' & Num==Observation, total_fte, FTE.Total))










### NATURAL CUBIC SPLINE INTERPOLATION

# Adding new FTE data



x <- c(1:29)
y <- completedata$FTE.Total[121:149]
print(y)


xi <- seq(min(x), max(x), by = 1)


spline_result <- spline(x, y, xout = xi, method = "natural")
print(spline_result$y)

spline_values <- spline_result$y[1:29]
print(spline_values)

plot(x, y, main = "defence FTE Natural Cubic Spline Interpolation", xlab = "X", ylab = "Y", col = "blue", pch = 19)
lines(spline_result, col = "red")


defence_indices <- which(completedata$Cluster == 'Defence')

completedata <- completedata %>%
  mutate(fte_ncsi = NA)

completedata <- completedata %>%
  mutate(fte_ncsi = ifelse(Cluster == 'Defence', NA, fte_ncsi))

completedata$fte_ncsi[defence_indices] <- spline_values


# WRITE TO DATAFRAME

write_xlsx(completedata, "outputs/results_defence_fte.xlsx")



