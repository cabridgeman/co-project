rm(list=ls())
options(scipen=999)

library(readxl)
library(dplyr)
library(magrittr)
library(scales)
library(ggplot2)
library(readODS)
library(writexl)
library(ggplot2)
library(patchwork)
library(scales)
library(cowplot)
library(extrafont)
library(showtext)

library(scales)
library(reshape2)
library(lubridate)

#font_import()
loadfonts(device = "win")
windowsFonts(Times = windowsFont("TT Times New Roman"))
font_add("Times New Roman", "times.ttf")
showtext_auto()

#### INITIALISING ####
setwd("C:/Users/christy.bridgeman/Documents/Working Dissertation/Data")

matrix <- read_excel("all_matrix.xlsx")
synergy <- read_excel("all_synergy.xlsx")
unity <- read_excel("all_unity.xlsx")
overseas <- read_excel("all_overseas.xlsx")
defence <- read_excel("all_defence.xlsx")

data <- bind_rows(matrix, synergy, unity, defence, overseas)

data$admin_total <- data$admin_total / 1000
data$admin_staff <- data$admin_staff  / 1000
data$admin_running_costs <- data$admin_running_costs / 1000
data$Date_Published <- as.Date(data$Date_Published, format = "%Y-%m-%d")


data <- data %>%
  mutate(Year = year(Date_Published))

yearly_means <- data %>%
  group_by(Year) %>%
  summarise(YearlyMean = mean(ln_Admin_Running_Cost_Intensity), .groups = 'drop')


data_with_means <- data %>%
  left_join(yearly_means, by = "Year")

annual_data <- data %>%
  group_by(Cluster, Year) %>%
  slice(1) %>% # Choosing the first row per year
  ungroup()


annual_data <- annual_data %>%
  group_by(Cluster) %>%
  mutate(Relative_Change = ((exp((ln_Admin_Running_Cost_Intensity - lag(ln_Admin_Running_Cost_Intensity)))) - 1)  * 100)






plot1 <- ggplot(annual_data, aes(x=Date_Published, y=Relative_Change, fill = Cluster))+
  geom_bar(stat = "identity", position = position_dodge(), width=300) +
  theme_classic()  +
  #geom_text(size = 2.2, position = position_dodge(width =300), vjust = -1, hjust=0.55, aes(label = paste0(round(Finance_q3_Concentration * 100), "%"))) +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 12)) + 
  scale_x_date(breaks = seq.Date(from = as.Date("2018-03-28"), to = as.Date("2024-06-30"), by = "12 month"), 
               date_labels = "%Y",
               limits = c(as.Date("2017-10-01"), as.Date("2024-08-30"))) +
  labs(title = "Admin Running Cost Intensities \n",
       x = '\n Year',
       y = (y="Annual Relative Change \n")) +
  scale_fill_manual(
    values = c("Matrix" = "#7CC674", "Synergy" = "#009596", "Unity" = "#73C5C5", 'Defence' = '#38812F', 'Overseas'='#002F5D')) +
  labs(x = "\n Year") +
  scale_y_continuous(labels = scales::label_percent(scale=1)) +
  coord_cartesian(
    ylim = c(-100, 100) 
  )

plot1 <- plot1 + annotate("text", x = as.Date("2024-04-20"), y = 105, label = "↑ +1107%", size=3) +
  geom_vline(xintercept = as.Date("2018-09-29"), linetype="solid", color = "#ededed") +
  geom_vline(xintercept = as.Date("2019-09-29"), linetype="solid", color = "#ededed") +
  geom_vline(xintercept = as.Date("2020-09-29"), linetype="solid", color = "#ededed") +
  geom_vline(xintercept = as.Date("2021-09-29"), linetype="solid", color = "#ededed") +
  geom_vline(xintercept = as.Date("2022-09-29"), linetype="solid", color = "#ededed") +
  geom_vline(xintercept = as.Date("2023-09-29"), linetype="solid", color = "#ededed") +
  geom_hline(yintercept=0, linetype="solid", color = "black") +
  annotate("text", x = as.Date("2018-08-05"), y = 105, label = "↑ +142%", size=3) 


plot1
























