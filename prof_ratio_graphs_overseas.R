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
library(tidyr)

library(scales)
library(reshape2)

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




plot1 <- ggplot(data, aes(x=Date_Published, y=Finance_q2_Concentration))+
  geom_bar(stat = "identity", position = position_dodge(), width=300, fill='#002F5D') +
  theme_classic()  +
  #geom_text(size = 2.2, position = position_dodge(width =300), vjust = -1, hjust=0.55, aes(label = paste0(round(Finance_q3_Concentration * 100), "%"))) +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(labels = scales::label_percent(scale=100)) + 
  scale_x_date(breaks = seq.Date(from = as.Date("2017-06-30"), to = as.Date("2024-06-30"), by = "12 month"), 
               date_labels = "%Y") +
  labs(title = "Finance Profession Concentration in Overseas Cluster \n",
       x = '\n Year',
       y = (y=expression(atop(bold("Concentration Ratio"),atop(italic("(at 50th percentile)*")))))) +
  scale_fill_manual(
    values = c()) +
  labs(x = "\n Year") 


plot1
























