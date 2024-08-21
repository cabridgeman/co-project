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

library("reshape2")

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

data <- bind_rows(matrix, synergy, unity, overseas, defence)

data$admin_total <- data$admin_total / 1000
data$admin_staff <- data$admin_staff  / 1000
data$admin_running_costs <- data$admin_running_costs / 1000
data$Date_Published <- as.Date(data$Date_Published, format = "%Y-%m-%d")


plot1 <- ggplot(data, aes(x = Date_Published, y = admin_total, colour = Cluster)) +
  stat_smooth(
    geom = 'line', method = 'loess', span = 1/4, alpha = 1, size=1.5
  ) +
  
  scale_colour_manual(
    values = c("Matrix" = "#f58231", "Synergy" = "#e6194B", "Unity" = "#911eb4", 'Overseas' = '#42d4f4', 'Defence' = '#3cb44b')) +
  labs(title = "Admin Total Over Time by Cluster", x = "Date Published", y = "Admin Total") +
  theme_cowplot(12)


plot1









