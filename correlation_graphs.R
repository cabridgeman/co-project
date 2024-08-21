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

matrix$Date_Published <- as.Date(matrix$Date_Published, format = "%Y-%m-%d")


# independent variable = x axis, dependent variable = y axis

plot1 <- ggplot(matrix, aes(x=Finance_q3_concentration_ncsi, y=ln_Admin_Staff_Intensity)) +
  geom_smooth(method='lm', colour='black', fill='lightgrey', size=1.2) +
  geom_point(shape=18, size=3.5, colour='#7cc573') +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(size = 16, vjust = -1.4),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  labs(title = "Finance Profession Concentration on Admin Staff \nExpenditure Intensity in Matrix Cluster \n",
       x = (x=expression(atop(bold("Finance Profession Concentration"),atop(italic("(at 75th percentile)"))))),
       y = (y=expression(atop(bold("Administrative Staff Expenditure Intensity"),atop(italic("(log transformed (base e))")))))) +
  scale_x_continuous(
    labels = function(x) paste0(formatC(x * 100, format = "f", digits = 0), "%"),  
    limits = c(0.1468786, 0.2382069),
    breaks = seq(0.14, 0.24, by = 0.01)
  )

plot1                
                



plot2 <- ggplot(unity, aes(x=HR_q3_concentration_ncsi, y=ln_Admin_Staff_Intensity)) +
  geom_smooth(method='lm', colour='black', fill='lightgrey', size=1.2) +
  geom_point(shape=18, size=3.5, colour='#72c3c5') +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(size = 16, vjust = -1.4),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  labs(title = "HR Profession Concentration on Admin Staff \nExpenditure Intensity in Unity Cluster \n",
       x = (x=expression(atop(bold("HR Profession Concentration"),atop(italic("(at 75th percentile)"))))),
       y = (y=expression(atop(bold("Administrative Staff Expenditure Intensity"),atop(italic("(log transformed (base e))")))))) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 100), 
    limits = c(0.4595972, 0.4859033),
    breaks = seq(0.40, 0.5, by = 0.005)
  )

plot2        

plot3 <- ggplot(unity, aes(x=Finance_q3_concentration_ncsi, y=ln_Total_Admin_Intensity)) +
  geom_smooth(method='lm', colour='black', fill='lightgrey', size=1.2, linetype='dashed') +
  geom_point(shape=18, size=3.5, colour='#73c4c5') +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(size = 16, vjust = -1.4),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  labs(title = "Finance Profession Concentration on Total Admin \nExpenditure Intensity in Unity Cluster \n",
       x = (x=expression(atop(bold("Finance Profession Concentration"),atop(italic("(at 75th percentile)"))))),
       y = (y=expression(atop(bold("Total Administrative Expenditure Intensity"),atop(italic("(log transformed (base e))")))))) +
  scale_x_continuous(
    labels = function(x) paste0(formatC(x * 100, format = "f", digits = 0), "%"),  
    limits = c(0.4569027, 0.4738095),
    breaks = seq(0.4469027, 0.5, by = 0.005)
  )

plot3 
                
                
                
                