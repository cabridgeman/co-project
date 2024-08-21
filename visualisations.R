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
matrix$Date_Published <- as.Date(matrix$Date_Published, format = "%Y-%m-%d")  # Adjust format as needed


matrix$admin_total <- matrix$admin_total / 1000
matrix$admin_staff <- matrix$admin_staff  / 1000
matrix$admin_running_costs <- matrix$admin_running_costs / 1000


# independent variable x axis, dependent y axis

library(ggplot2)
library(cowplot)  # For theme_cowplot



plot1 <- ggplot(matrix, aes(x = Date_Published)) +
  # Area graph for admin_total
  stat_smooth(
    aes(y = admin_total, fill = "admin_total"),
    geom = 'area', method = 'loess', span = 1/4,
    alpha = 0.5) + 
  # Line graph for admin_staff
  geom_smooth(aes(y = admin_staff, color = "admin_staff"),
              method = 'loess', span = 1/4,
              size = 1.6, se = FALSE) +
  # Line graph for admin_running_costs
  geom_smooth(aes(y = admin_running_costs, colour = "admin_running_costs"),
              method = 'loess', span = 1/4,
              size = 1.6, se = FALSE) +
  labs(title = "Administrative Expenditure in Matrix Cluster \n",
       x = '\n Year',
       y = (y=expression(atop(bold("Quarterly Expenditure"),atop(italic("(£100,000)")))))) +
  theme_cowplot(12) +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.margin = margin(t=-31)) +
  scale_x_date(breaks = seq.Date(from = as.Date("2017-06-30"), to = as.Date("2024-06-30"), by = "12 month"), 
               date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 3000, by = 500)) +
  scale_fill_manual(values = c("admin_total" = "#CCCCCC"), labels=c("Admin Total")) +
  scale_color_manual(values = c(
    "admin_staff" = "#DE8926",
    "admin_running_costs" = "#31a354"
  ), labels = c("Admin Running Costs", "Admin Staff")) +
  guides(
    fill = guide_legend(title = "Expenditure Type", order = 1),
    color = guide_legend(title = " ",),
    linetype = guide_legend(title = " ")
  )

print(plot1)



