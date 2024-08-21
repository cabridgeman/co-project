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

#matrix$HR_q3_Concentration <- matrix$HR_q3_Concentration * 100
#matrix$Finance_q3_Concentration <- matrix$Finance_q3_Concentration * 100


# independent variable x axis, dependent y axis

#plot1 <- ggplot(matrix, 
    #aes(x=Date_Published, y=HR_q3_Concentration, fill="HR Ratio at Q3")) +
   # geom_col()

#print(plot1)


matrix_long <- matrix %>%
  pivot_longer(cols = c(HR_q3_Concentration, Finance_q3_Concentration),
               names_to = "Concentration_Type",
               values_to = "Value") %>%
  mutate(Concentration_Type = recode(Concentration_Type,
                                     "HR_q3_Concentration" = "HR Ratio at Q3",
                                     "Finance_q3_Concentration" = "Finance Ratio at Q3"))


custom_colors <- c(
  "HR Ratio at Q3" = "#2066a8",  
  "Finance Ratio at Q3" = "#8cc5e3"  
)



plot2 <- ggplot(matrix_long, aes(x = Date_Published, y = Value, fill = Concentration_Type)) +
  geom_col(position = "dodge", width=300) +
  labs(title = "Profession Concentration by Year in Matrix Cluster",
       x = "Year",
       y = "Concentration Value",
       fill = "Profession") +
  theme_cowplot(12) +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        title = element_text(size=16),
        axis.text.x = element_text(size = 13),   # Size of x-axis tick labels
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size=13)) +
  scale_x_date(breaks = seq.Date(from = as.Date("2017-06-30"), to = as.Date("2024-06-30"), by = "12 month"), 
               date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5),  
                     breaks = seq(0, 0.5, by = 0.1)) +
  scale_fill_manual(values = custom_colors)

print(plot2)

