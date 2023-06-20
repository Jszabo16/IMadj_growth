setwd("C:\\Users") # set working dir 
final <- read.csv("Final.csv") # data set containing only the import-adjusted FD components for V4 countries and the Baltics

################################################################
## Required libraries 
library(tidyverse)
library(gridExtra)
library(zoo)
################################################################library(zoo)

## pivoting the csv. data
final_pivoted <- pivot_longer(final, 
                        cols = GrowthRate_C:GrowthRate_EX,
                        names_to = "Categories",
                        values_to = "Value") 

## renaming the import-adjusted growth categories 
final_pivoted <- final_pivoted %>% 
  mutate(Categories = case_when(
    Categories == "GrowthRate_C" ~ "Import-adjusted Private Consumption",
    Categories == "GrowthRate_I" ~ "Import-adjusted Investment",
    Categories == "GrowthRate_G" ~ "Import-adjusted Government Consumption",
    Categories == "GrowthRate_EX" ~ "Import-adjusted Exports",
    TRUE ~ Categories
  ))

## Subsetting countries into separate dfs
## Computing 3-year moving averages 

Czechia <- final_pivoted %>% 
  filter(Country == "Czechia") %>% 
  mutate(moving_avg = ave(Value, Categories, FUN = function(x) rollmean(x, k = 3, align = "right", fill = NA)))

Hungary <- final_pivoted %>% 
  filter(Country == "Hungary") %>% 
  mutate(moving_avg = ave(Value, Categories, FUN = function(x) rollmean(x, k = 3, align = "right", fill = NA)))

Poland <- final_pivoted %>% 
  filter(Country == "Poland") %>% 
  mutate(moving_avg = ave(Value, Categories, FUN = function(x) rollmean(x, k = 3, align = "right", fill = NA)))

Slovakia <- final_pivoted %>% 
  filter(Country == "Slovakia") %>% 
  mutate(moving_avg = ave(Value, Categories, FUN = function(x) rollmean(x, k = 3, align = "right", fill = NA)))

Estonia <- final_pivoted %>% 
  filter(Country == "Estonia") %>% 
  mutate(moving_avg = ave(Value, Categories, FUN = function(x) rollmean(x, k = 3, align = "right", fill = NA)))

Latvia <- final_pivoted %>% 
  filter(Country == "Latvia") %>% 
  mutate(moving_avg = ave(Value, Categories, FUN = function(x) rollmean(x, k = 3, align = "right", fill = NA)))

Lithuania <- final_pivoted %>% 
  filter(Country == "Lithuania") %>% 
  mutate(moving_avg = ave(Value, Categories, FUN = function(x) rollmean(x, k = 3, align = "right", fill = NA)))

## Plotting the moving-averages using stacked barplot 
CZ <- ggplot(Czechia, aes(fill = Categories, y = moving_avg, x = Year))+
  geom_bar(position = "stack", stat = "identity") + 
  labs(title = "Czechia", x = "", y = "Growth Contribution in %") + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))+
  scale_fill_manual(
    name = "Categories",
    values = c("Import-adjusted Private Consumption" = "lightblue", 
               "Import-adjusted Investment" = "darkolivegreen2", 
               "Import-adjusted Government Consumption" = "coral",
               "Import-adjusted Exports" = "darkgoldenrod1"
               )) +
  guides(fill = guide_legend(title = NULL))

HU <- ggplot(Hungary, aes(fill = Categories, y = moving_avg, x = Year))+
  geom_bar(position = "stack", stat = "identity") + 
  labs(title = "Hungary", x = "", y = "Growth Contribution in %") + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))+
  scale_fill_manual(
    name = "Categories",
    values = c("Import-adjusted Private Consumption" = "lightblue", 
               "Import-adjusted Investment" = "darkolivegreen2", 
               "Import-adjusted Government Consumption" = "coral",
               "Import-adjusted Exports" = "darkgoldenrod1"
    ))+
  guides(fill = guide_legend(title = NULL))

PL <- ggplot(Poland, aes(fill = Categories, y = moving_avg, x = Year))+
  geom_bar(position = "stack", stat = "identity") + 
  labs(title = "Poland", x = "", y = "Growth Contribution in %") + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))+
  scale_fill_manual(
    name = "Categories",
    values = c("Import-adjusted Private Consumption" = "lightblue", 
               "Import-adjusted Investment" = "darkolivegreen2", 
               "Import-adjusted Government Consumption" = "coral",
               "Import-adjusted Exports" = "darkgoldenrod1"
    ))+
  guides(fill = guide_legend(title = NULL))

SK <- ggplot(Slovakia, aes(fill = Categories, y = moving_avg, x = Year))+
  geom_bar(position = "stack", stat = "identity") + 
  labs(title = "Slovakia", x = "", y = "Growth Contribution in %") + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))+
  scale_fill_manual(
    name = "Categories",
    values = c("Import-adjusted Private Consumption" = "lightblue", 
               "Import-adjusted Investment" = "darkolivegreen2", 
               "Import-adjusted Government Consumption" = "coral",
               "Import-adjusted Exports" = "darkgoldenrod1"
    ))+
  guides(fill = guide_legend(title = NULL))

EE <- ggplot(Estonia, aes(fill = Categories, y = moving_avg, x = Year))+
  geom_bar(position = "stack", stat = "identity") + 
  labs(title = "Estonia", x = "", y = "Growth Contribution in %") + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))+
  scale_fill_manual(
    name = "Categories",
    values = c("Import-adjusted Private Consumption" = "lightblue", 
               "Import-adjusted Investment" = "darkolivegreen2", 
               "Import-adjusted Government Consumption" = "coral",
               "Import-adjusted Exports" = "darkgoldenrod1"
    ))+
  guides(fill = guide_legend(title = NULL))

LV <- ggplot(Latvia, aes(fill = Categories, y = moving_avg, x = Year))+
  geom_bar(position = "stack", stat = "identity") + 
  labs(title = "Latvia", x = "", y = "Growth Contribution in %") + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))+
  scale_fill_manual(
    name = "Categories",
    values = c("Import-adjusted Private Consumption" = "lightblue", 
               "Import-adjusted Investment" = "darkolivegreen2", 
               "Import-adjusted Government Consumption" = "coral",
               "Import-adjusted Exports" = "darkgoldenrod1"
    ))+
  guides(fill = guide_legend(title = NULL))

LT <- ggplot(Lithuania, aes(fill = Categories, y = moving_avg, x = Year))+
  geom_bar(position = "stack", stat = "identity") + 
  labs(title = "Lithuania", x = "", y = "Growth Contribution in %") + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))+
  scale_fill_manual(
    name = "Categories",
    values = c("Import-adjusted Private Consumption" = "lightblue", 
               "Import-adjusted Investment" = "darkolivegreen2", 
               "Import-adjusted Government Consumption" = "coral",
               "Import-adjusted Exports" = "darkgoldenrod1"
    ))+
  guides(fill = guide_legend(title = NULL))

## Putting all together 
grid.arrange(CZ, HU, PL, SK, nrow = 2, ncol = 2)
grid.arrange(EE, LV, LT, nrow = 2, ncol = 2)
