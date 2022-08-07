#Import required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(plotly)

#Read CO2 dataset file
df <- read.csv("co2-data.csv")
clean_df <- na.omit(df)
head(clean_df)


#Categorize country and years
country<- clean_df%>%
  select(country,co2, consumption_co2)%>%
  filter(country != "World")%>%
  group_by(country)%>%
  summarise(Average_co2 = mean(co2), Average_consumption_co2 = mean(consumption_co2))%>%
  arrange(desc(Average_co2))
head(country)


year<- clean_df%>%
  select(year,co2,consumption_co2)%>%
  group_by(year)%>%
  summarise(Total_co2 = sum(co2),Total_consumption_co2 = sum(consumption_co2))%>%
  arrange(desc(Total_co2))
head(year)

#plot the data
country <- clean_df %>%
  select(country, year, co2, consumption_co2, co2_per_capita,co2_per_gdp,co2_per_unit_energy) %>%
  filter(country != "World") %>%
  group_by(country, year) %>%
  summarise(
    Average_co2 = round(mean(co2),2),
    Average_consumption_co2 = round(mean(consumption_co2),2),
    Average_co2_per_capita = round(mean(co2_per_capita),2),
    Average_co2_per_gdp = round(mean(co2_per_gdp),2),
    Average_co2_per_unit_energy = round(mean(co2_per_unit_energy),2)
  ) %>%
  arrange(Average_co2)

country

library(plotly)
plot_ly(data = country, x=~Average_co2, y= ~Average_consumption_co2 )



year <- clean_df %>%
  select(year, co2, consumption_co2,co2_per_capita,co2_per_gdp,co2_per_unit_energy) %>%
  group_by(year) %>%
  summarise(
    Average_co2 = round(mean(co2),2),
    Average_consumption_co2 = round(mean(consumption_co2),2),
    Average_co2_per_capita = round(mean(co2_per_capita),2),
    Average_co2_per_gdp = round(mean(co2_per_gdp),2),
    Average_co2_per_unit_energy = round(mean(co2_per_unit_energy),2)
  ) %>%
  arrange(Average_co2)

plot_ly(data = year,x=~year,y=~Average_co2, type = "bar")