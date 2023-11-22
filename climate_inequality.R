install.packages("naniar")
library(tidyverse)
library(readxl)
library(ggplot2)
library(fastDummies)
library(naniar)

emdat <- read_excel("/Users/nourhanghanima/Downloads/public_emdat_incl_hist_2023-11-20.xlsx")

#slicing the data
natural_disasters <- subset(emdat, select = c("Disaster Type", "ISO", "Country", "Start Year",
                                              "Total Deaths")) |> 
                    mutate(Country = as.factor(Country)) |>
                    rename(year = "Start Year") |>
                    mutate(year = as.numeric(year)) |>
                    rename(total_deaths = "Total Deaths")



#calculating yearly deaths  
yearly_deaths <- natural_disasters |>
                group_by(year) |>
                mutate(total_yearly_deaths = sum(total_deaths, na.rm = TRUE)) |>
                distinct(year, .keep_all = TRUE)

#calculating total deaths by natural disaster type
yearly_deaths <- yearly_deaths |>
              rename(disaster_type = "Disaster Type") |>
              group_by(disaster_type) |>
              mutate(death_by_disaster = sum(total_deaths, na.rm = TRUE))

disaster_deaths <- yearly_deaths |>
                  distinct(disaster_type, .keep_all = TRUE)

#creating plot of progression of yearly deaths 
yearly_deaths_plot <- ggplot(yearly_deaths, aes(x = total_yearly_deaths, y = year)) +
  geom_line()

#creating plot of deaths per type of disaster
disaster_type_plot <- ggplot(disaster_deaths, aes(x=death_by_disaster, y = disaster_type)) +
  geom_col()


#including the Gini Index of Inequality to the data 
#cleaning up Gini data 
gini_data <- read_csv("/Users/nourhanghanima/downloads/P_Data_Extract_From_World_Development_Indicators/99d7fa98-918e-47a2-b5d7-158caabff942_Data.csv")
gini_data<- gini_data |>
  pivot_longer("1990 [YR1990]":"2022 [YR2022]", names_to = "year", values_to = "Gini_index") |>
  rename(gini_index = "Gini_index") |>
  rename(Country = "Country Name") |>
  mutate(inequality_level = case_when(
    gini_index >= 0 & gini_index < 30 ~ "low inequality",
    gini_index >= 30 & gini_index < 50 ~ "moderate inequality",
    gini_index > 50 ~ "high inequality"
  )) |>
  mutate(moderate_inequality = case_when(
    inequality_level == "moderate inequality" ~ 1
  )) |>
  mutate(high_inequality = case_when(
    inequality_level == "high inequality" ~ 1
  ))

gini_data$year <- as.numeric(substr(gini_data$year, 0, 4))

#including GDP data 
#cleaning up GDP data
gdp_data <- read_csv("/Users/nourhanghanima/downloads/GDP Data World Bank/fc317218-35b2-4fa4-a763-e06db0661e80_Data.csv")

gdp_data <- gdp_data |>
            pivot_longer("1990 [YR1990]": "2022 [YR2022]", names_to = "year", values_to = "GDP") |>
            rename(Country = "Country Name")
            
gdp_data$year <- as.numeric(substr(gdp_data$year, 0, 4))


#including the share of value added by agriculture to GDP
agriculture_data <- read_csv("/Users/nourhanghanima/downloads/Agriculture Data WorldBank/0de3cd5d-6e66-42cf-9bd4-6dfbbe7a6563_Data.csv")

agriculture_data <- agriculture_data |>
  pivot_longer("1990 [YR1990]": "2022 [YR2022]", names_to = "year", values_to = "agriculture_share") |>
  rename(Country = "Country Name")

agriculture_data$year <- as.numeric(substr(agriculture_data$year, 0, 4))


#including public expenditure data 
public_expenditure <- read_excel("/Users/nourhanghanima/downloads/imf-dm-export-20231122.xls")
public_expenditure <- public_expenditure |>
  mutate(across(2:223, as.numeric))

public_expenditure <- public_expenditure |>
                      mutate(across(where(is.character), ~na_if(., "no data"))) |>
                      pivot_longer("1800": "2021", names_to = "year", values_to="public_spending") |>
                      rename(Country = "Government expenditure, percent of GDP (% of GDP)") 
public_expenditure$year <- as.numeric(public_expenditure$year)




#merging the data sets together 
inequality_climate_data <- left_join(natural_disasters, gini_data, by = c("Country", "year"), 
                                     relationship = "many-to-many")
inequality_climate_data <- full_join(inequality_climate_data, yearly_deaths, by=c("Country", "year"), 
                                     relationship = "many-to-many")
inequality_climate_data <- left_join(inequality_climate_data, gdp_data, by=c("Country", "year"), 
                                     relationship="many-to-many")
inequality_climate_data <- left_join(inequality_climate_data, agriculture_data, by = c("Country", "year"),
                                     relationship = "many-to-many")
inequality_climate_data <- left_join(inequality_climate_data, public_expenditure, by = c("Country", "year"), 
                                     relationship = "many-to-many")

inequality_climate_data <- yearly_deaths |>
  group_by(year) |>
  mutate(total_yearly_deaths = sum(total_deaths))

#does high inequality have a relationship with total deaths resulting from natural disasters?
climate_inequality <- lm(total_yearly_deaths ~ public_spending + agriculture_share + 
                           gini_index, data = inequality_climate_data)




                    




                          


                  
