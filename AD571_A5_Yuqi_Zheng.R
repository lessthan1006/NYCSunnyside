install.packages("DBI")
install.packages("odbc")
install.packages("tidyverse")
install.packages("stringr")
install.packages("lubridate")

library(DBI)
library(odbc)
library(tidyverse)
library(stringr)
library(lubridate)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 server = "met-sql19.bu.edu",
                 database = "NYC Real Estate",
                 Port = 1433)

Borough <- dbReadTable(con, "Borough")
NYCCurrent <- dbReadTable(con, "NYCCurrent")
BuildingCode <- dbReadTable(con, "BuildingCode")
Neighborhood <- dbReadTable(con, "Neighborhood") %>%
  mutate(NbhoodName=str_trim(NbhoodName))
NYCHistorical <- dbReadTable(con, "NYCHistorical")


df.Historical <- NYCHistorical%>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  group_by(Year = year(SaleDate)) %>%
  filter(NbhoodName == "SUNNYSIDE", Status == "Residential")

summarise(df.Historical, TotalFT = sum(GrossSqFt),
          TotalSales = sum(SalePrice)) %>%
  mutate(Average=TotalSales/TotalFT)

df.Historical1 <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  group_by(Year = year(SaleDate)) %>%
  filter(NbhoodName == "SUNNYSIDE", Status == "Residential") %>%
  filter(GrossSqFt!=0, SalePrice!=0, LandSqFt!=0)

Result1 <- summarise(df.Historical1,Totalland=sum(GrossSqFt),
          TotalSales=sum(SalePrice)) %>%
  mutate(Average=TotalSales/Totalland)

df.Historical2 <- NYCHistorical%>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  group_by(Year = year(SaleDate)) %>%
  filter(NbhoodName == "WHITESTONE", Status == "Residential")

summarise(df.Historical2, TotalFT = sum(GrossSqFt),
          TotalSales = sum(SalePrice)) %>%
  mutate(Average=TotalSales/TotalFT)

df.Historical3 <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  group_by(Year = year(SaleDate)) %>%
  filter(NbhoodName == "WHITESTONE", Status == "Residential") %>%
  filter(GrossSqFt!=0, SalePrice!=0, LandSqFt!=0)

Result2 <- summarise(df.Historical3, TotalFT = sum(GrossSqFt),
          TotalSales = sum(SalePrice)) %>%
  mutate(Average=TotalSales/TotalFT)

df.Historical4 <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale" = "BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  group_by(Year = year(SaleDate)) %>%
  filter(NbhoodName == "ANNADALE", Status == "Residential")

summarise(df.Historical4, TotalFT = sum(GrossSqFt),
          TotalSales = sum(SalePrice)) %>%
  mutate(Average=TotalSales/TotalFT)
  
df.Historical5 <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale" = "BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  group_by(Year = year(SaleDate)) %>%
  filter(NbhoodName == "ANNADALE", Status == "Residential") %>%
  filter(GrossSqFt!=0, SalePrice!=0, LandSqFt!=0)

Result3 <- summarise(df.Historical5, TotalFT = sum(GrossSqFt),
                     TotalSales = sum(SalePrice)) %>%
  mutate(Average=TotalSales/TotalFT) 


p = ggplot()+
  geom_line(data = Result1, aes(x=Year, y=Average), color = "blue") +
  geom_line(data = Result2, aes(x=Year, y=Average), color = "red") +
  geom_line(data = Result3, aes(x=Year, y=Average), color = "brown")

print(p)


