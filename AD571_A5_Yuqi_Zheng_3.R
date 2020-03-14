install.packages("DBI")
install.packages("odbc")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("stringr")
install.packages("ggplot2")

library(DBI)
library(odbc)
library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)

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

#1a total number of sales since 2009
df.Historical <- NYCHistorical%>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  mutate(Year = year(SaleDate)) %>%
  filter(NbhoodName == "SUNNYSIDE",SalePrice!=0,GrossSqFt!=0,LandSqFt!=0, Year >= 2009)

summarise(df.Historical, TotalSale=sum(SalePrice), n=n())


#1b mean sale price and gross square footage for residential since 2009
df.Historical1 <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  mutate(Year = year(SaleDate)) %>%
  filter(NbhoodName == "SUNNYSIDE", Status == "Residential") %>%
  filter(GrossSqFt!=0, SalePrice!=0, LandSqFt!=0, Year >= 2009)

summarise(df.Historical1, MeanSqFt=mean(GrossSqFt), MeanSale=mean(SalePrice))


#1c five number summary of sale price and gross square feet for residential since 2009
quantile(df.Historical1$GrossSqFt)
quantile(df.Historical1$SalePrice)


#1d proportion status since 2009
prop.table(table(df.Historical$Status))
unique(df.Historical$Status)

#1e standard deviation of sale price for residential since 2009
summarise(df.Historical1, SDSalePrice = sd(SalePrice))


#1f correlation between sale price and gross square feet for residential since 2009
df.Historical2 <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  mutate(Year = year(SaleDate)) %>%
  filter(NbhoodName == "SUNNYSIDE", Status == "Residential") %>%
  filter(GrossSqFt!=0, SalePrice!=0, LandSqFt!=0, Year >= 2009) %>%
  select(Year, SalePrice, GrossSqFt)

cor(df.Historical2[c(-1)])


#2 k-means//total sale and total land of all residential neighborhoods since 2009 and filter outlier
df.Historical3 <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  mutate(Year = year(SaleDate)) %>%
  filter(Status == "Residential", GrossSqFt!=0, SalePrice!=0, LandSqFt!=0, Year >= 2009) %>%
  group_by(NbhoodName) %>%
  summarise(TotalSale=sum(SalePrice), TotalLand=sum(GrossSqFt), UnitPrice=TotalSale/TotalLand) %>%
  filter(UnitPrice!="Inf") %>%
  filter(TotalSale < 8.0e+09)

zscores <- scale(df.Historical3[c(-1)]) %>%
  as.data.frame()
k <- kmeans(zscores, centers = 5)

ggplot(df.Historical3)+geom_point(mapping = aes(x = TotalSale, y = TotalLand, size = UnitPrice, color = k$cluster))


#2 k-means clustering// three KPI: standard dev, median and price/GrossSqFt and filter outlier 
df.Historical4 <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  mutate(Year = year(SaleDate)) %>%
  filter(Status == "Residential", GrossSqFt!=0, SalePrice!=0, LandSqFt!=0, Year >= 2009) %>%
  group_by(NbhoodName) %>%
  summarise(Price=sum(SalePrice)/sum(GrossSqFt), MedSalePrice = median(SalePrice), SD=sd(SalePrice)) %>%
  filter(SD!="NaN", Price!="Inf") %>%
  filter(SD < 5e+07)

zscores1 <- scale(df.Historical4[c(-1)]) %>%
  as.data.frame()
k1 <- kmeans(zscores1, centers = 5)

S <- subset(df.Historical4, NbhoodName == "SUNNYSIDE")

ggplot(data=df.Historical4, aes(x = MedSalePrice, y = SD, size = Price, color = k1$cluster))+geom_point()+
                                    scale_color_gradient(low = "yellow", high = "darkblue")+
  geom_point(data = S, color = "red")


#3 t-test
df.SUNNYSIDE <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  mutate(Year = year(SaleDate)) %>%
  filter(NbhoodName == "SUNNYSIDE", Status == "Residential", GrossSqFt!=0, SalePrice!=0, LandSqFt!=0, Year >= 2009) 

df.WHITESTONE <- NYCHistorical %>%
  left_join(Neighborhood,by ="NbhoodID") %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(SaleDate,SalePrice,NbhoodName,GrossSqFt,LandSqFt,Status) %>%
  mutate(Year = year(SaleDate)) %>%
  filter(NbhoodName == "WHITESTONE", Status == "Residential", GrossSqFt!=0, SalePrice!=0, LandSqFt!=0, Year >= 2009) 

t.test(x = df.SUNNYSIDE$SalePrice, y = df.WHITESTONE$SalePrice, alternative = "t", conf.level = 0.95)
t.test(x = df.SUNNYSIDE$SalePrice, y = df.WHITESTONE$SalePrice, alternative = "g", conf.level = 0.99)

