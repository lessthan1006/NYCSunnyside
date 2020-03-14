install.packages("DBI")
install.packages("odbc")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("stringr")
install.packages("forecast")

library(dplyr)
library(DBI)
library(odbc)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(stringr)
library(forecast)


con<-dbConnect(odbc(),
               Driver = "SQL Server",
               server="met-sql19.bu.edu",
               database="NYC Real Estate")

Borough<- dbReadTable(con,"Borough")
NYCCurrent <- dbReadTable(con, "NYCCurrent")
BuildingCode<- dbReadTable(con,"BuildingCode")
Neighborhood<- dbReadTable(con,"Neighborhood")%>%
  mutate(NbhoodName=str_trim(NbhoodName))
NYCHistorical<- dbReadTable(con,"NYCHistorical")

df.Historical<- NYCHistorical %>%
  left_join(Neighborhood,by="NbhoodID") %>%
  left_join(BuildingCode,by=c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  mutate(Year=year(SaleDate)) %>%
  filter(NbhoodName=="SUNNYSIDE",SalePrice!=0,GrossSqFt!=0,LandSqFt!=0,Status=="Residential")%>%
  mutate(SalesQrt=quarter(SaleDate))


#1
df.Historical1<-filter(df.Historical,Year>=2009)%>%
  mutate(t=Year*4+SalesQrt-2008*4-4)%>%
  filter(t!=0)%>%
  group_by(t)%>%
  summarise(TotalSale=sum(SalePrice))

ts.1<-ts(df.Historical1$TotalSale,start =c(2009,1),frequency = 4 )
ts.model<-ets(ts.1,model="ZAA")
forecast(ts.model,8)
plot(forecast(ts.model,8))


#2
df.Historical2<-cbind(df.Historical1,c("Q1","Q2","Q3","Q4"))
names(df.Historical2)[3]<-"Quarter"

reg<-lm(data=df.Historical2,formula=TotalSale~Quarter+t)
summary(reg)

X<-data.frame(t=c(33,34,35,36,37,38,39,40),TotalSale=c(0,0,0,0,0,0,0,0),Quarter=c("Q1","Q2","Q3","Q4"))
predict.lm(reg,X,interval = "confidence")


#3, 4, 5
df.Historical3<-NYCHistorical %>%
  left_join(Neighborhood,by="NbhoodID") %>%
  left_join(BuildingCode,by=c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  mutate(Year=year(SaleDate)) %>%
  filter(NbhoodName=="SUNNYSIDE",SalePrice!=0,GrossSqFt!=0,LandSqFt!=0,Status=="Residential")%>%
  group_by(BuildingClassFinalRoll)

model<-lm(formula=SalePrice~SaleDate+YearBuilt+BuildingClassFinalRoll+GrossSqFt+ResidentialUnits,data=df.Historical3)
summary(model)


#6
df.Historical4<-NYCHistorical %>%
  left_join(Neighborhood,by="NbhoodID") %>%
  left_join(BuildingCode,by=c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  mutate(Year=year(SaleDate)) %>%
  filter(NbhoodName=="SUNNYSIDE",SalePrice!=0,GrossSqFt!=0,LandSqFt!=0,Status=="Residential")%>%
  mutate(model$residual)%>%
  group_by(BuildingClassFinalRoll)
View(df.Historical4)


