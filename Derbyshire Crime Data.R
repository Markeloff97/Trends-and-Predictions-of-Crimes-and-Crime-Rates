library(tidyverse)
library(dplyr)
library(readxl)
library(rgdal)
library(ggplot2)
library(tmap)
library(raster)


#Reading the shapefile for derbyshire
derbyshire <- readOGR(dsn="./Derbyshire Shapefile", layer="england_lsoa_2011")

#Reading the crime data for derbyshire
derbyData <- read_xlsx("Derbyshire Crime Data.xlsx")

#Renaming columns for easier access

derbyData <- rename(derbyData, c("LSOA code"="LSOA_code",
                                 "Crime type"="crime_type"))

#Removing rows containing NA values
derbyData <- derbyData[!is.na(derbyData$Longitude),]



#Joining shapefile and crime data based on LSOA code
derbyshire@data <- left_join(derbyshire@data, derbyData,
                            by=c('code'='LSOA_code'))

#Removing unwanted columns
derbyshire@data <- subset(derbyshire@data, select = c(crime_type,LSOA_code,Longitude,Latitude,Month))



#***********************CODE FOR TREND LINES***********************************************************


#Creating a new variable to plot trend lines
derby_trend <- group_by(derbyshire@data,Month,crime_type) %>% dplyr::summarise(count=n())

#Subsetting the top 5 crimes happeing in South Yorkshire over the years
derby_trend <- subset(derby_trend, derby_trend$crime_type %in% c("Burglary","Criminal damage and arson",
                                       "Other theft","Violence and sexual offences","Vehicle crime"))

#changing the data type of Month column from double to data format
derby_trend$Month <- as.Date(derby_trend$Month)

#PLotting trend lines for different crimes from 2017-11 to 2020-03
ggplot(data = derby_trend,aes(x=Month, y=count, color= crime_type))+ 
  geom_line(size=1)+ ylim(0,150)+
  scale_x_date(labels= date_format("%Y-%m"),breaks = date_breaks("3 months"))+ #scaling x-axis on gaps of 2 months
  scale_color_manual(values = c("violet", "orange","green","darkblue","red"))+ #custom colors for crime types
  labs(title = "Trends of different crimes over the years (Derbyshire)", 
       caption = "source:UK crime data", x="Date", y="Number of Crimes", color="Crime Types")


#**********************************CODE FOR HEAT MAP BASED ON NO OF CRIMES************************************

#Creating extra variable for Heat Map
derby_heatMap <- derbyshire

#counting the number of LSOA_code which inturn gives the number of crimes in that region
LSOA_count_derby <- group_by(derby_heatMap@data,code,) %>% dplyr::summarise(count=n())

#joining the two data
derby_heatMap@data <- join(derby_heatMap@data, LSOA_count_derby,by=c('code'='code'))

#filtering all the duplicate LSOA_codes from the data_frame
derby_heatMap@data <- derby_heatMap@data %>% filter(duplicated(derby_heatMap@data["code"])==FALSE)

#changing the mode of tmap to interactive
tmap_mode("view")

#plotting the crime count data 
tm_shape(derby_heatMap)+tm_fill("count",palette=c("yellow","red"), 
                              breaks=c(1,100,150,300,450,650),
                              title ="No of Crimes") +
  tm_layout(legend.outside = TRUE)+ tm_borders() +
  tm_layout(main.title = "Heat Map for South Yorkshire", title.size = 1)




#*******************************CODE FOR HEAT MAP BASED ON MAX NO OF A CRIME IN A REGION******************************************

#Creating extra variable for Heat Map
derby_maxCrime <- derbyshire

#to count the no of each type of crime happening in a region
crime_count_derby <- group_by(derby_maxCrime@data,code,crime_type) %>% dplyr::summarise(count=n())

#changing the name to avoid clash in-built function count()
crime_count_derby <- rename(crime_count_derby,c("count"="Count"))

#filtering duplicate LSOA_codes in the data.frame
derby_maxCrime@data <- derby_maxCrime@data %>% filter(duplicated(derby_maxCrime@data["code"])==FALSE)

#obtaning max count of a crime happeing in aa region
crime_count_derby <- setDT(crime_count_derby)[order(-Count,code), head(.SD, 1), by = code]


#joining both data.frames
derby_maxCrime@data <- join(derby_maxCrime@data, crime_count_derby,by=c('code'='code'))


#interactive mode
tmap_mode("view")

#plotting data based on crime_type to get heatmap of which region has which crime the most
tm_shape(derby_maxCrime)+tm_fill("crime_type",title ="Types of Crimes") +
  tm_layout(legend.outside = TRUE)+ tm_borders() +
  tm_layout(main.title = "Heat Map for South Yorkshire", title.size = 1)









#********************************************************************************
#View(derbyData)
#View(derbyshire@data)
View(duplicate)
duplicate<-derbyshire@data

duplicate<-filter(duplicate, duplicate$crime_type=="Burglary" | duplicate$crime_type=="Criminal damage and arson")
a<-duplicate%>% group_by( Month,crime_type)%>%dplyr::summarise(count=n())
View(a)

