library(tidyverse)
library(dplyr)
library(readxl)
library(tmap)
library(rgdal)
library(ggplot2)
library(mapdata)
library(raster)
#Obtaining shapefiles for different regions of South Yorkshire
sheffield <- readOGR(dsn="./BoundaryData", layer="england_lsoa_2011")
barnsley <- readOGR(dsn="./Barnsley", layer="england_lsoa_2011")
rotherham <- readOGR(dsn="./Rotherham", layer="england_lsoa_2011")
doncaster <- readOGR(dsn="./Doncaster", layer="england_lsoa_2011")

#Merging shapefiles to create a single shapefile

s_Yorkshire <- raster::union(sheffield,barnsley)
s_Yorkshire <- raster::union(s_Yorkshire,rotherham)
s_Yorkshire <- raster::union(s_Yorkshire,doncaster)

#arranging data from similar category together(Data pre-processing)
a <- s_Yorkshire[1:3]
names(a) <- c("Label","Region","LSOA_code")
View(a@data)
b <- s_Yorkshire[4:6]
names(b) <- c("Label","Region","LSOA_code")
View(b@data)
c <- s_Yorkshire[7:9]
names(c) <- c("Label","Region","LSOA_code")

#Merging Cleaned data together
s_Yorkshire <- rbind(a,b,c)

#Removing NA values from the acquired data
s_Yorkshire <- s_Yorkshire[!is.na(s_Yorkshire$Label),]


#Loading South Yorkshire crime data
crimeData <- read_xlsx("South Yorkshire Crime Data.xlsx", sheet=2)

#Changing Names of the columns
crimeData <- rename(crimeData,c("LSOA code"="LSOA_code",
                              "Crime type"="crime_type"))

#Removing NA values from the acquired data
crimeData <- crimeData[!is.na(crimeData$Longitude),]


#Joing crime data and shapefile data by LSOA_code
s_Yorkshire@data <- left_join(s_Yorkshire@data, crimeData,
                               by=c('LSOA_code'='LSOA_code'))

#Removing unwanted columns
s_Yorkshire@data <- subset(s_Yorkshire@data, select = c(crime_type,LSOA_code,Longitude,Latitude,Month))



#***********************CODE FOR TREND LINES***********************************************************


#Creating a new variable to plot trend lines
s_Y <- group_by(s_Yorkshire@data,Month,crime_type) %>% dplyr::summarise(count=n())


#Subseting the top 5 crimes happeing in South Yorkshire over the years
s_Y <- subset(s_Y, s_Y$crime_type %in% c("Burglary","Criminal damage and arson",
                                       "Other theft","Violence and sexual offences","Vehicle crime"))

#changing the data type of Month column from double to data format
s_Y$Month <- as.Date(s_Y$Month)

#PLotting trend lines for different crimes from 2017-11 to 2020-03
ggplot(data = s_Y,aes(x=Month, y=count, color= crime_type))+ 
  geom_line(size=1)+
  scale_x_date(labels= date_format("%Y-%m"),breaks = date_breaks("3 months"))+ #scaling x-axis on gaps of 3 months
  scale_color_manual(values = c("violet", "orange","green","darkblue","red"))+ #custom colors for crime types
  labs(title = "Trends of different crimes over the years (South Yorkshire)", 
       caption = "source:UK crime data", x="Date", y="Number of Crimes", color="Crime Types")




#**********************************CODE FOR HEAT MAP BASED ON NO OF CRIMES************************************

#Creating extra variable for Heat Map
s_Y_heatMap <- s_Yorkshire

#counting the number of LSOA_code which inturn gives the number of crimes in that region
LSOA_code_count <- group_by(s_Y_heatMap@data,LSOA_code,) %>% dplyr::summarise(count=n())

#joining the two data
s_Y_heatMap@data <- join(s_Y_heatMap@data, LSOA_code_count,by=c('LSOA_code'='LSOA_code'))

#filtering all the duplicate LSOA_codes from the data_frame
s_Y_heatMap@data <- s_Y_heatMap@data %>% filter(duplicated(s_Y_heatMap@data["LSOA_code"])==FALSE)

#changing the mode of tmap to interactive
tmap_mode("view")

#plotting the crime count data 
tm_shape(s_Y_heatMap)+tm_fill("count",palette=c("blue","white","red"), 
                               breaks=c(0,100,200,500,4500,5500,6500,8000,9000),
                               title ="No of Crimes") +
                      tm_layout(legend.outside = TRUE)+ tm_borders() +
                      tm_layout(main.title = "Heat Map for South Yorkshire based (No of Crimes)", 
                                title.size = 1)





#*******************************CODE FOR HEAT MAP BASED ON MOST HAPPENING CRIME IN A REGION******************************************

#Creating extra variable for Heat Map
s_Y_maxCrime <- s_Yorkshire

#to count the no of each type of crime happening in a region
crime_count <- group_by(s_Y_maxCrime@data,LSOA_code,crime_type) %>% dplyr::summarise(count=n())

#changing the name to avoid clash in-built function count()
crime_count <- rename(crime_count,c("count"="Count"))

#filtering duplicate LSOA_codes in the Shapefile data.frame
s_Y_maxCrime@data <- s_Y_maxCrime@data %>% filter(duplicated(s_Y_maxCrime@data["LSOA_code"])==FALSE)

#obtaning max count of a crime happening in a region
crime_count <- setDT(crime_count)[order(-Count, LSOA_code), head(.SD, 1), by = LSOA_code]


#joining both data.frames
s_Y_maxCrime@data <- join(s_Y_maxCrime@data, crime_count,by=c('LSOA_code'='LSOA_code'))


#interactive mode
tmap_mode("view")

#plotting data based on crime_type to get heatmap of which region has which crime the most
tm_shape(s_Y_maxCrime)+tm_fill("crime_type",title ="Types of Crimes") +
  tm_layout(legend.outside = TRUE)+ tm_borders() +
  tm_layout(main.title = "Heat Map for South Yorkshire (Crime Type)", title.size = 1)

#####################################################################################
