#Step A: Load and Merge datasets

#1)	Read in the census and the USArrests datasets and merge them. (just like HW6)

#Read in the census dataset
#Using the function created in HW3
cleanstate <- function()
{
  Statesnew <- read.csv("states.csv")
  Statesnew <- Statesnew[-1,-1]
  Statesnew <- Statesnew[-52,]
  Statesnew <- Statesnew[,-1:-3]
  colnames(Statesnew) <- c("stateName", "population", "popOver18", "percentOver18")
  return(Statesnew)
}
cleanstate()
StatePop <- cleanstate()
View(StatePop)

#Copy the USArrests dataset into a local variable
Arrests <- USArrests
Arrests$stateName <- row.names(Arrests)
View(Arrests)

#Create a merged dataframe -- with the attributes from both dataframes
Merged <- merge(StatePop, Arrests, By = "stateName")                       #Merging two datasets by stateName                
View(Merged)


#2)	 Create a new Data frame that has the area of each state (state.area), and the center of each state (state.center), and then merge (by stateName) it with your final data frame in step #1. 
stateName <- state.name                                     #Getting name of all the states
Area <- state.area                                          #Getting area of all the states
Center <- state.center                                      #Getting center of all the states (Latitude, Longitude)
States <- data.frame(stateName, Area, Center)               #Merging all the above 3 in one Dataframe
View(States)

MergedData <- merge(Merged, States, by = "stateName")       #Merging two dataframe in one single dataframe
View(MergedData)



#Step B: Generate a color coded map
#install ggmap and ggplot

install.packages("ggplot2")                                 #Installing ggplot2 package
library("ggplot2")

install.packages("ggmap")                                   #Installing ggmap package
library("ggmap")


#3)	Create a color coded map, based on the area of the state 

state <- map_data("state")                                 #To get the map of USA
MergedData$stateName <- tolower(MergedData$stateName)      #Converting all the state names to lower case

Basicmap <- ggplot(MergedData, aes(map_id = stateName))                         #To create ggplot and specify df and Map id
Basicmap <- Basicmap + geom_map(map = state, fill="white", color="black")       #Defining country map, color to fill and for boundary for states
Basicmap <- Basicmap + expand_limits(x = state$long, y = state$lat)             #Defining x axis and y axis
Basicmap <- Basicmap + coord_map() + ggtitle("Basic Map of USA")                #Using coord() to stop map from stretching and assigning title for map
Basicmap



#Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 


#4)	Repeat step B, but color code the map based on the murder rate of each state.
MurderMap <- ggplot(MergedData, aes(map_id = stateName))                        #To create ggplot and specify df and Map id
MurderMap <- MurderMap + geom_map(map = state, aes(fill=Murder))                #Defining country map and filling it with Murder Rate
MurderMap <- MurderMap + expand_limits(x = state$long, y = state$lat)           #Defining x axis as longitude and y axis as latitude
MurderMap <- MurderMap + coord_map() + ggtitle("Murder Rate of each State")     #Using coord() to stop map from stretching and assigning title for map
MurderMap


#5)	 Show the population as a circle per state (the larger the population, the larger the circle), using the location defined by the center of each state
MurderMapC <- MurderMap + geom_point(x = MergedData$x, y = MergedData$y, aes(size = MergedData$population))    #using geom_point to draw scatter plot with size of population on states(Bigger circle for larger population)
MurderMapC <- MurderMapC + ggtitle("Murder rate with population size map")                                     #Assigning title for map
MurderMapC

  
  
#Step D: Zoom the map


#6)	Repeat step C, but only show the states in the north east
#Hint: get the lat and lon of new york city
#Hint: set the xlim and ylim to NYC +/- 10

LatLong <- geocode(source= "dsk", "nyc,new york, ny")                                               #Finding Latitude and Longitude of NYC using DS toolkit
LatLong

MapNE <- ggplot(MergedData, aes(map_id = stateName))                                                #To create ggplot and specify df and Map id                         
MapNE <- MapNE + geom_map(map=state, aes(fill= Murder), color = "black")                            #Defining country map and filling it with Murder rate        
MapNE <- MapNE + expand_limits(x= state$long, y = state$lat)                                        #Using expand limits for defining the x axis as longitude and y axis as latitude
MapNE <- MapNE + coord_map() + ggtitle("Map of USA towards North Eastern Side")                     #Using coord() to stop map from stretching and assigning title for map
MapNE <- MapNE + geom_point(x = MergedData$x, y = MergedData$y, aes(size = MergedData$population))  #Using geom_point to draw scatter plot with size of population on states
MapNE <- MapNE + xlim(LatLong$lon-10,LatLong$lon+10) + ylim(LatLong$lat-10,LatLong$lat+10)          #Using xlim and ylim function to zoom on north eastern states
MapNE


