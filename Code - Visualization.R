#Step A: Load and Merge datasets


#1)	Read in the census dataset
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


#2)	Copy the USArrests dataset into a local variable
Arrests <- USArrests
Arrests$stateName <- row.names(Arrests)
View(Arrests)


#3)	Create a merged dataframe -- with the attributes from both dataframes
Merged <- merge(StatePop, Arrests, By = "stateName")                       #Merging two datasets by stateName                
View(Merged)



#Step B: Explore the Data ??? Understanding distributions

#Installing Package ggplot2
install.packages("ggplot2")
library("ggplot2")

#4)	Create a histogram using GGPLOT for the population and a different histogram for the murder rate

#Histogram for population
histpop <- ggplot(Merged, aes(x = population))                                        #Plotting histogram using ggplot package
histpop <- histpop + geom_histogram(color = "white", fill = "darkgrey", bins = 50)    #Dividing x axis into 50 bins and displaying counts with individual bars
histpop <- histpop + ggtitle("Histogram of Population")                               #Assigning title to the histogram
histpop                                                                               #To view the histogram

#Histogram for murder rate
histmurder <- ggplot(Merged, aes(x = Murder))
histmurder <- histmurder + geom_histogram(color = "white", fill = "darkgrey", bins = 50)
histmurder <- histmurder + ggtitle("Histogram of Murder Rate")
histmurder                                                          

#Histogram for Assault
histassault <- ggplot(Merged, aes(x = Assault))
histassault <- histassault + geom_histogram(color = "white", fill = "darkgrey", bins = 50)
histassault <- histassault + ggtitle("Histogram of Assault")
histassault

#Histogram for UrbanPop
histurbanpop <- ggplot(Merged, aes(x = UrbanPop))
histurbanpop <- histurbanpop + geom_histogram(color = "white", fill = "darkgrey", bins = 50)
histurbanpop <- histurbanpop + ggtitle("Histogram of UrbanPop")
histurbanpop

#Histogram for Rape
histrape <- ggplot(Merged, aes(x = Rape))
histrape <- histrape + geom_histogram(color = "white", fill = "darkgrey", bins = 50)
histrape <- histrape + ggtitle("Histogram of Rape")
histrape

#I have kept bins equal to 50 so that my histogram looks proper


#5)	Create a boxplot for the population, and a different boxplot for the murder rate

#Boxplot for population
boxpop <- ggplot(Merged, aes(x=factor(0), y=population))
boxpop <- boxpop + geom_boxplot(color = "black", fill = "white")
boxpop <- boxpop + ggtitle("Boxplot of Population")
boxpop

#Boxplot for Murder Rate
boxmurder <- ggplot(Merged, aes(x=factor(0), y=Murder))
boxmurder <- boxmurder + geom_boxplot(color = "black", fill = "white")
boxmurder <- boxmurder + ggtitle("Boxplot of Murder Rate")
boxmurder


#6)	Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)
#Histogram is a better option for visualization, as its more spread out. 
#Its easier to understand than boxplot.



#Step C: Which State had the Most Murders ??? bar charts


#7)	Calculate the number of murders per state
#Creating a new column murderstate
Merged$murderstate <- Merged$population * Merged$Murder/100
View(Merged)


#8)	Generate a bar chart, with the number of murders per state
#Bar Chart of murder per state
barmurderstate <- ggplot(Merged, aes(x=stateName, y=murderstate))
barmurderstate <- barmurderstate + geom_col()
barmurderstate <- barmurderstate + ggtitle("Bar Chart of Murder per State")
barmurderstate


#9)	Generate a bar chart, with the number of murders per state. Rotate text (on the X axis), so we can see x labels
#Rotating state names
barmurderstate2 <- barmurderstate + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barmurderstate2


#10)	Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the murder rate
#Reordering states by their murder rate
barmurderstate3 <- ggplot(Merged, aes(x=reorder(stateName, Murder), y=murderstate))
barmurderstate3 <- barmurderstate3 + geom_col()
barmurderstate3 <- barmurderstate3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barmurderstate3


#11)	Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar
barmurderstate4 <- ggplot(Merged, aes(x=reorder(stateName, Murder), y=murderstate, fill=percentOver18))
barmurderstate4 <- barmurderstate4 + geom_col()
barmurderstate4 <- barmurderstate4 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barmurderstate4 <- barmurderstate4 + ggtitle("Bar Chart of Murder per State and Population per State")
barmurderstate4



#Step D: Explore Murders ??? scatter chart


#12)	Generate a scatter plot ??? have population on the X axis, the percent over 18 on the y axis, and the size & color represent the Murder rate
scatter <- ggplot(Merged, aes(x = population, y = percentOver18))
scatter <- scatter + geom_point(aes(size = Murder, color = Murder))
scatter