#===================================================================


# Set a working directory to store all the related datasets and files.
# For Questions 1 to 6, you can use either basic plots or ggplot2.
# For Question 7, you should use ggplot2.


# Import the csv file (NYC_Flights.csv) and explore it using str and summary functions.
#====================== Write R code HERE ==========================

my_var<-read.csv("NYC_Flights.csv")




#===================================================================



#======= Question 1 (1 Point) =======
# Q1-1. Add a new variable, total_delay, which is the sum of dep_delay and arr_delay.
# Q1-2. Remove the columns "year" and "cancelled" from your data frame because it is all 2014 and there were no cancelled flights. 
#       Hint: Reassign a data frame excluding the variables. You might want to use negative indexs.
# Q1-3. Make a table of the number of flights by carrier, and then remove the carriers with less than 1000 flights in 2014.

#====================== Write R code HERE ==========================
#Q1-1
total_delay<- my_var$dep_delay + my_var$arr_delay
total_delay

my_var<- cbind(my_var, total_delay)
View(my_var)

#_______________________________________________________________________________
#Q1-2
df<-data.frame(my_var)
df<-subset(df, select=-c(1, 8))

my_var<-data.frame(df)
View(my_var)

#_______________________________________________________________________________

#Q1-3

my_table<-table(my_var$carrier)
View(my_table)


new_d<-subset(my_table, my_table > 1000)
new_d

#===================================================================



#======= Question 2 (1 Point) =======
# Q2. Sort flights (data frame from Question 1) by column "total_delay" in descending order. 
# Then, calculate the average of flight distance among the top 10 flights with the longest total delay.

#====================== Write R code HERE ==========================


sortflights_decreasing<-my_var[order(my_var$total_delay, decreasing = TRUE),]
sortflights_decreasing

View(sortflights_decreasing)

avr_top<-mean(sortflights_decreasing$distance[1:10],)
avr_top

#===================================================================



#======= Question 3 (1 Point) =======
# Q3. Draw box plots of total delay separately by carriers.

#====================== Write R code HERE ==========================


boxplot(total_delay~carrier,data=sortflights_decreasing, main="Carriers",
        xlab="Carrier", ylab="Total delay")



#===================================================================



#======= Question 4 (1 Point) =======
# Q4. Extract flights with a major delay (defined as more than 60 minutes delay in either departure or arrival).
# Then, draw a pie chart to examine the most frequently delayed carrier in NYC.

#====================== Write R code HERE ==========================

major_delay<-subset(sortflights_decreasing, sortflights_decreasing$dep_delay > 60 | sortflights_decreasing$arr_delay > 60)
View(major_delay)

table_carrier<-table(major_delay$carrier)
pie(table_carrier)

#===================================================================



#======= Question 5 (1 Point) =======
# Among the flights with a major delay, defined above,
# Q5-1. Visualize the number of flights by destination, in decreasing order, that departed from JFK airport. (Hint: sort function can be applied to various data types, such as data frame, vector, table)
# Q5-2. Visualize the histogram of total delay of flights that departed from JFK airport.

#====================== Write R code HERE ==========================
#Q5-1
num_flight<-subset(major_delay,major_delay$origin == "JFK")
flight_table<-table(num_flight$dest)

flight_dest<-sort(flight_table, decreasing = TRUE)
flight_dest
barplot(flight_dest)

#_______________________________________________________________________________
#Q5-2

flights_delay <- subset(major_delay, major_delay$origin == "JFK")
flights_delay_table <- table(flights_delay$total_delay)



hist(flights_delay_table)

#===================================================================



#======= Question 6 (1 Point) =======
# Among the flights with a major delay, defined above,
# Q6-1. Visualize the relationship between total delay time and flight distance.
# Q6-2. Do you agree that the longer the flight, the longer the delay? Argue based on correlation (Use comments to answer).

#====================== Write R code HERE ==========================
#Q6-1

cor(major_delay$total_delay, major_delay$distance, use="pairwise.complete.obs")

plot(major_delay$total_delay, major_delay$distance)

#_______________________________________________________________________________



#===================================================================



#======= Question 7 (4 Points) =======
# Install, if not installed yet, and activate ggplot2.
# Install a new package, plyr, and learn a new function, count(), therein (Hint: ? operator or googling).

# Among the flights with a major delay, defined above,
# Q7-1. Count the number of flights grouped by ("origin", "dest", "carrier") (Hint: use count function).
# Q7-2. Draw the point plots of the number of flights by carrier (i.e., x-axis: carrier and y-axis: number of counts) and change colors by carrier (Hint: geom_point).
# Q7-3. Add text labels of "destination" to the points over 300 major delays (Hint: geom_text or geom_text_repel).
# Q7-4. Separate the plots by "origin" (Hint: facet_wrap or facet_grid). 

#====================== Write R code HERE ==========================
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(ggrepel)
install.packages("ggrepel")

View(major_delay)

#Q7-1

num_of_flights <- major_delay %>%
  group_by(origin, dest, carrier) %>%
  count(carrier)
  
num_of_flights

#__________________________________________________________________________________________

#Q7-2
ggplot(num_of_flights, aes(x= carrier, y= n))+
  geom_point(aes(color = carrier), size = 3, shape=1)
#_____________________________________________________________________________________________

#Q7-3

ggplot(num_of_flights, aes(carrier, n)) +  
  geom_point(aes(color = carrier), size = 3, shape=1, stroke = 1.5)+ 
  geom_smooth(color="red", se=FALSE) + 
  geom_text_repel(data= subset(num_of_flights, subset = n>300), aes(label = dest))

#_______________________________________________________________________________________________________

#Q7-4

ggplot(num_of_flights, aes(carrier, n)) + 
  geom_point(aes(color = carrier), size = 3, shape=1, stroke = 1.5)+ 
  geom_smooth(color="red", se=FALSE)+ 
  geom_text_repel(data= subset(num_of_flights, subset = n>300), aes(label = dest))+ 
  facet_grid(.~origin)

#===================================================================
