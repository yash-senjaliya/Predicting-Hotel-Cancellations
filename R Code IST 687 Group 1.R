################################################################################

# IST 687 Final Project #

################################################################################

# Team: Manpreet Singh Saini
#       Yash Senjaliya   
#       Ruchita Harsora
#       Rahul Khairnar
#       Amanda Borttnoff
#       Chaitu Manthena


################################################################################

#-------------------------------------------------------------------------------

#Getting Started

# Run these three functions to get a clean test of project code
# dev.off() # Clear the graph window
# cat('\014')  # Clear the console
# rm(list=ls()) # Clear user objects from the environment

#-------------------------------------------------------------------------------

##PACKAGES AND LIBRARIES USED

#install.packages("zoo")
#install.packages("imputeTS")
#install.packages("quanteda.textplots")
#install.packages("quanteda")
#install.packages("tm")

library(readxl)
library(ggplot2)
library(readr)
library(zoo)
library(imputeTS)
library(dplyr)
library(quanteda.textplots)
library(quanteda)
library(tm)
library(caret) 
library(e1071)
library(kernlab) 

##-------------------------------------------------------------------------------

## IMPORTING DATASET INTO DATAFRAME FROM EXCEL


# H2_City <- read_excel("Desktop/SEM 1/IST 687/Project/H1-Hotel.xlsx")
# H1_Resort <- read_excel("Desktop/SEM 1/IST 687/Project/H1-Resort.xlsx")


#Using read_excel() to read in the H2_City and H1_Resort excel files

CityDS<-H2_City #Storing H2_City in CityDS
ResortDS<-H1_Resort #Storing H1_Resort in ResortDS

##-------------------------------------------------------------------------------

##VIEWING DATASET

View(CityDS)
View(ResortDS)

##-------------------------------------------------------------------------------

##DATA UNDERSTANDING

str(CityDS)
str(ResortDS)

#The following are all the variables in the CityDS and ResortDS dataset:

#IsCanceled is a factor variable where a value of 1 means cancelled and 0 means not cancelled

#LeadTime is a numeric variable which shows the days between creating a hotel booking and the 
#arrival date

#Arrival Date is a POSIXct variable, used to store arrival dates as yyyy-dd-mm

#ReservationStatusDate is a POSIXct variable, used to store the date the reservation status is 
#set in yyy-dd-mm format

#ReservationStatus is a factor variable with three options, Canceled, Check-Out, and No-Show

#StaysInWeekendNights is a numeric variable showing the amount of weekend nights for a booking

#StaysInWeekNights is a numeric variable showing the amount of week nights for a booking

#Adults is a numeric variable showing the amount of adult guests for a booking

#Children is a character variable showing the amount of children guests for a booking

#Babies is a numeric variable showing the amount of baby guests for a booking

#Meal is a factor variable with four options - BB (Bed and Breakfast), FB (Full Board),
#HB (Half Board), and SC (no specified meal package)

#Country is a character variable which gives the 3 letter country codes of countries represented
#in the data

#MarketSegment is a character variable which represents market segment designation

#DistributionChannel is a character variable show what ways booking were created

#IsRepeatedGuest is a factor variable with a value of 1 for repeated guests and 0 for one-time guests

#PreviousCancellations is a numeric variable that represents how many previous bookings were cancelled
#before the current booking

#PreviousBookingsNotCanceled is a numeric variable that represents how many bookings were not cancelled
#prior to the current booking

#ReservedRoomType is a character variable which shows different codes for room types booked

#AssignedRoomType is a character variable which shows the room actually assigned to the customer

#DepositType is a factor variable which shows whether customers made a deposit for a booking,
#did not make a deposit, or a refundable deposit

#Agent is a character variable and the ID of the travel agent that made the booking

#Company is a character variable, however is shows NULL values

#DaysInWaitingList is a numeric variable that shows how many days a room booking was on the waitlist
#before being confirmed

#ADR is a numeric variable which shows the average daily rate

#RequiredCarParkingSpaces is a numeric variable which shows the amount of parking spaces needed
#by the customer per booking

#TotalofSpecialRequests is a numeric variable which is the number of special requests made by
#the customer

sum(is.na(CityDS$Adults))
sum(is.na(CityDS$Children))
sum(is.na(CityDS$Babies))
sum(is.na(CityDS$Meal))
sum(is.na(CityDS$Country))
sum(is.na(CityDS$MarketSegment))
sum(is.na(CityDS$DistributionChannel))
sum(is.na(CityDS$IsRepeatedGuest))
sum(is.na(CityDS$PreviousCancellations))
sum(is.na(CityDS$PreviousBookingsNotCanceled))
sum(is.na(CityDS$ReservedRoomType))
sum(is.na(CityDS$AssignedRoomType))
sum(is.na(CityDS$BookingChanges))

##------------------------------------------------------------------------------
#Data Preprocessing and preparation


CityDS_no_na <- CityDS[!is.na(CityDS$Arrival.Date),]

ResortDS_no_na <- ResortDS[!is.na(ResortDS$Arrival.Date),]


#The function is.na returns a value of TRUE for any missing values found in the column Arrival Date,
#and then the operator '!' will give the opposite logical value, FALSE. By using brackets, the above
#lines of code index into CityDS and ResortDS to preserve only the data not containing NA values.

## CREATING A NEW SUBSET OF THE DATASET WITH RESERVATION STATUS AS CHECKOUT
h2_city_checkout <- subset(CityDS, ReservationStatus == "Check-Out") 

h1_resort_checkout <- subset(ResortDS, ReservationStatus == "Check-Out") 

#The subset() function creates a new dataset based on a conditional, in the above cases
#when ReservationStatus is equivalent to "Check-Out" a new dataframe is created for
#both the CityDS and ResortDS data.

## CREATING A NEW DURATION COLUMN
h2_city_checkout$Duration_of_Stay <- h2_city_checkout$StaysInWeekendNights+h2_city_checkout$StaysInWeekNights 

h1_resort_checkout$Duration_of_Stay <- h1_resort_checkout$StaysInWeekendNights+h1_resort_checkout$StaysInWeekNights 

#The column Duration_of_Stay is added to each dataframe - h2_city_checkout and h1_resort_checkout - to show the total
#amount of days guests with a reservation status of "Check-Out" were at the hotels. This variable is created by adding
#the two columns, StaysInWeekendNights and StaysInWeekNights, to get the total number of days.

## FINDING OUT THE ARRIVAL DATES TO FILL THE NA VALUES.
h2_city_checkout$Arrival_Date_Calc <- as.Date(h2_city_checkout$ReservationStatusDate) - h2_city_checkout$Duration_of_Stay 

h1_resort_checkout$Arrival_Date_Calc <- as.Date(h1_resort_checkout$ReservationStatusDate) - h1_resort_checkout$Duration_of_Stay

#Firstly, the as.date() function converts values into calendar dates. This function is applied to
#the ReservationStatusDate variable for both city and resort hotels to make sure the checkout dates
#are understood as calendar dates in R. Then Duration_of_Stay is subtracted from ReservationStatusDate
#to get the proper arrival dates and store them in a new column called Arrival_Date_Calc. This ensures
#any NAs in the original Arrival Date column are fixed.


## CREATING A DURATION IN THE MAIN DATAFRAME.
CityDS$Duration_of_Stay <- CityDS$StaysInWeekNights+CityDS$StaysInWeekendNights

ResortDS$Duration_of_Stay <- ResortDS$StaysInWeekNights+ResortDS$StaysInWeekendNights

#Duration_of_Stay is added as a column in the main CityDS and ResortDS dataframes (the dataframes
#which include all ReservationStatus types). This is done by again adding together the variables
#StaysInWeekNights and StaysInWeekendNights to calculate the total amount of days guests stayed
#at the hotels, and then storing the results in the new variable - Duration_of_Stay.

## CREATING ADR PER STAY COLUMN.
CityDS$ADR_per_stay <- CityDS$ADR*CityDS$Duration_of_Stay 

ResortDS$ADR_per_stay <- ResortDS$ADR*ResortDS$Duration_of_Stay 

#The average daily revenue per stay is calculated by multiplying average daily revenue(ADR) by the
#total number of days guests were at the hotel. The results of this are stored in a new variable
#called ADR_per_stay, and this has been done for both the CityDS data and ResortDS data. This variable
#will show how much revenue the hotels made from someone's entire stay.


CityDS$Children_No_NA <- na_interpolation(as.numeric((CityDS$Children)))

#the na_interpolation function is used to fill in missing values by making an estimation with
#the non NA values that come before and after it. In this case, the variable Children is first
#converted into numeric values using as.numeric(), then na_interpolation is used on it, and the
#results are stored in a new column, Children_No_NA, in the CityDS dataframe.

CityDS$Member_Count <- as.numeric(CityDS$Adults)+as.numeric(CityDS$Children_No_NA)

#In this line of code a new column is created called Member_Count. The as.numeric function is used
#on both the Adults and Children_No_Na variables to convert their values into numbers. Then, these
#two variables are added together to get the total amount of guests stored in Member_Count.

ResortDS$Children_No_NA <- na_interpolation(as.numeric((ResortDS$Children)))
ResortDS$Member_Count <- as.numeric(ResortDS$Adults)+as.numeric(ResortDS$Children_No_NA)

#The previous two processes are repeated, but this time for the ResortDS dataframe.

#Creating Season based on the months 

yqcity <- as.yearqtr(as.yearmon(CityDS$ReservationStatusDate, "%m/%d/%Y") + 1/12)
CityDS$Season <- factor(format(yqcity, "%q"), levels = 1:4,labels = c("winter", "spring", "summer", "fall"))

yqresort <- as.yearqtr(as.yearmon(ResortDS$ReservationStatusDate, "%m/%d/%Y") + 1/12)
ResortDS$Season <- factor(format(yqresort, "%q"), levels = 1:4,labels = c("winter", "spring", "summer", "fall"))

#In the above code, first as.yearmon() is used to convert the values in Arrival Date for both 
#CityDS and ResortDS into a yearmon object - in this case the format has been specificed as
#month-date-year by %m/%d/%y. Then the results of that step are run through the function as.yearqtr
#which converts the values into a year and quarter format. This is stored in yqcity for CityDS and
#yqresort for ResortDS. 

#In the next step yqcity and yqresort are formatted by quarter (%q) and labeled with each season,
#winter, spring, summer, and fall. These results are stored in a new column called Season for both
#the city and resort datasets.


Visitor_Type_Calc <- function(Member_Count){ 
  if (member_C <- as.integer(Member_Count))
  {
    return("Single")
  } else if(member_C == 2){
    return("Couple")
  }else{
    return("Family")
  }}

#The function created above is called Visitor_Type_Calc with the argument Member_Count. The next
#line will take the input of the function and run it through as.integer to convert the value to
#an integer and then store the results in member_C. The rest of the function is created using 
#conditional statements. The function will return the word "Single" unless member_C == 2 is TRUE.
#Then the function will return the word "Couple" - for two people. In the case that the input is
#higher than 2, the function returns the word "Family".

CityDS$Visitor_Type <- lapply(CityDS$Member_Count,Visitor_Type_Calc)
ResortDS$Visitor_Type <- lapply(ResortDS$Member_Count,Visitor_Type_Calc)

#The above two lines of code create a new variable, Visitor_Type for both the CityDS and ResortDS
#dataframes. The lapply function takes the column Member_Count from CityDS (and ResortDS) and 
#then applies the function created above, Visitor_Type_Calc, to each element of Member_Count.
#The results are then stored in Visitor_Type, showing whether is booking is for a single person,
#a couple, or a family.

type_convert(CityDS)
type_convert(ResortDS)

CityDS<-as.data.frame(lapply(CityDS, unlist))
ResortDS<-as.data.frame(lapply(ResortDS, unlist))

#Changing the Variable Types of Categorical Variables

#IsCancelled 
CityDS$IsCanceled<-as.factor(CityDS$IsCanceled)
ResortDS$IsCanceled<-as.factor(ResortDS$IsCanceled)

#Reservation Status
CityDS$ReservationStatus<-as.factor(CityDS$ReservationStatus)
ResortDS$ReservationStatus<-as.factor(ResortDS$ReservationStatus)

#Meal
CityDS$Meal<-as.factor(CityDS$Meal)
ResortDS$Meal<-as.factor(ResortDS$Meal)

#IsRepeatedGuest
CityDS$IsRepeatedGuest<-as.factor(CityDS$IsRepeatedGuest)
ResortDS$IsRepeatedGuest<-as.factor(ResortDS$IsRepeatedGuest)

#Deposit Type
CityDS$DepositType<-as.factor(CityDS$DepositType)
ResortDS$DepositType<-as.factor(ResortDS$DepositType)

#Deposit Type
CityDS$DepositType<-as.factor(CityDS$DepositType)
ResortDS$DepositType<-as.factor(ResortDS$DepositType)

#Season
CityDS$Season<-as.factor(CityDS$Season)
ResortDS$Season<-as.factor(ResortDS$Season)

#The variables IsCanceled, ReservationStatus, Meal, IsRepeatedGuest, DepositType, and Season,
#are all being converted into factors using the as.factor function. Each of these
#columns has a set amount of values, making them categorical variables.

##------------------------------------------------------------------------------

#Merging the datasets

CityDS$BelongsTo<-"City" #Creating a column in CityDS called BelongsTo that contains the word
                         #'City' to keep track of which data belongs to which dataset

ResortDS$BelongsTo<-"Resort" #Creating a column in ResortDS called BelongsTo that contains the word
                             #'Resort' to keep track of which data belongs to which dataset

HotelDS<-rbind(CityDS,ResortDS)  #The rbind function combines the dataframes CityDS and ResortDS
                                 #vertically. Originally CityDS contained 79,330 rows and 
                                 #ResortDS contained 40,060, the combined dataframe HotelDS then
                                 #has 119,390 rows.

View(HotelDS) #The View function allows the dataframe to be seen as a table 

##------------------------------------------------------------------------------
#EXPLORATORY DATA ANALYSIS

#1. Booking Cancellations (City Hotels Vs Resort Hotels)
#How many bookings were Canceled?

#City

city_cancelled_agg <- CityDS %>% group_by(IsCanceled) %>% summarise(Percentage = n()) %>% mutate(Percentage=Percentage/sum(Percentage)*100)

#This command creates a matrix called city_cancelled_agg which contains the count for each
#value in the variable IsCanceled from the CityDS_no_na dataframe. 31,583 reservations for
#city hotels were cancelled (1) and 8,477 reservations were not cancelled (0).

ggplot(data = city_cancelled_agg, aes(x = IsCanceled,  y = Percentage)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill ="steelblue") +
  scale_y_continuous(name = "No of Cancellations in percent",labels = scales::comma) +
  xlab(" 1 - Cancelled and 0 - NotCancelled ")  +
  ggtitle("No. of Cancellations in City") + 
  theme_minimal() 

#Using the ggplot function to create a bar graph with the data from city_cancelled_agg,
#where the x-axis is the variable IsCanceled, and the y-axis is the count of bookings that
#were cancelled or not. The title is set by ggtitle(), which is "No. of Cancellations in City".

#Resort

resort_cancelled_agg <- ResortDS %>% group_by(IsCanceled) %>% summarise(Percentage = n()) %>% mutate(Percentage=Percentage/sum(Percentage)*100)

#Similar to the city code above, this creates a matrix called resort_cancelled_agg. The matrix
#contains the count for bookings that were cancelled or not cancelled for ResortDS_no_na. There
#were 28,938 bookings that were not cancelled for resorts and 11,122 that were cancelled.

ggplot(data = resort_cancelled_agg, aes(x = IsCanceled,  y = Percentage)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill ="steelblue") +
  scale_y_continuous(name = "No of Cancellations",labels = scales::comma) +
  xlab(" 1 - Cancelled and 0 - NotCancelled ")  +
  ggtitle("No. of Cancellations in Resort") + 
  theme_minimal()
#Using the ggplot function to create a bar graph with the data from resort_cancelled_agg,
#where the x-axis is the variable IsCanceled, and the y-axis is the count of bookings that
#were cancelled or not. The title is set by ggtitle(), which is "No. of Cancellations in Resort".

#Combined Cancellations 

ggplot(data = HotelDS, aes(
  x = BelongsTo,
  y = LeadTime,
  fill = factor(IsCanceled)
)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Cancellation By Hotel Type",
    subtitle = "Based on Lead Time",
    x = "Hotel Type",
    y = "Lead Time (Days)"
  ) +
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Cancelled", "Not Cancelled")
  ) + theme_classic()

#Using ggplot to create a boxplot showing cancellation by hotel type (city or resort), with
#lead time on the y-axis.

#2. No. of booking in each year (City Vs Resort)
#What are the number of bookings for each year in each of the property?


#City


#The function is.na returns a value of TRUE for any missing values found in the column Arrival Date,
#and then the operator '!' will give the opposite logical value, FALSE. By using brackets, the above
#lines of code index into CityDS to preserve only the data not containing NA values.


CityDS_no_na$Arrival_year <- substring(CityDS_no_na$Arrival.Date,1,4)

#Creating a new column in CityDS_no_na called Arrival_year which will contain the year for each
#booking. 

city_year_agg <- CityDS_no_na %>% group_by(Arrival_year) %>% summarise(count = n())

#This command creates a matrix called city_year_agg which shows the count of bookings across
#three different years, 2015(15), 2016(16), and 2017(17). There were 12,173 city bookings in 
#2015, 16,213 in 2016, and 11,674 in 2017.

ggplot(data = city_year_agg, aes(x = Arrival_year,  y = count)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill ="steelblue") +
  scale_y_continuous(name = "No of Bookings",labels = scales::comma) +
  xlab("Arrival Year")  +
  ggtitle("No. of bookings for each year in City") + 
  theme_minimal()

#Using ggplot to create a bar graph of the number of bookings per year at the City hotels.

#Resort
ResortDS_no_na <- ResortDS[!is.na(ResortDS$Arrival.Date),]

#The function is.na returns a value of TRUE for any missing values found in the column Arrival Date,
#and then the operator '!' will give the opposite logical value, FALSE. By using brackets, the above
#lines of code index into ResortDS to preserve only the data not containing NA values.

ResortDS_no_na$Arrival_year <- substring(ResortDS_no_na$Arrival.Date,1,4)

#Creating a new column in ResortDS_no_na called Arrival_year which will contain the year for each
#booking. 

resort_year_agg <- ResortDS_no_na %>% group_by(Arrival_year) %>% summarise(count = n())

#This command creates a matrix called resort_year_agg which shows the count of bookings across
#three different years, 2015(15), 2016(16), and 2017(17). There were 8,329 resort bookings in 
#2015, 18,577 in 2016, and 13,154 in 2017.

ggplot(data = resort_year_agg, aes(x = Arrival_year,  y = count)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill ="steelblue") +
  scale_y_continuous(name = "No of Bookings",labels = scales::comma) +
  xlab("Arrival Year")  +
  ggtitle("No. of bookings for each year in Resort") + 
  theme_minimal()

#Using gglot to create a bar graph showing the number of bookings per year for resort hotels.


#3. Season wise and Month wise Generated ADR for City and Resort
#What is the busiest month for each property?


#Month
#City

CityDS_no_na$Arrivalmonth<- strftime(CityDS_no_na$Arrival.Date, "%m")

#Creating a new column in CityDS_no_na called Arrivalmonth which will contain the month for each
#booking. The strftime function breaks down the Arrival Date variable into month (%m).

city_month_agg <- CityDS_no_na %>% group_by(Arrivalmonth) %>% summarise(count = n())

#This command creates a matrix called resort_year_agg which shows the count of bookings across
#each month.

ggplot(data = city_month_agg, aes(x = Arrivalmonth,  y = count)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill ="steelblue") +
  scale_y_continuous(name = "Count",labels = scales::comma) +
  xlab(" All the Months in City data ")  +
  ggtitle("Monthly distribution of Bookings in City") + 
  theme_minimal() 

#Using ggplot to create a bar graph showing the distribution of bookings per month for city hotels.

#Month
#Resort

ResortDS_no_na$Arrivalmonth<- strftime(ResortDS_no_na$Arrival.Date, "%m")

#Creating a new column in ResortDS_no_na called Arrivalmonth which will contain the month for each
#booking. The strftime function breaks down the Arrival Date variable into month (%m).

resort_month_agg <- ResortDS_no_na %>% group_by(Arrivalmonth) %>% summarise(count = n())

#This command creates a matrix called resort_month_agg which shows the count of bookings across
#each month.

ggplot(data = resort_month_agg, aes(x = Arrivalmonth,  y = count)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill ="steelblue") +
  scale_y_continuous(name = "Count",labels = scales::comma) +
  xlab(" All the Months in Resort data ")  +
  ggtitle("Monthly distribution of Bookings in Resort") + 
  theme_minimal() 

#Using ggplot to create a bar graph showing the distribution of bookings per month for resort hotels

#Season
#City

plotofSeason_City<-ggplot(data=CityDS_no_na, aes(x=Season, y=ADR)) +  
  geom_bar(stat="identity", fill="steelblue") + 
  labs(x="Season", y="ADR", title="Season Vs ADR in Resort data") +
  theme_minimal()
plotofSeason_City

#Using ggplot to create a bar graph showing the average daily rate per season for city hotels.

#Season
#Resort

plotofSeason_resort<-ggplot(data=ResortDS_no_na, aes(x=Season, y=ADR)) +  
  geom_bar(stat="identity", fill="steelblue") + 
  labs(x="Season", y="ADR", title="Season Vs ADR in Resort data") +
  theme_minimal()
plotofSeason_resort

#Using ggplot to create a bar graph showing the average daily rate per season for resort hotels.

#4. Duration of Stay (Weekend and Weekday Nights)
#How long people stay in the hotels?

#City
CityDS_no_na$TotalStay <- CityDS_no_na$StaysInWeekendNights + CityDS_no_na$StaysInWeekNights

#The column TotalStay is added to the CityDS_no_na dataframe. StaysInWeekendNights and StaysInWeekNights
#are added together to get the total length of stay for each booking.

TotalStay <- CityDS_no_na$TotalStay[CityDS_no_na$IsCanceled == 0]

#The above line of code creates a list called TotalStay, which contains TotalStay (the total amount
#of days for a given booking) when IsCanceled is equivalent to 0 - or the booking was not cancelled.

city_stay_agg <- CityDS_no_na %>% group_by(TotalStay) %>% summarise(count = n())

#Creates a matrix called city_stay_agg which shows the count of each length of stay that comes up
#in the vector TotalStay.

ggplot(data = city_stay_agg, aes(x = TotalStay,  y = count)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill ="steelblue") +
  xlim(0,20) +
  labs(x="Total Booking Nights Stay",y="Total Booking", title="Total Booking Stays for Weeks and Weekend") +
  theme_minimal() 

#Using ggplot to create a bar graph which shows the distribution of total nights stayed per booking
#for city hotels.

#Resort

ResortDS_no_na$TotalStay <- ResortDS_no_na$StaysInWeekendNights + ResortDS_no_na$StaysInWeekNights

#The column TotalStay is added to the ResortDS_no_na dataframe. StaysInWeekendNights and StaysInWeekNights
#are added together to get the total length of stay for each booking.

TotalStay <- ResortDS_no_na$TotalStay[ResortDS_no_na$IsCanceled == 0]

#The above line of code creates a list called TotalStay, which contains TotalStay (the total amount
#of days for a given booking) when IsCanceled is equivalent to 0 - or the booking was not cancelled.

resort_stay_agg <- ResortDS_no_na %>% group_by(TotalStay) %>% summarise(count = n())

#Creates a matrix called resort_stay_agg which shows the count of each length of stay that comes up
#in the vector TotalStay.

ggplot(data = resort_stay_agg, aes(x = TotalStay,  y = count)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill ="steelblue") +
  xlim(0,20) +
  labs(x="Total Booking Nights Stay",y="Total Booking", title="Total Booking Stays for Weeks and Weekend in Resort") +
  theme_minimal()

#Using ggplot to create a bar graph which shows the distribution of total nights stayed per booking
#for resort hotels.

#5. ADR comparison for booking - Canceled Vs Not Canceled

#For City
Cancelleddf_City <- data.frame(CityDS$IsCanceled,CityDS$ADR)

#Using data.frame function to create
#a dataframe just containing the 
#the columns IsCanceled and ADR from CityDS

names(Cancelleddf_City) <- c('IsCancelled','ADR')

#The names function renames the columns
#in Cancelleddf_City to 'IsCancelled' and 'ADR'

View(Cancelleddf_City)

plotofCancellations_City<-ggplot(data=Cancelleddf_City, aes(x=IsCancelled, y=ADR)) +  
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + labs(title="Cancellations Vs ADR For City")
plotofCancellations_City

#Creating a bar graph called plotofCancellations_City which shows ADR for both cancelled(1) and not
#cancelled(0) bookings in city hotels.


#For Resort

Cancelleddf_Resort <- data.frame(ResortDS$IsCanceled,ResortDS$ADR)

#Using data.frame function to create
#a dataframe just containing the 
#the columns IsCanceled and ADR from ResortDS

names(Cancelleddf_Resort) <- c('IsCancelled','ADR')

#The names function renames the columns
#in Cancelleddf_Resort to 'IsCancelled' and 'ADR'

View(Cancelleddf_Resort)

plotofCancellations_Resort<-ggplot(data=Cancelleddf_Resort, aes(x=IsCancelled, y=ADR)) +
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + labs(title="Cancellations Vs ADR For Resort") 
plotofCancellations_Resort

#Creating a bar graph called plotofCancellations_Resort which shows ADR for both cancelled(1) and not
#cancelled(0) bookings in resort hotels.

#6. Distribution Channel Vs ADR

#For City
DC_City <- data.frame(CityDS$DistributionChannel,CityDS$ADR)
names(DC_City) <- c('DistributionChannel','ADR')
View(DC_City)

#Creating a dataframe from the DistributionChannel and ADR columns in CityDS. Then renaming the 
#columns with the names function to 'DistributionChannel' and 'ADR'.

plotofdistributionChannel_City<-ggplot(data=DC_City, aes(x=DistributionChannel, y=ADR)) +  
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + 
  labs(title="Distirbution Channels Vs ADR For City") 
plotofdistributionChannel_City

#Creating a bar graph in ggplot to show the ADR for the different Distribution Channels in City
#hotels.

#For Resort

DC_Resort <- data.frame(ResortDS$DistributionChannel,ResortDS$ADR)
names(DC_Resort) <- c('DistributionChannel','ADR')
View(DC_Resort)

#Creating a dataframe from the DistributionChannel and ADR columns in ResortDS. Then renaming the 
#columns with the names function to 'DistributionChannel' and 'ADR'.

plotofdistributionChannel_ResortDS<-ggplot(data=DC_Resort, aes(x=DistributionChannel, y=ADR)) +  
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() +
  labs(title="Distirbution Channels Vs ADR For Resort")
plotofdistributionChannel_ResortDS

#Creating a bar graph in ggplot to show the ADR for the different Distribution Channels in resort
#hotels.

#7. ADR comparison for Repeating Guests vs One-Time Guests

#For City
RG_City<- data.frame(CityDS$IsRepeatedGuest,CityDS$ADR)
names(RG_City) <- c('IsRepeatedGuest','ADR')
View(RG_City)

#Creating a dataframe called RG_City using the columns IsRepeatedGuest and ADR from CityDS. Then
#using the name function to change the column names to 'IsRepeatedGuest' and 'ADR'.

plotofrepeatedGuests_City<-ggplot(data=RG_City, aes(x=IsRepeatedGuest, y=ADR)) +  
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() +
  labs(x="Repeated Guests",y="ADR",
       title="Repeated Guests Vs ADR For Resort")
plotofrepeatedGuests_City

#Using ggplot to create a bar graph which shows ADR for guests that are repeated or not repeated in
#city hotels.

#For Resort
RG_Resort<- data.frame(ResortDS$IsRepeatedGuest,ResortDS$ADR)
names(RG_Resort) <- c('IsRepeatedGuest','ADR')
View(RG_Resort)

#Creating a dataframe called RG_Resort using the columns IsRepeatedGuest and ADR from ResortDS. Then
#using the name function to change the column names to 'IsRepeatedGuest' and 'ADR'.

plotofrepeatedGuests_Resort<-ggplot(data=RG_Resort, aes(x=IsRepeatedGuest, y=ADR)) + 
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() +
  labs(x="Repeated Guests",y="ADR",
       title="Repeated Guests Vs ADR For Resort")
plotofrepeatedGuests_Resort

#Using ggplot to create a bar graph which shows ADR for guests that are repeated or not repeated in
#resort hotels.

#8. Customer Type vs ADR

#For City
Customers_City<- data.frame(CityDS$CustomerType,CityDS$ADR)
names(Customers_City) <- c('CustomerType','ADR')
View(Customers_City)

#Creating a dataframe called Customers_City with the columns CustomerType and ADR from CityDS.
#Then using the names function to rename the columns 'CustomerType' and 'ADR'.

plotofrepeatedGuests_City<-ggplot(data=Customers_City, aes(x=CustomerType, y=ADR)) + 
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() +
  labs(x="Customer Type",y="ADR",
       title="Customer Type Vs ADR For City")
plotofrepeatedGuests_City

#Using ggplot to create a bar graph that shows the ADR across different customer types for city
#hotels.

#For Resort
Customers_Resort <- data.frame(ResortDS$CustomerType,ResortDS$ADR)
names(Customers_Resort) <- c('CustomerType','ADR')
View(Customers_Resort)

#Creating a dataframe called Customers_Resort with the columns CustomerType and ADR from ResortDS.
#Then using the names function to rename the columns 'CustomerType' and 'ADR'.

plotofcustomers_Resort<-ggplot(data=Customers_Resort, aes(x=CustomerType, y=ADR)) + 
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() +
  labs(x="Customer Type",y="ADR",
       title="Customer Type Vs ADR For Resort")
plotofcustomers_Resort

#Using ggplot to create a bar graph that shows the ADR across different customer types for resort
#hotels.


#9. Visitor Type Vs ADR 
#For City

VisitorType_City<- data.frame(CityDS_no_na$Visitor_Type,CityDS_no_na$ADR)
names(VisitorType_City) <- c('VisitorType','ADR')
View(VisitorType_City)

plotofvisitortype_City<-ggplot(data=VisitorType_City, aes(x=VisitorType, y=ADR)) + 
  geom_bar(stat="identity", fill="steelblue") + theme_minimal()+
  labs(x="Visitor Type",y="ADR",
       title="Visitor Type Vs ADR For City")
plotofvisitortype_City

#For Resort

VisitorType_Resort<- data.frame(ResortDS$Visitor_Type,ResortDS$ADR)
names(VisitorType_Resort) <- c('VisitorType','ADR')
View(VisitorType_Resort)

plotofvisitortype_Resort<-ggplot(data=VisitorType_Resort, aes(x=VisitorType, y=ADR)) +  
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() +
  labs(x="Visitor Type",y="ADR",
       title="Visitor Type Vs ADR For Resort")
plotofvisitortype_Resort



#10.Member Count Vs ADR

#For City

member_City<- data.frame(CityDS$Member_Count,CityDS$ADR)
names(member_City) <- c('Member','ADR')
View(member_City)

#Creating a dataframe called member_City which contains the columns Member_Count and ADR from the
#CityDS data. Then using the name function the columns are renamed to 'Member' and 'ADR'.

plotmember_City<-ggplot(data=member_City, aes(x=Member, y=ADR)) + 
  geom_bar(stat="identity", fill="steelblue") + theme_minimal()+
  labs(x="Member Count",y="ADR",
       title="Member Count Vs ADR For City")
plotmember_City

#Using ggplot to create a bar graph which shows the ADR per member count for city hotels.

#For Resort

member_resort<- data.frame(ResortDS$Member_Count,ResortDS$ADR)
names(member_resort) <- c('Member','ADR')
View(member_resort)

#Creating a dataframe called member_resort which contains the columns Member_Count and ADR from the
#ResortDS data. Then using the name function the columns are renamed to 'Member' and 'ADR'.

plotmember_resort<-ggplot(data=member_resort, aes(x=Member, y=ADR)) + 
  geom_bar(stat="identity", fill="steelblue") + theme_minimal()+ xlim(0,10) +
  labs(x="Member Count",y="ADR",
       title="Member Count Vs ADR For Resort")
plotmember_resort

#Using ggplot to create a bar graph which shows the ADR per member count for resort hotels.

#11.Special Requests Vs ADR

#For City

specialrequests_City<- data.frame(CityDS$TotalOfSpecialRequests,CityDS$ADR)
names(specialrequests_City) <- c('TotalOfSpecialRequests','ADR')
View(specialrequests_City)

#Creating a dataframe called specialrequests_City that contains the columns TotalOfSpecialRequests
#and ADR from CityDS. Then using the names function, the columns are renamed as 'TotalOfSpecialRequests'
#and ADR.

plotspecialrequests_City<-ggplot(data=specialrequests_City, aes(x=TotalOfSpecialRequests, y=ADR)) + 
  geom_bar(stat="identity", fill="steelblue") + theme_minimal()+
  labs(x="Special Requests",y="ADR",
       title="Special Requests Vs ADR For City")
plotspecialrequests_City

#Using ggplot to create a bar graph which shows the ADR per number of special requests in city hotels.

#For Resort

specialrequests_resort<- data.frame(ResortDS$TotalOfSpecialRequests,ResortDS$ADR)
names(specialrequests_resort) <- c('TotalOfSpecialRequests','ADR')
View(specialrequests_resort)

#Creating a dataframe called specialrequests_resort that contains the columns TotalOfSpecialRequests
#and ADR from ResortDS. Then using the names function, the columns are renamed as 'TotalOfSpecialRequests'
#and ADR.

plotspecialrequests_resort<-ggplot(data=specialrequests_resort, aes(x=TotalOfSpecialRequests, y=ADR)) + 
  geom_bar(stat="identity", fill="steelblue") + theme_minimal()+
  labs(x="Special Requests",y="ADR",
       title="Special Requests Vs ADR For Resort")
plotspecialrequests_resort

#Using ggplot to create a bar graph which shows the ADR per number of special requests in resort hotels.

#12. Average Revenue from Repeating Customers Vs One-Time Customers

# For City
one_time_customers_city_non_repeating <- CityDS[CityDS$IsRepeatedGuest==0,]

#Using subset() to create a new dataframe which contains only the data from CityDS which 
#includes guests that did not come more than once.

avg_revenue_non_repeating <- mean(one_time_customers_city_non_repeating$ADR_per_stay)
avg_revenue_non_repeating

#The average revenue from one time city customers was $323.71

one_time_customers_city_repeating <- CityDS[CityDS$IsRepeatedGuest==1,]

#Using subset() to create a new dataframe which contains only the data from CityDS which 
#includes guests that came more than once.

avg_revenue_repeating <- mean(one_time_customers_city_repeating$ADR_per_stay)
avg_revenue_repeating

#The average revenue from repeating city customers was $126.66

# For Resort
one_time_customers_resort_non_repeating <- ResortDS[ResortDS$IsRepeatedGuest==0,]

#Using subset() to create a new dataframe which contains only the data from ResortDS which 
#includes guests that did not come more than once.

avg_revenue_non_repeating_resort <- mean(one_time_customers_resort_non_repeating$ADR_per_stay)
avg_revenue_non_repeating_resort

#The average revenue from one time resort customers was $447.12

one_time_customers_resort_repeating <- ResortDS[ResortDS$IsRepeatedGuest==1,]

#Using subset() to create a new dataframe which contains only the data from ResortDS which 
#includes guests that came more than once.

avg_revenue_repeating_resort <- mean(one_time_customers_resort_repeating$ADR_per_stay)
avg_revenue_repeating_resort

#The average revenue from repeating resort customers was $184.19

#13. Meal Vs Customer Type Comparison

#Customer Type Vs Meal Visualization City Dataset

city_ds_agg <- CityDS %>% group_by(Meal, CustomerType) %>% summarise(count = n())

#Creating a matrix called city_ds_agg which shows the counts for Meal and CustomerType

ggplot(data = city_ds_agg, aes(x = Meal, fill = CustomerType, y = count)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Meal vs. Customer Type City Dataset")

#Using ggplot to create a bar graph which shows the different Meal plans on the x-axis
#and the count of meal plans on the y-axis. CustomerType is set to the bar graph fill, meaning
#the bars show the different customer types that had which Meal and how many of them did for city
#hotels.

#Customer Type Vs Meal Visualization Resort Dataset

resort_ds_agg <- ResortDS %>% group_by(Meal, CustomerType) %>% summarise(count = n())

#Creating a matrix called resort_ds_agg which shows the counts for Meal and CustomerType

ggplot(data = resort_ds_agg, aes(x = Meal, fill = CustomerType, y = count)) +
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Meal vs. Customer Type Resort Dataset")

#Using ggplot to create a bar graph which shows the different Meal plans on the x-axis
#and the count of meal plans on the y-axis. CustomerType is set to the bar graph fill, meaning
#the bars show the different customer types that had which Meal and how many of them did for resort
#hotels.

#14. Top 5 countries of maximum guests

#City

with(CityDS, barplot(rev(sort(table(Country))[0:-162]),main = "Top 5 countries in City dataset"))

#Using barplot() to create a bar graph which shows the top 5 countries represented in the CityDS
#dataset.

#Resort

with(ResortDS, barplot(rev(sort(table(Country))[0:-121]), main = "Top 5 countries in Resort dataset"))

#Using barplot() to create a bar graph which shows the top 5 countries represented in the ResortDS
#dataset.

##15 CREATING SUBSETS OF THE CITY AND RESORT TRANSIENT CUSTOMERS FOR PLOTTING 
## BARPLOTS AND HISTOGRAMS FURTHER.
transientDf_City <- subset(CityDS, CustomerType =="Transient")
transientDf_resort <- subset(ResortDS, CustomerType == "Transient")
table(transientDf_City$CustomerType)
table(transientDf_City$IsRepeatedGuest)

## CITY DATASET.
hist(main = "Transient guests visiting the hotel more than once",as.numeric(transientDf_City$IsRepeatedGuest), 
     ylab = "No. of Transient Guests", 
     xlab = "Repeaters or Not") ## TOTAL TRANSIENT GUESTS REPEATING/NOT REPEATING


repeating_barplot_city<-ggplot(data=CityDS, 
                               aes(x=CustomerType, y=IsRepeatedGuest)) +  
  geom_bar(stat="identity", fill="steelblue") + theme_minimal() +
  ggtitle("Total no. of guests repeating in every customer type") +
  xlab("Customer Type") + ylab("Repeating Frequency")

repeating_barplot_city ## TOTAL NUMBER OF GUESTS REPEATING IN EVERY CATEGORY.


## RESORT DATASET.
hist(main = "Transient guests visiting the hotel more than once",
     as.numeric(transientDf_resort$IsRepeatedGuest), 
     ylab = "No. of Transient Guests", 
     xlab = "Repeaters or Not") ## TOTAL TRANSIENT GUESTS REPEATING/NOT REPEATING

repeating_barplot_resort<-ggplot(data=ResortDS, aes(x=CustomerType, y=IsRepeatedGuest)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal() +ggtitle("Total no. of guests repeating in every customer type") + 
  xlab("Customer Type") + ylab("Repeating Frequency")
repeating_barplot_resort ## TOTAL NUMBER OF GUESTS REPEATING IN EVERY CATEGORY.



#16. Word Cloud

#For City
CountryCorpus_city <- corpus(CityDS$Country)

CountryCorpustoken_city <- tokens(CountryCorpus_city, remove_punct=TRUE)  
CountryDFM_city <- dfm(CountryCorpustoken_city, tolower = TRUE ) 
CountryDFM_city <- dfm_select(CountryDFM_city, pattern = stopwords("english"), selection = "remove", valuetype = "fixed")
View(CountryDFM_city)

textplot_wordcloud(CountryDFM_city, min_count = 1)

#For Resort

CountryCorpus_resort <- corpus(ResortDS$Country)

CountryCorpustoken_resort <- tokens(CountryCorpus_resort, remove_punct=TRUE)  
CountryDFM_resort <- dfm(CountryCorpustoken_resort, tolower = TRUE ) 
CountryDFM_resort <- dfm_select(CountryDFM_resort, pattern = stopwords("english"), selection = "remove", valuetype = "fixed")
View(CountryDFM_resort)

textplot_wordcloud(CountryDFM_resort, min_count = 1)

#17. Map

#For City

install.packages("countrycode")

# Map -- Number Of Reservations From Different Countries
Country_city <- CityDS[-which(CityDS$Country=="NULL"),]
table(Country_city$Country)
Country_city <- data.frame(table(Country_city$Country))
Country_city$Var1 <- as.character(Country_city$Var1)
colnames(Country_city) <- c("Country","NumberOfReservation")
world <- map_data('world')
library(countrycode)
Country_city$Country[which(Country_city$Country=="CN")] <- "CHN"
Country_city$Country[which(Country_city$Country=="TMP")] <- "TLS"
Country_city$Country_Name <- countrycode(Country_city$Country,"iso3c","country.name")
Country_city$Country_Name[which(Country_city$Country=="USA")] <- "USA"
dfNew_city <- merge(world, Country_city, all.x=TRUE, by.x="region", by.y="Country_Name")
dfNew_city <- dfNew_city[order(dfNew_city[,5]),]
mp_city <- ggplot(dfNew_city, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill= NumberOfReservation), colour = "white") +
  scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
  scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
  scale_fill_gradient(low = "blue", high="red") +
  labs(title="Number Of Reservations From Different Countries",
       y="Latitude", x="Longitude") +
  theme_light() 
mp_city

#For Resort

Country_resort <- ResortDS[-which(ResortDS$Country=="NULL"),]
table(Country_resort$Country)
Country_resort <- data.frame(table(Country_resort$Country))
Country_resort$Var1 <- as.character(Country_resort$Var1)
colnames(Country_resort) <- c("Country","NumberOfReservation")
world <- map_data('world')
library(countrycode)
Country_resort$Country[which(Country_resort$Country=="CN")] <- "CHN"
Country_resort$Country[which(Country_resort$Country=="TMP")] <- "TLS"
Country_resort$Country_Name <- countrycode(Country_resort$Country,"iso3c","country.name")
Country_resort$Country_Name[which(Country_resort$Country=="USA")] <- "USA"
dfNew_resort <- merge(world, Country_resort, all.x=TRUE, by.x="region", by.y="Country_Name")
dfNew_resort <- dfNew_resort[order(dfNew_resort[,5]),]
mp_resort <- ggplot(dfNew_resort, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill= NumberOfReservation), colour = "white") +
  scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
  scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
  scale_fill_gradient(low = "blue", high="red") +
  labs(title="Number Of Reservations From Different Countries",
       y="Latitude", x="Longitude") +
  theme_light() + coord_cartesian()
mp_resort



#18. Models 
str(CityDS)



Correlation_Matrix<-cor(H2_City[,c("ADR","IsCanceled","LeadTime","StaysInWeekendNights","StaysInWeekNights","Adults","Babies",
                                   "PreviousCancellations",      
                                   "PreviousBookingsNotCanceled","DaysInWaitingList",
                                   "RequiredCarParkingSpaces","TotalOfSpecialRequests" )], use="complete")

View(Correlation_Matrix)
#-------------------------------------------------------------------------------

lm1<-lm(ADR~Member_Count+TotalOfSpecialRequests,data=HotelDS)
summary(lm1)
#P-Value of F statistic is less than 0.05, so it is significant. 
#-------------------------------------------------------------------------------

logistic_model<- glm(IsCanceled ~ DaysInWaitingList + LeadTime + 
                       +   PreviousCancellations, family=binomial(logit), data=HotelDS)
summary(logistic_model)


mydata<-HotelDS
str(mydata)
type_convert(mydata)
str(mydata)

mydata<-as.data.frame(lapply(mydata, unlist))


zerocancel<-mydata[mydata$IsCanceled==0,c(1,2,6,7,15,16,17,18,24,29,34,33,32)] 
c(1,2,6,7,15,16,17,18,24,29,34,33,32)
onecancel<-mydata[mydata$IsCanceled==1,c(1,2,6,7,15,16,17,18,24,29,34,33,32) ]

table(onecancel$IsCanceled)

table(zerocancel$IsCanceled)

randomIndex<-sample(1:dim(onecancel)[1])
ten_thousandone<-randomIndex[1:10000]
onecancelsub<-onecancel[ten_thousandone,]


randomIndex_two<-sample(1:dim(zerocancel)[1])
ten_thousandone_two<-randomIndex_two[1:10000]
zerocancelsub<-zerocancel[ten_thousandone_two,]

finaldata<-rbind(zerocancelsub,onecancelsub)
View(finaldata)



randomIndex_final<-sample(1:dim(finaldata)[1])
randomIndex_final

#created a list of random indexes and stored it in randomIndex.

cutpt<- floor(2 * dim(finaldata)[1]/3)
cutpt
#as training data is two-third of the dataset, created cutpt which has 
#two-third value of dimension of dataset.

trainData <- finaldata[randomIndex_final[1:cutpt],]
#created trainData which has all columns till cutpt

testData <-finaldata[randomIndex_final[(cutpt+1):dim(finaldata)[1]],]
View(testData)
dim(trainData)


trainData$IsCanceled<-as.factor(trainData$IsCanceled)
testData$IsCanceled<-as.factor(testData$IsCanceled)

svmoutput<-ksvm(IsCanceled~., data=trainData, kernel="rbfdot",kpar="automatic", C=5, cross=5, prob.model=TRUE)
svmoutput


str(testData)
colnames(finaldata)
svmPred<-predict(svmoutput,trainData,type="response")
length(svmPred)
confusionMatrix(svmPred,trainData$IsCanceled)
head(svmPred)

#END OF PROJECT
