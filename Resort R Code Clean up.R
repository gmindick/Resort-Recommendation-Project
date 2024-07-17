# RESPONSE/DEPENDENT/OUTCOME Variable - ADR
# CATEGORICAL COLUMNS (By Default) - ReservationStatus, Meal, Country, MarketSegment, DistributionChannel, ReservedRoomType, AssignedRoomType, DepositType, 
# CustomerType
# Numerical Columns - Lead Time, StaysInWeekendNights, StaysInWeekNights, Adults, Children, Babies, PreviousCancellations, PreviousBookingsNotCanceled, BookingChanges,
# BookingChanges, DaysInWaitingList, ADR, RequiredCarParkingSpaces, TotalOfSpecialRequests
# Date Columns - Arrival.Date, ReservationStatusDate
# Ambiguous Columms - Agent (is a character column containing agent IDs), Company


cat('\014')
rm(list=ls())

EnsurePackage <- function(library) {
  library <- as.character(library)
  if(!require(library, character.only = T)){
    install.packages(pkgs = library,  repos="http://cran.r-project.org")
    require(library, character.only = T)
  }
}

EnsurePackage("gdata")
EnsurePackage("ggplot2")
EnsurePackage("readxl")
EnsurePackage("dplyr")
EnsurePackage("gridExtra")

#################################### Reading in the Data #################

getwd()
setwd("~/Desktop/Resort")
# Set working directory according to your config

resort_data <- data.frame(read_excel("H1-Resort.xlsx"))
city_data <- read_excel("H2-City.xlsx")


# colnames(resort_data)
# colnames(city_data)


########### Combining both dataset for better analysis.

# To combine rows using rbind, we need to match the row names. In both dataset, one column name is different, "Arrival.Date" in resort data & "Arrival Date" in city data

# converting  columns having dates as character to date data type. (Only 2 columns.....) and renaming arrival date column in both to apply rbind.

resort_data$ArrivalDate <- c(1:nrow(resort_data))
resort_data$ArrivalDate <- as.Date(resort_data$Arrival.Date)
resort_data$Arrival.Date <- NULL

resort_data$ReservationStatusDate <- as.Date(resort_data$ReservationStatusDate)


city_data$ArrivalDate <- c(1:nrow(city_data))
city_data$ArrivalDate <- as.Date(city_data$'Arrival Date')
city_data$'Arrival Date' <- NULL

resort_data$ReservationStatusDate <- as.Date(resort_data$ReservationStatusDate)

city_data$ReservationStatusDate <- as.Date(city_data$ReservationStatusDate)


resort_data$HotelType <- "resort"
city_data$HotelType <- "city"


# Now, we can combine the data.
combined_data <- rbind(city_data, resort_data)

#################################  CLEANING  DATA #########################################################################################################################
# -------------------------------------------------------------------------------------------------
###########################################################################################################################################################################


# View(combined_data)

# str(combined_data)

# summary(combined_data)  # Checking descriptive stats of columns.


# removing white spaces from categorical columns (https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r)
# because Deposit type column contains values like "No Deposit       " instead of "No Deposit".      
cols_to_be_rectified <- names(combined_data)[vapply(combined_data, is.character, logical(1))]
combined_data[,cols_to_be_rectified] <- lapply(combined_data[,cols_to_be_rectified], trimws)


#Cleaning column ADR. Removing excess characters and converting into numeric.
StringCleaner <- function(x) {
  x <- as.numeric(gsub(" .*", "", x))
  return(x)
}
combined_data$ADR <- StringCleaner(combined_data$ADR)


############ MAKING COLUMNS PROPER DATATYPE  #######

# https://datascience.stackexchange.com/questions/12018/when-to-choose-character-instead-of-factor-in-r

####### Convert character columns to factors and character columns containing numeric values to num.
#combined_data[sapply(combined_data, is.character)] <-
#  lapply(combined_data[sapply(combined_data, is.character)], as.factor)


combined_data$IsCanceled <- factor(combined_data$IsCanceled, labels = c("Not Canceled", "Canceled"))
combined_data$ReservationStatus <- as.factor(combined_data$ReservationStatus)
combined_data$Children <- as.numeric(combined_data$Children)
combined_data$Meal <- as.factor(combined_data$Meal)
combined_data$Country <- as.factor(combined_data$Country)
combined_data$MarketSegment <- as.factor(combined_data$MarketSegment)
combined_data$DistributionChannel <- as.factor(combined_data$DistributionChannel)
combined_data$IsRepeatedGuest <- factor(combined_data$IsRepeatedGuest,  labels = c("Not Repeated", "Repeated"))
combined_data$ReservedRoomType <- as.factor(combined_data$ReservedRoomType)
combined_data$AssignedRoomType <- as.factor(combined_data$AssignedRoomType)
combined_data$DepositType <- as.factor(combined_data$DepositType)
combined_data$CustomerType <- as.factor(combined_data$CustomerType)
combined_data$HotelType <- as.factor(combined_data$HotelType)


# str(combined_data)


#----------------------- checking for missing data  -------------------------

colSums(is.na(combined_data)) # Checking which columns have NA values.
# Arrival date has 39270 missing values. 
# we can get those values using ReservationStatusDate and TotalNumberOfDaysStayedWeekNIghts and TotalNumberOfDaysStayedWeekendNights where reservationstatus is checkout.

combined_data$ArrivalDate[which(is.na(combined_data$ArrivalDate) & combined_data$ReservationStatus == 'Check-Out')] <-
  combined_data$ReservationStatusDate - (combined_data$StaysInWeekendNights + combined_data$StaysInWeekNights)

# Now, 1519 rows remain that have NA in arrival date. We will remove those NA values using na.omit().
colSums(is.na(combined_data))  

# Removing all NA data.
combined_data <- na.omit(combined_data)   

anyNA(combined_data)   # FALSE meaning no NA values in dataset.


#################------------------- Adding new columns based on arrival date  -----------------##########################

#### SEASON COLUMN ######

EnsurePackage("lubridate")
combined_data$Season <- quarter(combined_data$ArrivalDate)
combined_data$Season[combined_data$Season == 1] <- "Spring"
combined_data$Season[combined_data$Season == 2] <- "Summer"
combined_data$Season[combined_data$Season == 3] <- "Fall"
combined_data$Season[combined_data$Season == 4] <- "Winter"

combined_data$Season <- as.factor(combined_data$Season)


### Arrival Year column ######

combined_data$ArrivalYear <- as.factor(format(combined_data$ArrivalDate, "%Y"))

#### Arrival Month Column ######

combined_data$ArrivalMonth <- as.factor(month.name[as.numeric(format(combined_data$ArrivalDate, "%m"))])


#######-----------------------Adding new categorical column, ParkingSpaceNeeded and AnySpecialRequests based on column 
# RequiredCarParkingSpaces and TotalOfSpecialRequests ----------------------------###########################
### Might be useful for association rule mining

combined_data$ParkingSpaceNeeded <- as.factor(ifelse(combined_data$RequiredCarParkingSpaces > 0 , "Yes", "No"))

combined_data$AnySpecialRequest <- as.factor(ifelse(combined_data$TotalOfSpecialRequests > 0 , "Yes", "No"))



###################----------------- adding new column Visitor Type. ----------------####################
########### Visitor Type can be Business Traveler, Solo Traveler, Couple and Family ########

table(combined_data$Adults)  # Can remove 0 adults column since babies and children won't visit alone assuming children need to be accompanied by an adult.

# Removing using filter command.
combined_data <- combined_data %>% dplyr::filter(Adults != 0)

# ADR is zero for adults greater than 4.  ( 16 rows)
combined_data$ADR[combined_data$Adults > 4]

# Removing those 16 rows as well.
combined_data <- combined_data %>% dplyr::filter(Adults < 5)

'%notin%' <- Negate('%in%')

# Adding the column
combined_data$VisitorType <- c(1:nrow(combined_data))   # Initializing

# SOLO TRAVELER
combined_data$VisitorType[which(combined_data$Adults == 1 & combined_data$Babies == 0 & combined_data$Children == 0  & 
                                  combined_data$CustomerType %notin% "Group" )] <- "Solo Traveler"

# COUPLE
combined_data$VisitorType[which(combined_data$Adults == 2 & combined_data$Babies == 0 & combined_data$Children == 0  & 
                                  combined_data$CustomerType %notin% "Group" )] <- "Couple"

# FAMILY
combined_data$VisitorType[which(combined_data$Adults >= 2 & (combined_data$Babies != 0 | combined_data$Children != 0 ) & combined_data$DistributionChannel != "Corporate" &
                                  combined_data$CustomerType %notin% "Contract")] <- "Family"

# BUSINESS TRAVEL
combined_data$VisitorType[which(combined_data$Babies == 0 & combined_data$Children == 0 & combined_data$DistributionChannel == "Corporate")] <- "Business Travel"


dataframe <-
  combined_data[which(
    combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family")
  ),
  c("Adults",
    "Children",
    "Babies",
    "DistributionChannel",
    "CustomerType")]

#Analyzing above dataframe, remaining rows are majorly 3 adults having either 1 or more children or 1 or more babies and distribution channel as TA/TO - can classify 
# them as families 

# FAMILY
combined_data$VisitorType[which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel") & combined_data$Adults >= 3 
                                & combined_data$DistributionChannel != "Corporate" & combined_data$CustomerType %notin% "Contract")] <- "Family"
# Families rarely will have contract bookings, so excluded Customer Type as Contract.

# Checking rows which are left to be labeled a visitor type
length(combined_data$VisitorType[which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family"))])      # 1214 rows.

# Removing remaining 1214 rows.
combined_data <- combined_data %>% slice(-which(combined_data$VisitorType %notin% c("Solo Traveler", "Couple", "Business Travel", "Family")))





#########################-------------Analyzing IsCanceled, ReservationStatus for any mismatched data-------------------##############################


#                         ReservationStatus
# IsCancelled      
#                 Check-Out         No Show        Canceled
# Canceled				   X                 
# Not Canceled                                        X 

# There shouldn't be any rows where ReservationStatus is Check-Out and Iscanceled shows Canceled. 
length(which(combined_data$IsCanceled == "Canceled" & combined_data$ReservationStatus %in% "Check-Out"))  # 0 rows. THIS IS FINE!!!!

# Similarly, there shouldn't be any rows where reservation Status is Canceled and IsCanceled contains Not Canceled.
length(which(combined_data$IsCanceled == "Not Canceled" & combined_data$ReservationStatus %in% "Canceled")) # 0 rows. THIS IS FINE!!!!   


####################----------------creating new Columns 1. Total Number of Days Stayed and 2. Average Revenue per Stay -------------############################### 

# Summing up no of days stayed in Weekend and Weekdays
combined_data$TotalNumberOfDaysStayed <- combined_data$StaysInWeekendNights + combined_data$StaysInWeekNights

# Checking how many rows have total number of days stayed as zero.
length(which(combined_data$TotalNumberOfDaysStayed == 0))   # Does it make sense to have this as 0? I dont think so.

# Removing rows where TotalNumberOfDaysStayed = 0
combined_data <- combined_data %>% slice(-which(combined_data$TotalNumberOfDaysStayed == 0))  

# Before creating new column, let's get rid of outliers in ADR.
summary(combined_data$ADR) # This shows 3rd quartile is 126 while max is 5400.

# Boxplot of ADR is difficult to analyze because of that max value. We will remove that big outlier.
ggplot(data = combined_data) + geom_boxplot(mapping = aes(x = ADR)) + coord_flip() 

# removing that one big outlier of ADR
combined_data <- combined_data[-which(combined_data$ADR > 5000),] 

# Now draw boxplot again.
ggplot(data = combined_data) + geom_boxplot(mapping = aes(x = ADR)) + coord_flip()  

#New Column 
combined_data$AvgRevenuePerStay <- combined_data$TotalNumberOfDaysStayed * combined_data$ADR


####################----------------creating new Column ADRType (Categorizing ADR values by a range) -------------###############################

# Histogram of ADR 
ggplot(data = combined_data) + geom_histogram(mapping = aes(x = ADR),color = "white") +
  scale_x_continuous(breaks = seq(from = 0, to = 500, by = 50)) + 
  geom_vline(xintercept = quantile(combined_data$ADR, 0.75), col = "red") +
  geom_vline(xintercept = quantile(combined_data$ADR, 0.15), col = "blue")

# Based on the plot, maybe we can create a new categorical column where 0-50 can be low ADR, 50-150 can be medium ADR and above 150 can be high ADR.
combined_data$ADRType <- cut(combined_data$ADR, breaks = c(0,50,150, Inf), labels = c("Low", "Medium", "High"), include.lowest = T)



######################   ------- Final Check for NA values after creating new columns ###################################

colSums(is.na(combined_data))   # ADRType has 1 NA.

# Removing that 1 NA value
combined_data <- na.omit(combined_data)


################---------------------- Looking for Outliers --------------------------------------------#########################

EnsurePackage("dlookr")
diagnose_outlier(combined_data)

str(combined_data)



################################################   CLEANING DONE!!!!!! READY TO MAKE PLOTS ############################################
# -------------------------------------------------------------------------------------------------------------------------------------
#######################################################################################################################################





############ Tables for variables. #################
str(combined_data)

sort(table(combined_data$IsCanceled), decreasing = T)
sort(table(combined_data$ReservationStatus), decreasing = T)

sort(table(combined_data$StaysInWeekendNights), decreasing = T)
sort(table(combined_data$StaysInWeekNights), decreasing = T)
sort(table(combined_data$Adults), decreasing = T)
sort(table(combined_data$Children), decreasing = T)
sort(table(combined_data$Babies), decreasing = T)

sort(table(combined_data$Meal), decreasing = T)
sort(table(combined_data$Country), decreasing = T)
sort(table(combined_data$MarketSegment), decreasing = T)
sort(table(combined_data$DistributionChannel), decreasing = T)
sort(table(combined_data$IsRepeatedGuest), decreasing = T)

sort(table(combined_data$PreviousCancellations), decreasing = T)
sort(table(combined_data$PreviousBookingsNotCanceled), decreasing = T)

sort(table(combined_data$ReservedRoomType), decreasing = T)
sort(table(combined_data$AssignedRoomType), decreasing = T)
sort(table(combined_data$BookingChanges), decreasing = T)
sort(table(combined_data$DepositType), decreasing = T)
sort(table(combined_data$DaysInWaitingList), decreasing = T)
sort(table(combined_data$CustomerType), decreasing = T)
sort(table(combined_data$RequiredCarParkingSpaces), decreasing = T)
sort(table(combined_data$TotalOfSpecialRequests), decreasing = T)
sort(table(combined_data$HotelType), decreasing = T)

sort(table(combined_data$ArrivalYear), decreasing = T)
sort(table(combined_data$ArrivalMonth), decreasing = T)
sort(table(combined_data$Season), decreasing = T)

sort(table(combined_data$ParkingSpaceNeeded), decreasing = T)

sort(table(combined_data$AnySpecialRequest), decreasing = T)


sort(table(combined_data$VisitorType), decreasing = T)
sort(table(combined_data$ADRType), decreasing = T)








########################## 


#######################################################------------- PLOTS  -----------------------###################################################

#Plot Boxplots/ Stacked Barcharts / Histogram / Map (since there is country data).
# Grouped Boxplots  --  Categorical variables on x-axis and numerical variables on y-axis.

# Cleaned resort data and city data seperately if needed for plots
resort_data <- combined_data %>% dplyr::filter(HotelType == "resort")
city_data <- combined_data %>% dplyr::filter(HotelType == "city")


############### -------------- Histogram of Numerical Variables -------------#################################



dplyr::select_if(combined_data, is.numeric)
# Lead Time , DaysInWaitingList, TotalNumberofDaysStayed




# 1. Visualizing range of ADR by number of Adults and Hotel Type

resort_plot <- ggplot(data = resort_data, aes(x = Adults, y = ADR)) + geom_point(color = "red") +
  coord_cartesian(xlim=c(1,4)) + scale_x_continuous(breaks = seq(1,4,1)) + xlab("Number of adults") + 
  ggtitle("Resort")

city_plot <- ggplot(data = city_data, aes(x = Adults, y = ADR)) + geom_point(color = "blue") +
  coord_cartesian(xlim=c(1,4)) + scale_x_continuous(breaks = seq(1,4,1)) + xlab("Number of adults") +
  ggtitle("City")

grid.arrange(resort_plot, city_plot, ncol = 2)



# 2 Season_VisitorType_ADR

Season_VisitorType_ADR <- data.frame(aggregate(combined_data$ADR, 
                                               by=list(type=combined_data$VisitorType,type=combined_data$Season),mean))
colnames(Season_VisitorType_ADR) <- c("VisitorType","Season","mean_ADR")
b1 <- ggplot(data=Season_VisitorType_ADR, mapping=aes(x = Season, y = mean_ADR, fill=VisitorType))+
  geom_bar(stat="identity",position=position_dodge(0.75),width=0.6)
b1

# ADR for visitor types of families is the highest regardless of the season, so offering discounts for 
###families to attract more family travelers, especially in the summer, is a good way to increase overall ADR.
###2. In addition, the ADR for visitor types of couples and singles is similar in most seasons, higher for 
###couples in the spring and higher for singles in the summer. Therefore, we should make it a secondary 
###priority to attract couple type visitors to stay in spring, and try to attract more single visitors in summer.

#3 Season_TotalNumberOfDaysStayed_ADR
colnames(combined_data)
Season_TotalNumberOfDaysStayed_ADR <- data.frame(aggregate(combined_data$ADR, 
                                          by=list(type=combined_data$TotalNumberOfDaysStayed,type=combined_data$Season),mean))
colnames(Season_TotalNumberOfDaysStayed_ADR) <- c("TotalNumberOfDaysStayed","Season","mean_ADR")
b2 <- ggplot(data=Season_TotalNumberOfDaysStayed_ADR, mapping=aes(x = TotalNumberOfDaysStayed, y = mean_ADR, color=Season))+
  geom_line()
b2

# In spring, ADR is highest for stays around 20 days and lower for stays around 16 or 17 days and after 23 
###days. Therefore, visitors with stays around 16 or 17 days, or greater than 23 days should be enticed to
###upgrade their stays to around 20 days by adjusting the price or offering some free services.
###2. In summer, when the length of stay is 10-20 days, the ADR fluctuates sharply. Therefore, hotels can 
###control the length of stay at 10, 13 or 16 days by offering package deals.
###3. In fall, the ADR is at its highest at 14 days of stay, so hotels should offer trips with 14 days of stay.
###Also when the number of stay days is greater than 18, ADR drops significantly and is at a lower level 
###until 26 days when it returns to normal. Therefore for visitors who stay in greater than 18 but less than 
###26 days, hotel should increase the rates to avoid the number of days of stay in this time period.
###4. In winter, ADR rises rapidly when the number of days of stay is greater than 28 days, so hotels should 
###attract visitors for longer stays (greater than 28 days) in winter.






#########################################################################################

###### Multiple Histograms from Grouped Data   ###############
# https://r-graphics.org/recipe-distribution-multi-hist

# 1. Analyzing ADR based on season (Seeing the distribution and Mean(ADR) season wise) ###################

# Calculate mean of ADR season wise to add a line to plot 
# https://stackoverflow.com/questions/63016568/how-to-plot-multiple-mean-lines-in-a-single-histogram-with-multiple-groups-prese
mean_stats_season_wise <- resort_data %>% group_by(Season) %>% summarise(mean = mean(ADR),n = n())

# create the plot
ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(Season ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red") + 
  geom_vline(data = mean_stats_season_wise, aes(xintercept = mean, color = Season), size = 1)


# Fall season, ADR is high as compared to other seasons. 


#2. Analyzing ADR based on VisitorType (Seeing the distribution and Mean(ADR) season wise) ###################

# Calculate mean of ADR VisitorType wise to add a line to plot 
# https://stackoverflow.com/questions/63016568/how-to-plot-multiple-mean-lines-in-a-single-histogram-with-multiple-groups-prese
mean_stats_VisitorType_wise <- resort_data %>% group_by(VisitorType) %>% summarise(mean = mean(ADR),n = n())

# create the plot
ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(VisitorType ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red") + 
  geom_vline(data = mean_stats_VisitorType_wise, aes(xintercept = mean, color = VisitorType), size = 1)


# 3. Analyzing ADR and customer type 

ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(CustomerType ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red")

# 4. Analyzing ADR and deposit type 

ggplot(data = resort_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(DepositType ~ .) + scale_x_continuous(breaks = seq(0,400,40)) +
  scale_fill_gradient("Count", low="green", high="red")








#########################################################################################






########################## Boxplots  #####################

##################### 1. Deposit Type and ADR ####################################

#Basic plot with outliers in magenta color. 
ADR_DepositType_Boxplot <- ggplot(data = resort_data) + aes(x = DepositType, y = ADR, group = DepositType) + 
  geom_boxplot(outlier.colour="magenta")
ADR_DepositType_Boxplot


#Adding color to boxes 
ADR_DepositType_Boxplot <-  ADR_DepositType_Boxplot + 
  aes(color = DepositType) 
ADR_DepositType_Boxplot

# Writing count and mean of datapoints for all categories. (https://gscheithauer.medium.com/how-to-add-number-of-observations-to-a-ggplot2-boxplot-b22710f7ef80)

stat_box_data <- function(y, upper_limit = max(resort_data$ADR) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

ADR_DepositType_Boxplot <- ADR_DepositType_Boxplot + 
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  )

#Another way to see the density of data points - Box plot displaying the data points to see for which category we have more data
ADR_DepositType_Boxplot <- 
  ADR_DepositType_Boxplot + geom_jitter(shape=16, position=position_jitter(0.2))

ADR_DepositType_Boxplot    # FINAL BOXPLOT



####################### Meal and ADR #####################################


#Basic plot with outliers in magenta color. 
ADR_Meal_Boxplot <- ggplot(data = resort_data) + aes(x = Meal, y = ADR, group = Meal) + 
  geom_boxplot(outlier.colour="black")
ADR_Meal_Boxplot


#Adding color to boxes 
ADR_Meal_Boxplot <-  ADR_Meal_Boxplot + 
  aes(color = Meal) 
ADR_Meal_Boxplot

# Writing count and mean of datapoints for all categories. (https://gscheithauer.medium.com/how-to-add-number-of-observations-to-a-ggplot2-boxplot-b22710f7ef80)

stat_box_data <- function(y, upper_limit = max(resort_data$ADR) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

ADR_Meal_Boxplot <- ADR_Meal_Boxplot + 
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  )
ADR_Meal_Boxplot

#Another way to see the density of data points - Box plot displaying the data points to see for which category we have more data
ADR_Meal_Boxplot <- 
  ADR_Meal_Boxplot + geom_jitter(shape=16, position=position_jitter(0.2))

# Changing X-axis labels to make it more clearer.
ADR_Meal_Boxplot <- 
  ADR_Meal_Boxplot + scale_x_discrete(labels = c("BB" = "Bed & Breakfast", "FB" = "Full Board", "HB" = "Half Board"))

ADR_Meal_Boxplot     # FINAL BOXPLOT

# CAN WE REMOVE SC because of less data points or can we combine SC and Undefined into one column??????????????


# ##########################################################################
# #ADR and adults.
# # with(resort_data, boxplot(ADR ~ Adults))
# 
# summary(resort_data$Adults)
# 
# #Basic plot (group = Adults is important to get different number of adults on x-axis.)
# ADR_Adults_Boxplot <- ggplot(data = resort_data) + aes(x = Adults, y = ADR, group = Adults) + 
#   geom_boxplot()
# 
# #Limiting the number of adults to 4.
# ADR_Adults_Boxplot + xlim("0", "1")
# 
# 
# 
# 
# # Countries and ADR
# 
# table(resort_data$Country)  # Getting the count of Country Values to figure out which countries to analyze. 
# # mean(table(resort_data$Country))
# 
# length(which(table(resort_data$Country) > 100)) # Checking how many countries have data points greater than 100 and using only those in the boxplot.
# 
# ADR_Country_Boxplot <- ggplot(data = resort_data) + aes(x = Country, y = ADR, group = Country) + 
#   geom_boxplot()
# 
# ADR_Country_Boxplot
# 
# 
# 
# # IsRepeatedGuest and ADR
# 
# #Basic plot
# ADR_IsRepeatedGuest_Boxplot <- ggplot(data = resort_data) + aes(x = IsRepeatedGuest, y = ADR, group = IsRepeatedGuest) + 
#   geom_boxplot()
#  
# ADR_IsRepeatedGuest_Boxplot

#################################################################################################################################


######################## PLOTTING MAP FOR COUNTRY DATA ###########################

#### NEED TO REFINE THIS. JUST A RAW IDEA OF HOW WE CAN DO THIS. ###############

# I think, first we need to convert country codes to full names and get the latitude and longitude for that and then 
# we can plot a jitter plot showing each occurence of a country as a point on the map. That way we will know from which country most of our visitors
# come from.

# (https://stackoverflow.com/questions/26818257/how-to-convert-country-codes-into-country-names-in-a-column-within-a-data-frame)

EnsurePackage("countrycode")

# There are different formats. Ours is ISO3 (https://unstats.un.org/unsd/tradekb/knowledgebase/country-code)

?countrycode
?codelist

resort_data$countryFullName <- countrycode(resort_data$Country, "iso3c", "country.name")
sort(table(resort_data$countryFullName))

colSums(is.na(resort_data))
# BUt using this, we get some NA values. Need to think how to deal with them. Gotta deal with United States as well. 

# Now, we have got country names, we need to get central coordinates of those country names. 

EnsurePackage("rgeos")
EnsurePackage("rworldmap")

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids)
# df$Country <- rownames(df)  # Converting rownames to a column. 
head(df)


# To be continued - how to match two dataframes based on values???????  -> Can use match() function.
# https://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc


resort_data$Countrylat <- df[match(resort_data$countryFullName, rownames(df)),"y"]
resort_data$Countrylon <- df[match(resort_data$countryFullName, rownames(df)),"x"]

# Again, more NA values. 


mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the countries on top
mp <- mp+ geom_point(aes(x=resort_data$Countrylon, y=resort_data$Countrylat) ,color="blue", size=1) 
mp + coord_map()



##################################################################################

# Bar charts - 

# We will have to reshape data to create side by side bar charts. Using tidyr, we can do that. pivot_longer() and pivot_wider() functions. 

EnsurePackage("ggthemes")

length(resort_data$IsCanceled)

# IsCancelled and DepositType


# IsCancelled and CustomerType








#  We can plot how many hotels were canceled for 2015, 2016, 2017


str(resort_data)

resort_data$Canceled <- ifelse(resort_data$IsCanceled == 1, "Canceled", "Not Canceled")

ggplot(data = resort_data, aes(x = Season, y = ADR, fill = Canceled)) +
  geom_bar(position = "dodge", stat = "identity")


ggplot(data = resort_data, aes(x = Season, y = ADR, fill = IsRepeatedGuest)) +
  geom_bar(position = "dodge", stat = "identity")


resort_data$ADR[which(resort_data$ADR <= 0)]



##################################################################################################

mean(combined_data$ADR)
colnames(combined_data)

mean(combined_data[])

ADR_By_Season <- ggplot(data = combined_data) + aes(x = Season, y = ADR, group = Season) + 
  geom_boxplot(outlier.colour="magenta")
ADR_By_Season

barplot(combined_data$ADR)

barplot(combined_data,names.arg=Season,xlab="Season",ylab="ADR",col="blue",
        main="Revenue chart",border="red")
Season <- combined_data$Season
ADR <- combined_data$ADR

ADRPlot <- ggplot(data=combined_data) + aes(x=Season, y=ADR) + geom_point() + 
  geom_smooth(method="lm", se=FALSE)
ADRPlot


counts <- table(combined_data$Season, combined_data$ADR)
barplot(counts, main="ADR by Season",
        xlab="Season")
###### Pie Plots #######################################################################

#R - code file for all the pieplots
#the code needs to placed in the main code file to run
install.packages("ggrepel")
library("ggrepel")
#plot piechat for visitor type
View(combined_data)
View(visitortable)
visitortable <- data.frame(table(combined_data$VisitorType))
visitortable$percent <- round(visitortable$Freq/sum(visitortable$Freq) * 100,digits = 2)
visitorplot <- ggplot(visitortable, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Visitor Type"))+ theme_void()
visitorplot

#meal,visitortype,customertype,season,deposit#plot
#plot piechart for mealplan
mealplan <-  data.frame(table(combined_data$Meal))
View(mealplan)
mealplan$percent <- round(mealplan$Freq/sum(mealplan$Freq) * 100,digits = 2)
mealplanplot <- ggplot(mealplan, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Meal Plan"))+ theme_void()
mealplanplot

#plot piechart for customertype
customertypeframe <-  data.frame(table(combined_data$CustomerType))
View(customertypeframe)
customertypeframe$percent <- round(customertypeframe$Freq/sum(customertypeframe$Freq) * 100,digits = 2)
customertypeplot <- ggplot(customertypeframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Customer Type"))+ theme_void()
customertypeplot

#plot piechart for season
seasonframe <-  data.frame(table(combined_data$Season))
View(seasonframe)
seasonframe$percent <- round(seasonframe$Freq/sum(seasonframe$Freq) * 100,digits = 2)
seasonframeplot <- ggplot(seasonframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Season"))+ theme_void()
seasonframeplot

#plot piechart for deposit
depositframe <-  data.frame(table(combined_data$DepositType))
View(seasonframe)
depositframe$percent <- round(depositframe$Freq/sum(depositframe$Freq) * 100,digits = 2)
depositframeplot <- ggplot(depositframe, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1,color = "white")  +
  coord_polar("y", start=0) +geom_label_repel(aes(label = percent), size=3, show.legend = F, nudge_x = 1) + guides(fill = guide_legend(title = "Deposit Status"))+ theme_void()
depositframeplot

table(resort_data$IsCanceled)
table(city_data$IsCanceled)
table(combined_data$IsCanceled)


season <- data.frame(combined_data$Season)
ADR <- data.frame(combined_data$ADR)
barplot

View(combined_data)
combined_data$IsRepeatedGuest

visitortypedf <- combined_data$VisitorType == "Couple"
library(arules)
data(visitortypedf)
itemFrequencyPlot(visitortypedf, support = 0.1)
visitortypedf
View(visitortypedf)


mean_stats_hotelType <- combined_data %>% group_by(HotelType) %>% summarise(mean = mean(ADR),count = n())

mean_stats_hotelType <- combined_data %>% group_by(HotelType) %>% summarise(mean = mean(ADR),count = n())



# create the plot
ggplot(data = combined_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(HotelType ~ .) + scale_x_continuous(breaks = seq(0,500,25)) +
  #scale_fill_gradient("Count", low="green", high="red") + 
  geom_vline(data = mean_stats_hotelType, aes(xintercept = mean, color = HotelType), size = 1) + 
  theme_grey()
# What visitor type has the most cancellations, and how can we prevent certain types from not cancelling? 

EnsurePackage("ggplot2")
EnsurePackage("arules")
EnsurePackage("arulesViz")

# Resort
#using dplyr package, making a new dataframe that contains count 
VisitorType_IsCancelled_resort <- resort_data %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

# Making the plot
VisitorType_IsCancelled_resort_plot <-
  ggplot(
    data = (VisitorType_IsCancelled_resort) ,
    mapping = aes(x = VisitorType, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(breaks = seq(0,30000,2500)) +
  scale_fill_brewer(palette = "Blues") +
  theme_grey() +
  labs(title = "Resort",
       x = "VisitorType",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 
VisitorType_IsCancelled_resort_plot


# City
#using dplyr package, making a new dataframe that contains count 
VisitorType_IsCancelled_city <- city_data %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

# Making the plot

VisitorType_IsCancelled_city_plot <-
  ggplot(
    data = (VisitorType_IsCancelled_city) ,
    mapping = aes(x = VisitorType, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge())  +
  scale_y_continuous(breaks = seq(0,30000,5000)) +
  scale_fill_brewer(palette = "Blues") +
  theme_grey() +
  labs(title = "City",
       x = "VisitorType",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
VisitorType_IsCancelled_city_plot

# Arranging both plots in same figure
grid.arrange(VisitorType_IsCancelled_resort_plot,
             VisitorType_IsCancelled_city_plot,
             nrow = 2)


#### Now we need to get some rules why couples are canceling in the city more?
### USE Association Rules Mining



city_couples <- city_data %>% dplyr::filter(VisitorType == "Couple")  # Using dplyr package, filtering rows for couples only.

# now, we create a dataframe containing only factor columns so that we can apply association rules mining. 
city_couples <- city_couples %>% select_if(is.factor)
str(city_couples)

# Remove column ReservationStatus
city_couples <- city_couples %>% select(-ReservationStatus)

# Need to convert our dataframe to a transaction matrix 
city_couples <- as(city_couples, "transactions")

# Inspecting the first transaction
inspect(city_couples[1])

# create a frequency plot
itemFrequencyPlot(city_couples, topN = 20, cex.names = 0.75)


rules <- apriori(data = city_couples, parameter = list(support = 0.2, confidence = 0.8), appearance = list(rhs = c("IsCanceled=Canceled")))

inspect(sort(rules, by = "lift", decreasing = F))




#68  Business_Ques_b(ii).R 
#@@ -0,0 +1,68 @@
  # What season has the most cancellations, and how can we prevent other seasons from having less cancellations? 
  
  
  
  EnsurePackage("ggplot2")
EnsurePackage("arules")
EnsurePackage("arulesViz")

# Resort
#using dplyr package, making a new dataframe that contains count 
Season_IsCancelled_resort <- resort_data %>% group_by(Season, IsCanceled) %>% summarise(count = n())

# Making the plot
Season_IsCancelled_resort_plot <-
  ggplot(
    data = (Season_IsCancelled_resort) ,
    mapping = aes(x = Season, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(breaks = seq(0,30000,2500)) +
  scale_fill_brewer(palette = "Blues") +
  theme_grey() +
  labs(title = "Resort",
       x = "Season",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) 
Season_IsCancelled_resort_plot


# City
#using dplyr package, making a new dataframe that contains count 
Season_IsCancelled_city <- city_data %>% group_by(Season, IsCanceled) %>% summarise(count = n())

# Making the plot

Season_IsCancelled_city_plot <-
  ggplot(
    data = (Season_IsCancelled_city) ,
    mapping = aes(x = Season, fill = IsCanceled, y = count)
  ) +
  geom_bar(stat = "identity", position = position_dodge())  +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_brewer(palette = "Blues") +
  theme_grey() +
  labs(title = "City",
       x = "Season",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
Season_IsCancelled_city_plot 

# Arranging both plots in same figure
grid.arrange(Season_IsCancelled_resort_plot,
             Season_IsCancelled_city_plot,
             nrow = 2)




# For city, summer has more canceled hotel than not canceled hotel.
# In other seasons also, people tend to cancel more in city than resort

# Use association rules mining to analyze

# create the plot
ggplot(data = combined_data, aes(x = ADR)) + geom_histogram(color = "black", aes(fill = ..count..), binwidth = 10) +
  facet_grid(HotelType ~ .) + scale_x_continuous(breaks = seq(0,500,25)) +
  #scale_fill_gradient("Count", low="green", high="red") + 
  geom_vline(data = mean_stats_hotelType, aes(xintercept = mean, color = HotelType), size = 1) + 
  theme_grey()

RepeatedGuests <- combined_data %>% filter(IsRepeatedGuest %in% "Repeated")
RepeatedGuests_city <- RepeatedGuests %>% filter(HotelType == "city")
RepeatedGuests_resort <- RepeatedGuests %>% filter(HotelType == "resort")

# Most of the repeated Guests are Business Travelers. 
table(RepeatedGuests$VisitorType)   

#########----------- Showing This as pie plot -----------############

# Creating a blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=13, face="bold"),
    legend.title=element_text(size=13, face="bold")
  )

RepeatedGuests_VisitorType_count <- RepeatedGuests %>% group_by(VisitorType) %>% summarise(count = n())

ggplot(data = RepeatedGuests_VisitorType_count, aes(x ="", y = count, fill = VisitorType)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  ggtitle("Percentage of Repeated Guests by Visitors") +
  geom_text(aes(x = 1.8, label = paste0(round((count/sum(count) * 100),1), "%")), position = position_stack(vjust = 0.5)) 




# Q1 - What hotel do repeated Guests prefer and Cancelation Status?

IsCanceled_HotelType_RepeatedGuest <- RepeatedGuests %>% group_by(HotelType, IsCanceled) %>% summarise(count = n())

ggplot(data = IsCanceled_HotelType_RepeatedGuest, aes(x = HotelType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Cancelation Rate Hotel Wise for Repeated Guests",
       x = "Type of Hotel",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,1500,150))

# If a guest is repeated, he is likely to cancel a City hotel than a resort hotel.


# Q2 - What type of visitors are repeated guests?


##### 2(i) Segregating by Hotel Type - 

HotelType_VisitorType_RepeatedGuest <- RepeatedGuests %>% group_by(VisitorType, HotelType) %>% summarise(count = n())

ggplot(data = HotelType_VisitorType_RepeatedGuest, aes(x = VisitorType, fill = HotelType, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "What type of visitors are repeated guests?",
       x = "Type of Visitor",
       y = "Count",
       fill = "Hotel Type") +
  scale_y_continuous(breaks = seq(0,1500,150))


#### 2 (ii)   - By Cancellation rate  ( For both resorts and Hotels)

# Resort 
IsCanceled_VisitorType_RepeatedGuest_resort <- RepeatedGuests_resort %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

IsCanceled_VisitorType_RepeatedGuest_resort_plot <- ggplot(data = IsCanceled_VisitorType_RepeatedGuest_resort, aes(x = VisitorType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "Resort hotels",
       x = "Type of Visitor",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,500,50))
IsCanceled_VisitorType_RepeatedGuest_resort_plot

# City
IsCanceled_VisitorType_RepeatedGuest_city <- RepeatedGuests_city %>% group_by(VisitorType, IsCanceled) %>% summarise(count = n())

IsCanceled_VisitorType_RepeatedGuest_city_plot <- ggplot(data = IsCanceled_VisitorType_RepeatedGuest_city, aes(x = VisitorType, fill = IsCanceled, y = count)) + 
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.title = element_text(size = 15)) +
  labs(title = "City hotels",
       x = "Type of Visitor",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0,1000,100))

IsCanceled_VisitorType_RepeatedGuest_city_plot
# Arrange both plots together 
grid.arrange(IsCanceled_VisitorType_RepeatedGuest_resort_plot, IsCanceled_VisitorType_RepeatedGuest_city_plot, nrow = 2)

combined <- IsCanceled_VisitorType_RepeatedGuest_city_plot + IsCanceled_VisitorType_RepeatedGuest_resort_plot & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# If the repeated guests are couples, they are more likely to cancel in city. 


# Month wise analysis  - 

######## ------- Cancelation Status ------------ ##########

ArrivalMonth_IsCanceled_resort <- resort_data %>% group_by(ArrivalMonth, IsCanceled) %>% summarise(count = n(), mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_resort, aes(x = ArrivalMonth, fill = IsCanceled, y = count)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Cancelation Status by Arrival Month for Resort hotels",
       x = "Arrival Month",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0, 3000, 300)) +
  scale_x_discrete(limits = month.name)


ArrivalMonth_IsCanceled_city <- city_data %>% group_by(ArrivalMonth, IsCanceled) %>% summarise(count = n(), mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_city, aes(x = ArrivalMonth, fill = IsCanceled, y = count)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.75),
           width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Cancelation Status by Arrival Month for City hotels",
       x = "Arrival Month",
       y = "Count",
       fill = "Cancelation Status") +
  scale_y_continuous(breaks = seq(0, 6000, 500)) +
  scale_x_discrete(limits = month.name)

####### ------------------------------------------- #############################


########### ------------------------- ADR -----------------###############


ArrivalMonth_IsCanceled_combined <- combined_data %>% group_by(ArrivalMonth, HotelType) %>% summarise(mean_ADR = mean(ADR))

ggplot(data = ArrivalMonth_IsCanceled_combined, aes(x = ArrivalMonth, y = mean_ADR, fill = HotelType)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0,200,25)) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Average Daily Rate over different months",
       x = "Arrival Month",
       y = "Average Daily Rate (in Euros)",
       fill = "Hotel Type")

# see in June and July, resort ADR shoots up.

######################------------------------------#############################



####################------------------------------ Lead time --------------------- ##########################


ggplot(data = combined_data, aes(x = ArrivalMonth, group = ArrivalMonth, y = LeadTime)) +
  geom_boxplot() + 
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0,600,40)) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")
  ) +
  labs(title = "Month wise Analysis of how many days earlier people book hotel?",
       x = "Arrival Month",
       y = "How many days earlier people book hotel?")



# Lead time is more in May, June. July (Summer Months) as people start planning for vacations beforehand. 


######################------------------------------#############################






############ ----------------  visitor types ---------------- #############################


VisitorType_ArrivalMonth_combined <- combined_data %>% group_by(VisitorType, ArrivalMonth) %>% summarise(count = n())


ggplot(data = VisitorType_ArrivalMonth_combined, aes(x = ArrivalMonth, fill = VisitorType, y = count)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(breaks = seq(0,8000,1000)) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Number of Visitors over different months",
       x = "Arrival Month",
       y = "Number of Visitors",
       fill = "Visitor Type")

# Family visits - Increase in July and August. Very less in November.
# Solo travelers - MOre in september and October.



######################------------------------------#############################
couples_city <- city_data %>% filter(VisitorType == "Couple")

table(couples_city$ReservedRoomType)
table(couples_city$AssignedRoomType)
table(couples_city)
couples_city <- couples_city[which(couples_city$ReservedRoomType %in% c("A", "B", "D", "E", "F")),]
couples_city <- couples_city[which(couples_city$AssignedRoomType %in% c("A", "B", "D", "E", "F")),]

# 
# 
# 
# Q1. What type of rooms are assigned and getting canceled by couples in cities?

IsCanceled_AssignedRoomType_Couples_City <- couples_city %>% group_by(IsCanceled, AssignedRoomType) %>% summarise(count = n())


ggplot(
  data = (IsCanceled_AssignedRoomType_Couples_City) ,
  mapping = aes(x = AssignedRoomType, fill = IsCanceled, y = count)
) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6)  +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_brewer(palette = "Blues") +
  theme_grey() +
  labs(title = "What type of rooms are Assigned to Couples in City hotels?",
       x = "Asssigned Room Type",
       y = "Count",
       fill = "Cancelation Status") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=count), color = "black", size = 3,position=position_dodge(width=0.8), vjust=-0.5)


# So, if AssignedRoomType is A, then cancellation ratio is much higher as compared to other rooms.  


# Q2 Analyzing mean(ADR) with ReservedRoomtype, we see that in cities, if couples tend to reserve room D, E and F, ADR is much higher than other rooms. --- #####



ggplot(data = couples_city, aes(x = ReservedRoomType, y = ADR, fill = ReservedRoomType)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "ADR according to Reserved Room Type for Couples in City hotels",
       x = "Reserved Room type",
       y = "Average Daily Rate (in Euros)") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13)) +
  scale_y_continuous(breaks = seq(0,400,50)) +
  theme(legend.position = "none")


# Association rules mining —— cancellation ~ VisitorType/DepositType/Season

#city hotel

library(arules)
library(arulesViz)

city_data$VisitorType <- as.factor(city_data$VisitorType)
Cencal_Visitor_city <- data.frame(city_data$IsCanceled, city_data$VisitorType)
Cencal_Visitor_city_trans <- as(Cencal_Visitor_city,"transactions")
rules1 <- apriori(Cencal_Visitor_city_trans, 
                  parameter=list(supp=0.005, conf=0.1), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("city_data.IsCanceled=Canceled")))
inspect(rules1)
#The reservation cancellation rate for business travel is only about 50% of the overall cancellation rate. Therefore, 
#city hotel can charge only a low deposit when making reservations for business travel.

Cencal_Deposit_city <- data.frame(city_data$IsCanceled, city_data$DepositType)
Cencal_Deposit_city_trans <- as(Cencal_Deposit_city,"transactions")
rules2 <- apriori(Cencal_Deposit_city_trans, 
                  parameter=list(supp=0.0001, conf=0.1), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("city_data.IsCanceled=Canceled")))
inspect(rules2)
#Non Refund – a deposit was made in the value of the total stay cost
#It's a strange correlation that the cancellation rate is nearly 100% for appointments that require a full deposit with 
#no refund. The cancellation rate for reservation that do not require a deposit is lower than the overall cancellation rate.

Cencal_Season_city <- data.frame(city_data$IsCanceled, city_data$Season)
Cencal_Season_city_trans <- as(Cencal_Season_city,"transactions")
rules3 <- apriori(Cencal_Season_city_trans, 
                  parameter=list(supp=0.0001, conf=0.1), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("city_data.IsCanceled=Canceled")))
inspect(rules3)
#City hotel has lower reservation cancellation rates in spring and winter, so hotels can reduce the deposit percentage
#appropriately in these two seasons. For summer, when the cancellation rate is higher, city hotel should increase the 
#deposit percentage appropriately.

#resort hotel

resort_data$VisitorType <- as.factor(resort_data$VisitorType)
Cencal_Visitor_resort <- data.frame(resort_data$IsCanceled, resort_data$VisitorType)
Cencal_Visitor_resort_trans <- as(Cencal_Visitor_resort,"transactions")
rules4 <- apriori(Cencal_Visitor_resort_trans, 
                  parameter=list(supp=0.005, conf=0.1), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("resort_data.IsCanceled=Canceled")))
inspect(rules4)
#For resort hotel, in addition to business travel, solo travelers also have a low cancellation rate for reservations. 
#Therefore hotel can also charge a lower deposit for solo travelers.

Cencal_Deposit_resort <- data.frame(resort_data$IsCanceled, resort_data$DepositType)
Cencal_Deposit_resort_trans <- as(Cencal_Deposit_resort,"transactions")
rules5 <- apriori(Cencal_Deposit_resort_trans, 
                  parameter=list(supp=0.0001, conf=0.1), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("resort_data.IsCanceled=Canceled")))
inspect(rules5)
#like city hotel.

Cencal_Season_resort <- data.frame(resort_data$IsCanceled, resort_data$Season)
Cencal_Season_resort_trans <- as(Cencal_Season_resort,"transactions")
rules6 <- apriori(Cencal_Season_resort_trans, 
                  parameter=list(supp=0.0001, conf=0.1), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("resort_data.IsCanceled=Canceled")))
inspect(rules6)
#Similar to the City Hotel, the reservation cancellation rate is low in spring and winter. However, the difference is 
#that resort hotel has the highest reservation cancellation rate in the fall, so the deposit percentage should be 
#increased appropriately for resort hotel in the fall.

#######################---------- Basic Plots-----------#######################

#plot piechat for visitor type and Season. Plot barplots for other categorical columns. 


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=13, face="bold"),
    legend.title=element_text(size=13, face="bold")
  )

# Visitor Type   ( This is fine!!)
combined_VisitorType_count <- combined_data %>% group_by(VisitorType) %>% summarise(count = n())

ggplot(data = combined_VisitorType_count, aes(x ="", y = count, fill = VisitorType)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  ggtitle("Percentage of Type of Visitors") +
  geom_text(aes(x = 1.7, label = paste0(round((count/sum(count) * 100),1), "%")), position = position_stack(vjust = 0.5)) 


# Season   ( This is fine!!)
combined_Season_count <- combined_data %>% group_by(Season) %>% summarise(count = n())

ggplot(data = combined_Season_count, aes(x ="", y = count, fill = Season)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  ggtitle("Percentage of Visitors by Season") +
  geom_text(aes(x = 1.7, label = paste0(round((count/sum(count) * 100),1), "%")), position = position_stack(vjust = 0.5)) 



# Market Segment   (Draw a barchart showing % on y-axis, Pi chart not looking good)
combined_MarketSegment_count <- combined_data %>% group_by(MarketSegment) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


ggplot(combined_MarketSegment_count, aes(MarketSegment, pct, fill = MarketSegment)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Percentage wise Market Segment", x = "Market Segment", y = "Percentage")


# Customer type
combined_CustomerType_count <- combined_data %>% group_by(CustomerType) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


ggplot(combined_CustomerType_count, aes(CustomerType, pct, fill = CustomerType)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  labs(title = "Percentage wise Type of Customers", x = "Market Segment", y = "Percentage")


# Deposit Type
combined_DepositType_count <- combined_data %>% group_by(DepositType) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


ggplot(combined_DepositType_count, aes(DepositType, pct, fill = DepositType)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  labs(title = "Percentage wise Type of Deposits", x = "Market Segment", y = "Percentage")

# Distribution Channel
combined_DistributionChannel_count <- combined_data %>% group_by(DistributionChannel) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))


ggplot(combined_DistributionChannel_count, aes(DistributionChannel, pct, fill = DistributionChannel)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  labs(title = "Percentage wise Distribution Channel", x = "Market Segment", y = "Percentage")


# MealType
combined_MealType_count <- combined_data %>% group_by(Meal) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))
combined_MealType_count$Meal <- factor(combined_MealType_count$Meal, levels=c("BB", "FB", "HB", "SC"), labels=c("Bed & Breakfast", "Full Board", "Half-Board","Not Known"))


ggplot(combined_MealType_count, aes(Meal, pct, fill = Meal)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, face = "bold")) +
  labs(title = "Percentage wise Meal Preference", x = "Market Segment", y = "Percentage") 

# Reservation Status
combined_ReservationStatus_count <- combined_data %>% group_by(ReservationStatus) %>% summarise(count = n()) %>%  mutate(pct = count/sum(count))

ggplot(combined_ReservationStatus_count, aes(ReservationStatus, pct, fill = ReservationStatus)) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = 1.1))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "None",
        axis.title.x = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  labs(title = "Percentage wise Reservation Status", x = "Market Segment", y = "Percentage") 


############### -------------- Complex Plots -------------#################################


# Map —— Number Of Reservations From Different Countries
Country_city <- city_data[-which(city_data$Country=="NULL"),]
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

Country_resort <- resort_data[-which(resort_data$Country=="NULL"),]
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
  theme_light() 
mp_resort
