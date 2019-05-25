#### Data Preparation ####

library(lubridate)
library(zoo)
library(imputeTS)
library(DataExplorer)
library(data.table)

# Reading the data
data<- read.csv("data for cleaning.csv")
head(data)
# check dimensions of the loaded dataset
dim(data)
# check structure of the dataset
str(data)
# removing feilds not required for the analysis
data$TempSent<- NULL
data$ConversationRCD<- NULL

#Convert dates to date format
data$Enquiry.Date<- as.character(data$Enquiry.Date)
data$Enquiry.Date<-mdy(data$Enquiry.Date)
data$Dep.Date<- as.character(data$Dep.Date)
data$Dep.Date<-mdy(data$Dep.Date)

# Apply feature engineering to create various date related columns which might give us great insights into data analysis
data$EnquiryYear<-factor(year(data$Enquiry.Date))
data$EnquiryMonth<-factor(month(data$Enquiry.Date))
data$EnquiryDay<-day(data$Enquiry.Date)
data$EnquiryWeekday<-factor(weekdays(data$Enquiry.Date))
data$DepYear<-factor(year(data$Dep.Date))
data$DepMonth<-factor(month(data$Dep.Date))
data$DepDay<-day(data$Dep.Date)
data$DepWeekday<-factor(weekdays(data$Dep.Date))

# Change enquiry.time to various time related columns to give us better insights
data$Enquiry.Time <- as.numeric(gsub("\\:.*$", "", data$Enquiry.Time))
data$Enquiry.Timecat<-ifelse(data$Enquiry.Time>=9 & data$Enquiry.Time<=21,"Business_Hour","Closed")
data$Enquiry.Timecat<-factor(data$Enquiry.Timecat)

data$Enquiry.Time_class <- with(data,  ifelse(Enquiry.Time >= 6 & Enquiry.Time<=12, "morning",
                                              ifelse(Enquiry.Time>12 & Enquiry.Time<=18, "afternoon", "night")))
data$Enquiry.Time<- NULL
data$Enquiry.Time_class<-factor(data$Enquiry.Time_class)

# change Dep.Date to seasons to give us better insights
yq <- as.yearqtr(as.yearmon(data$Dep.Date, "%m/%d/%Y") + 1/12)
data$DepartureSeason <- factor(format(yq, "%q"), levels = 1:4, 
                               labels = c("winter", "spring", "summer", "fall"))

# Removing Enquiry.Date and Dep.Date
data$Enquiry.Date<-NULL
data$Dep.Date<-NULL

# Understand the data further
summary(data)

# The variable Answered.by.specialist only recored 'Yes' when answered and left black when not answered. This should be converted to 1 for 'Yes' and 0 when not answered
data$Hotkey<-ifelse(data$Answered.by.specialist %in% 'Yes',"1","0")
data$Hotkey<-factor(data$Answered.by.specialist)

# Checking missing values
colSums(is.na(data))
# Replace missing values of duration with the mean value
data$Duration<-na.mean(data$Duration,option="mean")

# From the summary analysis carried out earlier, it is understood that there are some errors in the data. To remove these errors, a function is created.
outlierReplace = function(dataframe, cols, rows, newValue = NA)
{
  if (any(rows)) 
  {
    set(dataframe, rows, cols, newValue)
  }
}
# Variable lead.time should not have any negative values (OutlierReplace function is used to eliminate the errors)
outlierReplace(data, "Lead.Time",which(data$Lead.Time<0), NA)

# The variable infants has a maximum value of 255, this is likely to be an error based on the mean and median.
# To verify the error a scatter plot is used to get a better understanding
plot(data$Infants)
# Based on the plot, the row with 255 infants was eliminated as this could be a data entry error
outlierReplace(data,"Infants",which(data$Infants>5),NA)

# to remove all the 'NA' values
data<-na.omit(data)

# to avoid redundant levels in a categorical variable and to deal with rare levels, we can simply combine the different levels

# combine levels based on frequency destribution (combine levels having frequency of less than 5%)
plot(data$Destination)
data<-group_category(data=data, feature = "Destination", threshold=0.05, update=TRUE)
data$Destination<- factor(data$Destination)
plot_bar(data$Holiday.Type)
data<-group_category(data=data,feature = "Holiday.Type",threshold=0.05,update=TRUE)
data$Holiday.Type<-factor(data$Holiday.Type)
plot_bar(data$Dep.Airport)
data<-group_category(data=data, feature = "Dep.Airport", threshold=0.05,update=TRUE)
data$Dep.Airport<-factor(data$Dep.Airport)

# combine levels based on business logic

# From the plot of Transport.Type it is identified that some of the points are unlabeled, we can treat the unlabelled points as 'None Required'
# Combining unlabed points with the the 'None required' level
plot_bar(data$Transport.Type)
data$Transport.Type <- with(data,  ifelse(Transport.Type %in% "A","A",
                                          ifelse(Transport.Type %in% "B", "B", "None Required")))
data$Transport.Type<- factor(data$Transport.Type)

# Rare levels and unlabbeled points in Accom.type was grouped into 'None'
plot_bar(data$Accom.type)
data$Accom.type <- with(data,  ifelse(Accom.type %in% "grade2","grade2",
                                      ifelse(Accom.type %in% "grade1", "grade1", 
                                             ifelse(Accom.type %in% "grade3", "grade3","None"))))

data$Accom.type<- factor(data$Accom.type)

# It could be better to analyse based on gender than based on Title, converting title to M for male and F for female
data$Gender<- with(data, ifelse(Title %in% "Dr","M",
                                ifelse(Title %in% "Mr","M",
                                       ifelse(Title %in% "Ms","F",
                                              ifelse(Title %in% "Mrs","F","F")))))

data$Gender<-factor(data$Gender)


# The variable Booked.Status is the explantory variable (target variable) and it would be ideal to convert it into '1' and '0' before modelling
data$Booked.Status<-with(data,ifelse(Booked.Status %in% "YES","1","0"))
data$Booked.Status<-factor(data$Booked.Status)

# Final check 
str(data)
# Minor changes (convert Duration to an integer)
data$Duration<-as.integer(data$Duration)
# Save the cleaned data as a csv
write.csv(data,file='ReadyforModelling.csv')
