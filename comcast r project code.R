#code Step 1 - Import data into R environmen

library(readxl)
comcast = read.csv(file.choose())
View(comcast)
summary(comcast)
head(comcast)
str(comcast)

#we can see that the date formats are different
#Loading The Date Into Single Format
#Use Lubridate Library to Format the Date Column

library(lubridate)
li<-parse_date_time(x = comcast$Date,orders = c("d m y", "d B Y", "m/d/y"),
                    locale = Sys.getlocale("LC_TIME"))

data2<-comcast
data2$Date <- li

data2$Month <- format(as.Date(data2$Date), "%m")
data2$Month<- month.abb[as.integer(data2$Month)]
head(data2)
View(data2)

#code Step2- Provide the trend chart for the number of complaints at monthly and daily granularity levels
library(dplyr)
library(ggplot2)
data_date<-data2 %>% group_by(Date) %>% dplyr::summarise(frequency = n())
df <-data_date[order(-data_date$frequency),]
dff<-head(df)
dff

ggplot(data_date, aes(Date, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")
# results show max no.of complaints in June to be specific on Jun 24th 
ggplot(dff, aes(Date, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")
  
data_month<-data2 %>% 
  group_by(Month) %>% dplyr :: summarise(frequency = n())
data_month

#June shows max no of complaints 

data2$Month <- as.factor(data2$Month)
levels(data2$Month)

ggplot(data_month, aes(Month, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Month") + 
  ylab("Number of Complaints")

#code Step3- Provide a table with the frequency of complaint types
data3<-data2%>% mutate(Customer.Complaint = tolower(Customer.Complaint))
CustTable <- table(data3$Customer.Complaint)
CustTable <- data.frame(CustTable)
filtered<-CustTable %>% 
  rename(
    CustomerComplaintType = Var1,
    Frequency = Freq
  )
final <- filtered %>% arrange(desc(Frequency))
final_most<-head(final,20)
final_most

#code Step4- Which complaint types are maximum i.e., around internet, network issues, or across any other domains.
ggplot(head(final_most,6), aes(CustomerComplaintType, Frequency)) +
  geom_bar(stat = "identity")

#code Step5-Create a new categorical variable with value as Open and Closed
library(stringr)
library(tidyverse)
levels(comcast$Status)

library(plyr)
comcast$Status_new <- revalue(comcast$Status, c(Pending = 'Open', Solved ='Closed'))
head((comcast))

#code Step6 -Provide state wise status of complaints in a stacked bar chart
tab <- table(comcast$State,comcast$Status_new)
tab <- cbind(tab, Total = rowSums(tab))
head(tab,15)
library(dplyr)
ggplot(comcast, aes(y = State)) + geom_bar(aes(fill = Status_new))
#clearly Georgia has more complaints registered and more open 

ggplot(comcast, aes(y = Received.Via )) + geom_bar(aes(fill = Status_new))
df1 <- table(comcast$Received.Via, comcast$Status_new)
df1 <- cbind(df1, Total = rowSums(df1))
df1

#code Step7- Provide the percentage of complaints resolved & resolved till date
# via Internet
slices <- c(843, 262)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Internet")

#via Customer care calls
slices <- c(864, 255)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Call")








