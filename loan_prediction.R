
#Loading Libraries
library(dplyr)
library(stringr)
library(ggplot2)

#Loading dataset
loan_db = read.csv("train.csv", na.strings=c("","NA"))

#Quick description of Dataset
summary(loan_db)
str(loan_db)
colnames(loan_db)
nrow(loan_db)

sum(is.na(loan_db))         #7661 NAs
colSums(is.na(loan_db))     #7661 NAs in Employment type
loan_db = na.omit(loan_db)  #NAs removed

head(loan_db)

sum(duplicated(loan_db$UniqueID)) #No Duplicates in Primary Key

#Checking for Outliers

plot(quantile(loan_db$disbursed_amount,seq(0,1,0.01)))
loan_db$disbursed_amount[which(loan_db$disbursed_amount>85120.00)]=85120.00

plot(quantile(loan_db$asset_cost,seq(0,1,0.01)))
loan_db$asset_cost[which(loan_db$asset_cost>155916.00)]=155916.00
loan_db$asset_cost[which(loan_db$asset_cost>118079.72)]=118079.72

plot(quantile(loan_db$ltv,seq(0,1,0.01)))
loan_db$ltv[which(loan_db$ltv<39.3500)]=39.3500

summary(loan_db$branch_id)
length(unique(loan_db$branch_id))           #82 Unique Branch IDs
class(loan_db$branch_id)

length(unique(loan_db$supplier_id))         #2945 Unique Supplier IDs

summary(loan_db$manufacturer_id)
length(unique(loan_db$manufacturer_id))     #11 Unique Manufacturer IDs

summary(loan_db$Current_pincode_ID)
length(unique(loan_db$Current_pincode_ID))  #6659 Unique PIN codes



#Converting Date columns to Date format

summary(loan_db$Date.of.Birth)
loan_db$Date.of.Birth = as.Date(loan_db$Date.of.Birth, format = "%d-%m-%Y")
summary(loan_db$DisbursalDate)
loan_db$DisbursalDate = as.Date(loan_db$DisbursalDate,format = "%d-%m-%Y")



#Converting columns to factors
summary(loan_db$Employment.Type)
unique(loan_db$Employment.Type)
table(loan_db$Employment.Type)
levels(loan_db$Employment.Type) = c(1,0)  #Salaried = 1, Self Employed = 0
levels(loan_db$Employment.Type) = as.numeric(levels(loan_db$Employment.Type))[levels(loan_db$Employment.Type)]



unique(loan_db$branch_id)
head(loan_db$loan_default)
unique(loan_db$Employment.Type)
class(loan_db$Date.of.Birth)
loan_db$Date.of.Birth = as.Date(loan_db$Date.of.Birth, format = "%d-%m-%Y")
loan_db$DisbursalDate = as.Date(loan_db$DisbursalDate, format = "%d-%m-%Y")
levels(loan_db$Employment.Type)
unique(loan_db$Employment.Type)
loan_db=loan_db[!loan_db$Employment.Type=="",]
colnames(loan_db)

summary(loan_db$disbursed_amount)
summary(loan_db$asset_cost)
summary(loan_db$ltv)


class(loan_db$Date.of.Birth)
summary(loan_db$Date.of.Birth)
loan_db$Date.of.Birth = strptime(loan_db$Date.of.Birth, "%d/%m/%Y")
install.packages("anytime")
library(anytime)
loan_db$Date.of.Birth=anydate(loan_db$Date.of.Birth)

install.packages("lubridate")
library(lubridate)
mdy <- dmy(paste("01-",loan_db$Date.of.Birth, sep= ""))
loan_db$Date.of.Birth <- mdy

loan_db$Date.of.Birth = as.POSIXlt(loan_db$Date.of.Birth, format = "%d/%m/%Y") 
format(loan_db$Date.of.Birth)
