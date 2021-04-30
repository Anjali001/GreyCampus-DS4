###### Part 1 ######

country <- c('France','Croatia')
wins <- c(4,2)
for (i in 1:2){
  print(sprintf("Team %s wins %d",country[i],wins[i]))
}

###### Part 2 ######
head(mtcars)
cars<-mtcars

for (i in 8:11){
  cars[,i]<- as.factor(cars[,i])
}
str(cars[8:11])

########Part 3

data <- read.csv("C:/Users/Lenovo PC/Downloads/cat.csv")
library(dplyr)
Percent_Col_NA <- function(x){
  num<-nrow(x)
  percent<-(colSums(is.na(x))/num)*100
  return(percent)
}
data<-data %>% na_if("#N/A")
Percent_Col_NA(data)

########Part 4

data <- read.csv("C:/Users/Lenovo PC/Downloads/cat.csv")
library(dplyr)
Percent_Row_NA <- function(x){
  num<-ncol(x)
  percent<-(rowSums(is.na(x))/num)*100
  return(percent)
}
data<-data %>% na_if("#N/A")
Percent_Row_NA(data)


############ Converting to numeric THC/CO/CO2 etc
for (i in c(23:27,29:31)){
  data[,i]<- as.numeric(data[,i])
}
############
########Part 5
name<-colnames(data)
numeric_summary <- function(x){
  for (i in 1:ncol(x)){
    if (is.numeric(x[,i])==TRUE){
      print(name[i])
      print(summary(x[,i]))
      print(boxplot(x[,i],main = name[i]))
    }
  }
}
numeric_summary(data)

########Part 6
library(ggplot2)
library(dplyr)
generate_histogram<- function(x){
  data1 <- x %>% select_if(is.numeric)
  for (i in 1:ncol(data1)){
    print(ggplot(data.frame(data1[,i]),aes(data1[,i]))+geom_histogram(stat="bin")+xlab(name[i]))
  }
}
generate_histogram(data)


########Part 7
library(anytime)
data7to10 <- read.csv("C:/Users/Lenovo PC/Downloads/cat1.csv",stringsAsFactors=FALSE)
data7to10$First.FD.Date <- format(anytime::anydate(data7to10$First.FD.Date),"%d/%m/%Y")
data7to10$Last.FD.Date <- format(anytime::anydate(data7to10$Last.FD.Date),"%d/%m/%Y")
data7to10$FD.termination.date <- format(anytime::anydate(data7to10$FD.termination.date),"%d/%m/%Y")
head(data7to10)
########Part 8
data7to10$Date.of.Birth <- format(as.Date(data7to10$Date.of.Birth,"%d-%b-%y"),"%d/%m/%Y")
# sum=0
# for (i in data7to10$Date.of.Birth){
#   if (i==" "){
#     sum=sum+1
#   }
# }
# print(sum)
sum(is.na(data7to10$Date.of.Birth)) # 39 rows are missing in original data too
head(data7to10)
########Part 9
#There are 4 dates i.e. in column 2,3,5,8
data7to10$First.FD.Date <- as.Date(data7to10$First.FD.Date ,"%d/%m/%Y")
data7to10$Last.FD.Date <- as.Date(data7to10$Last.FD.Date,"%d/%m/%Y")
data7to10$FD.termination.date <- as.Date(data7to10$FD.termination.date,"%d/%m/%Y")
data7to10$Date.of.Birth <- as.Date(data7to10$Date.of.Birth,"%d/%m/%Y")

for (i in c(2,3,5,8)){
  print(class(data7to10[,i]))
}

########Part 10
#DoB can't be beyond 2021 as of now
library(lubridate)
for (i in 1:831){
  if (is.na(year(data7to10$Date.of.Birth[i]))==FALSE  && year(data7to10$Date.of.Birth[i]) > 2021){
    year(data7to10$Date.of.Birth[i]) = year(data7to10$Date.of.Birth[i])-100
  }
}
data7to10$Age <- as.double(difftime(data7to10$First.FD.Date,data7to10$Date.of.Birth,units="weeks")/52)
head(data7to10,20)
