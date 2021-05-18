###################################################################
covid_data <- read.csv('C:/Users/Lenovo PC/Downloads/COVID19.csv',stringsAsFactors = FALSE, na.strings = c("","NA"))
str(covid_data)
covid_data$Country.Other
#First 8 and Last 8 entries are either empty, continents, world or Total
covid_data <- covid_data[9:229,]
unique(covid_data$Country.Other)
###################################################################

covid_data<-data.frame(lapply(covid_data, gsub, pattern = ",|\\+", replacement = ""))
covid_data<-data.frame(lapply(covid_data, gsub, pattern = "N/A", replacement = NA))
colSums(is.na(covid_data)/nrow(covid_data))*100
#Only 7 is above 5 
covid_data <- covid_data[,c(-2,-7)]
###################################################################
colnames(covid_data)[colnames(covid_data) %in% 
                       c("Country.Other", "TotalCases","NewCases","TotalDeaths","TotalRecovered","NewRecovered","ActiveCases","Serious.Critical","Tot.Cases.1M.pop", "Deaths.1M.pop","TotalTests", "Tests.1M.pop", "Population", "Continent","X1.Caseevery.X.ppl", "X1.Deathevery.X.ppl", "X1.Testevery.X.ppl")]<- c("Country", "Total_Cases", "New_Cases","Total_Deaths","Total_Recovered","New_Recovered","Active_Cases","Critical_Cases","Total_Cases_pm", "Deaths_pm","Total_Tests", "Tests_pm", "Population", "Continent","Case_Every_X_ppl", "Death_Every_X_ppl", "Test_Every_X_ppl")
colnames(covid_data)
###################################################################
# Only Country and Continent are factors
str(covid_data)
library(dplyr)
covid_data <- covid_data %>%mutate_at(c(1,3:14,16:18), as.numeric,na.rm = TRUE)
str(covid_data)

##########CLEANED########################
hist(covid_data$Total_Cases)
library(ggplot2)
ggplot(covid_data, aes(x =Total_Cases))+geom_histogram(aes(y= ..density..),colour = "black",fill = "white",binwidth = 5)+stat_function(fun= dnorm,colour = "red",args = list(mean = mean(covid_data$Total_Cases), sd = sd(covid_data$Total_Cases)))
ggplot(covid_data, aes(x =Total_Deaths))+geom_histogram(aes(y= ..density..),colour = "black",fill = "white",binwidth = 5)+stat_function(fun= dnorm,colour = "red",args = list(mean = mean(covid_data$Total_Deaths), sd = sd(covid_data$Total_Deaths)))
ggplot(covid_data, aes(x =Total_Recovered))+geom_histogram(aes(y= ..density..),colour = "black",fill = "white",binwidth = 5)+stat_function(fun= dnorm,colour = "red",args = list(mean = mean(covid_data$Total_Recovered,na.rm = TRUE), sd = sd(covid_data$Total_Recovered,na.rm = TRUE)))

###########################################
plot(covid_data$Total_Cases,covid_data$Population)
cor(covid_data$Total_Cases,covid_data$Population,method="pearson")
############################################
plot(covid_data$Total_Cases_pm,covid_data$Population)
cor(covid_data$Total_Cases_pm,covid_data$Population,method="pearson")
############################################
#Tot Cases per million as if population is less, cases are less. But it doesn't mean that it's doing better than other nations with more population.
############################################
plot(covid_data$Total_Cases,covid_data$Total_Deaths)
cor(covid_data$Total_Cases,covid_data$Total_Deaths,method="pearson")
############################################
plot(covid_data$Total_Cases,covid_data$Deaths_pm)
cor(covid_data$Total_Cases,covid_data$Deaths_pm,method="pearson")
#Deaths PM , better comparison.
############################################
levels(covid_data$Continent)
boxplot(Total_Cases_pm ~ Continent, data = covid_data,na.exclude(covid_data$Continent),main = "Total Cases/1M pop by continent",col = "grey", frame = FALSE)
#Europe has highest total number of cases per million population on average
#Australia/Oceania has the least on average
############################################
boxplot(Deaths_pm ~ Continent, data = covid_data,na.exclude(covid_data$Continent),main = "Total deaths/1M pop by continent",col = "grey", frame = FALSE)
#Europe has highest death cases per million population on average
#Australia/Oceania has the least on average
############################################
covid_data$Country[which.max(covid_data$Tests_pm)]
#ARUBA is the best
############################################
covid_data$Country[which.min(covid_data$Tests_pm)]
#ALGERIA IS WORST TESTING
###############################################
ggplot(data=covid_data, aes(x=Continent, y=Tests_pm),frame=TRUE) +
  geom_bar(stat="identity", fill="pink")
############################################
shapiro.test(covid_data$Tests_pm)
#Pvalue is 1.284e-07 <0.05, Ho may be rejected. It is not normally distributed.
qqnorm(covid_data$Tests_pm)
qqline(covid_data$Tests_pm)
#Data is clearly skewed.