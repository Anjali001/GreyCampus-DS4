############################## Assignment 1 ############################

################################ Part 1 ################################
# Vector Vs Matrix

# Difference: A vector is a one-dimensional data structure while matrix is a two-dimensional data structure.

# Similarity: Both can store data of any single datatype (Homogenous)

################################ Part 2 ################################

# DataFrame Vs Matrix

#Difference: A data frame can have multiple data types but a matrix can store data of only one type.

#Similarity: Both are two-dimensional

################################ Part 3 ################################

y<-c(15,TRUE,"World") #Defining the variable
class(y) #Checking its class
print("Is it a vector?")
is.vector(y) # Checking if its a vector
y

#A vector is created but its class is "Character". 
#So, if we call it, everything comes in double quotes, signifying that all 3 are treated as Characters by R.

################################ Part 4 ################################

scores<-c(95,91,88)
names(scores)<-c("Statistics","Linear Algebra","Calculus")
print(scores)

################################ Part 5 ################################

typeof(scores)
typeof(names(scores))
#Type of scores is "double" and type of names(scores) is Character.

################################ Part 6 ################################

students<- c("Alice","Alicia","Haley")
subjects<-names(scores)
marks<-c(95,91,88,96,94,97,88,98,85)
#subjects <- c("Statistics","Linear Algebra","Calculus")
stud_marks_matrix<- matrix(data=marks,nrow=3,ncol=3,byrow=TRUE,
                           dimnames=(list(students,subjects)))
stud_marks_matrix

################################ Part 7 ################################

matrix_to_df <- data.frame(stud_marks_matrix)
matrix_to_df
print("What is its type:")
class(matrix_to_df)

################################ Part 8 ################################

country<-c("USA","India","Brazil","France","Russia")
tot_cases<-c("32505534","15609004","13977713","5296222","4718854")
tot_deaths<-c("581991","182570","375049","101180","106307")
covid_df<-data.frame(country,tot_cases,tot_deaths)
covid_df

################################ Part 9 ################################

data("mtcars")
str(mtcars)
cars<-mtcars
cars$cyl<-factor(cars$cyl)
cars[,8:11]<-lapply(cars[,8:11],as.factor)
str(cars)

########################################################################

