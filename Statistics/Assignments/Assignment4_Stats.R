sample_size <- 77882
#Assuming sample is perfect representative of population, we can say that 36.2% people support statement in sample too.
#Let S_A represent people who support statement(agree) and S_R represent those who oppose (refuse) the statement.
S_A <- round(0.362*sample_size)
S_R <- sample_size - S_A #Assuming people either accept the statement or refuse it.No one can be who didn't have a opinion.
#Let U represent people who went to university or have higher degree and No_U represent represent those who do not went to university or have higher degree
U <- round(0.138*sample_size)
No_U <- sample_size - U #Because people either went to university or not.Disjoint Case
#Let combination of S_A and U be AU, S_R and U be RU , S_A and NU be ANU , S_R and NU be RNU
AU <- round(0.036*sample_size)
RU <- U - AU
ANU <- S_A - AU
RNU <- S_R - RU
# Let's make a 2*2 matrix for better view
final_matrix <- matrix(data=c(AU,RU,U,ANU,RNU,No_U,S_A,S_R,sample_size),nrow=3,ncol=3, byrow=TRUE,dimnames=list(c("University+","No University","Total"),c("Stat_Agree","Stat_Refuse","Total")))
final_matrix
#####################################################################################################
print(final_matrix[1,1]/final_matrix[3,3])
#####################################################################################################
library(grid)
library(futile.logger)
library(VennDiagram)
venn.plot <- draw.pairwise.venn(
  area1 = 0.362,
  area2 = 0.138,
  cross.area = 0.036,
  category = c("C_1", "C_2"),
);
grid.draw(venn.plot);
grid.newpage();
######################################################################################################
prob <- (final_matrix[1,3]+final_matrix[3,1]-final_matrix[1,1])/final_matrix[3,3]
print(prob)
probNR <- (final_matrix[2,2]/final_matrix[3,3])*100
print(probNR)
######################################################################################################
#For independent P(A)*P(B) = P(A and B)
P_A <- final_matrix[3,1]/final_matrix[3,3]
P_B <- final_matrix[1,3]/final_matrix[3,3]
P_AB <- final_matrix[1,1]/final_matrix[3,3]
ifelse(P_A*P_B == P_AB,"Independent","They are not independent.")
######################################################################################################
Rprob <- 1-((final_matrix[3,2]/final_matrix[3,3])^5)
Rprob
######################################################################################################
Infected_P <- 25.9/100
Not_Infected_P <- 1-Infected_P
Infected_Positive_P <- 99.7/100
Infected_Negative_P <- 1-Infected_Positive_P
Not_Infected_Negative_P <- 92.6/100
Not_Infected_Positive_P <- 1-Not_Infected_Negative_P
######################################################################################################
print(Infected_Positive_P/(Infected_Positive_P+Not_Infected_Positive_P))
######################################################################################################
library(openintro)
treeDiag(main = c("Infected with HIV?","Tested Positive or Negative?"),p1 =c(Infected_P,Not_Infected_P),out1 = c("Yes", "No"),out2 = c("Test Positive", "Test Negative"),p2=list(c(Infected_Positive_P,Infected_Negative_P),c(Not_Infected_Positive_P,Not_Infected_Negative_P)))
print(0.2582/(0.2582+0.0548))
######################################################################################################
dbinom(x=8,size=10,prob=0.13)
######################################################################################################
power_f_prob<-0.25
tot_n <- 245
#We have to find distribution of friends
#if n is > 30 , it follows normal distribution
# We have to know average no. of power friends for mean and SD
#distribution of friends is binomial as one can be or cannot be a power user. So only 2 outcomes.
mean_f<- tot_n*power_f_prob
sd_f <- mean_f*(1-power_f_prob)
#P(Z>= 70-mean/s.d.)
z=(70-mean_f)/sd_f
prob2<- 1-pnorm(z,0,1) #Standard normal variate, P(Z<z) is pnorm
prob2
pbinom(q=70,size=245,prob=0.25,lower.tail=FALSE) + dbinom(70,245,0.25)



venn.plot <- draw.pairwise.venn(
  area1 = 0.362,
  area2 = 0.138,
  cross.area=0.036,
  category = c("Agree to Statement","University+"),
  euler.d =TRUE,
  scaled=TRUE,
  fill = c("red","green"),
  cex = 2,
  cat.cex = 2,
  cat.pos = c(285, 105),
  cat.dist = 0.09,
  cat.just = list(c(-1, -1), c(1, 1))
);
grid.draw(venn.plot);
grid.newpage();