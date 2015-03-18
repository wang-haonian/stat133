# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  
  m<- matrix(rep(initial.doctors, times=n.days), ncol= n.days)
for (i in 2:n.days){
 pair = sample(1: n.doctors, size=2)
 if(m[pair[1], i] != m[pair[2],i]){
   m.i = m[pair,i]
   m.i[m.i == 0] = sample(0:1, size = 1, prob = c(1-p, p))
   m[pair,i:n.days] = m.i
 } 
 }
return(m)
}
 

 # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output



# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
n.doctors<- 100
initial.doctors = sample(0:1,size = n.doctors, prob = c(0.9,0.1), replace = T)

m1=sim.doctors(initial.doctors,n.days = 500, n.doctors = n.doctors, p=0.5)
m2=sim.doctors(initial.doctors,n.days = 500, n.doctors = n.doctors, p=0.6)
m3=sim.doctors(initial.doctors,n.days = 500, n.doctors = n.doctors, p=0.7)
m4=sim.doctors(initial.doctors,n.days = 500, n.doctors = n.doctors, p=0.8)
m5=sim.doctors(initial.doctors,n.days = 500, n.doctors = n.doctors, p=0.9)



plot(x=c(1:500),y=colSums(m1),xlab="days", ylab="Number of doctors",type = 'l',col="red")
lines(x=c(1:500), y=colSums(m2),col="black")
lines(x=c(1:500),y=colSums(m3), col="blue")
lines(x=c(1:500),y=colSums(m4), col="grey")
lines(x=c(1:500),y=colSums(m5), col="yellow")
legend("bottomright", legend=c("p=0.5","p=0.6","p=0.7","p=0.8","p=0.9"), 
        fill=c("red", "black","blue","grey","yellow"))

# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

