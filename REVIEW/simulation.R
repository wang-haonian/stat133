# leave this here:
set.seed(123456)

# [5 pts]
# Write a function, [dice_sum()], that takes as input:
# [k] : the number of dice rolled
# [B] : the number of rolls
# and returns:
# [dsum] : a vector of length B where each element is the sum of a roll 
#          k dice 

# So if k=1 pick a number between 1 and 6 at random B times and return them,
# if k=2 then in each roll you pick twice a number between 1 and 6 at random,
# calculate their sum, do this B times and return
# and so on.

dice_sum <- function(k=2, B=100){
  replicate(B, sum(sample(1:6, size=k, replace=T)) )
}

# [5 pts]
# Lets run four simulations:
# Fix k=2 for all simulations
# Use B=20, 100, 1000, 5000 (in this order)

# For each value of B we will:

# Calculate the mean and sd of the output and store in 
# [ave.diceRoll] : a vector of length 4 where each entry is the mean of the simulation output
# [sd.diceRoll] : a vector of length 4 where each entry is the sd of the simulation output

# Then plot four histograms of the output from dice_sum()
# On each histogram the x-axis label should be "sum of dice roll" and the title should be
# "Histogram for B=[correct number]"

# CAREFUL : just run 4 simulations and save the mean and sd each time and make the histogram
# DO NOT : run 4 simulations to get the mean, then another 4 simulations to make the plots.

# NOTE : if for some reason you can not write the function [dice_sum] create four vectors
# of length 20, 100, 1000 and 5000 and complete the tasks below.

#----------------------
# To get all four histograms on one plot we include a command that splits the plotting
# screen into four panels, 2 in each row.  When a plotting function is called the plot
# goes into the first panel (upper left), when you call a plotting function again it goes
# into the second panel (upper right), etc.

# DO NOT DELETE THIS:
par(mfrow=c(2,2))
#----------------------

ave.diceRoll <- rep(0, 4)
sd.diceRoll <- rep(0,4)
Bvec <- c(20, 100, 1000, 5000)

for(i in 1:4){
  tmp <- dice_sum(k=2, B=Bvec[i])
  ave.diceRoll[i] <- mean(tmp)
  sd.diceRoll[i] <- sd(tmp)
  hist(tmp, xlab="sum of dice roll", main=paste("Histogram for B=", Bvec[i]))
}



set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
    rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  sample_y = tapply(y, x, sample, replace=TRUE)
  return (unlist(sample_y))
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  sample_error = sample(err, size = length(err), replace= FALSE)
  #fit[,1] = fit[,1] + sample_error 
  #return (fit[,1])
  y<-fit+sample_error
  return(y)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree == 1){
    lm=lm(y~ x)
    coeff = lm$coefficients
    return(coeff)  
  }
  
  
  else if (degree==2){
    lm = lm (y~x + I(x^2)) 
    coeff = lm$coefficients
    return (coeff)  
  }
}



oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  
  if (is.null(fit)){
    new_y = genBootY(data$x, data$y)  
  }
  else{
    new_y = genBootR(fit = fit[,1], err=fit[,2]) 
  }
  
  return(fitModel(data$x, new_y, degree=degree)) #return the coefficient
}
# resdiual = data - fit
### Use fitModel to fit a model to this bootstrap Y 

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  model_l = lm(data$y~data$x)
  errors_l = model_l$residuals 
  fits_l = model_l$fitted.values
  matrix_l = data.frame(fits_l, errors_l)
  
  model_q = lm (data$y~data$x + I(data$x^2))
  errors_q = model_q$residuals
  fits_q = model_q$fitted.values
  matrix_q=data.frame(fits_q, errors_q)
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  a=replicate(n=B, oneBoot(data=data, degree=1))
  b=replicate(n=B, oneBoot(data=data, degree=2))
  c=replicate(n=B, oneBoot(data=data, fit = matrix_l, degree=1))
  d=replicate(n=B, oneBoot(data=data, fit = matrix_q, degree=2))
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  coeff = list(a,b,c,d)
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data (4 scatter plots, 1 for each set up)
  plot(y~x, main="Visualizing Bootstrap", xlab="x", ylab="y")
  
  if (nrow(coeff) == 2){
    mapply(abline, a=coeff[1,], b=coeff[2,], col=rgb(0,0.9,0,alpha=0.5))
  } 
  
  if(nrow(coeff) == 3){
    mapply(function(a,b,c){curve(a+b*x+c*(x^2), add=TRUE, col=rgb(1,0.2,0.5, alpha=0.5))}, 
           a = coeff[1,], b=coeff[2,], c=coeff[3,])   
  }
  curve(trueCoeff[1]+trueCoeff[2]*x+trueCoeff[3]*(x^2), col="black", add=T, lwd=4)
}

### Add lines or curves for each row in coeff
### Use transparency
### You should use mapply to construct all 
### 1000 of the bootstrapped lines of best fit 
### Have a look at ?mapply for details.
### This can be done in ggplot2 or base graphics.

### Use trueCoeff to add true line/curve - 
###  Make the true line/curve stand out


### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
    bootPlot(myData$x, myData$y, 
             coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}

#mapply(f, c("a", "b", "c"), 1:3)
# f("a", 1), f("b",2), f("c",3)



#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  m <- matrix(sample(c(0,1,2),size=r*c,replace=T,prob=c((1-p),p/2,p/2)),nrow=r,ncol=c)
  return(m)
}


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  
  move.right <- function(m) {
    initial.matrix1 <- m
    for (i in 1:nrow(m)){
      for (j in 1:ncol(m)) {
        if (j > 1)
          if(j==ncol(m)) {
            if(initial.matrix1[i,j]==1 && initial.matrix[i,1]==0)
            {m[i,j]<-0 
             m[i,1]<-1}
          } else{
            if(initial.matrix1[i,j]==1 && initial.matrix[i,j+1]==0)
            {m[i,j]<-0 
             m[i,j+1]<-1}}
      }
    }
    return(m)
  }
  move.up <- function(m) {
    initial.matrix2 <- m
    for (i in 1:nrow(m)){
      for (j in 1:ncol(m)) {
        if(i==1) {if (initial.matrix2[1,j]==2 && initial.matrix2[nrow(m),j]==0)
        {m[1,j]<-0
         m[nrow(m),j]<-2} }
        else{if (initial.matrix2[i,j]==2 && initial.matrix2[i-1,j]==0)
        {m[i,j]<-0
         m[i-1,j]<-2}}
      }}
    return(m)
  }
  initial.matrix <- m
  m <- move.up(move.right(m))
  grid.new <- !identical (initial.matrix,m)
  return(list(m,grid.new))
  #return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)


bml.sim <- function(r, c, p){
  m <- bml.init(r,c,p)
  for (i in 1:1200) { 
    m <- bml.step(m)[[1]]
    number.steps <- i
    if (bml.step(m)[[2]]==F) break
  }
  return(number.steps)}
bml.sim1 <- function(r, c, p){
  for (i in 1:1200) { 
    m <- bml.step(m)[[1]]
    number.steps <- i
    if (bml.step(m)[[2]]==F) break 
  }
  return(m)}

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
  
  # Set up the output variable has.adopted.
  # We define it as a matrix with all 0 then use initial.doctors
  # to set the first column (first day)
  has.adopted <- matrix(0, nrow=n.doctors, ncol=n.days)
  has.adopted[,1] <- initial.doctors
  
  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p
  
  for(i in 2:n.days){
    # initialize the day to be the same as yesterday:
    has.adopted[,i] <- has.adopted[,i-1]
    # pick two random doctors
    two.doctors <- sample(1:n.doctors, size=2, replace=FALSE)
    
    # check if one has adopted and the other one hasn't
    # i.e. whether one is 0 and one is 1, or equivalently, their sum is 1
    # we can use either condition in the if-statment:
    
    # OPTION1: has.adopted[two.doctors[1], i] != has.adopted[two.doctors[2], i]
    # OPTION2: sum(has.adopted[two.doctors, i]) == 1
    
    
    if(sum(has.adopted[two.doctors, i]) == 1){
      # with probability p we change the non-adopted to an adopter
      # we can generate a Bernoullie 0/1 varilable or use a Uniform(0,1) variable:
      
      # ONE OPTION: if(rbinom(n=1, size=1, prob=p) == 1){
      if(runif(1) < p){
        # set both of the doctors to 1 (saves us checking who was 0 and who was 1)
        has.adopted[two.doctors, i] <- 1
      }
    }
  }
  
  # return the output
  return(has.adopted)
}