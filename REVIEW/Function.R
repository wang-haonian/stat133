# Write a function that takes as input a vector x and returns a standardized vector
## (i.e. x after subtracting the mean and dividing with the sd)
standardize <- function(x){
  
  return((x-mean(x, na.rm=T))/sd(x, na.rm=T))
}
# Please implement the function sqftByPrice. Your function should take the
# following arguments:
#
# <price.cutoff>: a numeric constant indicating the price cutoff used for houses
# <prices>      : a numeric vector of housing prices
# <lsqft>       : a numeric vector giving lsqft for each of the observations in
#               <prices> (i.e. this vector will need to be the same length as prices)
#
# Your function should return the average of <lsqft> for all houses with <price>
# stricly greater than <price.cutoff>.
sqftByPrice <- function(price.cutoff, prices, lsqft) {
  
  # your code here
  if(length(prices) != length(lsqft)) stop("prices and lsqft should be the same length")
  
  #subset.idcs <- prices > price.cutoff
  #subset.avg <- mean(lsqft[subset.idcs], na.rm=T)
  #return(subset.avg)
  
  # or
  mean( lsqft[ prices > price.cutoff ] , na.rm=T )
}

# Please implement the function bmiByheight. Your function should take the
# following arguments:
#
# <height.range>: a numeric vector of length 2 whose first and second elements
#   give the minimum height and maximum height to consider
# <height>: a numeric vector giving the height of each person
# <bmi>: a numeric vector giving the bmi of each person associated with <height>
#   (this should be the same length as <height>)
#
# Your function should return the average of <bmi> for all observations with
# <height> in the range (inclusive) specified by <height.range>

bmiByheight <- function(height.range, height, bmi) {
  
  # your code here
  height.range <- sort(height.range) # in case the range was given in [max, min]
  if(height.range[1] > max(height) | height.range[2] < min(height)) stop("check height range")
  mean(bmi[height >= height.range[1] & height <= height.range[2]])
  
}


# Implement the function "truncate", a function that trims a given vector by
# removing the upper and lower specified quantiles. Your function should take
# the following arguments:
#
# <input.vector>: the numeric vector to be truncated
# <trim>: a numeric value between 0 and 0.5 that specifies the upper and lower
# quantiles of <input.vector> that should be removed.
#
# Your function should return the following value:
#
# <truncated.vector>: the remaining values of input.vector after removing the
# upper and lower quantiles

truncate <- function(input.vector, trim) {
  
  
  stopifnot(0<=trim & trim<=0.5)
  
  #your code here
  quant.cutoff <- quantile(input.vector, c(trim, 1-trim))
  truncated.vector <- input.vector[input.vector>=min(quant.cutoff) &                                            
                                     input.vector<=max(quant.cutoff)]
  return(truncated.vector)
  
}

tryCatch(checkEquals(4:7, truncate(1:10, trim=0.25)), error=function(err)
  errMsg(err))

tryCatch(checkIdentical(integer(0), truncate(1:10, trim=0.5)),
         error=function(err) errMsg(err))


# Suppose that you are given some dataset where all variables are
# numeric. Further, assume that you consider a given variable for some
# observation to be an outlier if it is more than 1.5 IQRs from that variable's
# median value. Implement the function "outlierCutoff" that determines the
# min and max value that is not considered an outlier for each variable. your
# function should take the following arguments:
#
# <data>: a data frame consisting of only numeric variables
#
# Your function should return the following:
#
# <outlier.cutoffs>: a 2xnumber.variables matrix giving the lower and upper
# bound for non-outlier values. The first row should be the lower bound and the
# second the upper bound

outlierCutoff <- function(data) {
  
  # your code here
  
  outlier.cutoffs <- sapply(data, function(var) {
    var.iqr <- IQR(var)
    cutoff.min <- median(var) - 1.5 * var.iqr
    cutoff.max <- median(var) + 1.5 * var.iqr
    return(c(cutoff.min, cutoff.max))
  })
  
  return(outlier.cutoffs)
}

tryCatch(checkIdentical(outlier.cutoff.t, outlierCutoff(ex1.test)),
         error=function(err) errMsg(err))


# Again, suppose that you are given some dataset where all variables are numeric
# Further, assume that you are interested in removing outliers as defined in the
# previous part
# Implement a function "removeOutliers" that
# 1) caclulates the number of variables for each observation in the dataset that
# are considered outliers
# 2) removes any observation with more than some specified fraction of its
# variables as outliers. Your function should take the following arguments:
#
# <data>: a data frame where each variable is numeric
# <max.outlier.rate>: a numeric between 0 and 1 specifying the maximum allowable
# fraction of outliers (#outlier.variables / #variables)
#
# Your function should return the follwing:
#

# <subset.data>: a data frame with numeric variables where observations with
# unacceptably high rates of outliers (i.e. greater than <max.outliers>) have
# been removed.

removeOutliers <- function(data, max.outlier.rate) {
  
  stopifnot(max.outlier.rate>=0 & max.outlier.rate<=1)
  
  # your code here
  
  outlier.cutoffs <- outlierCutoff(data)
  high.outliers <- apply(data, 1, function(row) row > outlier.cutoffs[2,])
  low.outliers <- apply(data, 1, function(row) row < outlier.cutoffs[1,])
  outliers <- t(high.outliers + low.outliers)
  num.outliers <- rowSums(outliers)
  removed.idcs <- num.outliers/ncol(data) > max.outlier.rate
  return(data[!removed.idcs, ])
}


tryCatch(checkIdentical(remove.outlier.t, removeOutliers(ex1.test, 0.25)),
         error=function(err) errMsg(err))



# Implement the function "simpleNormSim". This function should generate several
# simulations of samples from a specified normal distribution. The number of
# variables sampled should be constant across simulations. The variables in
# each simulation should have equal variance but may have different means. Your
# function should take the following arguments:
#
# <sim.size>: a numeric constant indicating the number of samples in each
#   simulations.
# <means>: a numeric vector indicating the mean for each each simulation.
# <var>: a numeric constant indicating the variance of the random
#   variables in each simulation.
#
# Your function should return:
#
# <simulation>: a **list** that has the same length as <means>. The jth entry of
#   this list should be a sample of <sim.size> random normal variables with
#   variance given by <var> and mean given by the jth entry of <means>.

simpleNormSim <- function(means, sim.size=50, var=1) {
  
  # your code here
  simulation <- lapply(means, rnorm, n=sim.size, sd=sqrt(var))
  return(simulation)
}

set.seed(47)
tryCatch(checkIdentical(simple.norm.sim.t, simpleNormSim(c(25, 50, 75))),
         error=function(err) errMsg(err))


# Suppose you are given a data frame where all but one of the variables are
# numeric. The final variable (though not necessarily final in position) is a
# factor associated with different levels of your observations. Implement the
# function "meanByLevel" that returns the mean value for each of the numeric
# variables by the levels given from the factor variable. Your function should
# take the following arguments:
#
# <data>: a data frame where all but one of the variables are numeric. The final
#   variable is a factor giving the different levels of the observations. **The
#   factor variable is not necessarily the final variable in position.**
#
# Your function should return:
#
# <level.means>: the means of each of the variables broken down by each of the
#   levels (this should be a num.factors x num.numeric.variables matrix).

meanByLevel <- function(data) {
  
  # your code here
  factor.variable <- sapply(data, function(var) class(var)=='factor')
  level.means <- sapply(data[, !factor.variable], function(var)
    by(var, data[,factor.variable], mean))
  return(level.means)
}

tryCatch(checkIdentical(mean.by.level.t, meanByLevel(iris)), error=function(err)
  errMsg(err))


# Suppose you are given a data frame with the same structure as in the previous
# part of the question. You are interested in identifying the difference between
# the overall average for a given variable and the factor level average for that
# variable. You want this difference to be standardized by the overall standard
# deviation for that variable. Implement the function "stdLevelDiff" that does
# this for each of the numeric variables in your data frame. Your function
# should take the following arguments:
#
# <data>: a data frame where all but one of the variables are numeric. The final
#   variable is a factor giving the different levels of the observations. **The
#   factor variable is not necessarily the final variable in position**
#
# Your function should return:
#
# <level.diff> the difference between mean by factor level and overal
#   mean for each variable divided by the overall standard deviation for each
#   variable. This should be a num.factors x num.numeric.variables matrix.
#   NOTE: you may need to use R's transpose function to make sure that the
#   dimensions of your return value are correct.

stdLevelDiff <- function(data) {
  factor.variable <- sapply(data, function(var) class(var)=='factor')
  level.means <- meanByLevel(data)
  overall.means <- sapply(data[, !factor.variable], mean)
  overall.sd <- sapply(data[, !factor.variable], sd)
  level.diff <- apply(level.means, 1, function(level)
    (level - overall.means) / overall.sd)
  return(t(level.diff))
}

tryCatch(checkIdentical(std.level.diff.t, abs(stdLevelDiff(iris))),
         error=function(err) errMsg(err))

# Implement the function "listLengths". Your function should take the
# following arguments:
#
# <data.list>: a list whose elements are vectors of varying length
#
# Your function should return the following:
#
# <element.lengths>: a numeric vector whose entries are the lengths of each
#   element of <data.list>

listLengths <- function(data.list) {
  
  # your code here
  data.list <- sapply(data.list, length)
  return(data.list)
}

tryCatch(checkEquals(list.lengths.t, listLengths(ex3.test2)),
         error=function(err) errMsg(err))


# Implement the function "standMatrixVariables". Your function should take
# the folowing arguments:
#
# <data.matrix>: a numeric matrix whose columns correspond to variables
#
# Your function should return the following:
#
# <standardized.matrix>: an nxn matrix (where n is the number of variables
#   i.e. columns of <data.matrix). Entry (i,j) of this matrix should contain
#   the following value:
#
#      (mean(col.i) - mean(col.j)) / sd(col.i, col.j)
#
# where sd(col.i, col.j) is the standard deviation of all values from both
# column i and j.

standMatrixVariables <- function(data.matrix) {
  
  # your code here
  standardized.matrix <- apply(data.matrix, 2, function(col.i) {
    apply(data.matrix, 2, function(col.j) {
      (mean(col.i) - mean(col.j)) / sd(c(col.i, col.j))
    })
  })
  
  return(standardized.matrix)
}

tryCatch(checkEquals(stand.matrix.variables.t,
                     standMatrixVariables(ex3.test4)),
         error=function(err) errMsg(err))


# Load in the "babies.csv" dataset for this problem. Implement the function
# "testGroupsGestation" that takes the following arguments:
#
# <data>: any subset of the babies.csv dataset
# <group1.idcs>: a numeric vector giving the indices of some subset of <data>
# <group2.idcs>: a numeric vector giving the indices of some other subset of <data>. 
#   NOTE: the two idcs vectors should contain no overlapping elements.
# <test.alternative>: a character string equal to one of the following
#   c("two.sided, "less", "greater") specifying the directionalty of the t.test
#
# Your function should return the following output:
#
# <t.test.output> the entire output of the t.test comparing gestation
#   period for the two given groups. Use the group1 subset the first
#   argument, group 2 as the second argument and the alternative direction
#   specified by <test.alternative>

testGroupsGestation <- function(data, group1.idcs, group2.idcs,
                                test.alternative='two.sided') {
  
  stopifnot(!any(group1.idcs %in% group2.idcs))
  
  # your code here
  group1 <- data[group1.idcs, ]
  group2 <- data[group2.idcs, ]
  t.test.output <- t.test(group1$gestation, group2$gestation,
                          alternative=test.alternative) 
  return(t.test.output)
}


tGG.output <- testGroupsGestation(test.data, g1, g2,
                                  test.alternative='greater')$p.value 
tryCatch(checkEquals(test.groups.gestation.t$p.value, tGG.output), 
         error=function(err) errMsg(err))

# Implement the function "listLengths". 

# Input variable:
# <data.list>: a list whose elements are vectors of varying length

# Output variable:
# <element.lengths>: a numeric vector whose entries are the lengths of each
#   element of <data.list>

listLengths <- function(data.list) {
  data.list <- sapply(data.list, length)
  return(data.list)
}

#tryCatch(checkEquals(list.lengths.t, listLengths(func1.test)),
#         error=function(err) errMsg(err))

#### Function 2
#### Implement the function "powers"

# Input variable :
# <x> : a numeric vector of length n
# <k> : an integer

# Output variable
# <x.powers> : A matrix of size [n x k] where the first column is x, the second column x^2, the third column x^4, etc.
#              the column names should be : "x", "x^2", "x^3" etc.

powers <- function(x, k){
  if(!is.numeric(x)) warning("x should be a numeric vector")
  x.powers <- x
  for(i in 2:k){
    x.powers <- cbind(x.powers, x^i)
  }
  return(x.powers)
}

#### Function #3
#### Implement the function "recipeConversion"

# Input variable:
# <recipe> : A data frame with three columns named "amount", "unit" and "ingredient"

# Output variable:
# <recipe.metric> : A data frame with three columns where cups have been converted to ml and ounces to grams.
#                   the number in "amount" should be updated, and the entry in "unit" changed
#                   both ml and gr should be rounded to the nearest multiple of 5,
#                   e.g. a row that read : [2 cups flour] should now be [475 ml flour]
#                   Note, if the "unit" is neither "cup"/"cups" nor "oz" the row should not be changed

# The conversion constants are: 
# 1 cup = 236.6 ml and 1 oz = 28.3 gr
# Please use these exact numbers, do not add decimal places.

# "unit" can take any of a number of values but you need to find the rows where
# "unit" is : "cup", "cups" or "oz"

# Note: to find a match in "unit" you have a few different options, you can go row by row
# and check if the unit is equal to cup/cups/oz using the "==" operator, you can use the
# match() or %in% operators or finally you can look at the function grep(). 

# If the column names of the input data frame are not "amount", "unit" and "ingredient" the
# function should stop and print out an error message

# Put your code here
recipeConversion <- function(recipe){
  
  # check that we have the 3 required columns
  required.col <- c("amount", "unit", "ingredient")
  included.col <- colnames(recipe)
  if(! all(required.col %in% included.col) ) stop("Make sure to include the columns: amount, unit and ingredient.") 
  
  recipe.metric <- recipe
  
  # convert cups to ml
  cup.to.ml <- grep("cup", recipe$unit)
  recipe.metric[cup.to.ml, "amount"] <- recipe[cup.to.ml, "amount"] * 236.6
  recipe.metric[cup.to.ml, "unit"] <- "ml"
  
  # convert oz to gr
  oz.to.gr <- grep("oz", recipe$unit)
  recipe.metric[oz.to.gr, "amount"] <- recipe[oz.to.gr, "amount"] * 28.3
  recipe.metric[oz.to.gr, "unit"] <- "gr"
  
  return(recipe.metric)
}


#### Function #4a
# Implement the function "bootstrapVarEst"

# Input variable:
# <x> : data vector
# <B> : the number of boostrap iterations

# Output variable:
# <boot.sigma2.est> : Bootstrap estimate for the variance of the sample mean (see lecture notes)

bootstrapVarEst <- function(x, B){
  x.bar.boot <- NULL
  n <- length(x)
  for(i in 1:B){
    x.bar.boot[i] <- mean(sample(x, n, replace=TRUE))
  }
  return(boot.sigma2.est=var(x.bar.boot))
}

#### Function #4b
#### Implement the function "jackknifeVarEst"

# Input variable:
# <x> : data vector

# Output variable:
# <jack.sigma2.est> : Jackknife estimate for the variance of the sample mean (see lecture notes)

jackknifeVarEst <- function(x){
  x.bar.jack <- NULL
  for(i in 1:length(x)){
    x.bar.jack[i] <- mean(x[-i])
  }
  return(jack.sigma2.est=var(x.bar.jack))
}

#### Function #4c
#### Implement the function "samplingVarEst"

# Input variables:
# <x> : data vector
# <type> : string that takes the values "bootstrap" or "jackknife", the default should be bootstrap.

# Output variable:
# <sampling.sigma.est> : The bootstrap estimate if type="bootstrap" and the jackknife estimate if type="jackknife"

# Note: this function calls the previous two functions.

samplingVarEst <- function(x, type="bootstrap"){
  if(type=="bootstrap") return(bootstrapVarEst(x, 1000)) # Edited by Andy -- April 13th 2015
  else if(type=="jackknife") return(jackknifeVarEst(x))
  else warning("Type has to be bootstrap or jackknife.")
}



# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.



# Write a function [firstColToNames] that takes a matrix or a data frame
# and converts the first column to row names.
# Input:  a matrix or a data frame
# Output : a matrix of data frame that is like the input except 
# -- the first column has been removed
# -- what was the first column is now row names.

# Example, make a small 2x4 matrix, test:
# test <- matrix(c(1, 5, 3, 8, 2, 5, 7, 9), ncol=4, byrow=T)
# > test
# [,1] [,2] [,3] [,4]
# [1,]    1    5    3    8
# [2,]    2    5    7    9
# The output from firstColToNames(test) should be a 2x3 matrix with row names
# > firstColToNames(test)
#   [,1] [,2] [,3]
# 1    5    3    8
# 2    5    7    9

## firstColToNames <- function(  ){
##   <your code here>

## }

firstColToNames <- function(m){
  # students do not need to include this test
  if(length(dim(m))!=2) print("m is not a matrix or dataframe")
  else{
    names <- as.character(m[,1])
    m <- m[, -1]
    rownames(m) <- names
    return(m)
  }
}


# [6 pts]
# Write a function [longerRange()] with
# Input [m1 and m2] : two numeric matrices or data frames, don't have to have the same dimension 
# Output : 1 if the range of m1 is larger than or equal to the range of m2, 
#        : 2 otherwise 
# The range is the distance between the maximum and minimum values
# The function should ignore NA values (i.e. if a matrix has an entry that is NA)

## longerRange <- function(m1, m2){
##   <your code here>  
## }

longerRange <- function(m1, m2){
  tmp1 <- max(m1, na.rm=T)-min(m1, na.rm=T)
  tmp2 <- max(m2, na.rm=T)-min(m2, na.rm=T)
  if(tmp1 >= tmp2) return(1)
  else return(2)
}

# Write a function [TempConv()] that takes
# Inputs
# [t] : a temperature (numeric value)
# [scale] : a character "F" or "C" depending on whether the 
#           input temperature is in Fahrenheit or Celcius
# Output 
# [t.new] : the temperature converted to the other scale.

# The conversion formulas are: 
# F = C * 9/5 + 32
# C = (F - 32) * 5/9
# so e.g. 30 F=-1.11 C and 30 C = 86 F

## TempConv <- function(t, scale){
##   <your code here>

## }

TempConv <- function(t, scale){
  if(scale=="F") return((t - 32) * 5/9)
  else if(scale=="C") return(t * 9/5 + 32)
}
