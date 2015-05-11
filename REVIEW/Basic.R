# Create [x], a numeric vector of length 2000 with 
# entries: 6, 12, 18, etc.
x <- 6 * seq(1, 2000)
# Create [x], a numeric vector of length 1000 with 
# entries: 5, 10, 15, etc.
x<- seq(5,5000,by=5) 
# all the even numbers between 2 and 20, in increasing order.
x <- seq(2, 20, by=2)
# all the odd numbers between -1 and -19, in decreasing order
y <- seq(-1, -19, by=-2)
# << y >> : a vector of length 100 which has the even numbers from 2 to 200.
# For clarity put your code here, directly below the 
y <- 2*(1:100)
# the numbers 1 through 5, each repeated 4 times, in order (so 1 1 1 1 2 2 2 2 etc.)
z <- rep(1:5, each=4)
# << z >> : a vector of length 20 with character entries, "hw1", "hw2", ..., "hw20"
z <- paste("hw", 1:20, sep="")
z1 <- paste("hw", 1:20)
# Create a variable [[ nmat ]] that is a matrix of size 100 x 100 and whose
# elements are drawn from a Normal distribution with mean 6 and sd 2
# Do not remove the set.seed command
set.seed(42)
nmat <- matrix(rnorm(100^2, 6, 2), nrow=100)
# << m >> : a vector of length 100 with entries from a standard normal distribution
set.seed(42)
m <- rnorm(100)
# << mean.m >> : a scalar, the mean of the numbers in << m >>
mean.m <- mean(m)
# << sd.m >> : a scalar, the standard deviation of the numbers in << m >>
sd.m <- sd(m)
# << max.m >> : a scalar, the maximum of the numbers in << m >>
max.m <- max(m)
# Create [y], a logical vector of length 2000 
# with y[i]=T if x[i] is divisible by 10, otherwise F
y <- x %% 10 == 0
# Create [y], a logical vector of length 1000 
# with y[i]=T if x[i] is divisible by 10, otherwise F
y <- <your code here>
y<-ifelse(x%%10,F,T)
# Create [z], a numeric vector of length 111 with entries
# that are drawn from a standard normal distribution (hint: rnorm)
# *and* stored in increasing order
set.seed(42)
z <- <your code here>
z<- sort(rnorm(111),decreasing = F)
# Create [w], a random permutation of the numeric values of a deck of cards
# (i.e. just the numbers 1 through 13 each repeated 4 times)
set.seed(2718)
w <- sample(rep(seq(1, 13), each = 4), 52, replace = F)
# Create [v], a numeric vector with :
# a random permutation of the even numbers from 2 to 222
set.seed(31415)
v<-sample(2*(1:222))
# Create [w], a random permutation of the numeric values of a deck of cards
# (i.e. just the numbers 1 through 13 each repeated 4 times)
set.seed(2718)
w <- <your code here>
w<- sample(rep(1:13,each=4))
# Create a variable [[ n ]] that is a data frame of dimension 10 x 3
# where the first column is the numbers 1-10, the second column the letters a-j and the
# third column has T/F, T if the number in column 1 is even, F if the number in column 1 is odd.
# Then add the column names :  num, lett, even
n <- data.frame(num=1:10, lett=letters[1:10], even=1:10%%2==0)
# In one R command, create a variable [[ m ]] that is a 3-by-4 matrix and has
# the entries 10, 20, through 120, where the numbers are entered row by row
# (so the first row is [ 10 20 30 40 ]).
m <- matrix(seq(10, 120, by=10), nrow=3, byrow=T)
# Create [m], a matrix of size 10x10 with entries that are 
# Exponential random variables (hint: rexp) with rate 3
# (arrange the values by column, as per default)
set.seed(344)
m <- matrix(rexp(100, rate = 3), nrow = 10, byrow = F)
# Create [m], a matrix of size 10x10 with entries that are 
# Exponential random variables (hint: rexp) with rate 3
# (arrange the values by column, as per default)
set.seed(344)
m <- <your code here>
m1<- matrix(rexp(100,3),ncol=10)
# Create [l], a list with 12 elements, each a vector of length 100.
# Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
set.seed(71)
l <- lapply(1:12, function(x) rpois(100, 5))
# Create [l], a list with 12 elements, each a vector of length 100.
# Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
set.seed(71)
<your code here>
l<- list(); for (i in 1:12) l[[i]] <-rpois(n=100,lambda = 5)

load("family.rda")
# Create [f1] a subset of family with only women age 50 or over
f1 <- <your code here>
f1<-family[family$age>50&family$gender=="f",]
# Create [f2] a subset of family with only men 6 foot tall or more
f2 <- <your code here>
f2<- family[family$gender=="m"&family$height>=72,]
# Create [f3] a subset of family of people whose name starts with T
f3 <- <your code here>
f3<-family[substr(family$firstName,1,1)=="T",]  
# Create [f4] a subset of family with just the youngest individual (so just one row)
f4 <- <your code here>
f4<- family[family$age==min(family$age),]
# Create a new data frame 
# << family.men >> : a data frame that is a subset of family, with only the men
family.men <- family[family$gender=="m", ]
# << family.young >> : a data frame, subset of family, with only people *under* 40
family.young <- family[family$age<40, ]
# << family.30y68i >> : a data frame, subset of family, with only people *over* 30, *shorter* than 68 in
family.30y68i<-family[family$age>30&family$height<68,]
# Formula for BMI : BMI = (weight in lbs) / (height in in)^2 * 703
 # << bmi >> : a vector with the BMI of each family member 
bmi <- family$weight/family$height^2 * 703
# << family2 >> : family with an added column of BMI, with column name bmi
family2 <- data.frame(family, bmi)
# Write a logical expression to create a logical vector, called OW_NHANES, that is TRUE if 
# a member of family is obese and FALSE otherwise (you need to consider makes and females
# separately).
OW_NHANES <- (family$gender == "m" & family$bmi > 26) | 
  (family$gender == "f" & family$bmi > 25) 
# Here is an alternative way to create the same vector that introduces 
# some useful functions and ideas
# We will begin by creating a numeric vector called OW_limit that is 26 for each male in
# the family and 25 for each female in the family.
# To do this, first create a vector of length 2 called OWval whose first element 
# is 26 and second element is 25.
# OWval <- your code here
OWval <- c(26, 25)
# Create the OW_limit vector by subsetting OWval by position, where the 
# positions are the numeric values in the gender variable 
# (i.e. use as.numeric() to coerce the factor vector storing gender to a
# numeric vector)
OW_limit <- OWval[as.numeric(family$gender)]
# Finally, us OW_limit and bmi to create the desired logical vector, called OW_NHANES2
# which, like OW_NHANES, is TRUE if a member of family is obese and FALSE otherwise
OW_NHANES2 <- OW_limit < family$bmi
# Use the vector OW_limit and each person's height to find the weight 
# that they would have if their bmi was right at the limit (26 for men and 
# 25 for women).   Call this weight OW_weight
# To do this, you need to know the formula for BMI,
# bmi = (weight/2.2) / (2.54/100 * height)^2
# and use it to write weight as a function of bmi and height.
# Now calculate OW_weight 
OW_weight <- OW_limit * (2.54/100 * family$height)^2 * 2.2

load("KaiserBabies.rda") 
# Create a table [t] of the education level ($ed) of all married ($marital) first time ($parity=1) mothers:
t <- <your code here>
t<- table(infants$ed[infants$marital=="Married"&infants$parity==1])
# Calculate [mw], the average birthweight ($bwt) of all babies whose were full term, i.e. gestation equal or more than 259 days.
mw <- <your code here>
mw<-mean(infants$bwt[infants$gestation>=259])  
# Let us do a linear regression of birth weight (bwt) as a function of the length
# of the pregnancy
# make sure to keep this line:
fit <- lm(bwt ~ gestation, data=infants)
# Create a table which tallies the education level of mothers (varible ed in the data frame)
# against the education level of the fathers (variable ded in the data frame).
# Store the table in the variable [[ ed.table ]]
ed.table <- table(infants$ed, infants$ded)

load('SFhousing-1.rda')
# mean.price <- your code here
# med.price <- your code here
mean.price <- mean(housing$price)
med.price <- median(housing$price)
# For each house in the dataset, calculate the absolute difference between its
# price and the mean price of houses in Alameda. Store this as the variable
# <price.diffs>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset
# price.diffs <- your code here
price.diffs <- sapply(housing$price, function(p) abs(p - mean.price))
# IBH: this is simpler:
price.diffs <- abs(housing$price - mean.price)
# The variable br indicates the number of bedrooms in each house. Please create
# two new data frames that are subsets of the original data frame, according to
# these criteria:
# 1) houses with more than 3 rooms <housing.large>
# 2) houses with up to and including 3 rooms <housing.small>
housing.large <- housing[housing$br > 3, ]
housing.small <- housing[housing$br <= 3, ]
# For each of your subsets, create a vector giving the price of each house. Name
# these variables <housing.large.price> and <housing.small.price>.
housing.large.price <- housing.large$price
housing.small.price <- housing.small$price

load("family-quiz.rda")
# calculate the mean and median age in the family. Store these as the
# variables <mean.age> and <med.age> respectively.
# mean.age <- your code here
# med.age <- your code here
mean.age <- mean(family$age)
med.age <- median(family$age)
# For each person in the dataset, calculate the squared difference between its
# height and the mean height of the family. Store this as the variable
# <height.diffs>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset
# height.diffs <- your code here
height.diffs <- (family$height - mean(family$height))^2
# Please create the following two data frames and store them with the indicated names:
# 1) people whose age is strictly greater than <mean.age>:  <ppl.old>
# 2) people whose age is less than or equal to <mean.age>: <ppl.young>
ppl.old <- family[family$age > mean.age,]
ppl.young <- family[family$age <= mean.age,]
# For each of your subsets, create a vector giving the weight of each person. Name
# these variables <ppl.old.weight> and <ppl.young.weight>.
ppl.old.weight <- ppl.old$weight
ppl.young.weight <- ppl.young$weight


ex1.data <- read.csv('ex1-data.csv', header=T)
# The data you just loaded is stored as a data frame object. Your data frame
# should have one column with the name "x". You can extract a column from a data
# frame using the $ operator: <data.frame>$<column.name>. In our case, there is
# only one column, and the command ex1.data$x will extract it. A column
# extracted from a data frame is a vector object. Many operations in R can be
# performed on vectors but not on data frames. Please extract the x vector from
# <ex1.data> and store this vector as a variable named <ex1.data.v>.
# ex1.data.v <- your code here
ex1.data.v <- ex1.data$x
# find the class of <ex1.data.v>. Store this as a variable with the name
# <data.class>
# data.class <- your code here
data.class <- class(ex1.data.v)
# create the variable <n.ex1.data> that gives the number of observations in the
# vector <ex1.data.v>.
# n.ex1.data <- your code here
n.ex1.data <- length(ex1.data.v)
# calulate the mean of <ex1.data.v>. Store this as the variable
# <data.mean>. In addition, calculate the trimmed mean, setting the trim
# argument to 0.1. Store this as the variable <data.mean.trimmed>.
# data.mean <- your code here
data.mean <- mean(ex1.data.v)
data.mean.trimmed <- mean(ex1.data.v, trim=0.1)
# calculate the standard deviation of <ex1.data.v>. Store this as the variable
# <data.sd>.
data.sd <- sd(ex1.data.v)
# find the minimum and maximum values of <ex1.data.v>. Store these as variables
# with names <min.ex1.data> and <max.ex1.data> respectively.
min.ex1.data <- min(ex1.data.v)
max.ex1.data <- max(ex1.data.v)
# use R's summary function to find the min, 1st quartile, median, mean, 3rd
# quartile, and max of <ex1.data.v>. Store this as the variable <data.summary>.
# data.summary <- your code here
data.summary <- summary(ex1.data.v)
# create the vector <ex1.data.double>. This should be a vector of each
# observation from <ex1.data.v> doubled.
# ex1.data.double <- your code here
ex1.data.double <- 2 * ex1.data.v
# calculate the sum of the observations in <ex1.data.v>. Store this as the
# variable <ex1.data.sum>.
# ex1.data.sum <- your code here
ex1.data.sum <- sum(ex1.data.v)
# create a new vector of standard normal variables with the same length as
# <ex1.data.v>. Store this as a variable with the name <random.data>. Calculate
# the correlation between <ex1.data.v> and <random.data>. Store this as a
# variable with the name <data.cor>
set.seed(47) # THIS LINE MUST COME BEFORE YOUR GENERATED DATA. DO NOT REMOVE IT.
# random.data <- your code here
# data.cor <- your code here
random.data <- rnorm(100)
data.cor <- cor(ex1.data.v, random.data)


load("SFHousing.rda")

# Q4. (not graded)
# Use the following functions to examine the dataset
# objects(), class(), dim(), head(), names(), summary().

# How many cities are in the dataset, store the answer in the variable
# n.cities.
#n.cities <- your code here 
n.cities <- nrow(cities)


# How many house sales are included in the dataset?  Store the answer in
# the variable n.housesale.
#n.housesale <- your code here
n.housesale <- nrow(housing)


# How many of these house sales were in Berkeley?
#n.housesale.Berk <- your code here
n.housesale.Berk <- sum(housing$city=="Berkeley")

# Create a vector with the names of all variables in housing.
# all.housing.variable <- your code here
all.housing.variables <- names(housing)



# Q5.
# We will work the houses in Albany, Berkeley, Piedmont, and Emeryville only.
# Subset the data frame so that we have only houses in these cities
# and keep only the variables city, zip, price, br, bsqft, and year
# Call this new data frame BerkArea. This data frame should have 4059 observations
# and 6 variables.

# Create two vectors, one with the names of the cities we want to keep,
# one with the names of the variables we want to use.
# local.cities <- your code here
local.cities <- c("Albany","Berkeley","Piedmont", "Emeryville")

# some.housing.variables <- your code here
some.housing.variables <- c("city", "zip", "price", "br", "bsqft", "year")

# Create the smaller data frame
# BerkArea <- your code here
BerkArea <- housing[housing$city %in% local.cities, some.housing.variables ]

# Q6.
# We are interested in making plots of price and size of house, but before we do this
# we will further subset the data frame to remove the unusually large values.
# Use the quantile function to determine the 99th percentile of price and bsqft
# and eliminate all of those houses that are above either of these 99th percentiles
# Call this new data frame BerkArea, as well. It should have 3999 oobservations.

# BerkArea <- your code here
BerkArea <- BerkArea[(BerkArea$price <= quantile(BerkArea$price, p = 0.99))
                     & (BerkArea$bsqft <= quantile(BerkArea$bsqft, p = 0.99, na.rm = TRUE)), ]

# Q7.
# Create a new vector that is called pricePsqft by dividing the sale price by the square footage
# Add this new variable to the data frame.

# BerkArea$pricePsqft <- your code here
BerkArea$pricePsqft <- BerkArea$price / BerkArea$bsqft

#  Q8.
# Create a vector called br5 that is the number of bedrooms in the house, except
# if this number is greater than 5, it is set to 5.  That is, if a house has 5 or more
# bedrooms then br5 will be 5. Otherwise it will be the number of bedrooms.

# br5 <- your code here
br5 <- BerkArea$br
br5[BerkArea$br > 4] <- 5

# The following uses indicator functions for different intuition on a solution:
# br5 <- BerkArea$br * (BerkArea$br < 5) + 5 * (BerkArea$br > 4) 


# Q 9.
# Use the rainbow function to create a vector of 5 colors, call this vector rCols.
# When you call this function, set the alpha argument to 0.25 (we will describe what this does later)

# rCols <- your code here
rCols <- rainbow(5, alpha = 0.25)

# Create a vector called brCols of 4059 colors where each element's
# color corresponds to the number of bedrooms in the br5.
# For example, if the element in br5 is 3  then the color will be the third color in rCols.

# brCols <- your code here
brCols <- rCols[br5]

######
# We are now ready to make a plot!
# Try out the following code
# (examine each part of the command and use the help for plot() and for par() to find out what it does)

plot(pricePsqft ~ bsqft,
     data = BerkArea, 
     main = "Housing prices in the Berkeley Area",
     xlab = "Size of house (square ft)",
     ylab = "Price per square foot",
     col = brCols, pch = 19, cex = 0.5)
# Look at the help for legend(), examine what this function adds to the plot.
legend(legend = 1:5, fill = rCols, "topright")

