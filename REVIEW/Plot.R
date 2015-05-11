# The object fit is a list and in it you will find the coefficients of the regression
# Do a scatterplot of birthweight (on y-axis) vs. gestation (on x-axis)
# Add to the plot the line estimated in fit (hint: find the coefficients)
# Plot the line in red and put your own x- and y-axis labels on the plot.
plot(bwt ~ gestation, data=infants, xlab='gestation', ylab='bwt')
abline(a=fit$coef[1], b=fit$coef[2], col='red')
# Plot a histogram of the fathers' heights (varible dht in the data frame).
hist(infants$dht)

# Please create a plot of house price (y-axis) against bsqft (x-axis). Your plot
# should include the following features:
# 1) a title "Housing price vs Building sqft"
# 2) a red line with intercept=169500 and slope=275
# 3) plotting character set to 20
plot(housing$bsqft, housing$price, main="Housing price vs Building sqft",
     pch=20)
abline(a=169500, b=275, col='red')

plot(family$age, family$bmi,
     main = "BMI vs Age",
     xlab ="Age (Year)", ylab = "BMI")
abline(a = 22.89050, b = 0.03501, col = "blue")


# Make a plot of actual weight against the weight at which they would
# be overweight using the plot function.
# use the abline() function to include a red identity line.

# plot( your code here )
# abline( your code here )
plot(family$weight, OW_weight)
abline(a=0,b=1, col='red')

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

load("WR1500MeterMen.rda")

# The name of the object loaded is wr1500m
# The time (in the column "times") in these data are recorded in seconds, 
# and they are seconds over 3 minutes. 
# So a time of 70 is really 4 minutes and 10 seconds.

# Q1a. How many world records does this data frame contain?
#
# n.wr <- your code here
n.wr <- nrow(wr1500m)

# Q1b. Use R commands to find out who currently holds the world
# record in the men's 1500 meter.

# wr.name <- your code here
wr.name <- wr1500m$athlete[wr1500m$times == min(wr1500m$times)]
### Hicham El Guerrouj

# Let's look at the relationship between date and time.
# Q1c. What type of variable (numeric (continuous or discrete), nominal ordinal)
# are year and times? (no need to use R code to answer this question)

### year : discrete
### times : continuous

# When we are examining a variable to see how it changes in time,
# we typically make a line plot, with time on the x-axes and 
# the (x,y) values connected with line segments.

# Q2a. Begin by making a step plot of year by times for these data.
# (hint: use the type argument in plot)
# Don't bother to make the plot pretty yet; we will get to that later.
# But do add 180 to the times so that they are accurate measurements in seconds,
# store that in a new variable and add to the data frame.

# times_sec <- your code here
# wr1500m <- your code here
# plot( your code here )

times_sec <- wr1500m$times + 180
wr1500m <- data.frame(wr1500m, times_sec)  
plot(wr1500m$year, wr1500m$times_sec,  type = "s") 

# Q2b. Redo the plot using a date that incorporates the month as 
# well as the year. For example, in Sep 1904 the world record 
# was broken by James Lightbody. Use a date of 1904.75 for this
# date. If any month is NA, use 0.5 for the fraction.
# Create a new variable, new_year, with the date in this format but
# first find and set all missing months to 0.5
# Add new_year to the dataframe.

# your code here
# new_year <- your code here
# wr1500m <- your code here

# plot( your code here )

wr1500m$month[is.na(wr1500m$month)] = 6
new_year = wr1500m$year + wr1500m$month/12
wr1500m = data.frame(wr1500m, new_year)
plot(wr1500m$new_year, wr1500m$times_sec,  type = "s")

# Q3. The current world record was set in 1998. If we want to
# show that this record still stands in 2014, we could add a 
# horizontal line segment to the plot from 1998 to 2014 at the 
# 1998 record time.  
# To do this: remake the plot and set the xlim parameter 
# so that 2014 is included in the x-axis scale;
# then use the lines() function to add the additional segment.

# wr_1998 <- your code here
# plot( your code here )
# lines( your code here )

plot(wr1500m$new_year, wr1500m$times_sec,  type = "s",
     xlim = c(min(wr1500m$new_year), 2020))

wr_1998 = new_year[wr1500m$times == min(wr1500m$times)]
lines(x = c(wr_1998, 2014 + 9/12), y = rep(wr1500m$times_sec[51], 2))


# Q4. There are two times where the record stood for several
# years - in 1944 and 1998. Let's make it easier to see these
# dates and let's include the name of the athlete who set
# the record.  This additional reference information makes
# our plot richer.
# Add two grey vertical lines. One at 1944 and the other at 1998.
# Add the runner's name next to each vertical line.
# To do this, you will need the abline() function, the text() function,
# and you might want to consider the cex, col, pos, adj parameters.
# Also, do not type in the athlete's name. Instead, use subsetting
# of wr1500m$athlete to access it.

# wr_1944 <- your code here
# abline( your code here )
# abline( your code here )
# text( your code here )
# text( your code here )

wr_1944 = new_year[wr1500m$year == 1944]

abline(v = wr_1944, col = "grey")
abline(v = wr_1998, col = "grey")
## 2 points for all that, OK to use 1944 and 1998 instead of the new_year values,
## but -1 if not grey

text(wr_1944, wr1500m$times_sec[wr1500m$new_year == wr_1944], 
     labels = wr1500m$athlete[wr1500m$new_year == wr_1944],
     adj = 1, cex = 0.7, col = "blue")
text(wr_1998, wr1500m$times_sec[wr1500m$new_year == wr_1998], 
     labels = wr1500m$athlete[wr1500m$new_year == wr_1998],
     adj = 1, cex = 0.7, col = "red")


# Q5. Now we are ready to add other contextual information.
# Remake the plot as before but now adding axis labels and a title.
# This is the FINAL version of the plot of world record times.

plot(wr1500m$new_year, wr1500m$times_sec,  type = "s", 
     xlim = c(1892,2015), xlab = "Year",
     ylab = "Record Times (sec)", main = "World Records in Men's 1500 meter")

abline(v = wr_1944, col = "grey")
abline(v = wr_1998, col = "grey")
points(x = c(wr_1998, 2012+9/12),
       y = rep(wr1500m$times_sec[wr1500m$new_year == wr_1998], 2),
       type = "l")

text(wr_1944-0.5, wr1500m$times_sec[wr1500m$new_year == wr_1944], 
     labels = wr1500m$athlete[wr1500m$new_year == wr_1944],
     adj = 1, offset = 2, cex = 0.7, col = "blue")
text(wr_1998-0.5, wr1500m$times_sec[wr1500m$new_year == wr_1998], 
     labels = wr1500m$athlete[wr1500m$new_year == wr_1998],
     adj = 1, offset = 2, cex = 0.7, col = "red")

## 4 points (1 point each for: basic plot, labels on axes and title, 
##  extenison of line, 2 vertical lines with text names 

################################
# PLOT 2
# A lot of medal counting goes on during the Olympics.
# We might wonder about the relationship between number of medals
# a country has and the size of its population and its wealth.
# We collected data from various sources (ManyEyes, Guardian,
# ISO) to create this data frame with GDP, population, and other information
# about each country that participated in the Olympics.

# The data frame SO2012Ctry contains this information.
# It can be loaded into R with

# load( your code here )
# load("~/src/stat133/hw3/SummerOlympics2012Ctry.rda")
load("SummerOlympics2012Ctry.rda")

#Q6 Take a look at the variables in this data frame.
# What kind of variable is GDP and population?

### GDP : continuous
### population : discrete

# What about Total?
### Total : discrete


# To examine the relationship between these three variables,
# we could consider making a scatter plot of GDP against population
# and use plotting symbols that are proportional in size to
# the number of medals. 

# To begin, make a plot of GDP against population. 
# Consider which of the three principles of good graphics this
# plot violates and why.

# plot( your code here )
names(SO2012Ctry)
plot(SO2012Ctry$pop, SO2012Ctry$GDP)

### Data stand out, Values are plotted on the top of each other  
### Facilitate comparison OR poor scale, We should zoom in on the bulk of the data
### Should fill data region, lots of empty space
### Information rich, for example identify outliers

#Q7. Let's examine GDP per person (create this new variable yourself)
# and population. Use a log scale for both axes. Use the symbols()
# function rather than plot(), and create circles for the plotting
# symbols() where the area of the circle is proportional to the 
# total number of medals.

# GDP_per_person <- your code here
# SO2012Ctry <- your code here
# symbols( your code here )

GDP_per_person <-  SO2012Ctry$GDP/SO2012Ctry$pop
SO2012Ctry <- data.frame(SO2012Ctry, GDP_per_person)

symbols(log(SO2012Ctry$pop), log(SO2012Ctry$GDP_per_person), 
        circles = sqrt(SO2012Ctry$Total)/40, inches = FALSE)

# Q8. It appears that the countries with no medals are circles too.
# Remake the plot, this time using *only the countries that won medals*. 
# If necessary adjust the size of the circles.
# Then add the non-medal countries to the plot using the "." 
# plotting character.

# your plotting code here
with(SO2012Ctry[SO2012Ctry$Total > 0, ], 
     symbols(log(pop), log(GDP_per_person), 
             circles= sqrt(Total)/40, inches = FALSE))

with(SO2012Ctry[SO2012Ctry$Total == 0, ], 
     points(log(pop), log(GDP_per_person), pch = "."))

# Q9. Make the plot information rich by adding axis labels, 
# title, and label 5 of the more interesting points
# with the country name. Use text() to do this.

with(SO2012Ctry[SO2012Ctry$Total > 0, ], 
     symbols(log(pop), log(GDP_per_person), 
             circles= sqrt(Total)/40, inches = FALSE,
             xlab = "log of population", ylab = "log of GDP per person", 
             main = "Population vs GDP per person"))

with(SO2012Ctry[SO2012Ctry$Total == 0, ], 
     points(log(pop), log(GDP_per_person), pch = "."))

## Added points are the top five total medal earning countries
top5=order(SO2012Ctry$Total, decreasing = TRUE)[1:5]
text(log(SO2012Ctry$pop)[top5], log(SO2012Ctry$GDP_per_person)[top5], 
     labels = SO2012Ctry$ISO[top5],
     cex = seq(.8, .4, by = -.1), col = "red")

load("London2012ALL_ATHLETES.rda")
# There is one observation for each athlete. 
# (Actually, about 20 athletes have two records if they
# competed in different sporting events. Let's not worry about that.)

#Q13. We are interested in the relationship between Sport and Sex. 
# Examine the data frame and check which type of data each variable is.
names(athletes)
### Name : nominal
### Sex : nominal
### Sport : nominal
### Country : nominal
### MoreThan1Sport : ordinal or nominal is OK

# The table() and sum() functions might be helpful for answering 
# some of the questions below. 

# How many athletes competed in the 2012 Olympics?
# n.athletes <- your code here
n.athletes <- nrow(athletes)
###10903

# How many women competed?
n.women <- sum(athletes$Sex == "F")
### 4835

# What proportion of the participants were women?
# frac.women <- your code here
frac.women <- n.women/n.athletes

# How many sports were there?
# n.sports <- your code here
n.sports <- length(table(athletes$Sport))
### 33


#Q14. Make a barplot of Sport and Sex that emphasizes the 
# important differences. To do this, first make a table of 
# Sex by Sport. This will be the input to barplot(). 
# Make the barplot with the parameter beside = TRUE and 
# and again with beside = FALSE. Determine which of these 
# barplots provides the easiest comparison. 

# athTab <- your code here
# make barplots
athTab=table(athletes$Sport, athletes$Sex)
barplot(athTab, beside = TRUE)
barplot(athTab, beside = FALSE)

# what should beside be set to, T/F?
set.beside <- T

### Barplot with beside = TRUE provides the easiest comparison. 

#Q15. Remake the barplot above, but this time switch the order 
# of Sport and Sex in the call to table(). Use the value for
# the beside parameter that you decided was best for the 
# plot in Q 14. 
# athTab2 <- table()

athTab2=table(athletes$Sex, athletes$Sport)
barplot(athTab2, beside = TRUE)

# Compare the barplot with (Sex, Sport) vs (Sport, Sex). 
# Which makes a more interesting visual comparison, plot 1 or 2?
# store your answer in best.plot.

# best.plot <- your answer

### (Sex, Sport) emphasize the important comparison - gender


# Q16. Notice that the bars are in alphabetical order by sport.
# To facilitate comparisons, we might want to arrange
# the bars in order of participation in a sport. To do this,
# call order() on the return value from making a table of Sport alone.
# Assign this vector to a variable, say orderSport.
# Then reorder your two-way table of Sport and Sex,
# using the orderSport vector and [ ] to subset the table and rearrange
# the rows/cols. The resulting barplot should show bars in 
# increasing height.

# orderSport <- your code here
# barplot( your code here )

orderSport = order(table(athletes$Sport))
barplot(table(athletes$Sex, athletes$Sport)[, orderSport], beside = TRUE)


# Q17. Finally to make the plot more informaation rich, try turning
# the x-axis labels on their side. To do this, find a parameter
# in par() that will rotate the x-axis tick mark labels. Even though
# you found the parameter in the par() function, this
# parameter can be added in the call to barplot().
# Also find and use a parameter to shrink the text for these labels. 
# Lastly, add a title to the plot.

barplot(table(athletes$Sex, athletes$Sport)[ , orderSport],
        beside = TRUE, cex.names = 0.8, las = 3 , ylim = c(0,1200),
        main = "Olympic Sport Participation by Gender")


