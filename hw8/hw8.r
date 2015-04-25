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

