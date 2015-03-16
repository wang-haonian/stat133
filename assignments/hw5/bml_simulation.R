#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


source("bml_functions.R")
densities <- seq(0,1,by=0.1)
#10*10
op.density1 <- replicate(100, sapply(densities, function(p) bml.sim(10, 10, p)))
op.average1 <- apply(op.density1, 1, mean)
plot(densities, op.average1, xlab="density",ylab="how many steps to hit gridlock",main="10*10 matrix",
     pch=".",cex.main=0.8, cex=6, col="blue")

#10*15
op.density2 <- replicate(100, sapply(densities, function(p) bml.sim(10, 15, p)))
op.average2 <- apply(op.density2, 1, mean)
plot(densities, op.average2, xlab="density",ylab="how many steps to hit gridlock",main="10*15 matrix",
     pch=".",cex.main=0.8, cex=6, col="blue")

#15*15
op.density3 <- replicate(100, sapply(densities, function(p) bml.sim(15, 15, p)))
op.average3 <- apply(op.density3, 1, mean)
plot(densities, op.average3, xlab="density",ylab="how many steps to hit gridlock",main="15*15 matrix",
     pch=".",cex.main=0.8, cex=6, col="blue")

#20*20
op.density4 <- replicate(100, sapply(densities, function(p) bml.sim(20, 20, p)))
op.average4 <- apply(op.density4, 1, mean)
plot(densities, op.average3, xlab="density",ylab="how many steps to hit gridlock",main="20*20 matrix",
     pch=".",cex.main=0.8, cex=6, col="blue")

init.m1<-bml.init(10,10,0.4)
init.m2<-bml.init(20,20,0.4)
image(t(apply(init.m1,2,rev)),col=c("white","red","blue"))
image(t(apply(init.m2,2,rev)),col=c("white","red","blue"))

m <- matrix(sample(c(0,1,2),size=100,replace=T,prob=c((1-0.4),0.4/2,0.4/2)),nrow=10,ncol=10)
new.m1<-bml.sim1(init.m1)
image(t(apply(new.m1,2,rev)),col=c("white","red","blue"))

m <- matrix(sample(c(0,1,2),size=400,replace=T,prob=c((1-0.4),0.4/2,0.4/2)),nrow=20,ncol=20)
new.m2<-bml.sim1(init.m2)
image(t(apply(new.m2,2,rev)),col=c("white","red","blue"))

