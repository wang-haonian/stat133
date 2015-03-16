#################################################################################
#### Functions for BML Simulation Study


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