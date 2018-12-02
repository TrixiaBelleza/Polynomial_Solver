this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("belleza_exer4.r")

getRegColNames <- function(size) {
  colnames <- c()
  for (i in 1:(size+1)) {
    #concat x and number i
    colname = paste("a",i, sep="")
    colnames <- c(colnames, colname)  
  }
  colnames <- c(colnames, "RHS") #adds RHS at the last index of the vector
  return(colnames)
}
#returns -> c(1, 2, 3, ..., n)
getRegRowNames <- function(size) {
  rownames <- c()
  for (i in 1:(size+1)) {
    rownames <- c(rownames, i)      
  }
  return(rownames)
}
getACM <- function(x,y) {
  if((length(x) != length(y)) | (size < 1)){
    return(NA)
  }
  else{
    #declare matrix
    vars = getRegColNames(size)[1:(size+1)]
    augcoeffmatrix = matrix(data=NA, nrow=size+1, ncol=size+2, dimnames=list(getRegRowNames(size), getRegColNames(size)))
    RHScount = 0
    for(i in 1:(size+1)) { #for each row
      count = i-1
      for(j in 1:(size+2)){ #for each column
        if(j == (size+2)) {
          augcoeffmatrix[i,j] = sum((x**RHScount) * y)
          RHScount = RHScount + 1
        }
        else {
          augcoeffmatrix[i,j] = sum(x**count)
          count = count + 1
        }
      }
    }
    #print(augcoeffmatrix)
    return(list(variables = vars, augcoeffmatrix = augcoeffmatrix))
  }
}

PolynomialRegression <- function(size,x,y) {
  #size = degree
  polynomial = ""
  f = "f <- function(x)"
  a = getACM(x,y)
  acm = a$augcoeffmatrix
  print("Acm")
  print(acm)
  print(size)
  GaussianResult =  Gaussian(acm, size+1)
  print(GaussianResult)
  for(i in 1:(size+1)) {
    if(i!=(size+1)) {
      polynomial = paste(polynomial, " ", GaussianResult$solutionSet[[i]]," * x**", (i-1), " + ", sep="")
    }else{
      polynomial = paste(polynomial, " ", GaussianResult$solutionSet[[i]]," * x**", (i-1), "", sep="")
    }
  }
  s = eval(parse(text = paste(f,polynomial, sep=" ")))

  return(list(coefficients = GaussianResult$solutionSet, func = s))     
}
#x <- c(50,50,50,70,70,70,80,80,80,90,90,90,100,100,100)
#y <- c(3.3,2.8,2.9,2.3,2.6,2.1,2.5,2.9,2.4,3.0,3.1,2.8,3.3,3.5,3.0)
#size = 3

regression = PolynomialRegression(size,x,y)
#print(regression$func(100))
#print(regression$coefficients)
