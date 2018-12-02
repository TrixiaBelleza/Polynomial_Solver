this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#source("belleza_exer4.r")

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
    print(augcoeffmatrix)
    return(list(variables = vars, augcoeffmatrix = augcoeffmatrix))
  }
}
#Gets the pivot row based on the given max value in column
getPivotRow <- function(acm, currCol, rowNum, max_in_col_i) {
  for(i in 1:rowNum) {
    if(abs(acm[i, currCol]) == max_in_col_i) {
      return(list(row = acm[i,], index = i))    #returns the row and the index it was located
    }
  }
}
Gaussian <- function(acm, varCount) {
  for(i in 1:(varCount-1)) {    #for each column
    #Find pivot row
    max_in_col_i = max(abs(acm[i:varCount,i]))
    pivot = getPivotRow(acm, i, varCount, max_in_col_i)
    
    if(pivot$row[i] == 0) {
      return(NA)
    }
    else { 
      #Swap
      #do partial pivoting : swap(PIVOT_ROW, acm[i,i])
      temp = acm[i,]
      acm[i,] = pivot$row
      acm[pivot$index, ] = temp
    }
    
    #then make the lower diagonal zero
    for(j in (i+1):varCount) {
      PE = acm[i,i]
      mult = acm[j,i] / PE
      norm = mult * acm[i,]
      acm[j,] = acm[j,] - norm
    }
  }
  #Store x[n] to list kasi may value na siya.
  x <- c()
  lastVar = acm[varCount, varCount+1] / acm[varCount, varCount]
  x[varCount] = lastVar
  
  #store all b values (all values at the last column) to b
  b <- c()
  for(i in 1:varCount) {
    b <- c(b, acm[i, varCount+1])
  }
  #get the other remaining unknowns x[1] : x[n-1]
  for(i in (varCount-1):1) {
    coeffs = acm[i, (i+1) : varCount]
    knowns = x[(i+1) : varCount]
    coeffsCROSSknows = coeffs * knowns
    coeffWITHunknown = acm[i,i]
    x[i] = (b[i] - sum(coeffsCROSSknows)) / coeffWITHunknown
  }
  return(list(solutionSet = x, matrix = acm))     
}

PolynomialReg <- function(size,x,y) {
  #size = degree
  polynomial = ""
  f = "f <- function(x)"
  a = getACM(x,y)
  acm = a$augcoeffmatrix
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
  
  return(list(coefficients = GaussianResult$solutionSet, func = s, polynomial = polynomial))     
}
x <- c(20,50,50,70,70,70,80,80,80,90,90,90,100,100,100)
y <- c(3.3,2.8,2.9,2.3,2.6,2.1,2.5,2.9,2.4,3.0,3.1,2.8,3.3,3.5,3.0)
size = 3
#print(x)
regression = PolynomialReg(size,x,y)
#print(regression$func(100))
#print(regression$coefficients)
# remove r data: 
#rm(list=ls())