source("PolynomialRegression.R")
getColNames <- function(interval) {
  colnames <- c()
  for (i in 1:(interval)) {
    if(i!=1){
      colname = paste("a",i,sep="")
      colnames <- c(colnames, colname)
    }
    colname = paste("b",i, sep="")
    colnames <- c(colnames, colname)  
    colname = paste("c",i, sep="")
    colnames <- c(colnames, colname)  
  }
  colnames <- c(colnames, "RHS") #adds RHS at the last index of the vector
  return(colnames)
}

getRowNames <- function(size) {
  rownames <- c()
  for (i in 1:(size)) {
    rownames <- c(rownames, i)      
  }
  return(rownames)
}

getFuncPerInterval <- function(solutionSet, interval) {
  function_list <- c()
  solSet_index = 1
  for (i in 1:interval){
    if(i == 1){
      f = paste("f",i, " <- function(x) ", sep ="")
      func_interval = paste(f, solutionSet[solSet_index], "x + ", solutionSet[solSet_index+1], sep ="") #bx + c = f1(x) 
      #print(func_interval)
      function_list <- c(function_list, func_interval)
      solSet_index = solSet_index + 2
    }
    else{
      f = paste("f",i, " <- function(x) ", sep ="")
      func_interval = paste(f, solutionSet[solSet_index], "x**2 + ", solutionSet[solSet_index+1], "x + ", solutionSet[solSet_index+2], sep="") #bx + c = f1(x) 
      #print(func_interval)
      function_list <- c(function_list, func_interval)
      solSet_index = solSet_index + 3
    }
  }
  return(function_list)
}

getFuncWithinInterval <- function(x,functions_per_interval_list, x_value) {
  for (i in 1:length(functions_per_interval_list)) {
    if(x[i] <= x_value & x_value <= x[i+1]){
      return(functions_per_interval_list[i])
    }
  }
}

GetACM_QSI <- function(x,y) {
  a_column = 0
  b_column = 1
  c_column = 2
  row_count = 1
  
  interval = length(x)-1
  rhs_column = interval*3
  acm = matrix(data=0, nrow=(interval*3)-1, ncol=(interval*3), dimnames=list(getRowNames((interval*3)-1), getColNames(interval)))
  
  #Condition 1 -> Function values of adjacent polynomials must be equal to the interior knots
  for (i in 3:(interval+1)){
    a_coeff = x[i-1] * x[i-1]
    b_coeff = x[i-1]
    c_coeff = 1
    rhs = y[i-1]
    if((i-1) != 2){
      acm[row_count,a_column] = a_coeff
    }
    #a[i-1]*x[i-1]^2 + b[i-1]*x[i-1] + c[i-1] = y[i-1]
    acm[row_count,b_column] = b_coeff
    acm[row_count,c_column] = c_coeff
    acm[row_count,rhs_column] = rhs
    
    #a[i]*x[i-1]^2 + b[i]*x[i-1] + c[i] = y[i-1]
    acm[row_count+1, b_column+3] = b_coeff
    acm[row_count+1, a_column+3] = a_coeff
    acm[row_count+1, c_column+3] = c_coeff
    acm[row_count+1, rhs_column] = rhs
     
    a_column = a_column + 3
    b_column = b_column + 3
    c_column = c_column + 3
    row_count = row_count + 2
  }
  
  #Condition 2 The first and last functions must pass through the endpoints
  
  #a1*x[1]^2 + b1*x[1] + c1 = y[1]
  acm[row_count, 1] = x[1]
  acm[row_count, 2] = 1
  acm[row_count, rhs_column] = y[1]
  
  #an*x[n]^2 + bn*x[n] + cn = y[n] where n is the interval number
  acm[row_count+1, a_column] = x[interval+1]*x[interval+1]
  acm[row_count+1, b_column] = x[interval+1]
  acm[row_count+1, c_column] = 1
  acm[row_count+1, rhs_column] = y[interval+1]
  
  row_count = row_count + 2
  
  #Condition 3 
  a_column = 0
  b_column = 1
  c_column = 2
  for (i in 3:(interval+1)){
    if((i-1) != 2){
      acm[row_count, a_column] = 2*x[i-1]
    }
    #a[i-1]*2*x[i-1] + b[i-1] - a[i]*2*x[i-1] - b[i] = 0
    acm[row_count, b_column] = 1
    acm[row_count, a_column+3] = -2*x[i-1]
    acm[row_count, b_column+3] = -1
    
    a_column = a_column + 3
    b_column = b_column + 3
    c_column = c_column + 3
    row_count = row_count + 1
  }
  return(acm)
}

QuadraticSplineInterpolation <- function(x,y, x_value) {
  #sort the data with respect to x coordinate
  o = order(x)
  x = x[o]
  y = y[o]    #dapat yung y ay ka-match pa rin yung x niya
  
  acm = GetACM_QSI(x,y)
  interval = length(x)-1
  gaussian = Gaussian(acm,(interval*3)-1) #Gaussian input (acm, number of columns)
  func_per_interval = getFuncPerInterval(gaussian$solutionSet, interval) #Get f(x) for all intervals
  correct_func = getFuncWithinInterval(x,func_per_interval,x_value)  #Get correct f(x) for the interval where x_value lies
  
  return(list(func_per_interval = func_per_interval, correct_func = correct_func))
} 

x <- c(3,4.5,7,9)
y <- c(2.5,1,2.5,0.5)
x_value = 5
print(x_value)
qsi = QuadraticSplineInterpolation(x,y,x_value)
print(qsi$func_per_interval)
print(qsi$correct_func)
