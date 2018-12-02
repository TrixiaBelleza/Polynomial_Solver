this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("belleza_exer3.r")

#Import matrix from exer 3
#result = getACM()
#acm = result$augcoeffmatrix
#vars = result$variables
#varCount = length(vars)

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

GaussJordan <- function(acm, varCount) {
  for(i in 1:varCount) {
    if(i != varCount) {
      #Find pivot row
      max_in_col_i = max(abs(acm[i:varCount,i]))
      pivot = getPivotRow(acm, i, varCount, max_in_col_i)
      
      if(pivot$row[i] == 0) {
        #no unique solution
        return(NA)
      }
      else {
        #Swap
        #do partial pivoting : swap(PIVOT_ROW, acm[i,i])
        temp = acm[i,]
        acm[i,] = pivot$row
        acm[pivot$index, ] = temp
      }
    }
    acm[i, ] = acm[i, ] / acm[i,i]
    for(j in 1:varCount){
      if(i==j) {
        next
      }
      normalized_row = acm[j,i] * acm[i,]
    #  print(normalized_row)
      acm[j,] = acm[j,] - normalized_row
    }
  }
  #gets the solution set or the RHS
  x <- c()
  for(i in 1:varCount) {
    x <- c(x, acm[i, varCount+1])
  }
  return(list(solutionSet = x, variables = vars, matrix = acm))     
} 

#print("Gaussian: ")
#GaussianResult = Gaussian(acm, varCount)
#print(Gaussian(acm, varCount))
#print("Gauss-Jordan: ")
#print(GaussJordan(acm, varCount))