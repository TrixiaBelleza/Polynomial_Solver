hasNegative <- function(values) {
  for(i in 1:(length(values)-1)) {
    if(values[i] < 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}
#Returns the row with most negative number
mostNegativeRow <- function(values) {
  most_negative_value = 0
  most_negative_row = 0
  for (i in 1:(length(values)-1)) { #values-1 kasi di kasama yung last row
    
    if(values[i] < most_negative_value) {
      most_negative_value = values[i]
      most_negative_row = i
    }
  }
  return(most_negative_row)
}

getPhase1PivotColandElement <- function(acm,pivot_row) {
  largest_ratio = -1
  pivot_col_index = 0
  
  for(col in 1:(ncol(acm)-1)) {  #ncol(acm)-1 kasi hindi kasama si RHS
    if(acm[pivot_row, col] < 0) { #For each column with a negative entry in the pivot row
      ratio = acm[pivot_row, col] / acm[pivot_row, ncol(acm)]          #find the ratio using the right hand side column entry as denominator. RATIO = CurrIndex - RHS
      if(ratio > largest_ratio) {
        largest_ratio = ratio
        pivot_col_index = col
      }
    }
  } 

  pivot_element = acm[pivot_row, pivot_col_index]
  return(list(pivot_col = pivot_col_index, pivot_element = pivot_element))
}

getPivotCol <- function(acm) {
  max_magnitude = -10
  pivot_col = 0
  for (i in 1:(ncol(acm)-1)) {
    #check if negative & check if abs is greater than max_magnitude
    if((acm[nrow(acm), i] < 0) & (abs(acm[nrow(acm), i]) > max_magnitude)){
      max_magnitude = abs(acm[nrow(acm), i])
      pivot_col = i
    }
  }
  return(pivot_col)
}
computeTestRatio <- function(acm, pivot_column_index) {
  smallest_positive = 9999999999
  row = 0
  pivot_element = 0
  for(i in 1:(nrow(acm)-1)) {
    if(acm[i,pivot_column_index] != 0) {
      test_ratio = acm[i, ncol(acm)] / acm[i,pivot_column_index]
      if (test_ratio > 0 & test_ratio < smallest_positive) {
        smallest_positive = test_ratio
        row = i
        pivot_element = acm[i,pivot_column_index]
      }
    }
  }
  return(list(row = row, element = pivot_element))
}

getTemp <- function(acm, pivot_row_index, pivot_col_index, current_row) {
  temp <- c()
  for(col in 1:ncol(acm)) {
    temp_val = acm[current_row,pivot_col_index] * acm[pivot_row_index,col]
    temp <- c(temp, temp_val)
  }
  return(temp)
}

gaussJordan <- function(acm, pivot_row_index, pivot_col_index, pivot_element) {
  acm[pivot_row_index,] = acm[pivot_row_index,] / pivot_element     #PR = PR/PE
  
  #update each row, such that row = row - temp
  for(i in 1:nrow(acm)) {
    if(i != pivot_row_index) {
      temp = getTemp(acm, pivot_row_index, pivot_col_index, i)
      acm[i,] = acm[i,] - temp
    }
  }
  return(acm)
}

removeNegativeRHS <- function(acm){
  iterations <- list()
  has_negative = hasNegative(acm[,ncol(acm)]) 
  count = 1
  while(has_negative == TRUE) {
    iterations[[count]] <- acm 
    pivot_row = mostNegativeRow(acm[,ncol(acm)])
    pivot = getPhase1PivotColandElement(acm,pivot_row)
    acm = gaussJordan(acm, pivot_row, pivot$pivot_col, pivot$pivot_element)
    has_negative = hasNegative(acm[,ncol(acm)]) 
    count = count+1
  }
  
  return(list(acm = acm, iterations = iterations, count=count))
}

getSolutionSet <- function(acm) {
  #Get solution set
  sol_set <- c()
  
  for(col in 1:16){ #3 is the last col before slack var
    zero_count = 0
    one_count = 0
    row_index = 0
    if(col == 16) {
      col = ncol(acm) - 1  #get optimized value to sol_set
    }
    for(row in 1:nrow(acm)) {
      if(acm[row,col] == 1) {
        one_count = 1
        row_index = row
      }
      if(acm[row,col] == 0) {
        zero_count = zero_count + 1
      }
    }
    if(zero_count == (nrow(acm)-1) & one_count == 1) {
      sol_set <- c(sol_set, acm[row_index, ncol(acm)]) #get RHS of that row
    }
    else {
      sol_set <- c(sol_set, 0)
    }
  }
  sol_set[length(sol_set)] = sol_set[length(sol_set)] * -1 #change Z to positive since Z = -W
  return(sol_set)
}

Simplex <- function(acm) {
  iterations <- list()
  remove_negative_rhs =  removeNegativeRHS(acm)
  acm = remove_negative_rhs$acm
  iterations = remove_negative_rhs$iterations
  count = remove_negative_rhs$count
  #check if last row of acm has negative values
  has_negative = hasNegative(acm[nrow(acm),])
  while(has_negative == TRUE) {
    iterations[[count]] <- acm 
    #GET PIVOT COL, PIVOT ROW, PIVOT ELEMENT
    pivot_col_index = getPivotCol(acm)
    pivot = computeTestRatio(acm, pivot_col_index)
    pivot_row_index = pivot$row
    pivot_element = pivot$element
  
    #GAUSS-JORDAN
    acm = gaussJordan(acm, pivot_row_index, pivot_col_index, pivot_element)
    #check if last row of acm has negative values
    has_negative = hasNegative(acm[nrow(acm),])
    count = count + 1
  }
  
  sol_set = getSolutionSet(acm)
  return(list(sol_set = sol_set, iterations = iterations))
}

generateACM <- function(values, supply, demands) {
  #values is a list of values for objective function 
  #supply is a list of total supply for each plant
  #demands is a list of demand values for each warehouse
  acm = matrix(data=0, nrow=9, ncol=25)
  acm[1,] = c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,supply[1])
  acm[2,] = c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,supply[2])
  acm[3,] = c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,0,supply[3])
  acm[4,] = c(-1,0,0,0,0,-1,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0,0,0,(demands[1] * -1))
  acm[5,] = c(0,-1,0,0,0,0,-1,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0,0,(demands[2] * -1))
  acm[6,] = c(0,0,-1,0,0,0,0,-1,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0,(demands[3] * -1))
  acm[7,] = c(0,0,0,-1,0,0,0,0,-1,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,(demands[4] * -1))
  acm[8,] = c(0,0,0,0,-1,0,0,0,0,-1,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,(demands[5] * -1))
  acm[9,] = c(values[1],values[2],values[3],values[4],values[5],values[6],values[7],values[8],values[9],values[10],values[11],values[12],values[13],values[14],values[15],0,0,0,0,0,0,0,0,1,0)
  return(acm)  
}
#values =  c(10,8,6,5,4,6,5,4,3,6,3,4,5,5,9)
#supply = c(310,260,280)
#demand = c(180,80,200,160,220)
#acm = generateACM(values,supply,demand)
#print(acm)
#print("a")
#print("SIMPLEX!")
#simplex = Simplex(acm)
#iterations = simplex$iteration
#solution_set = simplex$sol_set

#print(iterations)