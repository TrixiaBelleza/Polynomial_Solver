getPivotCol <- function(acm) {
  max_magnitude = -1
  for (i in 1:(ncol(acm)-1)) {
    #check if negative & check if abs is greater than max_magnitude
    if((acm[nrow(acm), i] < 0) & (abs(acm[nrow(acm), i]) > max_magnitude)){
      max_magnitude = abs(acm[nrow(acm), i])
    }
  }
  for (i in 1:(ncol(acm)-1)) {
    if(abs(acm[nrow(acm), i]) == max_magnitude) {
      return(i)
    }
  }
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

hasNegative <- function(list_of_values) {
  for (i in 1:(length(list_of_values)-1)) {
    if(list_of_values[i] < 0) {
      
      return(TRUE)
    }
  }
  return(FALSE)
}

getLeftmostPositiveCol <- function(acm, curr_row, curr_col) {
  for(col in 1:curr_col) {
    if(acm[curr_row, col] > 0) {
      return(col)
    }
  }
}

isMatrixFeasible <- function(acm) {
  zero_count = 0
  neg_count = 0
  for(col in 4:ncol(acm)) {
    zero_count = 0
    neg_count = 0
    for(row in 1:nrow(acm)) {
      if(acm[row,col] == 0) {
        zero_count = zero_count + 1
      }
      if(acm[row,col] == -1) {
        neg_count = neg_count + 1
      }
    }
    
    if((zero_count == (nrow(acm)-1)) & (neg_count == 1)) {
      return(FALSE)
    }
  }
 
  return(TRUE)
  
}

findIndexNotFeasible <- function(acm) {
  negative_activate_var_count = 0
  col_index = 0
  row_index = 0
  for(col in 4:ncol(acm)) {
    for(row in 1:nrow(acm)) {
      if(acm[row,col] == -1) {
        negative_activate_var_count = negative_activate_var_count + 1
        col_index = col
        row_index = row
      }
    }
  }
  if(negative_activate_var_count == 1) {
    return(list(col_index = col_index, row_index = row_index))
  }
}
#acm = matrix(data=0, nrow=9, ncol=16)
acm = matrix(data=0, nrow=3, ncol=7)

acm[1,] = c(2,3,6,1,0,0,60)
acm[2,] = c(1,4,5,0,-1,0,40)
acm[3,] = c(3,2,3,0,0,1,0)
print("INITIAL TABLEU")
print(acm)

###### MAKING THE MINIMIZATION PROBLEM FEASIBLE FOR MAXIMIZATION ##########

#STEP 1 : Check columns starting from the slack variable column (num of constraints + 1)  
#if each row of the column has negative

#STEP 2: GAUSS JORDAN

is_matrix_feasible = isMatrixFeasible(acm)
while(is_matrix_feasible == FALSE) {
  #find the column not feasible
  not_feasible_index = findIndexNotFeasible(acm)
  not_feasible_col_index = not_feasible_index$col_index
 
  pivot_col_index = getLeftmostPositiveCol(acm,not_feasible_index$row_index, col)

  #get pivot row and element
  pivot = computeTestRatio(acm, pivot_col_index)
  pivot_row_index = pivot$row
  pivot_element = pivot$element
  #print(pivot_element)
  #GAUSS-JORDAN
  acm = gaussJordan(acm, pivot_row_index, pivot_col_index, pivot_element)
  print("new acm")
  print(acm)
  is_matrix_feasible = isMatrixFeasible(acm)
}

#STEP 3 : MAXIMIZATION SIMPLEX


#check if last row of acm has negative values
has_negative = hasNegative(acm[nrow(acm),])
count = 1
while(has_negative == TRUE) {
  print(paste("----ITERATION ", count, "-----"))
  print("acm in iter")
  print(acm)
  #GET PIVOT COL, PIVOT ROW, PIVOT ELEMENT
  pivot_col_index = getPivotCol(acm)
  print(pivot_col_index)
  pivot = computeTestRatio(acm, pivot_col_index)
  pivot_row_index = pivot$row
  pivot_element = pivot$element
  
  #GAUSS-JORDAN
  acm = gaussJordan(acm, pivot_row_index, pivot_col_index, pivot_element)
  print("new acm")
  print(acm)
  #check if last row of acm has negative values
  has_negative = hasNegative(acm[nrow(acm),])
  count = count + 1
}

#Get solution set
sol_set <- c()

for(col in 1:4){ #3 is the last col before slack var
  zero_count = 0
  one_count = 0
  row_index = 0
  if(col == 4) {
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
print(sol_set)


