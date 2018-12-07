getPivotCol <- function(acm) {
  max_magnitude = -1
  for (i in 1:(ncol(acm)-1)) {
    #check if negative & check if abs is greater than max_magnitude
    if((acm[nrow(acm), i] < 0) & (abs(acm[nrow(acm), i]) > max_magnitude)){
      max_magnitude = acm[nrow(acm), i] 
    }
  }
  for (i in 1:(ncol(acm)-1)) {
    if(acm[nrow(acm), i] == max_magnitude) {
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

hasNegative <- function(row) {
  for (i in 1:length(row)) {
    if(row[i] < 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}
acm = matrix(data=0, nrow=9, ncol=16)

#constraint 1
row_count = 1
for (i in 1:(ncol(acm)-1)) {
  if (i < 6) {
    acm[row_count, i] = 1
  }
  else if(i > 5 & i < 10) {
    acm[row_count+1, i] = 1
  }
  else{
    acm[row_count+2, i] = 1
  }
}

#constraint 2
col_count = 1
for (i in 4:(nrow(acm)-1)){
  acm[i, col_count] = 1
  acm[i, col_count+5] = 1
  acm[i, col_count+10] = 1
  col_count = col_count + 1
}

#objective function
acm[nrow(acm), 1] = 10
acm[nrow(acm), 2] = 8
acm[nrow(acm), 3] = 6
acm[nrow(acm), 4] = 5

acm[nrow(acm), 5] = 4
acm[nrow(acm), 6] = 6
acm[nrow(acm), 7] = 5
acm[nrow(acm), 8] = 4

acm[nrow(acm), 9] = 3
acm[nrow(acm), 10] = 6
acm[nrow(acm), 11] = 3
acm[nrow(acm), 12] = 4

acm[nrow(acm), 13] = 5
acm[nrow(acm), 14] = 5
acm[nrow(acm), 15] = 9
acm[nrow(acm), 16] = 1

#RHS
acm[1, ncol(acm)] = 310
acm[2, ncol(acm)] = 260
acm[3, ncol(acm)] = 280

acm[4, ncol(acm)] = 180
acm[5, ncol(acm)] = 80
acm[6, ncol(acm)] = 200

acm[7, ncol(acm)] = 160
acm[8, ncol(acm)] = 200

acm = t(acm)
print(acm)

#starting Column 9 to Column 16 - Slack variable

acm[16,] = acm[16,] * -1

row_count = 1
for (i in 8:15){
  acm = cbind(acm[,1:i], c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), acm[,i+1]) #insert column between col 8 and 9 
  acm[row_count, i+1] = 1
  row_count = row_count + 1
}
acm[16,16] = 1
acm[16,17] = 0

print("INITIAL TABLEU")
print(acm)
#check if last row of acm has negative values
has_negative = hasNegative(acm[nrow(acm),])
count = 1

while(has_negative == TRUE) {
  print(paste("----ITERATION ", count, "-----"))
  #print("acm in iter")
  #print(acm)
  #GET PIVOT COL, PIVOT ROW, PIVOT ELEMENT
  pivot_col_index = getPivotCol(acm)
  pivot = computeTestRatio(acm, pivot_col_index)
  pivot_row_index = pivot$row
  pivot_element = pivot$element
  
  #GAUSS-JORDAN
  acm = gaussJordan(acm, pivot_row_index, pivot_col_index, pivot_element)
  #print("new acm")
  print(acm)
  #check if last row of acm has negative values
  has_negative = hasNegative(acm[nrow(acm),])
  count = count + 1
}





#is_matrix_feasible = isMatrixFeasible(acm)
#while(is_matrix_feasible == FALSE) {
  #for(col in 4:ncol(acm)) {
    #is_column_feasible = isColumnFeasible(acm[,col]) 
    #if(is_column_feasible$bool == FALSE)
      #Get the row where it is negative then get the leftmost positive column
    #  pivot_col_index = getLeftmostPositiveCol(acm,is_column_feasible$row, col)
    #STEP 2 GAUSS JORDAN
    #Get pivot element and row
    #pivot = computeTestRatio(acm, pivot_col_index)
    #pivot_row_index = pivot$row
    #pivot_element = pivot$element
   # print(pivot_row_index)
    #GAUSS-JORDAN
  #  print(paste("piv col: ", pivot_col_index))
    #acm = gaussJordan(acm, pivot_row_index, pivot_col_index, pivot_element)
    #print("new acm")
    #print(acm)
  }
  
 # is_matrix_feasible = isMatrixFeasible(acm)
#}
