#getPivotCol <- function(acm) {
 # max_in_lastrow = max(abs(acm[nrow(acm), 1:(ncol(acm)-1)]))
  
  #for (i in 1:(ncol(acm)-1)) {
   # if(abs(acm[nrow(acm), i]) == max_in_lastrow) {
    #  return(i)
    #}
  #}
#}

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
acm = matrix(data=0, nrow=5, ncol=8)
acm[1,1] = 7
acm[1,2] = 11
acm[1,3] = 1
for(i in 4:7){
  acm[1,i] = 0
}
acm[1,8] = 77

acm[2,1] = 10
acm[2,2] = 8
acm[2,3] = 0
acm[2,4] = 1
for(i in 5:7){
  acm[2,i] = 0
}
acm[2,8] = 80

acm[3,1] = 1
acm[3,2] = 0
acm[3,3] = 0
acm[3,4] = 0
acm[3,5] = 1
acm[3,6] = 0
acm[3,7] = 0
acm[3,8] = 9

acm[4,1] = 0
acm[4,2] = 1
acm[4,3] = 0
acm[4,4] = 0
acm[4,5] = 0
acm[4,6] = 1
acm[4,7] = 0
acm[4,8] = 6

acm[5,] = c(-150,-175,0,0,0,0,1,0)

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