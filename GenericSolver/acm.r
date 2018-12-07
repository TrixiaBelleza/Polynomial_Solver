
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
print(acm)















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




































































































