#Author: Trixia Belleza 
#Section: B-5L CMSC 150
#input equations

#Constraints
E1 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 1 * x1 + 1 * x2 + 1 * x3 + 1 * x4 + 1 * x5 + 0 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + 0 * x10 + 0 * x11 + 0 * x12 + 0 * x13 + 0 * x14 + 0 * x15 - 310
E2 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 0 * x1 + 0 * x2 + 0 * x3 + 0 * x4 + 0 * x5 + 1 * x6 + 1 * x7 + 1 * x8 + 1 * x9 + 1 * x10 + 0 * x11 + 0 * x12 + 0 * x13 + 0 * x14 + 0 * x15 - 260
E3 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 0 * x1 + 0 * x2 + 0 * x3 + 0 * x4 + 0 * x5 + 0 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + 0 * x10 + 1 * x11 + 1 * x12 + 1 * x13 + 1 * x14 + 1 * x15 - 280
E4 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 1 * x1 + 0 * x2 + 0 * x3 + 0 * x4 + 0 * x5 + 1 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + 0 * x10 + 1 * x11 + 0 * x12 + 0 * x13 + 0 * x14 + 0 * x15 - 180
E5 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 0 * x1 + 1 * x2 + 0 * x3 + 0 * x4 + 0 * x5 + 0 * x6 + 1 * x7 + 0 * x8 + 0 * x9 + 0 * x10 + 0 * x11 + 1 * x12 + 0 * x13 + 0 * x14 + 0 * x15 - 80
E6 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 0 * x1 + 0 * x2 + 1 * x3 + 0 * x4 + 0 * x5 + 0 * x6 + 0 * x7 + 1 * x8 + 0 * x9 + 0 * x10 + 0 * x11 + 0 * x12 + 1 * x13 + 0 * x14 + 0 * x15 - 200
E7 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 0 * x1 + 0 * x2 + 0 * x3 + 1 * x4 + 0 * x5 + 0 * x6 + 0 * x7 + 0 * x8 + 1 * x9 + 0 * x10 + 0 * x11 + 0 * x12 + 0 * x13 + 1 * x14 + 0 * x15 - 160
E8 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 0 * x1 + 0 * x2 + 0 * x3 + 0 * x4 + 1 * x5 + 0 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + 1 * x10 + 0 * x11 + 0 * x12 + 0 * x13 + 0 * x14 + 1 * x15 - 220
E9 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) 10 * x1 + 8 * x2 + 6 * x3 + 5 * x4 + 4 * x5 + 6 * x6 + 5 * x7 + 4 * x8 + 3 * x9 + 6 * x10 + 3 * x11 + 4 * x12 + 5 * x13 + 5 * x14 + 9 * x15 - 1

  
#put them in a list
system <- list(E1, E2, E3, E4, E5, E6, E7, E8, E9);

#gets the vector of rownames by number of rows.
#returns -> c(1, 2, 3, ..., n)
getRowNames <- function(system) {
  rownames <- c()
  for (i in 1:length(system)) {
    rownames <- c(rownames, i)      
  }
  return(rownames)
}

#gets the vector of colnames by number of cols then appending an "x" with it.
#returns -> c("x1", "x2", "x3", ..., "RHS")
getColNames <- function(vars) {
  colnames <- c()
  for (i in 1:length(vars)) {
    #concat x and number i
    colname = paste("x",i, sep="")
    colnames <- c(colnames, colname)  
  }
  colnames <- c(colnames, "RHS") #adds RHS at the last index of the vector
  return(colnames)
}

#creates a list of terms per equation
listOfTerms <- function(system) {
  terms_list = c()
  for (i in 1:length(system)) {
    print(paste("system[i]: ", system[i]))
    #returns "0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4)"
    #   eq = paste(deparse(system[i])[2], deparse(system[i])[3], sep="")
    eq = deparse(system[i])[2]
    eq = substr(eq,1,nchar(eq)-1)  #remove unnecessary parenthesis at the last character.
    #returns "0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4"
    funcDef = deparse(system[[eq]])[2]  #from E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4; 
    print(funcDef)
    #missing_part = substr(deparse(system[[eq]])[2], 5, 20)
    #funcDef = paste(funcDef, missing_part, sep = "")     #it returns "function (x1, x2, x3) "
    
    
    eq_split = strsplit(eq, " \\+ ")  #split by " + "
    print(eq_split)
    terms_list <- c(terms_list, eq_split)   #returns "0.3 * x1"  "-0.2 * x2" "10 * x3"   "-71.4"
  }
  return(terms_list);
}

AugCoeffMatrix <- function(system){
  count=0 #keeps track kung pang-ilang row yung paglalagyan ng coefficient sa matrix
  unknown_count = 0
  shouldBeNA=0
  
  #get var count for each equation
  for(eq in 1:length(system)) {
    funcDef = deparse(system[[eq]])[1]  #from E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4; 
    missing_part = substr(deparse(system[[eq]])[2], 5, 20)
    funcDef = paste(funcDef, missing_part, sep = "")     #it returns "function (x1, x2, x3) "
    
    funcParams = substr(funcDef, 11, nchar(funcDef)-2) #from "function (x1, x2, x3) " it returns "x1, x2, x3"
    vars = strsplit(funcParams, "\\, ")  #split by comma + space 
    #from "x1, x2, x3" it returns "x1" "x2" "x3"
    
    #checks if unknown counts are all the same. Else, return NA
    if( (length(vars[[1]]) != unknown_count) & (unknown_count == 0) ) {
      unknown_count = length(vars[[1]])
    }
    #else if num of var for each equations != unknowns of all equation OR length of equations != num of unknowns
    else if((length(vars[[1]]) != unknown_count)) { 
      shouldBeNA = 1
      break
    }
  }
  
  if(shouldBeNA == 0) {
    #declare matrix
    augcoeffmatrix = matrix(data=0, nrow=length(system), ncol=length(vars[[1]])+1, dimnames=list(getRowNames(system), getColNames(vars[[1]])))
    for(j in 1:length(listOfTerms(system))){
      sorted_terms <- c()
      term = listOfTerms(system)[[j]]
      #print(length(term))
      for(i in 1:length(term)) {
        #checks if there's an "x" in the term
        if(grepl("x", term[i])) {
          
          index_ng_paglalagyan = as.numeric(substring(term[i], nchar(term[i]))) #let's say terms[i] = 0.3 * x1 
          #gets the number after x, thus, from this example, it returns 1.
          sorted_terms[index_ng_paglalagyan] = term[i];
        }
        #if there's no "x", it's a b coefficient!
        else{ 
          bCoeff = term[i]
        }
      }
      sorted_terms <- c(sorted_terms, as.numeric(bCoeff)*(-1))
      count = count + 1
      
      for(k in 1:length(sorted_terms)) {
        coeff = strsplit(sorted_terms[k], "\\*")[[1]][1] #Gets the coefficient by splitting the term by "*" then getting the first index of the split array
        augcoeffmatrix[count,k] = as.numeric(coeff)
      }
    }
    
    return(list(variables = vars[[1]], augcoeffmatrix = augcoeffmatrix))
  }
  else {
    return(NA)
  }
}
result = AugCoeffMatrix(system)
#print(result)

#print("result$augcoeffmatrix")
#print(result$augcoeffmatrix)

#THANK GOD IT FINALLY WORKS (i think.........)
