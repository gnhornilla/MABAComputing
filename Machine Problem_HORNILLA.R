#Noel Gabriel Hornilla
#Machine Problem

###################################################################

# Define an R function that computes the factorial of given an integer argument. 
#The output should be a vector of length 1.

factorial_function <- function (x) {
  fact = 1
  if (x != floor(x)) {return("Please enter and integer")} #Technically factorial is not defined for decimals (w/o using advanced math)
  else if (x<0) {return("Can't be negative")}
  else if(x==0) {return(1)}
  else {
      while(x>0){
      fact = fact * x
      x = x -1
      }
    return(fact)
  }
}
###################################################################

#Define an R function that removes NA values from a vector.

#Version 1: Check each element of the vector and remove NA values
remove_na <- function (x) {
  c=1
  while (c<=length(x)){
    if (is.na(x[c])) {x = x[-c]}
    c=c+1
  }
  return (x)
}

#Version 2: Use subsets
remove_na2 <- function(x){x[!is.na(x)]}


###################################################################
#Define an R function that sorts a given vector in decreasing order. 
#The output should be a vector of the same length. It should accept both numeric or character vectors.
#NOTE: Numeric vectors will be sorted from largest to smallest, character vectors will be sorted from 0 to 9 then Z to A with NAs in the last


#Version 1: Using exsisting sort function in R
sort_vector <- function (x) {
  x = rev(sort(x, na.last = FALSE))
  x
}

#Version 2: Without using existing sort function
sort_vector2 <- function (x) {
  counter = 1
  vector.sorted = NULL
  
  if(class(x)=="numeric"){
    x.order = order(-x)
    
    while(counter <= length(x)) {
      vector.sorted = c(vector.sorted, x[x.order[counter]])
      counter = counter + 1
    }
    
  }
  
  else if(class(x)=="character") {
    #Remove NA values (if any) first because they affect the order of character vectors sorted reverse alphabetically
    x.no_na = x[!is.na(x)]
    x.no_na.order = rev(order(x.no_na))
    
    while(counter <= length(x.no_na)){
      vector.sorted = c(vector.sorted, x.no_na[x.no_na.order[counter]])
      counter = counter + 1
    }
    
    #Need to add back NA values (if any) so that length of output vector is the same as the original vector
    count.NA = length(x) - length(x.no_na)
    
    while(count.NA > 0){
      vector.sorted = c(vector.sorted, NA)
      count.NA = count.NA - 1
    }
  }
  
  return(vector.sorted)
}

###################################################################
#Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) 
#depending whether the integer is a prime number or not.

isPrime = function (n) {
  if (n !=round(n)) {return("Please enter an integer")} #Decimals can't be prime
  else if(n<=0) {return(FALSE)} #0 and negative numbers can't be prime
  else if (sum(n%%c(1:n)==0) > 2) {return(FALSE)}
  else {return(TRUE)}
}

###################################################################
#Create a function that accepts a vector and and integer n and returns nth highest number

#Sort the vector from greatest to lowest then get the nth digit. By default, n=1
nthHighest = function (x,n=1){
  x = rev(sort(x, na.last = FALSE))
  x[n]
}

###################################################################
#Netpay problem

netpay = function (basic.monthly, allowance.nontax = 0, allowance.tax = 0) {
  annual.taxable = (basic.monthly + allowance.tax) * 12
  if (annual.taxable <= 250000) {tax = 0}
  else if (annual.taxable <= 400000) {tax = (annual.taxable - 250000) * 0.2} 
  else if (annual.taxable <= 800000) {tax = 30000 + (annual.taxable - 400000) * 0.25}
  else if (annual.taxable <= 2000000) {tax = 130000 + (annual.taxable - 800000) * 0.3}
  else if (annual.taxable <= 8000000) {tax = 490000 + (annual.taxable - 2000000) *0.32}
  else {tax = 2410000 + (annual.taxable - 8000000) * 0.35}
  
  net.monthly = basic.monthly + allowance.nontax + allowance.tax - (tax/12)
  
  return(net.monthly)
  }
