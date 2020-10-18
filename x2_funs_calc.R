#finding roots for a quadratic function

#quadratic function form: f(x) = a*x^2 + b*x + c

#function to calculate roots of function:
quadr <- function(a,b,c){
  if(delta(a,b,c) > 0){                 #delta > 0 
    x1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    return( list(x1 = x1, x2 = x2) )
  }
  else if(delta(a,b,c) == 0){           #delta = 0
    x0 = -b/(2*a)
    return( list(x0 = x0) )
  }
  else {                                #delta < 0
    print("There are no real roots.")
  } # third case D<0
}

# function to calculate delta:
delta<-function(a,b,c){
  b^2-4*a*c
}

#Examples:
quadr(1,-3,2)   #case 1 - delta > 0 
quadr(2,-4,2)   #case 2 - delta = 0 
quadr(2,5,6)    #case 3 - delta < 0 

