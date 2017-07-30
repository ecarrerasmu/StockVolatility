#JMJ
#Assignmet 11
#Created by: Ernesto Carrera
#

#install.packages("tseries")

library(tseries)



#The ticker for Bank of America is : BAC 
BAdata <- get.hist.quote('bac',quote="Close")

#Calculating the log returns
BAret <- log(lag(BAdata)) - log(BAdata)



BAvol <- sd(BAret) * sqrt(250) * 100



## volatility
get
Vol <- function(d, logrets)
{
  
  var = 0
  
  lam = 0
  
  varlist <- c()
  
  for (r in logrets) {
    
    lam = lam*(1 - 1/d) + 1
    
    var = (1 - 1/lam)*var + (1/lam)*r^2
    
    varlist <- c(varlist, var)
    
    
  }
  
  sqrt(varlist)
}


# Volatility with different decay values

volest <- Vol(10,BAret)



volest2 <- Vol(30,BAret)

volest3 <- Vol(100,BAret)

png(filename="BOFA.png")
plot(volest,type="l",main="Bank of America")

lines(volest2,type="l",col="red")

lines(volest3, type = "l", col="blue")




#Saving the plot in png format to be displayed in Github



dev.off()








