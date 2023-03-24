# MASTER IN DATA SCIENCE FOR ECONOMICS, BUSINESS, AND FINANCE -- A.A. 2022/23
# Assignment in Economic and Financial Data Science: modulo 1
# Gruppo 2 - Studenti: Luca Garbin, Laura Proto, Andrea Mentasti, Giacomo de Gioia

library("fOptions")
library("quantmod")
getSymbols("GOOG") 
plot(GOOG)
plot(GOOG$GOOG.Close)
ClosePrice <- as.numeric(GOOG$GOOG.Close)
S_corrente <- tail(ClosePrice,1L) 
Strike <- 1.05*S_corrente 

logRendimenti<- diff(log(ClosePrice)) 
Sample2ndMom <- mean(logRendimenti^2)
Delta <- 1/360 
sigma <- sqrt(1/Delta*Sample2ndMom)

i <- 0 
r <- log(1+i) 

TimeToMat <- 60/360 
N_period <- 3 

Put <- CRRBinomialTreeOption(TypeFlag ="pe", S=S_corrente, X=Strike,
                      Time=TimeToMat, r=r, b=r, sigma=sigma, n=N_period)
Put@price

#class(Put)
#slotNames(Put)
#str(Put)

BS_eval <- GBSOption(TypeFlag ="p", S=S_corrente, X=Strike, Time=TimeToMat, 
                     r=r, b=r, sigma=sigma)
#slotNames(BS_eval)
#str(BS_eval)

BS_eval@price

