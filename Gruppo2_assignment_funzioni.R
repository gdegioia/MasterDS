# MASTER IN DATA SCIENCE FOR ECONOMICS, BUSINESS, AND FINANCE -- A.A. 2022/23
# Assignment in Economic and Financial Data Science: modulo 1
# Gruppo 2 - Studenti: Luca Garbin, Laura Proto, Andrea Mentasti, Giacomo de Gioia


long_call_payoff = function(S, K) {
  return(max(S - K, 0))
}

butterfly_payoff = function(S, K1, K2, J, H) {
  omega <- J/(J+H) # peso della prima call 
  KK <- omega*K1 + (1-omega)*K2 # strike medio (short call della strategia)
  return (J*long_call_payoff(S, K1) - (J+H)*long_call_payoff(S, KK)+ H*long_call_payoff(S, K2))
}

#----------------------

BM_Put <- function(S0, u, d, K, TGrande=1, t0=0, r=0){
  
  Time2Mat <- TGrande-t0
  
  P <- matrix(c((1+r)^(Time2Mat),(1+r)^(Time2Mat), S0*u, S0*d),2,2) 
  InverseP <- solve(P) 
  
  quantita_asset <- InverseP %*% matrix(c(max(c(K-S0*u,0)), max(c(K-S0*d,0))),2,1)
  y <- quantita_asset[2,1]
  qu <- ((1+r)^Time2Mat-d)/(u-d)
  
  #Putprice <- (1/(1+r)^Time2Mat)*(qu*max(c(K-S0*u,0))+(1-qu)*max(c(K-S0*d,0)))
  Putprice <- (1/(1+r)^Time2Mat)*(qu*max(K-S0*u,0)+(1-qu)*max(K-S0*d,0))
  
  return(list(prezzo_put=Putprice, quantita_risky=y, prob_risk_neutral=c(qu=qu, qd=1-qu)))
  
}

Callprice <- function(Put, S0, K, fatt_cap) Put + S0 - K / fatt_cap

#---------------------

library("fOptions")
library("quantmod")

Put_Binomial_eval <- function (N_period){
  getSymbols("GOOG") 
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
  #N_period <- 3 
  Put <- CRRBinomialTreeOption(TypeFlag ="pe", S=S_corrente, X=Strike,
                               Time=TimeToMat, r=r, b=r, sigma=sigma, n=N_period)
  return(Put@price)
}
