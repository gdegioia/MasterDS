# MASTER IN DATA SCIENCE FOR ECONOMICS, BUSINESS, AND FINANCE -- A.A. 2022/23
# Assignment in Economic and Financial Data Science: modulo 1
# Gruppo 2 - Studenti: Luca Garbin, Laura Proto, Andrea Mentasti, Giacomo de Gioia

source("Gruppo2_assignment_funzioni.R")

sottostante <- 70
strike <- 75

long_call_payoff(S=sottostante, K=strike)

sottostante <- 80
strike <- 75

long_call_payoff(S=sottostante, K=strike)

#----------------------------------------
range_min <-70
range_max <- 130
#step <- 0.1
range <- seq(range_min, range_max, by=1) 
sottostante <- range #vettore di valori del prezzo dell'asset sottostante
strike_1 <- 90 #strike price della prima call option 
strike_2 <- 110 #strike price della seconda call option 
long_call_1 <- 2 #posizioni lunghe nella prima call option (strike K1)
long_call_2 <- 2 #posizioni lunghe nella seconda call option (strike K2)

payoff <- range # inizializzazione
for (s in range){
  payoff[s-range_min+1] <- butterfly_payoff(S=s, K1=strike_1, K2=strike_2, J=long_call_1, H=long_call_2)
}

plot(x=sottostante, y=payoff, type = "l", xlab = "Prezzo sottostante", ylab = "Payoff", 
     col = "red", lwd = 3, main = "Butterfly Payoff")


#***********************************************************************************************#
