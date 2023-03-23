# MASTER IN DATA SCIENCE FOR ECONOMICS, BUSINESS, AND FINANCE -- A.A. 2022/23
# Assignment in Economic and Financial Data Science: modulo 1
# Gruppo 2 - Studenti: Luca Garbin, Laura Proto, Andrea Mentasti, Giacomo de Gioia

source("Gruppo2_assignment_funzioni.R")
source("Gruppo2_assignment_3.R")

N_max <- 30
Put_Bin <- rep(0, N_max)

for (N in c(1:N_max)){
  
  Put_Bin[N] <- Put_Binomial_eval(N_period=N)
  
}
Put_Bin
BS_eval <- rep(BS_eval@price, N_max)
BS_eval

plot(x=c(1:N_max), y=Put_Bin, type="l", col="red", lwd=3,
     xlab="Numero di periodi (fissata la durata residua)", ylab="Valutazione di una put (europea)",
     main="Convergenza del metodo Binomial Tree")
lines (BS_eval, type = "l", lty="dashed", lwd =3,col="blue")
legend(15,7.8,c("Binomial Tree", "Black and Scholes"), col=c("red","blue"),
       lty=c(1,3), title.font=2)

