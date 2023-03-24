# MASTER IN DATA SCIENCE FOR ECONOMICS, BUSINESS, AND FINANCE -- A.A. 2022/23
# Assignment in Economic and Financial Data Science: modulo 1
# Gruppo 2 - Studenti: Luca Garbin, Laura Proto, Andrea Mentasti, Giacomo de Gioia

source("Gruppo2_assignment_funzioni.R")

sottostante <- 100
strike <- 110
epoca_iniziale <- 0
scadenza <- 1
tasso_cap_comp <- 0.05
fatt_cap = (1+tasso_cap_comp)^(scadenza-epoca_iniziale)
rialzista <- 1.10*fatt_cap
ribassista <- 0.8*fatt_cap

res <- BM_Put(S0=sottostante, u=rialzista, d=ribassista, K=strike, TGrande=scadenza, 
              t0=epoca_iniziale, r=tasso_cap_comp)
                   
res$prezzo_put  # esempio numerico
res$quantita_risky # esempio numerico 
res$prob_risk_neutral # esempio numerico

#--------------
Call_price <- Callprice(Put=res$prezzo_put, S0=sottostante, K=strike, fatt_cap=fatt_cap)
Call_price # esempio numerico

sottostante
Call_price
max(sottostante-strike/fatt_cap, 0)

if ((sottostante >= Call_price)&(Call_price>=max(sottostante-strike/fatt_cap, 0))){
  print("Le condizioni di Merton sono verificate")
}

#--------------
