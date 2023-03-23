# MASTER IN DATA SCIENCE FOR ECONOMICS, BUSINESS, AND FINANCE -- A.A. 2022/23
# Assignment in Economic and Financial Data Science: modulo 1
# Gruppo 2 - Studenti: Luca Garbin, Laura Proto, Andrea Mentasti, Giacomo de Gioia

library("fOptions")
library("quantmod")
getSymbols("GOOG") #tramite la funzione getSymbols possiamo scaricare la serie storica dell'andamento dei prezzi di un'azione. In questo caso Google
#plot(GOOG)
#plot(GOOG$GOOG.Close)
ClosePrice <- as.numeric(GOOG$GOOG.Close)# otteniamo il prezzo corrente dell'azione di Google
S_corrente <- tail(ClosePrice,1L) #ultimo prezzo di chiusura
#calcoliamo la volatilità storica come deviazione standard dei rendimenti logaritmici
#il rendimento logaritmico giornaliero è il logaritmo naturale (ln) del rapporto tra il prezzo di chiusura e il prezzo di chiusura del giorno prima
Strike <- 1.05*S_corrente  #preziamo l'opzione in che money con il prezzo di esercizio, o strike price > del prezzo di chiusura del sottostante
logRendimenti<- diff(log(ClosePrice)) #applichiamo il logaritmo al prezzo di chiusura e tramite la funzione diff
#che restituisce restituisce la differenza tra elementi consecutivi, otteniamo i rendimenti logaritmici giornalieri
#per ottenere la volatilità dobbiamo calcolare il parametro σ (scarto quadratico medio) e lo eseguiamo calcolando la media
#della sommatoria dei nostri rendimenti logaritmici al quadrato (Momento secondo campionario)
#il tutto diviso per una frazione di tempo continuo (se consideriamo i rendimenti giornalieri sarà una frazione dell'annualità)
Sample2ndMom <- mean(logRendimenti^2)
Delta <- 1/360 #divido per 1/360 per calcolare l'annualità
sigma <- sqrt(1/Delta*Sample2ndMom)
i <- 0 # poniamo il tasso di interesse su base annua pari a 0
r <- log(1+i) #convertiamo il tasso di interesse in capitalizzazione continua trovato precedentemente in capitalizzazione composta esponenziale (1+r)^t 
# ora valutiamo il prezzo di un'opzione Put europea  utilizzando la funzione CRRBinomialTreeOption che all'interno della libreria Foption mi permette di calcolare il prezzo di un'opzione 
# la nostra opzione Put con strike 1.05∗S0 ed S0= al prezzo corrente
TimeToMat <- 60/360 # la scadenza dell'opzione è posta a 60 giorni
N_period <- 3 # numero di periodi che andiamo ad inserire nell'albero binomiale, in questo caso: 3
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

