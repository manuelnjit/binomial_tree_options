binomial.tree <- function(option1, option2, method, initial, strike, rf, cdiv, vol, t, n){
  " The function binomial.tree is one that takes in all of the following 
  parameters and calculates the stock prices and option values for 
  a european or american call or put. The parameters work in the following 
  fashion. 
  option1: American or European
  option2: put or call 
  method: log (lognormal), CCR (Cox, Ross, Rubenstein), binomial (Hull/White)
  initial: initial stock price
  strike: strike price of the option 
  rf: the risk free interest rate. Project fixed at 4%
  cdiv: continuous dividends. Project fixed at 0% 
  vol: the volatility or sigma. Project fixed at 30%
  t: delta t or 1 period
  n: number of steps

  "
  #--------------------------------------------------------------------------------
  
  CRR <- function(vol, t, rf, cdiv){
    # The function CRR takes in the same parameters from binomial.tree
    # calculates u, d and p according to Cox, Ross, Rubenstein model. 
    
    CRR.values <- rep(0,3) # creating an array of length three made of all zeros
                           # to place the values u, d and p respectively    
    
    CRR.values[1] <- exp(vol*sqrt(t)) # CRR u formula assigned to index 1 of CRR.values
    CRR.values[2] <- 1/CRR.values[1]  # CRR d formula assigned to index 2 of CRR.values
    CRR.values[3] <- (exp((rf-cdiv)*t)-CRR.values[2])/(CRR.values[1]-CRR.values[2])
                                      # p value of CRR assigned to index 3 of CRR.values
    return(CRR.values)                # returns list of values
     
  }
  
  binomial <- function(vol, t, rf, cdiv){
    # The function binomial takes in the same parameters from binomial.tree
    # calculates u, d and p according to the binomial model (Hull/White)
    
    bi.values <- rep(0,3) # creating an array of length three made of all zeros
    # to place the values u, d and p respectively
    
    bi.values[1] <- exp((rf - cdiv)*t+vol*sqrt(t)) # binomial u formula assigned
                                                   # to index 1 of bi.values
    bi.values[2] <- exp((rf - cdiv)*t-vol*sqrt(t)) # binomial d formula assigned
                                                   # to index 2 of bi.values
    bi.values[3] <- (exp((rf-cdiv)*t)-bi.values[2])/(bi.values[1]-bi.values[2])
                                                   # p value of binomial model 
                                                   # assigned to index 3 of bi.values
    return(bi.values)                              # returns list of values
    
  }
  
  lognormal <- function(vol, t, rf, cdiv){
    # The function lognormal takes in the same parameters from binomial.tree
    # calculates u, d and p according to the lognormal model 
    
    log.values <- rep(0,3) # creating an array of length three made of all zeros
    # to place the values u, d and p respectively
    
    log.values[1] <- exp((rf-cdiv-(.5*vol*vol))*t+(vol*sqrt(t))) # lognormal u 
    log.values[2] <- exp((rf-cdiv-(.5*vol*vol))*t-(vol*sqrt(t))) # lognormal d 
    log.values[3] <- (exp((rf-cdiv)*t)-log.values[2])/(log.values[1]-log.values[2])
                                                                 # lognormal p 
    return(log.values)                                           # returns values
  }
    " the following if else statement finds our u, d and p according to 
    the parameter introduced in binomial.tree . According to what option2 is 
    the corresponding function will be called to find the appropriate values 
    which are then assigned to a.v (actual values).
    "
  if(method == "CRR"){
    a.v <- CRR(vol, t, rf, cdiv)
    u <- a.v[1]
    d <- a.v[2]
    p <- a.v[3]
  } else if(method == "binomial"){
    a.v <- binomial(vol, t, rf, cdiv)
    u <- a.v[1]
    d <- a.v[2]
    p <- a.v[3]
  } else if(method == "log"){
    a.v <- lognormal(vol, t, rf, cdiv)
    u <- a.v[1]
    d <- a.v[2]
    p <- a.v[3]
  }
  
  
  stockprice_creator <- function(initial, u, d, n) {
  " The function stockprice_creator always runs; it calculates the binomial 
  tree of the stock prices for n steps. It takes in the following parameters
  initial: the initial stock price
  u: the growth multiple. Given by the lognormal, CRR or binomial 
  d: the decrease multiple. Given by the lognormal, CRR or binomial. 
  n: the number of steps. Same parameter as the n given in binomial tree
  function. 
  
  Returns a list of all stock prices for n steps. 
  "
    X <- rep(0, sum(1:(n+1))) # Creating an array that will fit the number
                              # of stock prices for an n step binomial tree
    
    X[1] <- initial           # assigning the first value of the array 
                              # the initial stock price value
    index <- 2                # this is a counter to keep track of the indexed
                              # value in array. Starting at 2. 
    for (i in 1:n) {
      for (j in 0:i) {
        X[index] <- initial * u^j * d^(i-j)
        index <- index + 1
      }
    }
    "The double for loop solves for stock prices up to the nth step
    example: index 1 is the initial stock price
    index 2 is the initial stock price * u
    index 3 is the initial stock price *d (let initial stock price be So)
    index 4 is So*u^2
    index 5 is So*u*d
    index 6 is so*d^2 and so on. 
    "
    return(X)                 # returns value X which contains list of 
                              # stock prices for any equity at some initial 
                              # price. 
  }
  
  stockprice.list <- stockprice_creator(initial, u, d, n)
  
  
  Premium_finder <- function(option2, stockprice.list, strike, n, p, rf, cdiv){
  " The function Premium_finder calculates the payoff of the furthermost nodes
  and then evaluates each option price at every node found. Premium finder takes 
  in the same parameters as binomial.tree with the exception of stockprice.list
  "
    # The following if statement determines how the function payoff.initial 
    # will work. If option2 is call then the payoff.initial function will be 
    # the pay off of a call otherwise it will be set to the pay off of a put
    if(option2 == "call"){
      payoff.initial <- function(strike, stockprice){
        payoff <- max(0, stockprice - strike)
        return(payoff)
      }}else{
        payoff.initial <- function(strike, stockprice){
          payoff <- max(0, strike - stockprice)
          return(payoff)
        }
      }
    
    payoff.array <- rep(0, sum(1:(n+1))) 
    # payoff.array is a variable created to hold all payoff values according 
    # to a tree with n steps. 
    
    for(i in length(stockprice.list):(length(stockprice.list)-n)){
      # from the uppermost node to the lowermost node at the nth step
      # We are interested in calculating the payoff of a call or a put
      payoff.array[i] <- payoff.initial(strike, stockprice.list[i])
      # note that payoff.initial is a function that is predetermined to solve 
      # for these payoffs as a call or put according to input parameter
    }
    
    tracker <- length(stockprice.list)-n-1
    # tracker will be track of our indexed element in 
    # the array 
    j <- length(stockprice.list)
    # j will play the role of tracking the payoff element
    # which is required to find valuation of option at 
    # n - i step for i = 1,2, ..., n
    adjuster <- n
    # adjuster keeps track of when we should jump an 
    # indexed element to ensure payoff function of 
    # present value of P*SoU + (1-P)*SoD
    adjuster.helper <- 1
    # works with adjuster 
    if(option1 == "European"){
      payoff.initial <- function(strike, stockprice){
        payoff <- 0
        return(payoff)
      
    }}
    for(i in tracker:1){
      payoff.array[i] <- max(payoff.initial(strike, stockprice.list[i]),(exp(-rf*cdiv))*(p*payoff.array[j]+((1-p)*payoff.array[j-1])))
      j <- j-1
      if(j == 0){
        break
      }
      
      if(adjuster.helper == adjuster){
        j <- j-1
        adjuster.helper <- 0
        adjuster <- adjuster-1
      }
      adjuster.helper <- adjuster.helper + 1
    }
    return(payoff.array)
  }
  premium.list <- Premium_finder(option2, stockprice.list, strike, n, p, rf, cdiv)
  df <- cbind(stockprice.list, premium.list)
  return(df)
}

library(DiagrammeR)

MS.Amer.Put.A <- binomial.tree("American", "put", "CRR", 55.03, 55.03, .04, 0, .3, (3/12), 5)
MS.Amer.Put.I <- binomial.tree("American", "put", "CRR", 55.03, 60, .04, 0, .3, (3/12), 5)
MS.Euro.Call.I <- binomial.tree("European", "call", "CRR", 55.03, 50, .04, 0, .3, (3/12), 5)
MS.Euro.Call.A <- binomial.tree("European", "put", "CRR", 55.03, 55.03, .04, 0, .3, (3/12), 5)

DB.Euro.Call.I <- binomial.tree("European", "call", "CRR", 9.53, 5, .04, 0, .3, (3/12), 5)
DB.Euro.Call.A <- binomial.tree("European", "call", "CRR", 9.53, 9.53, .04, 0, .3, (3/12), 5)
DB.Amer.Put.I <- binomial.tree("American", "put", "CRR", 9.53, 15, .04, 0, .3, (3/12), 5)
DB.Amer.Put.A <- binomial.tree("American", "put", "CRR", 9.53, 9.53, .04, 0, .3, (3/12), 5)

grViz("
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]

  # several 'node' statements
  node [shape = box,
        fontname = Helvetica]
  A[label = '@@1']; 
  B[label = '@@2-1']; C[label = '@@2-2'];
  D[label = '@@3-1']; E[label = '@@3-2']; F[label = '@@3-3'];
  G[label = '@@4-1']; H[label = '@@4-2']; I[label = '@@4-3']; 
  J[label = '@@4-4']; K[label = '@@5-1']; L[label = '@@5-2'];
  M[label = '@@5-3']; N[label = '@@5-4']; O[label = '@@5-5'];

  node [shape = oval] // sets as oval
  P[label = '@@6-1']; Q[label = '@@6-2']; R[label = '@@6-3'];
  S[label = '@@6-4']; T[label = '@@6-5']; U[label = '@@6-6'];

  # several 'edge' statements
  A->B A->C B->D B->E C->E
  C->F D->G D->H E->H E->I 
  F->I F->J G->K G->L H->L
  H->M I->M I->N J->N J->O
  K->P K->Q L->Q L->R M->R
  M->S N->S N->T O->T O->U
}
[1]: paste0(MS.Euro.Call.I[1],'\\n', MS.Euro.Call.I[22])
[2]: paste0(MS.Euro.Call.I[2:3],'\\n', MS.Euro.Call.I[23:24])
[3]: paste0(MS.Euro.Call.I[4:6],'\\n', MS.Euro.Call.I[25:27])
[4]: paste0(MS.Euro.Call.I[7:10],'\\n', MS.Euro.Call.I[28:31])
[5]: paste0(MS.Euro.Call.I[11:15],'\\n', MS.Euro.Call.I[32:36])
[6]: paste0(MS.Euro.Call.I[16:21],'\\n', MS.Euro.Call.I[37:42])

")

MS.Euro.Call.I.5 <- binomial.tree("European", "call", "CRR", 55.03, 50, .04, 0, .3, (3/12), 5)
MS.Amer.Call.I.5 <- binomial.tree("American", "call", "CRR", 55.03, 50, .04, 0, .3, (3/12), 5)
MS.Euro.Put.I.5 <- binomial.tree("European", "put", "CRR", 55.03, 60, .04, 0, .3, (3/12), 5)
MS.Amer.Put.I.5 <- binomial.tree("American", "put", "CRR", 55.03, 60, .04, 0, .3, (3/12), 5)

MS.cbind.Call.I.5 <- cbind(MS.Euro.Call.I.5, MS.Amer.Call.I.5)
MS.cbind.Put.I.5 <- cbind(MS.Euro.Put.I.5, MS.Amer.Put.I.5)

MS.Euro.Call.A.5 <- binomial.tree("European", "call", "CRR", 55.03, 55.03, .04, 0, .3, (3/12), 5)
MS.Amer.Call.A.5 <- binomial.tree("American", "call", "CRR", 55.03, 55.03, .04, 0, .3, (3/12), 5)
MS.Euro.Put.A.5 <- binomial.tree("European", "put", "CRR", 55.03, 55.03, .04, 0, .3, (3/12), 5)
MS.Amer.Put.A.5 <- binomial.tree("American", "put", "CRR", 55.03, 55.03, .04, 0, .3, (3/12), 5)

MS.cbind.Call.A.5 <- cbind(MS.Euro.Call.A.5, MS.Amer.Call.A.5)
MS.cbind.Put.A.5 <- cbind(MS.Euro.Put.A.5, MS.Amer.Put.A.5)

MS.cbind.Call.I.5[1,1:4]
MS.cbind.Call.A.5[1,1:4]
MS.cbind.Put.I.5[1,1:4]
MS.cbind.Put.A.5[1,1:4]

MS.Euro.Call.I.30 <- binomial.tree("European", "call", "CRR", 55.03, 50, .04, 0, .3, (1/12), 30)
MS.Amer.Call.I.30 <- binomial.tree("American", "call", "CRR", 55.03, 50, .04, 0, .3, (1/12), 30)
MS.Euro.Put.I.30 <- binomial.tree("European", "put", "CRR", 55.03, 60, .04, 0, .3, (1/12), 30)
MS.Amer.Put.I.30 <- binomial.tree("American", "put", "CRR", 55.03, 60, .04, 0, .3, (1/12), 30)

MS.cbind.Call.I.30 <- cbind(MS.Euro.Call.I.30, MS.Amer.Call.I.30)
MS.cbind.Put.I.30 <- cbind(MS.Euro.Put.I.30, MS.Amer.Put.I.30)

MS.Euro.Call.A.30 <- binomial.tree("European", "call", "CRR", 55.03, 55.03, .04, 0, .3, (1/12), 30)
MS.Amer.Call.A.30 <- binomial.tree("American", "call", "CRR", 55.03, 55.03, .04, 0, .3, (1/12), 30)
MS.Euro.Put.A.30 <- binomial.tree("European", "put", "CRR", 55.03, 55.03, .04, 0, .3, (1/12), 30)
MS.Amer.Put.A.30 <- binomial.tree("American", "put", "CRR", 55.03, 55.03, .04, 0, .3, (1/12), 30)

MS.cbind.Call.A.30 <- cbind(MS.Euro.Call.A.30, MS.Amer.Call.A.30)
MS.cbind.Put.A.30 <- cbind(MS.Euro.Put.A.30, MS.Amer.Put.A.30)

MS.Euro.Call.I.30.log <- binomial.tree("European", "call", "log", 55.03, 50, .04, 0, .3, (1/12), 30)
MS.Euro.Put.I.30.log <- binomial.tree("European", "put", "log", 55.03, 60, .04, 0, .3, (1/12), 30)

MS.Euro.Call.A.30.log <- binomial.tree("European", "call", "log", 55.03, 55.03, .04, 0, .3, (1/12), 30)
MS.Euro.Put.A.30.log <- binomial.tree("European", "put", "log", 55.03, 55.03, .04, 0, .3, (1/12), 30)

MS.Amer.Call.I.30.bi <- binomial.tree("American", "call", "binomial", 55.03, 50, .04, 0, .3, (1/12), 30)
MS.Amer.Put.I.30.bi <- binomial.tree("American", "put", "binomial", 55.03, 60, .04, 0, .3, (1/12), 30)

MS.Amer.Call.A.30.bi <- binomial.tree("American", "call", "binomial", 55.03, 55.03, .04, 0, .3, (1/12), 30)
MS.Amer.Put.A.30.bi <- binomial.tree("American", "put", "binomial", 55.03, 55.03, .04, 0, .3, (1/12), 30)

MS.cbind.Call.I.30[1,1:4]
MS.cbind.Call.A.30[1,1:4]
MS.cbind.Put.I.30[1,1:4]
MS.cbind.Put.A.30[1,1:4]
MS.Euro.Call.I.30.log[1,1:2]
MS.Euro.Call.A.30.log[1,1:2]
MS.Euro.Put.I.30.log[1,1:2]
MS.Euro.Put.A.30.log[1,1:2]
MS.Amer.Call.I.30.bi[1,1:2]
MS.Amer.Call.A.30.bi[1,1:2]
MS.Amer.Put.I.30.bi[1,1:2]
MS.Amer.Put.A.30.bi[1,1:2]

DB.Euro.Call.I.5 <- binomial.tree("European", "call", "CRR", 9.53, 5, .04, 0, .3, (3/12), 5)
DB.Euro.Call.A.5 <- binomial.tree("European", "call", "CRR", 9.53, 9.53, .04, 0, .3, (3/12), 5)
DB.Amer.Call.I.5 <- binomial.tree("American", "call", "CRR", 9.53, 5, .04, 0, .3, (3/12), 5)
DB.Amer.Call.A.5 <- binomial.tree("American", "call", "CRR", 9.53, 9.53, .04, 0, .3, (3/12), 5)

DB.Amer.Put.I.5 <- binomial.tree("American", "put", "CRR", 9.53, 15, .04, 0, .3, (3/12), 5)
DB.Amer.Put.A.5 <- binomial.tree("American", "put", "CRR", 9.53, 9.53, .04, 0, .3, (3/12), 5)
DB.Euro.Put.I.5 <- binomial.tree("European", "put", "CRR", 9.53, 15, .04, 0, .3, (3/12), 5)
DB.Euro.Put.A.5 <- binomial.tree("European", "put", "CRR", 9.53, 9.53, .04, 0, .3, (3/12), 5)

DB.Euro.Call.I.5[1,1:2]
DB.Euro.Call.A.5[1,1:2]
DB.Amer.Call.A.5[1,1:2]
DB.Amer.Call.I.5[1,1:2]

DB.Amer.Put.I.5[1,1:2]
DB.Amer.Put.A.5[1,1:2]
DB.Euro.Put.A.5[1,1:2]
DB.Euro.Put.I.5[1,1:2]

DB.Euro.Call.I.30 <- binomial.tree("European", "call", "CRR", 9.53, 5, .04, 0, .3, (1/12), 30)
DB.Amer.Call.I.30 <- binomial.tree("American", "call", "CRR", 9.53, 5, .04, 0, .3, (1/12), 30)
DB.Euro.Put.I.30 <- binomial.tree("European", "put", "CRR", 9.53, 15, .04, 0, .3, (1/12), 30)
DB.Amer.Put.I.30 <- binomial.tree("American", "put", "CRR", 9.53, 15, .04, 0, .3, (1/12), 30)

DB.cbind.Call.I.30 <- cbind(DB.Euro.Call.I.30, DB.Amer.Call.I.30)
DB.cbind.Put.I.30 <- cbind(DB.Euro.Put.I.30, DB.Amer.Put.I.30)

DB.Euro.Call.A.30 <- binomial.tree("European", "call", "CRR", 9.53, 9.53, .04, 0, .3, (1/12), 30)
DB.Amer.Call.A.30 <- binomial.tree("American", "call", "CRR", 9.53, 9.53, .04, 0, .3, (1/12), 30)
DB.Euro.Put.A.30 <- binomial.tree("European", "put", "CRR", 9.53, 9.53, .04, 0, .3, (1/12), 30)
DB.Amer.Put.A.30 <- binomial.tree("American", "put", "CRR", 9.53, 9.53, .04, 0, .3, (1/12), 30)

DB.cbind.Call.A.30 <- cbind(DB.Euro.Call.A.30, DB.Amer.Call.A.30)
DB.cbind.Put.A.30 <- cbind(DB.Euro.Put.A.30, DB.Amer.Put.A.30)

DB.Euro.Call.I.30.log <- binomial.tree("European", "call", "log", 9.53, 5, .04, 0, .3, (1/12), 30)
DB.Euro.Put.I.30.log <- binomial.tree("European", "put", "log", 9.53, 15, .04, 0, .3, (1/12), 30)

DB.Euro.Call.A.30.log <- binomial.tree("European", "call", "log", 9.53, 9.53, .04, 0, .3, (1/12), 30)
DB.Euro.Put.A.30.log <- binomial.tree("European", "put", "log", 9.53, 9.53, .04, 0, .3, (1/12), 30)

DB.Amer.Call.I.30.bi <- binomial.tree("American", "call", "binomial", 9.53, 5, .04, 0, .3, (1/12), 30)
DB.Amer.Put.I.30.bi <- binomial.tree("American", "put", "binomial", 9.53, 15, .04, 0, .3, (1/12), 30)

DB.Amer.Call.A.30.bi <- binomial.tree("American", "call", "binomial", 9.53, 9.53, .04, 0, .3, (1/12), 30)
DB.Amer.Put.A.30.bi <- binomial.tree("American", "put", "binomial", 9.53, 9.53, .04, 0, .3, (1/12), 30)

DB.cbind.Call.I.30[1,1:4]
DB.cbind.Call.A.30[1,1:4]
DB.cbind.Put.I.30[1,1:4]
DB.cbind.Put.A.30[1,1:4]
DB.Euro.Call.I.30.log[1,1:2]
DB.Euro.Call.A.30.log[1,1:2]
DB.Euro.Put.I.30.log[1,1:2]
DB.Euro.Put.A.30.log[1,1:2]
DB.Amer.Call.I.30.bi[1,1:2]
DB.Amer.Call.A.30.bi[1,1:2]
DB.Amer.Put.I.30.bi[1,1:2]
DB.Amer.Put.A.30.bi[1,1:2]

Final_test.MS.Euro.Put.I.5 <- binomial.tree("European", "put", "CRR", 29.37, 30, .04, 0, .6, (1/12), 6)
Final_test.MS.Amer.Put.I.5 <- binomial.tree("American", "put", "CRR", 29.37, 30, .04, 0, .6, (1/12), 6)
Final_test.MS.Euro.Put.I.5[1,1:2]
Final_test.MS.Amer.Put.I.5[1,1:2]

