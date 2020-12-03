# bin_tree.R

## Table of contents
* [About](#about)
* [Example](#example)
* [Libraries](#libraries)
* [Setup](#setup)
* [Errors](#errors)
* [Ideas](#ideas)

## About
bin_tree.R calculates stock prices and option values for european and american calls or puts. Users have the ability 
to choose three methods to calculate option values: The lognormal, Cox/Ross/Rubenstein or Hull/White model. For this particular program 
the risk free rate is fixed at 4% and the volatility is fixed at 30%. 

## Example
The following example was done in Rstudio. <br/>

After running the function binomial.tree and MS.Euro.Call.I. 

[![Screenshot-from-2020-12-03-12-41-37.png](https://i.postimg.cc/4d1d7v2G/Screenshot-from-2020-12-03-12-41-37.png)](https://postimg.cc/PpPh0DkV)

After running grViz():

[![Screenshot-from-2020-12-03-12-42-34.png](https://i.postimg.cc/BZHWgQJ8/Screenshot-from-2020-12-03-12-42-34.png)](https://postimg.cc/z3zPDNb5)

## Libraries 
DiagrammeR - necessary to view returned values from function binomial.tree as tree. 

## Setup 
Git clone https://github.com/manuelnjit/binomial_tree_options.git <br/>
Open Rstudio and in the console type install.packages(DiagrammeR) <br/>
Finally check out the difference in option prices for different models.

## Errors
Using a fixed sigma is not realisitc. 

## Ideas
**Incorporate Binomial Tree in binomial.tree**: The function binomial.tree takes in ten different parameters including: whether the option
is American or European, a call or put and the current price of the stock in order to calculate the price of some option according to 
one of the three methods. However the function does not return the tree, it only returns stock prices and option values at each nth step. A 
nice addition would be to give the function binomial.tree the ability to produce the tree whenever the user would like to see it. 

