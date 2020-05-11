# Playeraction 


The purpose of `playeraction` R package , an alternative for [socceraction](https://github.com/ML-KULeuven/socceraction) a Python package developed by [Tom Decross](https://github.com/TomDecroos) , is to provide R functions to  convert event stream action mainly from [Opta](https://www.optasports.com) to **SPADL** (Soccer Player Action Description Language) and **VAEP** (Valuing Actions by Estimating Probabilities) .



## Installation

`playeraction` supports R (>= 3.5.1)

To install playeraction package from local source :
```
install.packages("<path_to_source>/playeraction.zip", repos=NULL, type="source")
```
or an alternative way using **devtools** package  :
```
devtools::install_local("<path_to_source>/playeraction.zip")
```


## Conversion to SPADL

`playeraction` will be able to convert provided Opta F24 Feed to an unified language for player action during a soccer game.

  
