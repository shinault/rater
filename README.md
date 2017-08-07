# rater

`rater` is a package for the R programming languages that implements a number of
rating and ranking methods.

The project website is 
[shinault.netlify.com/project/rater/](shinault.netlify.com/project/rater/).

You can install this package using `devtools` at the R command line.  First, 
make sure that `devtools` is installed.
```r
install.packages("devtools")
```
Then use the `install_github` function to install the package from GitHub.
```r
devtools::install_github("shinault/rater")
```

## Current Features

The scope of the package is currently very limited.
Development is in a very early phase; expect breaking changes.

The functions `massey` and `massey_adv` return the Massey and advanced Massey
ratings, respectively.  These ratings are based on points scored, but any 
useful metric can be used.

The function `colley` returns the Colley ratings based on net wins.

The function `markov` returns the Markov ratings based on a stochastic matrix
provided by the user.  This method has tremendous flexibility, as the transition
probabilities can be based on anything that proves useful.  In the context of 
sports this can be wins, points scored, or other sport-specific statistics.
The principal is the same as the PageRank algorithm.

The function `kendall` computes a modification of the Kendall τ coefficient.

Plots for comparing two ranking lists are provided by `rankplots`.


