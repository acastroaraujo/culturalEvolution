
<!-- README.md is generated from README.Rmd. Please edit that file -->

# culturalEvolution

<!-- badges: start -->
<!-- badges: end -->

**`culturalEvolution`** provides a set of convenient functions for
models of cultural evolution and/or diffusion processes.

## Installation

You can install the development version of **`culturalEvolution`** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("acastroaraujo/culturalEvolution")
```

Currently available functions:

- `recursion()`

- `multi_par_call()`

## Recursion

``` r
library(culturalEvolution)
```

You can construct *any* model with the `recursion()` function, a
mathematical expression, and the name of the “state variable.”

Each of these models is a *new* function of class “rfun” with the
following arguments:

- `params` a list of parameters that match those in the mathematical
  expression

- `init` the initial value for the proportion of the “trait.”

- `tn` the number of time periods.

Here are two very simple models from Henrich (2001):

``` r

env_learn <- recursion(
  var = q,
  expr = q + (1 - q)*P1 - q*P2
)

env_learn ## special print
#> Recursive equation:
#> q' = q + (1 - q) * P1 - q * P2 
#> 
#> Required parameters:
#> P1, P2

out <- env_learn(
  params = list(P1 = 0.308, P2 = 0.062), 
  tn = 100
)

str(out)
#> tibble [101 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ t: int [1:101] 0 1 2 3 4 5 6 7 8 9 ...
#>  $ q: num [1:101] 0 0.308 0.502 0.624 0.701 ...
#>  - attr(*, "params")=List of 2
#>   ..$ P1: num 0.308
#>   ..$ P2: num 0.062

plot(out, type = "l")
```

<img src="man/figures/README-recursion-example-1.png" width="100%" />

``` r

biased_transmission <- recursion(
  var = p,
  expr = p + p*(1 - p)*B
)

biased_transmission ## special print
#> Recursive equation:
#> p' = p + p * (1 - p) * B 
#> 
#> Required parameters:
#> B

out <- biased_transmission(
  params = list(B = 0.1), 
  init = 0.005,  ## q cannot start as zero in this model!
  tn = 100
)

plot(out, type = "l")
```

<img src="man/figures/README-recursion-example-2.png" width="100%" />

*Errors:*

Every recursive function will check that the supplied list of parameters
corresponds to the mathematical expression.

``` r
out <- biased_transmission(
  params = list(X = 0.1)
)
#> Error: B missing from the parameter list
```

The “state variable” and `t` *cannot* be supplied as a parameters:

``` r
out <- env_learn(
  params = list(q = 0.01)
)
#> Error: the parameter list cannot contain an object named q

out <- biased_transmission(
  params = list(p = 0.01)
)
#> Error: the parameter list cannot contain an object named p

out <- env_learn(
  params = list(t = 1)
)
#> Error: the parameter list cannot contain an object named t
```

**Using lists of parameters**

The `multi_par_call()` allows us to pass lists of parameters in the form
of data frames (i.e., one combination per row).

Here we create a similar graph to Figure 11 in Henrich (2001, 1006):

``` r

comb_mod <- recursion(q,
  expr = q + 0.5*(P1 + (L-1)*q) + 0.5*q*(1-q)*(b*(1 - a) + a*(2*q - 1))
)

grid <- tidyr::crossing(
  a = seq(0, 0.4, length.out = 100),
  b = 0.2, 
  L = 0.98, 
  P1 = 0.012
)

out <- multi_par_call(grid, comb_mod, init = 0, tn = 200)

str(out)
#> tibble [20,100 × 7] (S3: tbl_df/tbl/data.frame)
#>  $ .id: Factor w/ 100 levels "1","10","100",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ t  : int [1:20100] 0 1 2 3 4 5 6 7 8 9 ...
#>  $ q  : num [1:20100] 0 0.006 0.0125 0.0196 0.0274 ...
#>  $ a  : num [1:20100] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ b  : num [1:20100] 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 ...
#>  $ L  : num [1:20100] 0.98 0.98 0.98 0.98 0.98 0.98 0.98 0.98 0.98 0.98 ...
#>  $ P1 : num [1:20100] 0.012 0.012 0.012 0.012 0.012 0.012 0.012 0.012 0.012 0.012 ...

library(ggplot2)

out |> 
  ggplot(aes(t, q, group = .id, color = a)) + 
  geom_line() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  labs(color = "alpha") + 
  scale_color_viridis_c(option = "H") +
  theme_bw() 
```

<img src="man/figures/README-multi-par-example-1.png" width="100%" />

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-henrich2001" class="csl-entry">

Henrich, Joseph. 2001. “Cultural Transmission and the Diffusion of
Innovations: Adoption Dynamics Indicate That Biased Cultural
Transmission Is the Predominate Force in Behavioral Change.” *American
Anthropologist* 103 (4): 9921013.

</div>

</div>
