
rBAS <img src="inst/figures/rBAS.png" align="right" />
------------------------------------------------------

An R module aimed at implementation of Beetle Antennae Search (BAS) Algorithm and its mutations, for example, Beetle Swarm Antenae Search (BSAS) Algorithm.

------------------------------------------------------------------------

-   [Installation](#installation)
-   [Algorithm & References](#algorithm)
    -   [BAS](#bas)
    -   [BSAS](#bsas)
    -   [BAS-WPT](#bas-wpt)
-   [Examples](#examples)
    -   [Michalewicz function](#michalewicz-function)
    -   [Goldstein-Price function](#goldstein-price-function)
    -   [Implementation of BSAS](#bsas-algorithm)
-   [To do list](#to-do-list)
-   [Author](#author)
-   [Citation](#citation)
-   [License](#license)

Installation
------------

`rBAS` is currently not on CRAN. You can install `rBAS` from Github with:

``` r
devtools::install_github("jywang2016/rBAS")
```

Algorithm
---------

### BAS

[X. Y. Jiang, and S. Li, “BAS: beetle antennae search algorithm for optimization problems,” arXiv:1710.10724v1.](https://arxiv.org/abs/1710.10724)

### BSAS

The paper may be listed on arxiv soon. If you want to find materials in Chinese about BSAS, please click [here](https://github.com/jywang2016/BAS/blob/master/BAS_Swarm_documentation.html).

### BAS-WPT

[X. Y. Jiang, and S. Li, “Beetle Antennae Search without Parameter Tuning (BAS-WPT) for Multi-objective Optimization,” arXiv:1711.02395v1.](https://arxiv.org/abs/1711.02395)

Examples
--------

Use `help()` to see the document pages about functions in `rBAS`.

``` r
library(rBAS) #load package

help(BASoptim)
help(BSASoptim)
```

Two typocal test functions are applied to validate the efficacy of BAS/BSAS algorithm

### Michalewicz function

<img src="inst/figures/Mich.png"/>

``` r
library(rBAS)
mich <- function(x){
  y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
  y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
  return(y1+y2)
}
```

``` r
BASoptim(fn = mich,
         lower = c(-6,0), upper = c(-1,2),
         seed = 1, n = 100)
#> Iter:  0  xbest:  [ -4.672457 0.7442478 ], fbest=  -6.583555e-05 
#> Iter:  1  xbest:  [ -4.813013 0 ], fbest=  -0.09012505 
#> Iter:  2  xbest:  [ -4.813013 0 ], fbest=  -0.09012505 
#> Iter:  3  xbest:  [ -5.042063 0.5342482 ], fbest=  -0.5335012 
#> Iter:  4  xbest:  [ -5.042063 0.5342482 ], fbest=  -0.5335012 
#> Iter:  5  xbest:  [ -5.042063 0.5342482 ], fbest=  -0.5335012 
#> Iter:  6  xbest:  [ -5.042063 0.5342482 ], fbest=  -0.5335012 
#> Iter:  7  xbest:  [ -5.042063 0.5342482 ], fbest=  -0.5335012 
#> Iter:  8  xbest:  [ -5.042063 0.5342482 ], fbest=  -0.5335012 
#> Iter:  9  xbest:  [ -4.809573 1.495284 ], fbest=  -0.882469 
#> Iter:  10  xbest:  [ -4.809573 1.495284 ], fbest=  -0.882469 
#> Iter:  11  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  12  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  13  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  14  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  15  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  16  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  17  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  18  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  19  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  20  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  21  xbest:  [ -3.8701 1.397651 ], fbest=  -0.9768905 
#> Iter:  22  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  23  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  24  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  25  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  26  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  27  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  28  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  29  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  30  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  31  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  32  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  33  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  34  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  35  xbest:  [ -3.891725 1.55418 ], fbest=  -1.59477 
#> Iter:  36  xbest:  [ -5.028429 1.59817 ], fbest=  -1.619078 
#> Iter:  37  xbest:  [ -4.927904 1.521834 ], fbest=  -1.747204 
#> Iter:  38  xbest:  [ -4.927904 1.521834 ], fbest=  -1.747204 
#> Iter:  39  xbest:  [ -4.927904 1.521834 ], fbest=  -1.747204 
#> Iter:  40  xbest:  [ -4.927904 1.521834 ], fbest=  -1.747204 
#> Iter:  41  xbest:  [ -4.927904 1.521834 ], fbest=  -1.747204 
#> Iter:  42  xbest:  [ -4.927904 1.521834 ], fbest=  -1.747204 
#> Iter:  43  xbest:  [ -4.927904 1.521834 ], fbest=  -1.747204 
#> Iter:  44  xbest:  [ -4.927904 1.521834 ], fbest=  -1.747204 
#> Iter:  45  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  46  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  47  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  48  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  49  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  50  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  51  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  52  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  53  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  54  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  55  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  56  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  57  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  58  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  59  xbest:  [ -4.960227 1.591408 ], fbest=  -1.947337 
#> Iter:  60  xbest:  [ -4.973693 1.579025 ], fbest=  -1.959349 
#> Iter:  61  xbest:  [ -4.973693 1.579025 ], fbest=  -1.959349 
#> Iter:  62  xbest:  [ -4.973693 1.579025 ], fbest=  -1.959349 
#> Iter:  63  xbest:  [ -4.973693 1.579025 ], fbest=  -1.959349 
#> Iter:  64  xbest:  [ -4.973693 1.579025 ], fbest=  -1.959349 
#> Iter:  65  xbest:  [ -4.973693 1.579025 ], fbest=  -1.959349 
#> Iter:  66  xbest:  [ -4.973693 1.579025 ], fbest=  -1.959349 
#> Iter:  67  xbest:  [ -4.973693 1.579025 ], fbest=  -1.959349 
#> Iter:  68  xbest:  [ -4.967715 1.583254 ], fbest=  -1.961248 
#> Iter:  69  xbest:  [ -4.967715 1.583254 ], fbest=  -1.961248 
#> Iter:  70  xbest:  [ -4.967715 1.583254 ], fbest=  -1.961248 
#> Iter:  71  xbest:  [ -4.967715 1.583254 ], fbest=  -1.961248 
#> Iter:  72  xbest:  [ -4.967715 1.583254 ], fbest=  -1.961248 
#> Iter:  73  xbest:  [ -4.967715 1.583254 ], fbest=  -1.961248 
#> Iter:  74  xbest:  [ -4.967715 1.583254 ], fbest=  -1.961248 
#> Iter:  75  xbest:  [ -4.972746 1.566607 ], fbest=  -1.962718 
#> Iter:  76  xbest:  [ -4.972746 1.566607 ], fbest=  -1.962718 
#> Iter:  77  xbest:  [ -4.972746 1.566607 ], fbest=  -1.962718 
#> Iter:  78  xbest:  [ -4.96958 1.562077 ], fbest=  -1.963546 
#> Iter:  79  xbest:  [ -4.96958 1.562077 ], fbest=  -1.963546 
#> Iter:  80  xbest:  [ -4.96958 1.562077 ], fbest=  -1.963546 
#> Iter:  81  xbest:  [ -4.970852 1.571799 ], fbest=  -1.965519 
#> Iter:  82  xbest:  [ -4.970852 1.571799 ], fbest=  -1.965519 
#> Iter:  83  xbest:  [ -4.970852 1.571799 ], fbest=  -1.965519 
#> Iter:  84  xbest:  [ -4.970852 1.571799 ], fbest=  -1.965519 
#> Iter:  85  xbest:  [ -4.970852 1.571799 ], fbest=  -1.965519 
#> Iter:  86  xbest:  [ -4.970852 1.571799 ], fbest=  -1.965519
#> ----step < steptol---------stop the iteration------
#> $par
#> [1] -4.970852  1.571799
#> 
#> $value
#> [1] -1.965519
```

### Goldstein-Price function

<img src="inst/figures/Gold.png"/>

``` r
gold <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  y1 <- 1 + (x1 + x2 + 1)^2*(19 - 14*x1+3*x1^2 - 14*x2 + 6*x1*x2 + 3*x2^2)
  y2 <- 30 + (2*x1 -3*x2)^2*(18 - 32*x1 + 12*x1^2+48*x2-36*x1*x2 + 27*x2^2)
  return(y1*y2)
}
```

``` r
BASoptim(fn = gold,
         lower = c(-2,-2), upper = c(2,2),
         seed = NULL, n = 100)
#> Iter:  0  xbest:  [ -0.1229962 -1.3128 ], fbest=  264.8612 
#> Iter:  1  xbest:  [ -0.5245441 -0.6208753 ], fbest=  63.13738 
#> Iter:  2  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  3  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  4  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  5  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  6  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  7  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  8  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  9  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  10  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  11  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  12  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  13  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  14  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  15  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  16  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  17  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  18  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  19  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  20  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  21  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  22  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  23  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  24  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  25  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  26  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  27  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  28  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  29  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  30  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  31  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  32  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  33  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  34  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  35  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  36  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  37  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  38  xbest:  [ 0.2059563 -0.8305638 ], fbest=  16.832 
#> Iter:  39  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  40  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  41  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  42  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  43  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  44  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  45  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  46  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  47  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  48  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  49  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  50  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  51  xbest:  [ -0.06811286 -0.9229938 ], fbest=  6.938843 
#> Iter:  52  xbest:  [ -0.114416 -1.01844 ], fbest=  6.702868 
#> Iter:  53  xbest:  [ -0.07721961 -0.9771759 ], fbest=  5.076845 
#> Iter:  54  xbest:  [ -0.07721961 -0.9771759 ], fbest=  5.076845 
#> Iter:  55  xbest:  [ -0.07721961 -0.9771759 ], fbest=  5.076845 
#> Iter:  56  xbest:  [ -0.03196527 -0.949738 ], fbest=  4.521411 
#> Iter:  57  xbest:  [ -0.05556866 -0.9883434 ], fbest=  3.971174 
#> Iter:  58  xbest:  [ -0.01785186 -1.008966 ], fbest=  3.081688 
#> Iter:  59  xbest:  [ -0.01785186 -1.008966 ], fbest=  3.081688 
#> Iter:  60  xbest:  [ -0.001777592 -0.9958767 ], fbest=  3.009645 
#> Iter:  61  xbest:  [ -0.001777592 -0.9958767 ], fbest=  3.009645 
#> Iter:  62  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  63  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  64  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  65  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  66  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  67  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  68  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  69  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  70  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  71  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  72  xbest:  [ 0.001088534 -0.9975007 ], fbest=  3.002404 
#> Iter:  73  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  74  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  75  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  76  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  77  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  78  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  79  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  80  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  81  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  82  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  83  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  84  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  85  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703 
#> Iter:  86  xbest:  [ -0.002662872 -1.001157 ], fbest=  3.001703
#> ----step < steptol---------stop the iteration------
#> $par
#> [1] -0.002662872 -1.001157030
#> 
#> $value
#> [1] 3.001703
```

### BSAS algorithm

In order to save space, the BSAS algorithm code is executed with `trace` as `FALSE` because of too much trace information. You can set `trace` to `TRUE` and observe the trace messages.

``` r
BSASoptim(fn = mich,
          lower = c(-6,0), upper = c(-1,2),
          seed = 12, n = 100,k=5,
          trace = F)
#> $par
#> [1] -4.964138  1.558206
#> 
#> $value
#> [1] -1.961165
```

``` r
BSASoptim(fn = gold,
          lower = c(-2,-2), upper = c(2,2),
          seed = 11, n = 100,k=2,
          trace = F)
#> $par
#> [1]  0.009854699 -0.997594575
#> 
#> $value
#> [1] 3.021811
```

To do list
----------

-   ~~add implement of BSAS algorithm~~
-   add [BAS-WPT(without parameter tuning)](https://arxiv.org/abs/1711.02395)
-   add Shiny graphical interface

You can list your requirement in the [issues](https://github.com/jywang2016/rBAS/issues). Furthermore, if you have a good idea or codes about BAS's mutations, pull requests and discussion are welcome. Contact me by email: <jywang_2016@hust.edu.cn>

Author
------

`Jiangyu Wang`

[github page](https://github.com/jywang2016)

*School of Energy and Power Engineering, Huazhong University of Science and Technology*

`Shuai Li`

-   [personal homepage](http://www4.comp.polyu.edu.hk/~cssli/)

-   [Googlescholar](http://scholar.google.com/citations?hl=zh-CN&user=H8UOWqoAAAAJ)

*Department of Computing, The Hong Kong Polytechnic University*

Citation
--------

``` r
citation(package = 'rBAS')
#> 
#> To cite package 'rBAS' in publications use:
#> 
#>   Jiangyu Wang and Shuai Li (2018). rBAS: Implementation of the
#>   BAS algorithm and its mutation. R package version 0.0.0.9000.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {rBAS: Implementation of the BAS algorithm and its mutation},
#>     author = {Jiangyu Wang and Shuai Li},
#>     year = {2018},
#>     note = {R package version 0.0.0.9000},
#>   }
```

License
-------

The project is released under the terms of the GPL-3.0.
