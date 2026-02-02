# Cartesian product

Computes the Cartesian product of all the combinations of elements in a
list

## Usage

``` r
listCartesian(allList)
```

## Arguments

- allList:

  a list of lists

## Value

A list with all possible combinations from the input list of lists

## Examples

``` r
listCartesian(list(list(1, 2), list(3, 4)))
#> [[1]]
#> [[1]]$Var1
#> [1] 1
#> 
#> [[1]]$Var2
#> [1] 3
#> 
#> 
#> [[2]]
#> [[2]]$Var1
#> [1] 2
#> 
#> [[2]]$Var2
#> [1] 3
#> 
#> 
#> [[3]]
#> [[3]]$Var1
#> [1] 1
#> 
#> [[3]]$Var2
#> [1] 4
#> 
#> 
#> [[4]]
#> [[4]]$Var1
#> [1] 2
#> 
#> [[4]]$Var2
#> [1] 4
#> 
#> 
```
