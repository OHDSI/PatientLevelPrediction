# Calculate the model-based concordance, which is a calculation of the expected discrimination performance of a model under the assumption the model predicts the "TRUE" outcome as detailed in van Klaveren et al. https://pubmed.ncbi.nlm.nih.gov/27251001/

Calculate the model-based concordance, which is a calculation of the
expected discrimination performance of a model under the assumption the
model predicts the "TRUE" outcome as detailed in van Klaveren et al.
https://pubmed.ncbi.nlm.nih.gov/27251001/

## Usage

``` r
modelBasedConcordance(prediction)
```

## Arguments

- prediction:

  the prediction object found in the plpResult object

## Value

The model-based concordance value

## Details

Calculate the model-based concordance

## Examples

``` r
prediction <- data.frame(value = runif(100))
modelBasedConcordance(prediction)
#> [1] 0.8092385
```
