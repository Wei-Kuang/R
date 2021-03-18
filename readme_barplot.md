
# This function is under construction


**Plotting functions**
- f_barplot_onevar


## Example
```R
#### Load iris data from ggplot2 package
library(ggplot2)
head(iris)
DF = iris
str(DF) # Note: Species is already a factor.
table(DF$Species)


#### Example-1 ####
f_barplot_onevar(data.arg = DF,
                 var_target = 'Species',
                 y.limits.arg = c(0,100),
                 y.breaks.arg = seq(0,100,20),
                 coord_flip.arg = F)


#### Example-2 ####
f_barplot_onevar(data.arg = DF,
                 var_target = 'Species',
                 y.limits.arg = c(0,100),
                 y.breaks.arg = seq(0,100,20),
                 coord_flip.arg = T)

```
