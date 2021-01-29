# R

This folder contains my R codes to do modeling and data visualization.

**Plotting functions**
- [f_densityplot_cont_by_cate](https://github.com/Wei-Kuang/R/blob/main/readme_densityplot.md)
- [f_boxplot](https://github.com/Wei-Kuang/R/blob/main/readme_boxplot.md)
- [f_barplot_onevar](https://github.com/Wei-Kuang/R/blob/main/readme_barplot.md)


You can simply use source_url from package devtools. 
``` R
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/R/main/Plot_BoxDensity.R")
```

## f_densityplot_cont_by_cate
### Generate density plot for a continuous  variable grouped by a categorical variable.

<img src="image/densityplot_example.png" height="380"> 

```R
#### Load iris data from ggplot2 package
library(ggplot2)
head(iris)
DF = iris
str(DF) # Note: Species is already a factor. 
table(DF$Species) 


#### Analysis Plan
# Purpose: We want to see the distribution of Petal.Length by Species
# Outcome: Petal.Length
# Grouping variable: Species with 3 categories


#### Example-1 with a default color
result = f_densityplot_cont_by_cate(
         df_input =DF,
         cont.var.arg = 'Petal.Length',
         cate.var.arg = 'Species',
         col_pal.arg = NULL, # If NULL, the default color will be used
         legend.lab.arg='',
         xlab.arg = '',
         title.arg ='',
         x.limit.arg = c(0,8),
         x.breaks.arg =seq(0,8,2),
         y.max.arg= 3,
         y.break.arg = 1 )

#### Example-2 with a user's color
my_col_pal = c('darkolivegreen4','darkorange1','darkslategray3')
result = f_densityplot_cont_by_cate(
         df_input =DF,
         cont.var.arg = 'Petal.Length',
         cate.var.arg = 'Species',
         col_pal.arg = my_col_pal, 
         legend.lab.arg='',
         xlab.arg = '',
         title.arg ='',
         x.limit.arg = c(0,8),
         x.breaks.arg =seq(0,8,2),
         y.max.arg= 3,
         y.break.arg = 1 )

#### Example-3 with annotation on title, legend, and x axis
my_col_pal = c('darkolivegreen4','darkorange1','darkslategray3')
result = f_densityplot_cont_by_cate(
         df_input =DF,
         cont.var.arg = 'Petal.Length',
         cate.var.arg = 'Species',
         col_pal.arg = my_col_pal, 
         legend.lab.arg='Species',
         xlab.arg = 'Petal Length',
         title.arg ='Petal Length by Species',
         x.limit.arg = c(0,8),
         x.breaks.arg =seq(0,8,2),
         y.max.arg= 3,
         y.break.arg = 1 )



#### Example-4 with a user's ggplot theme
my_theme = theme(
  # hjust = 0.5 : Center of the title
  plot.title = element_text(size=15, face="bold"), 
  axis.title.x = element_text(size =15, face="bold"),
  axis.text.x = element_text(size = 12, face="bold"),
  axis.title.y = element_text(size =12,face="bold"),
  axis.text.y = element_text(size = 12,face="bold"),
  legend.title = element_text(size = 15, face = "bold" ),   
  legend.text = element_text(size = 12, face = "bold"),
  # Adjust facet
  strip.text.x = element_text(size = 12, face = "bold")
)

result = f_densityplot_cont_by_cate(
         df_input =DF,
         cont.var.arg = 'Petal.Length',
         cate.var.arg = 'Species',
         col_pal.arg = NULL, 
         legend.lab.arg='Species',
         xlab.arg = 'Petal Length',
         title.arg ='Petal Length by Species',
         x.limit.arg = c(0,8),
         x.breaks.arg =seq(0,8,2),
         y.max.arg= 3,
         y.break.arg = 1, 
         theme.arg = my_theme)

result
# # A tibble: 3 x 6
#   Species     Mean Median    SD     N N_Label        
#   <fct>      <dbl>  <dbl> <dbl> <int> <chr>          
# 1 setosa      1.46   1.5  0.174    50 setosa N=50    
# 2 versicolor  4.26   4.35 0.470    50 versicolor N=50
# 3 virginica   5.55   5.55 0.552    50 virginica N=50
```




## f_boxplot

<img src="image/boxplot_example.png" height="380"> 

```R
#### Load iris data from ggplot2 package
library(ggplot2)
head(iris)
DF = iris
str(DF) # Note: Species is already a factor. 
table(DF$Species) 


#### Analysis Plan
# Purpose: We want to see the distribution of Petal.Length by Species
# Outcome: Petal.Length
# Grouping variable: Species with 3 categories

#### Note: For this boxplot function, the color is fixed at gradient blue.

#### Example-1 with a default setting
result = f_boxplot(df_input = DF,
          cont.var='Petal.Length', 
          cate.var='Species', 
          title.arg = '', 
          xlab.arg='',
          ylab.arg='',
          y.limit.arg = c(0,10), y.breaks.arg = seq(0,10,2)
          )

#### Example-2 with the user's ggplot theme and annotated layers
my_theme = theme(
  # hjust = 0.5 : Center of the title
  plot.title = element_text(size=15, face="bold"), 
  axis.title.x = element_text(size =15, face="bold"),
  axis.text.x = element_text(size = 12, face="bold"),
  axis.title.y = element_text(size =12,face="bold"),
  axis.text.y = element_text(size = 12,face="bold"),
  legend.title = element_text(size = 15, face = "bold" ),   
  legend.text = element_text(size = 12, face = "bold"),
  # Adjust facet
  strip.text.x = element_text(size = 12, face = "bold")
)


result = f_boxplot(df_input = DF,
          cont.var='Petal.Length', 
          cate.var='Species', 
          title.arg = 'Petal Length by Species', 
          xlab.arg='Iris species',
          ylab.arg='Petal Length',
          y.limit.arg = c(0,10), y.breaks.arg = seq(0,10,2),
          theme.arg = my_theme
          )

result
#   Species     Mean Median    SD     N N_Label            
#   <fct>      <dbl>  <dbl> <dbl> <int> <chr>              
# 1 setosa      1.46   1.5  0.174    50 "setosa\n N=50"    
# 2 versicolor  4.26   4.35 0.470    50 "versicolor\n N=50"
# 3 virginica   5.55   5.55 0.552    50 "virginica\n N=50"
```

## Barplot
[a linkvfg](https://github.com/Wei-Kuang/R/blob/main/bar_plot_readme.md)
