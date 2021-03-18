# Regression

**modeling functions**
- f_scatter_plot (This is the simple regression.)
- f_linear_model (This is more general - multiple regression)
- f_size_lm (Researchers like to check the sample size for subgroup)

You can simply use source_url from package devtools. 
``` R
library('devtools')
source_url("https://raw.githubusercontent.com/Wei-Kuang/R/main/LinearRegression.R")
```

#### Example
```
library(datasets)
data(iris)
summary(iris)

# Setting
df_input= iris
var_outcome = 'Sepal.Length'
var_x = 'Species' # This is the empty place for the variable that we want to investigate

var_covar = c('Sepal.Width', 'Petal.Length') # These are the covariates

# Get Model
mymodel = f_linear_model(df_input.arg=df_input,
                         var_outcome.arg = var_outcome, 
                         var_x.arg = var_x, 
                         var_covar.arg = var_covar)
# Get N 
N_df = f_size_lm(model.arg = mymodel, var_outcome.arg = var_outcome )
N_df

# Get the Coef
Coef_df =  moderndive::get_regression_table(mymodel,digits=8) %>%
  mutate( CI95 = paste0( round(lower_ci,2),' - ',round(upper_ci,2) ) ) %>%
  dplyr::select( - c(lower_ci, upper_ci))


Report_df = full_join(x =N_df ,y=Coef_df, by= c('key'='term')) 
Report_df %>% select(-key)


R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from C:/Users/WeiKuang/Desktop/Stanford Work/WELL/Project/David_FactorAnalysis/Code/1_US_CFA/WELL_Score_function/WELL_Score_Function/version_09.23.20/.RData]

> #### package ####
> library('tidyverse')
-- Attaching packages -------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
v ggplot2 3.3.3     v purrr   0.3.4
v tibble  3.0.4     v dplyr   1.0.2
v tidyr   1.1.2     v stringr 1.4.0
v readr   1.4.0     v forcats 0.5.0
-- Conflicts ----------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
x dplyr::filter() masks stats::filter()
x dplyr::lag()    masks stats::lag()
> library('moderndive') # https://moderndive.github.io/moderndive/
> 
> 
> #########################################################
> #### scatterplot - simple regression - one Y & one X ####
> #########################################################
> f_scatter_plot = function(input_data, var_x.arg , var_y.arg,
+                           x.limit.arg = c(0,100), x.breaks.arg =seq(0,100,5),
+                           y.limit.arg = c(0,5), y.breaks.arg =seq(0,5,1),
+                           free_xy.arg=F){
+   
+   
+   data_complete = input_data %>% 
+     dplyr::select(var_x.arg, var_y.arg) %>%
+     filter(complete.cases(.))
+   
+   my_formula= as.formula(  paste( var_y.arg, "~", var_x.arg) )
+   fit <- lm(my_formula, data=data_complete)
+   lm_info = summary(fit)
+   lm_info$coefficients[1:2]
+   p_vale =  lm_info$coefficients %>% data.frame() %>% .[2,4]
+   #### text
+   library(grid)
+   
+   title = paste("R2 = ",       signif(summary(fit)$r.squared, 5),
+                 "Corr (r) =",   ((summary(fit)$r.squared) ^ (1/2)) %>% round(2),  
+                 "Intercept =", signif(fit$coef[[1]],5 ),
+                 "Slope =",     signif(fit$coef[[2]], 5),
+                 "\nP =",       signif(summary(fit)$coef[2,4], 5))
+   
+   grob <- grobTree(textGrob(title, 
+                             x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=13, fontface="italic")))
+   
+   #### plot
+   p_xy_free = input_data %>% ggplot(aes_string(x= var_x.arg, y = var_y.arg)) + 
+     geom_point() + 
+     geom_smooth(method = 'lm', se=F) + # method = 'lm'
+     annotation_custom(grob)
+   
+   p_xy_fixed = p_xy_free +
+     # x.y-axis markers
+     scale_x_continuous(limits = x.limit.arg, breaks= x.breaks.arg ) +
+     scale_y_continuous(limits = y.limit.arg, breaks= y.breaks.arg ) 
+     
+   
+   if (free_xy.arg == T){
+     print(p_xy_free)
+   }else{
+     print(p_xy_fixed)
+   }
+     
+ 
+  #### output ####
+   slope  = signif(fit$coef[[2]], 5)
+   p_value = signif(summary(fit)$coef[2,4], 5)
+   my_corr = ((summary(fit)$r.squared) ^ (1/2)) %>% round(2)
+   n = nrow(data_complete)
+   output = data.frame(X_Var= var_x.arg, Slope =slope, P_value=p_value, Corr= my_corr, N= n)
+   return(output)
+ }
> 
> # example:  f_scatter_plot(input_data = DF, var_x.arg= var, var_y.arg ='r_TS') 
> 
> 
> ######################################
> #### function to get the lm model ####
> ######################################
> df_input= DF
> var_outcome = 'well_score_9dom_well0'
> var_x = 'core_gender_well0'
> var_covar = c('portal_age_well0', 'dv_edu_3cat_well0')
> 
> f_linear_model = function(df_input.arg , var_outcome.arg,  var_x.arg, var_covar.arg) {
+   
+   #### Prepare variables
+   df_input       = df_input.arg
+   var_outcome    = var_outcome.arg
+   var_x          = var_x.arg
+   var_covar      = var_covar.arg
+   
+   #### Remove na
+   df_input_complete = df_input %>% 
+     dplyr::select(var_outcome, var_covar, var_x) %>%
+     dplyr::filter(complete.cases(.))
+   
+   #### Model
+   var_predictor = c(var_x, var_covar)
+   m_formula = as.formula( paste(var_outcome, paste(var_predictor, collapse = " + "), sep = " ~ ") )
+   model = lm( m_formula , data = df_input_complete)
+   
+   #### Output
+   return(model)
+ }
> 
> # Example
> # df_input= DF
> # var_outcome = 'well_score_9dom_well0'
> # var_x = 'core_gender_well0'
> # var_covar = c('portal_age_well0', 'dv_edu_3cat_well0')
> # 
> # mymodel = f_linear_model(df_input.arg=DF,
> #                          var_outcome.arg = 'well_score_9dom_well0', 
> #                          var_x.arg = 'core_gender_well0', 
> #                          var_covar.arg = var_covar)
> 
> 
> 
> #######################################
> #### LM function-1 sample_size_glm ####
> #######################################
> 
> f_size_lm = function(model.arg , var_outcome.arg){
+   #### Prepare the variable
+   data = model.arg$model 
+   My_Var = colnames(data)
+   Y= var_outcome.arg
+   
+   #### For loop
+   N_df = data.frame()
+   for (i in 1:length(My_Var)){
+     
+     #### Set var
+     var = My_Var[i]
+     # print(var)
+     
+     #### If var is contiuous or categorical
+     if (   is.numeric(data[, var ])   ) {
+       #### Con.Var: Sample size
+       n = data %>% dplyr::select(Y) %>% nrow()
+       df= data.frame(N = n)
+       df= add_column(df, Levels = NA, .before=1) 
+       df= add_column(df, Variable = var, .before=1) 
+     }else{
+       #### Cat.Var: Sample size
+       df = data %>% count_(var) 
+       colnames(df) = c('Levels','N')
+       df = add_column(df, Variable = var, .before=1) 
+     }
+     
+     #### Append dataframe
+     N_df = rbind(N_df, df)
+     
+   }# End of for loop
+   
+   #### DataFrame formation
+   N_df$Levels  = as.character(  N_df$Levels  )
+   #### Creat a key for matching model coef 
+   N_df$Levels[ is.na(N_df$Levels)] = '' # This is to ensure the key for numeric prediction is correct
+   key_vector = paste0(N_df$Variable, N_df$Levels)
+   N_df= add_column(N_df, key = key_vector, .before=1)    
+   
+   #### Return
+   return(N_df)
+ }# End of function
> 
> 
> #### Example
> # N_df = f_size_lm(model.arg = mymodel, var_outcome.arg = 'well_score_9dom_well0' )
> data(iris)
> iris
    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
1            5.1         3.5          1.4         0.2     setosa
2            4.9         3.0          1.4         0.2     setosa
3            4.7         3.2          1.3         0.2     setosa
4            4.6         3.1          1.5         0.2     setosa
5            5.0         3.6          1.4         0.2     setosa
6            5.4         3.9          1.7         0.4     setosa
7            4.6         3.4          1.4         0.3     setosa
8            5.0         3.4          1.5         0.2     setosa
9            4.4         2.9          1.4         0.2     setosa
10           4.9         3.1          1.5         0.1     setosa
11           5.4         3.7          1.5         0.2     setosa
12           4.8         3.4          1.6         0.2     setosa
13           4.8         3.0          1.4         0.1     setosa
14           4.3         3.0          1.1         0.1     setosa
15           5.8         4.0          1.2         0.2     setosa
16           5.7         4.4          1.5         0.4     setosa
17           5.4         3.9          1.3         0.4     setosa
18           5.1         3.5          1.4         0.3     setosa
19           5.7         3.8          1.7         0.3     setosa
20           5.1         3.8          1.5         0.3     setosa
21           5.4         3.4          1.7         0.2     setosa
22           5.1         3.7          1.5         0.4     setosa
23           4.6         3.6          1.0         0.2     setosa
24           5.1         3.3          1.7         0.5     setosa
25           4.8         3.4          1.9         0.2     setosa
26           5.0         3.0          1.6         0.2     setosa
27           5.0         3.4          1.6         0.4     setosa
28           5.2         3.5          1.5         0.2     setosa
29           5.2         3.4          1.4         0.2     setosa
30           4.7         3.2          1.6         0.2     setosa
31           4.8         3.1          1.6         0.2     setosa
32           5.4         3.4          1.5         0.4     setosa
33           5.2         4.1          1.5         0.1     setosa
34           5.5         4.2          1.4         0.2     setosa
35           4.9         3.1          1.5         0.2     setosa
36           5.0         3.2          1.2         0.2     setosa
37           5.5         3.5          1.3         0.2     setosa
38           4.9         3.6          1.4         0.1     setosa
39           4.4         3.0          1.3         0.2     setosa
40           5.1         3.4          1.5         0.2     setosa
41           5.0         3.5          1.3         0.3     setosa
42           4.5         2.3          1.3         0.3     setosa
43           4.4         3.2          1.3         0.2     setosa
44           5.0         3.5          1.6         0.6     setosa
45           5.1         3.8          1.9         0.4     setosa
46           4.8         3.0          1.4         0.3     setosa
47           5.1         3.8          1.6         0.2     setosa
48           4.6         3.2          1.4         0.2     setosa
49           5.3         3.7          1.5         0.2     setosa
50           5.0         3.3          1.4         0.2     setosa
51           7.0         3.2          4.7         1.4 versicolor
52           6.4         3.2          4.5         1.5 versicolor
53           6.9         3.1          4.9         1.5 versicolor
54           5.5         2.3          4.0         1.3 versicolor
55           6.5         2.8          4.6         1.5 versicolor
56           5.7         2.8          4.5         1.3 versicolor
57           6.3         3.3          4.7         1.6 versicolor
58           4.9         2.4          3.3         1.0 versicolor
59           6.6         2.9          4.6         1.3 versicolor
60           5.2         2.7          3.9         1.4 versicolor
61           5.0         2.0          3.5         1.0 versicolor
62           5.9         3.0          4.2         1.5 versicolor
63           6.0         2.2          4.0         1.0 versicolor
64           6.1         2.9          4.7         1.4 versicolor
65           5.6         2.9          3.6         1.3 versicolor
66           6.7         3.1          4.4         1.4 versicolor
67           5.6         3.0          4.5         1.5 versicolor
68           5.8         2.7          4.1         1.0 versicolor
69           6.2         2.2          4.5         1.5 versicolor
70           5.6         2.5          3.9         1.1 versicolor
71           5.9         3.2          4.8         1.8 versicolor
72           6.1         2.8          4.0         1.3 versicolor
73           6.3         2.5          4.9         1.5 versicolor
74           6.1         2.8          4.7         1.2 versicolor
75           6.4         2.9          4.3         1.3 versicolor
76           6.6         3.0          4.4         1.4 versicolor
77           6.8         2.8          4.8         1.4 versicolor
78           6.7         3.0          5.0         1.7 versicolor
79           6.0         2.9          4.5         1.5 versicolor
80           5.7         2.6          3.5         1.0 versicolor
81           5.5         2.4          3.8         1.1 versicolor
82           5.5         2.4          3.7         1.0 versicolor
83           5.8         2.7          3.9         1.2 versicolor
84           6.0         2.7          5.1         1.6 versicolor
85           5.4         3.0          4.5         1.5 versicolor
86           6.0         3.4          4.5         1.6 versicolor
87           6.7         3.1          4.7         1.5 versicolor
88           6.3         2.3          4.4         1.3 versicolor
89           5.6         3.0          4.1         1.3 versicolor
90           5.5         2.5          4.0         1.3 versicolor
91           5.5         2.6          4.4         1.2 versicolor
92           6.1         3.0          4.6         1.4 versicolor
93           5.8         2.6          4.0         1.2 versicolor
94           5.0         2.3          3.3         1.0 versicolor
95           5.6         2.7          4.2         1.3 versicolor
96           5.7         3.0          4.2         1.2 versicolor
97           5.7         2.9          4.2         1.3 versicolor
98           6.2         2.9          4.3         1.3 versicolor
99           5.1         2.5          3.0         1.1 versicolor
100          5.7         2.8          4.1         1.3 versicolor
101          6.3         3.3          6.0         2.5  virginica
102          5.8         2.7          5.1         1.9  virginica
103          7.1         3.0          5.9         2.1  virginica
104          6.3         2.9          5.6         1.8  virginica
105          6.5         3.0          5.8         2.2  virginica
106          7.6         3.0          6.6         2.1  virginica
107          4.9         2.5          4.5         1.7  virginica
108          7.3         2.9          6.3         1.8  virginica
109          6.7         2.5          5.8         1.8  virginica
110          7.2         3.6          6.1         2.5  virginica
111          6.5         3.2          5.1         2.0  virginica
112          6.4         2.7          5.3         1.9  virginica
113          6.8         3.0          5.5         2.1  virginica
114          5.7         2.5          5.0         2.0  virginica
115          5.8         2.8          5.1         2.4  virginica
116          6.4         3.2          5.3         2.3  virginica
117          6.5         3.0          5.5         1.8  virginica
118          7.7         3.8          6.7         2.2  virginica
119          7.7         2.6          6.9         2.3  virginica
120          6.0         2.2          5.0         1.5  virginica
121          6.9         3.2          5.7         2.3  virginica
122          5.6         2.8          4.9         2.0  virginica
123          7.7         2.8          6.7         2.0  virginica
124          6.3         2.7          4.9         1.8  virginica
125          6.7         3.3          5.7         2.1  virginica
126          7.2         3.2          6.0         1.8  virginica
127          6.2         2.8          4.8         1.8  virginica
128          6.1         3.0          4.9         1.8  virginica
129          6.4         2.8          5.6         2.1  virginica
130          7.2         3.0          5.8         1.6  virginica
131          7.4         2.8          6.1         1.9  virginica
132          7.9         3.8          6.4         2.0  virginica
133          6.4         2.8          5.6         2.2  virginica
134          6.3         2.8          5.1         1.5  virginica
135          6.1         2.6          5.6         1.4  virginica
136          7.7         3.0          6.1         2.3  virginica
137          6.3         3.4          5.6         2.4  virginica
138          6.4         3.1          5.5         1.8  virginica
139          6.0         3.0          4.8         1.8  virginica
140          6.9         3.1          5.4         2.1  virginica
141          6.7         3.1          5.6         2.4  virginica
142          6.9         3.1          5.1         2.3  virginica
143          5.8         2.7          5.1         1.9  virginica
144          6.8         3.2          5.9         2.3  virginica
145          6.7         3.3          5.7         2.5  virginica
146          6.7         3.0          5.2         2.3  virginica
147          6.3         2.5          5.0         1.9  virginica
148          6.5         3.0          5.2         2.0  virginica
149          6.2         3.4          5.4         2.3  virginica
150          5.9         3.0          5.1         1.8  virginica
> #######################################
> #### LM function-1 sample_size_glm ####
> #######################################
> library(datasets)
> library(datasets)
> data(iris)
> summary(iris)
  Sepal.Length    Sepal.Width     Petal.Length    Petal.Width          Species  
 Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100   setosa    :50  
 1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300   versicolor:50  
 Median :5.800   Median :3.000   Median :4.350   Median :1.300   virginica :50  
 Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199                  
 3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800                  
 Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500                  
> # Setting
> df_input= iris
> var_outcome = 'Sepal.Length'
> var_x = 'Species' # This is the empty place for the variable that we want to investigate
> var_covar = c('Sepal.Width', 'Petal.Length') # These are the covariates
> # Get Model
> mymodel = f_linear_model(df_input.arg=DF,
+                          var_outcome.arg = var_outcome, 
+                          var_x.arg = var_x, 
+                          var_covar.arg = var_covar)
Note: Using an external vector in selections is ambiguous.
i Use `all_of(var_outcome)` instead of `var_outcome` to silence this message.
i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
This message is displayed once per session.
 Error: Can't subset columns that don't exist.
x Column `Sepal.Length` doesn't exist.
Run `rlang::last_error()` to see where the error occurred. 
> #### package ####
> library('tidyverse')
> library('moderndive') # https://moderndive.github.io/moderndive/
> 
> 
> #########################################################
> #### scatterplot - simple regression - one Y & one X ####
> #########################################################
> f_scatter_plot = function(input_data, var_x.arg , var_y.arg,
+                           x.limit.arg = c(0,100), x.breaks.arg =seq(0,100,5),
+                           y.limit.arg = c(0,5), y.breaks.arg =seq(0,5,1),
+                           free_xy.arg=F){
+   
+   
+   data_complete = input_data %>% 
+     dplyr::select(var_x.arg, var_y.arg) %>%
+     filter(complete.cases(.))
+   
+   my_formula= as.formula(  paste( var_y.arg, "~", var_x.arg) )
+   fit <- lm(my_formula, data=data_complete)
+   lm_info = summary(fit)
+   lm_info$coefficients[1:2]
+   p_vale =  lm_info$coefficients %>% data.frame() %>% .[2,4]
+   #### text
+   library(grid)
+   
+   title = paste("R2 = ",       signif(summary(fit)$r.squared, 5),
+                 "Corr (r) =",   ((summary(fit)$r.squared) ^ (1/2)) %>% round(2),  
+                 "Intercept =", signif(fit$coef[[1]],5 ),
+                 "Slope =",     signif(fit$coef[[2]], 5),
+                 "\nP =",       signif(summary(fit)$coef[2,4], 5))
+   
+   grob <- grobTree(textGrob(title, 
+                             x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=13, fontface="italic")))
+   
+   #### plot
+   p_xy_free = input_data %>% ggplot(aes_string(x= var_x.arg, y = var_y.arg)) + 
+     geom_point() + 
+     geom_smooth(method = 'lm', se=F) + # method = 'lm'
+     annotation_custom(grob)
+   
+   p_xy_fixed = p_xy_free +
+     # x.y-axis markers
+     scale_x_continuous(limits = x.limit.arg, breaks= x.breaks.arg ) +
+     scale_y_continuous(limits = y.limit.arg, breaks= y.breaks.arg ) 
+     
+   
+   if (free_xy.arg == T){
+     print(p_xy_free)
+   }else{
+     print(p_xy_fixed)
+   }
+     
+ 
+  #### output ####
+   slope  = signif(fit$coef[[2]], 5)
+   p_value = signif(summary(fit)$coef[2,4], 5)
+   my_corr = ((summary(fit)$r.squared) ^ (1/2)) %>% round(2)
+   n = nrow(data_complete)
+   output = data.frame(X_Var= var_x.arg, Slope =slope, P_value=p_value, Corr= my_corr, N= n)
+   return(output)
+ }
> 
> # example:  f_scatter_plot(input_data = DF, var_x.arg= var, var_y.arg ='r_TS') 
> 
> 
> ######################################
> #### function to get the lm model ####
> ######################################
> df_input= DF
> var_outcome = 'well_score_9dom_well0'
> var_x = 'core_gender_well0'
> var_covar = c('portal_age_well0', 'dv_edu_3cat_well0')
> 
> f_linear_model = function(df_input.arg , var_outcome.arg,  var_x.arg, var_covar.arg) {
+   
+   #### Prepare variables
+   df_input       = df_input.arg
+   var_outcome    = var_outcome.arg
+   var_x          = var_x.arg
+   var_covar      = var_covar.arg
+   
+   #### Remove na
+   df_input_complete = df_input %>% 
+     dplyr::select(var_outcome, var_covar, var_x) %>%
+     dplyr::filter(complete.cases(.))
+   
+   #### Model
+   var_predictor = c(var_x, var_covar)
+   m_formula = as.formula( paste(var_outcome, paste(var_predictor, collapse = " + "), sep = " ~ ") )
+   model = lm( m_formula , data = df_input_complete)
+   
+   #### Output
+   return(model)
+ }
> 
> # Example
> # df_input= DF
> # var_outcome = 'well_score_9dom_well0'
> # var_x = 'core_gender_well0'
> # var_covar = c('portal_age_well0', 'dv_edu_3cat_well0')
> # 
> # mymodel = f_linear_model(df_input.arg=DF,
> #                          var_outcome.arg = 'well_score_9dom_well0', 
> #                          var_x.arg = 'core_gender_well0', 
> #                          var_covar.arg = var_covar)
> 
> 
> 
> #######################################
> #### LM function-1 sample_size_glm ####
> #######################################
> 
> f_size_lm = function(model.arg , var_outcome.arg){
+   #### Prepare the variable
+   data = model.arg$model 
+   My_Var = colnames(data)
+   Y= var_outcome.arg
+   
+   #### For loop
+   N_df = data.frame()
+   for (i in 1:length(My_Var)){
+     
+     #### Set var
+     var = My_Var[i]
+     # print(var)
+     
+     #### If var is contiuous or categorical
+     if (   is.numeric(data[, var ])   ) {
+       #### Con.Var: Sample size
+       n = data %>% dplyr::select(Y) %>% nrow()
+       df= data.frame(N = n)
+       df= add_column(df, Levels = NA, .before=1) 
+       df= add_column(df, Variable = var, .before=1) 
+     }else{
+       #### Cat.Var: Sample size
+       df = data %>% count_(var) 
+       colnames(df) = c('Levels','N')
+       df = add_column(df, Variable = var, .before=1) 
+     }
+     
+     #### Append dataframe
+     N_df = rbind(N_df, df)
+     
+   }# End of for loop
+   
+   #### DataFrame formation
+   N_df$Levels  = as.character(  N_df$Levels  )
+   #### Creat a key for matching model coef 
+   N_df$Levels[ is.na(N_df$Levels)] = '' # This is to ensure the key for numeric prediction is correct
+   key_vector = paste0(N_df$Variable, N_df$Levels)
+   N_df= add_column(N_df, key = key_vector, .before=1)    
+   
+   #### Return
+   return(N_df)
+ }# End of function
> # Setting
> df_input= iris
> var_outcome = 'Sepal.Length'
> var_x = 'Species' # This is the empty place for the variable that we want to investigate
> var_covar = c('Sepal.Width', 'Petal.Length') # These are the covariates
> # Get Model
> mymodel = f_linear_model(df_input.arg=DF,
+                          var_outcome.arg = var_outcome, 
+                          var_x.arg = var_x, 
+                          var_covar.arg = var_covar)
 Error: Can't subset columns that don't exist.
x Column `Sepal.Length` doesn't exist.
Run `rlang::last_error()` to see where the error occurred. 
> # Get Model
> mymodel = f_linear_model(df_input.arg=df_input,
+                          var_outcome.arg = var_outcome, 
+                          var_x.arg = var_x, 
+                          var_covar.arg = var_covar)
Note: Using an external vector in selections is ambiguous.
i Use `all_of(var_covar)` instead of `var_covar` to silence this message.
i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
This message is displayed once per session.
Note: Using an external vector in selections is ambiguous.
i Use `all_of(var_x)` instead of `var_x` to silence this message.
i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
This message is displayed once per session.
> # Get N 
> N_df = f_size_lm(model.arg = mymodel, var_outcome.arg = var_outcome )
Note: Using an external vector in selections is ambiguous.
i Use `all_of(Y)` instead of `Y` to silence this message.
i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
This message is displayed once per session.
Warning message:
`count_()` is deprecated as of dplyr 0.7.0.
Please use `count()` instead.
See vignette('programming') for more help
This warning is displayed once every 8 hours.
Call `lifecycle::last_warnings()` to see where this warning was generated. 
> N_df
                key     Variable     Levels   N
1      Sepal.Length Sepal.Length            150
2     Speciessetosa      Species     setosa  50
3 Speciesversicolor      Species versicolor  50
4  Speciesvirginica      Species  virginica  50
5       Sepal.Width  Sepal.Width            150
6      Petal.Length Petal.Length            150
> # Get the Coef
> Coef_df =  moderndive::get_regression_table(mymodel,digits=8) %>%
+   mutate( CI95 = paste0( round(lower_ci,2),' - ',round(upper_ci,2) ) ) %>%
+   dplyr::select( - c(lower_ci, upper_ci))
> Report_df = full_join(x =N_df ,y=Coef_df, by= c('key'='term')) 
> Report_df %>% select(-key)
      Variable     Levels   N   estimate  std_error statistic  p_value          CI95
1 Sepal.Length            150         NA         NA        NA       NA          <NA>
2      Species     setosa  50         NA         NA        NA       NA          <NA>
3      Species versicolor  50 -0.9558123 0.21519853 -4.441537 1.76e-05 -1.38 - -0.53
4      Species  virginica  50 -1.3940979 0.28566053 -4.880261 2.76e-06 -1.96 - -0.83
5  Sepal.Width            150  0.4322172 0.08138982  5.310458 4.00e-07   0.27 - 0.59
6 Petal.Length            150  0.7756295 0.06424566 12.072869 0.00e+00    0.65 - 0.9
7         <NA>       <NA>  NA  2.3903891 0.26226815  9.114294 0.00e+00   1.87 - 2.91
> # Model quality
> get_regression_summaries(mymodel)
# A tibble: 1 x 9
  r_squared adj_r_squared    mse  rmse sigma statistic p_value    df  nobs
      <dbl>         <dbl>  <dbl> <dbl> <dbl>     <dbl>   <dbl> <dbl> <dbl>
1     0.863          0.86 0.0931 0.305  0.31      229.       0     4   150
#> Report_df %>% select(-key)
#      Variable     Levels   N   estimate  std_error statistic  p_value          CI95
#1 Sepal.Length            150         NA         NA        NA       NA          <NA>
#2      Species     setosa  50         NA         NA        NA       NA          <NA>
#3      Species versicolor  50 -0.9558123 0.21519853 -4.441537 1.76e-05 -1.38 - -0.53
#4      Species  virginica  50 -1.3940979 0.28566053 -4.880261 2.76e-06 -1.96 - -0.83
#5  Sepal.Width            150  0.4322172 0.08138982  5.310458 4.00e-07   0.27 - 0.59
#6 Petal.Length            150  0.7756295 0.06424566 12.072869 0.00e+00    0.65 - 0.9
#7         <NA>       <NA>  NA  2.3903891 0.26226815  9.114294 0.00e+00   1.87 - 2.91

# Model quality
get_regression_summaries(mymodel)

#  r_squared adj_r_squared    mse  rmse sigma statistic p_value    df  nobs
#      <dbl>         <dbl>  <dbl> <dbl> <dbl>     <dbl>   <dbl> <dbl> <dbl>
#1     0.863          0.86 0.0931 0.305  0.31      229.       0     4   150
```
