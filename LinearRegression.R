#### package ####
library('tidyverse')
library('moderndive') # https://moderndive.github.io/moderndive/


#########################################################
#### scatterplot - simple regression - one Y & one X ####
#########################################################
f_scatter_plot = function(input_data, var_x.arg , var_y.arg,
                          x.limit.arg = c(0,100), x.breaks.arg =seq(0,100,5),
                          y.limit.arg = c(0,5), y.breaks.arg =seq(0,5,1),
                          free_xy.arg=F){
  
  
  data_complete = input_data %>% 
    dplyr::select(var_x.arg, var_y.arg) %>%
    filter(complete.cases(.))
  
  my_formula= as.formula(  paste( var_y.arg, "~", var_x.arg) )
  fit <- lm(my_formula, data=data_complete)
  lm_info = summary(fit)
  lm_info$coefficients[1:2]
  p_vale =  lm_info$coefficients %>% data.frame() %>% .[2,4]
  #### text
  library(grid)
  
  title = paste("R2 = ",       signif(summary(fit)$r.squared, 5),
                "Corr (r) =",   ((summary(fit)$r.squared) ^ (1/2)) %>% round(2),  
                "Intercept =", signif(fit$coef[[1]],5 ),
                "Slope =",     signif(fit$coef[[2]], 5),
                "\nP =",       signif(summary(fit)$coef[2,4], 5))
  
  grob <- grobTree(textGrob(title, 
                            x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=13, fontface="italic")))
  
  #### plot
  p_xy_free = input_data %>% ggplot(aes_string(x= var_x.arg, y = var_y.arg)) + 
    geom_point() + 
    geom_smooth(method = 'lm', se=F) + # method = 'lm'
    annotation_custom(grob)
  
  p_xy_fixed = p_xy_free +
    # x.y-axis markers
    scale_x_continuous(limits = x.limit.arg, breaks= x.breaks.arg ) +
    scale_y_continuous(limits = y.limit.arg, breaks= y.breaks.arg ) 
    
  
  if (free_xy.arg == T){
    print(p_xy_free)
  }else{
    print(p_xy_fixed)
  }
    

 #### output ####
  slope  = signif(fit$coef[[2]], 5)
  p_value = signif(summary(fit)$coef[2,4], 5)
  my_corr = ((summary(fit)$r.squared) ^ (1/2)) %>% round(2)
  n = nrow(data_complete)
  output = data.frame(X_Var= var_x.arg, Slope =slope, P_value=p_value, Corr= my_corr, N= n)
  return(output)
}

# example:  f_scatter_plot(input_data = DF, var_x.arg= var, var_y.arg ='r_TS') 

#### function to get the lm model ####
df_input= DF
var_outcome = 'well_score_9dom_well0'
var_x = 'core_gender_well0'
var_covar = c('portal_age_well0', 'dv_edu_3cat_well0')

f_linear_model = function(df_input.arg , var_outcome.arg,  var_x.arg, var_covar.arg) {
  
  #### Prepare vairables
  df_input       = df_input.arg
  var_outcome    = var_outcome.arg
  var_x          = var_x.arg
  var_covar      = var_covar.arg
  
  #### Remove na
  df_input_complete = df_input %>% 
    dplyr::select(var_outcome, var_covar, var_x) %>%
    dplyr::filter(complete.cases(.))
  
  #### Model
  var_predictor = c(var_x, var_covar)
  m_formula = as.formula( paste(var_outcome, paste(var_predictor, collapse = " + "), sep = " ~ ") )
  model = lm( m_formula , data = df_input_complete)
  
  #### Output
  return(model)
}

# Example
# df_input= DF
# var_outcome = 'well_score_9dom_well0'
# var_x = 'core_gender_well0'
# var_covar = c('portal_age_well0', 'dv_edu_3cat_well0')
# 
# mymodel = f_linear_model(df_input.arg=DF,
#                          var_outcome.arg = 'well_score_9dom_well0', 
#                          var_x.arg = 'core_gender_well0', 
#                          var_covar.arg = var_covar)



#### LM function-1 sample_size_glm ####

f_size_lm = function(model.arg , var_outcome.arg){
  #### Prepare the variable
  data = model.arg$model 
  My_Var = colnames(data)
  Y= var_outcome.arg
  
  #### For loop
  N_df = data.frame()
  for (i in 1:length(My_Var)){
    
    #### Set var
    var = My_Var[i]
    # print(var)
    
    #### If var is contiuous or categorical
    if (   is.numeric(data[, var ])   ) {
      #### Con.Var: Sample size
      n = data %>% dplyr::select(Y) %>% nrow()
      df= data.frame(N = n)
      df= add_column(df, Levels = NA, .before=1) 
      df= add_column(df, Variable = var, .before=1) 
    }else{
      #### Cat.Var: Sample size
      df = data %>% count_(var) 
      colnames(df) = c('Levels','N')
      df = add_column(df, Variable = var, .before=1) 
    }
    
    #### Append dataframe
    N_df = rbind(N_df, df)
    
  }# End of for loop
  
  #### DataFrame formation
  N_df$Levels  = as.character(  N_df$Levels  )
  #### Creat a key for matching model coef 
  N_df$Levels[ is.na(N_df$Levels)] = '' # This is to ensure the key for numeric prediction is correct
  key_vector = paste0(N_df$Variable, N_df$Levels)
  N_df= add_column(N_df, key = key_vector, .before=1)    
  
  #### Return
  return(N_df)
}# End of function


#### Example
# N_df = f_size_lm(model.arg = mymodel, var_outcome.arg = 'well_score_9dom_well0' )

