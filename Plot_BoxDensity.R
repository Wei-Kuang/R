


library('tidyverse')
library('lazyeval')
library('grid')

######################
#### Density Plot ####
######################


####################################################
#### functoin-0 Remove any NA/'Missing'/DK/9999 ####
####################################################
fun_remove_missming= function (dataframe, var1){
  require("lazyeval")
  require("dplyr")
  criteria <- interp(~ ! v %in% m & !is.na(v),
                     .values=list(
                       v = as.name(var1),
                       m = c(NA,'Missing','DK',9999)
                     )
  )
  
  #### Remove
  out = dataframe %>% dplyr::filter_( criteria)
  #### Drop level ( if the var1 is a factor) !!!!!
  if (  class(out[[var1]] ) =='factor'  ) {
    out[[var1]] = droplevels(  out[[var1]] )
  }
  
  #### out is a data frame
  return(out)
  
}

#### example
DF_testing  = data.frame( X= c(1:5, NA, 'Missing', 9999, 6:10), 
                          Y=c(NA, 'Missing', 1:5,9999, 6:10))
DF_testing
fun_remove_missming(DF_testing, var1 = 'X')



####################################
#### functino - p_val_converter ####
####################################


# Ann's p-value  rule
# If P values are reported, follow standard conventions for decimal places: 
# 
# for P values less than .001, report as "P<.001"; 
#  if p < 0.001, then p = "P<.001"
# for P values between .001 and .01, report the value to the nearest thousandth; 
#   if   0.001 < p and p < 0.01, then  p = round(p, 3)
# for P values greater than or equal to .01, report the value to the nearest hundredth; 
#   if p >= 0.01, then p = round(p,2)
# and for P values greater than .99, report as "P>.99." 
#   if P >.99, then p= "P>.99." 


# For studies with exponentially small P values (eg, genetic association studies), P values may be reported with exponents (eg, P=1×10−5)

p_val_converter = function( p ) {
  
  # if ( is.na(p) ){
  #   x = "NA"
  # }else if (p > 0.99){
  #           x= 'P>.99'
  # } else if ( p >= 0.01) {
  #             x = paste0('P=',round(p,2) )
  # } else if (0.001 <= p & p < 0.01) {
  #             x = paste0('P=', round(p,3) )
  # }else if (p < 0.001) {
  #           x = 'P<.001'
  # }
  
  if ( is.na(p) ){
    x = "NA"
  }else if (p > 0.99){
    x= 'P>.99'
  } else if ( p >= 0.01) {
    x = paste0('P=',round(p,2) )
  } else if (0.001 <= p & p < 0.01) {
    x = paste0('P=', round(p,3) )
  }else if ( 0.0001<= p & p < 0.001) {
    x = paste0('P=', round(p,4) )
  }else if( p <0.0001){
    x = 'P < 0.0001'
  }
  
  x = as.character(x)
  return(x)
}


p_val_converter (NA)
p_val_converter (1)
p_val_converter (0.5)
p_val_converter (0.02)
p_val_converter (0.01)
p_val_converter (0.019)
p_val_converter (0.001)
p_val_converter (0.0019)
p_val_converter (0.0002)
p_val_converter (0.00019)
p_val_converter (0.000019)




#########################
#### anova function} ####
#########################
f_anova =function(Y.arg, X.arg, data.arg){
  Y=Y.arg
  X=X.arg
  formula = paste0(Y,'~',X)
  test = aov(as.formula(formula), data=data.arg)
  pval = summary(test)[[1]] [["Pr(>F)"]][1] %>% p_val_converter()
  return(pval)
}


f_anova_stratification =function(Y.arg, X1.arg, X2.arg, data.arg){
  Y=Y.arg
  X1=X1.arg
  X2=X2.arg
  predictors = paste(c(X1,X2), collapse = ' : ')
  formula = paste0(Y,' ~ ',predictors)
  test = aov(as.formula(formula), data=data.arg)
  pval = summary(test)[[1]] [["Pr(>F)"]][1]%>% p_val_converter()
  return(pval)
}

# f_anova_stratification(Y.arg ='dom_9_total_score',X1.arg = 'action_new', X2.arg='age_new',data.arg = dat_China )



######################
#### ggplot theme ####
######################
My_Theme_1 = theme(
  # hjust = 0.5 : Center of the title
  plot.title = element_text(size=12, face="bold"), 
  
  axis.title.x = element_text(size =12, face="bold"),
  axis.text.x = element_text(size = 12, face="bold"),
  
  axis.title.y = element_text(size =12,face="bold"),
  axis.text.y = element_text(size = 12,face="bold"),
  
  legend.title = element_text(size = 12, face = "bold" ),   
  legend.text = element_text(size = 12, face = "bold"),
  
  # Adjust facet
  strip.text.x = element_text(size = 12, face = "bold")
)


###############################################
#### function- densityplot: Y.con by X.cat ####
###############################################


#### a density plot plot function to draw Y.con (continuous data) by subgroups in X.cat (categorical data) ####
f_densityplot_cont_by_cate = function(
  df_input, 
  cont.var.arg, 
  cate.var.arg, 
  legend.lab.arg, 
  xlab.arg, title.arg, col_pal.arg,
  x.limit.arg=c(0,95) , 
  x.breaks.arg= seq(0,95,5),
  y.max.arg=0.065,
  y.break.arg=1,
  theme.arg= My_Theme_1){
  
  #### Get the variable ready for this function 
  cont.var = cont.var.arg  # eg., well score
  cate.var = cate.var.arg  # eg,m age groups (30, 30-50, 50-60, 60+)
  
  #### remove missing ####
  df_plot = df_input %>% 
    fun_remove_missming(dataframe =., var1 = cont.var) %>%
    fun_remove_missming(dataframe =., var1 = cate.var)
  
  #### color ####
  col.n = unique(df_plot[[cate.var]]) %>% length()
  ## Gray tone
  # col_pal = palette(gray.colors(n = col.n, start = 0.8, end = 0.4,alpha = 0.8))
  ## Zissou tone
  require("wesanderson")
  col_pal <- wes_palette("Zissou1", n = col.n , type = "continuous") 
  if ( !is.null(col_pal.arg)) {col_pal=col_pal.arg }
  
  #### statistics ####
  # N.M table
  df_Stat = df_plot %>% group_by_(cate.var) %>%
    summarise_( Mean    = interp( ~ mean(v,   na.rm= T), v= as.name(cont.var)),
                Median  = interp( ~ median(v, na.rm= T), v= as.name(cont.var)),
                SD      = interp( ~ sd(v,     na.rm= T), v= as.name(cont.var)),
                N       = interp( ~ length(z          ), z= as.name(cate.var))
    )%>%
    mutate_(N_Label = interp( ~paste0(level,' N=',count), 
                              level =as.name(cate.var), count=as.name('N'))       
    ) 
  
  # claculate p-vale
  p_vale = f_anova(Y.arg = cont.var, X.arg = cate.var, data.arg = df_plot )
  grob <- grid::grobTree(
    grid::textGrob(
      label = p_vale, x=0.05,  y=0.97, hjust=0, 
      gp=gpar(col="darkred", fontsize=10, fontface="italic")
    )
  )
  
  #### Plot #### 
  p = ggplot(data=df_plot, aes_string(x= cont.var , color= cate.var)) + 
    geom_line(stat="density", size=0.8) +
    # Legend
    scale_color_manual(name= legend.lab.arg , values=col_pal, labels= df_Stat$N_Label) +
    # Vertical line
    geom_vline(xintercept = df_Stat$Mean, color=col_pal, linetype="dashed", size=0.8, alpha=0.5) +
    # pvalue
    annotation_custom(grob)+
    # Title and label
    ggtitle(title.arg)+ xlab(xlab.arg)+ ylab( "Density" )+ 
    # x.y-axis markers
    scale_x_continuous(limits = x.limit.arg, breaks= x.breaks.arg ) +
    scale_y_continuous(limits = c(0,y.max.arg), breaks= seq(0,y.max.arg, y.break.arg ) )+
    # Remove the grid line
    theme_bw()+ 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor =element_blank(),
          legend.position="right") +  theme.arg
  
  # print it
  print(p)
  
  # output
  return(df_Stat)
  
}


#### fucntion testing ####

# result = f_densityplot_cont_by_cate(
#                  df_input =DF, 
#                  cont.var.arg = 'CFA19items_well_score', 
#                  cate.var.arg = 'core_nature_min_new',
#                  col_pal.arg = NULL, 
#                  legend.lab.arg='', 
#                  xlab.arg = 'WELL score',
#                  title.arg ='', 
#                  x.limit.arg = c(0,100), 
#                  x.breaks.arg =seq(0,100,5),
#                  y.max.arg= 0.035)






##################
#### Box Plot ####
##################

#### function ####
f_boxplot = function(df_input, 
                     cate.var.arg, 
                     cont.var.arg,
                     y.limit.arg , 
                     y.breaks.arg,
                     xlab.arg, 
                     ylab.arg,
                     title.arg, 
                     theme.arg= My_Theme_1,
                     round.digit.arg =2){
  
  #### prepare the variables
  cate.var  = cate.var.arg
  cont.var  = cont.var.arg
  
  #df_input = df_input %>% select( all_of(c(cate.var, cont.var)))
  
  #### remove missing  ####
  df_input = df_input %>% 
    fun_remove_missming(dataframe =., var1 = cate.var) %>%
    fun_remove_missming(dataframe =., var1 = cont.var)
  #### remove levels ####
  df_input[[cate.var]] = droplevels(  df_input[[cate.var]] )
  

  
  #### statistics ####
  # N.M table
  df_Stat = df_input %>% group_by_(cate.var) %>%
    summarise_( Mean    = interp( ~ mean(v,   na.rm= T), v= as.name(cont.var)),
                Median  = interp( ~ median(v, na.rm= T), v= as.name(cont.var)),
                SD      = interp( ~ sd(v,     na.rm= T), v= as.name(cont.var)),
                N       = interp( ~ length(z          ), z= as.name(cate.var))
    )%>%
    mutate_(N_Label = interp( ~paste0(level,'\n N=',count), 
                              level =as.name(cate.var), count=as.name('N'))       
    ) 
  
  
  #### color ####
  col.n = unique(df_input[[cate.var]]) %>% length()
  #col_pal = palette(gray.colors(n = col.n, start = 0.9, end = 0.3,alpha = 0.8))
  require(RColorBrewer)
  col_pal <- brewer.pal(n=col.n, "Blues")
  
  #### claculate p-vale ####
  p_vale = f_anova(Y.arg = cont.var, X.arg = cate.var,data.arg = df_input )
  grob <- grid::grobTree(textGrob(label = p_vale, x=0.05,  y=0.97, hjust=0,
                                  gp=gpar(col="darkred", fontsize=10, fontface="italic")))
  
  #### Plot ####
  p= ggplot(data = df_input, aes_string(x=cate.var ,y=cont.var)) +
    #geom_violin(fill='grey') +
    geom_boxplot(alpha =0.5, fill= col_pal , width=0.9)+
    geom_text(data = df_Stat, 
              aes(label = round(Mean, round.digit.arg ) , y = Median + Median*(1/25)),
              col='black',fontface='bold')+
    scale_x_discrete(labels=df_Stat$N_Label )+
    # pvalue
    annotation_custom(grob)+
    # legend text and color
    scale_y_continuous(limits = y.limit.arg,  breaks= y.breaks.arg)+
    # title
    ggtitle( title.arg ) + xlab(xlab.arg) + ylab(ylab.arg)+
    # Remove the grid line
    theme_bw()+ 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor =element_blank(),
          legend.position="right") +  theme.arg  
  
  
  print(p)
  return(df_Stat)
  # Save
  # save plot to a specific folder
  # dir.create("output_Plot") # create a folder
  # filename = paste(X,Y,'.png',sep = '_')
  # ggsave(filename = filename, plot= p , width = 10, height = 10, units = "cm")
}


####  example ####


# f_boxplot(df_input = dat_China , 
#             #### Varaibles
#             cate.var=X, cont.var='dom_9_total_score',
#             title.arg = '', xlab.arg='Outdoor activity', 
#             ylab.arg='WELL score (9 domains)',
#             #### Y axis
#             y.limit.arg = c(10,92), y.breaks.arg = seq(20,92,20)
# )
