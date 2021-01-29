#### My_Theme_1
require('ggplot2')
My_Theme =
  # Remove the grid line
  theme_bw()+  
  theme(
  # hjust = 0.5 : Center of the title
  plot.title = element_text(size=12, face="bold"), 
  
  axis.title.x = element_text(size =12, face="bold"),
  axis.text.x = element_text(size = 12, face="bold"),
  
  axis.title.y = element_text(size =12,face="bold"),
  axis.text.y = element_text(size = 12,face="bold"),
  
  # legend
  legend.title = element_text(size = 12, face = "bold" ),   
  legend.text = element_text(size = 12, face = "bold"),
  legend.position="right",
  
  # Adjust facet
  strip.text.x = element_text(size = 12, face = "bold"),
  
  # Grid
  panel.grid.major = element_blank(),
  panel.grid.minor =element_blank()
  )





########################################################
#### BarPlot:  Single Variable with multiple levels ####
########################################################

f_barplot_onevar= function(data.arg, var_target, 
                           y.limits.arg = c(0,100), 
                           y.breaks.arg =seq(0,100,10), 
                           coord_flip.arg=FALSE,
                           reorder.arg=FALSE,
                           My_Theme.arg = My_Theme) {
  #### Library
  require("tidyverse")
  require('xlsx')
  require('data.table')    # copy
  require("wesanderson")   # color pal
  
  
  #### Var setting
  df = data.table::copy(data.arg)
  
  #### Remove the "missing category" from Var and Grouping.Var
  require("lazyeval")
  filter_criteria <- interp(~ !v1 %in% x ,
                            .values=list(
                              v1 = as.name(var_target),
                              x = c('Missing')
                            )
  )
  
  df = df %>% filter_(filter_criteria)
  
  
  #### Sample size computation
  dfy = df  %>% summarize( Count.y =n())
  dfy$label_y = dfy[,'Count.y']
  
  dfx = df %>% 
    dplyr::group_by_(var_target) %>%
    dplyr::summarize( Count.x = n()) 
  
  #### Merge    
  merge_df = dfx %>% mutate( Count.y = dfy$Count.y) %>% data.frame()
  merge_df$Percentage = round((merge_df$Count.x/  merge_df$Count.y)*100, 0)
  # View(merge_df)
  
  #### New Label of Sample size
  label_x = c()
  for (i in 1:nrow(merge_df)) {
    label_x[i] = paste0( merge_df[,var_target][i], '\nN= ', merge_df[,'Count.x'][i])
  }
  merge_df$label_x = label_x
  
  #### Reorder the x-label ####
  merge_df = merge_df %>% arrange_(var_target)
  merge_df$label_x = factor(merge_df$label_x, levels = merge_df$label_x) # use the original order  by the recoded variable-1
  
  
  if( reorder.arg==TRUE) {
    merge_df = merge_df %>% arrange(Count.x)
    merge_df$label_x = factor(merge_df$label_x, levels = merge_df$label_x)  
  }
  
  
  #### Title  
  my_title = paste0('Total N= ', dfy$label_y[1])
  my_legend_title = paste0(var_target)
  
  #### Plot ####
  p1 = merge_df %>% 
    ggplot(aes_string(x='label_x', y='Percentage'))+ 
    geom_bar(stat= 'identity', fill= 'gray70')+  
    ggtitle( my_title )+ xlab( '' )+ ylab('Percentage')+  
    #### Final adjustment
    scale_y_continuous(limits = y.limits.arg ,breaks= y.breaks.arg )+
    # Remove the grid line
    My_Theme.arg 
  # theme( axis.text.x = element_text(size = 13, face="bold", angle = 90, hjust = 1)) # turn the xlabel to be vertical
  
  if (coord_flip.arg == TRUE){
    p.final = p1+
      geom_text(aes(label=Percentage), vjust=0.8, hjust = -0.2, color="black", fontface="bold", position =  position_dodge(0.9), size=5)+
      coord_flip()
  }else{
    p.final = p1+
      geom_text(aes(label=Percentage), vjust= -0.2, hjust = 0.8, color="black", fontface="bold", position =  position_dodge(0.9), size=5)
  }
  
  #### Print Plot
  print(p.final)
  
  #### Output
  return(merge_df)
}     


#### example 
# f_barplot_onevar(DF = DF,var_target = 'gender_new',coord_flip.arg = F)


####################################
#### Composition plot  function ####
####################################

f_composition_plot = function(data.arg, var_target, var_group, col_pal.arg=NULL, title.arg='', my_legend_title = '', xlab.arg='' ,
                            coord_flip= FALSE, my_theme = My_Theme,text.arg=TRUE, digit.arg =1, output_setting = 'percentage'){
  
  #### Var for Developing
  # var_target =  'BMI_NEW_4.2'
  # var_group ='STAGE_NEW_CASECONTROL_3'
  # var_group =  'STATUS_NEW_CC'
  
  #data.arg=df_new_control; var_target ='Disease' ; var_group ='STATUS_NEW_CC';
  
  # data.arg = DF
  
  #### Library
  require("tidyverse")
  require('xlsx')
  require('data.table')    # copy
  require("wesanderson")   # color pal
  
  # Reorder the var Levels
  df = data.table::copy(data.arg)
  df[[var_target]] = factor(df[[var_target]] , levels = levels(data.arg[[var_target]]) )
  
  #### Remove the "missing category" 
  require("lazyeval")
  filter_criteria <- interp(~ !v1 %in% x & !g1 %in% x & !is.na(v1) & !is.na(g1),
                            .values=list(
                              v1 = as.name(var_target),
                              g1 = as.name(var_group),
                              x = c('Missing')
                            )
  )
  
  df = df %>% filter_(filter_criteria)
  ##### Remove the level with zero length  
  # df[[var_target]] = droplevels(df[[var_target]])
  # df[[var_group]] = droplevels(df[[var_group]])
  
  #### Sample size computation
  # numerator  
  dfy = df %>% 
    dplyr::group_by_(var_group) %>%
    dplyr::summarize( Count = n())
  # denominator 
  dfx = df %>% 
    dplyr::group_by_( var_group, var_target) %>%
    dplyr::summarize( Count = n(), .drop= FALSE)
  
  #### Merge  N and Percentage ####   
  merge_df = merge(x= dfx, y=dfy, by= var_group) 
  merge_df = merge_df[, c(2,1, 3:ncol(merge_df))]
  merge_df$Percentage = round((merge_df$Count.x /  merge_df$Count.y)*100, digit.arg )
  merge_df = merge_df %>% arrange_(var_group, var_target)

  
  #### Group1.N.size Label
  merge_df$label_x = paste0(merge_df[[var_group]],'\nN=', merge_df$'Count.y')
  merge_df$label_x = factor(merge_df$label_x )
  merge_df$key = paste(merge_df[[var_target]], merge_df[[var_group]], merge_df[['Count.x']],sep = '/')
  merge_df[['label_x']] = factor(merge_df$key, levels =merge_df$key, labels = merge_df$label_x)
  merge_df$label_x = droplevels(merge_df$label_x) 
    
  #### Color ####
  # color setting
  cn = df %>% select(var_target)  %>% pull() %>% unique() %>% length()  # cn: color number
  col_pal <- wes_palette("Zissou1", cn , type = "continuous") 
    
  if ( !is.null(col_pal.arg)) {
    col_pal=col_pal.arg
  }
    

  ##### Plot #####
  my_title = paste0(title.arg)
  
  p1 = ggplot(data= merge_df, aes_string(x='label_x', y='Percentage', fill= var_target))+ 
      geom_bar(stat= 'identity')+  
      #### Label
      ggtitle( my_title )+ xlab( xlab.arg )+ ylab( "Percentage")+
      #### legend text and color
      scale_fill_manual(name= my_legend_title, values = col_pal) +
      scale_y_continuous(limits = c(0,102),breaks=seq(0, 100, 10) )
    
  #### Rotation ####
    if (coord_flip ==  TRUE){
      p2 = p1+my_theme+ coord_flip()
    }else{
      p2 = p1+my_theme
    }
    
  ##### Text #####
  text_pct = geom_text(aes_string(label= 'Percentage'), 
                       size=4, fontface="bold", color="black", # 'white'
                       position = position_stack(vjust = 0.5) )
  
  if ( text.arg ==  TRUE){
    p3 = p2 + text_pct
  }else{
    p3 = p2
  }
  
  #### Final Plot ####
  print(p3)
    
  #### Report of Table ####
  merge_df =  merge_df %>% arrange_(var_group, var_target)
  
  
  #### Extract N
  crosstab = merge_df %>% 
    dplyr::select_(var_target, var_group, 'Count.x') %>% 
    spread_(key=var_group, value = 'Count.x')
  crosstab[[var_target]] = as.character( crosstab[[var_target]])
  colnames(crosstab)[-1] = paste0('N ', colnames(crosstab)[-1])
  df_N =crosstab 
  
  #### Extract Percent
  crosstab = merge_df  %>% 
    dplyr::select_(var_target, var_group, 'Percentage') %>% 
    spread_(key=var_group, value = 'Percentage')
  crosstab[[var_target]] = as.character( crosstab[[var_target]])
  colnames(crosstab)[-1] = paste0('P ',colnames(crosstab)[-1])
  df_P = crosstab
  
  
  #### Final NP table
  Final_Output = merge(x=df_N,
                       y=df_P,
                       by= var_target)

return(Final_Output)
}

#### example
# f_composition_plot(data.arg=DF, var_target='age_new_5cat', var_group='gender_new', 
#                    col_pal.arg=NULL, 
#                    title.arg ='', 
#                    my_legend_title = '', 
#                    xlab.arg='' ,
#                    coord_flip= FALSE, 
#                    my_theme = My_Theme, output_setting = 'percentage')


##################################
#### Missing composition plot ####
##################################
Missing_plot = function(data.arg, var_target, var_group, note='', my_legend_title =  '' , 
                        coord_flip= FALSE , my_theme = My_Theme){
  
  #### Library
  require("tidyverse")
  require('xlsx')
  require('data.table')    # copy
  require("wesanderson")   # color pal
  
  
  # var_target =  'STAGE_NEW_CASECONTROL_3'
  # var_group ='CENTER_NEW_2'
  # data.arg = copy(DF)
  
  #### Recode Level in Missing and Existing 
  vector = c()
  for ( i in 1:nrow(data.arg) ) {
    if( is.na(data.arg[i,var_target]) ){
      vector[i] = 'Missing'
    }else if ( data.arg[i,var_target] == 'Missing') {
      vector[i]= 'Missing'
    }else{
      vector[i]= 'Existing'
    }
  }
  vector= factor(vector , levels = c( 'Existing', 'Missing') )
  
  #### Replace
  data.arg[, var_target] = vector
  
  #### Sample size computation
  dfy = data.arg %>% 
    dplyr::group_by_(var_group) %>%
    dplyr::summarize( Count = n())
  # View(dfy)
  
  
  dfx = data.arg %>% 
    dplyr::group_by_( var_group, var_target) %>%
    dplyr::summarize( Count = n())
  # View(dfx)
  
  #### Merge    
  merge_df = left_join(x= dfx, y=dfy, by= var_group) 
  merge_df$Percentage = round((merge_df$Count.x /  merge_df$Count.y)*100,0)
  # View(merge_df)
  
  #### Group1.N.size Label
  merge_df$label_x = paste0(merge_df[[var_group]],'\nN=', merge_df$'Count.y')
  merge_df$label_x = factor(merge_df$label_x )
  merge_df$key = paste(merge_df[[var_target]], merge_df[[var_group]], merge_df[['Count.x']],sep = '/')
  merge_df[['label_x']] = factor(merge_df$key, levels =merge_df$key, labels = merge_df$label_x)
  merge_df$label_x = droplevels(merge_df$label_x)

  #### Visualization
  # color setting
  cn = length(  unique(data.arg[,var_target]))  # cn: color number
  pal <- rev( wes_palette("Zissou1", cn , type = "continuous") )
  # Title  
  my_title = paste0(note)
  # my_legend_title = strsplit(var_target, split = '_NEW')[[1]][1]
  
     
  #Plot
  p = ggplot(data= merge_df, aes_string(x='label_x', y='Percentage', fill= var_target))+ 
    geom_bar(stat= 'identity')+  
    
    ggtitle( my_title )+ xlab("")+ ylab( "Percentage")+
    #### legend text and color
   # scale_fill_manual(name =my_legend_title, breaks = c('Existing', 'Missing'), values = c("grey", "darkred") )+
    scale_fill_manual(name =my_legend_title, breaks = c('Existing', 'Missing'), 
                      values = wes_palette("Zissou1", cn , type = "continuous") ) +
    scale_y_continuous(limits = c(0,102),breaks=seq(0, 100, 10) )
  
    #### Print Plot
    if (coord_flip ==  TRUE){
      print(p+my_theme + coord_flip() )
    }else{
      print(p+my_theme )
    }
  return(merge_df)
}



#################
#### Example ####
#################


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
