####Setup####
  #Activate Library 
    library(tidyverse)
    library(haven)

####Get cleaned data####


      
      
####Data cleaning####
      DATA$mandate<-as_factor(DATA$mandate)
      

####Exercise 1: Deviation and electoral safety####
          
      #Graph
      DATA %>%
        ggplot()+
        aes(x=elecsafe_overall, y=deviation)+
        geom_point()+
        theme_bw()+
        geom_smooth(method="lm")
      
      #Model
        mod_1 <- lm (deviation ~ elecsafe_overall, data=DATA)
        
      
 
    
    
    
    
    

    
    