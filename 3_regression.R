####Was ist eine Regression?####
  #Activate Library 
  library(tidyverse)
  
  ####*BeispielDaten####
  #x =1000 random numbers between -10 and plus 10
  x<-runif(1000, min=-10, max=10)
  #Y= x * 2 
  y_real<-(2*x)
  
  #Intercept
  intercept<-3
  
  #Messfehler mit Mittelwert 0 
  error<- rnorm(n=1000,mean=0, sd=10)
  
  #Generate observed variables
  y<-y_real+error+intercept
  
  ####*Einfaches Punktediagram####
  ggplot()+
    aes(x=x, y=y)+
    geom_point()
  
  #Linie Zeichnen
  ggplot()+
    aes(x=x, y=y)+
    geom_point()+
    geom_smooth(method="lm")+
    geom_vline(xintercept=0, linetype="dashed")+
    geom_hline(yintercept=0, linetype="dashed")+
    theme_bw()

  ####*Regression Model####
  summary(lm(y~x))
  
  ###Bias und fehlende Variablen#
  
  #Missing variable 
  z<-runif(1000, min=-10, max=10)
  
   
  #New "real y# 
  y_real<-(2*x)+(40*z)
  
  #Generate observed variables
  y<-y_real+error+intercept
  
  summary(lm(y~x+z))

  
  
####Setup####
  ####*Packages
    library(haven)
    library(stargazer)
    library(texreg)
    library(sjPlot)
    library(ggrepel)

  ####*Daten Einlesen und anschauen####
  data_fed<-read_csv("fd-cross.csv")

  ####*Daten Saeubern  - Numerisch zu Faktor ####
  
  summary(data_fed$feddummy)
  
  data_fed<-
    data_fed %>% 
    mutate(feddummy = factor(dplyr::recode(feddummy,
                                    `0`="Nein",
                                    `1`="Ja")))
  
  
####Regression 1: Bivariate Regression####
    #Wachstum 
    fig_1<-
      data_fed%>%
      select(gdpppp94,democ,country)%>%
      drop_na()%>%
      ggplot()+
        aes(x=democ,y=gdpppp94, label=country)+
        geom_point()+
        geom_text_repel()+
        theme_bw()+
        labs(x="Anzahl der demokratischen Jahre 1900-2003", y="BIP zu Marktpreisen pro Kopf")
    fig_1
  
  #GDP und Demokratie 
  mod_1<-lm(gdpppp94~democ, data=data_fed)
  summary(mod_1)
  
  fig_1+
    geom_smooth(method = "lm")
  
  
  #GDP und Groesse
  fig_2<-
      data_fed%>%
      select(gdpppp94,area,country)%>%
      drop_na()%>%
      ggplot()+
        aes(x=area,y=gdpppp94, label=country)+
        geom_point()+
        geom_text_repel()+
        theme_bw()+
        labs(x="Fl?che in km2", y="BIP zu Marktpreisen pro Kopf")
    fig_2
  
  mod_2<-lm(gdpppp94~area, data=data_fed)
  summary(mod_2)
  
  fig_2+
    geom_smooth(method="lm")
  
  
####Regression 2: Multivariate 1- Faktor/Dummy ####
    fig_3<-
        data_fed%>%
        select(gdpppp94,democ,country,feddummy)%>%
        drop_na()%>%
        ggplot()+
          aes(x=democ,y=gdpppp94, color=feddummy)+
          geom_point()+
          theme_bw()+
          labs(x="Anzahl der demokratischen Jahre 1900-2003", y="BIP zu Marktpreisen pro Kopf", color="Foederal")
        fig_3
      
  
    mod_3<-lm(gdpppp94~democ+feddummy, data=data_fed)
    summary(mod_3)  
        
    
    #Visual
    data_fed%>%
        select(gdpppp94,democ,feddummy)%>%
        drop_na()%>%
        cbind(., pred=predict(mod_3))%>%
        ggplot()+
          aes(x=democ, y=gdpppp94, color=feddummy)+
          geom_point(alpha=1, size=1)+
          geom_line(mapping = aes(y=pred), size=1)+
        theme_bw()

    
####Regression 3: Multivariate full####
    mod_4<-lm(gdpppp94~democ+feddummy+dezrev2+popdens+area+incineq1+ethnic+religion, data= data_fed)
    
    summary(mod_4)
    

####Presenting Results####

  ####*Table of Results-Texreg####

  screenreg(list(mod_1,mod_3,mod_4))
  
  screenreg(list(mod_1,mod_3,mod_4),
            custom.coef.names = 
              c("Intercept",
                "Demokratische Jahre","Unitaerer Staat","Dezentralisierung","Bevoelkerungsdichte",
                "Flaeche","Ungleichheit-GINI", "Ethnische Fragmentierung","Religioese Fragmentierung"))
  
  
   name_vec<-c("Intercept",
                "Demokratische Jahre","Unitaerer Staat","Dezentralisierung","Bevoelkerungsdichte",
                "Flaeche","Ungleichheit-GINI", "Ethnische Fragmentierung","Religioese Fragmentierung")
  
  #Export to html 
  htmlreg(list(mod_1,mod_3,mod_4),
            custom.coef.names = name_vec,
          file="results.html")
                                  
  
  ####*Graphische Darstellung####
  plot_models(mod_4)
  
  plot_models(mod_4, std.est="std2")
  
  #Labels
  rev(name_vec[-1])
  
  
  plot_models(mod_4,
              std.est="std2",
              axis.labels = rev(name_vec[-1]))+
    theme_bw()+
    geom_hline(mapping=aes(yintercept=0), color="black", linetype="dashed")+
    labs(x="Variablen", title="BIP pro Kopf")+
    theme(legend.position = "none")
                 



  