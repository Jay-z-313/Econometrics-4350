library(wooldridge)
library(tidyverse)
library(modelsummary)
library(broom)
ceosal1 %>% 
  select(salary, roe)
%>% head(6) 

ceosal1%>% 
  lm(salary~roe,.) -> ceosal1.fit
ceosal1.fit %>% 
  modelsummary
( statistic = NULL, gof_map=c("nobs", "r.squared") ) 
ceosal1_dec <- ceosal1 %>% mutate(roedec = roe/100)
ceosal1_dec %>% select (
  salary, roe, roedec 
) %>%  head(5)
ceosal1_dec.fit<-lm(salary~roedec,data=ceosal1_dec) 
tidy(ceosal1_dec.fit) 
glance(ceosal1.fit)%>% select(r.squared) 

list( "roe:percentage"=ceosal1.fit,
      "roe:decimal"=ceosal1_dec.fit )
%>% modelsummary( statistic=NULL, gof_map=c("r.squared") 
                list(
                  "roe:percentage"=ceosal1.fit, 
                  "roe:decimal"=ceosal1_dec.fit
                ) %>%  
                  modelsummary(
                  statistic= NULL, 
                  gof_map="r.sqaured"
                )
    ceosal1_dol <- ceosal1 %>% mutate(
      salarydol = salary*1000
      ) 
    ceosal1_dol %>%
      lm(salarydol ~ roe, data = .) ->
      ceosal1_dol.fit
    ceosal1_dol.fit %>% glance()
    list( 
      "salary:\\$1000"=ceosal1.fit,
          "salary:\\$"=ceosal1_dol.fit 
      ) %>%  modelsummary( 
      statistic=NULL, gof_map="r.squared") 
    
    #This project analyzes the relationship between CEO compensation and 
    #firm performance using simple linear regression. Using the ceosal1 
    #dataset from the wooldridge package, I estimate multiple models relating 
    #CEO salary to return on equity (ROE).
    
    #First, I regress salary on ROE measured in percentage terms. I then
    #re-estimate the model after converting ROE to decimal form to examine 
    #how changes in variable units affect coefficient magnitudes. Next, I 
    #rescale salary from thousands of dollars to dollars and re-estimate the 
    #model to study the effect of dependent-variable scaling.
    
    #Across specifications, I compare estimated coefficients and 
    #goodness-of-fit measures using modelsummary. The analysis demonstrates 
    #that rescaling variables changes the numerical size of regression 
    #coefficients but leaves model fit (R²) unchanged. This confirms key 
    #theoretical properties of ordinary least squares estimation and 
    #illustrates how measurement units influence economic interpretation 
    #without altering statistical relationships.
    