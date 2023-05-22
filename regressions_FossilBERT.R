###
# Regressions for fossilBERT bachelor thesis
# author: Lucas Hemmi
# email: lucas.hemmi@student.unisg.ch
###



# loading libraries ----

#install.packages("plm", from = "source")
library("plm")
#install.packages("lfe", from = "source")
library("lfe")

#install.packages("tidyverse", from = "source")
library("tidyverse")
#install.packages("ggcorrplot", from = "source")
library("ggcorrplot")

#install.packages("fastDummies", from = "source")
library("fastDummies")

#install.packages("stargazer", from = "source")
library("stargazer")

library("lmtest")

#install.packages("table1")
library("table1")



rm(list = ls())

# Data ----

## loading data ----
data_selected_politicians = read.csv("selected_politicians_panel_data.csv", sep = ";")
data_us_politicians = read.csv("us_politicians_data.csv", sep = ";")
data_uk_politicians = read.csv("uk_politicians_data.csv", sep = ";")
data_australia_politicians = read.csv("australia_politicians_data.csv", sep = ";")

## data handling ----


# changing variable types
data_selected_politicians = dummy_cols(data_selected_politicians, select_columns = c("party"), remove_first_dummy = TRUE,)
data_selected_politicians$party = as.factor(data_selected_politicians$party)
data_selected_politicians$year = as.factor(data_selected_politicians$year)
data_selected_politicians$COP_name = as.factor(data_selected_politicians$COP_name)
data_selected_politicians$followers_millions = data_selected_politicians$followers / 1000000




data_us_politicians = dummy_cols(data_us_politicians, select_columns = c("party"), remove_first_dummy = TRUE)
data_us_politicians$party = as.factor(data_us_politicians$party)

data_us_politicians$region = as.factor(data_us_politicians$region)
data_us_politicians$age_2021 = 2021 - data_us_politicians$birth_year
data_us_politicians$followers_millions = data_us_politicians$mean_followers / 1000000
data_us_politicians$followers_interval = cut(data_us_politicians$mean_followers,c(0,50000,100000,1000000,Inf))




data_uk_politicians$party = as.factor(data_uk_politicians$party)
data_uk_politicians$followers_millions = data_uk_politicians$mean_followers / 1000000
data_uk_politicians$followers_interval = cut(data_uk_politicians$mean_followers,c(0,50000,100000,1000000,Inf))


data_australia_politicians$party = as.factor(data_australia_politicians$party)
data_australia_politicians$followers_millions = data_australia_politicians$mean_followers / 1000000
data_australia_politicians$followers_interval = cut(data_australia_politicians$mean_followers,c(0,50000,100000,1000000,Inf))


## descriptive analytics ----
### summary stats ----
summary(data_us_politicians)

table1::label(data_us_politicians$total_tweets) <- "Total Tweets"
table1::label(data_us_politicians$climate_related) <- "Climate Tweets"
table1::label(data_us_politicians$downplaying) <- "Downplaying Tweets"
table1::label(data_us_politicians$mean_followers) <- "Mean Followers"
table1::label(data_us_politicians$birth_year) <- "Birthyear"
table1::label(data_us_politicians$climate_ratio) <- "Climate Ratio"
table1::label(data_us_politicians$downplaying_ratio) <- "Downplaying Ratio"
table1::table1(~total_tweets + climate_related + downplaying + mean_followers + birth_year + climate_ratio + downplaying_ratio | party, data = data_us_politicians)


## corrplots ----

# source: https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi

### US ----
model.matrix(~0+., data=data_us_politicians[,c('downplaying_ratio',
                                               'climate_ratio',
                                               'net_proclim_twt_rate_hemmi',
                                               'age_2021',
                                               'LCV_lifetime_score_decimal',
                                               'followers_interval',
                                               
                                               #'followers_millions',
                                               'party')]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, 
             type="lower", 
             lab=TRUE, 
             lab_size=4,
             colors = c("#E46726", "white", "#6D9EC1"),
             title = "Correlation Matrix: US")



### UK ----
model.matrix(~0+., data=data_uk_politicians[,c('downplaying_ratio',
                                               'climate_ratio',
                                               'net_proclim_twt_rate_hemmi',
                                               'followers_interval',
                                               #'age_2021',
                                               #'LCV_lifetime_score_decimal',
                                               'party')]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, 
             type="lower", 
             lab=TRUE, 
             lab_size=4,
             colors = c("#E46726", "white", "#6D9EC1"),
             title = "Correlation Matrix: UK")



### Australia ----
model.matrix(~0+., data=data_australia_politicians[,c('downplaying_ratio',
                                               'climate_ratio',
                                               'net_proclim_twt_rate_hemmi',
                                               'followers_interval',
                                               #'age_2021',
                                               #'LCV_lifetime_score_decimal',
                                               'party')]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, 
             type="lower", 
             lab=TRUE, 
             lab_size=4,
             colors = c("#E46726", "white", "#6D9EC1"),
             title = "Correlaton Matrix: Austrlia")




### selected US politicians ----
model.matrix(~0+., data=data_selected_politicians[,c('COP_before_indicator',
                                                     'COP_indicator',
                                                     'COP_after_indicator',
                                                     'climate_ratio',
                                                     'downplaying_ratio',
                                                     #'year',
                                                      
                                                      #'age_2021',
                                                      #'LCV_lifetime_score_decimal',
                                                      'party')]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, 
             type="lower", 
             lab=TRUE, 
             lab_size=4,
             colors = c("#E46726", "white", "#6D9EC1"))




# linear regressions ----


## United States ----
### climate ratio as independent variable ----

#### 1 ----
climate_lm_1_us = lm(climate_ratio~
                       party
                     
                     ,data = data_us_politicians)

summary(climate_lm_1_us)


#### 2 ----
climate_lm_2_us = lm(climate_ratio~
                       party+
                       age_2021
                     
                     ,data = data_us_politicians)

summary(climate_lm_2_us)

#### 3 ----
climate_lm_3_us = lm(climate_ratio~
                       party+
                       age_2021+
                       followers_interval
                    
                     ,data = data_us_politicians)

summary(climate_lm_3_us)


anova(climate_lm_1_us,climate_lm_2_us,climate_lm_3_us)

stargazer(climate_lm_1_us,climate_lm_2_us,climate_lm_3_us, font.size = "small")



### downplaying ratio as independent variable ----

#### 1 ----
downplaying_lm_1_us = lm(downplaying_ratio~
                       party
                     
                     ,data = data_us_politicians)

summary(downplaying_lm_1_us)


#### 2 ----
downplaying_lm_2_us = lm(downplaying_ratio~
                       party+
                       age_2021
                     
                     ,data = data_us_politicians)

summary(downplaying_lm_2_us)

#### 3 ----
downplaying_lm_3_us = lm(downplaying_ratio~
                       party+
                       age_2021+
                       followers_interval
                       
                     
                     ,data = data_us_politicians)

summary(downplaying_lm_3_us)


anova(downplaying_lm_1_us,downplaying_lm_2_us,downplaying_lm_3_us)

stargazer(downplaying_lm_1_us,downplaying_lm_2_us,downplaying_lm_3_us, font.size = "small")



### proclimate tweet rate as independent variable ----

#### 1 ----

proclim_lm_1_us = lm(net_proclim_twt_rate_hemmi~
                           party
                         
                         ,data = data_us_politicians)

summary(proclim_lm_1_us)


#### 2 ----
proclim_lm_2_us = lm(net_proclim_twt_rate_hemmi~
                           party+
                           age_2021
                         
                         ,data = data_us_politicians)

summary(proclim_lm_2_us)

#### 3 ----
proclim_lm_3_us = lm(net_proclim_twt_rate_hemmi~
                           party+
                           age_2021+
                           followers_interval
                         
                         ,data = data_us_politicians)

summary(proclim_lm_3_us)


anova(proclim_lm_1_us,proclim_lm_2_us,proclim_lm_3_us)


stargazer(proclim_lm_1_us,proclim_lm_2_us,proclim_lm_3_us, font.size = "small")


confint(climate_lm_1_us, level = 0.95)
confint(climate_lm_2_us, level = 0.95)
confint(climate_lm_3_us, level = 0.95)



confint(downplaying_lm_1_us, level = 0.95)
confint(downplaying_lm_2_us, level = 0.95)
confint(downplaying_lm_3_us, level = 0.95)



confint(proclim_lm_1_us, level = 0.95)
confint(proclim_lm_2_us, level = 0.95)
confint(proclim_lm_3_us, level = 0.95)




## UK ----
### climate ratio as independent variable ----

#### 1 ----
climate_lm_1_uk = lm(climate_ratio~
                       party
                     
                     ,data = data_uk_politicians)

summary(climate_lm_1_uk)

#### 2 ----
climate_lm_2_uk = lm(climate_ratio~
                       party+
                       followers_interval
                     
                     ,data = data_uk_politicians)

summary(climate_lm_2_uk)


confint(climate_lm_2_uk)



### downplaying_ratio as independent variable ----
#### 1 ----
downplaying_lm_1_uk = lm(downplaying_ratio~
                       party
                     
                     ,data = data_uk_politicians)

summary(downplaying_lm_1_uk)


#### 2 ----
downplaying_lm_2_uk = lm(downplaying_ratio~
                       party+
                       followers_interval
                     
                     ,data = data_uk_politicians)

summary(downplaying_lm_2_uk)


confint(downplaying_lm_2_uk)

### proclimate tweet rate ----
#### 1 ----

proclim_lm_1_uk = lm(net_proclim_twt_rate_hemmi~
                       party
                     
                     ,data = data_uk_politicians)

summary(proclim_lm_1_uk)


#### 2 ----
proclim_lm_2_uk = lm(net_proclim_twt_rate_hemmi~
                       party+
                       followers_interval
                     
                     ,data = data_uk_politicians)

summary(proclim_lm_2_uk)





stargazer(climate_lm_1_uk,climate_lm_2_uk, downplaying_lm_1_uk, downplaying_lm_2_uk, font.size = "small")

stargazer(proclim_lm_1_uk, proclim_lm_2_uk, font.size = "small")




## Australia ----
### climate ratio as independent variable ----
#### 1 ----

climate_lm_1_australia = lm(climate_ratio~
                       party
                     
                     ,data = data_australia_politicians)

summary(climate_lm_1_australia)
#### 2 ----

climate_lm_2_australia = lm(climate_ratio~
                              party+
                              followers_interval
                            
                            ,data = data_australia_politicians)

summary(climate_lm_2_australia)



### downplaying_ratio as independent variable ----
#### 1 ----
downplaying_lm_1_australia = lm(downplaying_ratio~
                           party
                         
                         ,data = data_australia_politicians)

summary(downplaying_lm_1_australia)

#### 2 ----
downplaying_lm_2_australia = lm(downplaying_ratio~
                                  party+
                                  followers_interval
                                
                                ,data = data_australia_politicians)

summary(downplaying_lm_2_australia)


### proclimate tweet rate ----
#### 1 ----

proclim_lm_1_australia = lm(net_proclim_twt_rate_hemmi~
                       party
                     
                     ,data = data_australia_politicians)

summary(proclim_lm_1_australia)


#### 2 ----
proclim_lm_2_australia = lm(net_proclim_twt_rate_hemmi~
                       party+
                       followers_interval
                     
                     ,data = data_australia_politicians)

summary(proclim_lm_2_australia)




stargazer(climate_lm_1_australia, climate_lm_2_australia, font.size = "small")

stargazer(downplaying_lm_1_australia, downplaying_lm_2_australia, font.size = "small")

stargazer(proclim_lm_1_australia, proclim_lm_2_australia, font.size = "small")






# panel regression ----
# relevant sources: 
# https://www3.nd.edu/~rwilliam/stats3/Panel04-FixedVsRandom.pdf
# https://bookdown.org/ccolonescu/RPoE4/panel-data-models.html#mjx-eqn-eqpanelgeneq15
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4000708

## climate ratio ----
### pooled model ----
pooled_climate_1 = plm(climate_ratio~
                        COP_before_indicator+
                        COP_indicator+
                        COP_after_indicator+
                        paris_indicator+
                        party +
                        birth_year +
                        followers_millions +
                        year
                        , 
                      data = data_selected_politicians, 
                      index = c("name","date_full"), 
                      model = "pooling"
                      #effect="time"
)

summary(pooled_climate_1)


cluster_robust_pooled_climate_1 = coeftest(pooled_climate_1, vcov=vcovHC(pooled_climate_1,
                                           type="HC0",cluster="group"))



confint(cluster_robust_pooled_climate_1, level = 0.95)


stargazer(cluster_robust_pooled_climate_1)



### Fixed Effects Model ----

#### 1 ----
within_climate_1 = plm(climate_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator
                         #year#+
                         #paris_indicator+
                         #party +
                         #birth_year +
                         #followers_millions
                       , 
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "within"
                       #effect="time"
)

summary(within_climate_1)

pFtest(within_climate_1, pooled_climate_1)

#### 2 ----
within_climate_2 = plm(climate_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator#+
                         #party +
                         #birth_year +
                         #followers_millions
                       , 
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "within"
                       #effect="time"
)

summary(within_climate_2)


#### 3 ----
within_climate_3 = plm(climate_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator+
                         party +
                         birth_year +
                         followers_millions+
                         year
                       , 
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "within"
                       #effect="time"
)

summary(within_climate_3)








stargazer(within_climate_1,within_climate_2,within_climate_3, font.size = "small", title = "regression results entity (and time) fixed effects model")





### Random Effects Model ----

climateReTest <- plmtest(pooled_climate_1, effect="individual")
climateReTest

#### 1 ----
random_climate_1 = plm(climate_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator
                         #year#+
                         #paris_indicator+
                         #party +
                         #age +
                         #followers_millions
                       ,
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "random"
                       #effect="time"
)

summary(random_climate_1)


#### 2 ----
random_climate_2 = plm(climate_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator
                       #party +
                       #age +
                       #followers_millions
                       ,
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "random"
                       #effect="time"
)

summary(random_climate_2)

#### 3 ----
random_climate_3 = plm(climate_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator+
                         party +
                         birth_year+
                         followers_millions+
                         year
                       ,
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "random"
                       #effect="time"
)

summary(random_climate_3)


confint(random_climate_3)


stargazer(random_climate_1,random_climate_2,random_climate_3, font.size = "small", title = "regression results random effects model")


## downplaying ratio ----
### pooled model ----
pooled_downplaying_1 = plm(downplaying_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator+
                         party +
                         birth_year +
                         followers_millions+
                         year
                       , 
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "pooling"
                       #effect="time"
)

summary(pooled_downplaying_1)


cluster_downplaying_pooled = coeftest(pooled_downplaying_1, vcov=vcovHC(pooled_downplaying_1,
                                  type="HC0",cluster="group"))


confint(cluster_downplaying_pooled, level = 0.95)

stargazer(cluster_downplaying_pooled)




### Fixed Effects Model ----

#### 1 ----
within_downplaying_1 = plm(downplaying_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator#+
                       #paris_indicator+
                       #party +
                       #age +
                       #followers_millions
                       , 
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "within"
                       #effect="time"
)

summary(within_downplaying_1)



#### 2 ----
within_downplaying_2 = plm(downplaying_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator#+
                       #party +
                       #age +
                       #followers_millions
                       , 
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "within"
                       #effect="time"
)

summary(within_downplaying_2)


#### 3 ----
within_downplaying_3 = plm(downplaying_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator+
                         party +
                         birth_year +
                         followers_millions+
                         year
                       , 
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "within"
                       #effect="time"
)

summary(within_downplaying_3)



stargazer(within_downplaying_1,within_downplaying_2,within_downplaying_3, font.size = "small", title = "regression results entity (and time) fixed effects model")





### Random Effects Model ----

downplayingReTest <- plmtest(pooled_downplaying_1, effect="individual")
downplaying

#### 1 ----
random_downplaying_1 = plm(downplaying_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator#+
                       #paris_indicator+
                       #party +
                       #age +
                       #followers_millions
                       ,
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "random"
                       #effect="time"
)

summary(random_downplaying_1)


#### 2 ----
random_downplaying_2 = plm(downplaying_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator
                       #party +
                       #age +
                       #followers_millions
                       ,
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "random"
                       #effect="time"
)

summary(random_downplaying_2)

#### 3 ----
random_downplaying_3 = plm(downplaying_ratio~
                         COP_before_indicator+
                         COP_indicator+
                         COP_after_indicator+
                         paris_indicator+
                         party +
                         birth_year +
                         followers_millions+
                         year
                       ,
                       data = data_selected_politicians, 
                       index = c("name","date_full"), 
                       model = "random"
                       #effect="time"
)

summary(random_downplaying_3)

confint(random_downplaying_3)


stargazer(random_downplaying_1,random_downplaying_2,random_downplaying_3, font.size = "small", title = "regression results random effects model")




# ## fixed effects ----
# 
# fixed_climate_1 = plm(climate_ratio~
#                       party +
#                       age +
#                       followers_millions+
#                       COP_indicator+
#                       paris_indicator, 
#                   data = data_selected_politicians, 
#                   index = c("name","date_full"), 
#                   model = "pooling",
#                   #effect="pooling"
#                   )
# 
# 
# summary(fixed_climate_1)
# 
# fixef(fixed_climate_1)
# 
# pooltest(climate_ratio ~ party + age + COP_indicator, data = data_selected_politicians, index = c("name","date_full"), model = "within")
# pooltest(downplaying_ratio ~ party + age + COP_indicator, data = data_selected_politicians, index = c("name","date_full"), model = "within")
# 
# 
# 
# 
# fixed_downplaying_1 = plm(downplaying_ratio~
#                         party +
#                         #year +
#                         age +
#                         followers_millions+
#                         COP_indicator+
#                         paris_indicator,
#                         #climate_ratio, # include?
#                       data = data_selected_politicians, 
#                       index = c("name","date_full"), 
#                       model = "within"
#                       #effect="time"
# )
# 
# 
# summary(fixed_downplaying_1)
# 
# 