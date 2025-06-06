#### Load packages

library(psych)
library(ggplot2)
library(corrplot)
library(sjPlot)
library(lavaan)
library(semTools)
library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(apaTables)
library(car)
library(picante)

#### Import data

load("CFA_data.Rda")


# DATA PREPARATION

## rename WSDS to LISD

CFA_data <- rename(CFA_data, LISD_State_02=WSDS_State_02)
CFA_data <- rename(CFA_data, LISD_State_03=WSDS_State_03)
CFA_data <- rename(CFA_data, LISD_State_04=WSDS_State_04)
CFA_data <- rename(CFA_data, LISD_State_05=WSDS_State_05)
CFA_data <- rename(CFA_data, LISD_State_06=WSDS_State_06)
CFA_data <- rename(CFA_data, LISD_State_07=WSDS_State_07)
CFA_data <- rename(CFA_data, LISD_State_09=WSDS_State_09)
CFA_data <- rename(CFA_data, LISD_State_10=WSDS_State_10)
CFA_data <- rename(CFA_data, LISD_State_12=WSDS_State_12)
CFA_data <- rename(CFA_data, LISD_State_13=WSDS_State_13)
CFA_data <- rename(CFA_data, LISD_State_15=WSDS_State_15)
CFA_data <- rename(CFA_data, LISD_State_17=WSDS_State_17)

CFA_data <- rename(CFA_data, LISD_Trait_01=WSDS_Trait_01)
CFA_data <- rename(CFA_data, LISD_Trait_02=WSDS_Trait_02)
CFA_data <- rename(CFA_data, LISD_Trait_04=WSDS_Trait_04)
CFA_data <- rename(CFA_data, LISD_Trait_05=WSDS_Trait_05)
CFA_data <- rename(CFA_data, LISD_Trait_09=WSDS_Trait_09)
CFA_data <- rename(CFA_data, LISD_Trait_10=WSDS_Trait_10)
CFA_data <- rename(CFA_data, LISD_Trait_11=WSDS_Trait_11)
CFA_data <- rename(CFA_data, LISD_Trait_12=WSDS_Trait_12)
CFA_data <- rename(CFA_data, LISD_Trait_13=WSDS_Trait_13)
CFA_data <- rename(CFA_data, LISD_Trait_14=WSDS_Trait_14)
CFA_data <- rename(CFA_data, LISD_Trait_15=WSDS_Trait_15)
CFA_data <- rename(CFA_data, LISD_Trait_16=WSDS_Trait_16)
CFA_data <- rename(CFA_data, LISD_Trait_17=WSDS_Trait_17)
CFA_data <- rename(CFA_data, LISD_Trait_18=WSDS_Trait_18)
CFA_data <- rename(CFA_data, LISD_Trait_20=WSDS_Trait_20)
CFA_data <- rename(CFA_data, LISD_Trait_21=WSDS_Trait_21)
CFA_data <- rename(CFA_data, LISD_Trait_22=WSDS_Trait_22)
CFA_data <- rename(CFA_data, LISD_Trait_23=WSDS_Trait_23)


## select LISD items only

LISD_cfa <- CFA_data[,c(88:94,96:98,95,100,101:118)]
LISD_cfa_State <- CFA_data[,c(88:94,96:98,95,100)]
LISD_cfa_Trait <- CFA_data[,c(101:118)]


## Multivariate outliers

mahal = mahalanobis(LISD_cfa, colMeans(LISD_cfa, na.rm = TRUE), cov(LISD_cfa, use = "pairwise.complete"))
summary(mahal)
cutoff = qchisq(1-.001, ncol(LISD_cfa)) 
ncol(LISD_cfa) 
cutoff
summary (mahal < cutoff) 
noout_cfa = subset (LISD_cfa, mahal < cutoff)
noout_cfa_State <- noout_cfa[,c(1:12)]
noout_cfa_Trait <- noout_cfa[,13:30]
save(noout_cfa, file = "noout_cfa.RDa")


list(LISD_cfa, mahal > cutoff)


## Additivity assumption

correl = cor(noout_cfa, use = "pairwise.complete.obs") 
symnum(correl)


## Linearity assumption

random = rchisq(nrow(noout_cfa), 7)
fake = lm(random~., data = noout_cfa) 
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1) 


## Multivariate normality 

hist(standardized, col="darkgreen") 
fitted = scale(fake$fitted.values)
plot(fitted, standardized)
abline(0,0)
abline(v = 0)



## Descriptives & Skewness STATE

kable(describe(noout_cfa_State), table.attr = "style='width:30%;'") %>% 
  kableExtra::kable_styling()

multi.hist(LISD_cfa_State, dcol="red")


## Descriptives & Skewness TRAIT

kable(describe(noout_cfa_Trait), table.attr = "style='width:30%;'") %>% 
  kableExtra::kable_styling()

multi.hist(LISD_cfa_Trait, dcol="red")

## Descriptives ALL ITEMS
descriptives_noout_cfa <- as.data.frame(describe(noout_cfa))
kable(describe(descriptives_noout_cfa), table.attr = "style='width:30%;'") %>% 
  kableExtra::kable_styling()


## Correlations

correl <- cor(noout_cfa)
correlRound <- round(correl, digits = 3) 
correlRound

symnum(correl)

corrplot(correl, type="upper")

#### portion of correlations <.2:

correlUpper <- as.matrix(correl[upper.tri(correl)]) 
correlSmall <- abs(correlUpper) < 0.2 
sum(correlSmall) 
sum(correlSmall)/nrow(correlUpper) 


##### Delete unnecessary objects

remove(random,fake,standardized,fitted,mahal,cutoff,correl,correlRound,correlSmall,correlUpper,descriptives_noout_cfa)


#### separate trait and state

noout_cfa_State <- noout_cfa[,1:12]
noout_cfa_Trait <- noout_cfa[,13:30]




# CONFIRMATORY FACTOR ANALYSIS

## STATE: 2-factor-model

### Create models

model_2F_state<-'
lonely=~LISD_State_02+LISD_State_04+LISD_State_05+LISD_State_06+LISD_State_07+LISD_State_09+LISD_State_13+LISD_State_15+LISD_State_17
connected=~LISD_State_03+LISD_State_10+LISD_State_12
'

### FIT

fit_model_2F_state <- cfa(model_2F_state, data=noout_cfa_State, std.lv=TRUE, missing="fiml")


### OUTPUT

options(knitr.kable.NA = '')

summary(fit_model_2F_state, fit.measures = TRUE, standardized=TRUE)

### Power Analysis
semTools::findRMSEApower(.05, .08, 53, 304, .05, 1) #(rmsea0, rmseaA, df, n, alpha = 0.05, group = 1)



## Reliability

factor_S_lonely <- noout_cfa_State[,c("LISD_State_02","LISD_State_04","LISD_State_05","LISD_State_06","LISD_State_07","LISD_State_09","LISD_State_13","LISD_State_15","LISD_State_17")]
factor_S_connected <- noout_cfa_State[,c("LISD_State_03","LISD_State_10","LISD_State_12")]
psych::alpha(factor_S_lonely, check.keys = TRUE)
psych::alpha(factor_S_connected, check.keys = TRUE)


#McDonald's omega (McDonald, 1999)
psych::omega(factor_S_lonely, check.keys = TRUE)
psych::omega(factor_S_connected, check.keys = TRUE)


## Factor Loadings & Modification Indices

#Factor Loadings

factorloadings_model_2F_state <- parameterEstimates(fit_model_2F_state, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
factorloadings_model_2F_state

#Residuals and Moficiation Indices

residuals(fit_model_2F_state, type = "cor")
cov_table <- residuals(fit_model_2F_state, type = "cor")$cov
cov_table 
modificationIndices(fit_model_2F_state, sort.=TRUE, minimum.value=3)


## 1-FACTOR-SOLUTION

model_1F_state<-'
state=~LISD_State_02+LISD_State_04+LISD_State_05+LISD_State_06+LISD_State_07+LISD_State_09+LISD_State_13+LISD_State_15+LISD_State_17
+LISD_State_03+LISD_State_10+LISD_State_12
'
fit_model_1F_state <- cfa(model_1F_state, data=noout_cfa_State, std.lv=TRUE, missing="fiml")
summary(fit_model_1F_state, fit.measures = TRUE, standardized=TRUE)


## STATE MODEL COMPARISON

anova(fit_model_2F_state, fit_model_1F_state)



# TRAIT

## 3-Factor Solution


### Create Models

#### strict

model_3F_trait_strict<-'
loneliness=~LISD_Trait_02+LISD_Trait_16+LISD_Trait_21+LISD_Trait_22+LISD_Trait_23
sociability=~LISD_Trait_01+LISD_Trait_09+LISD_Trait_10+LISD_Trait_18+LISD_Trait_20
support_closeness=~LISD_Trait_04+LISD_Trait_12+LISD_Trait_13+LISD_Trait_14
'

### FIT 

fit_model_3F_trait_strict <- cfa(model_3F_trait_strict, data=noout_cfa_Trait, std.lv=TRUE, missing="fiml")

### OUTPUT

options(knitr.kable.NA = '')
summary(fit_model_3F_trait_strict, fit.measures = TRUE, standardized=TRUE)


###### Residuals and Moficiation Indices

residuals(fit_model_3F_trait_strict, type = "cor")
cov_table <- residuals(fit_model_3F_trait_strict, type = "cor")$cov
cov_table
modificationIndices(fit_model_3F_trait_strict, sort.=TRUE, minimum.value=3)


## EXPLORATORY restriction (modification indices, reliability): remove Item21

## strict w/o Trait 21

model_3F_trait_wo21_strict<-'
loneliness=~LISD_Trait_02+LISD_Trait_16+LISD_Trait_22+LISD_Trait_23
sociability=~LISD_Trait_01+LISD_Trait_09+LISD_Trait_10+LISD_Trait_18+LISD_Trait_20
support_closeness=~LISD_Trait_04+LISD_Trait_12+LISD_Trait_13+LISD_Trait_14
'


## 1-FACTOR-SOLUTION

model_1F_trait<-'
trait=~LISD_Trait_02+LISD_Trait_04+LISD_Trait_13+LISD_Trait_16+LISD_Trait_21+LISD_Trait_22+LISD_Trait_23+LISD_Trait_01+LISD_Trait_09+LISD_Trait_10+LISD_Trait_18+LISD_Trait_20+LISD_Trait_12
'
fit_model_1F_trait <- cfa(model_1F_trait, data=noout_cfa_Trait, std.lv=TRUE, missing="fiml")
summary(fit_model_1F_trait, fit.measures = TRUE, standardized=TRUE)


### FIT 

fit_model_3F_trait_wo21_strict <- cfa(model_3F_trait_wo21_strict, data=noout_cfa_Trait, std.lv=TRUE, missing="fiml")


### OUTPUT

options(knitr.kable.NA = '')
summary(fit_model_3F_trait_wo21_strict, fit.measures = TRUE, standardized=TRUE)


### Power Analysis

semTools::findRMSEApower(.05, .08, 62, 304, .05, 1) #(rmsea0, rmseaA, df, n, alpha = 0.05, group = 1)

## TRAIT MODEL COMPARISON

# 1-Factor Solution
anova(fit_model_3F_trait_strict, fit_model_1F_trait)
# 3-Factors without Trait21
anova(fit_model_3F_trait_strict, fit_model_3F_trait_wo21_strict)


# continue with Model without 21


## Reliability

factor_T_loneliness <- noout_cfa_Trait[,c("LISD_Trait_02","LISD_Trait_16","LISD_Trait_22","LISD_Trait_23")]
factor_T_sociability <- noout_cfa_Trait[,c("LISD_Trait_01","LISD_Trait_09","LISD_Trait_10","LISD_Trait_18","LISD_Trait_20")]
factor_T_support <- noout_cfa_Trait[,c("LISD_Trait_04","LISD_Trait_12","LISD_Trait_13","LISD_Trait_14")]
psych::alpha(factor_T_loneliness, check.keys = TRUE)
psych::alpha(factor_T_sociability, check.keys = TRUE)
psych::alpha(factor_T_support, check.keys = TRUE)


#McDonald's omega (McDonald, 1999)
psych::omega(factor_T_loneliness, check.keys = TRUE)
psych::omega(factor_T_sociability, check.keys = TRUE)
psych::omega(factor_T_support, check.keys = TRUE)


## Factor Loadings & Modification Indices

###### Factor Loadings

factorloadings_model_3F_trait_wo21_strict <- parameterEstimates(fit_model_3F_trait_wo21_strict, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
factorloadings_model_3F_trait_wo21_strict

###### Residuals and Moficiation Indices

residuals(fit_model_3F_trait_wo21_strict, type = "cor")
cov_table <- residuals(fit_model_3F_trait_wo21_strict, type = "cor")$cov
cov_table
modificationIndices(fit_model_3F_trait_wo21_strict, sort.=TRUE, minimum.value=3)



## clean workspace

remove(efa_State_2F,efa_State_2F_strict,efa_State_2F_wo11,efa_Trait_3F,efa_Trait_3F_strict,fit_model_1F_state,fit_model_1F_trait,fit_model_2F_state,fit_model_3F_trait_strict,fit_model_3F_trait_wo05,fit_model_3F_trait_strict,fit_model_3F_trait_wo21_strict)
remove(cov_table,factor_S_connected,factor_S_lonely,factor_T_loneliness,factor_T_sociability,factor_T_support,factorloadings_model_2F_state,factorloadings_model_3F_trait_wo21_strict,model_1F_state,model_1F_trait,model_2F_state,model_3F_trait_strict,model_3F_trait_wo21_strict)
remove(LISD_cfa,LISD_cfa_State,LISD_cfa_Trait)



# SCALE VALIDATION

## Import EFA data

load("EFA_data_final.Rda")
#load("noSkew_efa.Rda")
#load("noout_cfa.Rda")


## Preparation

### separate trait and state

noSkew_efa_State <- noSkew_efa[,1:15]
noSkew_efa_Trait <- noSkew_efa[,16:38]
#noout_cfa_State <- noout_cfa[,1:12]
#noout_cfa_Trait <- noout_cfa[,12:30]


### LISD: invert items, calculate subscale scores


# State Factor2: invert State12 
EFA_data$LISD_State_12_inv <- 6 - EFA_data$LISD_State_12
CFA_data$LISD_State_12_inv <- 6 - CFA_data$LISD_State_12
#Trait Factor2: invert Trait18 
EFA_data$LISD_Trait_18_inv <- 6 - EFA_data$LISD_Trait_18
CFA_data$LISD_Trait_18_inv <- 6 - CFA_data$LISD_Trait_18
# Trait Factor3: invert Trait04, 13, 14 (otherwise "lack of closeness/support") 
EFA_data$LISD_Trait_04_inv <- 6 - EFA_data$LISD_Trait_04
CFA_data$LISD_Trait_04_inv <- 6 - CFA_data$LISD_Trait_04
EFA_data$LISD_Trait_13_inv <- 6 - EFA_data$LISD_Trait_13
CFA_data$LISD_Trait_13_inv <- 6 - CFA_data$LISD_Trait_13
EFA_data$LISD_Trait_14_inv <- 6 - EFA_data$LISD_Trait_14
CFA_data$LISD_Trait_14_inv <- 6 - CFA_data$LISD_Trait_14


#Subscale Scores State
EFA_data$LISD_State_F1 <- with(EFA_data, (LISD_State_02+LISD_State_04+LISD_State_05+LISD_State_06+
                                            LISD_State_07+LISD_State_09+LISD_State_13+LISD_State_15+LISD_State_17)/9)
EFA_data$LISD_State_F2 <- with(EFA_data, (LISD_State_03+LISD_State_10+LISD_State_12_inv)/3)
CFA_data$LISD_State_F1 <- with(CFA_data, (LISD_State_02+LISD_State_04+LISD_State_05+LISD_State_06+
                                            LISD_State_07+LISD_State_09+LISD_State_13+LISD_State_15+LISD_State_17)/9)
CFA_data$LISD_State_F2 <- with(CFA_data, (LISD_State_03+LISD_State_10+LISD_State_12_inv)/3)


#Subscale Scores Trait
EFA_data$LISD_Trait_F1 <- with(EFA_data, (LISD_Trait_02+LISD_Trait_16+LISD_Trait_22+LISD_Trait_23)/4)
EFA_data$LISD_Trait_F2 <- with(EFA_data, (LISD_Trait_01+LISD_Trait_09+LISD_Trait_10+
                                            LISD_Trait_18_inv+LISD_Trait_20)/5)
EFA_data$LISD_Trait_F3 <- with(EFA_data, (LISD_Trait_04_inv+LISD_Trait_12+LISD_Trait_13_inv+LISD_Trait_14_inv)/4)
CFA_data$LISD_Trait_F1 <- with(CFA_data, (LISD_Trait_02+LISD_Trait_16+LISD_Trait_22+LISD_Trait_23)/4)
CFA_data$LISD_Trait_F2 <- with(CFA_data, (LISD_Trait_01+LISD_Trait_09+LISD_Trait_10+
                                            +LISD_Trait_18_inv+LISD_Trait_20)/5)
CFA_data$LISD_Trait_F3 <- with(CFA_data, (LISD_Trait_04_inv+LISD_Trait_12+LISD_Trait_13_inv+LISD_Trait_14_inv)/4)


## VALIDITY

# select variables

#### Group Questionnaire Scores
questionnaires_EFA <- EFA_data[,c("STAIState_sum","STAITrait_sum","MSPSS_sum","MSPSS_wItem2","SIAS_sum","PHQ2_sum","BDIV_sum")]
#CFA: also Big Five, SGSE
questionnaires_CFA <- CFA_data[,c("STAIState_sum","STAITrait_sum","MSPSS_sum","MSPSS_wItem2","SIAS_sum","PHQ2_sum","BDIV_sum")]
BigFive_SGSE_CFA <- CFA_data[,c("NEO_N","NEO_E","NEO_O","NEO_A","NEO_C","SGSE_shy","SGSE_sociable")]


#### Sociodemographics
sociodemographics_EFA <- EFA_data[,c("age","numHousehold","numContacts","compliance_01","compliance_02","compliance_03","compliance_05")]
sociodemographics_CFA <- CFA_data[,c("age","numHousehold","numContacts","compliance_01","compliance_02","compliance_03","compliance_05")]

#### occupation

occupation_EFA <- EFA_data[,c("occupationStudent", "occupationJob")]
occupation_CFA <- CFA_data[,c("occupationStudent", "occupationJob")]


#### remove multivariate outliers LISD

#EFA: 16 participants
#"311", "435", "488", "515", "520", "552", "582", "629", "637", "670", "748", "802", "812","822", "888", "1038"
questionnaires_EFA <- 
  questionnaires_EFA[!(rownames(questionnaires_EFA) %in% 
                         c("311", "435", "488", "515", "520", "552", "582", "629", "637", "670", "748", "802", "812","822", "888", "1038")),]
sociodemographics_EFA <- 
  sociodemographics_EFA[!(rownames(sociodemographics_EFA) %in% 
                            c("311", "435", "488", "515", "520", "552", "582", "629", "637", "670", "748", "802", "812","822", "888", "1038")),]
occupation_EFA <- 
  occupation_EFA[!(rownames(occupation_EFA) %in% 
                            c("311", "435", "488", "515", "520", "552", "582", "629", "637", "670", "748", "802", "812","822", "888", "1038")),]
EFA_survey <- EFA_data
EFA_data <- 
  EFA_data[!(rownames(EFA_data) %in% 
               c("311", "435", "488", "515", "520", "552", "582", "629", "637", "670", "748", "802", "812","822", "888", "1038")),]

#CFA: 14 participants
#7, 12, 16, 41, 99, 121, 136, 192, 201, 275, 295, 299, 325, 360
#"94","99","103","128","187","209", "225","281","290","366","386","391","420","455"
questionnaires_CFA <- 
  questionnaires_CFA[!(rownames(questionnaires_CFA) %in% 
                         c("94","99","103","128","187","209", "225","281","290","366","386","391","420","455")),]
BigFive_SGSE_CFA <- 
  BigFive_SGSE_CFA[!(rownames(BigFive_SGSE_CFA) %in% 
                       c("94","99","103","128","187","209", "225","281","290","366","386","391","420","455")),]
sociodemographics_CFA <- 
  sociodemographics_CFA[!(rownames(sociodemographics_CFA) %in% 
                            c("94","99","103","128","187","209", "225","281","290","366","386","391","420","455")),]
occupation_CFA <- 
  occupation_CFA[!(rownames(occupation_CFA) %in% 
                            c("94","99","103","128","187","209", "225","281","290","366","386","391","420","455")),]
CWsurvey <- CFA_data
CFA_data <- 
  CFA_data[!(rownames(CFA_data) %in% 
               c("94","99","103","128","187","209", "225","281","290","366","386","391","420","455")),]


#### create dataframes

LISD_subscales_EFA <- EFA_data[,c("LISD_State_F1","LISD_State_F2","LISD_Trait_F1","LISD_Trait_F2","LISD_Trait_F3")]
LISD_subscales_CFA <- CFA_data[,c("LISD_State_F1","LISD_State_F2","LISD_Trait_F1","LISD_Trait_F2","LISD_Trait_F3")]
LISD_subscales_EFA <- round(LISD_subscales_EFA, digits = 2)
LISD_subscales_CFA <- round(LISD_subscales_CFA, digits = 2)

all_questionnaires_EFA <- cbind(LISD_subscales_EFA, questionnaires_EFA)
all_questionnaires_CFA <- cbind(LISD_subscales_CFA, questionnaires_CFA)

# merge questionnaires
all_questionnaires_EFA$Sample = "Study 1"
all_questionnaires_CFA$Sample = "Study 2"
#add ID
EFA_CFA_quest = rbind(all_questionnaires_EFA, all_questionnaires_CFA)
EFA_CFA_quest$ID = rownames(EFA_CFA_quest)
EFA_CFA_quest <- EFA_CFA_quest %>%  select(ID, everything())
EFA_CFA_quest$ID = as.factor(EFA_CFA_quest$ID)
rownames(EFA_CFA_quest) <- NULL  ## reset rownames
# correct
EFA_CFA_quest$LISD_State_F1<- as.numeric(EFA_CFA_quest$LISD_State_F1)
EFA_CFA_quest$LISD_State_F2<- as.numeric(EFA_CFA_quest$LISD_State_F2)
EFA_CFA_quest$LISD_State_F1<- as.numeric(EFA_CFA_quest$LISD_State_F1)
EFA_CFA_quest$LISD_State_F2<- as.numeric(EFA_CFA_quest$LISD_State_F2)
EFA_CFA_quest$LISD_Trait_F1<- as.numeric(EFA_CFA_quest$LISD_Trait_F1)
EFA_CFA_quest$LISD_Trait_F2<- as.numeric(EFA_CFA_quest$LISD_Trait_F2)
EFA_CFA_quest$LISD_Trait_F3<- as.numeric(EFA_CFA_quest$LISD_Trait_F3)
EFA_CFA_quest$LISD_Trait_F1<- as.numeric(EFA_CFA_quest$LISD_Trait_F1)
EFA_CFA_quest$LISD_Trait_F2<- as.numeric(EFA_CFA_quest$LISD_Trait_F2)
EFA_CFA_quest$LISD_Trait_F3<- as.numeric(EFA_CFA_quest$LISD_Trait_F3)

# merge sociodemographics
sociodemographics_EFA$Sample = "Study 1"
sociodemographics_CFA$Sample = "Study 2"
#add ID
EFA_CFA_sociodemographics = rbind(sociodemographics_EFA, sociodemographics_CFA)
EFA_CFA_sociodemographics$ID = rownames(EFA_CFA_sociodemographics)
EFA_CFA_sociodemographics <- EFA_CFA_sociodemographics %>%  select(ID, everything())
EFA_CFA_sociodemographics$ID = as.factor(EFA_CFA_sociodemographics$ID)
rownames(EFA_CFA_sociodemographics) <- NULL  ## reset rownames

EFA_CFA_sociodemographics$age <- as.numeric(EFA_CFA_sociodemographics$age)
EFA_CFA_sociodemographics$numHousehold <- as.numeric(EFA_CFA_sociodemographics$numHousehold)
EFA_CFA_sociodemographics$numContacts <- as.numeric(EFA_CFA_sociodemographics$numContacts)


# merge occupation
occupation_EFA$Sample = "Study 1"
occupation_CFA$Sample = "Study 2"
#add ID
EFA_CFA_occupation = rbind(occupation_EFA, occupation_CFA)
EFA_CFA_occupation$ID = rownames(EFA_CFA_occupation)
EFA_CFA_occupation <- EFA_CFA_occupation %>%  select(ID, everything())
EFA_CFA_occupation$ID = as.factor(EFA_CFA_occupation$ID)
rownames(EFA_CFA_occupation) <- NULL  ## reset rownames


## Descriptive Analysis

### Differences in Sociodemographics between Samples

describeBy(EFA_CFA_sociodemographics, EFA_CFA_sociodemographics$Sample)

##### Levene Tests

leveneTest(EFA_CFA_sociodemographics$age, EFA_CFA_sociodemographics$Sample)#sign
leveneTest(EFA_CFA_sociodemographics$numHousehold, EFA_CFA_sociodemographics$Sample) #sign
leveneTest(EFA_CFA_sociodemographics$numContacts, EFA_CFA_sociodemographics$Sample) #sign
leveneTest(EFA_CFA_sociodemographics$compliance_01, EFA_CFA_sociodemographics$Sample) #sign
leveneTest(EFA_CFA_sociodemographics$compliance_02, EFA_CFA_sociodemographics$Sample)#sign
leveneTest(EFA_CFA_sociodemographics$compliance_03, EFA_CFA_sociodemographics$Sample)
leveneTest(EFA_CFA_sociodemographics$compliance_05, EFA_CFA_sociodemographics$Sample)

##### t-test

t.test(EFA_CFA_sociodemographics$age~EFA_CFA_sociodemographics$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_sociodemographics$numHousehold~EFA_CFA_sociodemographics$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_sociodemographics$numContacts~EFA_CFA_sociodemographics$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_sociodemographics$compliance_01~EFA_CFA_sociodemographics$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_sociodemographics$compliance_02~EFA_CFA_sociodemographics$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_sociodemographics$compliance_03~EFA_CFA_sociodemographics$Sample, var.equal = TRUE)
t.test(EFA_CFA_sociodemographics$compliance_05~EFA_CFA_sociodemographics$Sample, var.equal = TRUE)


# Occupation

## Differences in Occupation between Samples

attach(EFA_CFA_occupation)
occupationStudent[is.na(occupationStudent)] <- -1
occupationJob[is.na(occupationJob)] <- -1

TAB_occupationStudent = table(occupationStudent, Sample)
TAB_occupationStudent
chisq.test(TAB_occupationStudent, correct=T)

TAB_occupationJob = table(occupationJob, Sample)
TAB_occupationJob
chisq.test(TAB_occupationJob, correct=T)

detach(EFA_CFA_occupation)


# Convergent & Discriminant Validity

## Correlations Factors - Questionnaires


names(questionnaires_EFA)[c(1:7)] <- c("STAIstate","STAItrait","MSPSS","MSPSS_noItem2","SIAS","PHQ2","BDI-V")
names(questionnaires_CFA)[c(1:7)] <- c("STAIstate","STAItrait","MSPSS","MSPSS_noItem2","SIAS","PHQ2","BDI-V")
names(LISD_subscales_EFA) [c(1:5)] <- c("State_F1","State_F2","Trait_F1","Trait_F2","Trait_F3")
names(LISD_subscales_CFA) [c(1:5)] <- c("State_F1","State_F2","Trait_F1","Trait_F2","Trait_F3")


Factors_Quest_EFA <- cbind(LISD_subscales_EFA, questionnaires_EFA)
Factors_Quest_CFA <- cbind(LISD_subscales_CFA, questionnaires_CFA)
apa.cor.table(Factors_Quest_EFA, filename = "Corr_Factors_Quest_EFA.doc", show.conf.interval = FALSE)
apa.cor.table(Factors_Quest_CFA, filename = "Corr_Factors_Quest_CFA.doc", show.conf.interval = FALSE)


#r and p-values
cor.table(Factors_Quest_EFA)
cor.table(Factors_Quest_CFA)

#### Plots EFA vs. CFA

quest_EFA_Correl <- cor(LISD_subscales_EFA, questionnaires_EFA)
quest_EFA_Correl <- round(quest_EFA_Correl, digits = 3)
quest_EFA_Correl

quest_CFA_Correl <- cor(LISD_subscales_CFA, questionnaires_CFA)
quest_CFA_Correl <- round(quest_CFA_Correl, digits = 3)
quest_CFA_Correl

corrplot(quest_EFA_Correl)
corrplot(quest_CFA_Correl)


#### Overview Correlations LISD only

#EFA
Factors_Correl_EFA <- cor(LISD_subscales_EFA, LISD_subscales_EFA)
corrplot(Factors_Correl_EFA, type="upper")
#CFA
Factors_Correl_CFA <- cor(LISD_subscales_CFA, LISD_subscales_CFA)
corrplot(Factors_Correl_CFA, type="upper")

Factors_Factors_EFA <- cbind(LISD_subscales_EFA, LISD_subscales_EFA)
Factors_Factors_CFA <- cbind(LISD_subscales_CFA, LISD_subscales_CFA)
apa.cor.table(LISD_subscales_EFA, filename = "Correl_LISD_EFA.doc", show.conf.interval = FALSE)
apa.cor.table(LISD_subscales_CFA, filename = "Correl_LISD_CFA.doc", show.conf.interval = FALSE)


#### CFA: Big Five Correlations


Factors_BigFive_SGSE_CFA <- cbind(LISD_subscales_CFA, BigFive_SGSE_CFA)
Factors_BigFive_SGSE_CFA_Correl <- cor(LISD_subscales_CFA, BigFive_SGSE_CFA)
Factors_BigFive_SGSE_CFA_Correl <- round(Factors_BigFive_SGSE_CFA_Correl, digits = 3)
corrplot(Factors_BigFive_SGSE_CFA_Correl)
BigFive_Quest_CFA <- cbind(BigFive_SGSE_CFA, questionnaires_CFA)
BigFive_Quest_CFA_Correl <- cor(BigFive_SGSE_CFA, questionnaires_CFA)
BigFive_Quest_CFA_Correl <- round(BigFive_Quest_CFA_Correl, digits = 3)
corrplot(BigFive_Quest_CFA_Correl)

# p values Big Five
cor.table(Factors_BigFive_SGSE_CFA)

## clear workspace 

remove(EFA_survey,noout_cfa,noout_cfa_State,noout_cfa_Trait,noSkew_efa,noSkew_efa_State,noSkew_efa_Trait)
remove(BigFive_Quest_CFA, BigFive_Quest_CFA_Correl,BigFive_SGSE_CFA,Factors_BigFive_SGSE_CFA,Factors_BigFive_SGSE_CFA_Correl,Factors_Correl_CFA,Factors_Correl_EFA,Factors_Factors_CFA,Factors_Factors_EFA,
       Factors_Quest_CFA,Factors_Quest_EFA,quest_CFA_Correl,quest_EFA_Correl)
remove(EFA_CFA_occupation,occupation_CFA,occupation_EFA,occupationJob, occupationStudent, TAB_occupationJob,TAB_occupationStudent)

save.image(file = "CFA_data_final.Rda")
