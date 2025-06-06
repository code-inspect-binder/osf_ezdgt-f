#### Load packages

library(psych)
library(ggplot2)
library(nFactors)
library(corrplot)
library(sjPlot)
library(lavaan)
library(knitr)
library(kableExtra)
library(dplyr)

#setwd("F:/Publications/5_Social Distancing Scale/Submission_Frontiers/data and scripts")

#### Import data

load("EFA_data.Rda")


# DATA PREPARATION

## rename WSDS to LISD

EFA_data <- rename(EFA_data, LISD_State_01=WSDS_State_01)
EFA_data <- rename(EFA_data, LISD_State_02=WSDS_State_02)
EFA_data <- rename(EFA_data, LISD_State_03=WSDS_State_03)
EFA_data <- rename(EFA_data, LISD_State_04=WSDS_State_04)
EFA_data <- rename(EFA_data, LISD_State_05=WSDS_State_05)
EFA_data <- rename(EFA_data, LISD_State_06=WSDS_State_06)
EFA_data <- rename(EFA_data, LISD_State_07=WSDS_State_07)
EFA_data <- rename(EFA_data, LISD_State_08=WSDS_State_08)
EFA_data <- rename(EFA_data, LISD_State_09=WSDS_State_09)
EFA_data <- rename(EFA_data, LISD_State_10=WSDS_State_10)
EFA_data <- rename(EFA_data, LISD_State_11=WSDS_State_11)
EFA_data <- rename(EFA_data, LISD_State_12=WSDS_State_12)
EFA_data <- rename(EFA_data, LISD_State_13=WSDS_State_13)
EFA_data <- rename(EFA_data, LISD_State_14=WSDS_State_14)
EFA_data <- rename(EFA_data, LISD_State_15=WSDS_State_15)
EFA_data <- rename(EFA_data, LISD_State_16=WSDS_State_16)
EFA_data <- rename(EFA_data, LISD_State_17=WSDS_State_17)

EFA_data <- rename(EFA_data, LISD_Trait_01=WSDS_Trait_01)
EFA_data <- rename(EFA_data, LISD_Trait_02=WSDS_Trait_02)
EFA_data <- rename(EFA_data, LISD_Trait_03=WSDS_Trait_03)
EFA_data <- rename(EFA_data, LISD_Trait_04=WSDS_Trait_04)
EFA_data <- rename(EFA_data, LISD_Trait_05=WSDS_Trait_05)
EFA_data <- rename(EFA_data, LISD_Trait_06=WSDS_Trait_06)
EFA_data <- rename(EFA_data, LISD_Trait_07=WSDS_Trait_07)
EFA_data <- rename(EFA_data, LISD_Trait_08=WSDS_Trait_08)
EFA_data <- rename(EFA_data, LISD_Trait_09=WSDS_Trait_09)
EFA_data <- rename(EFA_data, LISD_Trait_10=WSDS_Trait_10)
EFA_data <- rename(EFA_data, LISD_Trait_11=WSDS_Trait_11)
EFA_data <- rename(EFA_data, LISD_Trait_12=WSDS_Trait_12)
EFA_data <- rename(EFA_data, LISD_Trait_13=WSDS_Trait_13)
EFA_data <- rename(EFA_data, LISD_Trait_14=WSDS_Trait_14)
EFA_data <- rename(EFA_data, LISD_Trait_15=WSDS_Trait_15)
EFA_data <- rename(EFA_data, LISD_Trait_16=WSDS_Trait_16)
EFA_data <- rename(EFA_data, LISD_Trait_17=WSDS_Trait_17)
EFA_data <- rename(EFA_data, LISD_Trait_18=WSDS_Trait_18)
EFA_data <- rename(EFA_data, LISD_Trait_19=WSDS_Trait_19)
EFA_data <- rename(EFA_data, LISD_Trait_20=WSDS_Trait_20)
EFA_data <- rename(EFA_data, LISD_Trait_21=WSDS_Trait_21)
EFA_data <- rename(EFA_data, LISD_Trait_22=WSDS_Trait_22)
EFA_data <- rename(EFA_data, LISD_Trait_23=WSDS_Trait_23)


## select LISD items only

LISD_efa <- EFA_data[,198:237]
LISD_efa_State <- EFA_data[,198:214]
LISD_efa_Trait <- EFA_data[,215:237]


## EXPLORATORY FA

## Multivariate outliers

mahal = mahalanobis(LISD_efa, colMeans(LISD_efa, na.rm = TRUE), cov(LISD_efa, use = "pairwise.complete"))
summary(mahal)
cutoff = qchisq(1-.001, ncol(LISD_efa)) 
ncol(LISD_efa) 
cutoff
summary (mahal < cutoff) 
noout_efa = subset (LISD_efa, mahal < cutoff)
noout_efa_State <- noout_efa[,1:17]
noout_efa_Trait <- noout_efa[,18:40]


list(LISD_efa, mahal > cutoff)


## Additivity assumption
#run bivariate correlations that are >.90:

correl = cor(noout_efa, use = "pairwise.complete.obs") 
symnum(correl)


## Linearity assumption

random = rchisq(nrow(noout_efa), 7)
fake = lm(random~., data = noout_efa) 
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1) 


## Multivariate normality 

hist(standardized, col="lightblue") 
fitted = scale(fake$fitted.values)
plot(fitted, standardized)
abline(0,0)
abline(v = 0)


## Descriptives & Skewness STATE

kable(describe(noout_efa_State), table.attr = "style='width:30%;'") %>% 
  kableExtra::kable_styling()

multi.hist(noout_efa_State, dcol="red") 


## Descriptives & Skewness TRAIT

kable(describe(noout_efa_Trait), table.attr = "style='width:30%;'") %>% 
  kableExtra::kable_styling()

multi.hist(noout_efa_Trait, dcol="red") 


## Descriptives ALL ITEMS

descriptives_noout_efa <- as.data.frame(describe(noout_efa))
kable(describe(descriptives_noout_efa), table.attr = "style='width:30%;'") %>% 
  kableExtra::kable_styling()


## Exclude Item State 08 und State 14 (Skewness > 2.0)

noSkew_efa <-noout_efa [,-c(8,14)]
save(noSkew_efa, file = "noSkew_efa.RDa")


## Correlations

correl <- cor(noSkew_efa)
correlRound <- round(correl, digits = 3) 

symnum(correl) 

corrplot(correl, type="upper") 

#Portion of correlations <.2:

correlUpper <- as.matrix(correl[upper.tri(correl)]) 
correlSmall <- abs(correlUpper) < 0.2 
sum(correlSmall) 
sum(correlSmall)/nrow(correlUpper) 


## Update assumptions for noSKew

random <- rchisq(nrow(noSkew_efa), 7)
fake <- lm(random~., data = noSkew_efa) 
standardized <- rstudent(fake) 
fitted <- scale(fake$fitted.values) 
#Normality
hist(standardized, breaks = 15) 
#Linearity: 
qqnorm(standardized)
abline(0,1)
#Homogeneity:
plot(fitted,standardized)
abline(0,0)
abline(v = 0)


#### Delete unnecessary objects from workspace

remove(correl,correlRound,correlSmall,correlUpper,descriptives_noout_efa,fake,fitted,noout_efa_Trait,noout_efa_State,cutoff,mahal,random,standardized)


#### separate trait and state

noSkew_efa_State <- noSkew_efa[,c(1:15)]
noSkew_efa_Trait <- noSkew_efa[,c(16:38)]


# Exploratory Factor Analysis: 

# STATE SCALE

## Assumptions

cortest.bartlett(noSkew_efa_State) 
KMO(noSkew_efa_State) 

## Number of factors

#### Parallel analysis

fa.parallel(noSkew_efa_State, fm="pa", fa="fa")

#### MAP test

vss(noSkew_efa_State)

#### Screeplot

eigenvalues_efa <- eigen(cor(noSkew_efa_State))
parallelanalysis_efa <- parallel(subject=nrow(noSkew_efa_State), var=ncol(noSkew_efa_State), rep=100, cent=.05)
nScree_efa <- nScree(x=eigenvalues_efa$values, aparallel=parallelanalysis_efa$eigen$qevpea)
screeplot_efa <- plotnScree(nScree_efa)

remove(eigenvalues_efa, parallelanalysis_efa, nScree_efa, screeplot_efa)


# 2-Factors STATE

#### Communality

efa_notrotated_State_2F <- fa(noSkew_efa_State, nfactors = 2, fm = "pa", rotate="none")
efa_notrotated_State_2F$communality 


#### Exclude Communalities < .20: States 01, 16:

efa_round1_State_2F <- fa(noSkew_efa_State[,-c(1,14)],nfactors = 2 , fm = "pa", rotate="promax")
print(efa_round1_State_2F$loadings, sort= TRUE, cutoff = 0.3)

efa_round1_State_2F
#--> Crossloadings with delta <.20 & Communality <.50: State13 (but second loading <.35)
#--> Crossloading only: State04, State17 (loading <.35), State06


#### Exclude Loadings <.35:

efa_State_2F_round2 <- fa(noSkew_efa_State[,-c(1,14)], nfactors = 2, fm = "pa", rotate="promax")
print(efa_State_2F_round2$loadings, sort= TRUE, cutoff = 0.349)


efa_State_2F_round2
#--> Crossloading only: State04, State06
#--> State09: kept, Loading .349


#### After reliability check (see below): Exclude State05, State09, State10

efa_round3_State_2F <- fa(noSkew_efa_State[,-c(1,5,8,9,14)], nfactors = 2, fm = "pa", rotate="promax")
print(efa_round3_State_2F$loadings, sort= TRUE, cutoff = 0.349)


efa_round3_State_2F

efa_round4_State_2F <- fa(noSkew_efa_State[,-c(1,5,8,9,12,14)], nfactors = 2, fm = "pa", rotate="promax")
print(efa_round4_State_2F$loadings, sort= TRUE, cutoff = 0.349)


efa_round4_State_2F
#--> Crossloading State13, Communality < .50 --> remove


### Alternative: exclude State11 (measurement error in CFA survey), compare solutions

#### Communality

efa_notrotated_State_2F <- fa(noSkew_efa_State, nfactors = 2, fm = "pa", rotate="none")
efa_notrotated_State_2F$communality 

#### Exclude communalities and State11

efa_round1_State_2F_wo11 <- fa(noSkew_efa_State[,-c(1,10,14)], nfactors = 2, fm = "pa", rotate="promax")
print(efa_round1_State_2F_wo11$loadings, sort= TRUE, cutoff = 0.349)


efa_round1_State_2F_wo11
#--> Crossloading States 04, 06, but communalities >.50


## define possible 2-Factor solutions for comparison

#2 Factors
efa_State_2F <- efa_State_2F_round2 
#without States 05, 09, 11, 13
efa_State_2F_strict <- efa_round4_State_2F 
#without State11
efa_State_2F_wo11 <- efa_round1_State_2F_wo11


## Fit Indices

##### original

efa_CFI_State_2F <- 1 - ((efa_State_2F$STATISTIC-efa_State_2F$dof)/(efa_State_2F$null.chisq-efa_State_2F$null.dof))
efa_CFI_State_2F
efa_State_2F

##### without State11

efa_CFI_State_2F_wo11 <- 1 - ((efa_State_2F_wo11$STATISTIC-efa_State_2F_wo11$dof)/(efa_State_2F_wo11$null.chisq-efa_State_2F_wo11$null.dof))
efa_CFI_State_2F_wo11
efa_State_2F_wo11


##### after considering reliability, difficulty, discimination:
##### without States 05, 09, 10, 13 

efa_CFI_State_2F_strict <- 1 - ((efa_State_2F_strict$STATISTIC-efa_State_2F_strict$dof)/(efa_State_2F_strict$null.chisq-efa_State_2F_strict$null.dof))
efa_CFI_State_2F_strict
efa_State_2F_strict


## Subscales & Reliability

##### original


efa_factor1_State_2F <- noSkew_efa_State[,c(2,4,5,6,7,8,12,13,15)] 
efa_factor2_State_2F <- noSkew_efa_State[,c(3,9:11)] 
psych::alpha(efa_factor1_State_2F, check.keys = TRUE) 
psych::alpha(efa_factor2_State_2F, check.keys = TRUE)
#--> Exclusion State05 weakens fit


##### without State11

efa_factor1_State_2F_wo11 <- noSkew_efa_State[,c(2,4,5,6,7,8,12,13,15)] 
efa_factor2_State_2F_wo11 <- noSkew_efa_State[,c(3,9,11)] 
psych::alpha(efa_factor1_State_2F_wo11, check.keys = TRUE) 
psych::alpha(efa_factor2_State_2F_wo11, check.keys = TRUE) 
#--> Items State05,09 not removed
#--> leads to exclusion State12 --> Factor2 only 2 items


##### without States 05, 09, 10, 13

efa_factor1_State_2F_strict <- noSkew_efa_State[,c(2,4,6,7,13,15)]
efa_factor2_State_2F_strict <- noSkew_efa_State[,c(3,10,11)] 
psych::alpha(efa_factor1_State_2F_strict, check.keys = TRUE) 
psych::alpha(efa_factor2_State_2F_strict, check.keys = TRUE) 


# Discrimination & Difficulty

### 2 Factors STATE original

#Factor 1
ia_F1_state_2F <- psych::alpha(efa_factor1_State_2F, check.keys = TRUE) 
cronbachsalpha_F1_state_2F <- ia_F1_state_2F$total$raw_alpha
alphaMinusItem_F1_state_2F <- ia_F1_state_2F$alpha.drop$raw_alpha   
itemDiscrimination_F1_state_2F <- ia_F1_state_2F$item.stats$r.drop       
itemDifficulty_F1_state_2F <- ia_F1_state_2F$item.stats$mean/5       

ia.tabelle_F1_state_2F <- data.frame(
  cronbachsalpha_F1_state_2F, alphaMinusItem_F1_state_2F, itemDiscrimination_F1_state_2F, itemDifficulty_F1_state_2F)    
ia.tabelle_F1_state_2F <- round(ia.tabelle_F1_state_2F, 4)
rownames(ia.tabelle_F1_state_2F) <- colnames(efa_factor1_State_2F)
ia.tabelle_F1_state_2F


#Factor 2
ia_F2_state_2F <- psych::alpha(efa_factor2_State_2F, check.keys = TRUE) 
cronbachsalpha_F2_state_2F <- ia_F2_state_2F$total$raw_alpha
alphaMinusItem_F2_state_2F <- ia_F2_state_2F$alpha.drop$raw_alpha   
itemDiscrimination_F2_state_2F <- ia_F2_state_2F$item.stats$r.drop       
itemDifficulty_F2_state_2F <- ia_F2_state_2F$item.stats$mean/5       

ia.tabelle_F2_state_2F <- data.frame(
  cronbachsalpha_F2_state_2F, alphaMinusItem_F2_state_2F, itemDiscrimination_F2_state_2F, itemDifficulty_F2_state_2F)    
ia.tabelle_F2_state_2F <- round(ia.tabelle_F2_state_2F, 4)
rownames(ia.tabelle_F2_state_2F) <- colnames(efa_factor2_State_2F)
ia.tabelle_F2_state_2F


### 2 Factors STATE without State11

#Factor 1
ia_F1_state_2F_wo11 <- psych::alpha(efa_factor1_State_2F_wo11, check.keys = TRUE) 
cronbachsalpha_F1_state_2F_wo11 <- ia_F1_state_2F_wo11$total$raw_alpha
alphaMinusItem_F1_state_2F_wo11 <- ia_F1_state_2F_wo11$alpha.drop$raw_alpha   
itemDiscrimination_F1_state_2F_wo11 <- ia_F1_state_2F_wo11$item.stats$r.drop       
itemDifficulty_F1_state_2F_wo11 <- ia_F1_state_2F_wo11$item.stats$mean/5       

ia.tabelle_F1_state_2F_wo11 <- data.frame(
  cronbachsalpha_F1_state_2F_wo11, alphaMinusItem_F1_state_2F_wo11, itemDiscrimination_F1_state_2F_wo11, itemDifficulty_F1_state_2F_wo11)    
ia.tabelle_F1_state_2F_wo11 <- round(ia.tabelle_F1_state_2F_wo11, 4)
rownames(ia.tabelle_F1_state_2F_wo11) <- colnames(efa_factor1_State_2F_wo11)
ia.tabelle_F1_state_2F_wo11


#Factor 2
ia_F2_state_2F_wo11 <- psych::alpha(efa_factor2_State_2F_wo11, check.keys = TRUE)
cronbachsalpha_F2_state_2F_wo11 <- ia_F2_state_2F_wo11$total$raw_alpha
alphaMinusItem_F2_state_2F_wo11 <- ia_F2_state_2F_wo11$alpha.drop$raw_alpha   
itemDiscrimination_F2_state_2F_wo11 <- ia_F2_state_2F_wo11$item.stats$r.drop       
itemDifficulty_F2_state_2F_wo11 <- ia_F2_state_2F_wo11$item.stats$mean/5       

ia.tabelle_F2_state_2F_wo11 <- data.frame(
  cronbachsalpha_F2_state_2F_wo11, alphaMinusItem_F2_state_2F_wo11, itemDiscrimination_F2_state_2F_wo11, itemDifficulty_F2_state_2F_wo11)    
ia.tabelle_F2_state_2F_wo11 <- round(ia.tabelle_F2_state_2F_wo11, 4)
rownames(ia.tabelle_F2_state_2F_wo11) <- colnames(efa_factor2_State_2F_wo11)
ia.tabelle_F2_state_2F_wo11


## Final 2-Factor Solution

efa_output_state <- tab_fa(noSkew_efa_State[,-c(1,10,14)], nmbr.fctr = 2, method = "pa", rotation ="promax",fctr.load.tlrn = 0,
                                title ="EFA State 2 Factor Solution", show.cronb = FALSE)
efa_output_state



# power analysis
semTools::findRMSEApower(.05, .08, 43, 244, .05, 1) #(rmsea0, rmseaA, df, n, alpha = 0.05, group = 1)


## clean workspace
remove(efa_State_2F,efa_State_2F_strict, efa_State_2F_wo11)
remove(efa_factor1_State_2F,efa_factor1_State_2F_strict,efa_factor2_State_2F,efa_factor2_State_2F_strict)
remove(efa_notrotated_State_2F,efa_output_state,efa_round1_State_2F,efa_round1_State_2F_wo11,efa_State_2F_round2,efa_round3_State_2F,efa_round4_State_2F)
remove(ia_F1_state_2F,ia_F1_state_2F_wo11,ia_F2_state_2F,ia_F2_state_2F_wo11)
remove(ia.tabelle_F1_state_2F,ia.tabelle_F1_state_2F_wo11,ia.tabelle_F2_state_2F,ia.tabelle_F2_state_2F_wo11)
remove(alphaMinusItem_F1_state_2F,alphaMinusItem_F1_state_2F_wo11,alphaMinusItem_F2_state_2F,alphaMinusItem_F2_state_2F_wo11,cronbachsalpha_F1_state_2F,cronbachsalpha_F1_state_2F_wo11,cronbachsalpha_F2_state_2F,cronbachsalpha_F2_state_2F_wo11,efa_CFI_State_2F,efa_CFI_State_2F_strict,efa_CFI_State_2F_wo11)
remove(itemDifficulty_F1_state_2F,itemDifficulty_F1_state_2F_wo11,itemDifficulty_F2_state_2F,itemDifficulty_F2_state_2F_wo11,itemDiscrimination_F1_state_2F,itemDiscrimination_F1_state_2F_wo11,itemDiscrimination_F2_state_2F,itemDiscrimination_F2_state_2F_wo11)




# Exploratory Factor Analysis: 
# TRAIT SCALE


#NOTE. 
#first EFA uses same criteria (including liberties) as EFA with State scale. 
#due to the aim of reducing the item number on the originally longer trait scale, the EFA is then redone with more strict exclusion criteria. 

## Assumptions

cortest.bartlett(noSkew_efa_Trait)
KMO(noSkew_efa_Trait)


## Number of factors

#### Parallel analysis

fa.parallel(noSkew_efa_State, fm="pa", fa="fa")

#### MAP test

vss(noSkew_efa_State)

#### Screeplot

eigenvalues_efa_Trait <- eigen(cor(noSkew_efa_Trait))
parallelanalysis_efa_Trait <- parallel(subject=nrow(noSkew_efa_Trait), var=ncol(noSkew_efa_Trait), rep=100, cent=.05)
nScree_efa_Trait <- nScree(x=eigenvalues_efa_Trait$values, aparallel=parallelanalysis_efa_Trait$eigen$qevpea)
screeplot_efa_Trait <- plotnScree(nScree_efa_Trait)


# 3-Factor-Solution TRAIT

#### Communality 

efa_notrotated_Trait_3F <- fa(noSkew_efa_Trait, nfactors = 3, fm = "pa", rotate="none")
efa_notrotated_Trait_3F$communality


#### Exclude communalities <.2: Traits 03, 06, 19 

efa_round1_Trait_3F <- fa(noSkew_efa_Trait[,-c(3,6,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round1_Trait_3F$loadings, sort= TRUE, cutoff = 0.3)

efa_round1_Trait_3F


#### Exclude Loadings <.35:

efa_round2_Trait_3F <- fa(noSkew_efa_Trait[,-c(3,6,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round2_Trait_3F$loadings, sort= TRUE, cutoff = 0.349)

efa_round2_Trait_3F

#### Exclude Trait 08:

efa_round3_Trait_3F <- fa(noSkew_efa_Trait[,-c(3,6,8,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round3_Trait_3F$loadings, sort= TRUE, cutoff = 0.349)

efa_round3_Trait_3F

#### Exclude Trait 14:

efa_round4_Trait_3F <- fa(noSkew_efa_Trait[,-c(3,6,8,14,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round4_Trait_3F$loadings, sort= TRUE, cutoff = 0.349)

efa_round4_Trait_3F


## 3-Factor Solution: 

efa_Trait_3F <- efa_round4_Trait_3F 


## Fit Indices

efa_CFI_Trait_3F <- 1 - ((efa_Trait_3F$STATISTIC-efa_Trait_3F$dof)/(efa_Trait_3F$null.chisq-efa_Trait_3F$null.dof))
efa_CFI_Trait_3F
efa_Trait_3F

## Factor Solution

efa_output_Trait_3F <- tab_fa(noSkew_efa_Trait[,-c(3,6,8,14,19)], nmbr.fctr = 3, method = "pa", rotation ="promax",fctr.load.tlrn = 0,
                              title ="EFA 3 Factor Solution Trait", show.cronb = FALSE)
efa_output_Trait_3F


## Reliability

efa_factor1_Trait_3F <- noSkew_efa_Trait[,c(2,4,7,13,16,21,22,23)] 
efa_factor2_Trait_3F <- noSkew_efa_Trait[,c(1,9,10,11,15,17,18,20)]
efa_factor3_Trait_3F <- noSkew_efa_Trait[,c(5,12)]
psych::alpha(efa_factor1_Trait_3F, check.keys = TRUE)
psych::alpha(efa_factor2_Trait_3F, check.keys = TRUE)
psych::alpha(efa_factor3_Trait_3F, check.keys = TRUE) 



## Discrimination & Difficulty 

#Factor 1
ia_F1_trait_3F <- psych::alpha(efa_factor1_Trait_3F, check.keys = TRUE) 
cronbachsalpha_F1_trait_3F <- ia_F1_trait_3F$total$raw_alpha
alphaMinusItem_F1_trait_3F <- ia_F1_trait_3F$alpha.drop$raw_alpha   
itemDiscrimination_F1_trait_3F <- ia_F1_trait_3F$item.stats$r.drop       
itemDifficulty_F1_trait_3F <- ia_F1_trait_3F$item.stats$mean/5       

ia.tabelle_F1_trait_3F <- data.frame(
  cronbachsalpha_F1_trait_3F, alphaMinusItem_F1_trait_3F, itemDiscrimination_F1_trait_3F, itemDifficulty_F1_trait_3F)    
ia.tabelle_F1_trait_3F <- round(ia.tabelle_F1_trait_3F, 4)
rownames(ia.tabelle_F1_trait_3F) <- colnames(efa_factor1_Trait_3F)
ia.tabelle_F1_trait_3F

#Factor 2
ia_F2_trait_3F <- psych::alpha(efa_factor2_Trait_3F, check.keys = TRUE) 
cronbachsalpha_F2_trait_3F <- ia_F2_trait_3F$total$raw_alpha
alphaMinusItem_F2_trait_3F <- ia_F2_trait_3F$alpha.drop$raw_alpha   
itemDiscrimination_F2_trait_3F <- ia_F2_trait_3F$item.stats$r.drop       
itemDifficulty_F2_trait_3F <- ia_F2_trait_3F$item.stats$mean/5       

ia.tabelle_F2_trait_3F <- data.frame(
  cronbachsalpha_F2_trait_3F, alphaMinusItem_F2_trait_3F, itemDiscrimination_F2_trait_3F, itemDifficulty_F2_trait_3F)    
ia.tabelle_F2_trait_3F <- round(ia.tabelle_F2_trait_3F, 4)
rownames(ia.tabelle_F2_trait_3F) <- colnames(efa_factor2_Trait_3F)
ia.tabelle_F2_trait_3F

#Factor 3
ia_F3_trait_3F <- psych::alpha(efa_factor3_Trait_3F, check.keys = TRUE) 
cronbachsalpha_F3_trait_3F <- ia_F3_trait_3F$total$raw_alpha
alphaMinusItem_F3_trait_3F <- ia_F3_trait_3F$alpha.drop$raw_alpha   
itemDiscrimination_F3_trait_3F <- ia_F3_trait_3F$item.stats$r.drop       
itemDifficulty_F3_trait_3F <- ia_F3_trait_3F$item.stats$mean/5       

ia.tabelle_F3_trait_3F <- data.frame(
  cronbachsalpha_F3_trait_3F, alphaMinusItem_F3_trait_3F, itemDiscrimination_F3_trait_3F, itemDifficulty_F3_trait_3F)    
ia.tabelle_F3_trait_3F <- round(ia.tabelle_F3_trait_3F, 4)
rownames(ia.tabelle_F3_trait_3F) <- colnames(efa_factor3_Trait_3F)
ia.tabelle_F3_trait_3F


# RE-DO: 3-Factor EFA TRAIT-Scale 
#### without Trait05

#### Communality 

efa_notrotated_Trait_3F <- fa(noSkew_efa_Trait[,-c(5)], nfactors = 3, fm = "pa", rotate="none")
efa_notrotated_Trait_3F$communality


#### Exclude communalities <.2: Traits 03, 06, 19 + Trait05
#### Exclude Loadings <.35

efa_round1_Trait_3F <- fa(noSkew_efa_Trait[,-c(3,5,6,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round1_Trait_3F$loadings, sort= TRUE, cutoff = 0.349)

efa_round1_Trait_3F


#### Exclude Trait08 (Loads <.35), Trait 18 (Crossloading):

efa_round2_Trait_3F <- fa(noSkew_efa_Trait[,-c(3,5,6,8,18,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round2_Trait_3F$loadings, sort= TRUE, cutoff = 0.349)

efa_round2_Trait_3F
#--> Crossloading Trait04 (F1), BUT comm .63

## 3-Factor Solution: 

efa_Trait_3F <- efa_round2_Trait_3F 

## Fit Indices

efa_CFI_Trait_3F <- 1 - ((efa_Trait_3F$STATISTIC-efa_Trait_3F$dof)/(efa_Trait_3F$null.chisq-efa_Trait_3F$null.dof))
efa_CFI_Trait_3F
efa_Trait_3F

## Factor Solution

efa_output_Trait_3F <- tab_fa(noSkew_efa_Trait[,-c(3,5,6,8,18,19)], nmbr.fctr = 3, method = "pa", rotation ="promax",fctr.load.tlrn = 0,
                              title ="EFA 3 Factor Solution Trait w/o Trait05", show.cronb = FALSE)
efa_output_Trait_3F


## Reliability

efa_factor1_Trait_3F <- noSkew_efa_Trait[,c(1,9,10,11,15,17,20)] 
efa_factor2_Trait_3F <- noSkew_efa_Trait[,c(2,7,16,21,22,23)]
efa_factor3_Trait_3F <- noSkew_efa_Trait[,c(4,12,13,14)]
psych::alpha(efa_factor1_Trait_3F, check.keys = TRUE)
psych::alpha(efa_factor2_Trait_3F, check.keys = TRUE)
psych::alpha(efa_factor3_Trait_3F, check.keys = TRUE) 


## Discrimination & Difficulty

#Faktor 1
ia_F1_trait_3F <- psych::alpha(efa_factor1_Trait_3F, check.keys = TRUE) 
cronbachsalpha_F1_trait_3F <- ia_F1_trait_3F$total$raw_alpha
alphaMinusItem_F1_trait_3F <- ia_F1_trait_3F$alpha.drop$raw_alpha   
itemDiscrimination_F1_trait_3F <- ia_F1_trait_3F$item.stats$r.drop       
itemDifficulty_F1_trait_3F <- ia_F1_trait_3F$item.stats$mean/5       

ia.tabelle_F1_trait_3F <- data.frame(
  cronbachsalpha_F1_trait_3F, alphaMinusItem_F1_trait_3F, itemDiscrimination_F1_trait_3F, itemDifficulty_F1_trait_3F)    
ia.tabelle_F1_trait_3F <- round(ia.tabelle_F1_trait_3F, 4)
rownames(ia.tabelle_F1_trait_3F) <- colnames(efa_factor1_Trait_3F)
ia.tabelle_F1_trait_3F


#Faktor 2
ia_F2_trait_3F <- psych::alpha(efa_factor2_Trait_3F, check.keys = TRUE) 
cronbachsalpha_F2_trait_3F <- ia_F2_trait_3F$total$raw_alpha
alphaMinusItem_F2_trait_3F <- ia_F2_trait_3F$alpha.drop$raw_alpha   
itemDiscrimination_F2_trait_3F <- ia_F2_trait_3F$item.stats$r.drop       
itemDifficulty_F2_trait_3F <- ia_F2_trait_3F$item.stats$mean/5       

ia.tabelle_F2_trait_3F <- data.frame(
  cronbachsalpha_F2_trait_3F, alphaMinusItem_F2_trait_3F, itemDiscrimination_F2_trait_3F, itemDifficulty_F2_trait_3F)    
ia.tabelle_F2_trait_3F <- round(ia.tabelle_F2_trait_3F, 4)
rownames(ia.tabelle_F2_trait_3F) <- colnames(efa_factor2_Trait_3F)
ia.tabelle_F2_trait_3F


#Faktor 3
ia_F3_trait_3F <- psych::alpha(efa_factor3_Trait_3F, check.keys = TRUE) 
cronbachsalpha_F3_trait_3F <- ia_F3_trait_3F$total$raw_alpha
alphaMinusItem_F3_trait_3F <- ia_F3_trait_3F$alpha.drop$raw_alpha   
itemDiscrimination_F3_trait_3F <- ia_F3_trait_3F$item.stats$r.drop       
itemDifficulty_F3_trait_3F <- ia_F3_trait_3F$item.stats$mean/5       

ia.tabelle_F3_trait_3F <- data.frame(
  cronbachsalpha_F3_trait_3F, alphaMinusItem_F3_trait_3F, itemDiscrimination_F3_trait_3F, itemDifficulty_F3_trait_3F)    
ia.tabelle_F3_trait_3F <- round(ia.tabelle_F3_trait_3F, 4)
rownames(ia.tabelle_F3_trait_3F) <- colnames(efa_factor3_Trait_3F)
ia.tabelle_F3_trait_3F





# REDO: 3-Factor EFA TRAIT-Scale 
#### without Traits 05, 07 & 17 (Reliability), 11 & 15 (Difficulty)

#### Exclude communalities <.2: Traits 03, 06, 19

efa_round1_Trait_3F_strict <- fa(noSkew_efa_Trait[,-c(3,5,6,7,11,15,17,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round1_Trait_3F_strict$loadings, sort= TRUE, cutoff = 0.3)


efa_round1_Trait_3F_strict

#### Exclude Trait08, Loadings <.35

efa_round2_Trait_3F_strict <- fa(noSkew_efa_Trait[,-c(3,5,6,7,8,11,15,17,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round2_Trait_3F_strict$loadings, sort= TRUE, cutoff = 0.349)


efa_round2_Trait_3F_strict

#### Exclude Trait13, Loadings <.35

efa_round3_Trait_3F_strict <- fa(noSkew_efa_Trait[,-c(3,5,6,7,8,11,13,15,17,19)], nfactors = 3, fm = "pa", rotate="promax")
print(efa_round3_Trait_3F_strict$loadings, sort= TRUE, cutoff = 0.349)


efa_round3_Trait_3F_strict
#--> keep 13, otherwise Factor3 only 1 item


## 3-Factor Solution: 

efa_Trait_3F_strict <- efa_round2_Trait_3F_strict 


## Fit Indices

efa_CFI_Trait_3F_strict <- 1 - ((efa_Trait_3F_strict$STATISTIC-efa_Trait_3F_strict$dof)/(efa_Trait_3F_strict$null.chisq-efa_Trait_3F_strict$null.dof))
efa_CFI_Trait_3F_strict
efa_Trait_3F_strict

## Factor Solution

efa_output_Trait_3F_strict <- tab_fa(noSkew_efa_Trait[,-c(3,5,6,7,8,11,15,17,19)], nmbr.fctr = 3, method = "pa", rotation ="promax", fctr.load.tlrn = 0,
                                     title ="EFA 3 Factor Solution Trait Strict", show.cronb = FALSE)
efa_output_Trait_3F_strict


## Reliability

efa_factor1_Trait_3F_strict <- noSkew_efa_Trait[,c(2,16,21,22,23)]
efa_factor2_Trait_3F_strict <- noSkew_efa_Trait[,c(1,9,10,18,20)] #
efa_factor3_Trait_3F_strict <- noSkew_efa_Trait[,c(4,12,13,14)] 
psych::alpha(efa_factor1_Trait_3F_strict, check.keys = TRUE) 
psych::alpha(efa_factor2_Trait_3F_strict, check.keys = TRUE) 
psych::alpha(efa_factor3_Trait_3F_strict, check.keys = TRUE)


## Discrimination & Difficulty

#Faktor 1
ia_F1_trait_3F_strict <- psych::alpha(efa_factor1_Trait_3F_strict, check.keys = TRUE) 
cronbachsalpha_F1_trait_3F_strict <- ia_F1_trait_3F_strict$total$raw_alpha
alphaMinusItem_F1_trait_3F_strict <- ia_F1_trait_3F_strict$alpha.drop$raw_alpha   
itemDiscrimination_F1_trait_3F_strict <- ia_F1_trait_3F_strict$item.stats$r.drop       
itemDifficulty_F1_trait_3F_strict <- ia_F1_trait_3F_strict$item.stats$mean/5       

ia.tabelle_F1_trait_3F_strict <- data.frame(
  cronbachsalpha_F1_trait_3F_strict, alphaMinusItem_F1_trait_3F_strict, itemDiscrimination_F1_trait_3F_strict, itemDifficulty_F1_trait_3F_strict)    
ia.tabelle_F1_trait_3F_strict <- round(ia.tabelle_F1_trait_3F_strict, 4)
rownames(ia.tabelle_F1_trait_3F_strict) <- colnames(efa_factor1_Trait_3F_strict)
ia.tabelle_F1_trait_3F_strict


#Faktor 2
ia_F2_trait_3F_strict <- psych::alpha(efa_factor2_Trait_3F_strict, check.keys = TRUE) 
cronbachsalpha_F2_trait_3F_strict <- ia_F2_trait_3F_strict$total$raw_alpha
alphaMinusItem_F2_trait_3F_strict <- ia_F2_trait_3F_strict$alpha.drop$raw_alpha   
itemDiscrimination_F2_trait_3F_strict <- ia_F2_trait_3F_strict$item.stats$r.drop       
itemDifficulty_F2_trait_3F_strict <- ia_F2_trait_3F_strict$item.stats$mean/5       

ia.tabelle_F2_trait_3F_strict <- data.frame(
  cronbachsalpha_F2_trait_3F_strict, alphaMinusItem_F2_trait_3F_strict, itemDiscrimination_F2_trait_3F_strict, itemDifficulty_F2_trait_3F_strict)    
ia.tabelle_F2_trait_3F_strict <- round(ia.tabelle_F2_trait_3F_strict, 4)
rownames(ia.tabelle_F2_trait_3F_strict) <- colnames(efa_factor2_Trait_3F_strict)
ia.tabelle_F2_trait_3F_strict


#Faktor 3
ia_F3_trait_3F_strict <- psych::alpha(efa_factor3_Trait_3F_strict, check.keys = TRUE) 
cronbachsalpha_F3_trait_3F_strict <- ia_F3_trait_3F_strict$total$raw_alpha
alphaMinusItem_F3_trait_3F_strict <- ia_F3_trait_3F_strict$alpha.drop$raw_alpha   
itemDiscrimination_F3_trait_3F_strict <- ia_F3_trait_3F_strict$item.stats$r.drop       
itemDifficulty_F3_trait_3F_strict <- ia_F3_trait_3F_strict$item.stats$mean/5       

ia.tabelle_F3_trait_3F_strict <- data.frame(
  cronbachsalpha_F3_trait_3F_strict, alphaMinusItem_F3_trait_3F_strict, itemDiscrimination_F3_trait_3F_strict, itemDifficulty_F3_trait_3F_strict)    
ia.tabelle_F3_trait_3F_strict <- round(ia.tabelle_F3_trait_3F_strict, 4)
rownames(ia.tabelle_F3_trait_3F_strict) <- colnames(efa_factor3_Trait_3F_strict)
ia.tabelle_F3_trait_3F_strict


## Final 3-Factor Solution TRAIT

efa_output_Trait_3F_strict



# power analysis
semTools::findRMSEApower(.05, .08, 52, 244, .05, 1) #(rmsea0, rmseaA, df, n, alpha = 0.05, group = 1)



## final omega and alpha

### state

psych::omega(efa_factor1_State_2F_wo11, check.keys = TRUE) 
psych::omega(efa_factor2_State_2F_wo11, check.keys = TRUE) 


## trait

efa_factor1_Trait_3F_strict_wo21 <- noSkew_efa_Trait[,c(2,16,22,23)]
#with 21
psych::omega(efa_factor1_Trait_3F_strict, check.keys = TRUE) 
#without 21
psych::omega(efa_factor1_Trait_3F_strict_wo21, check.keys = TRUE) 
psych::omega(efa_factor2_Trait_3F_strict, check.keys = TRUE) 
psych::omega(efa_factor3_Trait_3F_strict, check.keys = TRUE)



## clean workspace
remove(efa_factor1_State_2F_wo11,efa_factor1_Trait_3F,efa_factor1_Trait_3F_strict,efa_factor2_State_2F_wo11,efa_factor2_Trait_3F,efa_factor2_Trait_3F_strict,efa_factor3_Trait_3F,efa_factor3_Trait_3F_strict,efa_factor1_Trait_3F_strict_wo21)
remove(efa_Trait_3F,efa_Trait_3F_strict)
remove(efa_notrotated_Trait_3F,efa_output_Trait_3F,efa_output_Trait_3F_strict,efa_round1_Trait_3F,efa_round1_Trait_3F_strict,efa_round2_Trait_3F,efa_round2_Trait_3F_strict,efa_round3_Trait_3F,efa_round3_Trait_3F_strict,efa_round4_Trait_3F)
remove(eigenvalues_efa_Trait,ia_F1_trait_3F,ia_F1_trait_3F_strict,ia_F2_trait_3F,ia_F2_trait_3F_strict,ia_F3_trait_3F,ia_F3_trait_3F_strict)
remove(ia.tabelle_F1_trait_3F,ia.tabelle_F1_trait_3F_strict,ia.tabelle_F2_trait_3F,ia.tabelle_F2_trait_3F_strict,ia.tabelle_F3_trait_3F,ia.tabelle_F3_trait_3F_strict)
remove(nScree_efa_Trait,parallelanalysis_efa_Trait,screeplot_efa_Trait)
remove(alphaMinusItem_F1_trait_3F,alphaMinusItem_F1_trait_3F_strict,alphaMinusItem_F2_trait_3F,alphaMinusItem_F2_trait_3F_strict,alphaMinusItem_F3_trait_3F,alphaMinusItem_F3_trait_3F_strict,cronbachsalpha_F1_trait_3F,cronbachsalpha_F1_trait_3F_strict,cronbachsalpha_F2_trait_3F,cronbachsalpha_F2_trait_3F_strict,cronbachsalpha_F3_trait_3F,cronbachsalpha_F3_trait_3F_strict)
remove(efa_CFI_Trait_3F,efa_CFI_Trait_3F_strict,itemDifficulty_F1_trait_3F,itemDifficulty_F1_trait_3F_strict,itemDifficulty_F2_trait_3F,itemDifficulty_F2_trait_3F_strict,itemDifficulty_F3_trait_3F,itemDifficulty_F3_trait_3F_strict,itemDiscrimination_F1_trait_3F,itemDiscrimination_F1_trait_3F_strict,itemDiscrimination_F2_trait_3F,itemDiscrimination_F2_trait_3F_strict,itemDiscrimination_F3_trait_3F,itemDiscrimination_F3_trait_3F_strict)



## save final EFA set
save.image("EFA_data_final.Rda")



