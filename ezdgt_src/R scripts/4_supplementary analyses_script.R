
## SUPPLEMENTARY ANALSES ##
library(dplyr)
library(heplots)
library(car)
library(psych)

load("EFA_data_final.Rda")
load("CFA_data_final.Rda")



# Differences between samples

### STATE: 2-Factor solution

### Distributions

#EFA sample State F1
hist(EFA_data$LISD_State_F1, freq=FALSE, col="gray", saab="EFA_data$LISD_State_F1", main="LISD State Faktor 1 EFA")
curve(dnorm(x, mean=mean(EFA_data$LISD_State_F1), sd=sd(EFA_data$LISD_State_F1)), add=TRUE, col="red")
#CFA sample State F1
hist(CFA_data$LISD_State_F1, freq=FALSE, col="gray", dave="CFA_data$LISD_State_F1", main="LISD State Faktor 1 CFA")
curve(dnorm(x, mean=mean(CFA_data$LISD_State_F1), sd=sd(CFA_data$LISD_State_F1)), add=TRUE, col="red")
#EFA sample State F2
hist(EFA_data$LISD_State_F2, freq=FALSE, col="gray", xlab="EFA_data$LISD_State_F2", main="LISD State Faktor 2 EFA")
curve(dnorm(x, mean=mean(EFA_data$LISD_State_F2), sd=sd(EFA_data$LISD_State_F2)), add=TRUE, col="red")
#CFA sample State F2
hist(CFA_data$LISD_State_F2, freq=FALSE, col="gray", xlab="CFA_data$LISD_State_F2", main="LISD State Faktor 2 CFA")
curve(dnorm(x, mean=mean(CFA_data$LISD_State_F2), sd=sd(CFA_data$LISD_State_F2)), add=TRUE, col="red")


### TRAIT: strict 3-Factor solution

### Distributions

#EFA sample Trait F1
hist(EFA_data$LISD_Trait_F1, freq=FALSE, col="gray", xlab="EFA_data$LISD_Trait_F1", main="LISD Trait Faktor 1 EFA")
curve(dnorm(x, mean=mean(EFA_data$LISD_Trait_F1), sd=sd(EFA_data$LISD_Trait_F1)), add=TRUE, col="red")
#CFA sample Trait F1
hist(CFA_data$LISD_Trait_F1, freq=FALSE, col="gray", xlab="CFA_data$LISD_Trait_F1", main="LISD Trait Faktor 1 CFA")
curve(dnorm(x, mean=mean(CFA_data$LISD_Trait_F1), sd=sd(CFA_data$LISD_Trait_F1)), add=TRUE, col="red")
#EFA sample Trait F2
hist(EFA_data$LISD_Trait_F2, freq=FALSE, col="gray", xlab="EFA_data$LISD_Trait_F2", main="LISD Trait Faktor 2 EFA")
curve(dnorm(x, mean=mean(EFA_data$LISD_Trait_F2), sd=sd(EFA_data$LISD_Trait_F2)), add=TRUE, col="red")
#CFA sample Trait F2
hist(CFA_data$LISD_Trait_F2, freq=FALSE, col="gray", xlab="CFA_data$LISD_Trait_F2", main="LISD Trait Faktor 2 CFA")
curve(dnorm(x, mean=mean(CFA_data$LISD_Trait_F2), sd=sd(CFA_data$LISD_Trait_F2)), add=TRUE, col="red")
#EFA sample Trait F3
hist(EFA_data$LISD_Trait_F3, freq=FALSE, col="gray", xlab="EFA_data$LISD_Trait_F3", main="LISD Trait Faktor 3 EFA")
curve(dnorm(x, mean=mean(EFA_data$LISD_Trait_F3), sd=sd(EFA_data$LISD_Trait_F3)), add=TRUE, col="red")
#CFA sample Trait F3
hist(CFA_data$LISD_Trait_F3, freq=FALSE, col="gray", xlab="CFA_data$LISD_Trait_F3", main="LISD Trait Faktor 3 CFA")
curve(dnorm(x, mean=mean(CFA_data$LISD_Trait_F3), sd=sd(CFA_data$LISD_Trait_F3)), add=TRUE, col="red")


### Differences in Questionnaire Scores between Samples

describeBy(EFA_CFA_quest, EFA_CFA_quest$Sample)

#### EFA
descr_all_questionnaires_EFA <- describe(all_questionnaires_EFA)
descr_all_questionnaires_EFA <- round(descr_all_questionnaires_EFA, digits = 2)
descr_all_questionnaires_EFA
descr_sociodemographics_EFA <- describe(sociodemographics_EFA)
descr_sociodemographics_EFA <- round(descr_sociodemographics_EFA, digits = 2)
descr_sociodemographics_EFA
#### CFA
descr_all_questionnaires_CFA <- describe(all_questionnaires_CFA)
descr_all_questionnaires_CFA <- round(descr_all_questionnaires_CFA, digits = 2)
descr_all_questionnaires_CFA
descr_sociodemographics_CFA <- describe(sociodemographics_CFA)
descr_sociodemographics_CFA <- round(descr_sociodemographics_CFA, digits = 2)
descr_sociodemographics_CFA


##### Levene Tests

#questionnaires
leveneTest(EFA_CFA_quest$STAIState_sum, EFA_CFA_quest$Sample) #sign
leveneTest(EFA_CFA_quest$STAITrait_sum, EFA_CFA_quest$Sample) #sign
leveneTest(EFA_CFA_quest$MSPSS_sum, EFA_CFA_quest$Sample) #sign
leveneTest(EFA_CFA_quest$SIAS_sum, EFA_CFA_quest$Sample) #sign
leveneTest(EFA_CFA_quest$PHQ2_sum, EFA_CFA_quest$Sample)
leveneTest(EFA_CFA_quest$BDIV_sum, EFA_CFA_quest$Sample) #sign

#Factors
leveneTest(EFA_CFA_quest$LISD_State_F1, EFA_CFA_quest$Sample)
leveneTest(EFA_CFA_quest$LISD_State_F2, EFA_CFA_quest$Sample) #sign
leveneTest(EFA_CFA_quest$LISD_Trait_F1, EFA_CFA_quest$Sample) #sign
leveneTest(EFA_CFA_quest$LISD_Trait_F2, EFA_CFA_quest$Sample)
leveneTest(EFA_CFA_quest$LISD_Trait_F3, EFA_CFA_quest$Sample) #sign


##### t-test

#questionnaires
t.test(EFA_CFA_quest$STAIState_sum~EFA_CFA_quest$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_quest$STAITrait_sum~EFA_CFA_quest$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_quest$MSPSS_sum~EFA_CFA_quest$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_quest$SIAS_sum~EFA_CFA_quest$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_quest$PHQ2_sum~EFA_CFA_quest$Sample, var.equal = TRUE)
t.test(EFA_CFA_quest$BDIV_sum~EFA_CFA_quest$Sample, var.equal = FALSE, alternative = "two.sided")

#Factors
t.test(EFA_CFA_quest$LISD_State_F1~EFA_CFA_quest$Sample, var.equal = TRUE)
t.test(EFA_CFA_quest$LISD_State_F2~EFA_CFA_quest$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_quest$LISD_Trait_F1~EFA_CFA_quest$Sample, var.equal = FALSE, alternative = "two.sided")
t.test(EFA_CFA_quest$LISD_Trait_F2~EFA_CFA_quest$Sample, var.equal = TRUE)
t.test(EFA_CFA_quest$LISD_Trait_F3~EFA_CFA_quest$Sample, var.equal = FALSE, alternative = "two.sided")





##### create binomial variables infection
CFA_data$infectionsOwn[CFA_data$infectionsOwn == "Nein / Vermutlich nicht"] <- 2
CFA_data$infectionsOwn[CFA_data$infectionsOwn == "Ja / Vermutlich"] <- 1
CFA_data$infectionsOwn[CFA_data$infectionsOwn == "Keine Angabe"] <- -1
CFA_data$infectionKnownTotal <- ifelse(CFA_data$infectionsPrivate == -1 & 
                                         CFA_data$infectionsWork == -1 & 
                                         CFA_data$infectionsOwn == 2, 0, 1)
CFA_data$infectionKnownTotal[CFA_data$infectionsOwn == 3] <- 0
CFA_data$infectionKnownOthers <- ifelse(CFA_data$infectionsPrivate == -1 & 
                                          CFA_data$infectionsWork == -1, 0, 1)

EFA_data$infectionsPrivate[is.na(EFA_data$infectionsPrivate)] <- -1
EFA_data$infectionsWork[is.na(EFA_data$infectionsWork)] <- -1
EFA_data$infectionKnownTotal <- ifelse(EFA_data$infectionsPrivate == -1 & 
                                         EFA_data$infectionsWork == -1 & 
                                         EFA_data$infectionsOwn == 2, 0, 1)
EFA_data$infectionKnownOthers <- ifelse(EFA_data$infectionsPrivate == -1 & 
                                          EFA_data$infectionsWork == -1, 0, 1)
CFA_data$infectionKnownTotal[CFA_data$infectionsWork == 3] <- 0
CFA_data$infectionKnownOthers[CFA_data$infectionsWork == 3] <- 0


##### create meanscore Safety Behaviour (Compliance)
EFA_data$safetybehav_mean <- with(EFA_data, compliance_01+compliance_02+compliance_03+compliance_05)/4
CFA_data$safetybehav_mean <- with(CFA_data, compliance_01+compliance_02+compliance_03+compliance_05)/4
CFA_data$all_safetybehav_mean <- with(CFA_data, compliance_01+compliance_02+compliance_03+compliance_04+compliance_05+compliance_06+compliance_07)/7


#### correct variables

CFA_data$LISD_State_F1 <- round(CFA_data$LISD_State_F1, digits=2)
CFA_data$LISD_Trait_F2 <- round(CFA_data$LISD_Trait_F2, digits=2)

CFA_data$health[CFA_data$health == "Sehr gut"] <- 1
CFA_data$health[CFA_data$health == "Gut"] <- 2
CFA_data$health[CFA_data$health == "Mittelmaessig"] <- 3
CFA_data$health[CFA_data$health == "Schlecht"] <- 4
CFA_data$health[CFA_data$health == "Sehr schlecht"] <- 5

CFA_data$numVirtContacts[CFA_data$numVirtContacts == "04. Mai"] <- 4.5
CFA_data$numVirtContacts[CFA_data$numVirtContacts == "03. Jun"] <- 4.5

CFA_data$civilStatus[CFA_data$civilStatus == "Single"] <- 1
CFA_data$civilStatus[CFA_data$civilStatus == "In fester Beziehung"] <- 2
CFA_data$civilStatus[CFA_data$civilStatus == "Verheiratet, mit Ehepartner*in zusammenlebend"] <- 3
CFA_data$civilStatus[CFA_data$civilStatus == "Verheiratet, von Ehepartner*in getrennt lebend"] <- 4
CFA_data$civilStatus[CFA_data$civilStatus == "Geschieden"] <- 5
CFA_data$civilStatus[CFA_data$civilStatus == "Verwitwet"] <- 6
CFA_data$civilStatus[CFA_data$civilStatus == "Sonstiger:"] <- 7
CFA_data$civilStatus[CFA_data$civilStatus == "kompliziert"] <- 7

CFA_data$education[CFA_data$education == "Noch vor dem Schulabschluss"] <- 1
CFA_data$education[CFA_data$education == "Schule beendet ohne Abschluss"] <- 2
CFA_data$education[CFA_data$education == "Volks- oder Hauptschule"] <- 3
CFA_data$education[CFA_data$education == "Realschule / Mittlere Reife / Fachschulreife"] <- 4
CFA_data$education[CFA_data$education == "Fachhochschulreife / Abschluss einer Fachoberschulreife"] <- 5
CFA_data$education[CFA_data$education == "Abitur, allgemeine Hochschulreife"] <- 6
CFA_data$education[CFA_data$education == "Anderer Schulabschluss (z.B. im Ausland erworben)"] <- 7



### Frequencies 

#### other variables

Bvariables_EFA <- subset(EFA_data, select=c(health, hobbies, activity, alcohol, substances, pleasantActivities, unpleasantActivities, infectionsPrivate, infectionsWork, infectionsOwn, infectionExtent, infectionRisk, infectionKnownOthers, infectionKnownTotal, education, occupation, occupationStudent, occupationApprenticeship, occupationJob, occupationUnoccupied, workCorona_01, workCorona_02, workCorona_03, workCorona_04, workCorona_05))
Bvariables_CFA <- subset(CFA_data, select=c(health, hobbies, activity, alcohol, substances, pleasantActivities, unpleasantActivities, infectionsPrivate, infectionsWork, infectionsOwn, infectionExtent, infectionRisk, infectionKnownOthers, infectionKnownTotal, education, occupation, occupationStudent, occupationApprenticeship, occupationJob, occupationUnoccupied, workCorona_01, workCorona_02, workCorona_03, workCorona_04, workCorona_05))



#### remove multivariate outliers LISD

#EFA:
Bvariables_EFA <- 
  Bvariables_EFA[!(rownames(Bvariables_EFA) %in% 
                     c("311", "435", "488", "515", "520", "552", "582", "629", "637", "670", "748", "802", "812","822", "888", "1038")),]
#CFA
Bvariables_CFA <- 
  Bvariables_CFA[!(rownames(Bvariables_CFA) %in% 
                     c("94","99","103","128","187","209", "225","281","290","366","386","391","420","455")),]



Bvariables_EFA$Sample = "Study 1"
Bvariables_CFA$Sample = "Study 2"
#add ID
EFA_CFA_Bvariables = rbind(Bvariables_EFA, Bvariables_CFA)
EFA_CFA_Bvariables$ID = rownames(EFA_CFA_Bvariables)
EFA_CFA_Bvariables <- EFA_CFA_Bvariables %>%  select(ID, everything())
EFA_CFA_Bvariables$ID = as.factor(EFA_CFA_Bvariables$ID)
rownames(EFA_CFA_Bvariables) <- NULL  ## reset rownames


# Sample Comparison

attach(EFA_CFA_Bvariables)

#### handle NAs

hobbies[is.na(hobbies)] <- -1
activity[is.na(activity)] <- -1
alcohol[is.na(alcohol)] <- -1
substances[is.na(substances)] <- -1
pleasantActivities[is.na(pleasantActivities)] <- -1
unpleasantActivities[is.na(unpleasantActivities)] <- -1


### Education

legend_6 <- c("1=pre-graduation", "2=no graduation", 
              "3=Volks-/Hauptschule", "4=Realschule/mittlere Reife",
              "5=Fachhochschulreife", "6=Abitur",
              "7=others (e.g.,foreign")

TAB_education = table(education, Sample)
TAB_education
PROP_education<-prop.table(TAB_education)
round(100*PROP_education, digits = 2) # %
chisq.test(TAB_education, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_education, beside = T, col = rainbow(7))
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_6, fill = rainbow(7), bty = "n", ncol = 3, inset = -0.3)
par(opar)



### Health

legend_1 <- c("1 = Very good", "2 = Good", "3 = Medium", "4 = Bad", "5 = Very bad")

TAB_health = table(health, Sample, exclude = "-1")
TAB_health
PROP_health<-prop.table(TAB_health, margin = 2)
round(100*PROP_health, digits = 2) # %
chisq.test(TAB_health, correct=T)



opar = par(oma = c(2,0,0,0))
barplot(TAB_health, beside = T, col = cm.colors(5))
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_1, fill = cm.colors(5), bty = "n", ncol = 3, inset = -0.15)
par(opar)



### Activities


legend_2 <- c("1 = No", "2 = Daily", "3 = Several times/week", "4 = Once/week", "5 = Once/month", "6 = Less than once/month")


#### Hobbies


TAB_hobbies = table(hobbies, Sample, exclude = c("-1","-9"))
TAB_hobbies
PROP_hobbies<-prop.table(TAB_hobbies, margin = 2)
round(100*PROP_hobbies, digits = 4)
chisq.test(TAB_hobbies, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_hobbies, beside = T, col = terrain.colors(6))
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_2, fill = terrain.colors(6), bty = "n", ncol = 3, inset = -0.15)
par(opar)


#### Physical Activity 

TAB_activity = table(activity, Sample, exclude = c("-1","-9"))
TAB_activity
PROP_activity<-prop.table(TAB_activity, margin = 2)
round(100*PROP_activity, digits = 2) # %
chisq.test(TAB_activity, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_activity, beside = T, col = terrain.colors(6))
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_2, fill = terrain.colors(6), bty = "n", ncol = 3, inset = -0.15)
par(opar)

#### Alcohol consumption


TAB_alcohol = table(alcohol, Sample, exclude = c("-1","-9"))
TAB_alcohol
PROP_alcohol<-prop.table(TAB_alcohol, margin = 2)
round(100*PROP_alcohol, digits = 2) # %
chisq.test(TAB_alcohol, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_alcohol, beside = T, col = terrain.colors(6))
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_2, fill = terrain.colors(6), bty = "n", ncol = 3, inset = -0.15)
par(opar)


#### Substance consumption


legend_3 <- c("1 = No", "3 = Yes, multiple times a week", "4 = Yes, once/week", "5 = Yes, once/month", "6 = Yes, less than once/month")
TAB_substances = table(substances, Sample, exclude = c("-1","-9"))
TAB_substances
PROP_substances<-prop.table(TAB_substances, margin = 2)
round(100*PROP_substances, digits = 2) # %
chisq.test(TAB_substances, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_substances, beside = T, col = terrain.colors(5))
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_3, fill = terrain.colors(5), bty = "n", ncol = 3, inset = -0.15)
par(opar)


#### Pleasant Activities


TAB_pleasantActivities = table(pleasantActivities, Sample, exclude = c("-1","-9"))
TAB_pleasantActivities
PROP_pleasantActivities<-prop.table(TAB_pleasantActivities, margin = 2)
round(100*PROP_pleasantActivities, digits = 2) # %
chisq.test(TAB_pleasantActivities, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_pleasantActivities, beside = T, col = terrain.colors(6))
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_2, fill = terrain.colors(6), bty = "n", ncol = 3, inset = -0.15)
par(opar)


#### Unpleasant Activities


TAB_unpleasantActivities = table(unpleasantActivities, Sample, exclude = c("-1","-9"))
TAB_unpleasantActivities
PROP_unpleasantActivities<-prop.table(TAB_unpleasantActivities, margin = 2)
round(100*PROP_unpleasantActivities, digits = 2) # %
chisq.test(TAB_unpleasantActivities, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_unpleasantActivities, beside = T, col = terrain.colors(6))
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_2, fill = terrain.colors(6), bty = "n", ncol = 3, inset = -0.15)
par(opar)



### Infections

legend_4 <- c("NA", "1 = mild course of disease", "2 = severe course of disease", "3 = Death")
mycols = c("tan", "yellow1", "orange1" , "red")


#### Private

TAB_infectionsPrivate = table(infectionsPrivate, Sample)
TAB_infectionsPrivate
PROP_infectionsPrivate<-prop.table(TAB_infectionsPrivate, margin = 2)
round(100*PROP_infectionsPrivate, digits = 2) # %
chisq.test(TAB_infectionsPrivate, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_infectionsPrivate, beside = T, col = mycols)
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_4, fill = mycols, bty = "n", ncol = 4, inset = -0.15)
par(opar)


#### Work / other Students

TAB_infectionsWork = table(infectionsWork, Sample)
TAB_infectionsWork
PROP_infectionsWork<-prop.table(TAB_infectionsWork, margin = 2)
round(100*PROP_infectionsWork, digits = 2) # %
chisq.test(TAB_infectionsWork, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_infectionsWork, beside = T, col = mycols)
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_4, fill = mycols, bty = "n", ncol = 4, inset = -0.15)
par(opar)


#### Self

legend_5 <- c("1 = Yes / Probably", "2 = No / Probably not")
mycols2 <- c("red", "lightgreen")

TAB_infectionsOwn = table(infectionsOwn, Sample, exclude = -9)
TAB_infectionsOwn
PROP_infectionsOwn<-prop.table(TAB_infectionsOwn, margin = 2)
round(100*PROP_infectionsOwn, digits = 2) # %
chisq.test(TAB_infectionsOwn, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_infectionsOwn, beside = T, col = mycols2)
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_5, fill = mycols2, bty = "n", ncol = 3, inset = -0.15)
par(opar)


#### Infections experienced
#at all?

legend_7 <- c("0 = No", "1 = Yes")
mycols3 <- c("lightgreen", "red")

TAB_infectionKnownTotal = table(infectionKnownTotal, Sample)
TAB_infectionKnownTotal
PROP_infectionKnownTotal<-prop.table(TAB_infectionKnownTotal, margin = 2)
round(100*PROP_infectionKnownTotal, digits = 2) # %
chisq.test(TAB_infectionKnownTotal, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_infectionKnownTotal, beside = T, col = mycols3)
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_7, fill = mycols3, bty = "n", ncol = 3, inset = -0.15)
par(opar)


TAB_infectionKnownOthers = table(infectionKnownOthers, Sample)
TAB_infectionKnownOthers
PROP_infectionKnownOthers<-prop.table(TAB_infectionKnownOthers, margin = 2)
round(100*PROP_infectionKnownOthers, digits = 2) # %
chisq.test(TAB_infectionKnownOthers, correct=T)


opar = par(oma = c(2,0,0,0))
barplot(TAB_infectionKnownOthers, beside = T, col = mycols3)
par(opar)
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x = "bottom", legend = legend_7, fill = mycols3, bty = "n", ncol = 3, inset = -0.15)
par(opar)

detach(EFA_CFA_Bvariables)


# Regression Analysis EFA

EFA_data$z_LISD_State_F1 <-scale(EFA_data$LISD_State_F1)
EFA_data$z_LISD_State_F2 <-scale(EFA_data$LISD_State_F2)
EFA_data$LISD_State_F2_inv <- 6 - EFA_data$LISD_State_F2
EFA_data$z_LISD_State_F2_inv <- scale(EFA_data$LISD_State_F2_inv)
EFA_data$z_LISD_Trait_F1<-scale(EFA_data$LISD_Trait_F1)
EFA_data$z_LISD_Trait_F2<-scale(EFA_data$LISD_Trait_F2)
EFA_data$z_LISD_Trait_F3<-scale(EFA_data$LISD_Trait_F3)

EFA_data$z_STAIState_sum <-scale(EFA_data$STAIState_sum)
EFA_data$z_PHQ2_sum <-scale(EFA_data$PHQ2_sum)
EFA_data$z_BDIV_sum <-scale(EFA_data$BDIV_sum)
EFA_data$z_age <-scale(EFA_data$age)

EFA_data$Dfemale <- recode(EFA_data$gender, "2=1; 1=0")
# complianceCurrent:  No, not anymore/never = 0, Yes / Yes, partly = 1,
# 'Ja'=1; 'Ja, teilweise'=2; 'Nein, nicht mehr'=3; 'Nein, auch zuvor nicht'=4"
EFA_data$DcomplianceYES <- recode(EFA_data$complianceCurrent, "1=1; 2=1; 3=0; 4=0")

### Create Regression Sample "EFA_data_Reg"

vars = c("z_LISD_State_F1", "z_LISD_State_F2", "z_LISD_Trait_F1", "z_LISD_Trait_F2", "z_LISD_Trait_F3",
         "z_STAIState_sum" ,"z_PHQ2_sum", "z_BDIV_sum", "z_age","Dfemale","DcomplianceYES",
         "LISD_State_F1", "LISD_State_F2", "LISD_Trait_F1", "LISD_Trait_F2", "LISD_Trait_F3")
EFA_data_Reg = EFA_data[vars]
EFA_data_Reg <- EFA_data_Reg[complete.cases(EFA_data_Reg),]

#### prepare Target Variables

# State Anxiety
EFA_data_Reg$Anxiousness <- EFA_data_Reg$z_STAIState_sum

# only Depression
EFA_data_Reg$Depressed <- with(EFA_data_Reg, z_PHQ2_sum + z_BDIV_sum)/2



#### Regressions

##### Anxiousness
Anx_main = lm(Anxiousness ~ z_LISD_State_F1+z_LISD_State_F2+z_LISD_Trait_F1+z_LISD_Trait_F2+z_LISD_Trait_F3, EFA_data_Reg)
summary(Anx_main)
etasq(Anx_main)

Anx_main_2 = lm(Anxiousness ~z_LISD_State_F1+z_LISD_State_F2+z_LISD_Trait_F1+z_LISD_Trait_F2+z_LISD_Trait_F3+z_age+Dfemale+DcomplianceYES, EFA_data_Reg)
summary(Anx_main_2)
etasq(Anx_main_2)

#two-way interactions with group
Anx_two = lm(Anxiousness ~ z_age*z_LISD_State_F1+z_age*z_LISD_State_F2+z_age*z_LISD_Trait_F1+z_age*z_LISD_Trait_F2+z_age*z_LISD_Trait_F3
             +Dfemale*z_LISD_State_F1+Dfemale*z_LISD_State_F2+Dfemale*z_LISD_Trait_F1+Dfemale*z_LISD_Trait_F2+Dfemale*z_LISD_Trait_F3
             +DcomplianceYES*z_LISD_State_F1+DcomplianceYES*z_LISD_State_F2+DcomplianceYES*z_LISD_Trait_F1+
               DcomplianceYES*z_LISD_Trait_F2+DcomplianceYES*z_LISD_Trait_F3, EFA_data_Reg)
summary(Anx_two)
etasq(Anx_two)

### compare regression models
anova(Anx_main, Anx_main_2)
anova(Anx_main, Anx_two)
anova(Anx_main_2, Anx_two)

car::vif(Anx_main)
car::vif(Anx_main_2)
car::vif(Anx_two)

##### Depressed
Depressed_main = lm(Depressed ~ z_LISD_State_F1+z_LISD_State_F2+z_LISD_Trait_F1+z_LISD_Trait_F2+z_LISD_Trait_F3, EFA_data_Reg)
summary(Depressed_main)

Depressed_main_2 = lm(Depressed ~z_LISD_State_F1+z_LISD_State_F2+z_LISD_Trait_F1+z_LISD_Trait_F2+z_LISD_Trait_F3+z_age+Dfemale+DcomplianceYES, EFA_data_Reg)
summary(Depressed_main_2)
etasq(Depressed_main_2)

#two-way interactions with group
Depressed_two = lm(Depressed ~ z_age*z_LISD_State_F1+z_age*z_LISD_State_F2+z_age*z_LISD_Trait_F1+z_age*z_LISD_Trait_F2+z_age*z_LISD_Trait_F3
                   +Dfemale*z_LISD_State_F1+Dfemale*z_LISD_State_F2+Dfemale*z_LISD_Trait_F1+Dfemale*z_LISD_Trait_F2+Dfemale*z_LISD_Trait_F3
                   +DcomplianceYES*z_LISD_State_F1+DcomplianceYES*z_LISD_State_F2+DcomplianceYES*z_LISD_Trait_F1+
                     DcomplianceYES*z_LISD_Trait_F2+DcomplianceYES*z_LISD_Trait_F3, EFA_data_Reg)
summary(Depressed_two)
etasq(Depressed_two)

### compare regression models
anova(Depressed_main, Depressed_main_2)
anova(Depressed_main, Depressed_two)
anova(Depressed_main_2, Depressed_two)

car::vif(Depressed_main)
car::vif(Depressed_main_2)
car::vif(Depressed_two)



remove(activity, alcohol,hobbies,legend_1,legend_2,legend_3,legend_4,legend_5,legend_6,legend_7,mycols,mycols2,mycols3,pleasantActivities,PROP_activity,PROP_alcohol,PROP_education,PROP_health,PROP_hobbies,PROP_infectionKnownOthers,PROP_infectionKnownTotal,PROP_infectionsOwn,PROP_infectionsPrivate,PROP_infectionsWork,PROP_pleasantActivities,PROP_substances,PROP_unpleasantActivities,substances,TAB_activity,TAB_alcohol,TAB_education,TAB_health,TAB_hobbies,TAB_infectionKnownOthers,TAB_infectionKnownTotal,TAB_infectionsOwn,TAB_infectionsPrivate,TAB_infectionsWork,TAB_pleasantActivities,TAB_substances,TAB_unpleasantActivities,unpleasantActivities)
remove(descr_all_questionnaires_CFA,descr_all_questionnaires_EFA,descr_sociodemographics_CFA,descr_sociodemographics_EFA)
remove(LISD_efa,LISD_efa_State,LISD_efa_Trait,noout_efa,noSkew_efa,noSkew_efa_State,noSkew_efa_Trait)
remove(opar,vars, questionnaires_CFA,questionnaires_EFA,sociodemographics_CFA,sociodemographics_EFA)
