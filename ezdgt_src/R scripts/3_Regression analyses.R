
#### Load packages
library(ggplot2)
library(lavaan)
library(dplyr)
library(car)
library(ggpubr)
library(gridExtra)
library(heplots)


#### Import data

load("CFA_data_final.Rda")


# MULTIPLE REGRESSION (Sample 2)

# Data Preparation

CFA_data$safetybehav_mean <- with(CFA_data, compliance_01+compliance_02+compliance_03+compliance_05)/4
CFA_data$all_safetybehav_mean <- with(CFA_data, compliance_01+compliance_02+compliance_03+compliance_04+compliance_05+compliance_06+compliance_07)/7
Sample2 <- CFA_data[,c("gender","age","civilStatus","livingCondition","garden","numHousehold","health","hobbies", "activity","alcohol","substances","pleasantActivities","unpleasantActivities","dailystructure",
                       "mentalHealth","mentalHealthLastYear","infectionsPrivate","infectionsWork","infectionsElse","infectionsOwn","infectionExtent","infectionRisk","education","occupationStudent","occupationApprenticeship",
                       "occupationJob","occupationUnoccupied","occuptionHomemaker","occupationPension","workCorona_01","workCorona_02","workCorona_03","workCorona_04","workCorona_05","numContacts","numVirtContacts","contactsDirect",
                       "contactsVirtual","VirtualCommTime","complianceCurrent", "compliance_01" ,"compliance_02", "compliance_03","compliance_04","compliance_05","compliance_06", "compliance_07","compliance_Duration","compliance_End",
                       "STAIState_sum","STAITrait_sum","MSPSS_sum", "SIAS_sum","PHQ2_sum","BDIV_sum","ASI3_physical","NEO_N", "NEO_E","NEO_O", "NEO_A","NEO_C",
                       "LISD_State_F1","LISD_State_F2","LISD_Trait_F1", "LISD_Trait_F2","LISD_Trait_F3","safetybehav_mean") ]


#### Data Cleaning and Preparation

Sample2$LISD_State_F1 <- round(Sample2$LISD_State_F1, digits=2)
Sample2$LISD_State_F2 <- round(Sample2$LISD_State_F2, digits=2)
Sample2$LISD_Trait_F2 <- round(Sample2$LISD_Trait_F2, digits=2)
Sample2$LISD_State_F2_inv <- 6 - Sample2$LISD_State_F2
Sample2$safetybehav_mean <- round(Sample2$safetybehav_mean, digits=2)

Sample2$poorHealth[Sample2$health == 5] <- 1
Sample2$poorHealth[Sample2$health == 4] <- 2
Sample2$poorHealth[Sample2$health == 3] <- 3
Sample2$poorHealth[Sample2$health == 2] <- 4
Sample2$poorHealth[Sample2$health == 1] <- 5


#### 1 = daily, 2 = multiple times/week, 3 = 1/week, 4 = less than 1/week
Sample2$infreqPhysActivity[Sample2$activity == 1] <- 4
Sample2$infreqPhysActivity[Sample2$activity == 6] <- 4
Sample2$infreqPhysActivity[Sample2$activity == 5] <- 4
Sample2$infreqPhysActivity[Sample2$activity == 4] <- 3
Sample2$infreqPhysActivity[Sample2$activity == 3] <- 2
Sample2$infreqPhysActivity[Sample2$activity == 2] <- 1

Sample2$infreqHobbies[Sample2$hobbies == 1] <- 4
Sample2$infreqHobbies[Sample2$hobbies == 6] <- 4
Sample2$infreqHobbies[Sample2$hobbies == 5] <- 4
Sample2$infreqHobbies[Sample2$hobbies == 4] <- 3
Sample2$infreqHobbies[Sample2$hobbies == 3] <- 2
Sample2$infreqHobbies[Sample2$hobbies == 2] <- 1

Sample2$infreqDailyStructure[Sample2$dailystructure == 1] <- 4
Sample2$infreqDailyStructure[Sample2$dailystructure == 6] <- 4
Sample2$infreqDailyStructure[Sample2$dailystructure == 5] <- 4
Sample2$infreqDailyStructure[Sample2$dailystructure == 4] <- 3
Sample2$infreqDailyStructure[Sample2$dailystructure == 3] <- 2
Sample2$infreqDailyStructure[Sample2$dailystructure == 2] <- 1

Sample2$infreqPleasantActiv[Sample2$pleasantActivities == 1] <- 4
Sample2$infreqPleasantActiv[Sample2$pleasantActivities == 6] <- 4
Sample2$infreqPleasantActiv[Sample2$pleasantActivities == 5] <- 4
Sample2$infreqPleasantActiv[Sample2$pleasantActivities == 4] <- 3
Sample2$infreqPleasantActiv[Sample2$pleasantActivities == 3] <- 2
Sample2$infreqPleasantActiv[Sample2$pleasantActivities == 2] <- 1


#### 4 = daily, 3 = multiple times/week, 2 = 1/week, 1 = less than 1/week
Sample2$freqUnpleasantActiv[Sample2$unpleasantActivities == 1] <- 1
Sample2$freqUnpleasantActiv[Sample2$unpleasantActivities == 6] <- 1
Sample2$freqUnpleasantActiv[Sample2$unpleasantActivities == 5] <- 1
Sample2$freqUnpleasantActiv[Sample2$unpleasantActivities == 4] <- 2
Sample2$freqUnpleasantActiv[Sample2$unpleasantActivities == 3] <- 3
Sample2$freqUnpleasantActiv[Sample2$unpleasantActivities == 2] <- 4

Sample2$freqAlcohol[Sample2$alcohol == 1] <- 1
Sample2$freqAlcohol[Sample2$alcohol == 6] <- 1
Sample2$freqAlcohol[Sample2$alcohol == 5] <- 1
Sample2$freqAlcohol[Sample2$alcohol == 4] <- 2
Sample2$freqAlcohol[Sample2$alcohol == 3] <- 3
Sample2$freqAlcohol[Sample2$alcohol == 2] <- 4



Sample2 <- within(Sample2, {
  health <- NULL
  hobbies <- NULL
  activity <- NULL 
  pleasantActivities <- NULL
  unpleasantActivities <- NULL
  alcohol <- NULL 
  substances <- NULL
})


#### create dummy variables


# gender
Sample2$Dfemale <- recode(Sample2$gender, "'weiblich'=1; 'maennlich'=0; 'divers'=0; 'keine Angabe'=0")
Sample2$Dmale <- recode(Sample2$gender, "'maennlich'=1; 'weiblich'=0; 'divers'=0; 'keine Angabe'=0")
Sample2$Ddiverse <- recode(Sample2$gender, "'divers'=1; 'weiblich'=0; 'maennlich'=0; 'keine Angabe'=0")
Sample2$DNoGenderInd <- recode(Sample2$gender, "'keine Angabe'=1;'weiblich'=0; 'maennlich'=0; 'divers'=0")

Sample2$DComplianceFully <- recode(Sample2$complianceCurrent, "'Ja'=1; 'Ja, teilweise'=0; 'Nein auch zuvor nicht'=0; 'Nein, nicht mehr'=0")
Sample2$DCompliancePartly <- recode(Sample2$complianceCurrent, "'Ja'=0; 'Ja, teilweise'=1; 'Nein auch zuvor nicht'=0; 'Nein, nicht mehr'=0")
Sample2$DComplianceNever <- recode(Sample2$complianceCurrent, "'Ja'=0; 'Ja, teilweise'=0; 'Nein auch zuvor nicht'=1; 'Nein, nicht mehr'=0")
Sample2$DComplianceNotAnymore <- recode(Sample2$complianceCurrent, "'Ja'=0; 'Ja, teilweise'=0; 'Nein auch zuvor nicht'=0; 'Nein, nicht mehr'=1")



#### create binary variables


##### basics for further exploration (if interested)


# Age: 0 < 40 yo, 1 >= 40 yo
Sample2$age_above40 <- ifelse(Sample2$age < 40, 0, 1)

# civilStatus: 0 = not in relationship, 1 = in relationship
Sample2$DrelationshipYES <- recode(Sample2$civilStatus, "2:3=1; 1=0; 4:7=0")

# living 
Sample2$DNoOutside <- recode(Sample2$garden, "'Keines'=1; 'Balkon/Terrasse ohne Garten'=0; 'Garten'=0")
Sample2$DSmallFlat <- recode(Sample2$livingCondition, "'1-Zimmer-Wohnung'=1; 'Mehrzimmer-Wohnung'=0; 'Mehrzimmer-wohnung'=0; 'Haus'=0" )

##### health
Sample2$DmentalDisEver <- recode(Sample2$mentalHealth, "'Ja'=1; 'Nein'=0; 'Weiss nicht'=0; 'Keine Angabe'=0")
Sample2$DmentalDisLastYear <- recode(Sample2$mentalHealthLastYear, "'Ja'=1; 'Nein'=0; 'Weiss nicht'=0; 'Keine Angabe'=0; '[NA] nicht beantwortet'=0")
Sample2$DRiskSelfKnown <- recode(Sample2$infectionRisk,  "'Ja'=1; 'Nein'=0; 'Weiss nicht'=0; 'Keine Angabe'=0")
Sample2$DSevereInfectionSelf <- recode(Sample2$infectionExtent,  "'Schwerer Verlauf'=1; 'Leichter Verlauf'=0; '[NA] nicht beantwortet'=0; 'Keine Angabe'=0")


# complianceCurrent:  No, not anymore/never = 0, Yes / Yes, partly = 1,
Sample2$DcomplianceYES <- recode(Sample2$complianceCurrent, "'Ja'=1; 'Ja, teilweise'=1; 'Nein, nicht mehr'=0; 'Nein, auch zuvor nicht'=0")



##### education / occupation
# education: 0 = low/middle/unfinished, Yes = high
Sample2$Dhigheducation <- recode(Sample2$education, "5:7=1;1:4=0")

Sample2$DStudent <- recode(Sample2$occupationStudent, "'TRUE'=1; 'FALSE'=0")
Sample2$DJob <- recode(Sample2$occupationJob, "'TRUE'=1; 'FALSE'=0")
Sample2$DUnoccupied <- recode(Sample2$occupationUnoccupied, "'TRUE'=1; 'FALSE'=0")



##### Social Contact
Sample2$DlessContactsDirect <- recode(Sample2$contactsDirect, "'Ich treffe weniger Menschen als zuvor'=1; 'Nein, nicht veraendert'=0; 'Ich treffe mehr Menschen als zuvor'=0")
Sample2$DlessContactsVirtual <- recode(Sample2$contactsVirtual, "'Ich habe mit weniger Menschen virtuellen Kontakt als zuvor'=1; 'Nein, nicht veraendert'=0; 'Ich habe mit mehr Menschen virtuellen Kontakt als zuvor'=0")
Sample2$DLowVirtualCommTime <- recode(Sample2$VirtualCommTime, "'0 - 60 Min.'=1; '1 - 2 Std.'=0; '2 - 4 Std.'=0; '> 4 Std.'=0")


#### z-standardization

Sample2$age <- as.numeric(Sample2$age)
Sample2$numHousehold <- as.numeric(Sample2$numHousehold)
Sample2$numContacts <- as.numeric(Sample2$numContacts)
Sample2$numVirtContacts <- as.numeric(Sample2$numVirtContacts)
Sample2$z_age <- scale(Sample2$age)
Sample2$z_numHousehold <- scale(Sample2$numHousehold)
Sample2$z_numContacts <- scale(Sample2$numContacts)
Sample2$z_numVirtContacts <- scale(Sample2$numVirtContacts)
Sample2$poorHealth <- as.numeric(Sample2$poorHealth)
Sample2$z_poorHealth <- scale(Sample2$poorHealth)
Sample2$z_infreqHobbies <- scale(Sample2$infreqHobbies)
Sample2$z_infreqPhysActivity <- scale(Sample2$infreqPhysActivity)
Sample2$z_infreqDailyStructure <- scale(Sample2$infreqDailyStructure)
Sample2$z_infreqPleasantActiv <- scale(Sample2$infreqPleasantActiv)

Sample2$z_freqUnpleasantActivities <- scale(Sample2$freqUnpleasantActiv)
Sample2$z_freqAlcohol <- scale(Sample2$freqAlcohol)

Sample2$z_work01_jobendangered <- scale(Sample2$workCorona_01)
Sample2$z_work02_jobreduced <- scale(Sample2$workCorona_02)
Sample2$z_work03_homeoffice <- scale(Sample2$workCorona_03)
Sample2$z_work04_morework <- scale(Sample2$workCorona_04)
Sample2$z_work05_financesworse <- scale(Sample2$workCorona_05)

Sample2$z_c01_stayhome <- scale(Sample2$compliance_01)
Sample2$z_c02_avoidPhysContact <- scale(Sample2$compliance_02)
Sample2$z_c03_avoidTouchObjects <- scale(Sample2$compliance_03)
Sample2$z_c04_avoidSocialInside <- scale(Sample2$compliance_04)
Sample2$z_c05_safetydistance <- scale(Sample2$compliance_05)
Sample2$z_c06_avoidGroups <- scale(Sample2$compliance_06)
Sample2$z_c07_avoidMltplHouseholds <- scale(Sample2$compliance_07)
Sample2$z_safetybehav_mean <- scale(Sample2$safetybehav_mean)

Sample2$z_STAIState_sum <-scale(Sample2$STAIState_sum)
Sample2$z_STAITrait_sum <-scale(Sample2$STAITrait_sum)
Sample2$z_MSPSS_sum <-scale(Sample2$MSPSS_sum)
Sample2$z_SIAS_sum <-scale(Sample2$SIAS_sum)
Sample2$z_PHQ2_sum <-scale(Sample2$PHQ2_sum)
Sample2$z_BDIV_sum <-scale(Sample2$BDIV_sum)
Sample2$z_NEO_N <-scale(Sample2$NEO_N)
Sample2$z_NEO_E <-scale(Sample2$NEO_E)
Sample2$z_NEO_O <-scale(Sample2$NEO_O)
Sample2$z_NEO_A <-scale(Sample2$NEO_A)
Sample2$z_NEO_C <-scale(Sample2$NEO_C)
Sample2$z_LISD_State_F1 <-scale(Sample2$LISD_State_F1)
Sample2$z_LISD_State_F2 <-scale(Sample2$LISD_State_F2)
Sample2$z_LISD_State_F2_inv <- scale(Sample2$LISD_State_F2_inv)
Sample2$z_LISD_Trait_F1<-scale(Sample2$LISD_Trait_F1)
Sample2$z_LISD_Trait_F2<-scale(Sample2$LISD_Trait_F2)
Sample2$z_LISD_Trait_F3<-scale(Sample2$LISD_Trait_F3)




### Create Regression Sample "Sample2_Reg"

vars = c("z_LISD_State_F1", "z_LISD_State_F2", "z_LISD_Trait_F1", "z_LISD_Trait_F2", "z_LISD_Trait_F3",
         "z_STAIState_sum" ,"z_PHQ2_sum", "z_BDIV_sum",
         "z_age","Dfemale","DcomplianceYES","z_safetybehav_mean",
         "LISD_State_F1", "LISD_State_F2", "LISD_Trait_F1", "LISD_Trait_F2", "LISD_Trait_F3")
Sample2_Reg = Sample2[vars]

#save(Sample2_Reg, file = "Sample2_Reg.RDa")


#### prepare Target Variables


# State Anxiety
Sample2_Reg$Anxiety <- Sample2_Reg$z_STAIState_sum

# only Depression
Sample2_Reg$Depressed <- with(Sample2_Reg, z_PHQ2_sum + z_BDIV_sum)/2



## Regressions

##### Anxiety

Anx_main = lm(Anxiety ~ z_LISD_State_F1+z_LISD_State_F2+z_LISD_Trait_F1+z_LISD_Trait_F2+z_LISD_Trait_F3, Sample2_Reg)
summary(Anx_main)
etasq(Anx_main)

Anx_main_2 = lm(Anxiety ~z_LISD_State_F1+z_LISD_State_F2+z_LISD_Trait_F1+z_LISD_Trait_F2+z_LISD_Trait_F3+z_age+Dfemale+DcomplianceYES, Sample2_Reg)
summary(Anx_main_2)
etasq(Anx_main_2)

#two-way interactions with group
Anx_two = lm(Anxiety ~ z_age*z_LISD_State_F1+z_age*z_LISD_State_F2+z_age*z_LISD_Trait_F1+z_age*z_LISD_Trait_F2+z_age*z_LISD_Trait_F3
             +Dfemale*z_LISD_State_F1+Dfemale*z_LISD_State_F2+Dfemale*z_LISD_Trait_F1+Dfemale*z_LISD_Trait_F2+Dfemale*z_LISD_Trait_F3
             +DcomplianceYES*z_LISD_State_F1+DcomplianceYES*z_LISD_State_F2+DcomplianceYES*z_LISD_Trait_F1+
               DcomplianceYES*z_LISD_Trait_F2+DcomplianceYES*z_LISD_Trait_F3, Sample2_Reg)
summary(Anx_two)
etasq(Anx_two)

### compare regression models
anova(Anx_main, Anx_main_2)
anova(Anx_main, Anx_two)
anova(Anx_main_2, Anx_two)

car::vif(Anx_main)
car::vif(Anx_main_2)
car::vif(Anx_two)


### Plots Anxiety

##### main effects

#LISD
ggplot(Sample2_Reg,aes(y=Anxiety,x=z_LISD_State_F1))+
  geom_point(color = "indianred4")+geom_smooth(method="lm", color = "black", size = 0.5)+theme_classic()
ggplot(Sample2_Reg,aes(y=Anxiety,x=z_LISD_Trait_F1))+
  geom_point(color = "indianred4")+geom_smooth(method="lm", color = "black", size = 0.5)+theme_classic()
ggplot(Sample2_Reg,aes(y=Anxiety,x=z_LISD_Trait_F2))+
  geom_point(color = "indianred4")+geom_smooth(method="lm", color = "black", size = 0.5)+theme_classic()

#others
ggplot(Sample2_Reg,aes(y=Anxiety,x=Dfemale))+geom_point(color = "plum")+
  geom_smooth(method="lm", color = "black", size = 0.5)+theme_classic()
ggplot(Sample2_Reg,aes(y=Anxiety,x=DcomplianceYES))+
  geom_point(color = "gold")+geom_smooth(method="lm", color = "black", size = 0.5)+theme_classic()


### interaction effects

#### interaction z_LISD_State_F1 * compliance
Sample2_Reg %>% 
  ggplot() +
  aes(x = z_LISD_State_F1, y = Anxiety, group = DcomplianceYES, color = DcomplianceYES) +
  scale_colour_gradient(low = "firebrick2", high = "forestgreen")+
  geom_point(color = "black", alpha = .7) +
  geom_smooth(method = "lm")+
  theme_classic()

#### (marginal) interaction z_LISD_Trait_F1 * compliance
Sample2_Reg %>% 
  ggplot() +
  aes(x = z_LISD_Trait_F1, y = Anxiety, group = DcomplianceYES, color = DcomplianceYES) +
  #scale_colour_continuous(type = "viridis")+
  scale_colour_gradient(low = "firebrick2", high = "forestgreen")+
  geom_point(color = "black", alpha = .7) +
  geom_smooth(method = "lm")+
  theme_classic()




##### Depressed

Depressed_main = lm(Depressed ~ z_LISD_State_F1+z_LISD_State_F2+z_LISD_Trait_F1+z_LISD_Trait_F2+z_LISD_Trait_F3, Sample2_Reg)
summary(Depressed_main)
etasq(Depressed_main)

Depressed_main_2 = lm(Depressed ~z_LISD_State_F1+z_LISD_State_F2+z_LISD_Trait_F1+z_LISD_Trait_F2+z_LISD_Trait_F3+z_age+Dfemale+DcomplianceYES, Sample2_Reg)
summary(Depressed_main_2)
etasq(Depressed_main_2)


#two-way interactions with group
Depressed_two = lm(Depressed ~ z_age*z_LISD_State_F1+z_age*z_LISD_State_F2+z_age*z_LISD_Trait_F1+z_age*z_LISD_Trait_F2+z_age*z_LISD_Trait_F3
                   +Dfemale*z_LISD_State_F1+Dfemale*z_LISD_State_F2+Dfemale*z_LISD_Trait_F1+Dfemale*z_LISD_Trait_F2+Dfemale*z_LISD_Trait_F3
                   +DcomplianceYES*z_LISD_State_F1+DcomplianceYES*z_LISD_State_F2+DcomplianceYES*z_LISD_Trait_F1+
                     DcomplianceYES*z_LISD_Trait_F2+DcomplianceYES*z_LISD_Trait_F3, Sample2_Reg)
summary(Depressed_two)
etasq(Depressed_twp)


### compare regression models
anova(Depressed_main, Depressed_main_2)
anova(Depressed_main, Depressed_two)
anova(Depressed_main_2, Depressed_two)


car::vif(Depressed_main)
car::vif(Depressed_main_2)
car::vif(Depressed_two)


### Plots Depressed

##### plot preparation: divide z_age into -SD, mean, +SD
attach(Sample2_Reg)

Sample2_Reg$age_3groups <- 
  case_when(z_age > mean(z_age)+sd(z_age) ~ "high",
            z_age < mean(z_age)+sd(z_age) & z_age > mean(z_age)-sd(z_age) ~ "mean",
            z_age < mean(z_age)-sd(z_age) ~ "low")
detach(Sample2_Reg)

##### main effects
ggplot(Sample2_Reg,aes(y=Depressed,x=z_LISD_State_F1))+
  geom_point(color = "slategray3")+geom_smooth(method="lm", color = "black", size = 0.5)+theme_classic()
ggplot(Sample2_Reg,aes(y=Depressed,x=z_LISD_State_F2))+
  geom_point(color = "slategray3")+geom_smooth(method="lm", color = "black", size = 0.5)+theme_classic()
ggplot(Sample2_Reg,aes(y=Depressed,x=z_LISD_Trait_F2))+
  geom_point(color = "slategray3")+geom_smooth(method="lm", color = "black", size = 0.5)+theme_classic()

#### interaction z_age * z_LISD_Trait_F2
Sample2_Reg %>% 
  ggplot() +
  aes(x = z_LISD_Trait_F2, y = Depressed, group = age_3groups, color = age_3groups) +
  geom_point(color = "black", alpha = .7) +
  scale_colour_manual(values = c("tomato3", "yellowgreen", "gold")) +
  geom_smooth(method = "lm")





## Plots Manuscript

### Plots Manuscript with raw W-LISD scores 

### Anxiety

plot1_1 <- Sample2_Reg %>%
  ggplot(aes(x = LISD_State_F1,
             y = Anxiety)) +
  geom_point(color = "#00AFBB")+
  geom_smooth(method = lm, color = "black",
              size = 0.5)+
  xlim(1,5)+
  ylim(-2,3)+
  labs(x = "lonely & isolated (State 1)",
       y = "Anxiety (z)", 
       Title = "State Factor 1")+
  theme_classic()

plot2_1 <- Sample2_Reg %>%
  ggplot(aes(x = LISD_State_F2,
             y = Anxiety)) +
  geom_point(color = "#55C667FF")+
  geom_smooth(method = lm, color = "black",
              size = 0.5)+
  xlim(1,5)+
  ylim(-2,3)+
  labs(x = "supported & connected (State 2)",
       y = "Anxiety (z)", 
       Title = "State Factor 2")+
  theme_classic()



plot3_1 <- Sample2_Reg %>%
  ggplot(aes(x = LISD_Trait_F1,
             y = Anxiety)) +
  geom_point(color = "#E7B800")+
  geom_smooth(method = lm, color = "black",
              size = 0.5)+
  xlim(1,5)+
  ylim(-2,3)+
  labs(x = "loneliness & isolation (Trait 1)",
       y = "Anxiety (z)", 
       Title = "Trait Factor 1")+
  theme_classic()



plot4_1 <- Sample2_Reg %>%
  ggplot(aes(x = LISD_Trait_F2,
             y = Anxiety)) +
  geom_point(color = "#FC4E07")+
  geom_smooth(method = lm, color = "black",
              size = 0.5)+
  xlim(1,5)+
  ylim(-2,3)+
  labs(x = "sociability & sense of belonging (Trait 2)",
       y = "Anxiety (z)", 
       Title = "Trait Factor 1")+
  theme_classic()


plot1_1 
plot2_1
plot3_1
plot4_1

### Depression

plot1_2 <- Sample2_Reg %>%
  ggplot(aes(x = LISD_State_F1,
             y = Depressed)) +
  geom_point(color = "#00AFBB")+
  geom_smooth(method = lm, color = "black",
              size = 0.5)+
  xlim(1,5)+
  ylim(-2,3)+
  labs(x = "lonely & isolated (State 1)",
       y = "Depression (z)", 
       Title = "State Factor 1")+
  theme_classic()


plot2_2 <- Sample2_Reg %>%
  ggplot(aes(x = LISD_State_F2,
             y = Depressed)) +
  geom_point(color = "#55C667FF")+
  geom_smooth(method = lm, color = "black",
              size = 0.5)+
  xlim(1,5)+
  ylim(-2,3)+
  labs(x = "supported & connected (State 2)",
       y = "Depression (z)", 
       Title = "State Factor 2")+
  theme_classic()


plot3_2 <- Sample2_Reg %>%
  ggplot(aes(x = LISD_Trait_F1,
             y = Depressed)) +
  geom_point(color = "#E7B800")+
  geom_smooth(method = lm, color = "black",
              size = 0.5)+
  xlim(1,5)+
  ylim(-2,3)+
  labs(x = "loneliness & isolation (Trait 1)",
       y = "Depression (z)", 
       Title = "Trait Factor 1")+
  theme_classic()



plot4_2 <- Sample2_Reg %>%
  ggplot(aes(x = LISD_Trait_F2,
             y = Depressed)) +
  geom_point(color = "#FC4E07")+
  geom_smooth(method = lm, color = "black",
              size = 0.5)+
  xlim(1,5)+
  ylim(-2,3)+
  labs(x = "sociability & sense of belonging (Trait 2)",
       y = "Depression (z)", 
       Title = "Trait Factor 1")+
  theme_classic()


plot1_2 
plot2_2
plot3_2
plot4_2



### create figure with 4 panels 


panel1<-ggarrange(plot1_1, plot1_2, plot3_1, plot3_2, plot4_1, plot4_2, ncol =2, nrow = 3, labels = c('A', 'D', 'B', 'E', 'C', 'F'), widths = 0.5, heights = 1.5)
ggsave("LISD_manuscript_figure1.jpg", panel1, device='tiff', dpi=300)

panel2<-ggarrange(plot1_1, plot1_2, plot3_1, plot3_2, plot4_1, plot4_2, ncol =2, nrow = 3, labels = c('A', 'D', 'B', 'E', 'C', 'F'), widths = 0.5, heights = 1.5)
ggsave("LISD_manuscript_figure1_hd.tiff", panel2, device='tiff', dpi=300)

#nebeneinander
panel3<-ggarrange(plot1_1, plot3_1, plot4_1, plot1_2, plot3_2, plot4_2, ncol = 3, nrow = 2, labels = c('A', 'B', 'C','D', 'E', 'F'))+
ggsave("LISD_manuscript_figure1_rearranged.jpg", panel3, device='tiff', dpi=300)



### clear workspace
remove(vars,Anx_main, Anx_main_2, Anx_two, Depressed_main, Depressed_main_2, Depressed_two, Sample2,Sample2_Reg)
remove(plot1_1,plot1_2,plot2_1,plot2_2,plot3_1,plot3_2,plot4_1,plot4_2, panel1, panel2, panel3)


