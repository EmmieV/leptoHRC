######## descriptive analysis 
######## Follow-up data HRC lepto
######## Emilie Vallee
######## last code update 2022-06-21
library (lubridate)
#today()


#packages
library(RODBC)
library(reshape2)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)

# Import data -------------------------------------------------------------

  #local copy, last updated 2021-11-30 
#setwd("~/work/HRC lepto/follow up data analysis")

#testdb<-file.path("E_offlineOnly.accdb")
myconn = odbcConnectAccess2007('LeptoHRC.accdb')

## Identify tables in database
sqlTables(myconn)

# get the table of follow up questionnaire
FUdat = sqlFetch(myconn,"SMFUQ")

  #at 2022-06-20  there are 75 questionnaires
#remove the pretest questionnaires
FUdat <- FUdat[FUdat$QID>4,]

# get the names to match with questionnaire
#names(FUdat)

# need to get: date acute questionnaire, date onset symptoms, date diagnsosis
  # serovar and other lab tests results 

# get serovar
labtests = sqlFetch(myconn,"LabTests")
#names(labtests)
    #merge on FKParticipantID (81 have tests - 37% of FU cases)
  
# get case info
cases = sqlFetch(myconn,"CaseQuestionnaire")
#names(cases)

#cases2 = sqlFetch(myconn,"CaseQuestionnaire2") 
#names (cases2)

cases3 = sqlFetch(myconn,"CaseQuestionnaire3")
#names (cases3)

# Data preparation  ----------------------------------------------------

# create a variable that says if the case had any symptom at onset (obviously all will have...)
FUdat$AnyReported<-NA
FUdat$AnyReported<-FUdat$HeadacheReported +
                    FUdat$FeverReported +
                    FUdat$SoreEyesReported +
                    FUdat$LightSensitivityReported +
                    FUdat$MusclePainLegsReported +
                    FUdat$MusclePainBackReported +
                    FUdat$NauseaReported +
                    FUdat$DiarrhoeaReported +
                    FUdat$AbdominalPainReported +
                    FUdat$CoughReported +
                    FUdat$FatigueReported

FUdat$AnyReportedBin <- 1
FUdat$AnyReportedBin <- ifelse(FUdat$AnyReported==0, 0, FUdat$AnyReportedBin)

# same for if they still have symptoms at follow up interview
FUdat$AnyPersistent <- 0
FUdat$AnyPersistent <- FUdat$HeadachePersistent + FUdat$FeverPersistent +
  FUdat$SoreEyesPersistent +  FUdat$LightSensitivityPersistent +
  FUdat$MusclePainLegsPersistent + FUdat$MusclePainBackPersistent +
  FUdat$NauseaPersistent + FUdat$DiarrhoeaPersistent +
  FUdat$AbdominalPainPersistent + FUdat$CoughPersistent +
  FUdat$FatiguePersistent  

FUdat$AnyPersistentBin <- 1
FUdat$AnyPersistentBin <- ifelse(FUdat$AnyPersistent==0, 0, FUdat$AnyPersistentBin)

#get a subset of cases data with variables of interest to analyse FU data
cases.short<-cases[cases$Type=="Case", c("ParticipantID", "InterviewDate", "DHB", "HomeType", "DateFirstLepto", "DateFirstSoughtHelp", 
                       "Hospitalised", "HospitalisedNights", "ICUAdmission", "ICUAdmissionDays", "LeptoTimeOff", "LeptoTimeOffDays", 
                       "AntibioticsLepto", "AntibioticsDoxycycline", "AntibioticsAmoxicillin", "AntibioticsErythromycin", "AntibioticsOthers", "AntibioticsUnsure", "DoseMissed", "DoseMissedTimes", 
                       "DOB", "Age", "AgeBand", "Gender", "EthnicityNZEuropean", "EthnicityNZMaori", "EthnicitySamoan", "EthnicityCookIsMaori", 
                       "EthnicityTongan", "EthnicityNiuean", "EthnicityChinese", "EthnicityIndian", "EthnicityOthers", "Ethnicity")]

cases3.short<-cases3[cases3$Type=="Case", c("ParticipantID", "HealthSmoke", "HealthSmokeRegular", "HealthHayfever", "HealthHayfeverYears", "HealthAsthma", "HealthAsthmaYears", 
                         "HealthDiabetes", "HealthDiabetesYears", "HealthHeart", "HealthHeartYears", "HealthLung", "HealthLungYears", "HealthLungYears", 
                         "HealthAnxiety", "HealthAnxietyYears", "HealthDepressions", "HealthDepressionsYears", "HealthOther", "HealthOtherSpecify", "HealthOtherYears", 
                         "RegularMedication", "RegularMedicationSpecify", "Antibiotics", "AntibioticsDetails", "LeptoBefore", "LeptoBeforeWhen", "RiskTired", "RiskNervous", 
                         "RiskNotCalm", "RiskHopeless", "RiskRestless", "RiskCouldNotSitStill", "RiskDepressed", "RiskEffort", "RiskSad", "RiskWorthless")]


#get subset of lab data with variables of interest 
labtests.short<-labtests[, c("FKParticipantID", "TestType", "SamplingPeriod", "CollectionDate", "TestDescription", "Results")]
  #several test results for each case obviously
  # just serology for now 
sero<-labtests[labtests$TestType%in%c("MAT", "IgM"),]
sero[sero$TestType=="IgM", ]$TestDescription<-"IgM"
sero$ResultsNum<-0
sero$ResultsNum<-ifelse(sero$Results%in%c("positive", "Positive"), 1, sero$ResultsNum) #for now neg/NA/under investigation are 0
sero$TestDescription<-as.factor(sero$TestDescription)
sero$TestDescription[sero$TestDescription=="Hardjo"]<-"Hardjobovis"
  
serowide<- dcast(sero, FKParticipantID+Laboratory+CollectionDate+TestDate+SamplingPeriod~TestDescription, value.var = 'ResultsNum', fun.aggregate = sum)
serowide.agg <- serowide[, c(1, 6:15)] %>% 
  group_by(FKParticipantID) %>%
  summarise_each(sum) %>%
ungroup()  

# keep other variables in FU dat in a separate dataframe 
FUshort<-FUdat[, c(2, 144:221)]

# get another for the FU symptoms
FUsymptoms<- FUdat[, 1:143]


# create a column indicating the duration of "any" symptom
FUsymptoms$AnyLength<-NA
FUsymptoms$AnyLength<-apply (FUsymptoms[, 32:42], 1, max, na.rm=T)


# transform to long format based on symptoms (1 row per person-symptom instead of per person) - neeed for plotting
  # 1 < a week
  # 2 a week to a month
  # 3 > 1 to 3 months
  # 4 >3 to 5 months
  # 5 6+ months 

FU_long <-  FUsymptoms[, c(2, 32:42, 144)] %>% 
  tidyr::gather("symptoms", "value", c(2:13)) %>% 
  dplyr::group_by(symptoms) %>% 
  dplyr::mutate(one_week =  ifelse(value == 1| is.na(value)==T, 0, -1)) %>% 
  dplyr::mutate(one_month =  ifelse(value <3 | is.na(value)==T, 0, -1)) %>% 
  dplyr::mutate(three_months =  ifelse(value <4 | is.na(value)==T, 0, -1)) %>% 
  dplyr::mutate(six_months =  ifelse(value <5 | is.na(value)==T, 0, -1)) 


FU_long$symptoms<-as.factor(FU_long$symptoms)
levels(FU_long$symptoms)<- c( "AbdominalPain", "Any", "Cough",           
                               "Diarrhoea", "Fatigue",         
                               "Fever", "Headache",        
                               "LightSensitivity", "MusclePainBack",  
                               "MusclePainLegs", "Nausea",          
                               "SoreEyes" )

#now pass another time to long format based on time point 
FUlong_plot <-  FU_long %>% 
  tidyr::gather("TimePoint", "Reported", c(4:7))

FUlong_plot<-FUlong_plot[, c(1:2, 4:5)]


#now we need to repeat the process for the first ("reported") timepoints that were entered differently (already existing columns for each symptom)
dat_long_d0 <- FUdat[, c(2, 5:15,219)] %>% 
  tidyr::gather("symptoms", "Reported", 2:13)
  #and add a timepoint column with d0
dat_long_d0.2<-cbind(dat_long_d0, "TimePoint" = rep ("d0", dim(dat_long_d0)[1]))
  #and rename symptoms
dat_long_d0.2$symptoms<-as.factor(dat_long_d0.2$symptoms)
levels(dat_long_d0.2$symptoms)<-c( "AbdominalPain", "Any", "Cough",           
                                   "Diarrhoea", "Fatigue",         
                                   "Fever", "Headache",        
                                   "LightSensitivity", "MusclePainBack",  
                                   "MusclePainLegs", "Nausea",          
                                   "SoreEyes" )

#check the time between onset and follow-up interview
dattime<-merge(FUdat[, c("FKParticipantID", "InterviewDate")], cases[, c("ParticipantID", "DateFirstLepto")], by.x="FKParticipantID", by.y="ParticipantID")
dattime$timeAcuteFU<-NA
dattime$InterviewDate<-as.Date(dattime$InterviewDate, format="%Y-%m-%d")
dattime$DateFirstLepto<-as.Date(dattime$DateFirstLepto, format="%Y-%m-%d")
dattime$timeAcuteFU<-dattime$InterviewDate-dattime$DateFirstLepto
hist(as.numeric(dattime$timeAcuteFU), main = "days between onset of symptoms and follow-up interview", xlab="days")
summary(as.numeric(dattime$timeAcuteFU))
summary(as.numeric(dattime$timeAcuteFU/30.4)) # in months

# and do the same with the last timepoint 
dat_long_8mo <- FUdat[, c(2, 89:99,221 )] %>% 
  tidyr::gather("symptoms", "Reported", c(2:13))  

dat_long_8mo.2<-cbind(dat_long_8mo, "TimePoint" = rep ("eight_months", dim(dat_long_8mo)[1]))
#and rename symptoms
dat_long_8mo.2$symptoms<-as.factor(dat_long_8mo.2$symptoms)
levels(dat_long_8mo.2$symptoms)<-c( "AbdominalPain", "Any", "Cough",           
                                   "Diarrhoea", "Fatigue",         
                                   "Fever", "Headache",        
                                   "LightSensitivity", "MusclePainBack",  
                                   "MusclePainLegs", "Nausea",          
                                   "SoreEyes" )


#and now add the 2 dataframes for d0 and 8 months to FUlong_plot
FU_plot <- rbind(dat_long_8mo.2, dat_long_d0.2)
FUlong_plot<-FUlong_plot[, c(1, 2, 4, 3)]
FUlong_plot$TimePoint<-as.factor(FUlong_plot$TimePoint)
FU_plot<-dplyr::bind_rows(FUlong_plot, FU_plot)
#summary(FU_plot)
FU_plot$TimePoint<- factor(FU_plot$TimePoint, levels=c("d0", "one_week", "one_month", "three_months", "six_months", "eight_months"))


# describe new symptoms ---------------------------------------------------

table(FUdat$OtherSymptom1, useNA = "ifany")
table(FUdat$OtherSymptom1Length)


# Plot symptoms as a function of time -------------------------------------


FU_plot_prop <- FU_plot %>% 
  group_by(TimePoint, symptoms) %>% 
  summarise(Reported = mean(abs(Reported))*100)

ggplot(data=FU_plot_prop[FU_plot_prop$symptoms=="Any",], aes(x=TimePoint, y=Reported, group=symptoms)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  labs(
    title = "Any symptom",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )


ggplot(data=FU_plot_prop, aes(x=TimePoint, y=Reported, group=symptoms)) +
  geom_line()+
  geom_point()+
  facet_wrap(vars(symptoms))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim (0, 100) +
  labs(
    title = "All symptoms",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still symptom"
  )



# severity and frequency --------------------------------------------------
(par(mfrow=c(1,2)))
barplot(table(FUdat$HeadacheFreq, useNA = "ifany"), col = c("black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "not reported"), main="Frequency of headache", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$FeverFreq, useNA = "ifany"), col = c("black", "black", "black", "grey", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "NA", "not reported"), main="Frequency of fever", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$SoreEyesFreq, useNA = "ifany"), col = c("black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "not reported"), main="Frequency of sore eyes", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$LightSensitivityFreq, useNA = "ifany"), col = c("black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "not reported"), main="Frequency of light sensitivity", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$MusclePainLegsFreq, useNA = "ifany"), col = c("black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "not reported"), main="Frequency of muscle pain in legs", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$MusclePainBackFreq, useNA = "ifany"), col = c("black", "black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "monthly", "not reported"), main="Frequency of muscle pain in back", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$NauseaFreq, useNA = "ifany"), col = c("black", "black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "monthly", "not reported"), main="Frequency of nausea", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$DiarrhoeaFreq, useNA = "ifany"), col = c("black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "not reported"), main="Frequency of diarrhoea", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$AbdominalPainFreq, useNA = "ifany"), col = c("black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "not reported"), main="Frequency of abdominal pain", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$CoughFreq, useNA = "ifany"), col = c("black", "black", 'black',  "grey", "grey"), names.arg = c("Daily", "2-3/week", 'weekly', "999", "not reported"), main="Frequency of cough", ylim = c(0, dim(FUdat)[1]))
barplot(table(FUdat$FatigueFreq, useNA = "ifany"), col = c("black", "black", "black", "black", "grey"), names.arg = c("Daily", "2-3/week", "weekly", "<5 times", "not reported"), main="Frequency of fatigue", ylim = c(0, dim(FUdat)[1]))
(par(mfrow=c(1,1)))

(par(mfrow=c(1,2)))
colsev<-brewer.pal (5, "RdYlGn")
barplot(table(as.factor(FUdat$HeadacheSeverity), useNA = "ifany"), col=c(rev(colsev), "grey"), xlab = "1 = mild to 5 = severe", main = "severity of headache", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$FeverSeverity), useNA = "ifany"), col=c(rev(colsev), "grey", "grey"), xlab = "1 = mild to 5 = severe", main = "severity of fever", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$SoreEyesSeverity), useNA = "ifany"), col=c(rev(colsev), "grey"), xlab = "1 = mild to 5 = severe", main = "severity of sore eyes", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$LightSensitivitySeverity), useNA = "ifany"), col=c(rev(colsev), "grey"), xlab = "1 = mild to 5 = severe", main = "severity of light sensitivity", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$MusclePainLegsSeverity), useNA = "ifany"), col=c(rev(colsev), "grey"), xlab = "1 = mild to 5 = severe", main = "severity of muscle pain in the legs", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$MusclePainBackSeverity), useNA = "ifany"), col=c(rev(colsev), "grey"), xlab = "1 = mild to 5 = severe", main = "severity of muscle pain in the back", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$NauseaSeverity), useNA = "ifany"), col=c(rev(colsev), "grey", "grey"), xlab = "1 = mild to 5 = severe", main = "severity of nausea", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$DiarrhoeaSeverity), useNA = "ifany"), col=c(rev(colsev), "grey"), xlab = "1 = mild to 5 = severe", main = "severity of diarrhoea", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$AbdominalPainSeverity), useNA = "ifany"), col=c(rev(colsev), "grey"), xlab = "1 = mild to 5 = severe", main = "severity of abdominal pain", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$CoughSeverity), useNA = "ifany"), col=c(rev(colsev)[1:4], "grey"), xlab = "1 = mild to 5 = severe", main = "severity of cough", ylim = c(0, dim(FUdat)[1]))
barplot(table(as.factor(FUdat$FatigueSeverity), useNA = "ifany"), col=c(rev(colsev)[2:5], "grey"), xlab = "1 = mild to 5 = severe", main = "severity of fatigue", ylim = c(0, dim(FUdat)[1]))
(par(mfrow=c(1,1)))


# prepare data for comparison ---------------------------------------------


#merge all the previously made subsets with FU_plot  (on ParticipantID/FKParticipantID)
dat<-merge(FU_plot, cases.short, by.x="FKParticipantID", by.y = "ParticipantID", all.x = T)
dat<-merge(dat, cases3.short, by.x="FKParticipantID", by.y = "ParticipantID", all.x = T)
dat<-merge(dat, serowide.agg, by.x = "FKParticipantID", by.y = "FKParticipantID", all.x = T)



# comparison by serovar -----------------------------------------------
summary(dat[, 74:83]) # no Australis, no Grippotyphosa
#Canicola: one seropositive, persistent fatigue

dat_prop <- dat[dat$IgM>0,] %>% 
  group_by(TimePoint, symptoms) %>% 
  summarise(Reported = mean(abs(Reported))*100)

ggplot(data=dat_prop[dat_prop$symptoms=="Any",], aes(x=TimePoint, y=Reported, group=symptoms)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  labs(
    title = "Any symptom, seropositive for IgM",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

#copenhageni, hardjobovis, pomona, tarassovi
table(labtests$TestDescription, labtests$Results)

datuniq<-dat[dat$symptoms=="Any"&dat$TimePoint=="d0",]
table(datuniq$Ballum, useNA = "ifany")
table(datuniq$Hardjobovis, useNA = "ifany")
table(datuniq$Pomona, useNA = "ifany")
table(datuniq$Tarassovi, useNA = "ifany")
table(datuniq$IgM, useNA = "ifany")



# comparison by age -------------------------------------------------------

#table(is.na(cases$DOB), cases$Age) #14 did not give any
#table(cases$AgeBand) # 3 
#table(cases$Age, cases$AgeBand, useNA = "ifany") #same 3 and also gave their DOB

dat$Age2<-NA
dat$Age2<-as.numeric(dat$InterviewDate - dat$DOB)/365.25
summary(dat$Age2)
hist(dat$Age2)

dat$AgeCat<-NA
dat$AgeCat<- ifelse(dat$Age2<20, "19 and less", 
                    ifelse(dat$Age2>=20 & dat$Age2<30, "20-29", 
                           ifelse(dat$Age2>=30 & dat$Age2<40, "30-39", 
                                  ifelse(dat$Age2>= 40 & dat$Age2<50, "40-49", 
                                         ifelse(dat$Age2>=50 & dat$Age2<60, "50-59", 
                                                ifelse(dat$Age2>=60 & dat$Age2<70, "60-69", 
                                                       ifelse(dat$Age2>=70, "70 and more", dat$AgeCat)))))))

#table(dat$AgeCat, useNA = "ifany")/72

dat_age <- dat %>% 
  group_by(TimePoint, symptoms, AgeCat) %>% 
  summarise(Reported = mean(abs(Reported))*100)

ggplot(data=dat_age[dat_age$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=AgeCat, group=AgeCat)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Spectral")+
  labs(
    title = "by age group",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )


# comparison by gender -------------------------------------------------------
 
  dat[dat$Gender=="M",]$Gender<-"Male"
  dat[dat$Gender=="F",]$Gender<-"Female"
  
   dat_gender <- dat %>% 
    group_by(TimePoint, symptoms, Gender) %>% 
    summarise(Reported = mean(abs(Reported))*100)
  
  ggplot(data=dat_gender[dat_gender$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=Gender, group=Gender)) +
    geom_line()+
    geom_point()+
    theme_bw()+
    ylim (0, 100) +
    scale_color_brewer(palette="Set1")+
  labs(
    title = "by gender",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

#  by dhb -----------------------------------------------------------------

#table(dat$DHB)/72
  dat_DHB <- dat %>% 
    group_by(TimePoint, symptoms, DHB) %>% 
    summarise(Reported = mean(abs(Reported))*100)
  
  ggplot(data=dat_DHB[dat_DHB$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=DHB, group=DHB)) +
    geom_line()+
    geom_point()+
    theme_bw()+
    ylim (0, 100) +
    scale_color_brewer(palette="Paired")+
  labs(
    title = "by DHB",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )
  
  
  ggplot(data=dat_DHB[dat_DHB$symptoms=="Any"&dat_DHB$DHB%in%c("Hawke's Bay", "Midcentral", "Northland", "Waikato"),], aes(x=TimePoint, y=Reported, color=DHB, group=DHB)) +
    geom_line()+
    geom_point()+
    theme_bw()+
    ylim (0, 100) +
    scale_color_brewer(palette="Paired")+
    labs(
      title = "by DHB",
      x = "Time since onset of symptoms",
      y = "Proportion of cases still having at least 1 symptom" 
    )
  
  
# by hospitalisation/ICU ------------------------------------------------------
table(dat$Hospitalised)/72
  table(dat$ICUAdmission)/72

dat$HospICU<-NA
dat$HospICU<-dat$Hospitalised
dat$HospICU<-ifelse(dat$ICUAdmission<0 & is.na(dat$ICUAdmission)==F, "ICU", dat$HospICU)
dat[dat$HospICU==-1, ]$HospICU<-"Hospitalised"
dat[dat$HospICU==0, ]$HospICU<-"Not Hospitalised"

dat_hosp <- dat %>% 
  group_by(TimePoint, symptoms, HospICU) %>% 
  summarise(Reported = mean(abs(Reported))*100)

ggplot(data=dat_hosp[dat_hosp$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HospICU, group=HospICU)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Paired") +
labs(
  title = "by hospitalisation",
  x = "Time since onset of symptoms",
  y = "Proportion of cases still having at least 1 symptom" 
)


# by antibiotics treatment ------------------------------------------------
table(dat$Antibiotics, useNA = "ifany")/72 #only 2??? not coherent when looking at antibiotics names
dat_amox <- dat %>% 
  group_by(TimePoint, symptoms, AntibioticsAmoxicillin) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_amox$AntibioticsAmoxicillin<-as.factor(dat_amox$AntibioticsAmoxicillin)

ggplot(data=dat_amox[dat_amox$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=AntibioticsAmoxicillin, group=AntibioticsAmoxicillin)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  #scale_color_brewer(palette="Paired") +
  labs(
    title = "by antibiotics treatment - Amoxicillin",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )



dat_doxi <- dat %>% 
  group_by(TimePoint, symptoms, AntibioticsDoxycycline) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_doxi$AntibioticsDoxycycline<-as.factor(dat_doxi$AntibioticsDoxycycline)

ggplot(data=dat_doxi[dat_doxi$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=AntibioticsDoxycycline, group=AntibioticsDoxycycline)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  #scale_color_brewer(palette="Paired") +
  labs(
    title = "by antibiotics treatment - Doxycycline",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )


table(dat$AntibioticsErythromycin, useNA = "ifany")/72


# by ethnicity ------------------------------------------------------------
table(dat$EthnicityNZMaori, useNA = "ifany")/72 #9 Maori
table(dat$EthnicityNZEuropean, useNA = "ifany")/72 #63 europeans
table(dat$EthnicityCookIsMaori, useNA = "ifany")/72 #none
table(dat$EthnicitySamoan, useNA = "ifany")/72 #none
table(dat$EthnicityTongan, useNA = "ifany")/72 #none
table(dat$EthnicityNiuean, useNA = "ifany")/72 #None
table(dat$EthnicityChinese, useNA = "ifany")/72 #none
table(dat$EthnicityIndian, useNA = "ifany")/72 #1 Indian
table(dat$EthnicityOthers, useNA = "ifany")/72 #5 others
table(dat$Ethnicity, useNA = "ifany")/72 #2 South African (only)

table(dat$EthnicityNZEuropean, dat$EthnicityNZMaori)/72 # 3 are both, 6 just Maori, 60 just NZ europ, 1 indian, 2 SA, 3 unknown? 

dat$Ethnic<-NA
dat$Ethnic<-paste(dat$EthnicityNZEuropean, dat$EthnicityNZMaori)
dat[dat$Ethnic=="-1 -1",]$Ethnic<-"Maori" #Prioritise Maori
dat[dat$Ethnic=="-1 0",]$Ethnic<-"Europ"
dat[dat$Ethnic=="0 -1",]$Ethnic<-"Maori"
dat[dat$Ethnic=="0 0",]$Ethnic<-"Other"


dat_ethnic <- dat %>% 
  group_by(TimePoint, symptoms, Ethnic) %>% 
  summarise(Reported = mean(abs(Reported))*100)

ggplot(data=dat_ethnic[dat_ethnic$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=Ethnic, group=Ethnic)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Paired") +
  labs(
    title = "by ethnicity",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )


# by comorbidities --------------------------------------------------------
table(dat$HealthSmoke, useNA = "ifany")/72 #6 smokers
dat_smoke <- dat %>% 
  group_by(TimePoint, symptoms, HealthSmoke) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_smoke$HealthSmoke<-as.factor(dat_smoke$HealthSmoke)

ggplot(data=dat_smoke[dat_smoke$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HealthSmoke, group=HealthSmoke)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Paired") +
  labs(
    title = "by smoking",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

table(dat$HealthSmokeRegular, useNA = "ifany")/72 #14
dat_smokereg <- dat %>% 
  group_by(TimePoint, symptoms, HealthSmokeRegular) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_smokereg$HealthSmokeRegular<-as.factor(dat_smokereg$HealthSmokeRegular)

ggplot(data=dat_smokereg[dat_smokereg$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HealthSmokeRegular, group=HealthSmokeRegular)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Paired") +
  labs(
    title = "by smoking (regular)",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

table(dat$HealthHayfever, useNA = "ifany")/72 #10
dat_hayfev <- dat %>% 
  group_by(TimePoint, symptoms, HealthHayfever) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_hayfev$HealthHayfever<-as.factor(dat_hayfev$HealthHayfever)

ggplot(data=dat_hayfev[dat_hayfev$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HealthHayfever, group=HealthHayfever)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "by hay fever",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

table(dat$HealthAsthma, useNA = "ifany")/72 #6
dat_asthma <- dat %>% 
  group_by(TimePoint, symptoms, HealthAsthma) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_asthma$HealthAsthma<-as.factor(dat_asthma$HealthAsthma)

ggplot(data=dat_asthma[dat_asthma$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HealthAsthma, group=HealthAsthma)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "by asthma",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

table(dat$HealthDiabetes, useNA = "ifany")/72 #4
dat_diabetes <- dat %>% 
  group_by(TimePoint, symptoms, HealthDiabetes) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_diabetes$HealthDiabetes<-as.factor(dat_diabetes$HealthDiabetes)

ggplot(data=dat_diabetes[dat_diabetes$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HealthDiabetes, group=HealthDiabetes)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "by diabetes",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

table(dat$HealthHeart, useNA = "ifany")/72 #4
dat_heart <- dat %>% 
  group_by(TimePoint, symptoms, HealthHeart) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_heart$HealthHeart<-as.factor(dat_heart$HealthHeart)

ggplot(data=dat_heart[dat_heart$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HealthHeart, group=HealthHeart)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "by heart",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

table(dat$HealthLung, useNA = "ifany")/72 # None
table(dat$HealthAnxiety, useNA = "ifany")/72 #7
dat_anx <- dat %>% 
  group_by(TimePoint, symptoms, HealthAnxiety) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_anx$HealthAnxiety<-as.factor(dat_anx$HealthAnxiety)

ggplot(data=dat_anx[dat_anx$symptoms=="Any"&dat_anx$HealthAnxiety%in%c("0", "-1"),], aes(x=TimePoint, y=Reported, color=HealthAnxiety, group=HealthAnxiety)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "by anxiety",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

table(dat$HealthDepressions, useNA = "ifany")/72 #10
dat_depress <- dat %>% 
  group_by(TimePoint, symptoms, HealthDepressions) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_depress$HealthDepressions<-as.factor(dat_depress$HealthDepressions)

ggplot(data=dat_depress[dat_depress$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HealthDepressions, group=HealthDepressions)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "by depression",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )

table(dat$HealthOtherSpecify, useNA = "ifany")/72 #get high blood pressure from others n=7

dat$HealthHighBloodPressure<-0
dat$HealthHighBloodPressure<-ifelse(dat$HealthOtherSpecify=="High blood pressure"&is.na(dat$HealthOtherSpecify)==F, 1, dat$HealthHighBloodPressure)
dat$HealthHighBloodPressure<-ifelse(dat$HealthOtherSpecify=="High blood pressure,gout,arthritis-15 years, had cancer 7 years ago"&is.na(dat$HealthOtherSpecify)==F, 1, dat$HealthHighBloodPressure)
dat$HealthHighBloodPressure<-ifelse(dat$HealthOtherSpecify=="Blood pressure and got gall bladder removed 2 years ago"&is.na(dat$HealthOtherSpecify)==F, 1, dat$HealthHighBloodPressure)

table(dat$HealthHighBloodPressure, useNA = "ifany")/72

dat_HBP <- dat %>% 
  group_by(TimePoint, symptoms, HealthHighBloodPressure) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_HBP$HealthHighBloodPressure<-as.factor(dat_HBP$HealthHighBloodPressure)

ggplot(data=dat_HBP[dat_HBP$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=HealthHighBloodPressure, group=HealthHighBloodPressure)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "by hihg blood pressure",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )


# previous lepto ----------------------------------------------------------
table(dat$LeptoBefore, useNA = "ifany")/72

dat_leptobef <- dat %>% 
  group_by(TimePoint, symptoms, LeptoBefore) %>% 
  summarise(Reported = mean(abs(Reported))*100)

dat_leptobef$LeptoBefore<-as.factor(dat_leptobef$LeptoBefore)

ggplot(data=dat_leptobef[dat_leptobef$symptoms=="Any",], aes(x=TimePoint, y=Reported, color=LeptoBefore, group=LeptoBefore)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim (0, 100) +
  scale_color_brewer(palette="Paired") +
  labs(
    title = "by previous lepto",
    x = "Time since onset of symptoms",
    y = "Proportion of cases still having at least 1 symptom" 
  )



# Work life ---------------------------------------------------------------

cases.extra<-cases[cases$Type=="Case", c("ParticipantID", "JobBeforeLepto1", "JobBeforeLepto2", "JobBeforeLepto3", "JobNotWorking")]
dat2<-merge(FUdat, cases.extra, by.x = "FKParticipantID", by.y = "ParticipantID", all.x = T)

#jobs before lepto vs. at follow-up
dat2[, c("JobBeforeLepto1", 'CurrentJob1')]

#total days off work 
boxplot(FUdat$TotalDays, main="Total days off work")
summary(FUdat$TotalDays)
FUdat$TotalDaysCat<-NA
FUdat$TotalDaysCat<-ifelse(FUdat$TotalDays==0, "None", 
                           ifelse(FUdat$TotalDays>0 & FUdat$TotalDays<7, "<1 week", 
                                  ifelse(FUdat$TotalDays>=7 & FUdat$TotalDays<14, "1-2 weeks", 
                                         ifelse(FUdat$TotalDays>=14 & FUdat$TotalDays<21, "2-3 weeks", 
                                                ifelse(FUdat$TotalDays>=21 & FUdat$TotalDays<28, "3-4 weeks", 
                                                       ifelse(FUdat$TotalDays>=28 & FUdat$TotalDays<60, "1-2 months", 
                                                              ifelse(FUdat$TotalDays>=60, "2 months or more", FUdat$TotalDaysCat)))))))

FUdat$TotalDaysCat<-factor(FUdat$TotalDaysCat, levels = c("None", "<1 week", "1-2 weeks", "2-3 weeks", "3-4 weeks", "1-2 months", "2 months or more"), ordered = T)
barplot(table(FUdat$TotalDaysCat), main = "Total time off work")


#paid sick leave
table(FUdat$PaidSickLeave, useNA = "ifany")

#workplace support
barplot(table(FUdat$WorkplaceSupport), names.arg = c("Very well", "Well", "Neutral", "Not well", "Not very well at all", "NA"), main = "Workplace supported recovery")



# Workplace compensation --------------------------------------------------

#eligibility
table(FUdat$CompensationEligibility, useNA = "ifany")
table(FUdat$NonEligibilityReason, useNA = "ifany")

#claim lodged by GP
table(FUdat$ClaimLodgedGP, useNA = "ifany")
table(FUdat$NotLodgedReason, useNA = "ifany")

#claim accepted
table(FUdat$ClaimAccepted, useNA="ifany")

#entitlement
table(FUdat$EntitlementIncome, useNA="ifany")
table(FUdat$EntitlementRehabCosts, useNA="ifany")
table(FUdat$EntitlementLumpSum, useNA="ifany")
table(FUdat$EntitlementOtherSpecify, useNA="ifany")

#Compensation experience
barplot(table(FUdat$CompensationExperience, useNA = "ifany"), names.arg = c("Extremely satisfied", "Satisfied", "Dissatisfied", "Extremely dissatisfied", "NA"), col=c("green", "lightgreen", "orange", "red", "grey"), main = "Compensation experience")

#Need improvement
table(FUdat$ImprovementSupport, useNA="ifany")
table(FUdat$ImprovementComms, useNA="ifany")
table(FUdat$ImprovementTimeliness, useNA="ifany")
table(FUdat$ImprovementOthersSpecify, useNA="ifany")

#Went well 
table(FUdat$WentWellSupport, useNA="ifany")
table(FUdat$WentWellComms, useNA="ifany")
table(FUdat$WentWellTImeliness, useNA="ifany")
table(FUdat$AreasWentWellSpecify, useNA="ifany")


# K10  --------------------------------------------------------------------

#get K10 data acute and follow-up
FUK10<-FUdat[, c("FKParticipantID", "RiskTired", "RiskNervous", "RiskNotCalm", "RiskHopeless", "RiskRestless", "RiskCouldNotSitStill", "RiskDepressed", "RiskEffort", "RiskSad", "RiskWorthless")]
casesK10<- cases3[cases3$Type=="Case", c("ParticipantID", "RiskTired", "RiskNervous", "RiskNotCalm", "RiskHopeless", "RiskRestless", "RiskCouldNotSitStill", "RiskDepressed", "RiskEffort", "RiskSad", "RiskWorthless")]

K10<-merge(FUK10, casesK10, by.x = "FKParticipantID", by.y = "ParticipantID", all.x = T)

K10long <- K10 %>%
  tidyr::gather("K10", "Frequency", c(2:21))


K10long$Time<-str_sub(K10long$K10, start=-1)
K10long$K10<-substr(K10long$K10, 1, nchar(K10long$K10)-2)

K10long$Time<-as.factor(K10long$Time)
levels(K10long$Time)<-c("Follow-up", "Acute")
K10long$Time<-factor(K10long$Time, levels=c("Acute", "Follow-up"), ordered = T)
K10long$K10<-as.factor(K10long$K10)
K10long$K10<-factor(K10long$K10, levels=c("RiskTired", "RiskNervous", "RiskNotCalm", "RiskHopeless", "RiskRestless", "RiskCouldNotSitStill", "RiskDepressed", "RiskEffort", "RiskSad", "RiskWorthless"), ordered=T)


K10plot<- K10long %>%
  group_by(Time, K10) %>%
  count(Frequency) %>%
  ungroup()  

K10plot$Frequency<-as.factor(K10plot$Frequency)
levels(K10plot$Frequency)<-c("None", "A little", "Some", "Most", "All")

ggplot(K10plot, aes(x=Frequency, y=n, fill=Time, group=Time)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_fill_brewer(palette="Set2")+
  facet_wrap(vars(K10))



# Costs -------------------------------------------------------------------

#number of doctor consultations
table(FUdat$LeptoConsultationsNotes)


hist(FUdat$LeptoConsultations)
barplot(table(FUdat$LeptoConsultations), main="Number of doctor consultations for leptospirosis since onset")


barplot(table(FUdat$CurrentIncome), names.arg = c("Never", "Rarely", "Occasionally", "Always"), main="Lepto impacted on meeting your everyday needs")
table(FUdat$CostChildcare)
table(FUdat$CostDoctors)
summary(FUdat$CostDoctorsAmt)
table(FUdat$CostMedicines)
summary(FUdat$CostMedicinesAmt)
table(FUdat$CostOtherHealth)
summary(FUdat$CostOtherHealthAmt)
table(FUdat$CostTransport)
summary(FUdat$CostTransportAmt)
table(FUdat$CostTimeOff)
summary(FUdat$CostTimeOffAmt)
table(FUdat$CostHousework)
table(FUdat$CostGardening)

# Social and family life --------------------------------------------------

barplot(table(FUdat$SocialRestrictions), names.arg = c("Never", "Rarely", "Occasionally", "Often", "Always"), main="Lepto restricted you from participating in social/cultural events")

barplot(table(FUdat$CopedLeptoJourney), names.arg = c("Very well", "Well", "Neutral", "Not well", "Not well at all"), main="Coping with lepto journey")

barplot(table(FUdat$ReturnedNormalcy), names.arg = c("Very well", "Well", "Neutral", "Not well", "Not very well at all"), main="Returned to usual daily activities")


barplot(table(FUdat$FamilySupport), names.arg = c("Very well", "Well", "Not well"), main="Support family and friends")
