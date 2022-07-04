#libraries

library(fastDummies)
library(texreg)


#load the second dataset obtained as transformation of the original one

data2<-read.csv("Cervical_brace_2.csv",sep=",")

#some preprocessing operations

data2[is.na(data2[,11]),11]<-"no"
data2[is.na(data2[,12]),12]<-"no"
data2[is.na(data2[,13]),13]<-"no"
data2[is.na(data2[,14]),14]<-"no"
data2[is.na(data2[,15]),15]<-"no"
data2[is.na(data2[,16]),16]<-"no"
data2[which(data2[,11]=="si, no"),11]<-"si"
data2[is.na(data2[,17]),17]<-"no"
data2[is.na(data2[,18]),18]<-"no"
data2[is.na(data2[,19]),19]<-"no"
data2[is.na(data2[,20]),20]<-"no"
data2[is.na(data2[,21]),21]<-"no"
data2[is.na(data2[,22]),22]<-"no"
data2[is.na(data2[,23]),23]<-"no"
data2[is.na(data2[,24]),24]<-"no"
data2[is.na(data2[,25]),25]<-"no"
data2[is.na(data2[,26]),26]<-"no"
data2[is.na(data2[,27]),27]<-"no"

tofactor<-c(2,3,6:8,10:29)
data2[,tofactor]<-lapply(data2[,tofactor],factor)

data2[,11]<-droplevels(data2[,11])
data2$Bacino.di.utenza.afferente.all.Ospedale<-factor(data2$Bacino.di.utenza.afferente.all.Ospedale,
                                                     levels=c("<100.000","100.000-500.000","500.000-1.000.000",">1.000.000"))
### SURGERIES EXPLORATORY ANALYSIS

summary(data2$Collare)
#collars: 144 soft, 138 no collar, 298 rigid

tib2<-as_tibble(data2)
dplyr::group_by(tib2,Intervento)%>%dplyr::summarize(n=n())
#only 1 surgery is performed by all the surgeons

surg<-dplyr::group_by(tib2,Intervento,Collare)%>%dplyr::summarize(n=n())
colnames(surg)<-c("Surgery","Collar","amount")

surg$Surgery<-c(rep("1 level DAA with plate" , 3) ,
                               rep("1 level DAA without plate" , 3) ,
                               rep("2 level DAA without plate" , 3) ,
                               rep("DAA of more than 1 level with plate" , 3),
                               rep("Circumferential fixation" , 3),rep("Occipito-cervical fixation" , 3),
                               rep("Cervical laminectomy with posterior stabilization" , 3),
                               rep("Cervical laminectomy without stabilization" , 3),
                               rep("Open door laminoplasty" , 3), rep("C1-C2 Stabilization" , 3) )
levels(surg$Collar)<-c("soft","no collar","rigid")

#we can explore the amount of prescription for each collar
spread(surg,Collar,amount)

#compute percentages to create a proper plot
prescription<-spread(surg,Collar,amount)
prescription_df<-as.data.frame(prescription)

for(i in 1:10){
 prescription_df[i,2:4]<-prescription_df[i,2:4]/sum(prescription_df[i,2:4])
}

prescription_tib<-as_tibble(prescription_df)
prescription_long<-gather(prescription_tib,soft,'no collar',rigid,key=collar,value=amount)%>%arrange(Surgery)
prescription_long$Surgery = str_wrap(prescription_long$Surgery, width = 15)

ggplot(prescription_long[1:15,],aes(y=Surgery,x=amount,fill=collar))+geom_col(position = position_dodge())+
  geom_text(aes(label = paste0(sprintf("%1.1f", amount*100),"%")), position = position_dodge(0.9), hjust = 1, size = 5.3, color = "black")+
  labs(y="",title="Cervical brace prescription",x="percentage")+scale_fill_brewer(palette="Dark2")+
  theme_bw()

ggplot(prescription_long[16:30,],aes(y=Surgery,x=amount,fill=collar))+geom_col(position = position_dodge())+
  geom_text(aes(label = paste0(sprintf("%1.1f", amount*100),"%")), position = position_dodge(0.9), hjust = 1, size = 5.3, color = "black")+
  labs(y="",title="Cervical brace prescription",x="percentage")+scale_fill_brewer(palette="Dark2")+
  theme_bw()



# create dummy variables for logistic regression
data_du<-data2[,-c(22:27)]
data_du<-dummy_cols(data_du,select_columns = c("Specializzazione","Tipologia.di.ospedale","Ospedale.di.attività","Regione","Bacino.di.utenza.afferente.all.Ospedale",             
                                      "Principale.patologia.trattata","Intervento"))
#removing original variables
data_du<-data_du[,-c(2,3,6,7,8,10,22)]

#creating proper factors
data_du[,17:42] <- lapply(data_du[,17:42] , factor)

for(i in 5:15){
  data_du[i]<-as.factor(as.numeric(data_du[,i])-1)
}
#1 yes 0 no

colnames(data_du)<-c("ID","Age","Experience in cervical surgery","log(surgeries per year)","Post-operative pain",
                  "Earlier mobilization of the patient","Arthrodesis ratio increase","Patient 'expects' a brace",
                  "Legal and medical protection","Post surgical kyphosis rate reduction","Literature","Personal experience",
                  "Colleagues' teaching ","Local customs","Tutela medico legale ","Collar","Neurosurgery","Orthopaedic",
                  "'other' hospital","Polyclinic hospital","University hospital","Affiliate hospital",                                            
                  "Public hospital","Central Italy","Northern Italy","Southern Italy and islands","Catchment area <100.000",                              
                  "Catchment area 100.000-500.000","Catchment area 500.000-1.000.000","Catchment area >1.000.000","Elective pathology","Traumatic pathology",                                      
                  "1 level discectomy and anterior arthrodesis with plate surgery"," 1 level discectomy and anterior arthrodesis without plate surgery",     
                  "2 level discectomy and anterior arthrodesis without plate surgery"," 1 or more level discectomy and anterior arthrodesis with plate surgery",
                  "Circumferential fixation","Occipito-cervical fixation",                                     
                  "Cervical laminectomy with posterior stabilization","Cervical laminectomy without stabilization",              
                  "Open door laminoplasty","C1-C2 Stabilization")
levels(data_du$Collar)<-c("soft","no collar","rigid")


############INFERENCE


####   SOFT VS RIGID COLLAR

data_sr<-data_du[data_du$Collar=="soft"|data_du$Collar=="rigid",]
data_sr$Collar<-droplevels(data_sr$Collar)
data_sr$Collar<-relevel(data_sr$Collar,ref="soft")
summary(data_sr$Collar)

#standardize quantitative predictors

data_sr[,3]<-(data_sr[,3]-mean(data_sr[,3]))/sqrt(var(data_sr[,3]))
data_sr[,4]<-(data_sr[,4]-mean(data_sr[,4]))/sqrt(var(data_sr[,4]))

#full model estimation
#removing 1 dummy variable for each categorical variable in order to avoid multicollinearity
mod_sr<-glm(Collar~.-ID - Age-Neurosurgery-`Catchment area 100.000-500.000`
            -`'other' hospital`-`Northern Italy`-`Public hospital`
            -`Elective pathology`-`1 level discectomy and anterior arthrodesis with plate surgery`
            ,family="binomial",data=data_sr)
summary(mod_sr)
exp(coef(mod_sr))
AIC(mod_sr)#456.2

#stepwise selection
mod_sr2<-stepAIC(mod_sr, direction = "both", trace = FALSE)
texreg(mod_sr2,single.row = T,scalebox = 0.7,stars = c(0.01, 0.05, 0.1))

summary(mod_sr2)
summ(mod_sr2,exp=T)
AIC(mod_sr2)#439.2

#variance inflation factor
vif(mod_sr2)#ok

#Hosmer-Lemwshow test
HLtest(mod_sr2,g=24)#ok

#Pearson's statistics gof test
rp<-residuals(mod_sr2,type="pearson") 
pchisq(sum(rp^2),420,lower.tail = F)#ok

plot(mod_sr2,which=4)
influenceIndexPlot(mod_sr2)
#397 has high influence on the model
data_du["397",]

outlierTest(mod_sr2)
#but it turns out it is not an outlier
#we keep it in the model

rw<-rstudent(mod_sr2)
eta<-predict(mod_sr2,data_sr)

gg<-data.frame(eta,rw,data_sr$Collar)
levels(gg$data_sr.Collar)<-c("indianred3", "cornflowerblue")

ggplot(gg,aes(x=eta,y=rw))+geom_point(col=(gg$data_sr.Collar))+geom_smooth(col="blueviolet")+theme_bw()+
  labs(y="Studentized residuals",
       x="Linear predictor")+ theme(
         axis.title.x = element_text(color = "black", size = 16, face = "bold"),
         axis.title.y = element_text(color = "black", size = 16, face = "bold"))
#everything ok, no systematic trend exists


####   SOFT VS NO COLLAR

data_sn<-data_du[data_du$Collar=="soft"|data_du$Collar=="no collar",]
data_sn$Collar<-relevel(data_sn$Collar,ref="soft")
data_sn$Collar<-droplevels(data_sn$Collar)
summary(data_sn$Collar)

#excluding predictors that create perfect separation
data_sn<-data_sn[,-c(5:10)]

#standardize quantitative predictors
data_sn[,3]<-(data_sn[,3]-mean(data_sn[,3]))/sqrt(var(data_sn[,3]))
data_sn[,4]<-(data_sn[,4]-mean(data_sn[,4]))/sqrt(var(data_sn[,4]))

#full model estimation
mod_sn<-glm(Collar~.-ID - Age-Neurosurgery-`Catchment area 100.000-500.000`
            -`'other' hospital`-`Northern Italy`-`Public hospital`
            -`Elective pathology`-`1 level discectomy and anterior arthrodesis with plate surgery`
            ,family="binomial",data=data_sn)
summary(mod_sn)
exp(coef(mod_sn))
AIC(mod_sn)#316.2

#stepwise selection
mod_sn2<-stepAIC(mod_sn,direction="both")
summary(mod_sn2)
summ(mod_sn2,exp=T)
AIC(mod_sn2)#303.4

#variance inflation factor
vif(mod_sn2)#ok

#hosmer-lemeshow test
HLtest(mod_sn2,g=17)#H0 rejected

#Pearson's statistics gof test
rp2<-residuals(mod_sn2,type="pearson") 
pchisq(sum(rp2^2),266,lower.tail = F)#H0 rejected

sort(abs(rstudent(mod_sn2)))
#3 observation with high influence on the model
data_du[c("135","309","279"),]

rw2<-rstudent(mod_sn2)
eta2<-predict(mod_sn2)
gg2<-data.frame(rw2,eta2,data_sn$Collar)
levels(gg2$data_sn.Collar)<-c("green3", "orangered")

ggplot(gg2,aes(x=eta2,y=rw2))+geom_point(col=gg2$data_sn.Collar)+theme_bw()+
  geom_smooth(col="blue4")+labs(y="Studentized residuals",x="Linear predictor")+ theme(
    axis.title.x = element_text(color = "black", size = 16, face = "bold"),
    axis.title.y = element_text(color = "black", size = 16, face = "bold"))
#dispersion is not constant and there is a systematic trend


outlierTest(mod_sn2)
#there are no outlier detected but we remove the 3 observations to see if there
#are improvements in the model

data_sn<-data_du[data_du$Collar=="soft"|data_du$Collar=="no collar",]
data_sn$Collar<-relevel(data_sn$Collar,ref="soft")
data_sn$Collar<-droplevels(data_sn$Collar)

data_sn<-data_sn[,-c(5:10)]

row.names.remove <- c("135","309","279")
data_sn[c("135","309","279"),]
data_sn<-data_sn[!(row.names(data_sn) %in% row.names.remove), ]

data_sn[,3]<-(data_sn[,3]-mean(data_sn[,3]))/sqrt(var(data_sn[,3]))
data_sn[,4]<-(data_sn[,4]-mean(data_sn[,4]))/sqrt(var(data_sn[,4]))

#full model estimation
mod_sn<-glm(Collar~.-ID - Age-Neurosurgery-`Catchment area 100.000-500.000`
            -`'other' hospital`-`Northern Italy`-`Public hospital`
            -`Elective pathology`-`1 level discectomy and anterior arthrodesis with plate surgery`
            ,family="binomial",data=data_sn)

summary(mod_sn)
AIC(mod_sn)#283.6

#stepwise selection
mod_sn2<-stepAIC(mod_sn,direction="both")
summary(mod_sn2)
summ(mod_sn2,exp=T)
texreg(mod_sn2,single.row = T,fontsize="tiny",stars = c(0.01, 0.05, 0.1))

AIC(mod_sn2)#270.9

#hosmer lemeshow test
HLtest(mod_sn2,g=19)#ok

#Pearson's statistics gof test
rp3<-residuals(mod_sn2,type="pearson") 
pchisq(sum(rp3^2),261,lower.tail = F)#ok

eta3<-predict(mod_sn2)
rw3<-rstudent(mod_sn2)
gg3<-data.frame(rw3,eta3,data_sn$Collar)
levels(gg3$data_sn.Collar)<-c("green3", "orangered")

ggplot(gg3,aes(x=eta3,y=rw3))+geom_point(col=gg3$data_sn.Collar)+theme_bw()+
  geom_smooth(col="blue4")+labs(y="Studentized residuals",x="Linear predictor")+ theme(
    axis.title.x = element_text(color = "black", size = 16, face = "bold"),
    axis.title.y = element_text(color = "black", size = 16, face = "bold"))
#definitely a better dispersion


outlierTest(mod_sn2)
#no outliers

################################################################################
