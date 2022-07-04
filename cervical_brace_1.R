#libraries

library("plotrix")
library("ggplot2")
library("tidyverse")
library("naniar")
library("visdat")
library("ggplot2")
library("corrplot")
library("gridExtra")
library("grid")
library("readr")
library("dplyr")
library("tidyr")

#import dataset 'Cervical_brace_1' through 'import dataset' and convert it into a df

data<-as.data.frame(Cervical_brace_1)
str(data)

####   DATA PREPARATION

#exploring missing values

data2<-data
colnames(data2)<-c(1:104)
vis_dat(data2[,1:52])
vis_dat(data2[,53:104])

#removing 2 obs related to surgeons outside Italy and 1 full missing obs

data<-data[-c(11,54),]#!
data<-data[complete.cases(data$Specializzazione),]

#fixing ID number and row names

data$ID[1:61]<-seq(1,61,1)
rownames(data)<-seq(1,61,1)

#missing value imputation (information from internet)
data$`Tipologia di ospedale`[53]<-"Altro"

#missing value imputation (taken from an other observation that refers to the same hospital)
data$`Bacino di utenza afferente all'Ospedale`[41]<-"100.000-500.000"

#converting variables into factor 
data$`Tipologia di ospedale`<-as.factor(data$`Tipologia di ospedale`)
data$`Ospedale di attività`<-as.factor(data$`Ospedale di attività`)
data$`Principale patologia trattata`<-as.factor(data$`Principale patologia trattata`)
data$`Bacino di utenza afferente all'Ospedale`<-as.factor(data$`Bacino di utenza afferente all'Ospedale`)
data$Specializzazione<-as.factor(data$Specializzazione)

#fixing wrong answers in the year of birth variable
data$`Anno di nascita`
lung<-vector()
for(i in 1:dim(data)[1]){
  lung[i]<-nchar(data$`Anno di nascita`[i])
}

wrong_date<-which(lung!=4)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
data$`Anno di nascita`[wrong_date]<-as.numeric(substrRight(data$`Anno di nascita`[wrong_date],4))
data$`Anno di nascita`<-as.numeric(data$`Anno di nascita`)

#fixing wrong answers in the year of experience variable
data$`Anni di esperienza in chirurgia spinale`<-as.numeric(substring(data$`Anni di esperienza in chirurgia spinale`,first=1,last=2))

#adjusting other values
data$`Regione di attività`<-tolower(data$`Regione di attività`)
unique(data$`Regione di attività`)
data$`Regione di attività`[data$`Regione di attività`=="campani"]<-"campania"
data$`Regione di attività`[data$`Regione di attività`=="emilia"]<-"emilia romagna"
data$`Regione di attività`[data$`Regione di attività`=="sicilia calabria"]<-"sicilia"

data$`Ospedale di attività`[data$`Ospedale di attività`=="Convezionato"]<-"Convenzionato"

data$`Su un totale ipotetico di 10 interventi, quanti sono svolti con approccio posteriore?`[is.na(data$`Su un totale ipotetico di 10 interventi, quanti sono svolti con approccio posteriore?`)]<-0

names <- c(14:104)
data[,names]<-lapply(data[,names],tolower)
data[,names] <- lapply(data[,names] , factor)

data$`Bacino di utenza afferente all'Ospedale`[data$`Bacino di utenza afferente all'Ospedale` ==">1000000"]<-">1.000.000"
data$`Bacino di utenza afferente all'Ospedale`[data$`Bacino di utenza afferente all'Ospedale` =="500.000-1000.000"]<-"500.000-1.000.000"
data$`Bacino di utenza afferente all'Ospedale`<-droplevels(data$`Bacino di utenza afferente all'Ospedale`)
data$`Ospedale di attività`<-droplevels(data$`Ospedale di attività`)


#input missing values (amount of surgeries per year) through conditioned mean
#to catchment area of the hospital

data$`Numero di interventi cervicali annui`<-as.numeric(parse_number(data$`Numero di interventi cervicali annui`))

missing<-which(is.na(data$`Numero di interventi cervicali annui`))

tib<-as_tibble(data)
gru<-group_by(tib,`Bacino di utenza afferente all'Ospedale`)
dplyr::summarize(gru,n=n(),aver=mean(`Numero di interventi cervicali annui`,na.rm=T))

data[missing,c(9:10)]
data$`Numero di interventi cervicali annui`[c(6,43,53)]<-63
data$`Numero di interventi cervicali annui`[c(18,28)]<-73



#####  EXPLORATORY ANALYSIS


prop.table(table(data$Specializzazione))
#Specialisation: Neurosurgery 95.08%, Orthopaedic 4.92%

prop.table(table(data$`Tipologia di ospedale`))
#Hospital type: Polyclinic 27.87%, University 26.23% Other 45.90%

prop.table(table(data$`Ospedale di attività`))
#Hospital: Public 73.77%, Affiliated 26.23%

prop.table(table(data$`Bacino di utenza afferente all'Ospedale`))
#Catchment area <100.000 6.56%, 100.000-500.000 40.98%, 500.000-1.000.000 31.15%,
#> 1.000.000 21.31%

prop.table(table(data$`Principale patologia trattata`))
#Main pathology: Elective 85.25%, Traumatic 14.75%


#converting the 20 regions into 3 zones (North, Center and South)

colnames(data)[8]<-"Zona"
data$Zona
data$Zona[c(4,5,8,10,12,13,16,17,18,20,23,24,25,27,29,34,37,41,44,45,46,47,50,51,53,55,56,57,58,59,60)]<-"Nord"
data$Zona[c(3,7,11,30,32,35,36,49,52,54,61)]<-"Centro"
data$Zona[c(1,2,6,9,14,15,19,21,22,26,28,31,33,38,39,40,42,43,48)]<-"Sud e isole"
data$Zona<-as.factor(data$Zona)
prop.table(table(data$Zona))
#Northern Italy 50.82%, Central Italy 18.03%, Southern Italy and Islands 31.14%

#some plots for numerical variables

p1 <- ggplot(data, aes_string(data$`Anni di esperienza in chirurgia spinale`)) +
  geom_boxplot(fill ="#FF6666",outlier.colour = "blue", outlier.shape = 1) + coord_flip()+
  labs(x="Experience in cervical spine surgery")
p2 <- ggplot(data , aes_string(data$`Anno di nascita`)) + 
  geom_boxplot(fill="dodgerblue",outlier.colour = "blue", outlier.shape = 1) + coord_flip()+
  labs(x="Year of birth")
p3 <- ggplot(data, aes_string(data$`Numero di interventi cervicali annui`)) + 
  geom_boxplot(fill="green",outlier.colour = "blue", outlier.shape = 1) + coord_flip()+
  labs(x="Cervical surgeries per year")
boxes <- list(p1,p2,p3)
do.call(grid.arrange, c(boxes, nrow = 1))


gg1 <- ggplot(data, aes_string(x=data$`Anni di esperienza in chirurgia spinale`)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 10)+
  geom_density(alpha=.4, fill="#FF6666")+
  labs(x="Experience in cervical spine surgery")
gg2 <- ggplot(data, aes_string(x=data$`Anno di nascita`)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 10)+
  geom_density(alpha=.4, fill="dodgerblue")+
  labs(x="Year of birth")
gg3 <- ggplot(data, aes_string(x=data$`Numero di interventi cervicali annui`)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 20)+
  geom_density(alpha=.4, fill="green")+
  labs(x="Cervical surgeries per year")
hists <- list(gg1,gg2,gg3)
do.call(grid.arrange, c(hists, nrow = 1))

#foundations' importance

summary(data[,94:98])[2,]
c(24,53,39,26,20)/sum(c(24,53,39,26,20))
#Literature 14.81%, Personal experience 32.72%, Colleagues'teaching 24.07%, 
#Local customs  16.05%, Legal and medical protection 12.35%


#clinical features' importance rigid collar

summary(data[,99:101])[2,]
c(23,25,50)/sum(c(23,25,50))
#Advanced patient's age 23.47%, Previous operations in anamnesis 25.51%
#Poor bone quality 51.02%

#clinical features' importance soft collar

summary(data[,102:104])[2,]
c(23,7,7)/sum(c(23,7,7))

#Advanced patient's age 62.16%, Previous operations in anamnesis 18.92%,
#Poor bone quality 18.92%

#duration of prescriptions

data_dur<-data[,c(14,21,22,29,30,37,38,45,46,53,54,61,62,69,70,77,78,85,86,93)]
summary(data_dur)
levels(data_dur[,1])<-c("morbido","nessun collare","rigido")
dur<-as_tibble(data_dur)

colnames(dur)[c(seq(1,19,2))]<-c("1 level DAA without plate","2 level DAA without plate","1 level DAA with plate",
                                "DAA of more than 1 level with plate","Cervical laminectomy without stabilization",
                                "Cervical laminectomy with posterior stabilization","Open door laminoplasty" , 
                                "C1-C2 Stabilization" ,"Occipito-cervical fixation" , "Circumferential fixation")
colnames(dur)[c(seq(2,20,2))]<-sprintf("duration%s",seq(1:10))

#using gather function to get the long version of the dataset
dur_long<-tidyr::gather(dur,"1 level DAA without plate","2 level DAA without plate","1 level DAA with plate",
                  "DAA of more than 1 level with plate","Cervical laminectomy without stabilization",
                  "Cervical laminectomy with posterior stabilization","Open door laminoplasty" , 
                  "C1-C2 Stabilization" ,"Occipito-cervical fixation" , "Circumferential fixation",key="intervento",value="collare")

#some adjustments
levels(dur_long$duration1)<-c(levels(dur_long$duration1),"> 4 mesi")
dur_long$duration1[62:122]<-dur_long$duration2[62:122]
dur_long$duration1[123:183]<-dur_long$duration3[123:183]
dur_long$duration1[184:244]<-dur_long$duration4[184:244]
dur_long$duration1[245:305]<-dur_long$duration5[245:305]
dur_long$duration1[306:366]<-dur_long$duration6[306:366]
dur_long$duration1[367:427]<-dur_long$duration7[367:427]
dur_long$duration1[428:488]<-dur_long$duration8[428:488]
dur_long$duration1[489:549]<-dur_long$duration9[489:549]
dur_long$duration1[550:610]<-dur_long$duration10[550:610]


duration<-select(dur_long,-num_range("duration", 2:10))%>%
  filter(collare!="nessun collare")%>%
  group_by(intervento,collare,duration1)%>%
  dplyr::summarise(n=n())%>%filter(duration1!="NA")

#some transformations and translations
duration_df<-as.data.frame(duration)
colnames(duration_df)<-c("surgery","collar","duration","amount")
duration_df$collar<-as.factor(duration_df$collar)
levels(duration_df$collar)<-c("soft","rigid")
duration_df$duration<-as.factor(duration_df$duration)
levels(duration_df$duration)<-c("< 4 weeks", "2-4 months", "4-8 weeks"," > 4 months")

#we can explore the df to analyse the prescription after a specific surgery
duration_df

#for instance
duration_df[(duration_df$collar=="soft")&(duration_df$surgery=="1 level DAA without plate"),]


#compute the percentages to plot them

#soft
soft<-duration_df[(duration_df$collar=="soft"),]

#creating a tibble with all the possible combination of surgery and duration of prescriptions
surge<-c(rep("1 level DAA without plate" , 4) ,
         rep("2 level DAA without plate" , 4) ,
         rep("1 level DAA with plate" , 4) ,
         rep("DAA of more than 1 level with plate" , 4),
         rep("Cervical laminectomy without stabilization" , 4),
         rep("Cervical laminectomy with posterior stabilization" , 4),
         rep("Open door laminoplasty" , 4), rep("C1-C2 Stabilization" , 4),
         rep("Occipito-cervical fixation" , 4), rep("Circumferential fixation" , 4) )

dura<-factor(rep(c("< 4 weeks","4-8 weeks","2-4 months"," > 4 months"),10),
             levels=c("< 4 weeks","4-8 weeks","2-4 months"," > 4 months"))

tibb<-as_tibble(data.frame(surge,dura))
colnames(tibb)<-c("surgery","duration")

#joining it with the real dataset
soft_full<-left_join(tibb,soft)
soft_full$amount[is.na(soft_full$amount)]<-0


total_soft<-group_by(soft_full,surgery)%>%dplyr::summarize(sum(amount))
soft_def<-left_join(soft_full,total_soft)%>%arrange(surgery)
soft_def$percentage<-soft_def$amount/soft_def$`sum(amount)`

soft_def$surgery = str_wrap(soft_def$surgery, width = 15)

ggplot(soft_def[1:20,],aes(x=percentage,y=surgery,fill=duration))+geom_col(position = position_dodge2(preserve = "single"))+
  geom_text(aes(label = paste0(sprintf("%1.1f", percentage*100),"%")), position = position_dodge(0.9), hjust = 1, size = 4.5, color = "black")+
  labs(title="Soft brace prescriptions'duration",y="")+scale_fill_brewer(palette="BuPu")+theme_bw()


ggplot(soft_def[21:40,],aes(x=percentage,y=surgery,fill=duration))+geom_col(position = position_dodge2(preserve = "single"))+
  geom_text(aes(label = paste0(sprintf("%1.1f", percentage*100),"%")), position = position_dodge(0.9), hjust = 1, size = 4.5, color = "black")+
  labs(title="Soft brace prescriptions'duration",y="")+scale_fill_brewer(palette="BuPu")+theme_bw()


#rigid
rigid<-duration_df[(duration_df$collar=="rigid"),]

rigid_full<-left_join(tibb,rigid)
rigid_full$amount[is.na(rigid_full$amount)]<-0

total_rigid<-group_by(rigid_full,surgery)%>%dplyr::summarize(sum(amount))
rigid_def<-left_join(rigid_full,total_rigid)%>%arrange(surgery)
rigid_def$percentage<-rigid_def$amount/rigid_def$`sum(amount)`

rigid_def$surgery = str_wrap(rigid_def$surgery, width = 15)

ggplot(rigid_def[1:20,],aes(x=percentage,y=surgery,fill=duration))+geom_col(position = position_dodge2(preserve = "single"))+
  geom_text(aes(label = paste0(sprintf("%1.1f", percentage*100),"%")), position = position_dodge(0.9), hjust = 1, size = 4.5, color = "black")+
  labs(title="Rigid brace prescriptions'duration",y="")+scale_fill_brewer(palette="YlOrRd")+theme_bw()


ggplot(rigid_def[21:40,],aes(x=percentage,y=surgery,fill=duration))+geom_col(position = position_dodge2(preserve = "single"))+
  geom_text(aes(label = paste0(sprintf("%1.1f", percentage*100),"%")), position = position_dodge(0.9), hjust = 1, size = 4.5, color = "black")+
  labs(title="Rigid brace prescriptions'duration",y="")+scale_fill_brewer(palette="YlOrRd")+theme_bw()


###############################################################################
