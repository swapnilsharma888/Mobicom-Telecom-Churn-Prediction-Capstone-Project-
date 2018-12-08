getwd()
setwd("C:\\DS Full stack\\Capstone ")
telecom<-read.csv("telecomfinal.csv",header = T, na.string=c("","-","NA"))
library(dplyr)
library(irr)
library(rpart)
library(gains)
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
names(telecom)
summary(telecom)
str(telecom)

# Create data quality report
library(dataQualityR)
num.file<-paste(tempdir(),"/dq_num.csv",sep="")
cat.file<-paste(tempdir(),"/dq_cat.csv",sep="")
checkDataQuality(data = telecom,out.file.num = num.file, out.file.cat = cat.file,numeric.cutoff = 10)
dq_num<-read.csv(num.file)
dq_cat<-read.csv(cat.file)

# Omit variables with high missing values.
telecom<-select(telecom, -c(retdays,income,dwlltype,dwllsize,mailordr,occu1,numbcars,wrkwoman,
                 solflag,proptype,mailresp,cartype,children,div_type))
names(telecom)
colSums(is.na(telecom))

#Missing value treatment
plot(telecom$mou_Mean,telecom$mou_Range)
a<-which(is.na(telecom$mou_Mean))
b<-telecom[a,]
summary(b) # Most of the missing values of variables from these 181 rows only. Remove these 181 rows.
telecom1<-telecom[-a,]
summary(telecom1)

p<-which(is.na(telecom1$avg6mou))  # have all 2037 missing values of both "avg6mou" and "avg6qty"
q<-telecom1[p,]
summary(q)

plot(telecom1$avg6mou,telecom1$avg3mou)  # linear relationship
plot(telecom1$avg6qty,telecom1$avg3qty)  # linear relationship
summary(telecom1)


# missing value of "avg6mou" is replaces with values of avg3mou" for the same row as linear relationship 
telecom1$avg6mou[is.na(telecom1$avg6mou)]<-telecom1$avg3mou[is.na(telecom1$avg6mou)]
telecom1$avg6qty[is.na(telecom1$avg6qty)]<-telecom1$avg3qty[is.na(telecom1$avg6qty)]


# missing value treatment for "hnd_price" and "hnd_webcap"
# both of these variables dont have common rows for missing values.
# first impute missing value of "hnd_price" and then for "hnd_webcap"
names(telecom1)
plot(telecom1$hnd_price,telecom1$hnd_webcap)
table(telecom1$hnd_price,telecom1$hnd_webcap)
         # 40 is the cutoff hnd_price for WC OR WCMB ( CHECK FROM TABLE)
m1<-mean(telecom1$hnd_price[telecom1$hnd_price<40],na.rm = T)
m2<-mean(telecom1$hnd_price[telecom1$hnd_price>40],na.rm = T)
m3<-mean(telecom1$hnd_price,na.rm = T)

u<-which(is.na(telecom1$hnd_price))
v<-telecom1[u,]
telecom1<-telecom1[-u,]
v$hnd_price<-ifelse(v$hnd_webcap=="WC",m1,ifelse(v$hnd_webcap=="WCMB",m2,m3))
sum(is.na(v$hnd_price))
telecom1<-rbind(telecom1,v)

unique((telecom1$hnd_webcap))
u1<-which(is.na(telecom1$hnd_webcap))
v1<-telecom1[u1,]
telecom1<-telecom1[-u1,]
v1$hnd_webcap<-if_else(v1$hnd_price<=40,"WC","WCMB")
table(v1$hnd_price,v1$hnd_webcap)
telecom1<-rbind(telecom1,v1)

# Remove 1 row for common missing value from "hnd_price" and "hnd_webcap"
telecom2<-telecom1[-which(is.na(telecom1$hnd_price)),]
colSums(is.na(telecom2))


#Treating missing values in "prizm_social_one"
summary(telecom2$prizm_social_one)
telecom2$prizm_social_one<-ifelse(is.na(telecom2$prizm_social_one),"missing",as.character(telecom2$prizm_social_one))
class(telecom2$prizm_social_one)
telecom2$prizm_social_one<-as.factor(telecom2$prizm_social_one)



#Missing value treatment for "Change_mou". this variable have no relationship with any variable( checked with plot)
telecom2$change_mou[is.na(telecom2$change_mou)]<-mean(telecom2$change_mou,na.rm = T)
colSums(is.na(telecom2))


# Missing value treatment for "csa" and "area"
telecom2$csa<-ifelse(is.na(telecom2$csa),"missing",as.character(telecom2$csa))
telecom2$csa<-as.factor(telecom2$csa)
telecom2$area<-ifelse(is.na(telecom2$area),"missing",as.character(telecom2$area))
telecom2$area<-as.factor(telecom2$area)
colSums(is.na(telecom2))


# Remove missing values for maritial and other variables( all variables have common missing value rows)
telecom2<-telecom2[-(which(is.na(telecom2$marital))),]
colSums(is.na(telecom2))
names(telecom2)
telecom_3<-telecom2

#Binning of Categorical variable(csa). "csa" has so many levels. covert to 4 levels depending on event rate.
telecom_3%>%count(churn,levels=csa)%>%filter(churn==1)->datC1
datC1$N<-unclass(telecom_3%>%filter(csa%in%datC1$levels)%>%count(csa))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("csa",nrow(datC1))

colSums(is.na(datC1))
summary(datC1$ChurnPerc)
datC1$quant<-ntile(datC1$ChurnPerc,4)
datC1<-datC1[,c(2,7)]
names(datC1)<-c("csa","quat_csa")
length(unique(datC1$csa))
length(unique(telecom_3$csa))
telecom_3<-merge(telecom_3,datC1,by="csa",all.x = T)
sum(is.na(telecom_3$quat_csa))
telecom_3$quat_csa<-ifelse(is.na(telecom_3$quat_csa),"1", as.character(telecom_3$quat_csa))
telecom_3$quat_csa<-as.factor(telecom_3$quat_csa)
telecom_3$csa<-telecom_3$quat_csa
class(telecom_3$csa)
telecom_3<-telecom_3[,-68]


# Binning of "area" but we cant do as all level event rate ( churn ratio) is very close to each other.
telecom_3%>%count(churn,levels=area)%>%filter(churn==1)->datC2
datC2$N<-unclass(telecom_3%>%filter(area%in%datC2$levels)%>%count(area))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("area",nrow(datC2))


# Binning of "crclscod". covert to 4 levels depending on event rate.
telecom_3%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC3
datC3$N<-unclass(telecom_3%>%filter(crclscod%in%datC3$levels)%>%count(crclscod))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("crclscod",nrow(datC3))

colSums(is.na(datC3))
summary(datC3$ChurnPerc)
datC3$quant<-ntile(datC3$ChurnPerc,4)
datC3<-datC3[,c(2,7)]
names(datC3)<-c("crclscod","quat_crclscod")
length(unique(datC3$crclscod))
length(unique(telecom_3$crclscod))
telecom_3<-merge(telecom_3,datC3,by="crclscod",all.x = T)
sum(is.na(telecom_3$quat_crclscod))
telecom_3$quat_crclscod<-ifelse(is.na(telecom_3$quat_crclscod),"1", as.character(telecom_3$quat_crclscod))
telecom_3$quat_crclscod<-as.factor(telecom_3$quat_crclscod)
telecom_3$crclscod<-telecom_3$quat_crclscod
class(telecom_3$crclscod)
telecom_3<-telecom_3[,-68]
sum(is.na(telecom_3))
str(telecom_3)

# Binning of "ethnic". covert to 4 levels depending on event rate.
telecom_3%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC4
datC4$N<-unclass(telecom_3%>%filter(ethnic%in%datC4$levels)%>%count(ethnic))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N
datC4$Var.Name<-rep("ethnic",nrow(datC4))

colSums(is.na(datC4))
summary(datC4$ChurnPerc)
datC4$quant<-ntile(datC4$ChurnPerc,4)
datC4<-datC4[,c(2,7)]
names(datC4)<-c("ethnic","quat_ethnic")
length(unique(datC4$ethnic))
length(unique(telecom_3$ethnic))
telecom_3<-merge(telecom_3,datC4,by="ethnic",all.x = T)
sum(is.na(telecom_3$quat_ethnic))
telecom_3$quat_ethnic<-as.factor(telecom_3$quat_ethnic)
telecom_3$ethnic<-telecom_3$quat_ethnic
class(telecom_3$ethnic)
telecom_3<-telecom_3[,-68]
sum(is.na(telecom_3))

#outlier Treatment ( only one row we need to delete which is outlier for most of the variables)
telecom_3<-telecom_3[-which(telecom_3$mou_Mean>12000),]
telecom_churn<-telecom_3
str(telecom_churn)
summary(telecom_churn)


#Dummy Variable Creation
telecom_churn$prism_social_dummy_R<-ifelse(telecom_churn$prizm_social_one=="R",1,0)
telecom_churn$prism_social_dummy_T<-ifelse(telecom_churn$prizm_social_one=="T",1,0)

telecom_churn$area_dummy_HOUSTON<-ifelse(telecom_churn$area=="HOUSTON AREA",1,0)
telecom_churn$area_dummy_NORTHWEST<-ifelse(telecom_churn$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)

telecom_churn$hnd_webcap_dummy_WC<-ifelse(telecom_churn$hnd_webcap=="WC",1,0)

# Split Data into Test and Train

set.seed(300)
index<-sample(nrow(telecom_churn),0.7*nrow(telecom_churn),replace = F)
telecom_train<-telecom_churn[index,]
telecom_test<-telecom_churn[-index,]

mod1<-glm(churn~.,data=telecom_train[,-65],family = "binomial")

mod2<-glm(churn~ethnic+crclscod+csa+totmrc_Mean+rev_Range +mou_Range+change_mou+owylis_vce_Range+months+totcalls+eqpdays+
            adjqty +rev_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+asl_flag+prism_social_dummy_R+
           prism_social_dummy_T+area_dummy_HOUSTON+area_dummy_NORTHWEST+refurb_new+hnd_webcap_dummy_WC+
           age1+age2+models+hnd_price+actvsubs+uniqsubs+mou_pead_Mean+drop_dat_Mean+drop_vce_Mean+adjmou
            ,data=telecom_train[,-65],family = "binomial")


mod3<-glm(churn~ethnic+crclscod+csa+totmrc_Mean+rev_Range +mou_Range+change_mou+owylis_vce_Range+months+eqpdays+
            rev_Mean+comp_vce_Mean+avg3mou+avgmou+asl_flag+prism_social_dummy_R+
            prism_social_dummy_T+area_dummy_HOUSTON+refurb_new+
            age1+models+hnd_price+actvsubs+uniqsubs+drop_vce_Mean
          ,data=telecom_train[,-65],family = "binomial")
summary(mod3)
mod3$coefficients       # to check variable importance ( higher cofficient is more important)

#step(mod2,direction = "both")

#mod4<-glm(churn~ethnic + crclscod + csa + totmrc_Mean + rev_Range + mou_Range + 
#            change_mou + owylis_vce_Range + months + totcalls + eqpdays + 
#            adjqty + rev_Mean + comp_vce_Mean + plcd_vce_Mean + avg3mou + 
#            avgmou + asl_flag + prism_social_dummy_R + prism_social_dummy_T + 
#            area_dummy_HOUSTON + refurb_new + age1 + age2 + models + 
#            hnd_price + actvsubs + uniqsubs + drop_vce_Mean + adjmou
#          ,data=telecom_train[,-65],family = "binomial")


# We will take "mod3" as the final model

pred<-predict(mod3,type = "response",newdata = telecom_test)
table(telecom_churn$churn)
table(telecom_churn$churn)/nrow(telecom_churn)
pred<-ifelse(pred>=0.2392,1,0)

confusionMatrix(pred,telecom_test$churn,positive = "1")
# Accuracy is 60.1 %

# Model Validation with the help of ROCR curve

library(ROCR)
pred<-predict(mod3,type = "response",newdata = telecom_test)
pred<-prediction(pred,telecom_test$churn)
roc<-performance(pred,"tpr","fpr")
plot(roc)
abline(a=0,b=1)
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
# Result of AUC is 0.6380

gains(telecom_test$churn,predict(mod3,type = "response",newdata = telecom_test),groups = 10)


# Selection of top 20% customer will cover 31.3% customers who are likely to get churned
telecom_test$prob<-predict(mod3,type = "response",newdata = telecom_test)
summary(telecom_test$prob)
quantile(telecom_test$prob,p=(1:10)/10)

names(telecom_test)
Targeted<-telecom_test[telecom_test$prob>0.3095 & telecom_test$prob<=0.8116,"Customer_ID"]
Targeted

