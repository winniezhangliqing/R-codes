rm(list=ls())

##main analtsis
library(survival)
library(dplyr)
library(tidyverse)


data.imput.done <- read.csv("D:/UKB/data/data.impute.done.csv")
colnames(data.imput.done)
data_main <- data.imput.done
colnames(data_main)
summary(data_main)


#exposure processing-----------------------------------------------------------

#total-stpes
quantile(data_main$total_steps,c(0.333333,0.66666666,1))

data_main$total_steps_3gp[data_main$total_steps<= 8861.329   ] <- 1
data_main$total_steps_3gp[data_main$total_steps> 8861.329   &  data_main$total_steps<=11616.429      ] <- 2
data_main$total_steps_3gp[data_main$total_steps> 11616.429  ] <- 3

#incidental-steps
quantile(data_main$incidental_steps,c(0.333333,0.66666666,1))

data_main$incidental_steps_3gp[data_main$incidental_steps<= 5505.474   ] <- 1
data_main$incidental_steps_3gp[data_main$incidental_steps> 5505.474   &  data_main$incidental_steps<=7043.195      ] <- 2
data_main$incidental_steps_3gp[data_main$incidental_steps> 7043.195  ] <- 3

#purposeful-steps
quantile(data_main$purposeful_steps,c(0.333333,0.66666666,1))

data_main$purposeful_steps_3gp[data_main$purposeful_steps<= 2833.931   ] <- 1
data_main$purposeful_steps_3gp[data_main$purposeful_steps> 2833.931   &  data_main$purposeful_steps<=4652.209      ] <- 2
data_main$purposeful_steps_3gp[data_main$purposeful_steps> 4652.209  ] <- 3

#peak30-steps
quantile(data_main$peak30,c(0.333333,0.66666666,1))

data_main$peak30_3gp[data_main$peak30<= 102.7000   ] <- 1
data_main$peak30_3gp[data_main$peak30> 102.7000   &  data_main$peak30<=112.4000      ] <- 2
data_main$peak30_3gp[data_main$peak30> 112.4000  ] <- 3



##function---model1-totalsteps
model1<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  x<-as.factor(x)   
  cox1 <- coxph(Surv(ti,out) ~x + age.im + sex.im , data=dataname)   
  cox2 <- coxph(Surv(ti,out) ~x + age.im + sex.im + centre.im + wear_duration.im + Wear_season.im + Employment.im +        
                  ethnic.im + education.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im, data=dataname)
  cox3 <- coxph(Surv(ti,out) ~x + age.im  + sex.im +centre.im + wear_duration.im + Wear_season.im + Employment.im +        
                  +ethnic.im + education.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im  + HTN.im + diabetes.im + 
                   PM2.5.im + LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im, data=dataname)
  level <- c("Low","Moderate","High")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  d <- cbind("HR" = summary(cox3)$conf.int[1:z,c(1)],
             "low" = summary(cox3)$conf.int[1:z,c(3)],
             "up" = summary(cox3)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox3)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  d <- rbind(reference,d)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- cbind(d,level, "exposure" = rep(exposure), "model" = rep("model3"))
  c <- rbind(a,b,d)
  result<-as.data.frame(c)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.3f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)
}


##function---model2-incidental-steps
model2<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  x<-as.factor(x)   
  cox1 <- coxph(Surv(ti,out) ~x + age.im + sex.im , data=dataname)  
  cox2 <- coxph(Surv(ti,out) ~x + age.im + sex.im + centre.im + wear_duration.im + Wear_season.im + Employment.im +      
                  ethnic.im + education.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im, data=dataname)
  cox3 <- coxph(Surv(ti,out) ~x + age.im  + sex.im +centre.im + wear_duration.im + Wear_season.im + Employment.im +      
                  +ethnic.im + education.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im  + HTN.im + diabetes.im + 
                  PM2.5.im + LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im + purposeful_steps, data=dataname)
  level <- c("Low","Moderate","High")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  d <- cbind("HR" = summary(cox3)$conf.int[1:z,c(1)],
             "low" = summary(cox3)$conf.int[1:z,c(3)],
             "up" = summary(cox3)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox3)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  d <- rbind(reference,d)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- cbind(d,level, "exposure" = rep(exposure), "model" = rep("model3"))
  c <- rbind(a,b,d)
  result<-as.data.frame(c)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.3f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)
}

##function---model3-purposeful-steps
model3<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  x<-as.factor(x)   
  cox1 <- coxph(Surv(ti,out) ~x + age.im + sex.im , data=dataname)   
  cox2 <- coxph(Surv(ti,out) ~x + age.im + sex.im + centre.im + wear_duration.im + Wear_season.im + Employment.im +        
                  ethnic.im + education.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im, data=dataname)
  cox3 <- coxph(Surv(ti,out) ~x + age.im  + sex.im +centre.im + wear_duration.im + Wear_season.im + Employment.im +       
                  +ethnic.im + education.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im  + HTN.im + diabetes.im + 
                  PM2.5.im + LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im + incidental_steps, data=dataname)
  level <- c("Low","Moderate","High")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  d <- cbind("HR" = summary(cox3)$conf.int[1:z,c(1)],
             "low" = summary(cox3)$conf.int[1:z,c(3)],
             "up" = summary(cox3)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox3)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  d <- rbind(reference,d)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- cbind(d,level, "exposure" = rep(exposure), "model" = rep("model3"))
  c <- rbind(a,b,d)
  result<-as.data.frame(c)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.3f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)
}

##function---model4-steps-intensity peank 30
model4<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  x<-as.factor(x)   
  cox1 <- coxph(Surv(ti,out) ~x + age.im + sex.im , data=dataname)   
  cox2 <- coxph(Surv(ti,out) ~x + age.im + sex.im + centre.im + wear_duration.im + Wear_season.im + Employment.im +       
                  ethnic.im + education.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im, data=dataname)
  cox3 <- coxph(Surv(ti,out) ~x + age.im  + sex.im +centre.im + wear_duration.im + Wear_season.im + Employment.im +        
                  +ethnic.im + education.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im  + HTN.im + diabetes.im + 
                  PM2.5.im + LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im + total_steps, data=dataname)
  level <- c("Low","Moderate","High")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  d <- cbind("HR" = summary(cox3)$conf.int[1:z,c(1)],
             "low" = summary(cox3)$conf.int[1:z,c(3)],
             "up" = summary(cox3)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox3)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  d <- rbind(reference,d)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- cbind(d,level, "exposure" = rep(exposure), "model" = rep("model3"))
  c <- rbind(a,b,d)
  result<-as.data.frame(c)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.3f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)
}

# main analysis------------------------------------------------------------
#Incident 11outcomes-total steps
total_steps_model<-model1(data_main$total_steps_3gp,"total_steps  ~incident_11outcomes",data_main$FUduration.Acti_11outcomes.HDC.ICD10,data_main$Incidental.Acti_11outcomes.HDC.ICD10 ,data_main)

#Incident depression-incidental steps

incidental_steps_model<-model2(data_main$incidental_steps_3gp,"Incidental_steps  ~incident_11outcomes",data_main$FUduration.Acti_11outcomes.HDC.ICD10,data_main$Incidental.Acti_11outcomes.HDC.ICD10 ,data_main)

#Incident depression-purposal steps
purposeful_steps_model<-model3(data_main$purposeful_steps_3gp,"purposeful_steps  ~incident_11outcomes",data_main$FUduration.Acti_11outcomes.HDC.ICD10,data_main$Incidental.Acti_11outcomes.HDC.ICD10 ,data_main)

#incident depression-peak30
Peak30_model<-model4(data_main$peak30_3gp,"peak30  ~incident_11outcomes",data_main$FUduration.Acti_11outcomes.HDC.ICD10,data_main$Incidental.Acti_11outcomes.HDC.ICD10 ,data_main)


z <- rbind(total_steps_model,incidental_steps_model,purposeful_steps_model,Peak30_model)
write.csv(z,"D:/UKB/data/step/main_cox_result_11outcomes.csv")



# main analysis------------------------------------------------------------
#Incident 6outcomes-total steps
total_steps_model<-model1(data_main$total_steps_3gp,"total_steps  ~incident_6outcomes",data_main$FUduration.Acti_6outcomes.HDC.ICD10,data_main$Incidental.Acti_6outcomes.HDC.ICD10 ,data_main)


incidental_steps_model<-model2(data_main$incidental_steps_3gp,"Incidental_steps  ~incident_6outcomes",data_main$FUduration.Acti_6outcomes.HDC.ICD10,data_main$Incidental.Acti_6outcomes.HDC.ICD10 ,data_main)


purposeful_steps_model<-model3(data_main$purposeful_steps_3gp,"purposeful_steps  ~incident_6outcomes",data_main$FUduration.Acti_6outcomes.HDC.ICD10,data_main$Incidental.Acti_6outcomes.HDC.ICD10 ,data_main)


Peak30_model<-model4(data_main$peak30_3gp,"peak30  ~incident_6outcomes",data_main$FUduration.Acti_6outcomes.HDC.ICD10,data_main$Incidental.Acti_6outcomes.HDC.ICD10 ,data_main)

z <- rbind(total_steps_model,incidental_steps_model,purposeful_steps_model,Peak30_model)
write.csv(z,"D:/UKB/data/step/main_cox_result_6outcomes.csv")




# main analysis------------------------------------------------------------
#Incident depression-total steps
total_steps_model<-model1(data_main$total_steps_3gp,"total_steps  ~incident_depression",data_main$FUduration.acti.Depression.HDC.ICD10,data_main$Incidental.acti.Depression.HDC.ICD10 ,data_main)


incidental_steps_model<-model2(data_main$incidental_steps_3gp,"Incidental_steps  ~incident_depression",data_main$FUduration.acti.Depression.HDC.ICD10,data_main$Incidental.acti.Depression.HDC.ICD10 ,data_main)


purposeful_steps_model<-model3(data_main$purposeful_steps_3gp,"purposeful_steps  ~incident_depression",data_main$FUduration.acti.Depression.HDC.ICD10,data_main$Incidental.acti.Depression.HDC.ICD10 ,data_main)


Peak30_model<-model4(data_main$peak30_3gp,"peak30  ~incident_depression",data_main$FUduration.acti.Depression.HDC.ICD10,data_main$Incidental.acti.Depression.HDC.ICD10 ,data_main)


z <- rbind(total_steps_model,incidental_steps_model,purposeful_steps_model,Peak30_model)
write.csv(z,"D:/UKB/data/step/main_cox_result_depression.csv")





# main analysis------------------------------------------------------------
#Incident anxiety disorders -total steps
total_steps_model<-model1(data_main$total_steps_3gp,"total_steps  ~incident_anxiety disorders",data_main$FUduration.anxiety.HDC.ICD10, data_main$Incidental.anxiety.HDC.ICD10, data_main)

#Incident anxiety disorders -incidental steps

incidental_steps_model<-model2(data_main$incidental_steps_3gp,"Incidental_steps  ~incident_anxiety disorders",data_main$FUduration.anxiety.HDC.ICD10, data_main$Incidental.anxiety.HDC.ICD10, data_main)

#Incident anxiety disorders -purposal steps
purposeful_steps_model<-model3(data_main$purposeful_steps_3gp,"purposeful_steps  ~incident_anxiety disorders",data_main$FUduration.anxiety.HDC.ICD10, data_main$Incidental.anxiety.HDC.ICD10, data_main)

#Incident anxiety disorders -peak30
Peak30_model<-model4(data_main$peak30_3gp,"peak30  ~incident_anxiety disorders",data_main$FUduration.anxiety.HDC.ICD10, data_main$Incidental.anxiety.HDC.ICD10, data_main)


z <- rbind(total_steps_model,incidental_steps_model,purposeful_steps_model,Peak30_model)
write.csv(z,"D:/UKB/data/step/main_cox_result_anxiety.csv")




# main analysis------------------------------------------------------------
#substance use disorders use disorders -total steps
total_steps_model<-model1(data_main$total_steps_3gp,"total_steps  ~incident_substance use disorders",data_main$FUduration.SubstanceUse.HDC.ICD10, data_main$Incidental.SubstanceUse.HDC.ICD10, data_main)

#substance use disorders use disorders -incidental steps

incidental_steps_model<-model2(data_main$incidental_steps_3gp,"Incidental_steps  ~incident_substance use disorders",data_main$FUduration.SubstanceUse.HDC.ICD10, data_main$Incidental.SubstanceUse.HDC.ICD10, data_main)

#substance use disorders use disorders -purposal steps
purposeful_steps_model<-model3(data_main$purposeful_steps_3gp,"purposeful_steps  ~incident_substance use disorders",data_main$FUduration.SubstanceUse.HDC.ICD10, data_main$Incidental.SubstanceUse.HDC.ICD10, data_main)

#substance use disorders use disorders -peak30
Peak30_model<-model4(data_main$peak30_3gp,"peak30  ~incident_substance use disorders",data_main$FUduration.SubstanceUse.HDC.ICD10, data_main$Incidental.SubstanceUse.HDC.ICD10, data_main)


z <- rbind(total_steps_model,incidental_steps_model,purposeful_steps_model,Peak30_model)
write.csv(z,"D:/UKB/data/step/main_cox_result_substance use.csv")




# main analysis------------------------------------------------------------
#sleep disorders -total steps
total_steps_model<-model1(data_main$total_steps_3gp,"total_steps  ~incident_Sleep disorders",data_main$FUduration.sleepdisord.HDC.ICD10, data_main$Incidental.sleepdisord.HDC.ICD10, data_main)


incidental_steps_model<-model2(data_main$incidental_steps_3gp,"Incidental_steps  ~incident_Sleep disorders",data_main$FUduration.sleepdisord.HDC.ICD10, data_main$Incidental.sleepdisord.HDC.ICD10, data_main)


purposeful_steps_model<-model3(data_main$purposeful_steps_3gp,"purposeful_steps  ~incident_Sleep disorders",data_main$FUduration.sleepdisord.HDC.ICD10, data_main$Incidental.sleepdisord.HDC.ICD10, data_main)


Peak30_model<-model4(data_main$peak30_3gp,"peak30  ~incident_Sleep disorders",data_main$FUduration.sleepdisord.HDC.ICD10, data_main$Incidental.sleepdisord.HDC.ICD10, data_main)


z <- rbind(total_steps_model,incidental_steps_model,purposeful_steps_model,Peak30_model)
write.csv(z,"D:/UKB/data/step/main_cox_result_Sleepdisord.csv")



# main analysis------------------------------------------------------------
#dementia-total steps
total_steps_model<-model1(data_main$total_steps_3gp,"total_steps  ~incident_dementia",data_main$FUduration.dementia.HDC.ICD10, data_main$Incidental.dementia.HDC.ICD10, data_main)


incidental_steps_model<-model2(data_main$incidental_steps_3gp,"Incidental_steps  ~incident_dementia",data_main$FUduration.dementia.HDC.ICD10, data_main$Incidental.dementia.HDC.ICD10, data_main)


purposeful_steps_model<-model3(data_main$purposeful_steps_3gp,"purposeful_steps  ~incident_dementia",data_main$FUduration.dementia.HDC.ICD10, data_main$Incidental.dementia.HDC.ICD10, data_main)


Peak30_model<-model4(data_main$peak30_3gp,"peak30  ~incident_dementia",data_main$FUduration.dementia.HDC.ICD10, data_main$Incidental.dementia.HDC.ICD10, data_main)


z <- rbind(total_steps_model,incidental_steps_model,purposeful_steps_model,Peak30_model)
write.csv(z,"D:/UKB/data/step/main_cox_result_dementia.csv")





