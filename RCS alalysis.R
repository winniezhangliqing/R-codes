rm(list=ls())
data_main <- read.csv("D:/UKB/data/data.impute.done.csv")

library(rms)
library(ggplot2)
library(ggpubr)
library(scales)


colnames(data_main)
#########################################incident-11outcomes-total-steps#####################################
dd <-datadist(data_main)
options(datadist='dd')
hist(data_main$total_steps)
dd$limits$total_steps[2]<-780
#incident-11outcomes-totalsteps-----------------
fit.totalsteps.incident_11outcomes <- cph(Surv(FUduration.Acti_11outcomes.HDC.ICD10,Incidental.Acti_11outcomes.HDC.ICD10) ~ rcs(total_steps,3) + age.im  + sex.im +
                                            ethnic.im + education.im + centre.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im + Employment.im+
                                            wear_duration.im + PM2.5.im + Wear_season.im+ LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im, data=data_main)
HR.totalsteps.incident_11outcomes <-rms::Predict(fit.totalsteps.incident_11outcomes, total_steps, fun=exp,  ref.zero = TRUE)
anova(fit.totalsteps.incident_11outcomes)

a <- ggplot()+geom_line(data=HR.totalsteps.incident_11outcomes,aes(total_steps, yhat), color = "#619BA2",linewidth=1)+
  theme(legend.title = element_text(size=16, face="bold"),legend.direction = "vertical",
        legend.position=c(0.5, 0.9), text = element_text(size=12)) + 
  scale_linetype_manual(name="",values=c("solid"))+
  geom_ribbon(data = HR.totalsteps.incident_11outcomes, aes(total_steps, ymin = lower, ymax = upper),alpha = 0.5,fill="#B2D8EE")+
  labs(x="Total steps/day",y="Adjusted HRs for incident composite mental disorders")+
  scale_x_continuous(limits = c(0, 25000), breaks=c(0,5000,10000,15000,20000,25000))+geom_hline(yintercept=1,linewidth=0.5,linetype="dashed",colour="gray50")+
  scale_y_continuous(limits = c(0, 1.2),breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2))+
  geom_vline(xintercept=10000,linewidth=0.5,linetype="dashed",colour="gray50")+
  theme_pubr(base_size = 18,legend=c(0.6, 1))+
  annotate("text", 
           x = 12500, y = 1.1, 
           label = expression(italic("P"["overall"]~"<0.0001"~"; P"["nonlinearity"]~" <0.0001")), 
           size = 6,fontface="italic"
  )+
  annotate("text", 
           x = 100, y =1.2, 
           label = expression(" "), 
           size = 6,fontface="italic"
  )

a

ggsave(a,filename = "D:/UKB/data/step/figure/RCS_steps_11outcomes.PDF",dpi = 300,width = 9,height =7)

#########################################incident-6outcomes-total-steps#####################################
dd <-datadist(data_main)
options(datadist='dd')
hist(data_main$total_steps)
dd$limits$total_steps[2]<-780


fit.totalsteps.incident_6outcomes <- cph(Surv(FUduration.Acti_6outcomes.HDC.ICD10,Incidental.Acti_6outcomes.HDC.ICD10) ~ rcs(total_steps,3) + age.im  + sex.im +
                                            ethnic.im + education.im + centre.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im + Employment.im+
                                            wear_duration.im + PM2.5.im + Wear_season.im+ LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im, data=data_main)
HR.totalsteps.incident_6outcomes <-rms::Predict(fit.totalsteps.incident_6outcomes, total_steps, fun=exp,  ref.zero = TRUE)
anova(fit.totalsteps.incident_6outcomes)

b <- ggplot()+geom_line(data=HR.totalsteps.incident_6outcomes,aes(total_steps, yhat), color = "#619BA2",linewidth=1)+
  theme(legend.title = element_text(size=16, face="bold"),legend.direction = "vertical",
        legend.position=c(0.5, 0.9), text = element_text(size=12)) + 
  scale_linetype_manual(name="",values=c("solid"))+
  geom_ribbon(data = HR.totalsteps.incident_6outcomes, aes(total_steps, ymin = lower, ymax = upper),alpha = 0.5,fill="#B2D8EE")+
  labs(x="Total steps/day",y="Adjusted HRs for incident other mental disorders")+
  scale_x_continuous(limits = c(0, 25000), breaks=c(0,5000,10000,15000,20000,25000))+geom_hline(yintercept=1,linewidth=0.5,linetype="dashed",colour="gray50")+
  scale_y_continuous(limits = c(0, 1.2),breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2))+
  theme_pubr(base_size = 18,legend=c(0.6, 1))+
  annotate("text", 
           x = 12500, y = 1.1, 
           label = expression(italic("P"["overall"]~"<0.0001"~"; P"["nonlinearity"]~" <0.0001")), 
           size = 6,fontface="italic"
  )+
  annotate("text", 
           x = 100, y =1.2, 
           label = expression(" "), 
           size = 6,fontface="italic"
  )

b

ggsave(b,filename = "D:/UKB/data/step/figure/RCS_steps_6outcomes.PDF",dpi = 300,width = 9,height =7)



#########################################incident-depression-total-steps#####################################
dd <-datadist(data_main)
options(datadist='dd')
hist(data_main$total_steps)
dd$limits$total_steps[2]<-780


fit.totalsteps.incident_Depression <- cph(Surv(FUduration.acti.Depression.HDC.ICD10,Incidental.acti.Depression.HDC.ICD10) ~ rcs(total_steps,3) + age.im  + sex.im +
                                            ethnic.im + education.im + centre.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im + Employment.im+
                                            wear_duration.im + PM2.5.im + Wear_season.im+ LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im, data=data_main)
HR.totalsteps.incident_Depression <-rms::Predict(fit.totalsteps.incident_Depression, total_steps, fun=exp,  ref.zero = TRUE)
anova(fit.totalsteps.incident_Depression)

c <- ggplot()+geom_line(data=HR.totalsteps.incident_Depression,aes(total_steps, yhat), color = "#619BA2",linewidth=1)+
  theme(legend.title = element_text(size=16, face="bold"),legend.direction = "vertical",
        legend.position=c(0.5, 0.9), text = element_text(size=12)) + 
  scale_linetype_manual(name="",values=c("solid"))+
  geom_ribbon(data = HR.totalsteps.incident_Depression, aes(total_steps, ymin = lower, ymax = upper),alpha = 0.5,fill="#B2D8EE")+
  labs(x="Total steps/day",y="Adjusted HRs for incident depression disorder")+
  scale_x_continuous(limits = c(0, 25000), breaks=c(0,5000,10000,15000,20000,25000))+geom_hline(yintercept=1,linewidth=0.5,linetype="dashed",colour="gray50")+
  scale_y_continuous(limits = c(0, 1.2),breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2))+
  theme_pubr(base_size = 18,legend=c(0.6, 1))+
  annotate("text", 
           x = 12500, y = 1.1, 
           label = expression(italic("P"["overall"]~"<0.0001"~"; P"["nonlinearity"]~" =0.0057")), 
           size = 6,fontface="italic"
  )+
  annotate("text", 
           x = 100, y =1.2, 
           label = expression(" "), 
           size = 6,fontface="italic"
  )

c

ggsave(c,filename = "D:/UKB/data/step/figure/RCS_steps_depression.PDF",dpi = 300,width = 9,height =7)

#########################################incident-anxiety-total-steps#####################################
dd <-datadist(data_main)
options(datadist='dd')
hist(data_main$total_steps)
dd$limits$total_steps[2]<-780


fit.totalsteps.incident_Anxiety <- cph(Surv(FUduration.anxiety.HDC.ICD10,Incidental.anxiety.HDC.ICD10) ~ rcs(total_steps,3) + age.im  + sex.im +
                                         ethnic.im + education.im + centre.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im + Employment.im+
                                         wear_duration.im + PM2.5.im + Wear_season.im+ LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im, data=data_main)
HR.totalsteps.incident_Anxiety <-rms::Predict(fit.totalsteps.incident_Anxiety, total_steps, fun=exp,  ref.zero = TRUE)
anova(fit.totalsteps.incident_Anxiety)

d <- ggplot()+geom_line(data=HR.totalsteps.incident_Anxiety,aes(total_steps, yhat), color = "#619BA2",linewidth=1)+
  theme(legend.title = element_text(size=16, face="bold"),legend.direction = "vertical",
        legend.position=c(0.5, 0.9), text = element_text(size=12)) + 
  scale_linetype_manual(name="",values=c("solid"))+
  geom_ribbon(data = HR.totalsteps.incident_Anxiety, aes(total_steps, ymin = lower, ymax = upper),alpha = 0.5,fill="#B2D8EE")+
  labs(x="Total steps/day",y="Adjusted HRs for incident anxiety disorder ")+
  scale_x_continuous(limits = c(0, 25000), breaks=c(0,5000,10000,15000,20000,25000))+geom_hline(yintercept=1,linewidth=0.5,linetype="dashed",colour="gray50")+
  scale_y_continuous(limits = c(0, 1.2),breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2))+
  theme_pubr(base_size = 18,legend=c(0.6, 1))+
  annotate("text", 
           x = 12500, y = 1.1, 
           label = expression(italic("P"["overall"]~"<0.0001"~"; P"["nonlinearity"]~" =0.0112")), 
           size = 6,fontface="italic"
  )+
  annotate("text", 
           x = 100, y =1.2, 
           label = expression(" "), 
           size = 6,fontface="italic"
  )

d

ggsave(d,filename = "D:/UKB/data/step/figure/RCS_steps_anxiety.PDF",dpi = 300,width = 9,height =7)
#########################################incident-dementia-total-steps#####################################
dd <-datadist(data_main)
options(datadist='dd')
hist(data_main$total_steps)
dd$limits$total_steps[2]<-780


fit.totalsteps.incident_Dementia <- cph(Surv(FUduration.dementia.HDC.ICD10,Incidental.dementia.HDC.ICD10) ~ rcs(total_steps,3) + age.im  + sex.im +
                                          ethnic.im + education.im + centre.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im + Employment.im+
                                          wear_duration.im + PM2.5.im + Wear_season.im+ LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im, data=data_main)
HR.totalsteps.incident_Dementia <-rms::Predict(fit.totalsteps.incident_Dementia, total_steps, fun=exp,  ref.zero = TRUE)
anova(fit.totalsteps.incident_Dementia)

e<- ggplot()+geom_line(data=HR.totalsteps.incident_Dementia,aes(total_steps, yhat), color = "#619BA2",linewidth=1)+
  theme(legend.title = element_text(size=16, face="bold"),legend.direction = "vertical",
        legend.position=c(0.5, 0.9), text = element_text(size=12)) + 
  scale_linetype_manual(name="",values=c("solid"))+
  geom_ribbon(data = HR.totalsteps.incident_Dementia, aes(total_steps, ymin = lower, ymax = upper),alpha = 0.5,fill="#B2D8EE")+
  labs(x="Total steps/day",y="Adjusted HRs for incident dementia ")+
  scale_x_continuous(limits = c(0, 25000), breaks=c(0,5000,10000,15000,20000,25000))+geom_hline(yintercept=1,linewidth=0.5,linetype="dashed",colour="gray50")+
  scale_y_continuous(limits = c(0, 1.2),breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2))+
  theme_pubr(base_size = 18,legend=c(0.6, 1))+
  annotate("text", 
           x = 12500, y = 1.1, 
           label = expression(italic("P"["overall"]~"<0.0001"~"; P"["nonlinearity"]~" =0.0005")), 
           size = 6,fontface="italic"
  )+
  annotate("text", 
           x = 100, y =1.2, 
           label = expression(" "), 
           size = 6,fontface="italic"
  )

e

ggsave(e,filename = "D:/UKB/data/step/figure/RCS_steps_dementia.PDF",dpi = 300,width = 9,height =7)


#########################################incident-sleepdisord-total-steps#####################################
dd <-datadist(data_main)
options(datadist='dd')
hist(data_main$total_steps)
dd$limits$total_steps[2]<-780


fit.totalsteps.incident_Sleepdisord <- cph(Surv(FUduration.sleepdisord.HDC.ICD10,Incidental.sleepdisord.HDC.ICD10) ~ rcs(total_steps,3) + age.im  + sex.im +
                                             ethnic.im + education.im + centre.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im + Employment.im+
                                             wear_duration.im + PM2.5.im + Wear_season.im+ LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im, data=data_main)
HR.totalsteps.incident_Sleepdisord <-rms::Predict(fit.totalsteps.incident_Sleepdisord, total_steps, fun=exp,  ref.zero = TRUE)
anova(fit.totalsteps.incident_Sleepdisord)

f <- ggplot()+geom_line(data=HR.totalsteps.incident_Sleepdisord,aes(total_steps, yhat), color = "#619BA2",linewidth=1)+
  theme(legend.title = element_text(size=16, face="bold"),legend.direction = "vertical",
        legend.position=c(0.5, 0.9), text = element_text(size=12)) + 
  scale_linetype_manual(name="",values=c("solid"))+
  geom_ribbon(data = HR.totalsteps.incident_Sleepdisord, aes(total_steps, ymin = lower, ymax = upper),alpha = 0.5,fill="#B2D8EE")+
  labs(x="Total steps/day",y="Adjusted HRs for incident sleep disorder ")+
  scale_x_continuous(limits = c(0, 25000), breaks=c(0,5000,10000,15000,20000,25000))+geom_hline(yintercept=1,linewidth=0.5,linetype="dashed",colour="gray50")+
  scale_y_continuous(limits = c(0, 1.2),breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2))+
  theme_pubr(base_size = 18,legend=c(0.6, 1))+
  annotate("text", 
           x = 12500, y = 1.1, 
           label = expression(italic("P"["overall"]~"<0.0001"~"; P"["nonlinearity"]~" =0.2003")), 
           size = 6,fontface="italic"
  )+
  annotate("text", 
           x = 100, y =1.2, 
           label = expression(" "), 
           size = 6,fontface="italic"
  )

f

ggsave(f,filename = "D:/UKB/data/step/figure/RCS_steps_sleep.PDF",dpi = 300,width = 9,height =7)

#########################################incident-SubstanceUse-total-steps#####################################
dd <-datadist(data_main)
options(datadist='dd')
hist(data_main$total_steps)
dd$limits$total_steps[2]<-780


fit.totalsteps.incident_SubstanceUse <- cph(Surv(FUduration.SubstanceUse.HDC.ICD10,Incidental.SubstanceUse.HDC.ICD10) ~ rcs(total_steps,3) + age.im  + sex.im +
                                              ethnic.im + education.im + centre.im + Smoking.im + Alcohol.im + TDI.im + Healthy_diet.im + BMI.im + HTN.im + diabetes.im + Employment.im+
                                              wear_duration.im + PM2.5.im + Wear_season.im+ LongstandingIllness.im + HTN_cho_dia_medi_combined.im + psychotropic.acti.im, data=data_main)
HR.totalsteps.incident_SubstanceUse <-rms::Predict(fit.totalsteps.incident_SubstanceUse, total_steps, fun=exp,  ref.zero = TRUE)
anova(fit.totalsteps.incident_SubstanceUse)

g <- ggplot()+geom_line(data=HR.totalsteps.incident_SubstanceUse,aes(total_steps, yhat), color = "#619BA2",linewidth=1)+
  theme(legend.title = element_text(size=16, face="bold"),legend.direction = "vertical",
        legend.position=c(0.5, 0.9), text = element_text(size=12)) + 
  scale_linetype_manual(name="",values=c("solid"))+
  geom_ribbon(data = HR.totalsteps.incident_SubstanceUse, aes(total_steps, ymin = lower, ymax = upper),alpha = 0.5,fill="#B2D8EE")+
  labs(x="Total steps/day",y="Adjusted HRs for incident substance use disorder ")+
  scale_x_continuous(limits = c(0, 25000), breaks=c(0,5000,10000,15000,20000,25000))+geom_hline(yintercept=1,linewidth=0.5,linetype="dashed",colour="gray50")+
  scale_y_continuous(limits = c(0, 1.2),breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2))+
  theme_pubr(base_size = 18,legend=c(0.6, 1))+
  annotate("text", 
           x = 12500, y = 1.1, 
           label = expression(italic("P"["overall"]~"<0.0001"~"; P"["nonlinearity"]~" =0.2112")), 
           size = 6,fontface="italic"
  )+
  annotate("text", 
           x = 100, y =1.2, 
           label = expression(" "), 
           size = 6,fontface="italic"
  )

g

ggsave(g,filename = "D:/UKB/data/step/figure/RCS_steps_substance.PDF",dpi = 300,width = 9,height =7)


