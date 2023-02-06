##load self-written functions and dependencies for this analysis
source("utils.R") 

##########read the data
mydata1<-read_excel("2.2 & 2.1 PHQ Data Extract for Lang.xlsx",sheet = "2.1 PHQ Data", col_types = NULL, na="")
mydata2<-read_excel("2.2 & 2.1 PHQ Data Extract for Lang.xlsx",sheet = "2.2 PHQ Data", col_types = NULL, na="")
##combine two datasets
mydata<-rbind.data.frame(mydata1,mydata2)

colnames(mydata)<-c("id","PID","Group","enrolltime","completetime","app_starttime","age","gender",
                    paste0(rep(paste0("Week",0:5,"_score_"),each=8,times=1),paste0("Q",1:8)),
                    paste0("Week",0:5,"_time"),paste0("Week",0:5,"_totalscore"))
##recode the group
mydata$Study<-paste0(str_split_fixed(mydata$Group," ",5)[,1],
                  str_split_fixed(mydata$Group," ",5)[,2])
mydata$Group<-ifelse(str_split_fixed(mydata$Group," ",5)[,2]=="2.1",
                     str_split_fixed(mydata$Group," ",5)[,4],
                     str_split_fixed(mydata$Group," ",5)[,5])
  
mydata<-mydata[,c(2,ncol(mydata),3:(ncol(mydata)-1))]

##calculate the time spacing
time_temp<-as.data.frame(mydata[,grep("_time",colnames(mydata))])
for (i in 1:ncol(time_temp)) {
  for (j in 1:nrow(time_temp)) {
    time_temp[j,i]<-as.character(str_split_fixed(unlist(time_temp[j,i])," ",2)[,1])
  }
}
time_temp1<-matrix(,nrow(time_temp),ncol(time_temp))
for (i in 1:ncol(time_temp)) {
  for (j in 1:nrow(time_temp)) {
    time_temp1[j,i]<-ifelse(time_temp[j,i]=="",NA,as.numeric(difftime(time_temp[j,i],time_temp[j,1],units="days")))
  }
}
mydata[,grep("_time",colnames(mydata))]<-as.matrix(time_temp1)/7

##create group labels for baseline level symptom severity based on Kroenke et al.(2009)
mydata$Base_sev_cat<-as.factor(
  ifelse(mydata$Week0_totalscore<5,"No",
         ifelse(mydata$Week0_totalscore<10,"Mild",
                ifelse(mydata$Week0_totalscore<15,"Moderate",
                       ifelse(mydata$Week0_totalscore<20,"Moderate_Severe","Severe")))
  ))
##reorder the level
mydata$Base_sev_cat<-factor(mydata$Base_sev_cat,levels = c("No","Mild","Moderate","Moderate_Severe","Severe"))

mydata<-mydata[,c(1:3,ncol(mydata),4:(ncol(mydata)-1))]

##create the age groups
mydata$age_grp<-ifelse(mydata$age<18,"age13-17","age18+")
mydata$age_grp<-factor(mydata$age_grp,levels = c("age13-17","age18+"))
mydata<-mydata[,c(1:4,ncol(mydata),8:(ncol(mydata)-1))]

#########end of data input and format editting

#################
#################1.Treatment VS Control (0-5 week): Combined
#################

###create different subjectlists 
weekly_missing_points<-rowSums(is.na(mydata[,grep("_time",colnames(mydata))]))
##select participants with baseline severity >= 5 AND baseline weeks;mITT with PHQ>=5
subjlist_mITT_phq5<-mydata$PID[(mydata$Week0_totalscore>4 & weekly_missing_points <6)]
##select participants with baseline severity >= 5 AND no missing weeks;PP with PHQ>=5
subjlist_PP_phq5<-mydata$PID[(mydata$Week0_totalscore>4 & weekly_missing_points <1)]
##select participants with baseline severity >= 10 AND baseline weeks;mITT with PHQ>=10
subjlist_mITT_phq10<-mydata$PID[(mydata$Week0_totalscore>9 & weekly_missing_points <6)]
##select participants with baseline severity >= 10 AND no missing weeks;PP with PHQ>=10
subjlist_PP_phq10<-mydata$PID[(mydata$Week0_totalscore>9 & weekly_missing_points <1)]


##select data for further analysis
sel_data<-mydata[weekly_missing_points<6,]
sel_data_item<-sel_data[,grep(str_c(c("PID","Study","Group","Base_sev_cat","age_grp","age","gender",
                                      paste0("Week",0:5,"_score_")),collapse = "|"),colnames(sel_data))]

###data preparation
score_data_long<-melt(sel_data_item,id.vars = c("PID","Study","Group","Base_sev_cat","age_grp","age","gender"),value.name = "score")
score_data_long$week<-as.integer(str_remove_all(str_split_fixed(score_data_long$variable,"_",3)[,1],"Week"))
score_data_long$ques<-str_split_fixed(score_data_long$variable,"_",3)[,3]
##merge the spacing data 
spacing_data<-sel_data[,grep(str_c(c("PID","Study","Group","Base_sev_cat","age_grp","age","gender",
                                     paste0("Week",0:5,"_time")),collapse = "|"),colnames(sel_data))]
spacing_data_long<-melt(spacing_data,id.vars = c("PID","Study","Group","Base_sev_cat","age_grp","age","gender"),value.name = "time")
spacing_data_long<-spacing_data_long[order(spacing_data_long$PID),]
spacing_data_long_rep<-spacing_data_long[rep(1:nrow(spacing_data_long),each=8),]
score_data_long<-score_data_long[order(score_data_long$PID,score_data_long$week,score_data_long$ques),]
score_data_long$time<-spacing_data_long_rep$time
score_data_long<-score_data_long[,c("PID","Study","Group","Base_sev_cat","age_grp","week","time","ques","score")]

##GLMM with MI
ini<-mice(score_data_long,maxit = 0,method = "norm.predit")
pred<-ini$pred
pred["score","PID"]<- -2
pred["time","PID"]<- -2
##change PID as integer
score_data_long$PID<-as.integer(as.character(score_data_long$PID))
set.seed(1)
imp <- mice(data = score_data_long, pred = pred, method = "2l.pan", m = 100, print = F)
summary(imp)
implist <- mids2mitml.list(imp)

###get the imputed data
temp_score<-matrix(,nrow(implist[[1]]),100)
temp_time<-matrix(,nrow(implist[[1]]),100)
for (i in 1:100) {
  temp_score[,i]<-as.numeric(unlist(implist[[i]][9]))
  temp_time[,i]<-as.numeric(unlist(implist[[i]][7]))
}
score_data_long_imp<-cbind.data.frame(score_data_long[,c("PID","Study","Group","Base_sev_cat","age_grp","week","ques")],
                                      rowMeans(temp_time),rowMeans(temp_score))
colnames(score_data_long_imp)<-c("PID","Study","Group","Base_sev_cat","age_grp","week","ques","imp_time","imp_score")

#save the averaged imputed data for different subjectlists (long form)
write.csv(score_data_long_imp,file = "Limbix_data_2.1&2.2_TreatmentVSControl_MI_Allsubj_159subj_long.csv",row.names = F)
####mITT phq>=10
write.csv(score_data_long_imp[(score_data_long_imp$PID %in% subjlist_mITT_phq10),],
          file="Limbix_data_2.1&2.2_TreatmentVSControl_MI_mITT_phq10_121subj_long.csv",row.names = F)
####PP phq>=10
write.csv(score_data_long_imp[(score_data_long_imp$PID %in% subjlist_PP_phq10),],
          file="Limbix_data_2.1&2.2_TreatmentVSControl_MI_PP_phq10_86subj_long.csv",row.names = F)
####mITT phq>=5
write.csv(score_data_long_imp[(score_data_long_imp$PID %in% subjlist_mITT_phq5),],
          file="Limbix_data_2.1&2.2_TreatmentVSControl_MI_mITT_phq5_153subj_long.csv",row.names = F)
####PP phq>=5
write.csv(score_data_long_imp[(score_data_long_imp$PID %in% subjlist_PP_phq5),],
          file="Limbix_data_2.1&2.2_TreatmentVSControl_MI_PP_phq5_109subj_long.csv",row.names = F)

##wide form
score_data_imp<-reshape(score_data_long_imp,idvar = c("PID","Study","Group","Base_sev_cat","age_grp","week"),timevar = "ques",direction = "wide")
score_data_imp$weekly_score<-rowSums(score_data_imp[,grep("score",colnames(score_data_imp))])
score_data_imp$weekly_time<-rowMeans(score_data_imp[,grep("time",colnames(score_data_imp))])

#save the averaged imputed data for different subjectlists (wide form)
write.csv(score_data_imp,file = "Limbix_data_2.1&2.2_TreatmentVSControl_MI_Allsubj_159subj_wide.csv",row.names = F)
####mITT phq>=10
write.csv(score_data_imp[(score_data_imp$PID %in% subjlist_mITT_phq10),],
          file="Limbix_data_2.1&2.2_TreatmentVSControl_MI_mITT_phq10_121subj_wide.csv",row.names = F)
####PP phq>=10
write.csv(score_data_imp[(score_data_imp$PID %in% subjlist_PP_phq10),],
          file="Limbix_data_2.1&2.2_TreatmentVSControl_MI_PP_phq10_86subj_wide.csv",row.names = F)
####mITT phq>=5
write.csv(score_data_imp[(score_data_imp$PID %in% subjlist_mITT_phq5),],
          file="Limbix_data_2.1&2.2_TreatmentVSControl_MI_mITT_phq5_153subj_wide.csv",row.names = F)
####PP phq>=5
write.csv(score_data_imp[(score_data_imp$PID %in% subjlist_PP_phq5),],
          file="Limbix_data_2.1&2.2_TreatmentVSControl_MI_PP_phq5_109subj_wide.csv",row.names = F)

##backup the original imputed list
bk_implist<-implist

###################
######GLMM analysis
###################
#conduct the analysis for mITT phq>=10
for (i in 1:100) {
  implist[[i]]<-bk_implist[[i]][bk_implist[[i]]$PID %in% subjlist_mITT_phq10,]
}

fit.all <- with(implist, lmer(score ~ 1 + Group*week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                              control = lmerControl(optimizer ="Nelder_Mead")))
fit.main <- with(implist, lmer(score ~ 1 + Group + week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
fit.study <- with(implist, lmer(score ~ 1 + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
fit.base <- with(implist, lmer(score ~ 1 + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                                control = lmerControl(optimizer ="Nelder_Mead")))

fit.group<-with(implist, lmer(score ~ 1 + Group + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                              control = lmerControl(optimizer ="Nelder_Mead")))
fit.time<-with(implist, lmer(score ~ 1 + week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                             control = lmerControl(optimizer ="Nelder_Mead")))

sink("GLMM_mITT_phq10.txt")
print("Main effect of Group")
testModels(fit.group,fit.study, method = "D1")
cat("\n")
print("Main effect of time/week")
testModels(fit.time, fit.study, method = "D1")
cat("\n")
print("Interaction")
testModels(fit.all, fit.main, method = "D1")
cat("\n")
print("Effect of study")
testModels(fit.study, fit.base, method = "D1")
sink()

#conduct the analysis for mITT phq>=5
for (i in 1:100) {
  implist[[i]]<-bk_implist[[i]][bk_implist[[i]]$PID %in% subjlist_mITT_phq5,]
}

fit.all <- with(implist, lmer(score ~ 1 + Group*week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                              control = lmerControl(optimizer ="Nelder_Mead")))
fit.main <- with(implist, lmer(score ~ 1 + Group + week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
fit.study <- with(implist, lmer(score ~ 1 + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                                control = lmerControl(optimizer ="Nelder_Mead")))
fit.base <- with(implist, lmer(score ~ 1 + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))

fit.group<-with(implist, lmer(score ~ 1 + Group + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                              control = lmerControl(optimizer ="Nelder_Mead")))
fit.time<-with(implist, lmer(score ~ 1 + week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                             control = lmerControl(optimizer ="Nelder_Mead")))

sink("GLMM_mITT_phq5.txt")
print("Main effect of Group")
testModels(fit.group,fit.study, method = "D1")
cat("\n")
print("Main effect of time/week")
testModels(fit.time, fit.study, method = "D1")
cat("\n")
print("Interaction")
testModels(fit.all, fit.main, method = "D1")
cat("\n")
print("Effect of study")
testModels(fit.study, fit.base, method = "D1")
sink()


#conduct the analysis for PP phq>=10
for (i in 1:100) {
  implist[[i]]<-bk_implist[[i]][bk_implist[[i]]$PID %in% subjlist_PP_phq10,]
}

fit.all <- with(implist, lmer(score ~ 1 + Group*week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                              control = lmerControl(optimizer ="Nelder_Mead")))
fit.main <- with(implist, lmer(score ~ 1 + Group + week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
fit.study <- with(implist, lmer(score ~ 1 + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                                control = lmerControl(optimizer ="Nelder_Mead")))
fit.base <- with(implist, lmer(score ~ 1 + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))

fit.group<-with(implist, lmer(score ~ 1 + Group + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                              control = lmerControl(optimizer ="Nelder_Mead")))
fit.time<-with(implist, lmer(score ~ 1 + week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                             control = lmerControl(optimizer ="Nelder_Mead")))

sink("GLMM_PP_phq10.txt")
print("Main effect of Group")
testModels(fit.group,fit.study, method = "D1")
cat("\n")
print("Main effect of time/week")
testModels(fit.time, fit.study, method = "D1")
cat("\n")
print("Interaction")
testModels(fit.all, fit.main, method = "D1")
cat("\n")
print("Effect of study")
testModels(fit.study, fit.base, method = "D1")
sink()


#conduct the analysis for PP phq>=5
for (i in 1:100) {
  implist[[i]]<-bk_implist[[i]][bk_implist[[i]]$PID %in% subjlist_PP_phq5,]
}

fit.all <- with(implist, lmer(score ~ 1 + Group*week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                              control = lmerControl(optimizer ="Nelder_Mead")))
fit.main <- with(implist, lmer(score ~ 1 + Group + week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
fit.study <- with(implist, lmer(score ~ 1 + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                                control = lmerControl(optimizer ="Nelder_Mead")))
fit.base <- with(implist, lmer(score ~ 1 + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))

fit.group<-with(implist, lmer(score ~ 1 + Group + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                              control = lmerControl(optimizer ="Nelder_Mead")))
fit.time<-with(implist, lmer(score ~ 1 + week + Study + time + (1 + time | PID)+ (1 | ques),REML = FALSE, 
                             control = lmerControl(optimizer ="Nelder_Mead")))

sink("GLMM_PP_phq5.txt")
print("Main effect of Group")
testModels(fit.group,fit.study, method = "D1")
cat("\n")
print("Main effect of time/week")
testModels(fit.time, fit.study, method = "D1")
cat("\n")
print("Interaction")
testModels(fit.all, fit.main, method = "D1")
cat("\n")
print("Effect of study")
testModels(fit.study, fit.base, method = "D1")
sink()
