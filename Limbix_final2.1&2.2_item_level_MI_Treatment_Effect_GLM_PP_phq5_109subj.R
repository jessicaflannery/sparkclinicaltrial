##load self-written functions and dependencies for this analysis
source("utils.R") 

##########read the data
data_head<-"PP_phq5_109subj"
score_data_imp_wide<-read.csv(file=paste0("Limbix_data_2.1&2.2_TreatmentVSControl_MI_",data_head,"_wide.csv"))
score_data_imp_long<-read.csv(file=paste0("Limbix_data_2.1&2.2_TreatmentVSControl_MI_",data_head,"_long.csv"))
#score_data_long<-reshape(score_data_imp_long,idvar = c("PID","Study","Group","Base_sev_cat","age_grp","week"),timevar = "ques",direction = "wide")

##plot with imp scores
View(score_data_imp_wide)
score_data_imp_wk_long<-score_data_imp_wide[,c("PID","Group","Base_sev_cat","age_grp","week",
                                       "weekly_time","weekly_score")]
colnames(score_data_imp_wk_long)<-c("PID","Group","Base_sev_cat","age_grp","Week",
                                 "imp_time","imp_score")
score_data_sum_wk_imp<-summarySE(score_data_imp_wk_long,measurevar = "imp_score",groupvars = c("Group","Week"),na.rm = T)

png(filename = paste0("Limbix_score_byTreatmentGroup_week0-5_imputed_itemlevel_",data_head,".jpeg"),width = 1200,height = 600,res=150)
ggplot(data= score_data_sum_wk_imp,aes(x=Week,y=imp_score,group=Group,color=Group)) +
  geom_line(size=1) +
  geom_point(size=3,pch=16) +
  geom_errorbar(aes(ymin=imp_score-se,ymax=imp_score+se),width=0.2,size=1) +
  geom_line(data = score_data_imp_wk_long,aes(x=imp_time,y=imp_score,group=PID,color=Group),alpha=0.4) +
  geom_point(data = score_data_imp_wk_long,aes(x=imp_time,y=imp_score,group=PID,color=Group),size=1,pch=16,alpha=0.4) +
  xlab(label = "Weeks") +
  #scale_color_manual(values = c("deepskyblue1","firebrick1")) +
  theme_bw()
dev.off()
##abacus plot by baseline severity in Treatment group (imp data)
score_data_sum_wk_imp1<-summarySE(score_data_imp_wk_long,measurevar = "imp_score",groupvars = c("Group","Base_sev_cat","Week"),na.rm = T)
png(filename = paste0("Limbix_score_byBaselinesev_Treatment_week0-5_withMI_",data_head,".jpeg"),width = 1200,height = 600,res=150)
ggplot(data= score_data_sum_wk_imp1[score_data_sum_wk_imp1$Group=="Treatment",],aes(x=Week,y=imp_score,group=Base_sev_cat,color=Base_sev_cat)) +
  geom_line(size=1) +
  geom_point(size=3,pch=16) +
  geom_errorbar(aes(ymin=imp_score-se,ymax=imp_score+se),width=0.2,size=1) +
  geom_line(data = score_data_imp_wk_long[score_data_imp_wk_long$Group=="Treatment",],aes(x=imp_time,y=imp_score,group=PID,color=Base_sev_cat),alpha=0.4) +
  geom_point(data = score_data_imp_wk_long[score_data_imp_wk_long$Group=="Treatment",],aes(x=imp_time,y=imp_score,group=PID,color=Base_sev_cat),size=1,pch=16,alpha=0.4) +
  xlab(label = "Weeks") +
  theme_bw()
dev.off()
##abacus plot by baseline severity in Control group (imp data)
png(filename = paste0("Limbix_score_byBaselinesev_Control_week0-5_withMI_",data_head,".jpeg"),width = 1200,height = 600,res=150)
ggplot(data= score_data_sum_wk_imp1[score_data_sum_wk_imp1$Group=="Control",],aes(x=Week,y=imp_score,group=Base_sev_cat,color=Base_sev_cat)) +
  geom_line(size=1) +
  geom_point(size=3,pch=16) +
  geom_errorbar(aes(ymin=imp_score-se,ymax=imp_score+se),width=0.2,size=1) +
  geom_line(data = score_data_imp_wk_long[score_data_imp_wk_long$Group=="Control",],aes(x=imp_time,y=imp_score,group=PID,color=Base_sev_cat),alpha=0.4) +
  geom_point(data = score_data_imp_wk_long[score_data_imp_wk_long$Group=="Control",],aes(x=imp_time,y=imp_score,group=PID,color=Base_sev_cat),size=1,pch=16,alpha=0.4) +
  xlab(label = "Weeks") +
  theme_bw()
dev.off()

# ###Traditional ANOVA with imputed data
# score_data_imp_long$PID<-as.factor(score_data_imp_long$PID)
# score_data_imp_long$Group<-as.factor(score_data_imp_long$Group)
# score_data_imp_long$week<-as.factor(score_data_imp_long$week)
# summary(aov(imp_score~Group*week+Study+Error(PID/week),data=score_data_imp_long))

##mixed-effect linear model
##get the descriptive
sink(paste0("Limbix_final2.1&2.2_GLM_analysis_",data_head,".txt"))
print(score_data_sum_wk_imp)
score_data_imp_long$week<-as.integer(as.character(score_data_imp_long$week))
mod<-lmer(imp_score ~ 1 + Group*week + Study + imp_time + (1 + imp_time | PID) + (1 | ques), data = score_data_imp_long, REML = T,
          control = lmerControl(optimizer ="Nelder_Mead"))

cat("\n")
print("Overall effect: Interaction")
print("mod<-lmer(imp_score ~ 1 + Group*week + Study + imp_time + (1 + imp_time | PID) + (1 | ques), data = score_data_imp_long, REML = T,
                control = lmerControl(optimizer =\"Nelder_Mead\"))")
print(summary(mod))

cat("\n")
print("in Treatment group")
mod_t<-lmer(imp_score ~ 1 + week + imp_time + (1 + imp_time | PID) + (1 | ques), 
            data = score_data_imp_long[score_data_imp_long$Group=="Treatment",], REML = T,
            control = lmerControl(optimizer ="Nelder_Mead"))
print(summary(mod_t))

cat("\n")
print("in Control group")
mod_c<-lmer(imp_score ~ 1 + week + imp_time + (1 + imp_time | PID) + (1 | ques), 
            data = score_data_imp_long[score_data_imp_long$Group=="Control",], REML = T,
            control = lmerControl(optimizer ="Nelder_Mead"))
print(summary(mod_c))

cat("\n")
print("effect size")
require("MuMIn")
mod_null<-lmer(imp_score ~ 1 + (1 | PID) + (1 | ques), data = score_data_imp_long, REML = T,
               control = lmerControl(optimizer ="Nelder_Mead"))
summary(mod_null)
mod_base<-lmer(imp_score ~ 1 + imp_time + (1 + imp_time | PID) + (1 | ques), data = score_data_imp_long, REML = T,
               control = lmerControl(optimizer ="Nelder_Mead"))
summary(mod_base)
mod_noInter<-lmer(imp_score ~ 1 + Group+week + imp_time + (1 + imp_time | PID) + (1 | ques), data = score_data_imp_long, REML = T,
                  control = lmerControl(optimizer ="Nelder_Mead"))
mod<-lmer(imp_score ~ 1 + Group*week + imp_time + (1 + imp_time | PID) + (1 | ques), data = score_data_imp_long, REML = T,
          control = lmerControl(optimizer ="Nelder_Mead"))
summary(mod)
summary(mod_noInter)

require(MuMIn)
cat("\n")
print("pseudo R-square for null, base, complete, and no-interaction models in order")
cat("\n")
print(r.squaredGLMM(mod_null))
print(r.squaredGLMM(mod_base))
print(r.squaredGLMM(mod))
print(r.squaredGLMM(mod_noInter))

##cohen's f-square for the interaction term
##fixed effect variability
(r.squaredGLMM(mod)[1]-r.squaredGLMM(mod_noInter)[1])
#cohen_f2<-(r.squaredGLMM(mod)[1]-r.squaredGLMM(mod_noInter)[1])/(1-(r.squaredGLMM(mod)[1]-r.squaredGLMM(mod_noInter)[1]))
#print(cohen_f2)
cat("\n")
print("cohen's f-square")
cohen_f2_alt<-(r.squaredGLMM(mod)[1]-r.squaredGLMM(mod_noInter)[1])/(1-r.squaredGLMM(mod)[1])
print(cohen_f2_alt)
sink()

