##load self-written functions and dependencies for this analysis
source("utils.R") 

set.seed(222)
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

###participants with baseline severity >= 10 AND baseline weeks;mITT with PHQ>=10
######Visualization of missing data pattern at the week level
sel_data<-mydata[mydata$PID %in% subjlist_mITT_phq10,]
#####Treatment vs Control week0-week5
png(filename = "Limbix_2.1&2.2_missing_pattern_week0-5_mITT_phq10_subj121.jpeg",width = 1200,height = 600,res=150)
vis_miss(sel_data[,grep(str_c(paste0("Week",0:5,"_time"),collapse = "|"),colnames(sel_data))])
dev.off()
#percentage by group
png(filename = "Limbix_2.1&2.2_missing_pattern_by_TreatmentGroup_mITT_phq10_subj121.jpeg",width = 800,height = 600,res=150)
gg_miss_fct(sel_data[,grep(str_c(c(paste0("Week",0:5,"_time"),"Group"),collapse = "|"),colnames(sel_data))],fct = Group)
dev.off()
#percentage by severity
png(filename = "Limbix_2.1&2.2_missing_pattern_by_BaselineSev_mITT_phq10_subj121.jpeg",width = 800,height = 600,res=150)
gg_miss_fct(sel_data[,grep(str_c(c(paste0("Week",0:5,"_time"),"_cat"),collapse = "|"),colnames(sel_data))],fct = Base_sev_cat)
dev.off()
#percentage by gender
png(filename = "Limbix_2.1&2.2_missing_pattern_by_gender_mITT_phq10_subj121.jpeg",width = 800,height = 600,res=150)
gg_miss_fct(sel_data[,grep(str_c(c(paste0("Week",0:5,"_time"),"gender"),collapse = "|"),colnames(sel_data))],fct = gender)
dev.off()
##percentage by age groups
png(filename = "Limbix_2.1&2.2_missing_pattern_by_age_mITT_phq10_subj121.jpeg",width = 800,height = 600,res=150)
gg_miss_fct(sel_data[,grep(str_c(c(paste0("Week",0:5,"_time"),"age_grp"),collapse = "|"),colnames(sel_data))],fct = age_grp)
dev.off()


#################little test of MCAR
##based on missing dates; 
##NOTE: due to package updates, little test is done with mcar_test with parameter changes
data_date<-sel_data[,grep(str_c(paste0("Week",0:5,"_time"),collapse = "|"),colnames(sel_data))]
little_dates<-mcar_test(data_date[,-1])
sink("Little's test for MCAR_TreatVSControl_0-5week_mITT_phq10_subj121.text")
cat("Little's test for MCAR based on testing dates\n")
cat("chi.sq (",little_dates$df,") = ",little_dates$statistic, ", p =", little_dates$p.value,"\n")
cat("amount of missing data:\n")
print(little_dates$missing.patterns)
sink()
####get some basics
table(sel_data[,c("Study","Group")])

#####Stastical tests for missing value patterns based on counts (only do this for dates)
sink("Missing_data_analysis_Spark2.1&2.2_0-5week_mITT_phq10_subj121.text")
sel_data$miss_count<-rowSums(is.na(sel_data[,grep("_time",colnames(sel_data))]))
sel_data$miss_count_bin<-ifelse(sel_data$miss_count==0,0,1) ##binary: 0 or non-zero
print("with Study group")
t.test(miss_count~Study,data=sel_data, var.equal = T)
table(sel_data[,c("Study","miss_count")])
chisq.test(table(sel_data[,c("Study","miss_count")]),simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("Study","miss_count_bin")])
chisq.test(table(sel_data[,c("Study","miss_count_bin")]),simulate.p.value = T)

cat("\n")
print("with Treatment group")
t.test(miss_count~Group,data=sel_data, var.equal = T)
table(sel_data[,c("Group","miss_count")])
chisq.test(table(sel_data[,c("Group","miss_count")]),simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("Group","miss_count_bin")])
chisq.test(table(sel_data[,c("Group","miss_count_bin")]),simulate.p.value = T)

cat("\n")
print("with Week")
data_date_miss<-as.table(rbind(colSums(is.na(data_date)),nrow(data_date)-colSums(is.na(data_date))))
print(data_date_miss)
print("A-missing; B-no missing")
chisq.test(data_date_miss,simulate.p.value = T)

cat("\n")
print("Treatment by Week")
temp<-as.table(rbind(colSums(is.na(sel_data[sel_data$Group=="Treatment",grep("_time",colnames(sel_data))])),
                     colSums(is.na(sel_data[sel_data$Group=="Control",grep("_time",colnames(sel_data))]))))
print(temp) 
print("A is Treatment group; B is Control group")
chisq.test(temp[,-1],simulate.p.value = T)

cat("\n")
print("relationship with the baseline severity")
cor.test(sel_data$Week0_totalscore,sel_data$miss_count,method = "spearman",exact = F)
table(sel_data[,c("Base_sev_cat","miss_count")])
chisq.test(table(sel_data[,c("Base_sev_cat","miss_count")])[-(1:2),],simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("Base_sev_cat","miss_count_bin")])
chisq.test(table(sel_data[,c("Base_sev_cat","miss_count_bin")])[-(1:2),],simulate.p.value = T)

cat("\n")
print("Severity by Week")
temp<-as.table(rbind(colSums(is.na(sel_data[sel_data$Base_sev_cat=="Mild",grep("_time",colnames(sel_data))])),
                     colSums(is.na(sel_data[sel_data$Base_sev_cat=="Moderate",grep("_time",colnames(sel_data))])),
                     colSums(is.na(sel_data[sel_data$Base_sev_cat=="Moderate_Severe",grep("_time",colnames(sel_data))])),
                     colSums(is.na(sel_data[sel_data$Base_sev_cat=="Severe",grep("_time",colnames(sel_data))]))))
print(temp) 
print("A-D: Mild, Moderate, Moderate-Severe, Severe")
chisq.test(temp[-1,-1], simulate.p.value = T)
print("Chi-sq test for group and baseline severity")
temp<-table(sel_data[,c("Group","Base_sev_cat")])
print(temp)
print("chisq.test with no and mild group")
chisq.test(temp[,-(1:2)],simulate.p.value = T)


cat("\n")
print("effect of age")
cor.test(sel_data$age,sel_data$miss_count,method = "spearman",exact = F)
table(sel_data[,c("age_grp","miss_count")])
chisq.test(table(sel_data[,c("age_grp","miss_count")]),simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("age_grp","miss_count_bin")])
chisq.test(table(sel_data[,c("age_grp","miss_count_bin")]),simulate.p.value = T)

cat("\n")
print("effect of gender")
table(sel_data[,c("gender","miss_count")])
chisq.test(table(sel_data[,c("gender","miss_count")]),simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("gender","miss_count_bin")])
chisq.test(table(sel_data[,c("gender","miss_count_bin")]),simulate.p.value = T)
print("only male and female")
chisq.test(table(sel_data[sel_data$gender!="X (non-binary)",c("gender","miss_count_bin")]),simulate.p.value = T)
print("female vs. non-female")
chisq.test(as.table(matrix(c(62,19,24,16),2,2,byrow = T)),simulate.p.value = T)
sink()



#################################################################################
###participants with baseline severity >= 10 AND baseline weeks;mITT with PHQ>=5
######Visualization of missing data pattern at the week level
sel_data<-mydata[mydata$PID %in% subjlist_mITT_phq5,]
#####Treatment vs Control week0-week5
png(filename = "Limbix_2.1&2.2_missing_pattern_week0-5_mITT_phq5_subj153.jpeg",width = 1200,height = 600,res=150)
vis_miss(sel_data[,grep(str_c(paste0("Week",0:5,"_time"),collapse = "|"),colnames(sel_data))])
dev.off()
#percentage by group
png(filename = "Limbix_2.1&2.2_missing_pattern_by_TreatmentGroup_mITT_phq5_subj153.jpeg",width = 800,height = 600,res=150)
gg_miss_fct(sel_data[,grep(str_c(c(paste0("Week",0:5,"_time"),"Group"),collapse = "|"),colnames(sel_data))],fct = Group)
dev.off()
#percentage by severity
png(filename = "Limbix_2.1&2.2_missing_pattern_by_BaselineSev_mITT_phq5_subj153.jpeg",width = 800,height = 600,res=150)
gg_miss_fct(sel_data[,grep(str_c(c(paste0("Week",0:5,"_time"),"_cat"),collapse = "|"),colnames(sel_data))],fct = Base_sev_cat)
dev.off()
#percentage by gender
png(filename = "Limbix_2.1&2.2_missing_pattern_by_gender_mITT_phq5_subj153.jpeg",width = 800,height = 600,res=150)
gg_miss_fct(sel_data[,grep(str_c(c(paste0("Week",0:5,"_time"),"gender"),collapse = "|"),colnames(sel_data))],fct = gender)
dev.off()
##percentage by age groups
png(filename = "Limbix_2.1&2.2_missing_pattern_by_age_mITT_phq5_subj153.jpeg",width = 800,height = 600,res=150)
gg_miss_fct(sel_data[,grep(str_c(c(paste0("Week",0:5,"_time"),"age_grp"),collapse = "|"),colnames(sel_data))],fct = age_grp)
dev.off()


#################little test of MCAR
##based on missing dates; 
##NOTE: due to package updates, little test is done with mcar_test with parameter changes
data_date<-sel_data[,grep(str_c(paste0("Week",0:5,"_time"),collapse = "|"),colnames(sel_data))]
little_dates<-mcar_test(data_date[,-1])
sink("Little's test for MCAR_TreatVSControl_0-5week_mITT_phq5_subj153.text")
cat("Little's test for MCAR based on testing dates\n")
cat("chi.sq (",little_dates$df,") = ",little_dates$statistic, ", p =", little_dates$p.value,"\n")
cat("amount of missing data:\n")
print(little_dates$missing.patterns)
sink()
####get some basics
table(sel_data[,c("Study","Group")])

#####Stastical tests for missing value patterns based on counts (only do this for dates)
sink("Missing_data_analysis_Spark2.1&2.2_0-5week_mITT_phq5_subj153.text")
sel_data$miss_count<-rowSums(is.na(sel_data[,grep("_time",colnames(sel_data))]))
sel_data$miss_count_bin<-ifelse(sel_data$miss_count==0,0,1) ##binary: 0 or non-zero
print("with Study group")
t.test(miss_count~Study,data=sel_data, var.equal = T)
table(sel_data[,c("Study","miss_count")])
chisq.test(table(sel_data[,c("Study","miss_count")]),simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("Study","miss_count_bin")])
chisq.test(table(sel_data[,c("Study","miss_count_bin")]),simulate.p.value = T)

cat("\n")
print("with Treatment group")
t.test(miss_count~Group,data=sel_data, var.equal = T)
table(sel_data[,c("Group","miss_count")])
chisq.test(table(sel_data[,c("Group","miss_count")]),simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("Group","miss_count_bin")])
chisq.test(table(sel_data[,c("Group","miss_count_bin")]),simulate.p.value = T)

cat("\n")
print("with Week")
data_date_miss<-as.table(rbind(colSums(is.na(data_date)),nrow(data_date)-colSums(is.na(data_date))))
print(data_date_miss)
print("A-missing; B-no missing")
chisq.test(data_date_miss,simulate.p.value = T)

cat("\n")
print("Treatment by Week")
temp<-as.table(rbind(colSums(is.na(sel_data[sel_data$Group=="Treatment",grep("_time",colnames(sel_data))])),
                     colSums(is.na(sel_data[sel_data$Group=="Control",grep("_time",colnames(sel_data))]))))
print(temp) 
print("A is Treatment group; B is Control group")
chisq.test(temp[,-1],simulate.p.value = T)

cat("\n")
print("relationship with the baseline severity")
cor.test(sel_data$Week0_totalscore,sel_data$miss_count,method = "spearman",exact = F)
table(sel_data[,c("Base_sev_cat","miss_count")])
chisq.test(table(sel_data[,c("Base_sev_cat","miss_count")])[-(1:2),],simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("Base_sev_cat","miss_count_bin")])
chisq.test(table(sel_data[,c("Base_sev_cat","miss_count_bin")])[-(1:2),],simulate.p.value = T)

cat("\n")
print("Severity by Week")
temp<-as.table(rbind(colSums(is.na(sel_data[sel_data$Base_sev_cat=="Mild",grep("_time",colnames(sel_data))])),
                     colSums(is.na(sel_data[sel_data$Base_sev_cat=="Moderate",grep("_time",colnames(sel_data))])),
                     colSums(is.na(sel_data[sel_data$Base_sev_cat=="Moderate_Severe",grep("_time",colnames(sel_data))])),
                     colSums(is.na(sel_data[sel_data$Base_sev_cat=="Severe",grep("_time",colnames(sel_data))]))))
print(temp) 
print("A-D: Mild, Moderate, Moderate-Severe, Severe")
chisq.test(temp[-1,-1], simulate.p.value = T)
print("Chi-sq test for group and baseline severity")
temp<-table(sel_data[,c("Group","Base_sev_cat")])
print(temp)
print("chisq.test with no and mild group")
chisq.test(temp[,-(1:2)],simulate.p.value = T)


cat("\n")
print("effect of age")
cor.test(sel_data$age,sel_data$miss_count,method = "spearman",exact = F)
table(sel_data[,c("age_grp","miss_count")])
chisq.test(table(sel_data[,c("age_grp","miss_count")]),simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("age_grp","miss_count_bin")])
chisq.test(table(sel_data[,c("age_grp","miss_count_bin")]),simulate.p.value = T)

cat("\n")
print("effect of gender")
table(sel_data[,c("gender","miss_count")])
chisq.test(table(sel_data[,c("gender","miss_count")]),simulate.p.value = T)
print("Binary for missing: 0-no missing; 1-missing")
table(sel_data[,c("gender","miss_count_bin")])
chisq.test(table(sel_data[,c("gender","miss_count_bin")]),simulate.p.value = T)
print("only male and female")
chisq.test(table(sel_data[sel_data$gender!="X (non-binary)",c("gender","miss_count_bin")]),simulate.p.value = T)
print("female vs. non-female")
chisq.test(as.table(matrix(c(74,24,35,20),2,2,byrow = T)),simulate.p.value = T)
sink()







