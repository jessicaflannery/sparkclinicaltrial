###p values for GLMM
p.glmm<-c(0.073,0.010,0.004,0.000)
p.glmm.adj<-p.adjust(p.glmm, method = "fdr")
p.glmm.df<-rbind.data.frame(p.glmm,p.glmm.adj)
colnames(p.glmm.df)<-c("mITT_phq10","PP_phq10","mITT_phq5","PP_phq5")
rownames(p.glmm.df)<-c("uncorr. p", "fdr-adj. p")

###p values for GLM
p.glm<-c(0.05830,0.012579,0.00265,0.000466)
p.glm.adj<-p.adjust(p.glm, method = "fdr")
p.glm.df<-rbind.data.frame(p.glm,p.glm.adj)
colnames(p.glm.df)<-c("mITT_phq10","PP_phq10","mITT_phq5","PP_phq5")
rownames(p.glm.df)<-c("uncorr. p", "fdr-adj. p")

sink("p-value_adjustment_for_interaction_effect.txt")
cat("p values for GLMM: Interaction\n")
print(p.glmm.df)
cat("\n")

cat("p values for GLM: Interaction\n")
print(p.glm.df)
cat("\n")
sink()
