#This script peforms symptom score and FNC post-hoc correlations with 
#mean-z scores of PTSD data only

library("ggplot2")

#set current working directory
cwd = "/Users/thomasvanasse/Google Drive/RESEARCH/PTSD_RS_ICA/SHARED_DATA/Resting-State-Profile-PTSD/post-hoc-analysis"

#Create data_frame and order levels
self_report = read.csv(file=sprintf("%s/ptsd_only/subject_scores.csv", cwd))
self_report_df = data.frame(self_report)
self_report_df$Group <- factor(self_report_df$Group, levels=c('ptsd'))

#read SDR names file (SDR order to SDR name)
SDR_names = read.csv(file=sprintf("%s/../SDR_names.csv", cwd), header = TRUE)


i <- 1
for (i in 1:49){
mydata = read.table(sprintf("%s/SDR_mean_z/MEANZ_OUTPUT_SDR%d.txt",cwd, i))

#add the mean z-score to data
self_report_df$meanz <- mydata[26:76,]

print(sprintf("SDR_%d",i))

#Correlate PCL w/ all SDRs
if (cor.test(self_report_df$PCL_V1,self_report_df$meanz)$p.value < 0.05){
  print("PCL p-value < 0.05")

  plot <- ggplot(self_report_df, aes(x=PCL_V1, y=meanz, color=Group))

  plot + geom_point(colour='#00BFC4') + xlab("PCL scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$PCL_V1),2), cor.test(self_report_df$meanz,self_report_df$PCL_V1)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/PCL_correlation_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}

#Correlate BDI w/ all SDRs 
if (cor.test(self_report_df$meanz,self_report_df$BDI_V1)$p.value < 0.05){
  print("BDI p-value < 0.05")
  
  plot <- ggplot(self_report_df, aes(x=BDI_V1, y=meanz, color=Group))
  
  plot + geom_point(colour='#00BFC4') + xlab("BDI scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$BDI_V1),2), cor.test(self_report_df$meanz,self_report_df$BDI_V1)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/BDI_correlation_%s.png",cwd, SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}

#Correlate BAI w/ all SDRs (they did not complete questionaire)
temp_df <- self_report_df[11,]
self_report_df <- self_report_df[-c(11), ]

if (cor.test(self_report_df$meanz,self_report_df$BAI_V1)$p.value < 0.05){
  print("BAI p-value < 0.05")
  
  plot <- ggplot(self_report_df, aes(x=BAI_V1, y=meanz, color=Group))
  
  plot + geom_point(colour='#00BFC4') + xlab("BDI scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$BAI_V1),2), cor.test(self_report_df$meanz,self_report_df$BAI_V1)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/BAI_correlation_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}
#Add datapoint back in
self_report_df <- rbind(self_report_df, temp_df)
self_report_df <- self_report_df[order(self_report_df$GIFT_ID),]

#Correlate PSS_Re w/ all SDRs 
if (cor.test(self_report_df$meanz,self_report_df$PSS_RE)$p.value < 0.05){
  print("PSS_RE p-value < 0.05")
  
  plot <- ggplot(self_report_df, aes(x=PSS_RE, y=meanz, color=Group))
  
  plot + geom_point(colour='#00BFC4') + xlab("PSSI - Re-experiencing Scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$PSS_RE),2), cor.test(self_report_df$meanz,self_report_df$PSS_RE)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/PSS_reexperiencing_%s.png",cwd, SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}


#Correlate PSS_AV w/ all SDRs 
if (cor.test(self_report_df$meanz,self_report_df$PSS_AV)$p.value < 0.05){
  print("PSS_AV p-value < 0.05")
  
  plot <- ggplot(self_report_df, aes(x=PSS_AV, y=meanz, color=Group))
  
  plot + geom_point(colour='#00BFC4') + xlab("PSSI - Avoidance Scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$PSS_AV),2), cor.test(self_report_df$meanz,self_report_df$PSS_AV)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/PSS_avoidance_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}

#Correlate PSS_AR w/ all SDRs 
if (cor.test(self_report_df$meanz,self_report_df$PSS_AR)$p.value < 0.05){
  print("PSS_AR p-value < 0.05")
  
  plot <- ggplot(self_report_df, aes(x=PSS_AR, y=meanz, color=Group))
  
  plot + geom_point(colour='#00BFC4') + xlab("PSSI - Arousal Scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$PSS_AR),2), cor.test(self_report_df$meanz,self_report_df$PSS_AR)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/PSS_arousal_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}

#Correlate PSS_TOT w/ all SDRs 

if (cor.test(self_report_df$meanz,self_report_df$PSS_TOT)$p.value < 0.05){
  print("PSS_TOT p-value < 0.05")
  
  plot <- ggplot(self_report_df, aes(x=PSS_TOT, y=meanz, color=Group))
  
  plot + geom_point(colour='#00BFC4') + xlab("PSSI Total Scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$PSS_TOT),2), cor.test(self_report_df$meanz,self_report_df$PSS_TOT)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/PSS_total_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}

#remove meanz from self_report data frame for next iteration
self_report_df$meanz <- NULL

}

#Correlate FNCs
if (cor.test(self_report_df$vspatial_sens2_z,self_report_df$PSS_AV)$p.value < 0.05){
  print("FNC p-value < 0.05")
  
  lm.m <- lm(self_report_df$vspatial_sens2_z~self_report_df$PSS_AV)
  cooksd <- cooks.distance(lm.m)
  
  plot <- ggplot(self_report_df, aes(x=PSS_AV, y=vspatial_sens2_z, color=Group))
  
  plot + geom_point(colour='#00BFC4') + xlab("Avoidance") + ylab("FNC (Fisher z)") +
    ggtitle(sprintf("r=%g (p=%.2e)",  round(cor(self_report_df$PSS_AV,self_report_df$vspatial_sens2_z),2), cor.test(self_report_df$vspatial_sens2_z,self_report_df$PSS_AV)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/FNC_(IC-13,IC-2).png",cwd), width = 5, height = 5, dpi = 300)
}


if (cor.test(self_report_df$rfp_sens2_z,self_report_df$PSS_AR)$p.value < 0.05){
  print("FNC p-value < 0.05")
  
  plot <- ggplot(self_report_df, aes(x=PSS_AR, y=rfp_sens2_z, color=Group))
  
  plot + geom_point(colour='#00BFC4') + xlab("Arousal") + ylab("FNC (Fisher z)") +
    theme(plot.title= element_text(hjust = 0.5)) + 
    ggtitle(sprintf("r=%g (p=%.2e)",  round(cor(self_report_df$PSS_AR,self_report_df$rfp_sens2_z),2), cor.test(self_report_df$rfp_sens2_z,self_report_df$PSS_AR)$p.value)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/ptsd_only/FNC_(IC-9,IC-2).png",cwd), width = 5, height = 5, dpi = 300)
}


