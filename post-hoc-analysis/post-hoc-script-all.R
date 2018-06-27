#This script peforms violin/boxplots of mean-z scores per SDR, and also performs 
#correlations of mean-z scores and symptom scores

library("ggplot2")

#set current working directory
cwd = "/Users/thomasvanasse/Google Drive/RESEARCH/PTSD_RS_ICA/SHARED_DATA/Resting-State-Profile-PTSD"

#create data_frame and order levels
self_report = read.csv(file=sprintf("%s/post-hoc-analysis/subject_scores.csv", cwd))
self_report_df = data.frame(self_report)
self_report_df$Group <- factor(self_report_df$Group, levels=c('cc','tec','ptsd'))

#read SDR names file (SDR order to SDR name)
SDR_names = read.csv(file=sprintf("%s/SDR_names.csv", cwd), header = TRUE)

#loop through every SDR (49 in total)
i <- 1
for (i in 1:49){
mydata = read.table(sprintf("%s/post-hoc-analysis/SDR_mean_z/MEANZ_OUTPUT_SDR%d.txt", cwd, i))

#add the mean z-score to data
self_report_df$meanz <- mydata[1:105,]

print(sprintf("SDR_%d",i))

#Correlate PCL w/ all SDRs (only those with p < 0.001)
if (cor.test(self_report_df$PCL_V1,self_report_df$meanz)$p.value < 0.001){
  print("PCL p-value < 0.001")

  plot <- ggplot(self_report_df, aes(x=PCL_V1, y=meanz, color=Group))

  plot + geom_point(aes(shape=Group, color=Group)) + xlab("PCL scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$PCL_V1),2), cor.test(self_report_df$meanz,self_report_df$PCL_V1)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/post-hoc-analysis/ptsd_tec_cc_graphs/PCL_correlation_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}

#Correlate BDI w/ all SDRs 
if (cor.test(self_report_df$meanz,self_report_df$BDI_V1)$p.value < 0.001){
  print("BDI p-value < 0.001")
  
  plot <- ggplot(self_report_df, aes(x=BDI_V1, y=meanz, color=Group))
  
  plot + geom_point(aes(shape=Group, color=Group)) + xlab("BDI scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$BDI_V1),2), cor.test(self_report_df$meanz,self_report_df$BDI_V1)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/post-hoc-analysis/ptsd_tec_cc_graphs/PCL_correlation_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}

#Correlate BAI w/ all SDRs 
#Remove 2011's data point (they did not complete questionaire)
temp_df <- self_report_df[36,]
self_report_df <- self_report_df[-c(36), ]

if (cor.test(self_report_df$meanz,self_report_df$BAI_V1)$p.value < 0.001){
  print("BAI p-value < 0.001")
  
  plot <- ggplot(self_report_df, aes(x=BAI_V1, y=meanz, color=Group))
  
  plot + geom_point(aes(shape=Group, color=Group)) + xlab("BAI scores") + ylab("Mean z-score") +
    ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$BAI_V1),2), cor.test(self_report_df$meanz,self_report_df$BAI_V1)$p.value)) + 
    theme(plot.title= element_text(hjust = 0.5)) + 
    geom_smooth(aes(group = 1), method = "lm", colour="black") + 
    theme_gray(base_size = 18) + guides(fill=FALSE) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
    theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.position="none")
  ggsave(sprintf("%s/post-hoc-analysis/ptsd_tec_cc_graphs/BAI_correlation_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
}
#Add datapoint back in
self_report_df <- rbind(self_report_df, temp_df)
self_report_df <- self_report_df[order(self_report_df$GIFT_ID),]

cc <- data.frame(group = "cc", value= mydata[1:25,])
tec <- data.frame(group = "tec", value = mydata[77:105,])
ptsd <- data.frame(group = "ptsd", value = mydata[26:76,])
df_meanz <- rbind(cc,tec,ptsd)
df_meanz$group <- as.factor(df_meanz$group)

#geom_boxplot()
ggplot(df_meanz, aes(x=group, y=value, fill=group)) + ggtitle(sprintf("%s, Pattern: %s", SDR_names$SDR[i], SDR_names$Pattern[i]))  + 
  geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") +
  ylab("Mean z-score") + theme_gray(base_size = 18) + guides(fill=FALSE) + xlab(NULL) + theme(plot.title = element_text(hjust=0.5)) +
  theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
  theme(axis.text=element_text(size=26), axis.title=element_text(size=24,face="bold"), plot.title = element_text(size = 24, face = "bold"))
ggsave(sprintf("%s/post-hoc-analysis/ptsd_tec_cc_graphs/Boxplot_%s.png",cwd,SDR_names$SDR[i]), width = 7, height = 5, dpi = 300)


#remove meanz from self_report data frame for next iteration
self_report_df$meanz <- NULL
}


