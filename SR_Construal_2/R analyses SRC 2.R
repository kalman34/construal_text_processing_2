setwd("C:/Users/Kal Victor/Google Drive/Columbia/SR_Construal_2")

src <- read.csv("Construal_SR_2_170613_merged.csv")
####### EXCLUSIONS ######
# 225 completed the study

#exclude attnc @207#
src <- src [which(src$attn1 == "reading")]
# final count: 207 participants #

src<- subset(src, attn1=="reading")

library(mediation)
library(psych)
library(ggplot2)
theme_set(theme_bw(base_size = 28)) 
library(rockchalk)
library (car)



src$vmessage<-as.numeric(src$vmessage)
src$vrecall<-as.numeric(src$vrecall)

src$Michael1<-as.factor(src$Michael1)
src$likem<-as.factor(src$likem)

srlike_Michael1 <- subset(srlike, Michael1==1)
srdislike_Michael1 <- subset(srdislike, Michael1==1)

srlike_Michael2 <- subset(srlike, Michael1==0)
srdislike_Michael2 <- subset(srdislike, Michael1==0)
#########

srlike <- subset(src, likem==1)
srdislike <- subset(src, likem==0)

mc_neg <- subset (src, Michael1==1)
mc_pos <- subset (src, Michael1==0)

summary (lm (mvalence~Michael1, src))
summary (lm (rvalence~Michael1, src)) 

summary (lm (mvalence~likem, src))
summary (lm (rvalence~likem, src)) 


summary(lm (mvalence ~likem + Michael1, data=src))
summary(lm (rvalence ~likem + Michael1, data=src))

summary(lm (mvalence ~likem*Michael1, data=src))
summary(lm (rvalence ~likem*Michael1, data=src))

summary (lm (msg_sum~likem, src))
summary (lm (msg_sum~Michael1, src))

summary (lm (recall_sum~Michael1, src))
summary (lm (recall_sum~likem, src))

summary (lm (msg_sum~likem + Michael1, src))
summary (lm (msg_sum~likem*Michael1, src))

summary (lm (recall_sum~likem*Michael1, src))
summary (lm (recall_sum~likem + Michael1, src))


summary (lm (msg_sum~likem, mc_pos))
summary (lm (msg_sum~likem, mc_neg))

summary (lm (msg_sum~Michael1, srlike))
summary (lm (msg_sum~Michael1, srdislike))

summary (lm (recall_sum~likem, mc_pos))
summary (lm (recall_sum~Michael1, srlike))
summary (lm (recall_sum~Michael1, srdislike))
summary (lm (recall_sum~likem, mc_neg))


summary (lm (msg_sum~likem, mc_pos))
summary (lm (msg_sum~likem, mc_neg))

##############################
#6/13/17


ggplot(src, aes(likem, msg_sum)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=likem), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=likem), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  facet_wrap(~Michael1)
  

srlike_Michael1
srdislike_Michael1 

srlike_Michael2
srdislike_Michael2

#effects of manips on relational trust and epistemic etc.
