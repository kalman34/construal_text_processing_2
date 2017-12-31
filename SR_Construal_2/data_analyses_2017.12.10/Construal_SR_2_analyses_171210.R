#### set up ####
setwd("~/Google Drive/Columbia/RESEARCH/CURRENT/SR_Construal/SR_Construal_2")

src <- read.csv(file="Construal_SR_2_valence_construal_merged_171210.csv")

####### EXCLUSIONS ######

# 225 completed the study

# failed attn1 (survey): at 207
src <- src[ which(src$attn1 == "reading") , ]


# exclusions (wrote about Sam, or tech issue): at 198
src <- src[ which(is.na(src$excl == T)) , ]

#### COMPUTING #####

attach(src)

# closeness measure
src$closeness <- (((8-close_1) + (8-close_2) + close_3 + close_4 + (8-close_5) + (8-close_6) + close_7 + close_8)/8)

# Relational Trust
src$reltrust = (reltrust_1 + reltrust_2 + reltrust_3 + reltrust_4)/4

# Epistemic Trust
src$epistrust = (epistrust_1 + epistrust_2 + epistrust_3 + epistrust_4)/4

# message trust
src$messtrust = (messtrust_1 + messtrust_2 + messtrust_3 + messtrust_4)/4

# valence change
src$vchange <- src$rvalence-src$mvalence

detach(src)

src$vmessage<-as.numeric(src$mvalence)
src$vrecall<-as.numeric(src$rvalence)


#RECODING

#audience likes michael
src$likem[is.na(src$likem)==T] <- 0
src$aud.like <- src$likem 

#Michael1
src$Michael1[is.na(src$Michael1)==T] <- 0

# SUBSETS

srlike <- subset(src, likem==1)
srdislike <- subset(src, likem==0)

mc_neg <- subset (src, Michael1==1)
mc_pos <- subset (src, Michael1==0)


###### SAYING IS BELIEIVNG #####

summary(lm(mvalence ~ likem, data=src))
summary(lm(rvalence ~ likem, data=src))

summary(lm (mvalence ~likem + Michael1, data=src))
summary(lm (rvalence ~likem + Michael1, data=src))

summary(lm (mvalence ~likem*Michael1, data=src))
summary(lm (rvalence ~likem*Michael1, data=src))


#### CONSTRUAL ####

# message
summary (lm (msg_sum~likem, src))
summary (lm (msg_sum~Michael1, src))

summary (lm (msg_sum~likem + Michael1, src))
summary (lm (msg_sum~likem*Michael1, src))

summary (lm (msg_avg~likem, src))
summary (lm (msg_avg~Michael1, src))

summary (lm (msg_avg~likem + Michael1, src))
summary (lm (msg_avg~likem*Michael1, src))

# recall
summary (lm (rec_sum~Michael1, src))
summary (lm (rec_sum~likem, src))

summary (lm (rec_sum~likem*Michael1, src))
summary (lm (rec_sum~likem + Michael1, src))

summary (lm (rec_avg~Michael1, src))
summary (lm (rec_avg~likem, src))

summary (lm (rec_avg~likem*Michael1, src))
summary (lm (rec_avg~likem + Michael1, src))

