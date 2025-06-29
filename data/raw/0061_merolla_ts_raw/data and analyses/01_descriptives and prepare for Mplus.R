
dat <- read.csv("./data Spring 2020.csv")


#### Methods ####

#### create data file with person level variables only

library(plyr)

pmeans <- ddply(dat, .(id), summarize,
                Path1_pre = mean(Path1_pre, na.rm=T),
                Path2_pre = mean(Path2_pre, na.rm=T),
                Path3_pre = mean(Path3_pre, na.rm=T),
                Path4_pre = mean(Path4_pre, na.rm=T),
                Agen1_pre = mean(Agen1_pre, na.rm=T),
                Agen2_pre = mean(Agen2_pre, na.rm=T),
                Agen3_pre = mean(Agen3_pre, na.rm=T),
                Agen4_pre = mean(Agen4_pre, na.rm=T),               
                Agency_pre = mean(Agency_pre, na.rm=T), 
                Pathways_pre = mean(Pathways_pre, na.rm=T), 
                Hope_pre = mean(Hope_pre, na.rm=T),
                Path1_post = mean(Path1_post, na.rm=T),
                Path2_post = mean(Path2_post, na.rm=T),
                Path3_post = mean(Path3_post, na.rm=T),
                Path4_post = mean(Path4_post, na.rm=T),
                Agen1_post = mean(Agen1_post, na.rm=T),
                Agen2_post = mean(Agen2_post, na.rm=T),
                Agen3_post = mean(Agen3_post, na.rm=T),
                Agen4_post = mean(Agen4_post, na.rm=T), 
                Pathways_post = mean(Pathways_post, na.rm=T), 
                Hope_post = mean(Hope_post, na.rm=T),
                
                LS1_pre = mean(LS1_pre, na.rm=T), 
                LS2_pre = mean(LS2_pre, na.rm=T), 
                LS3_pre = mean(LS3_pre, na.rm=T), 
                LS4_pre = mean(LS4_pre, na.rm=T), 
                LS5_pre = mean(LS5_pre, na.rm=T),
                LS_pre = mean(LS_pre, na.rm=T),
                LS1_post = mean(LS1_post, na.rm=T), 
                LS2_post = mean(LS2_post, na.rm=T), 
                LS3_post = mean(LS3_post, na.rm=T), 
                LS4_post = mean(LS4_post, na.rm=T), 
                LS5_post = mean(LS5_post, na.rm=T),
                LS_post = mean(LS_post, na.rm=T))

library(psych)

#### Scales

describe(pmeans$Hope_pre)
describe(pmeans$Hope_post)

alpha(pmeans[,c("Path1_pre","Path2_pre","Path3_pre","Path4_pre",
                "Agen1_pre","Agen2_pre","Agen3_pre","Agen4_pre")])
alpha(pmeans[,c("Path1_post","Path2_post","Path3_post","Path4_post",
                "Agen1_post","Agen2_post","Agen3_post","Agen4_post")])


describe(pmeans$LS_pre)
describe(pmeans$LS_post)

alpha(pmeans[,c("LS1_pre","LS2_pre","LS3_pre","LS4_pre","LS5_pre")])
alpha(pmeans[,c("LS1_post","LS2_post","LS3_post","LS4_post","LS5_post")])



#### differences between channels

dat[!is.na(dat$phone==0) & dat$phone==0 & dat$video==0 & dat$text==0, "channel"] <- "0_face"
dat[!is.na(dat$phone==0) & dat$phone==1, "channel"] <- "1_phone"
dat[!is.na(dat$phone==0) & dat$video==1, "channel"] <- "2_video"
dat[!is.na(dat$phone==0) & dat$text==1, "channel"] <- "3_text"


library(nlme)
library(emmeans)

mod <- lme(resp ~ factor(channel), random = ~1|id, data=dat, na.action="na.omit")
summary(mod)
anova(mod)
ems <- emmeans(mod, specs="channel")
ems
contrast(ems, method="pairwise", adjust="tukey")

mod <- lme(conn ~ factor(channel), random = ~1|id, data=dat, na.action="na.omit")
summary(mod)
anova(mod)
ems <- emmeans(mod, specs="channel")
ems
contrast(ems, method="pairwise", adjust="tukey")


dat$channel <- NULL


### Recode gender --> female

dat$female <- dat$gender-1



##### prepare full data for Mplus
library(MplusAutomation)

prepareMplusData(dat, "./Mplus/data Spring 2020.dat",
                 inpfile =  "./Mplus/input.inp")

