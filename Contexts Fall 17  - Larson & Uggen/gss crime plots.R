####################################################
#### GSS Crime Trends Contexts 2017
#### Ryan Larson and Chris Uggen
###################################################

library(foreign)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(srvyr)
options(survey.lonely.psu = "adjust") #center variance contribution around grand mean
#options(survey.lonely.psu = "certainty") #make no contribution to variance

#######################################
#read in data/data maniplation
#############################
gss <- read.dta(file="C:/Users/DELL/Documents/UMN/GSS_stata/GSS7216_R1a.dta", convert.factors = F,
                missing.type = F)
gss <- gss %>% filter(!is.na(vpsu)) %>% mutate(weight=ifelse(year<2004, wtssall, wtssnr)) %>%
  mutate(weight.courts = ifelse(year==1973|year==1982, formwt, weight))

ucr <- read.csv(file="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/UCR crime 1975-2014.csv")
ucr <- ucr %>% select_("Year", "Violent.Crime.rate", "Property.crime.rate") %>% 
  mutate(crime.rate=Violent.Crime.rate+Property.crime.rate) %>% filter(!is.na(Year)) %>% rename(year=Year)

bjs <- read.csv(file="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/bjs imprisonment.csv")

ex <- read.csv(file="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/execution_database.csv", stringsAsFactors = F)
ex <- ex %>% mutate(Date=as.Date(Date, format="%m/%d/%Y")) %>% mutate(year=as.numeric(format(Date, format = "%Y"))) %>% 
  group_by(year) %>% summarize(exec=n())
year <- c(1975,1976, 1978, 1980)
exec <- c(rep(0,4))
miss <- cbind(year,exec)
ex <- rbind(ex, miss) %>% filter(!is.na(exec), !is.na(year))
ex <- ex[order(ex$year),]
##########################################
#Figure 1: yearly weighted fear by sex; UCR crime rate superimposed
##########################################
#gss design-corrected data - weighted for fear
gss.design.fear <- gss %>% 
  select_("year","fear","sex","sample","weight", "vpsu", "vstrat") %>% 
  mutate(sample=as.numeric(sample)) %>% 
  filter(!(sample==4|sample==5|sample==7)) %>% #removing 1982 and 1987 black oversample
  mutate(fear=as.numeric(fear)) %>%
  mutate(fear=replace(fear, fear==2, 0)) %>%
  as_survey_design(ids= vpsu,weights= weight,strata= vstrat, nest=T)

fear.gen <- gss.design.fear %>% 
  filter(!is.na(fear)) %>%
  mutate(sex = ifelse(sex==1, "Male", "Female")) %>%
  group_by(year, sex) %>% 
  summarize(prop = survey_mean(fear, vartype=c("se", "ci"))) %>%
  mutate(perc=100*prop, perc.se=100*prop_se, 
         ci.l=100*prop_low, ci.h=100*prop_upp)
fear.gen <-  left_join(fear.gen, ucr, by="year")

write.csv(fear.gen, file = "C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/fear.csv")


ggplot(fear.gen)+  
  geom_line(aes(x=year, y=perc, group=sex, linetype=sex), size=1)+
  scale_linetype_manual(values=c("twodash","dotted"))+
  geom_line(aes(x=year, y=crime.rate/60, col="arrests \n per 100,000"), size=1.5, lty="solid")+
  geom_ribbon(aes(x=year, ymin=ci.l, ymax=ci.h, fill=sex), alpha=.5)+
  scale_fill_grey()+
  scale_color_grey()+
  ggtitle("GSS Crime Fears, 1975-2016", subtitle = "Afraid to walk at night in neighborhood?")+
  labs(colour="UCR", fill="GSS: Fear", linetype="GSS: Fear")+
  scale_x_continuous(limits=c(1975,2016), breaks=seq(1975,2016,5))+
  scale_y_continuous(sec.axis=sec_axis(~.*60, name="Arrest Rate per 100,000 (UCR)"))+
  xlab("Year")+ylab("Percent of Population (GSS)")+
  theme_bw()

ggplot(fear.gen)+  
  geom_line(aes(x=year, y=perc, group=sex, linetype=sex), size=.5)+
  scale_linetype_manual(values=c("twodash","dotted"))+
  geom_line(aes(x=year, y=crime.rate/60, col="arrests \n per 100,000"), size=.5, lty="solid")+
  geom_ribbon(aes(x=year, ymin=ci.l, ymax=ci.h, fill=sex), alpha=.5)+
  scale_fill_grey()+
  scale_color_grey()+
  ggtitle("GSS Crime Fears by Gender, 1975-2016", subtitle = "Afraid to walk at night in neighborhood?")+
  labs(colour="UCR", fill="GSS: Fear", linetype="GSS: Fear")+
  scale_x_continuous(limits=c(1975,2016), breaks=seq(1975,2016,5))+
  scale_y_continuous(sec.axis=sec_axis(~.*60))+
  xlab("")+ylab("")+
  theme_hc()+
  theme(legend.position = "none",  text = element_text(size=5, family="arial"))+
  annotate("text", label="UCR Arrest Rate per 100,000", x=2005, y=85, size=1.5)+
  annotate("text", label="Male", x=1978, y=30, size=1.5)+
  annotate("text", label="Female", x=1978, y=72, size=1.5)
 

  
ggsave(filename="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/1.tiff", 
       width=4.4,height=2.15, units="in",dpi=300, device="tiff")
ggsave(filename="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/1.eps", 
       width=4.4,height=2.15, units="in",dpi=300, device="eps")

########################################################
#Figure2: courts, incarceration superimposed - change to courts total not broken down by race
#######################################################



#gss design-corrected data - weighted for courts
gss.design.court <- gss %>% 
  select_("year","sample","weight", "vpsu", "vstrat", 
          "courts", "weight.courts") %>% 
  mutate(sample=as.numeric(sample)) %>% 
  filter(!(sample==4|sample==5|sample==7)) %>% #removing 1982 and 1987 black oversample
  mutate(courts = as.numeric(courts)) %>%
  mutate(courts = replace(courts, courts==2|courts==3,0)) %>%
  as_survey_design(ids= vpsu,weights= weight.courts,strata= vstrat, nest=T)

courts <- gss.design.court %>% 
  filter(!is.na(courts)) %>%
  group_by(year) %>% 
  summarize(prop = survey_mean(courts, vartype=c("se", "ci"))) %>%
  mutate(perc=100*prop, perc.se=100*prop_se, 
         ci.l=100*prop_low, ci.h=100*prop_upp) 
courts <-  left_join(courts, bjs, by="year")
courts$courts <- rep("Courts", length(courts$year))

write.csv(courts, file = "C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/courts.csv")



ggplot(courts)+  
  geom_line(aes(x=year, y=perc, lty=courts), size=1)+
  scale_linetype_manual(values=c("dashed"))+
  geom_line(aes(x=year, y=rate/10, col="imprisonment \n per 100,000"), size=)+
  scale_color_grey()+
  geom_ribbon(aes(x=year, ymin=ci.l, ymax=ci.h), alpha=.5)+
  ggtitle("GSS Courts, 1975-2016", subtitle="Are Courts too Harsh?")+
  scale_x_continuous(limits=c(1975,2016), breaks=seq(1975,2016,5))+
  scale_y_continuous(limits=c(0,60),sec.axis=sec_axis(~.*10, name="Imprisonment per 100,000 (BJS)"))+
  xlab("Year")+ylab("Percent of Population (GSS)")+
  labs(colour="BJS", linetype="GSS")+
  theme_bw()  

ggplot(courts)+  
  geom_line(aes(x=year, y=perc, lty=courts), size=.5)+
  scale_linetype_manual(values=c("dashed"))+
  geom_line(aes(x=year, y=rate/10, col="imprisonment \n per 100,000"), size=.5)+
  scale_color_grey()+
  geom_ribbon(aes(x=year, ymin=ci.l, ymax=ci.h), alpha=.5)+
  ggtitle("GSS Courts, 1975-2016", subtitle="Are Courts too Harsh?")+
  scale_x_continuous(limits=c(1975,2016), breaks=seq(1975,2016,5))+
  scale_y_continuous(limits=c(0,60),sec.axis=sec_axis(~.*10))+
  xlab("")+ylab("")+
  theme_hc()+
  theme(legend.position = "none",  text = element_text(size=5, family="arial"))+
  annotate("text", label="BJS Imprisonment per 100,000", x=1990, y=45, size=1.5)+
  annotate("text", label="GSS Percent", x=1983, y=8, size=1.5)

ggsave(filename="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/2.tiff", 
       width=4.4,height=2.15, units="in",dpi=300, device="tiff")
ggsave(filename="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/2.eps", 
       width=4.4,height=2.15, units="in",dpi=300, device="eps")
#######################################################
#Figure 3: polhitok (approval) by race
#########################################

#gss design-corrected data - weighted for polhitok
gss.design.pol <- gss %>% 
  select_("year","sample","weight", "vpsu", "vstrat", 
           "race", "polhitok") %>% 
  mutate(sample=as.numeric(sample)) %>% 
  filter(!(sample==4|sample==5|sample==7)) %>% #removing 1982 and 1987 black oversample
  mutate(polhit=as.numeric(polhitok)) %>%
  mutate(polhit=replace(polhit, polhit==2, 0)) %>%  
  as_survey_design(ids= vpsu,weights= weight,strata= vstrat, nest=T)

pol.race <- gss.design.pol %>% 
  filter(!is.na(polhit)) %>%
  group_by(year, race) %>% 
  summarize(prop = survey_mean(polhit, vartype=c("se", "ci"))) %>%
  mutate(perc=100*prop, perc.se=100*prop_se, 
         ci.l=100*prop_low, ci.h=100*prop_upp) %>%
  mutate(race = ifelse(race==1, "White", ifelse(race==2, "Black","Other")))



#temp removing bounds for other category
pol <- pol.race
pol$ci.h[pol$race=="Other"] <- NA
pol$ci.l[pol$race=="Other"] <- NA

pol.race$ci.l[pol.race$race=="Other"& pol.race$year==1975] <- 0
pol.race$ci.h[pol.race$race=="Other"& pol.race$year==1976] <- 100

write.csv(pol, file = "C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/pol.csv")



ggplot(pol)+  
  geom_line(aes(x=year, y=perc, group=race, linetype=race), size=1)+
  scale_linetype_manual(values=c("solid","twodash","dotted"))+
  geom_ribbon(aes(x=year, ymin=ci.l, ymax=ci.h, fill=race), alpha=.5)+
  scale_fill_grey()+
  ggtitle("GSS Police Force, 1975-2016", subtitle="Approve of policeman striking male citizen?")+
  labs(fill="GSS: Force", linetype="GSS: Force")+
  scale_x_continuous(limits=c(1975,2016), breaks=seq(1975,2016,5))+
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,20))+
  xlab("Year")+ylab("Percent of Population (GSS)")+
  theme_bw()  

ggplot(pol)+  
  geom_line(aes(x=year, y=perc, group=race, linetype=race), size=.5)+
  scale_linetype_manual(values=c("solid","twodash","dotted"))+
  geom_ribbon(aes(x=year, ymin=ci.l, ymax=ci.h, fill=race), alpha=.5)+
  scale_fill_grey()+
  ggtitle("GSS Police Force by Race, 1975-2016", subtitle="Approve of policeman striking male citizen?")+
  labs(fill="GSS: Force", linetype="GSS: Force")+
  scale_x_continuous(limits=c(1975,2016), breaks=seq(1975,2016,5))+
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,20))+
  xlab("")+ylab("")+
  theme_hc()+
  theme(legend.position = "none",  text = element_text(size=5, family="arial"))+
  annotate("text", label="White", x=1980, y=90, size=1.5)+
  annotate("text", label="Black", x=1977, y=35, size=1.5)+
  annotate("text", label="Other", x=1979, y=70, size=1.5)

ggsave(filename="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/3.tiff", 
       width=4.4,height=2.15, units="in",dpi=300, device="tiff")
ggsave(filename="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/3.eps", 
       width=4.4,height=2.15, units="in",dpi=300, device="eps")


##############################################
### Death penalty by political affiliation (3-cat) overlay executions from wash po dec. 24 2016 - death penalty information center
##############################################
#gss design-corrected data - weighted for cappun
gss.design.cap <- gss %>% 
  select_("year","sample","weight", "vpsu", "vstrat", 
          "partyid", "cappun") %>% 
  mutate(sample=as.numeric(sample)) %>% 
  filter(!(sample==4|sample==5|sample==7)) %>% #removing 1982 and 1987 black oversample
  mutate(cappun=as.numeric(cappun)) %>%
  mutate(cappun=replace(cappun, cappun==2, 0)) %>%  
  as_survey_design(ids= vpsu,weights= weight,strata= vstrat, nest=T)

gss.design.cap$variables$party <- rep(NA, length(gss.design.cap$variables$partyid))
gss.design.cap$variables$party[gss.design.cap$variables$partyid==0] <- "Democrat"
gss.design.cap$variables$party[gss.design.cap$variables$partyid==1] <- "Democrat"
gss.design.cap$variables$party[gss.design.cap$variables$partyid==2] <- "Independent, other"
gss.design.cap$variables$party[gss.design.cap$variables$partyid==3] <- "Independent, other"
gss.design.cap$variables$party[gss.design.cap$variables$partyid==4] <- "Independent, other"
gss.design.cap$variables$party[gss.design.cap$variables$partyid==5] <- "Republican"
gss.design.cap$variables$party[gss.design.cap$variables$partyid==6] <- "Republican"
gss.design.cap$variables$party[gss.design.cap$variables$partyid==7] <- "Independent, other"
table(gss.design.cap$variables$party)

cap.pol <- gss.design.cap %>% 
  filter(!is.na(cappun)) %>%
  group_by(year, party) %>% 
  summarize(prop = survey_mean(cappun, vartype=c("se", "ci"))) %>%
  mutate(perc=100*prop, perc.se=100*prop_se, 
         ci.l=100*prop_low, ci.h=100*prop_upp) 
cap.pol <-  left_join(cap.pol, ex, by="year")

write.csv(cap.pol, file = "C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/cap.pol.csv")




ggplot(cap.pol)+  
  geom_line(aes(x=year, y=perc, group=party, linetype=party), size=1)+
  scale_linetype_manual(values=c("longdash","twodash","dotted"))+
  geom_line(aes(x=year, y=exec, col="executions"), size=1.5)+
  scale_color_manual(values=c("black"))+
  scale_fill_grey()+
  geom_ribbon(aes(x=year, ymin=ci.l, ymax=ci.h, fill=party), alpha=.5)+
  ggtitle("GSS Capital Punishment, 1975-2016", subtitle = "Do you favor or oppose the death penalty for persons convicted of murder?")+
  labs(colour="DPIC", fill="GSS: Cappun", linetype="GSS: Cappun")+
  scale_x_continuous(limits=c(1975,2016), breaks=seq(1975,2016,5))+
  scale_y_continuous(limits=c(0,100),sec.axis=sec_axis(~., name="Executions by Year"))+
  xlab("Year")+ylab("Percent of Population (GSS)")+
  theme_bw()

ggplot(cap.pol)+  
  geom_line(aes(x=year, y=perc, group=party, linetype=party), size=.5)+
  scale_linetype_manual(values=c("longdash","twodash","dotted"))+
  geom_line(aes(x=year, y=exec, col="executions"), size=.5)+
  scale_color_manual(values=c("black"))+
  scale_fill_grey()+
  geom_ribbon(aes(x=year, ymin=ci.l, ymax=ci.h, fill=party), alpha=.5)+
  ggtitle("GSS Capital Punishment by Party ID, 1975-2016", subtitle = "Do you favor or oppose the death penalty for persons convicted of murder?")+
  labs(colour="DPIC", fill="GSS: Cappun", linetype="GSS: Cappun")+
  scale_x_continuous(limits=c(1975,2016), breaks=seq(1975,2016,5))+
  scale_y_continuous(limits=c(0,100),sec.axis=sec_axis(~.))+
  xlab("")+ylab("")+
  theme_hc()+
  theme(legend.position = "none",  text = element_text(size=5, family="arial"))+
  annotate("text", label="DPIC Executions", x=1998, y=30, size=1.5)+
  annotate("text", label="Democrat", x=2016, y=42, size=1.5)+
  annotate("text", label="Independent, other", x=2014, y=73, size=1.5)+
  annotate("text", label="Republican", x=2013, y=93, size=1.5)

ggsave(filename="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/4.tiff", 
       width=4.4,height=2.15, units="in",dpi=300, device="tiff")
ggsave(filename="C:/Users/DELL/Documents/UMN/UMN Projects/GSS Crime Trends/4.eps", 
       width=4.4,height=2.15, units="in",dpi=300, device="eps")
