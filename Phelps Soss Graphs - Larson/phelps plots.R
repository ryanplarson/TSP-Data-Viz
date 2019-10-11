#####################################################
###### Plots for Phelps 2017 Soss 
###### Ryan Larson
######################################################

library(ggplot2)
library(ggthemes)
library(RColorBrewer)

################################
### Figure 1
###############################

#Data
figure.1 <- read.csv(file="C:/Users/juicy/Documents/UMN/TSP/phelps_table1_genderdata.csv", stringsAsFactors = F)
figure.1$Response <- factor(figure.1$Response, levels = c("No further education level taken/completed","Remedial education (Grades 1-9)","High school education or GED training; no certificate/degree received",
                                                          "Received high school diploma or GED","Attended trade school, college, or university; no certificate/degree received",
                                                          "Certificate from a college or trade school","Associate degree",
                                                          "Bachelor's degree (e.g. BA, AB, BS) or advanced degree (e.g. MA, MD, PhD)"))


ggplot(data=figure.1, aes(x=Response, y=Percent, fill=Response))+
  geom_bar(position="dodge",stat="identity", colour="black")+
  stat_identity(geom = "text",
                aes(label = paste(Percent, "%")),
                hjust = -.5)+
  scale_fill_brewer(palette="Spectral", guide=FALSE)+
  facet_wrap(~ Gender)+
  coord_flip()+
  expand_limits(y=c(0,60)) +
  labs(title="Figure 1. Academic Education During Imprisonment", 
       x="", 
       y="Percent",
       subtitle="Highest level of completed education since admission to prison")+
  theme_classic()+
  scale_x_discrete(limits=rev(c("No further education level taken/completed","Remedial education (Grades 1-9)","High school education or GED training; no certificate/degree received",
                            "Received high school diploma or GED","Attended trade school, college, or university; no certificate/degree received",
                            "Certificate from a college or trade school","Associate degree",
                            "Bachelor's degree (e.g. BA, AB, BS) or advanced degree (e.g. MA, MD, PhD)")),
                   labels=rev(c("No further education level taken/completed","Remedial education (Grades 1-9)","High school education or GED training;\n no certificate/degree received",
                            "Received high school diploma or GED","Attended trade school, college, or university;\n no certificate/degree received",
                            "Certificate from a college or trade school","Associate degree",
                            "Bachelor's degree (e.g. BA, AB, BS) \n or advanced degree (e.g. MA, MD, PhD)")))+
  theme(axis.text.x = element_text(colour="grey20",size=11,face="plain"),
        axis.text.y = element_text(colour="grey20",size=11,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=14,face="bold"),
        plot.title= element_text(size=14, face="bold"), 
        plot.subtitle=element_text(size=11, face="italic"))

############################
###figure 2
##########################


#Data
figure.2 <- read.csv(file="C:/Users/juicy/Documents/UMN/TSP/phelps_table2_genderdata.csv", stringsAsFactors = F)
figure.2$Response <- factor(figure.2$Response, levels = c("Currently enrolled in educational program",
                                                          "Desire a formal degree/certificate"))
figure.2$Gender <- factor(figure.2$Gender, levels= c("Men","Women"))
figure.2

#figure 2
ggplot(data=figure.2, aes(x=Gender, y=Percent, fill=Gender))+
  geom_bar(position="dodge",stat="identity", colour="black")+
  facet_grid(.~Response)+
  stat_identity(geom = "text",
                aes(label = paste(Percent, "%")),
                vjust = -.5)+
  scale_fill_manual(values = c("#D53E4F","#3288BD"))+
  labs(title="Figure 2. Formal Degree/Certificate Programs in Prisons", 
       x="", 
       y="Percent",
       subtitle = "")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),  
        axis.title.y = element_text(colour="grey20",size=12,face="bold"),
        plot.title= element_text(size=14, face="bold"),
        legend.key.size = unit(1.5, 'lines'))


##################################
# Figure 3
###################################

#Data
figure.3 <- read.csv(file="C:/Users/juicy/Documents/UMN/TSP/phelps_table3_genderdata.csv", stringsAsFactors = F)
figure.3$Percent <- figure.3$ï..Percent
figure.3$Response <- factor(figure.3$Response, levels = c("Wanted to work","Sent to jail, prison or detention center","Did not like school or was bored in school",
                                                          "Expelled from school or asked to leave",
                                                          "Financial problems","Family reasons such as illness or death","Did not do well in school",
                                                          "Personal illness, disability, or pregnancy","Other or missing"))
figure.3

ggplot(data=figure.3, aes(x=Response, y=Percent, fill=Response))+
  geom_bar(position="dodge",stat="identity", colour="black")+
  stat_identity(geom = "text",
           aes(label = paste(Percent, "%")),
           hjust = -.5)+
  scale_fill_brewer(palette="Spectral", guide=FALSE)+
  facet_grid(~Gender)+
  coord_flip()+
  expand_limits(y=c(0,25)) +
  labs(title="Figure 3. Reasons for Leaving Education Before Prison", 
       x="", 
       y="Percent")+
  theme_classic()+
  scale_x_discrete(limits=rev(c("Wanted to work","Sent to jail, prison or detention center","Did not like school or was bored in school",
                                "Expelled from school or asked to leave",
                                "Financial problems","Family reasons such as illness or death","Did not do well in school",
                                "Personal illness, disability, or pregnancy","Other or missing")),
                   labels=rev(c("Wanted to work","Sent to jail, prison or detention center","Did not like school or was bored in school",
                                "Expelled from school or asked to leave",
                                "Financial problems","Family reasons such as illness or death","Did not do well in school",
                                "Personal illness, disability, or pregnancy","Other or missing")))+
  theme(axis.text.x = element_text(colour="grey20",size=11,face="plain"),
        axis.text.y = element_text(colour="grey20",size=11,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=14,face="bold"),
        plot.title= element_text(size=14, face="bold"), 
        plot.subtitle=element_text(size=11, face="italic"))


###################
# Figure 4
####################

#Data
figure.4 <- read.csv(file="C:/Users/juicy/Documents/UMN/TSP/phelps_table4_genderdata.csv", stringsAsFactors = F)
figure.4 <- figure.4[1:4,]
figure.4$Response <- factor(figure.4$Response, levels = c("Re-entry & vocational programs", "Parenting classes"))
figure.4$Gender <- factor(figure.4$Gender, levels= c("Men","Women"))
figure.4$Percent <- figure.4$ï..Percent
figure.4

#figure 4
ggplot(data=figure.4, aes(x=Gender, y=Percent, fill=Gender))+
  geom_bar(position="dodge",stat="identity", colour="black")+
  facet_grid(.~Response)+
  stat_identity(geom = "text",
                aes(label = paste(Percent, "%")),
                vjust = -.5)+
  scale_fill_manual(values = c("#D53E4F","#3288BD"))+
  labs(title="Figure 4. Other Program Participation During Imprisonment", 
       x="", 
       y="Percent",
       subtitle = "Percent reporting participation since admission to prison")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),  
        axis.title.y = element_text(colour="grey20",size=12,face="bold"),
        plot.title= element_text(size=14, face="bold"),
        legend.key.size = unit(1.5, 'lines'))

