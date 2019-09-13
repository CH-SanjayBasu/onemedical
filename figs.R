setwd("~/Box/Analytics Team/Research/Research projects/One medical")
library(ggplot2)

#1a

x = c("one", "two", "three","four", "five", "six","seven", "eight", "nine","ten", "eleven", "twelve")
y = c(-26.8,-35.1,-16.1,48.3,-57.5,3.8,2.4,-33.8,-29.3,-48.6,-30.1,-39.5)
se = -c(-4.948979592,-6.173469388,-3.214285714,-3.826530612,-4.285714286,-3.775510204,-4.642857143,-5.663265306,-4.489795918,-5.816326531,-4.387755102,-4.540816327)
ylo = y - 1.96*se
yhi = y + 1.96*se
lab = c("Total Spend", "Emergency", "Hospital", "Primary Care", "Specialist", "Mental Health", "Physical Therapy", "Rx", "Drug admin", "Surgery", "Labs", "Radiology")
labpos = c(rep(-110,12))  
grouping = factor(c("#1B9E77", "#D95F02", "#D95F02" , "#7570B3", "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#66A61E" ,"#E6AB02","#E6AB02"))

test <- data.frame(x, y, se, ylo, yhi, lab, labpos, grouping)


test$x <- reorder(test$x, c(12,11,10,9,8,7,6,5,4,3,2,1))

ggplot(test, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=grouping)) +
  geom_pointrange(shape=15, size=0.7, position=position_dodge(width=c(0.1))) +
  coord_flip() +
  geom_hline(aes(yintercept=0), lty=2) +
  xlab('Category') +
  scale_x_discrete(breaks=NULL) +
#  scale_y_continuous(limits = c(-110,170), breaks = seq(from=-100, to=200, by=50)) +
  theme_bw() +
  ylab("% Difference with On-site/Near-site Primary Care Clinic Use") +
  xlab("") +
  ggtitle("Spending") +
  theme(legend.position="none")+
  geom_text(data=test, aes(x = x, y = labpos, label = lab, hjust=0)) + scale_color_brewer(palette = "Dark2")

# 1b

x = c("one", "two", "three","four", "five", "six","seven", "eight", "nine","ten")
y = c(-2.4,-0.6,-2.5,-5.8,-0.1,-4.2,-6.6,-4.4,-7,-3.2)
se = -c(-0.510204082,-0.153061224,-0.56122449,-0.357142857,-0.357142857,-1.071428571,-0.612244898,-0.408163265,-1.020408163,-0.306122449)    
ylo = y - 1.96*se
yhi = y + 1.96*se
lab = c("Emergency", "Hospital", "Primary Care", "Specialist", "Mental Health", "Physical Therapy", "Drug admin", "Surgery", "Labs", "Radiology")
labpos = c(rep(-12,10))  
grouping = factor(c( "#D95F02", "#D95F02" , "#7570B3", "#7570B3", "#7570B3", "#7570B3",  "#E7298A", "#66A61E" , "#E6AB02","#E6AB02"))

test <- data.frame(x, y, se, ylo, yhi, lab, labpos, grouping)



test$x <- reorder(test$x, c(10,9,8,7,6,5,4,3,2,1))

ggplot(test, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=grouping)) +
  geom_pointrange(shape=15, size=0.7, position=position_dodge(width=c(0.1))) +
  coord_flip() +
  geom_hline(aes(yintercept=0), lty=2) +
  xlab('Category') +
  scale_x_discrete(breaks=NULL) +
#  scale_y_continuous(limits = c(-5,20), breaks = seq(from=-5, to=20, by=5)) +
  theme_bw() +
  ylab("% Difference with On-site/Near-site Primary Care Clinic Use") +
  xlab("") +
  ggtitle("Utilization") +
  theme(legend.position="none")+
  geom_text(data=test, aes(x = x, y = labpos, label = lab, hjust=0)) + scale_color_brewer(palette = "Dark2")


#1c

x = c("one", "two", "three","four", "five", "six","seven", "eight", "nine","ten")
y = c(2.2,213.6,25,11.8,1,43.1,12.2,-19.7,7.2,-15.9)
se = -c(-2.704081633,-7.806122449,-1.326530612,-2.193877551,-6.071428571,-2.602040816,-3.673469388,-6.479591837,-2.602040816,-4.234693878)   
ylo = y - 1.96*se
yhi = y + 1.96*se
lab = c("Emergency", "Hospital", "Primary Care", "Specialist", "Mental Health", "Physical Therapy", "Drug admin", "Surgery", "Labs", "Radiology")
labpos = c(rep(-110,10))  
grouping = factor(c( "#D95F02", "#D95F02" , "#7570B3", "#7570B3", "#7570B3", "#7570B3",  "#E7298A", "#66A61E" , "#E6AB02","#E6AB02"))

test <- data.frame(x, y, se, ylo, yhi, lab, labpos, grouping)



test$x <- reorder(test$x, c(10,9,8,7,6,5,4,3,2,1))

ggplot(test, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=grouping)) +
  geom_pointrange(shape=15, size=0.7, position=position_dodge(width=c(0.1))) +
  coord_flip() +
  geom_hline(aes(yintercept=0), lty=2) +
  xlab('Category') +
  scale_x_discrete(breaks=NULL) +
#  scale_y_continuous(limits = c(-110,170), breaks = seq(from=-100, to=200, by=50)) +
  theme_bw() +
  ylab("% Difference with On-site/Near-site Primary Care Clinic Use") +
  xlab("") +
  ggtitle("Spend per Episode") +
  theme(legend.position="none")+
  geom_text(data=test, aes(x = x, y = labpos, label = lab, hjust=0)) + scale_color_brewer(palette = "Dark2")



#2a

x = c("one", "two", "three","four", "five", "six","seven", "eight", "nine","ten", "eleven", "twelve")
y = c(-110.5098,-15.28605,-5.0715,7.2933,-16.58875,0.2831,0.2028,-4.225,-3.12045,-19.197,-3.5217,-4.345)
se = -c(-20.40711735,-2.688545918,-1.0125,-0.577806122,-1.236428571,-0.28127551,-0.392321429,-0.707908163,-0.478163265,-2.29744898,-0.513367347,-0.499489796)
ylo = y - 1.96*se
yhi = y + 1.96*se
lab = c("Total Spend", "Emergency", "Hospital", "Primary Care", "Specialist", "Mental Health", "Physical Therapy", "Rx", "Drug admin", "Surgery",  "Labs", "Radiology")
labpos = c(rep(-50,12))  
grouping = factor(c("#1B9E77", "#D95F02", "#D95F02" , "#7570B3", "#7570B3", "#7570B3", "#7570B3", "#E7298A", "#E7298A", "#66A61E" , "#E6AB02","#E6AB02"))

test <- data.frame(x, y, se, ylo, yhi, lab, labpos, grouping)


test$x <- reorder(test$x, c(12,11,10,9,8,7,6,5,4,3,2,1))

ggplot(test, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=grouping)) +
  geom_pointrange(shape=15, size=0.7, position=position_dodge(width=c(0.1))) +
  coord_flip() +
  geom_hline(aes(yintercept=0), lty=2) +
  xlab('Category') +
  scale_x_discrete(breaks=NULL) +
 # scale_y_continuous(limits = c(-110,170), breaks = seq(from=-100, to=200, by=50)) +
  theme_bw() +
  ylab("$ Difference with On-site/Near-site Primary Care Clinic Use  ($US 2019)") +
  xlab("") +
  ggtitle("Spending") +
  theme(legend.position="none")+
  geom_text(data=test, aes(x = x, y = labpos, label = lab, hjust=0)) + scale_color_brewer(palette = "Dark2")

# 2b

x = c("one", "two", "three","four", "five", "six","seven", "eight", "nine","ten")
y = c(-1.878,-0.0444,-4.9225,-3.5873,-0.0406,-11.7327,-8.217,-2.6928,-30.1735,-1.8448)
se = -c(-0.399234694,-0.011326531,-1.10505102,-0.220892857,-0.145,-2.993035714,-0.762244898,-0.249795918,-4.398469388,-0.176479592)    
ylo = y - 1.96*se
yhi = y + 1.96*se
lab = c("Emergency", "Hospital", "Primary Care", "Specialist", "Mental Health", "Physical Therapy", "Drug admin", "Surgery", "Labs", "Radiology")
labpos = c(rep(-60,10))  
grouping = factor(c( "#D95F02", "#D95F02" , "#7570B3", "#7570B3", "#7570B3", "#7570B3",  "#E7298A", "#66A61E" , "#E6AB02","#E6AB02"))

test <- data.frame(x, y, se, ylo, yhi, lab, labpos, grouping)



test$x <- reorder(test$x, c(10,9,8,7,6,5,4,3,2,1))

ggplot(test, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=grouping)) +
  geom_pointrange(shape=15, size=0.7, position=position_dodge(width=c(0.1))) +
  coord_flip() +
  geom_hline(aes(yintercept=0), lty=2) +
  xlab('Category') +
  scale_x_discrete(breaks=NULL) +
#  scale_y_continuous(limits = c(-5,20), breaks = seq(from=-5, to=20, by=5)) +
  theme_bw() +
  ylab("Utilization Difference with On-site/Near-site Primary Care Clinic Use  (per 1,000 member-months)") +
  xlab("") +
  ggtitle("Utilization") +
  theme(legend.position="none")+
  geom_text(data=test, aes(x = x, y = labpos, label = lab, hjust=0)) + scale_color_brewer(palette = "Dark2")


#2c

x = c("one", "two", "three","four", "five", "six","seven", "eight", "nine","ten")
y = c(8.6559,5770.2972,38.9125,14.9329,1.373,24.76095,7.3261,-75.51995,1.9368,-20.6382)
se = -c(-10.63920918,-210.8784949,-2.064744898,-2.776352041,-8.336071429,-1.494872449,-2.205918367,-24.83951531,-0.69994898,-5.496632653)   
ylo = y - 1.96*se
yhi = y + 1.96*se
lab = c("Emergency", "Hospital", "Primary Care", "Specialist", "Mental Health", "Physical Therapy", "Drug admin", "Surgery",  "Labs", "Radiology")
labpos = c(rep(500,10))  
grouping = factor(c( "#D95F02", "#D95F02" , "#7570B3", "#7570B3", "#7570B3", "#7570B3",  "#E7298A", "#66A61E" , "#E6AB02","#E6AB02"))

test <- data.frame(x, y, se, ylo, yhi, lab, labpos, grouping)



test$x <- reorder(test$x, c(10,9,8,7,6,5,4,3,2,1))

ggplot(test, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=grouping)) +
  geom_pointrange(shape=15, size=0.7, position=position_dodge(width=c(0.1))) +
  coord_flip() +
  geom_hline(aes(yintercept=0), lty=2) +
  xlab('Category') +
  scale_x_discrete(breaks=NULL) +
#  scale_y_continuous(limits = c(-110,170), breaks = seq(from=-100, to=200, by=50)) +
  theme_bw() +
  ylab("$ Difference with On-site/Near-site Primary Care Clinic Use  ($US 2019)") +
  xlab("") +
  ggtitle("Spend per Episode") +
  theme(legend.position="none")+
  geom_text(data=test, aes(x = x, y = labpos, label = lab, hjust=0)) + scale_color_brewer(palette = "Dark2")




