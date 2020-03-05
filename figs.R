setwd("~/Box/Analytics Team/Research/Research projects/One medical")
library(ggplot2)

#1a

x = c("one", "two", "three","four", "five", "six","seven", "eight", "nine","ten", "eleven", "twelve")
y = c(-45, -32.8, -15.5, 108.7, -53.8, 19.8, 9, -25.5, -4.7, -42.8, -31.9, -41.3)
se = c(5.153061224, 5.663265306, 3.06122449, 3.673469388, 3.775510204, 3.62244898, 4.693877551, 5.102040816, 3.979591837, 5.204081633, 4.081632653, 4.132653061)
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
y = c(-2.5, -0.9, 1.2, -4.9, 1.1, -2.9, -2.5, -4, -6.4, -3.4)
se = c(0.510204082, 0.204081633, 0.408163265, 0.255102041, 0.357142857, 0.918367347, 0.459183673, 0.408163265, 0.918367347, 0.306122449)    
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
y = c(29.2, 259, 35, 21.7, -7.3, 64.1, 20.6, 5.9, 9.9, -1.7)
se = c(2.908163265, 6.836734694, 1.275510204, 1.93877551, 5.153061224, 2.346938776, 3.673469388, 5.969387755, 2.5, 3.87755102)   
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
y = c(-167.0625, -15.9244, -4.5415, 19.83775, -10.6255, 0.495, 0.5625, -2.69025, -0.5311, -14.2952, -3.3176, -3.8409)
se = c(19.1307398, 2.749515306, 0.896938776, 0.670408163, 0.745663265, 0.090561224, 0.293367347, 0.538265306, 0.449693878, 1.738163265, 0.424489796, 0.384336735)
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
y = c(-1.87, -0.0828, 1.6518, -2.5774, 0.37895, -6.0813, -2.69, -2.226, -22.3808, -1.6949)
se = c(0.381632653, 0.01877551, 0.561836735, 0.134183673, 0.123035714, 1.925816327, 0.494081633, 0.227142857, 3.211530612, 0.152602041)    
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
y = c(112.36525, 6467.6304, 46.8325, 22.31235, -27.2397, 32.93325, 9.6748, -18.9921, 1.335, -12.08535)
se = c(13.90538265, 180.0385714, 1.838010204, 2.416683673, 8.067117347, 1.299030612, 2.652244898, 19.5467602, 0.6675, 5.038877551)   
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




