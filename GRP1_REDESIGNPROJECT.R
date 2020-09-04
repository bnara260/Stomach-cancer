#R code for redesign project on Analysis of  Incidence Rates on Stomach Cancer in USA
setwd("D:/STATS/REDESIGN_PRO")
library(tidyverse)
library(micromapST)

# READING DATA SET
stomcan <- read.csv(file = 'stomachcancer.csv',header=TRUE, as.is=TRUE)
str(stomcan)

# CREATING LINKED MICROMAP
#Panel description data frame
panelDesc <- data.frame(
           type=c('maptail','id','dotconf','dotconf'),
           lab1=c(NA,NA,'Age Adjusted Incidence Rate','Recent 5-Year Trend in '),
           lab2=c(NA,NA,'cases per 100,000 and 95% CIs','Incidence Rates and 95% CIs'),
           lab3=c(NA,NA,'Data Source : CDC State Cancer Profile website',NA),
           col1 = c(NA,NA,'Age.Adjusted.Incidence.Rate.cases.per.100.000','Recent.5.Year.Trend.in.Incidence.Rates'),
          col2 = c(NA,NA,'Lower.95..CI', 'Lower.95..CI.Trend'),
           col3 = c(NA,NA, 'Upper.95..CI','Upper.95..CI.Trend')
)

#To Save and Deploy micromap plot in a pdf file
fName = "Stomach Cancer Incidence Rates In USA dotconf.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(stomcan, panelDesc,
           rowNamesCol='State',
           rowNames='full',
           plotNames='full',
           sortVar='Recent.5.Year.Trend.in.Incidence.Rates',ascend=FALSE,
           title=c("Analysis of  Incidence Rates on Stomach Cancer in USA in between 2012-2016",
                   "For all Races,Ages and Gender"),
           ignoreNoMatches=TRUE)
dev.off()

#Additional Visualizations
X11();
is.na(stomcan)
d_f<-stomcan %>%
  group_by(Recent.Trend)%>%
  summarise(Age.Adjusted.Incidence.Rate.cases.per.100.000=mean(Age.Adjusted.Incidence.Rate.cases.per.100.000),
            Lower.95..Confidence.Interval=min(Lower.95..CI),
            Upper.95..Confidence.Interval=max(Upper.95..CI),
            Average.Annual.Count=sum(Average.Annual.Count))

d_f$Average.Annual.Count

# BAR PLOT
barp <- ggplot(d_f, aes(x = Recent.Trend , y = Recent.Trend,y=Average.Annual.Count)) +
  geom_bar(aes(color=Recent.Trend,fill=Recent.Trend),
           color="black",
           stat = "identity", position = position_dodge(0.8),
           width = 0.7) + scale_y_continuous(breaks = seq(0,35000,1000)) + scale_fill_manual(values = c("green", "yellow","red"))+
  labs(x=" Stomach Cancer Trends in between 2012-2016",y="Average annual count",title="Total Count of Stomach Cancer Patients in U.S.A in between 2012 to 2016")
barp


# BOX PLOT
stomcan %>% select(Age.Adjusted.Incidence.Rate.cases.per.100.000)%>% summary()

boxp<-  ggplot(stomcan, aes(x=Recent.Trend,y=Age.Adjusted.Incidence.Rate.cases.per.100.000)) +
  geom_boxplot(aes(color=Recent.Trend,fill=Recent.Trend),show.legend = FALSE) +
  scale_fill_manual(values = c("yellow","blue","maroon"))+
  scale_y_continuous(breaks = seq(0,20,0.25))+
  labs(x="Recent trend",y="Age adjusted incidence rates for 100,000 cases",title = "An Age adjusted incidence rates BOX PLOT") 
boxp


# PLOTTING ORDER
p_order<-ggplot(stomcan, aes(x = Recent.5.Year.Trend.in.Incidence.Rates, y = reorder(State,-Recent.5.Year.Trend.in.Incidence.Rates))) +
  geom_point(
    shape = 20,
    fill = "green",
    size = 3,
    color = "red"
  ) +
  scale_x_continuous(breaks = seq(-5,5,0.50))+
  
  labs(x = "Recent 5 year trend",
       y = "USA States",
       title = "A recent 5 year trend in each USA state 
       in between 2012-2016 ")

p_order
