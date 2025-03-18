library(ggpubr)
library(rstatix)
require(lubridate)
require(tidyverse)
require(ggplot2)
#data1<- read.csv( "pupae1.csv",sep=",")

#data1$Treatment <- as.character(data1$Treatment)

#data1$Lenght<- gsub(",", ".", data1$Lenght)

#data1$Lenght<-as.numeric(data1$Lenght)
#data1 <- data1 %>%
 # mutate(Date = str_extract(Folder, "\\d{2}_\\d{2}_\\d{2}"),  # Extracts the date pattern
       #  Date = dmy(gsub("_", "", Date)))  # Removes underscores & converts to Date
#write.csv(data1, "pupa1.csv")
Treatment=c("A","B","C","D","E","F")
temp=c(20,20,20,30,30,30)
Oxygen=c(100,75,50,100,75,50)
treat=cbind(Treatment,temp,Oxygen)
treat=as.data.frame(treat)

#Then turn it back into a factor with the levels in the correct order
data1<- read.csv( "pupa2#.csv",sep=",")
data1=merge(data1,treat,by=c("Treatment"))
summary(lm(Lenght~temp+Oxygen+temp*Oxygen,data=data1))

anova=aov(data1$Lenght~data1$Treatment)
TK=TukeyHSD(anova)


TK_data<-as.data.frame(TK[1]) # the [1] locates the part of the output to be exported

write.csv(TK_data, 'TK_data.csv')

require(dplyr)

treatment_counts <- data1 %>%
  count(Treatment, Date)

data1 %>% count(Treatment)
data1 %>%
  group_by(Treatment) %>%
  summarise_at(vars(uM), list(name =median))
require(ggplot2)
data1 %>% count(Treatment)

library(data.table)

data1[ ,list(mean=mean("Lenght")), by=data1$Treatment]               
aggregate(data1$Lenght, list(data1$Treatment), FUN=mean)

# Create a box plot
boxplot(data1$Lenght~data1$Treatment,xlab="Treatment",ylab="Size, mm", ylim=c(4.5,10.5))
text(2,10, "20°C", col = "black", cex = 1.1)
text(5,10, "30°C", col = "black", cex = 1.1)
text(1,4.7, "100%", col = "black", cex = 1.1)
text(2,4.7, "75%", col = "black", cex= 1.1)
text(3,4.7, "50%", col = "black", cex = 1.1)
text(4,4.7, "100%", col = "black", cex = 1.1)
text(5,4.7, "75%", col = "black", cex = 1.1)
text(6,4.7, "50%", col = "black", cex = 1.1)

plot(1:10, xlab=expression('hi'[5]*'there'[6]^8*'you'[2]))

bxp <- ggboxplot(data1, x = "Treatment", y = "Lenght", fill = "Treatment", palette = c("#00AFBB","#C49A00","#53B400","#00C094","#00B6EB","#A58AFF","#FB61D7"))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Treatment", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))




p <- ggplot(data1, aes(x=Date, y=Lenght))+geom_boxplot()
p+facet_wrap(~Treatment)+theme_bw()



library(ggpubr)
library(rstatix)
data<- read.csv( "larvae_1.csv",sep=",")

data$treatment <- as.character(data$treatment)
#Then turn it back into a factor with the levels in the correct order

#emergence by treattment
treatment_counts=na.omit(treatment_counts)
treatment_counts$date=as.Date(treatment_counts$Date,format = "%d/%m/%Y")
require(ggplot2)
plot=ggplot(treatment_counts,aes(x=date,y=n))+geom_line()
plot=plot+ggtitle("Pupal exuviae collected by treatment") +
  xlab("Date)") + ylab("Number of exuviae")
plot=plot+theme(legend.position = "none")
plot+facet_wrap(~Treatment)+theme(legend.position = "none")+theme_bw()



x3=subset(data,data$Instar=="III")
anova=aov(x3$Lenght~data$treatment)
TukeyHSD(anova)
require(dplyr)
data %>% count(treatment)
data %>%
  group_by(treatment) %>%
  summarise_at(vars(uM), list(name =median))
require(ggplot2)
# Create a box plot
stat.test <- data %>%
  t_test(uM~treatment) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test
# Create a box plot

bxp <- ggboxplot(data, x = "treatment", y = "uM", fill = "treatment", palette = c("#00AFBB","#C49A00","#53B400","#00C094","#00B6EB","#A58AFF","#FB61D7"))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "treatment", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

model2=lm(uM~Temperature+Na2S03+Temperature*Na2S03,data=data)
