library(ggpubr)
library(rstatix)
data<- read.csv( "Measure_larvae.csv",sep=",")
x=as.vector(data$Length)
data$treatment <- as.character(data$Treatment)
data=data[-(804:1048574),]
data$Length<- gsub(",", ".", data$Length)

data$Length<-as.numeric(data$Length)
data=na.omit(data)

#Then turn it back into a factor with the levels in the correct order

anova=aov(data$Length~data$Treatment)
TukeyHSD(anova)
require(dplyr)
data %>% count(Treatment)
data %>%
  group_by(Treatment) %>%
  summarise_at(vars(Length), list(name =median))
require(ggplot2)
# Create a box plot
stat.test <- data %>%
  t_test(Length~treatment) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test
# Create a box plot

bxp <- ggboxplot(data, x = "Treatment", y = "Length", fill = "Treatment", palette = c("#00AFBB","#C49A00","#53B400","#00C094","#00B6EB","#A58AFF","#FB61D7"))

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





library(ggpubr)
library(rstatix)
#data<- read.csv( "larvae_1.csv",sep=",")

data$treatment <- as.character(data$treatment)
#Then turn it back into a factor with the levels in the correct order

require(ggplot2)
plot=ggplot(data,aes(x=Treatment,y=Length,colour=as.factor(Treatment), group=as.factor(Treatment)))+geom_boxplot()
plot+facet_wrap(vars(Instar))+theme_bw()


x3=subset(data,data$Instar=="III")
anova=aov(x3$Length~data$treatment)
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



#head capsules
data_hc<- read.csv( "HC.csv",sep=",")
data_hc$treatment <- substr(data_hc$Folder, 1, 1)
boxplot(data_hc$Lenght~data_hc$treatment)
#merge body and hc measurments
#extract photo name from string "data"
#data$Photo.names <- substr(data$Photo, nchar(data$Photo) - 16, nchar(data$Photo))
#print(data$Photo.names)
#larvae<- merge(data,data_hc, by = c("treatment"))
#larvae=larvae[-(804:120871),]
#larvae$treatment <- substr(larvae$Photo.names.x, 1, 1)
#plot hc vs body length by teatment
#p <- ggplot(larvae, aes(x=Length, y=Lenght))+geom_point()+geom_smooth(method="lm")
#p+facet_wrap(~treatment)+theme_bw()



body=aggregate(data$Length, list(data$treatment), FUN=mean)
hc=aggregate(data_hc$Lenght, list(data_hc$treatment), FUN=mean)
hc$mean1=hc$x
body$mean=body$x
larvae<- merge(body,hc, by = c("Group.1"))
plot(larvae$mean1~larvae$mean)
cor(larvae$mean1,larvae$mean)
