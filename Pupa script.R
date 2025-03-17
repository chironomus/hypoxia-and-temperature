library(ggpubr)
library(rstatix)
data1<- read.csv( "pupae1.csv",sep=",")

data1$Treatment <- as.character(data1$Treatment)

data1$Lenght<- gsub(",", ".", data1$Lenght)

data1$Lenght<-as.numeric(data1$Lenght)


#Then turn it back into a factor with the levels in the correct order

anova=aov(data1$Lenght~data1$Treatment)
TukeyHSD(anova)
require(dplyr)
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
boxplot(data1$Lenght~data1$Treatment)
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

model2=lm(uM~Temperature+Na2S03+Temperature*Na2S03,data=data)




library(ggpubr)
library(rstatix)
data<- read.csv( "larvae_1.csv",sep=",")

data$treatment <- as.character(data$treatment)
#Then turn it back into a factor with the levels in the correct order

require(ggplot2)
plot=ggplot(data,aes(x=Treatment,y=Lenght,colour=as.factor(Treatment), group=as.factor(Treatment)))+geom_boxplot()
plot+facet_wrap(vars(Instar))+theme_bw()


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
