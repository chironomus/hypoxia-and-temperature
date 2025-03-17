library(ggpubr)
library(rstatix)
data<- read.csv( "larvae_1.csv",sep=",")

data$Treatment <- as.character(data$Treatment)
#Then turn it back into a factor with the levels in the correct order

require(ggplot2)
plot=ggplot(data,aes(x=Treatment,y=Lenght,colour=as.factor(Treatment), group=as.factor(Treatment)))+geom_boxplot()
plot+facet_wrap(vars(Instar))+theme_bw()


x3=subset(data,data$Instar=="III")
x4=subset(data,data$Instar=="IV")
anova=aov(data$Lenght~data$Treatment*data$Instar)
anova=aov(x4$Lenght~x4$Treatment)
anova=aov(x3$Lenght~x3$Treatment)
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
