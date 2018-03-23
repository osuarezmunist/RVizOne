# RB1 Edits
library(dplyr)
library(dslabs)
data(murders)
library(ggthemes)
library(ggrepel)


r <- murders %>% summarize(rate= sum(total)/sum(population)*10^6) %>% .$rate
p <- murders %>% ggplot(aes(x=population/10^6,y=total,label=abb))+
    geom_abline(intercept = log10(r), lty=2,color="darkgrey")+
    geom_point(aes(col=region),size=3)+
  geom_text_repel()+
 #   geom_text(nudge_x = .08)+
    scale_x_log10()+
    scale_y_log10()+
    xlab("Pop in mil")+
    ylab("Tots Murd")+
    ggtitle("US Murds")+
    scale_color_discrete(name="Region")
    
p <- p + theme_economist() 
p

h <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
h + geom_density(binwidth = 1, fill="blue", col="black")
h <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height))
h + geom_qq()

params<- heights %>% filter(sex=="Male") %>% summarize(mean=mean(height), sd=sd(height))
h <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height))
h + geom_qq(dparams = params) + geom_abline()

library(gridExtra)
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p + geom_histogram(binwidth = 1, fill="blue", col="black")
p2 <- p + geom_histogram(binwidth = 2, fill="blue", col="black")
p3 <- p + geom_histogram(binwidth = 3, fill="blue", col="black")
grid.arrange(p1,p2,p3,ncol=3)


library(tidyverse)
s <- heights %>% filter(sex == "Male") %>% summarize(average=mean(height),std=sd(height))


heights %>% group_by(sex) %>% summarize(mean=mean(height), sd=sd(height))

murders$rate<- murders$total/murders$population*10^5
murders %>% group_by(region) %>% summarize(median=median(rate))
murders %>% arrange(rate) %>% head()
murders %>% arrange(region,desc(rate)) %>% top_n(10)
