setwd("~/Desktop/world-happiness")
happiness<-read.csv(file = "2016.csv")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("digest")
library(digest)
library(ggplot2)
library(tidyverse)


install.packages("corrplot")
library(corrplot)
happiness$Dystopia.Residual
summary(happiness)


ggplot(happiness,aes(x=Happiness.Score,y=Region)) + geom_boxplot(fill='blue',color='black',width = 0.5)+ 
  labs(x='Happiness_Score',y='Region of Happiness',title = 'Visualization of Happiness Region')

ggplot(happiness,aes(x=Family) + geom_bar(fill='grey',color='black',width=0.5)+ 
  labs(x='Family',y='Region',title = 'Visualization of family')

ggplot(happiness,aes(x=Happiness.Score)) + geom_histogram(bins=30,color='black',fill='green')+ 
  labs(x='Happiness Score',y='Count',title = 'Visualization of Happiness Score')

happiness$Family<- round(happiness$Family,digits = 1)

ggplot(happiness,aes(x=Family, fill=Region)) + geom_histogram(bins=10)+ 
  scale_fill_brewer(palette = 'RdBu')
  labs(x='Happiness Score',y='Count',title = 'Visualization of Happiness Score')



ggplot(happiness,aes(x=Region)) + geom_bar()+ scale_fill_manual()
ggplot(happiness,aes(x=Region,fill=Dystopia.Residual)) + geom_bar(width=1)+ scale_fill_brewer(palette = 'RdBu')
  
corr_happiness<-happiness[3:13]
corrplot(cor(corr_happiness))

average_happiness<-happiness %>%group_by(Region) %>% summarise(Happiness.Score=mean(Happiness.Score))
                                                               
                                                               