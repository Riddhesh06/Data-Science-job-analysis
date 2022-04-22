library(ggplot2)    
library(dplyr)      #data manipulation
library(tidyverse)

#Importing dataset
df<-read.csv("C:/Users/riddh/Downloads/job_dataset.csv")

names(df)
list.files(path = "../input")
getwd()

#showing columns
colnames(df)


#key skills for data science domain preferable languages R,Python,SQL
print(count(df,Python))
print("-------")
print(count(df,R.Prog))
print("-------")
print(count(df,SAS))
print("-------")
print(count(df,Excel))
print("-------")
print(count(df,SQL))
print("-------")
print(count(df,Hadoop))

#counting the various data jobs
gb <- df %>% group_by(Job.Title)%>% summarise(
  count=n()
)
gb

gb <- df %>% group_by(Job.Title)%>% summarise(
  Python = round(sum(Python)/n(),2)*100,
  R.Prog = round(sum(R.Prog)/n(),2)*100,
  Excel = round(sum(Excel)/n(),2)*100,
  Hadoop = round(sum(Hadoop)/n(),2)*100,
  SAS = round(sum(SAS)/n(),2)*100,
  SQL = round(sum(SQL)/n(),2)*100
)
gb

structure<-ggplot(gb)

#Data Job which required python language
structure+geom_col(aes(x=Job.Title,y=Python),fill = "lightgreen")+
  theme(axis.title.x=element_text(colour='black',size=16),
        axis.title.y=element_text(colour='black',size=16),
        axis.text.x=element_text(colour='darkgreen',size=13),
        axis.text.y=element_text(colour='darkgreen',size=13),
        legend.title=element_text(colour='black',size=20),
        legend.text=element_text(colour='black',size=20))
options(repr.plot.width=12, repr.plot.height=6)

#Data Job which required R language
structure+geom_col(aes(x=Job.Title,y=R.Prog))+
  theme(axis.title.x=element_text(colour='black',size=16),
        axis.title.y=element_text(colour='black',size=16),
        axis.text.x=element_text(colour='darkgreen',size=13),
        axis.text.y=element_text(colour='darkgreen',size=13),
        legend.title=element_text(colour='black',size=20),
        legend.text=element_text(colour='black',size=20))
options(repr.plot.width=12, repr.plot.height=6)

#Data Job which required SQL language
structure+geom_col(aes(x=Job.Title,y=SQL), fill = 'purple')+
  theme(axis.title.x=element_text(colour='black',size=16),
        axis.title.y=element_text(colour='black',size=16),
        axis.text.x=element_text(colour='darkgreen',size=13),
        axis.text.y=element_text(colour='darkgreen',size=13),
        legend.title=element_text(colour='black',size=20),
        legend.text=element_text(colour='black',size=20))
options(repr.plot.width=12, repr.plot.height=6)

#Locations in India to be looked
str<-ggplot(df[!df$Location=='-1',])

str+geom_bar(aes(x=Location),fill='darkgreen')+
  theme(axis.title.x=element_text(colour='black',size=20),
        axis.title.y=element_text(colour='black',size=20),
        axis.text.x=element_text(colour='darkgreen',size=16,angle = 90),
        axis.text.y=element_text(colour='darkgreen',size=15),
        legend.title=element_text(colour='black',size=20),
        legend.text=element_text(colour='black',size=20))
options(repr.plot.width=20, repr.plot.height=8)

#Size of companies which are actively hiring
str<-ggplot(df[!df$Size=='-1',])
str+geom_bar(aes(x=Size),fill='orange')+
  theme(axis.title.x=element_text(colour='black',size=20),
        axis.title.y=element_text(colour='black',size=20),
        axis.text.x=element_text(colour='darkgreen',size=16,angle = 90),
        axis.text.y=element_text(colour='darkgreen',size=15),
        legend.title=element_text(colour='black',size=20),
        legend.text=element_text(colour='black',size=20))
options(repr.plot.width=20, repr.plot.height=8)