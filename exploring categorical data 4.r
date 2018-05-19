#Exploring Categorical Data
setwd("C://Users//Administrator//Desktop//eda//Exploring categorical data")
comics<-read.csv("comics.csv",header = T)
comics
levels(comics$align)
levels(comics$id)
table(comics$id)
table(comics$align)
table(comics$id,comics$aling)


library(ggplot2)
#2.Contingency table review
table(comics$id,comics$align)
sdstr(comics)


#3.dropping levels
library(dplyr)
comics<-comics %>% filter(comics$align =="Reformed Criminals") %>% droplevels()
levels(comics$align)
comics<-comics %>% filter(comics$align !="Reformed Criminals") %>% droplevels()
levels(comics$align)

#4.side by side bar chart
library(ggplot2)
ggplot(comics,aes(x=id, fill = align))+geom_bar()
ggplot(comics,aes(x=align, fill = id))+geom_bar()
names(comics)

ggplot(comics,aes(x=align, fill = gender))+geom_bar()
ggplot(comics,aes(x=align, fill = gender))+geom_bar(position = "dodge")
table(comics$align,comics$gender)

ggplot(comics,aes(x=gender,fill=align))+geom_bar(position = "dodge")+
      theme(axis.text = element_text(angle = 90))

#Conditional proportions
tab_cnt<-table(comics$id,comics$align)
tab_cnt
prop.table(tab_cnt)

prop.table(tab_cnt,1) ##condition on row (row sum is 1)
prop.table(tab_cnt,2) ##condition on column 
##conditional bar chart
ggplot(comics,aes(x=id, fill = align))+geom_bar(position = "fill")
     +ylab("proportion")



########excrsis 2#########
tab <- table(comics$align, comics$gender)
options(scipen = 999, digits = 3) # Print fewer digits
prop.table(tab)     # Joint proportions
prop.table(tab, 2)  

ggplot(comics,aes(x=align,fill = gender))+geom_bar()

ggplot(comics,aes(x=align,fill = gender))+geom_bar(position="fill")+ylab("proportion")



#########distribution########
table(comics$id)
tab_cnt<-table(comics$id,comics$align)
tab_cnt 
ggplot(comics,aes(x = id))+geom_bar()

######faceted####
ggplot(comics,aes(x=id))+geom_bar()+facet_wrap(~align)+theme(axis.text = element_text(angle = 90))


##################3exc#########
comics$align<-factor(comics$align,levels=c("Bad","Neutral","Good"))
levels(comics$align)

ggplot(comics,aes(x = align))+geom_bar()
ggplot(comics,aes(x=align))+geom_bar()+facet_wrap(~gender)
 

########## create a barchart of id and orient the labels vertically so that they are
#easier TO READ . The default coloring may look drab by comparision , so
#that they're easier to read. The default coloring may look drab by comparison, 
#so change the fill of the bars to "chartreuse".

ggplot(comics,aes(x = id))+geom_bar(fill="chartreuse")+theme(axis.text = element_text(angle = 90))


#######piechart on id variables of comics dataset########
x<-c(10,20,30)
labels<-c("bad","neutral","good")
pie(x,labels)


table<- comics %>% group_by(id) %>% summarise(count = n())
ggplot(table,aes(x="",y=count,fill= id))+geom_bar(width = 1,stat ="identity")+coord_polar(theta = "y",start=0)

