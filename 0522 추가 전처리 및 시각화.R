library(dplyr)
library(ggplot2)
library(corrplot)
library(grid)
library(gridExtra)
library(car)
library(data.table)
getwd()
setwd("C:/Users/vz050/OneDrive/바탕 화면/데마/data")
data<-read.csv("new_abalone.csv")
data$Sex<-factor(data$Sex)
data$Adult<-factor(data$Adult)
data$Volume<-round(data$Volume,2)
data$Area<-round(data$Area,2)
data<-data%>%
  mutate(ratio_AV=round(Volume/Area,2))

write.csv(data,"new_abalone2.csv")
#데이터 구조 확인
str(data) #4012행 16열
head(data)
summary(data)
names(data)

for (i in c(2:8, 10:12, 14:16)){
  assign( paste("box_",names(data)[i],sep=""),
          data%>%
            ggplot()+
            geom_boxplot(aes(y= get(names(data)[i]) ))+
            scale_x_discrete(element_blank())+
            ylab(names(data)[i]) )
  assign( paste("hist_",names(data)[i],sep=""),
          data%>%
            ggplot()+
            geom_histogram( aes(x= get(names(data)[i]) ),col="black",fill="skyblue")+
            xlab(names(data)[i]) )
}
title_list<-c("성별","껍질에서 긴 부분의 길이","껍질에서 Length의 수직 방향의 길이","두께","내장을 제외한 살의 무게",
              "피를 제거한 내장의 무게", "물기를 제거한 껍데기의 무게", "윤문 수", "성패 여부", "세 가지 무게 변수의 합", 
              "전복의 전체 무게", "수분", "부피", "전복의 수평면적", "밀도", 
              "면적대비 부피")

for (i in c(2:8, 10:12, 14:16)){
  grid.arrange( get(paste("box_",names(data)[i],sep="")) ,
                get(paste("hist_",names(data)[i],sep="")),
                ncol=2, top=textGrob( title_list[i], gp=gpar(fontsize = 30,fontface = 'bold')) )
  
}
#부피
box_Volume<-data%>%
  ggplot()+
  geom_boxplot(aes(y= Volume ))+
  scale_x_discrete(element_blank())+
  ylab("Volume")+
  scale_y_continuous(labels = scales::comma)
hist_Volume<-data%>%
  ggplot()+
  geom_histogram( aes(x= Volume ) ,col="black",fill="skyblue")+
  xlab("Volume")+
  scale_x_continuous(labels = scales::comma)  
grid.arrange(box_Volume,hist_Volume,ncol=2,
             top=textGrob( "부피", gp=gpar(fontsize = 30,fontface = 'bold')))

#성별
bar_sex<-data%>%
  ggplot()+
  geom_bar(aes(x=Sex,fill=Sex  ))+
  scale_x_discrete("Sex", 
                   labels = c("0" = "암","1" = "수", "2" = "치패"))+
  ggtitle("성별")+
  theme(plot.title=element_text(size=30, face='bold'),legend.position = 'None')
#성패여부
bar_adult<-data%>%
  ggplot()+
  geom_bar(aes(x=Adult,fill=Adult  ))+
  scale_x_discrete("Adult", 
                   labels = c("0" = "치패","1" = "성패"))+
  ggtitle("성패 여부")+파파고
  theme(plot.title=element_text(size=30, face='bold'),legend.position = 'None')
grid.arrange(bar_sex,bar_adult,ncol=2)

##################################################
corrplot( cor(data[,c(2:8, 10:16)]) ,
          method="number",
          type="lower")


for (i in c(2:8, 10:12, 14:16)){
  assign( paste("box_",names(data)[i],"_per_Adult",sep=""),
          data%>%
            ggplot()+
            geom_boxplot(aes(y= get(names(data)[i]),fill= Adult))+
            facet_wrap(Adult~.)+
            scale_x_discrete(element_blank())+
            scale_fill_discrete( labels=c("치패","성패"))+
            ggtitle(paste("성패 여부별 ",title_list[i],sep=""))+
            ylab(names(data)[i])+
            theme(plot.title=element_text(size=30, face='bold') ))
  
}
for (i in c(2:8, 10:12, 14:16)){
  print(get( paste("box_",names(data)[i],"_per_Adult",sep="") ))
}



##################################################
##################################################
#rings 범주화
summary(data$Rings)
names(data)
data2<-data%>%
  mutate(Rings2=ifelse(Rings<=5,1,
                       ifelse(Rings<=10,2,
                              ifelse(Rings<=15,3,4) ) ))
table(data2$Rings2)

write.csv(data2,"new_abalone2.csv")

sqrt(0.33586936488169367)
