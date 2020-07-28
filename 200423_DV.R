library(ggplot2)
library(ggmap)
library(ggsn)
library(dplyr)
library(lubridate)
library(animation)
library(gganimate)
library(ggthemes)
library(gifski)
library(jpeg)
library(extrafont)

for(i in seq(8)){
  assign(paste0('sensor',i)
         ,read.table(paste0('C:/Users/user/Desktop/IOT/rawdata/sensor',i,'.txt')))
}



sensor1<-sensor1[1:51136,c(1,3,4)]
sensor2<-sensor2[1:51136,c(1,3,4)]
sensor3<-sensor3[1:51136,c(1,3,4)]
sensor4<-sensor4[1:51136,c(1,3,4)]
sensor5<-sensor5[1:51136,c(1,3,4)]
sensor6<-sensor6[1:51136,c(1,3,4)]
sensor7<-sensor7[1:51136,c(1,3,4)]
sensor8<-sensor8[1:51136,c(1,3,4)]


for (i in dim(sensor1[3])[1]:2){
  if (sensor1[i,3]==1 & sensor1[i,3]==sensor1[i-1,3]){
    sensor1[i,3]<-0
  }
}
for (i in dim(sensor2[3])[1]:2){
  if (sensor2[i,3]==1 & sensor2[i,3]==sensor2[i-1,3]){
    sensor2[i,3]<-0
  }
}

for (i in dim(sensor3[3])[1]:2){
  if (sensor3[i,3]==1 & sensor3[i,3]==sensor3[i-1,3]){
    sensor3[i,3]<-0
  }
}
for (i in dim(sensor4[3])[1]:2){
  if (sensor4[i,3]==1 & sensor4[i,3]==sensor4[i-1,3]){
    sensor4[i,3]<-0
  }
}
for (i in dim(sensor5[3])[1]:2){
  if (sensor5[i,3]==1 & sensor5[i,3]==sensor5[i-1,3]){
    sensor5[i,3]<-0
  }
}
for (i in dim(sensor6[3])[1]:2){
  if (sensor6[i,3]==1 & sensor6[i,3]==sensor6[i-1,3]){
    sensor6[i,3]<-0
  }
}
for (i in dim(sensor7[3])[1]:2){
  if (sensor7[i,3]==1 & sensor7[i,3]==sensor7[i-1,3]){
    sensor7[i,3]<-0
  }
}
for (i in dim(sensor8[3])[1]:2){
  if (sensor8[i,3]==1 & sensor8[i,3]==sensor8[i-1,3]){
    sensor8[i,3]<-0
  }
}

sensors<-cbind(sensor1[1], sensor1[3],sensor2[3],sensor3[3],sensor4[3],sensor5[3],sensor6[3],sensor7[3],sensor8[3])

names(sensors)<-c('time','S1','S2','S3','S4','S5','S6','S7','S8')

sensors


#1 ?????? ?????? ??
sensor_sum<-apply(sensors[-1], 2 ,sum)
sensor_sum_names<-names(sensor_sum)
sensor_sum<-data.frame(sensor_sum)
sensor_sum$sensor_sum_names<-sensor_sum_names

sensor_sum
color<-data.frame(c(rep('skyblue',6),'red','skyblue'))
names(color)<-'color'
sensor_sum<-cbind(sensor_sum,color)
sensor_sum

p<-ggplot(data=sensor_sum, aes(x=sensor_sum_names,y=sensor_sum, fill=color))+geom_bar(stat='identity')+coord_flip()+
  labs(title='?????? ???? Ƚ?? (11:00 ~ 14:00)', x='???? ??ȣ', y='???? Ƚ??')+theme_bw()+
  theme(axis.text.x = element_text(colour="grey20", size=18, angle=45, hjust=.5, vjust=.5),
        axis.text.y = element_text(colour="grey20", size=18), text=element_text(size=15),
        legend.position = 'none')+
  geom_text(stat='identity', aes(label=sensor_sum), hjust=-0.5)+  theme(plot.title=element_text(face="bold", size=20))

p


#2 animate
sensors$time<-substr(sensors$time,1,8)
sensors_agg<-aggregate(sensors[2:9], by=list(Time=sensors$time), FUN=sum)
#sensors_agg<-as.data.frame(sensors_agg, row.names = sensors_agg$Time)[2:9]
sensors_agg_ex<-gather(sensors_agg, sensor_no, count, S1:S8, factor_key=TRUE) %>% arrange(Time)

sensors_agg_ex$X <- rep(c(1.75,1.75,1.75,1.75,1.85,2.09,1.85,2))
sensors_agg_ex$Y <- rep(c(2.15,2.01,1.7,1.22,0.66,0.61,0.44,0.28))

sensors_agg_ex$Time<-as_datetime(paste0('2020-04-22 ', sensors_agg_ex$Time))


sensors_agg_ex<-sensors_agg_ex %>%
  filter(Time >=as_datetime('2020-04-22 11:10:00')) %>%
  filter(Time <=as_datetime('2020-04-22 11:30:00'))
  


office<-readJPEG('C:/Users/user/Desktop/IOT/office1.jpg')
g <- rasterGrob(office, interpolate=FALSE)

ab<-ggplot(data = sensors_agg_ex, aes(x = X, y = Y, size  = count, color = sensor_no)) +
  annotation_custom(g,0,3,0,3) +geom_point() + theme_bw() +
  coord_cartesian(xlim=c(0,3),ylim=c(0,3))+
  labs(title = 'Datetime: {frame_time}') + 
  transition_time(Time) + ease_aes('linear')+
  theme(axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank())

ab  


abc<-animate(ab,duration = 180, nframes=600, fps=24)
abc

#3 11:30 시 이후의 동선

V3<-data.frame(cbind(substr(sensors$time,1,5),sensors$S7))
V3$X2 <- as.numeric(V3$X2)-1

V3_agg<-aggregate(V3$X2, by=list(V3$X1), sum)

names(V3_agg)=c('Group','Count')
V3_agg<-V3_agg[11:41,]

V3_agg$color1<-c(rep('skyblue',6),'blue',rep('skyblue',5),'blue',rep('skyblue',18))
V3_agg$size<-c(rep(3,6),5,rep(3,5),5,rep('skyblue',18))

ggplot(V3_agg,aes(x=Group,y=Count))+geom_line(group=1)+geom_point(size=3, color=V3_agg$color1)+
  geom_vline(xintercept = c(which.max(V3_agg$Count),13), color=c('red','blue'),size=2,linetype='dotted')+theme_bw()+
  geom_text(stat='identity', aes(label=Count), hjust=-0.5, vjust=-1, size=5)+
  labs(x='Timeslot',y='Count')+ggtitle('��?ɽð??? ???? ???? ???? ?ð?��? (S7)')+
  theme(plot.title=element_text(face="bold", size=20, vjust=2))

