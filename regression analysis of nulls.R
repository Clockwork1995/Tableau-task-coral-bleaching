x <- ts(c(1,2,3,4,5,6,7,8))
hardcoralsite1 <- data.frame(x,y=c(NA,0.1934,0.2045,0.3998,0.4078,0.4099,0.4178,0.4432))
softcoralsite2 <- data.frame(x,y=c(NA,NA,NA,0.1488,0.1578,0.1721,0.1923,0.2123))
seafansite2 <- data.frame(x,y=c(NA,NA,0.4598,0.4639,0.485,0.5021,0.5432,0.5632))
bluecoralsite2 <- data.frame(x,y=c(NA,NA,NA,0.1737,0.1779,0.1878,0.2134,0.2321))
hardcoralsite2 <- data.frame(x,y=c(NA,NA,0.2482,0.2477,0.2489,0.2532,0.3012,0.3489))
seapensite2 <- data.frame(x,y=c(NA,NA,0.2861,0.289,0.2948,0.3089,NA,NA))
softcoralsite3<- data.frame(x,y=c(NA,NA,NA,0.2589,0.2689,0.3721,NA,NA))
seafansite3<- data.frame(x,y=c(NA,NA,0.0634,0.0828,0.1045,NA,NA,NA))
hardcoralsite3<- data.frame(x,y=c(NA,NA,0.1733,0.17650,0.18440,0.18340,NA,NA))
seapensite3 <- data.frame(x,y=c(NA,NA,0.0859,0.0879,0.0901,0.09012,0.0912,0.0934))
hardcoralsite7 <- data.frame(x,y=c(NA,0.1503,0.1499,0.1545,0.1689,0.1823,0.2576,0.3212))
softcoralsite8<-data.frame(x,y=c(NA,NA,NA,0.5889,0.6003,0.6012,0.6321,0.6523))
seafansite8<-data.frame(x,y=c(NA,NA,0.3245,0.3866,0.3984,0.4089,0.4532,0.4823))
bluecoralsite8<- data.frame(x,y=c(NA,NA,NA,0.3993,0.4078,0.4098,0.4321,0.4532))
hardcoralsite8<- data.frame(x,y=c(NA,NA,0.131,0.1384,0.1488,0.1456,0.1556,0.2045))
seapensite8<- data.frame(x,y=c(NA,NA,0.0371,0.0389,0.0419,0.0434,NA,NA))


ind <- function(t)
{
  x<-dim(length(t))
  x[which(!is.na(t))] =1
  x[which(is.na(t))] = 0
  return(x)
}


hardcoralsite1$i<-ind(hardcoralsite1$y)
hardcoralsite1
softcoralsite2$i<-ind(softcoralsite2$y)
softcoralsite2
seafansite2$i<-ind(seafansite2$y)
seafansite2
bluecoralsite2$i<-ind(bluecoralsite2$y)
bluecoralsite2
hardcoralsite2$i<-ind(hardcoralsite2$y)
hardcoralsite2
seapensite2$i<-ind(seapensite2$y)
seapensite2
softcoralsite3$i<-ind(softcoralsite3$y)
softcoralsite3
seafansite3$i<-ind(seafansite3$y)
seafansite3
hardcoralsite3$i<-ind(hardcoralsite3$y)
hardcoralsite3
seapensite3$i<-ind(seapensite3$y)
seapensite3
hardcoralsite7$i<-ind(hardcoralsite7$y)
hardcoralsite7
softcoralsite8$i<-ind(softcoralsite8$y)
softcoralsite8
seafansite8$i<-ind(seafansite8$y)
seafansite8
bluecoralsite8$i<-ind(bluecoralsite8$y)
bluecoralsite8
hardcoralsite8$i<-ind(hardcoralsite8$y)
hardcoralsite8
seapensite8$i<-ind(seapensite8$y)
seapensite8



lm(y~x,hardcoralsite1)

for(j in 1:nrow(hardcoralsite1))
{
  if(hardcoralsite1$i[j]==0)
  {hardcoralsite1$y[j]=0.14197+0.04236*hardcoralsite1$x[j]}
}

hardcoralsite1  


lm(y~x,softcoralsite2)

for(j in 1:nrow(softcoralsite2))
{
  if(softcoralsite2$i[j]==0)
  {softcoralsite2$y[j]=0.07976+0.01615*softcoralsite2$x[j]}
}

softcoralsite2 


lm(y~x,seafansite2) #seafansite2



for(j in 1:nrow(seafansite2))
{
  if(seafansite2$i[j]==0)
  {seafansite2$y[j]=0.38155+0.02206*seafansite2$x[j]}
}


seafansite2



lm(y~x,bluecoralsite2) 

for(j in 1:nrow(bluecoralsite2))
{
  if(bluecoralsite2$i[j]==0)
  {bluecoralsite2$y[j]=0.10560+0.01523*bluecoralsite2$x[j]}
}

bluecoralsite2


lm(y~x,hardcoralsite2) 

for(j in 1:nrow(hardcoralsite2))
{
  if(hardcoralsite2$i[j]==0)
  {hardcoralsite2$y[j]=0.16966+0.01909*hardcoralsite2$x[j]}
}

hardcoralsite2


lm(y~x,seapensite2) 
plot(seapensite2$x,seapensite2$y)
seapensite2


for(j in 1:nrow(seapensite2))
{
  if(seapensite2$i[j]==0)
  {seapensite2$y[j]=0.26131+0.00742*seapensite2$x[j]}
}

seapensite2


lm(y~x,softcoralsite3)

plot(softcoralsite3$x,softcoralsite3$y)
softcoralsite3

for(j in 1:nrow(softcoralsite3))
{
  if(softcoralsite3$i[j]==0)
  {softcoralsite3$y[j]=0.01697+0.05660*softcoralsite3$x[j]}
}


softcoralsite3


lm(y~x,seafansite3)

plot(seafansite3$x,seafansite3$y)
seafansite3

for(j in 1:nrow(seafansite3))
{
  if(seafansite3$i[j]==0)
  {seafansite3$y[j]=0.001367+0.020550*seafansite3$x[j]}
}

seafansite3


lm(y~x,seapensite3)


for(j in 1:nrow(seapensite3))
{
  if(seapensite3$i[j]==0)
  {seapensite3$y[j]=0.082318+0.001355*seapensite3$x[j]}
}

seapensite3



lm(y~x,hardcoralsite7)


for(j in 1:nrow(hardcoralsite7))
{
  if(hardcoralsite7$i[j]==0)
  {hardcoralsite7$y[j]=0.06283+0.02700*hardcoralsite7$x[j]}
}

hardcoralsite7

lm(y~x,softcoralsite8)



for(j in 1:nrow(softcoralsite8))
{
  if(softcoralsite8$i[j]==0)
  {softcoralsite8$y[j]=0.51980+0.01586*softcoralsite8$x[j]}
}

softcoralsite8


lm(y~x,seafansite8)

for(j in 1:nrow(seafansite8))
{
  if(seafansite8$i[j]==0)
  {seafansite8$y[j]=0.25195+0.02855*seafansite8$x[j]}
}

seafansite8

lm(y~x,bluecoralsite8)

for(j in 1:nrow(bluecoralsite8))
{
  if(bluecoralsite8$i[j]==0)
  {bluecoralsite8$y[j]=0.34118+0.01321*bluecoralsite8$x[j]}
}

bluecoralsite8

lm(y~x,hardcoralsite8)

plot(hardcoralsite8$x,hardcoralsite8$y)
hardcoralsite8

for(j in 1:nrow(hardcoralsite8))
{
  if(hardcoralsite8$i[j]==0)
  {hardcoralsite8$y[j]=0.08863+0.01188*hardcoralsite8$x[j]}
}

hardcoralsite8

lm(y~x,seapensite8)

plot(seapensite8$x,seapensite8$y)
seapensite8

for(j in 1:nrow(seapensite8))
{
  if(seapensite8$i[j]==0)
  {seapensite8$y[j]=0.03047+0.00219*seapensite8$x[j]}
}

seapensite8


lm(y~x,hardcoralsite3)

plot(hardcoralsite3$x,hardcoralsite3$y)
hardcoralsite3

for(j in 1:nrow(hardcoralsite3))
{
  if(hardcoralsite3$i[j]==0)
  {hardcoralsite3$y[j]=0.16221+0.00382*hardcoralsite3$x[j]}
}



hardcoralsite3



fixednull <- data.frame(hardcoralsite1,softcoralsite2,seafansite2,bluecoralsite2,hardcoralsite2,
                         seapensite2,softcoralsite3,seafansite3,hardcoralsite3,seapensite3,hardcoralsite7,
                         softcoralsite8,seafansite8,bluecoralsite8,hardcoralsite8,seapensite8)

fixednull

write.csv(fixednull, "fixednull.csv")


hardcoralsite1

softcoralsite2

seafansite2

bluecoralsite2

hardcoralsite2

seapensite2

softcoralsite3

seafansite3

hardcoralsite3

seapensite3

hardcoralsite7

softcoralsite8

seafansite8

bluecoralsite8

hardcoralsite8

seapensite8



