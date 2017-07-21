DX <-
DT <-
S <- (DX1/DT1)*10^-6


Stress <- (X2_3$`(N)`/S2/10^9)
Strain <- (X2_3$`(mm/mm)`*100)
SSC2_3 <- data.frame(Strain,Stress)
Stress <- (X2_4$`(N)`/S2/10^9)
Strain <- (X2_4$`(mm/mm)`*100)
SSC2_4 <- data.frame(Strain,Stress)
Stress <- (X2_5$`(N)`/S2/10^9)
Strain <- (X2_5$`(mm/mm)`*100)
SSC2_5 <- data.frame(Strain,Stress)



Stress <- (X4_1$`(N)`/S4/10^9)
Strain <- (X4_1$`(mm/mm)`*100)
SSC4_1 <- data.frame(Strain,Stress)
Stress <- (X4_2$`(N)`/S4/10^9)
Strain <- (X4_2$`(mm/mm)`*100)
SSC4_2 <- data.frame(Strain,Stress)
Stress <- (X4_3$`(N)`/S4/10^9)
Strain <- (X4_3$`(mm/mm)`*100)
SSC4_3 <- data.frame(Strain,Stress)
Stress <- (X4_4$`(N)`/S4/10^9)
Strain <- (X4_4$`(mm/mm)`*100)
SSC4_4 <- data.frame(Strain,Stress)
Stress <- (X4_5$`(N)`/S4/10^9)
Strain <- (X4_5$`(mm/mm)`*100)
SSC4_5 <- data.frame(Strain,Stress)

rm(X2_1,X2_2,X2_3,X2_4,X2_5)
rm(X3_1,X3_2,X3_3,X3_4,X3_5)
rm(X4_1,X4_2,X4_3,X4_4,X4_5)

tab11 <- length(SSC1_1$Strain)
tab11

SSC11 <- SSC1_1
SSC12 <- SSC1_2
SSC13 <- SSC1_3
SSC14 <- SSC1_4
SSC15 <- SSC1_5
SSC21 <- SSC2_1
SSC22 <- SSC2_2
SSC23 <- SSC2_3
SSC24 <- SSC2_4
SSC25 <- SSC2_5
SSC31 <- SSC3_1
SSC32 <- SSC3_2
SSC33 <- SSC3_3
SSC34 <- SSC3_4
SSC35 <- SSC3_5
SSC41 <- SSC4_1
SSC42 <- SSC4_2
SSC43 <- SSC4_3
SSC44 <- SSC4_4
SSC45 <- SSC4_5
rm(SSC1_1)
rm(SSC1_2)
rm(SSC1_3)
rm(SSC1_4)
rm(SSC1_5)
rm(SSC2_1)
rm(SSC2_2)
rm(SSC2_3)
rm(SSC2_4)
rm(SSC2_5)
rm(SSC3_1)
rm(SSC3_2)
rm(SSC3_3)
rm(SSC3_4)
rm(SSC3_5)
rm(SSC4_1)
rm(SSC4_2)
rm(SSC4_3)
rm(SSC4_4)
rm(SSC4_5)

test11 <- c(rep("1",times=length(SSC11$Strain)))
test12 <- c(rep("2",times=length(SSC12$Strain)))
test13 <- c(rep("3",times=length(SSC13$Strain)))
test14 <- c(rep("4",times=length(SSC14$Strain)))
test15 <- c(rep("5",times=length(SSC15$Strain)))
test21 <- c(rep("1",times=length(SSC21$Strain)))
test22 <- c(rep("2",times=length(SSC22$Strain)))
test23 <- c(rep("3",times=length(SSC23$Strain)))
test24 <- c(rep("4",times=length(SSC24$Strain)))
test25 <- c(rep("5",times=length(SSC25$Strain)))
test31 <- c(rep("1",times=length(SSC31$Strain)))
test32 <- c(rep("2",times=length(SSC32$Strain)))
test33 <- c(rep("3",times=length(SSC33$Strain)))
test34 <- c(rep("4",times=length(SSC34$Strain)))
test35 <- c(rep("5",times=length(SSC35$Strain)))
test41 <- c(rep("1",times=length(SSC41$Strain)))
test42 <- c(rep("2",times=length(SSC42$Strain)))
test43 <- c(rep("3",times=length(SSC43$Strain)))
test44 <- c(rep("4",times=length(SSC44$Strain)))
test45 <- c(rep("5",times=length(SSC45$Strain)))
	      

tab11 <- c(rep("EQ",times=length(SSC11$Strain)))
tab12 <- c(rep("EQ",times=length(SSC12$Strain)))
tab13 <- c(rep("EQ",times=length(SSC13$Strain)))
tab14 <- c(rep("EQ",times=length(SSC14$Strain)))
tab15 <- c(rep("EQ",times=length(SSC15$Strain)))
tab21 <- c(rep("SC",times=length(SSC21$Strain)))
tab22 <- c(rep("SC",times=length(SSC22$Strain)))
tab23 <- c(rep("SC",times=length(SSC23$Strain)))
tab24 <- c(rep("SC",times=length(SSC24$Strain)))
tab25 <- c(rep("SC",times=length(SSC25$Strain)))
tab31 <- c(rep("DC",times=length(SSC31$Strain)))
tab32 <- c(rep("DC",times=length(SSC32$Strain)))
tab33 <- c(rep("DC",times=length(SSC33$Strain)))
tab34 <- c(rep("DC",times=length(SSC34$Strain)))
tab35 <- c(rep("DC",times=length(SSC35$Strain)))
tab41 <- c(rep("JZ",times=length(SSC41$Strain)))
tab42 <- c(rep("JZ",times=length(SSC42$Strain)))
tab43 <- c(rep("JZ",times=length(SSC43$Strain)))
tab44 <- c(rep("JZ",times=length(SSC44$Strain)))
tab45 <- c(rep("JZ",times=length(SSC45$Strain)))

test1 <- c(test11,test12,test13,test14,test15)
test2 <- c(test21,test22,test23,test24,test25)
test3 <- c(test31,test32,test33,test34,test35)
test4 <- c(test41,test42,test43,test44,test45)

barplot(H,names.arg=N,xlab="Treat",ylab="Ma
	x Strain %",col=rainbow(),
main="Max Strain")


MaxStrain1_1 <- max(SSC1_1$Strain)
MaxStrain1_2 <- max(SSC1_2$Strain)
MaxStrain1_3 <- max(SSC1_3$Strain)
MaxStrain1_4 <- max(SSC1_4$Strain)
MaxStrain1_5 <- max(SSC1_5$Strain)
MaxStrain2_1 <- max(SSC2_1$Strain)
MaxStrain2_2 <- max(SSC2_2$Strain)
MaxStrain2_3 <- max(SSC2_3$Strain)
MaxStrain2_4 <- max(SSC2_4$Strain)
MaxStrain2_5 <- max(SSC2_5$Strain)
MaxStrain3_1 <- max(SSC3_1$Strain)
MaxStrain3_2 <- max(SSC3_2$Strain)
MaxStrain3_3 <- max(SSC3_3$Strain)
MaxStrain3_4 <- max(SSC3_4$Strain)
MaxStrain3_5 <- max(SSC3_5$Strain)
MaxStrain4_1 <- max(SSC4_1$Strain)
MaxStrain4_2 <- max(SSC4_2$Strain)
MaxStrain4_3 <- max(SSC4_3$Strain)
MaxStrain4_4 <- max(SSC4_4$Strain)
MaxStrain4_5 <- max(SSC4_5$Strain)



MaxStress1_1 <- max(SSC1_1$Stress)
MaxStress1_2 <- max(SSC1_2$Stress)
MaxStress1_3 <- max(SSC1_3$Stress)
MaxStress1_4 <- max(SSC1_4$Stress)
MaxStress1_5 <- max(SSC1_5$Stress)
MaxStress2_1 <- max(SSC2_1$Stress)
MaxStress2_2 <- max(SSC2_2$Stress)
MaxStress2_3 <- max(SSC2_3$Stress)
MaxStress2_4 <- max(SSC2_4$Stress)
MaxStress2_5 <- max(SSC2_5$Stress)
MaxStress3_1 <- max(SSC3_1$Stress)
MaxStress3_2 <- max(SSC3_2$Stress)
MaxStress3_3 <- max(SSC3_3$Stress)
MaxStress3_4 <- max(SSC3_4$Stress)
MaxStress3_5 <- max(SSC3_5$Stress)
MaxStress4_1 <- max(SSC4_1$Stress)
MaxStress4_2 <- max(SSC4_2$Stress)
MaxStress4_3 <- max(SSC4_3$Stress)
MaxStress4_4 <- max(SSC4_4$Stress)
MaxStress4_5 <- max(SSC4_5$Stress)

MaxStrain1 <- mean(MaxStrain1_1,MaxStrain1_2,MaxStrain1_3,MaxStrain1_4,MaxStrain1_5)
MaxStrain2 <- mean(MaxStrain2_1,MaxStrain2_2,MaxStrain2_3,MaxStrain2_4,MaxStrain2_5)
MaxStrain3 <- mean(MaxStrain3_1,MaxStrain3_2,MaxStrain3_3,MaxStrain3_4,MaxStrain3_5)
MaxStrain4 <- mean(MaxStrain4_1,MaxStrain4_2,MaxStrain4_3,MaxStrain4_4,MaxStrain4_5)

library(ggplot2)
qplot(x=Strain, y=Stress, data=SSCT, color=Sample, shape=Sample, main="SSC")

library(ggplot2)
qplot(x=Strain, y=Stress, data=SSCT, color=Sample, geom="line", main="Strain-Stress Curve")
 
S2 <- split(S$SC,S$SC$test) #用split函数进行分组

theme_classic()
ggplot()


theme_set(theme_classic())
qplot(x=Strain,y=Stress,data=S$EQ,color=test,margins=TRUE,geom="line",xlab="Strain %", ylab="Stress GPa", main="EQ Strain-Stress Curve")

qplot(x=test, y=Stress, data = S$EQ , fill=test , geom = "histogram", stat="summary", fun.y="max",main="EQ Stress in Each Test")

(max.Stress <- with(S$EQ, aggregate(Stress~test, FUN=max)))

qplot(x=test, y=Stress, data=max.Stress, fill=test, geom="bar",stat="identity",main="EQ Stress in Each Test")

qplot(x=Strain,y=Stress,data=S$EQ,color=test,margins=TRUE,xlim=c(0,10),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="EQ Strain-Stress Curve")

rm(test11)
rm(test12)
rm(test13)
rm(test14)
rm(test15)
rm(test21)
rm(test22)
rm(test23)
rm(test24)
rm(test25)
rm(test31)
rm(test32)
rm(test33)
rm(test34)
rm(test35)
rm(test41)
rm(test42)
rm(test43)
rm(test44)
rm(test45)

S2 <- (DX2/DT2)*10^-6
library(xlsx)
write.xlsx(SSC1_1,"~/Desktop/20170502-2.is_tens_RawData/SSC.xlsx","sheet1")

SSC2_3 <- data.frame(Strain,Stress)

(max.Stress.EQ <- with(S$EQ, aggregate(Stress~test, FUN=max)))
(max.Stress.SC <- with(S$SC, aggregate(Stress~test, FUN=max)))
(max.Stress.DC <- with(S$DC, aggregate(Stress~test, FUN=max)))
(max.Stress.JZ <- with(S$JZ, aggregate(Stress~test, FUN=max)))

(max.Strain.EQ <- with(S$EQ, aggregate(Strain~test, FUN=max)))
(max.Strain.SC <- with(S$SC, aggregate(Strain~test, FUN=max)))
(max.Strain.DC <- with(S$DC, aggregate(Strain~test, FUN=max)))
(max.Strain.JZ <- with(S$JZ, aggregate(Strain~test, FUN=max)))

qplot(x=Strain,y=Stress,data=S$EQ,color=test,margins=TRUE,xlim=c(0,10),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="EQ Strain-Stress Curve")
qplot(x=Strain,y=Stress,data=S$SC,color=test,margins=TRUE,xlim=c(0,10),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="SC Hotsetting Strain-Stress Curve")
qplot(x=Strain,y=Stress,data=S$DC,color=test,margins=TRUE,xlim=c(0,10),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="DC Hotsetting Strain-Stress Curve")
qplot(x=Strain,y=Stress,data=S$JZ,color=test,margins=TRUE,xlim=c(0,10),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="JZ Hotsetting Strain-Stress Curve")


a <- qplot(x=Strain,y=Stress,data=S$EQ,color=test,margins=TRUE,xlim=c(0,10),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="EQ Strain-Stress Curve")
b <- qplot(x=Strain,y=Stress,data=S$SC,color=test,margins=TRUE,xlim=c(0,12),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="SC Hotsetting Strain-Stress Curve")
c <- qplot(x=Strain,y=Stress,data=S$DC,color=test,margins=TRUE,xlim=c(0,10),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="DC Hotsetting Strain-Stress Curve")
d <- qplot(x=Strain,y=Stress,data=S$JZ,color=test,margins=TRUE,xlim=c(0,10),ylim=c(0,1),geom="line",xlab="Strain %", ylab="Stress GPa", main="JZ Hotsetting Strain-Stress Curve")
library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
}
print(a, vp = vplayout(1,1)) 
print(b, vp = vplayout(1,2))  
print(c, vp = vplayout(2,1))
print(d, vp = vplayout(2,2)) ##可以用来画出一张多图

MaxStress <- c(MaxStress1_1,
	           MaxStress1_2,
	           MaxStress1_3,
	           MaxStress1_4,
	           MaxStress1_5,
	           MaxStress2_1,
	           MaxStress2_2,
	           MaxStress2_3,
	           MaxStress2_4,
	           MaxStress2_5,
	           MaxStress3_1,
	           MaxStress3_2,
	           MaxStress3_3,
	           MaxStress3_4,
	           MaxStress3_5,
	           MaxStress4_1,
	           MaxStress4_2,
	           MaxStress4_3,
	           MaxStress4_4,
	           MaxStress4_5)

p <- ggplot(MS,aes(x=Sample,y=MaxStress,fill=Test))+geom_bar(position="dodge",stat="identity")  #对于条形图的y轴就是数据框中原本的数值时，必须将geom_bar()函数中stat(统计转换)参数设置为'identity'，即对原始数据集不作任何统计变换，而该参数的默认值为'count'，即观测数量。

p <- ggplot(MS,aes(x=Sample,y=MaxStress,fill= factor(Test)))+geom_bar(position="dodge",stat="identity",width = 0.8)+geom_text(mapping = aes(label = round(MaxStress,4)), colour = 'black', vjust = 1, hjust = 0.5,position = position_dodge(0.8))
p <- ggplot(MS,aes(x=Sample,y=MaxStrain,fill= factor(Test)))+geom_bar(position="dodge",stat="identity",width = 0.8)+geom_text(mapping = aes(label = round(MaxStrain,4)), colour = 'black', vjust = 1, hjust = 0.5,position = position_dodge(0.8))

p <- ggplot(MS,aes(x=interaction(Sample,Test),y=MaxStress,fill= factor(Test)))+geom_bar(position="dodge",stat="identity",width = 0.8)+geom_text(mapping = aes(label = round(MaxStress,4)),position = position_dodge(0.9))

p <- ggplot(MS,aes(x=Sample,y=MaxStress,fill= factor(Test)))+geom_bar(position="dodge",stat="identity",width = 0.8)+geom_text(mapping = aes(label = round(MaxStress,4)), colour = 'black', vjust = 1, hjust = 0.5,position = position_dodge(0.8))+ylim(0,1)+scale_fill_discrete(name="Test")+theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),plot.title = element_text("Ultra Stregth of each Sample"))


p <- ggplot(MS,aes(x=Sample,y=MaxStress,fill= factor(Test)))+geom_bar(position="dodge",stat="identity",width = 0.8)+labs(x="Sample",y="Stress GPa",title="Stress")+ylim(0,1)+geom_text(mapping = aes(label = round(MaxStress,3)), colour = 'black', vjust = 0.005, hjust = 0.5,position = position_dodge(0.8))+scale_fill_discrete(name="Test")+theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5))     

##ggplot2画条形图示范代码
p <- ggplot(MS,aes(x=Sample,y=MaxStress,fill= factor(Test)))+ #选择所需数据
     geom_bar(position="dodge",stat="identity",width = 0.8)+ #条形图
     labs(x="Sample",y="Stress GPa",title="Stress")+ #坐标轴的标签与图标题
     ylim(0,1)+  #y轴范围
     geom_text(mapping = aes(label = round(MaxStress,3)), colour = 'black', vjust = 0.005, hjust = 0.5,position = position_dodge(0.8))+ #标签设置
     scale_fill_discrete(name="Test")+ #图例标题
     theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5))    #整体主题设置 
p

p <- ggplot(MS,aes(x=Sample,y=MaxStrain,fill= factor(Test)))+
     geom_bar(position="dodge",stat="identity",width = 0.8)+
     labs(x="Sample",y="Strain %",title="Elongation at Break Each Sample")+
     geom_text(mapping = aes(label = round(MaxStrain,3)), colour = 'black', vjust = 0.005, hjust = 0.5,position = position_dodge(0.8))+
     scale_fill_discrete(name="Test")+
     theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5))     
p

p <- ggplot(MS,aes(x=Sample,y=MaxStress,fill= factor(Test)))+
    geom_bar(position="dodge",stat="identity",width = 0.8)+
    labs(x="Sample",y="Stress GPa",title="Ultimate Strength of Each Sample")+
    geom_text(mapping = aes(label = round(MaxStress,3)), colour = 'black', vjust = -1, hjust = 0.5,position = position_dodge(0.8))+
    ylim(0,1)+
    scale_fill_discrete(name="Test")+
    theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5))     
p


write.csv(SSCT,"20170502SSC.csv",row.names = FALSE,fileEncoding = "utf8")

theme_zg <- function(..., bg='white')
{
	require(grid)
	theme_classic(...)
	+theme(rect=element_rect(fill=bg), 
	plot.margin=unit(rep(0.5,4), 'lines'),
	panel.background=element_rect(fill='transparent', color='black'),
	panel.border=element_rect(fill='transparent', color='transparent'), 
	panel.grid=element_blank(),
	axis.title = element_text(color='black', vjust=0.1),
	axis.ticks.length = unit(-0.4,"lines"),
	axis.ticks = element_line(color='black'),
	legend.title=element_blank(),
	legend.key=element_rect(fill='transparent', color='transparent'))
}







###Kindle书榜爬虫
library(XML)

givePrice = function(rootNode){
  price<-xpathSApply(rootNode,"//strong[@class='price']",xmlValue)
  price
  ## 收费付费混排,我只要付费的价格(循环补齐式选择)
  (price = price[c(T,F)])
  ## 喂,你给我认真处理下数据,把￥去掉,再转数字
  strsplit(price," ") -> price.c
  unlist(price.c)[c(F,T)] -> price.c
  as.numeric(price.c)[1:20] -> price
  price
}

giveRate = function(rootNode){
  rate<-xpathSApply(rootNode,"//a[@style='text-decoration:none']/span",xmlValue)
  rate[c(T,F,F,F)] -> rate
  strsplit(rate,"平均") -> rate.c
  unlist(rate.c)[c(F,T)] -> rate.c
  strsplit(rate.c," 星") -> rate.c
  unlist(rate.c) -> rate.c
  as.numeric(rate.c) ->　rate
  rate
}

giveNumber = function(rootNode){
  number<-xpathSApply(rootNode,"//span[@class='crAvgStars']/a",xmlValue)
  number[c(T,F)] -> number.c
  sub(",","",number.c) ->number.c
  as.numeric(number.c)
}

giveNames = function(rootNode){
  names <- xpathSApply(rootNode,"//div[@class='zg_title']/a",xmlValue)
  names[c(T,F)]
}

giveAuthors = function(rootNode){
  authors <- xpathSApply(rootNode,"//div[@class='zg_byline']",xmlValue)
  authors[c(T,F)] -> authors
  sub("\n\n\n\n\n\n\n~ ","",authors)
}

getAmazonBy1 = function(URL){
  Sys.sleep(runif(1,1,2))
  doc<-htmlParse(URL[1],encoding="UTF-8")
  rootNode<-xmlRoot(doc)
  data.frame(
  Price = givePrice(rootNode),
  # 求价格
  Rate = giveRate(rootNode),
  # 求评价
  Number = giveNumber(rootNode),
  # 求评价数
  Name = giveNames(rootNode),
  # 求书名
  Author = giveAuthors(rootNode)
  # 求作者
  )
}

#################主程序部分############################3

mainfunction = function(URL){
  data = rbind(
            getAmazonBy1(URL[1]),
            getAmazonBy1(URL[2]),
            getAmazonBy1(URL[3]),
            getAmazonBy1(URL[4]),
            getAmazonBy1(URL[5]))
  data = cbind(data,1:100) 

}

################运行部分###############################3
URL = paste0("http://www.amazon.cn/gp/bestsellers/digital-text/116169071/ref=zg_bs_116169071_pg_1?ie=UTF8&pg=",1:5)

data = mainfunction(URL) 
write.csv(data, file = paste0(Sys.Date(),".csv"))





###对数据分类统计，并添加误差线
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # 计算长度
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # 以 groupvars 为组,计算每组的长度,均值,以及标准差
    # ddply 就是 dplyr 中的 group_by + summarise
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # 重命名  
    datac <- plyr::rename(datac, c("mean" = measurevar))

    # 计算标准偏差
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    # 计算置信区间
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}
MSS <- summarySE(MS,measurevar = "MaxStress",groupvars = "Sample")
MSN <- summarySE(MS,measurevar = "MaxStrain",groupvars = "Sample")
MSM <- summarySE(MS,measurevar = "Modulus",groupvars = "Sample")

ggplot(MSS, aes(x=Sample, y=MaxStress, fill=Sample)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3,width = 0.5) +      # Thinner lines
    geom_errorbar(aes(ymin=MaxStress-se, ymax=MaxStress+se),
                  size=.5,    # Thinner lines
                  width=.2,
                  position=position_dodge(0.9)) +
    xlab("Sample") +
    ylab("Stress GPa") +
    ggtitle("Ultimate Strength of Each Sample") +
    scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.1)) +
    theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5,size = 22),axis.title.x=element_text(size=18,hjust=0.5),axis.title.y=element_text(size=18,hjust=0.5),axis.text.x=element_text(size=16),axis.text.y=element_text(size=14))+
    guides(fill=FALSE)

ggplot(MSN, aes(x=Sample, y=MaxStrain, fill=Sample)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3,width = 0.5) +      # Thinner lines
    geom_errorbar(aes(ymin=MaxStrain-se, ymax=MaxStrain+se),
                  size=.5,    # Thinner lines
                  width=.2,
                  position=position_dodge(0.9)) +
    xlab("Sample") +
    ylab("Strain %") +
    ggtitle("Elongation at Break of Each Sample") +
    scale_y_continuous(limits = c(0,12),breaks=seq(0,12,1)) +
    theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5,size = 22),axis.title.x=element_text(size=18,hjust=0.5),axis.title.y=element_text(size=18,hjust=0.5),axis.text.x=element_text(size=16),axis.text.y=element_text(size=14))+
    guides(fill=FALSE)


ggplot(MSM, aes(x=Sample, y=Modulus, fill=Sample)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    geom_errorbar(aes(ymin=Modulus-se, ymax=Modulus+se),
                  size=.5,    # Thinner lines
                  width=.2,
                  position=position_dodge(0.9)) +
    xlab("Sample") +
    ylab("Modulus GPa") +
    ggtitle("Elastic Modulus of Each Sample") +
    scale_y_continuous(limits = c(0,0.25),breaks=seq(0,0.25,0.05)) +
    theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5,size = 22),axis.title.x=element_text(size=18,hjust=0.5),axis.title.y=element_text(size=18,hjust=0.5),axis.text.x=element_text(size=16),axis.text.y=element_text(size=14))+
    guides(fill=FALSE)

ggplot(SSC, aes(x=Strain, y=Stress, color=Sample,shape = Sample)) + 
    geom_point(size = 2)+
    geom_line(size = 1)+
    scale_shape_manual(values = c(15,16,17,18))+
    xlab("Stain %") +
    ylab("Stress GPa") +
    scale_y_continuous(limits = c(0,0.9),breaks=seq(0,0.9,0.1)) +
    scale_x_continuous(limits = c(0,11),breaks=seq(0,11,1)) +
    ggtitle("Stress-Strain Curve of Each Sample") +
    theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5,size = 22),axis.title.x=element_text(size=18,hjust=0.5),axis.title.y=element_text(size=18,hjust=0.5),axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),legend.key.size = unit(2,"lines"),legend.background = element_blank(),legend.text = element_text(size = 12))
panel.border =element_rect()

ggplot(HTTDMA, aes(x=Temperature, y=Stress, color=factor(HTT))) + 
    geom_line(size = 2)+
    xlab("Temperature") +
    ylab("Stress N/tex")+
    scale_x_continuous(limits = c(30,210) , breaks=seq(30,210,30)) +
    ggtitle("Inner Stress of Each Sample") +
    theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5,size = 22),axis.title.x=element_text(size=18,hjust=0.5),axis.title.y=element_text(size=18,hjust=0.5),axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),legend.key.size = unit(2,"lines"),legend.background = element_blank(),legend.text = element_text(size = 12),legend.title = element_blank())


##求取模量
ML11 <- filter(S$EQ,test == "1", Strain < 1)
ML12 <- filter(S$EQ,test == "2", Strain < 1)
ML13 <- filter(S$EQ,test == "3", Strain < 1)
ML14 <- filter(S$EQ,test == "4", Strain < 1)
ML15 <- filter(S$EQ,test == "5", Strain < 1)
ML21 <- filter(S$SC,test == "1", Strain < 1)
ML22 <- filter(S$SC,test == "2", Strain < 1)
ML23 <- filter(S$SC,test == "3", Strain < 1)
ML24 <- filter(S$SC,test == "4", Strain < 1)
ML25 <- filter(S$SC,test == "5", Strain < 1)
ML31 <- filter(S$DC,test == "1", Strain < 1)
ML32 <- filter(S$DC,test == "2", Strain < 1)
ML33 <- filter(S$DC,test == "3", Strain < 1)
ML34 <- filter(S$DC,test == "4", Strain < 1)
ML35 <- filter(S$DC,test == "5", Strain < 1)
ML41 <- filter(S$JZ,test == "1", Strain < 1)
ML42 <- filter(S$JZ,test == "2", Strain < 1)
ML43 <- filter(S$JZ,test == "3", Strain < 1)
ML44 <- filter(S$JZ,test == "4", Strain < 1)
ML45 <- filter(S$JZ,test == "5", Strain < 1)
myfit11 <- lm(Stress~Strain,data = ML11)
myfit12 <- lm(Stress~Strain,data = ML12)
myfit13 <- lm(Stress~Strain,data = ML13)
myfit14 <- lm(Stress~Strain,data = ML14)
myfit15 <- lm(Stress~Strain,data = ML15)
myfit21 <- lm(Stress~Strain,data = ML21)
myfit22 <- lm(Stress~Strain,data = ML22)
myfit23 <- lm(Stress~Strain,data = ML23)
myfit24 <- lm(Stress~Strain,data = ML24)
myfit25 <- lm(Stress~Strain,data = ML25)
myfit31 <- lm(Stress~Strain,data = ML31)
myfit32 <- lm(Stress~Strain,data = ML32)
myfit33 <- lm(Stress~Strain,data = ML33)
myfit34 <- lm(Stress~Strain,data = ML34)
myfit35 <- lm(Stress~Strain,data = ML35)
myfit41 <- lm(Stress~Strain,data = ML41)
myfit42 <- lm(Stress~Strain,data = ML42)
myfit43 <- lm(Stress~Strain,data = ML43)
myfit44 <- lm(Stress~Strain,data = ML44)
myfit45 <- lm(Stress~Strain,data = ML45)
TG11 <- coefficients(myfit11)[2]
TG12 <- coefficients(myfit12)[2]
TG13 <- coefficients(myfit13)[2]
TG14 <- coefficients(myfit14)[2]
TG15 <- coefficients(myfit15)[2]
TG21 <- coefficients(myfit21)[2]
TG22 <- coefficients(myfit22)[2]
TG23 <- coefficients(myfit23)[2]
TG24 <- coefficients(myfit24)[2]
TG25 <- coefficients(myfit25)[2]
TG31 <- coefficients(myfit31)[2]
TG32 <- coefficients(myfit32)[2]
TG33 <- coefficients(myfit33)[2]
TG34 <- coefficients(myfit34)[2]
TG35 <- coefficients(myfit35)[2]
TG41 <- coefficients(myfit41)[2]
TG42 <- coefficients(myfit42)[2]
TG43 <- coefficients(myfit43)[2]
TG44 <- coefficients(myfit44)[2]
TG45 <- coefficients(myfit45)[2]
Modulus <- c(TG11,TG12,TG13,TG14,TG15,TG21,TG22,TG23,TG24,TG25,TG31,TG32,TG33,TG34,TG35,TG41,TG42,TG43,TG44,TG45)

SSC1 <- filter(S$EQ,test == "3")
SSC2 <- filter(S$SC,test == "3")
SSC3 <- filter(S$DC,test == "3")
SSC4 <- filter(S$JZ,test == "3")


HTTDMA$Temperature <- as.numeric(HTTDMA$Temperature)
HTTDMA$Force <- as.numeric(HTTDMA$Force)
HTTDMA$Stress <- as.numeric(HTTDMA$Stress)


ggplot(X20170702Creep, aes(x=Decay Time,y=Strain, color=Test, shape = Test)) + 
    geom_point(size = 2)+
    scale_shape_manual(values = c(15,16,17))+
    xlab("Stain %") +
    ylab("Stress GPa") + +
    scale_x_continuous(limits = c(0,11),breaks=seq(0,11,1)) +
    ggtitle("Stress-Strain Curve of Each Sample") +
    theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5,size = 22),axis.title.x=element_text(size=18,hjust=0.5),axis.title.y=element_text(size=18,hjust=0.5),axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),legend.key.size = unit(2,"lines"),legend.background = element_blank(),legend.text = element_text(size = 12))
panel.border =element_rect()

Creep1 <- filter(Test1, Recoverable Compliance == 0)
Creep1 <- filter(Test1, Recoverable Compliance == 0)
Creep1 <- filter(Test1, Recoverable Compliance == 0)

ggplot(Creep1, aes(x=Time,y=Strain)) + 
    geom_point(size = 2)+
    theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5,size = 22),axis.title.x=element_text(size=18,hjust=0.5),axis.title.y=element_text(size=18,hjust=0.5),axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),legend.key.size = unit(2,"lines"),legend.background = element_blank(),legend.text = element_text(size = 12))
panel.border =element_rect()

ggplot(Creep2, aes(x=Time,y=Strain)) + 
    geom_point(size = 2)+
    geom_line(size = 1)+
    theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5,size = 22),axis.title.x=element_text(size=18,hjust=0.5),axis.title.y=element_text(size=18,hjust=0.5),axis.text.x=element_text(size=16),axis.text.y=element_text(size=14),legend.key.size = unit(2,"lines"),legend.background = element_blank(),legend.text = element_text(size = 12))
panel.border =element_rect()

#Burger模型拟合
Burger <- function(t,b0,b1,b2,a,t2)
{
  (a/b0) + (a/b1) * (1-exp(-t/t2)) + ((a*t)/b2)
}

fit1 <- nls(Strain ~ Burger(DecayTime,b0,b1,b2,Stress,t2), data = Creep2 , start = list(b0=0.01,b1=0.01,b2=0.01,t2=0.1))
summary(fit1)
lines(Creep2$DecayTime, predict(fit1), col = "red", lty = 2, lwd = 3)

plot(y, -5, 5, main = expression(epsilon == frac(sigma,E[1])+frac(sigma,E[2])*(1-exp(-frac(t,tau)))+frac(sigma*t,eta[3])), lwd = 3, col = "blue")
expression(epsilon == frac(sigma,E[1])+frac(sigma,E[2])*(1-exp(-frac(t,tau)))+frac(sigma*t,eta[3]))
expression(epsilon == frac(sigma,coef(fit1)[1])+frac(sigma,E[2])*(1-exp(-frac(t,tau)))+frac(sigma*t,eta[3]))
gongshi1 <- bquote(epsilon == frac(sigma,.(coef(fit1)[1]))+frac(sigma,.(coef(fit1)[2]))*(1-exp(-frac(t,.(coef(fit1)[4]))))+frac(sigma*t,.(coef(fit1)[3])))
plot(y, -5, 5, main = gongshi1, lwd = 3, col = "blue")

gongshi1 <- bquote(epsilon == frac(sigma,.(round(coef(fit1)[1]),2))+frac(sigma,.(coef(fit1)[2]))*(1-exp(-frac(t,.(coef(fit1)[4]))))+frac(sigma*t,.(coef(fit1)[3])))
plot(y, -5, 5, main = gongshi1, lwd = 3, col = "blue")

ggplot(Creep2,mapping = aes(DecayTime,Strain))+geom_point()+geom_smooth()


##ggplot2添加拟合曲线的例子
conc <- c(2.856829, 5.005303, 7.519473, 22.101664, 27.769976, 39.198025, 45.483269, 203.784238)
rate <- c(14.58342, 24.74123, 31.34551, 72.96985, 77.50099, 96.08794, 96.96624, 108.88374)
L.minor <- data.frame(conc, rate)
L.minor.m1 <- nls(rate ~ Vm * conc/(K + conc), data = L.minor, #采用M-M动力学方程
                  start = list(K = 20, Vm = 120), #初始值设置为K=20，Vm=120
                  trace = TRUE) #占线拟合过程
#确定x轴范围并构建数据集
min <- range(L.minor$conc)[1]
max <- range(L.minor$conc)[2]
line.data <- data.frame(conc = seq(min, max, length.out = 1000))
#用模型预测数据构建数据集
line.data$p.predict <- predict(L.minor.m1, newdata = line.data)
 
require(ggplot2)
M_Mfunction <- ggplot() +
  geom_point(aes(x = conc, y = rate), data = L.minor,
             alpha = 0.5, size = 5, color = "red") +
  geom_line(aes(x = conc, y = p.predict), data = line.data,
            size = 1, color = "blue") +
  scale_x_continuous(
    name = expression(Substrate ~~ concentration(mmol ~~ m^3)),#采用expression来表示数学公式
    breaks = seq(0, 200, by = 25)) +
  scale_y_continuous(
    name = "Uptake rate (weight/h)",
    breaks = seq(0, 120, by = 10)) +
  geom_text(aes(x = 100, y = 60),
            label = "bolditalic(f(list(x, (list(K, V[m])))) == frac(V[m]%.%x, K+x))",
            #注意 geom_text中如果用expression()来进行表达，必须开启parse = TRUE
            #同时以字符串""的形式表示，不能使用expression
            parse = TRUE, 
            size = 5, family = "times"
            ) +
  theme_bw() +
  theme(
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))


##160c Creep Curve Fitting
Creep160 <- read_csv("~/Desktop/160Creep/160Creep.csv")
Creep160$Test <- as.factor(Creep160$Test)

ggplot(Creep160, aes(x=Time,y=Strain, color=Test)) + 
     geom_point(size = 4,shape =17)+
     xlab("Time (min)") +
     ylab("Strain (%)") + 
     theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 12)
      )

CreepMain <- filter(Creep160, StrainRecovery == 0)

ggplot(CreepMain, aes(x=DecayTime,y=Strain, color=Test)) + 
     geom_point(size = 4,shape =17)+
     geom_smooth(se = FALSE)+
     xlab("Time (min)") +
     ylab("Strain (%)") + 
     theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 12)
      )

Burger <- function(t,e1,e2,eta,sigma,tau)
{
  (sigma/e1) + (sigma/e2) * (1-exp(-t/tau)) + ((sigma*t)/eta)
}
CreepMain$Time.s <- CreepMain$DecayTime*60 ##换算成s
DL <- 0.1223
DV <- 1.2364
S <- (DL/DV)*10^-6
SS <- S/1000
CreepMain$StressFixed <- CreepMain$StaticForce/SS ##换算成Pa
CreepMain$StressFixed.MPa <- CreepMain$StressFixed/(10^6) ##换算成MPa
CreepMain$DStrain <- CreepMain$Strain/CreepMain$DecayTime ##蠕变速率

ggplot(CreepMain, aes(x=DecayTime,y=DStrain, color=Test)) + 
     geom_point(size = 4,shape =17, alpha = 0.5)+
     geom_smooth(se = FALSE)+
     scale_x_continuous(
     name = "Time (min)",
    breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(
    name = bquote(Creep ~~ Rate ~~ (min^-1)),
    limits = c(0,40),
    breaks = seq(0, 40, by = 10)) +
    mytheme

CreepMain[CreepMain == Inf] <- 40 ##出现了Inf影响拟合

ggplot(CreepMain, aes(x=DecayTime,y=DStrain, color=Test)) + 
     geom_point(size = 7,shape =17, alpha = 0.8)+
     stat_smooth(geom = "smooth", position = "identity", span = 0.1)+
     scale_x_continuous(
     name = "Time (min)",
    breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(
    name = bquote(Creep ~~ Rate ~~ (min^-1)),
    limits = c(0,40),
    breaks = seq(0, 40, by = 10)) +
    theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.position = c(0.5,0.7)
      )
    
ggplot(CreepMain, aes(x=DecayTime,y=Strain, color=Test)) + 
     geom_point(size = 6,shape =17)+
     stat_smooth(geom = "smooth", position = "identity", span = 0.1)+
     scale_x_continuous(
     name = "Time (min)",
    breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(
    name = "Strain (%)",
    limits = c(0,2.5),
    breaks = seq(0, 2.5, by = .5)) + 
     theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.position = c(0.1,0.8)
      )


mytheme <- theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 12)
      )

CreepMain$StrainNP <- CreepMain$Strain/100
CreepM <- split(CreepMain,CreepMain$Test)  ##按测试分组

Test1.fit <- nls(Strain ~ Burger(DecayTime,e1,e2,eta,StressFixed.MPa,tau), data = CreepM$`1` , start = list(e1 = 0.01,e2 = 0.01,eta = 0.01,tau = 0.01))
summary(Test1.fit)
Test1.Pre <- predict(Test1.fit)
plot(CreepM$`1`$DecayTime,CreepM$`1`$Strain)
lines(CreepM$`1`$DecayTime,Test1.Pre,col = "red", lty = 2, lwd = 3)

Test2.fit <- nls(Strain ~ Burger(DecayTime,e1,e2,eta,StressFixed.MPa,tau), data = CreepM$`2` , start = list(e1 = 0.01,e2 = 0.01,eta = 0.01,tau = 0.01))
summary(Test2.fit)
Test2.Pre <- predict(Test2.fit)
plot(CreepM$`2`$DecayTime,CreepM$`2`$Strain)
lines(CreepM$`2`$DecayTime,Test2.Pre,col = "red", lty = 2, lwd = 3)

Test3.fit <- nls(Strain ~ Burger(DecayTime,e1,e2,eta,StressFixed.MPa,tau), data = CreepM$`3` , start = list(e1 = 0.01,e2 = 0.01,eta = 0.01,tau = 0.01))
summary(Test3.fit)
Test3.Pre <- predict(Test3.fit)
plot(CreepM$`3`$DecayTime, CreepM$`3`$Strain)
lines(CreepM$`3`$DecayTime, Test3.Pre, col = "red", lty = 2, lwd = 3)

Test4.fit <- nls(Strain ~ Burger(DecayTime,e1,e2,eta,StressFixed.MPa,tau), data = CreepM$`4` , start = list(e1 = 0.01,e2 = 0.01,eta = 0.01,tau = 0.01))
summary(Test4.fit)
Test4.Pre <- predict(Test4.fit)
plot(CreepM$`4`$DecayTime, CreepM$`4`$Strain)
lines(CreepM$`4`$DecayTime, Test4.Pre, col = "red", lty = 2, lwd = 3)

Test5.fit <- nls(Strain ~ Burger(DecayTime,e1,e2,eta,StressFixed.MPa,tau), data = CreepM$`5` , start = list(e1 = 10, e2 = 10, eta = 100, tau = 0.01))
summary(Test5.fit)
Test5.Pre <- predict(Test5.fit)
plot(CreepM$`5`$DecayTime, CreepM$`5`$Strain)
lines(CreepM$`5`$DecayTime, Test5.Pre, col = "red", lty = 2, lwd = 3)

ggplot(CreepM$'2', aes(x=DecayTime,y=Strain)) +
     geom_line(aes(x=DecayTime,y=Test2.Pre), size = 2, color = "red")+ 
     geom_point(size = 4,shape =17, color = "black", alpha = .5)+
     scale_x_continuous(
     name = "Time (min)",
    breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(
    name = "Strain (%)",
    limits = c(0,2.5),
    breaks = seq(0, 2.5, by = .5)) + 
     theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      )

 gongshi <- bquote(epsilon == frac(sigma,.(round(coef(Test2.fit)[1]),4))
            +frac(sigma,.(round(coef(Test2.fit)[2]),4))
            *(1-exp(-frac(t,.(round(coef(Test2.fit)[4]),4))))
            +frac(sigma*t,.(round(coef(Test2.fit)[3]),4)))
 plot(CreepM$`5`$DecayTime, CreepM$`5`$Strain, main =)

BurgerRate <- function(t,e2,eta,sigma,tau)
{
   (sigma/(e2*tau)) * (exp(-t/tau)) + (sigma/eta)
}
Test2.fit.rate <- nls(DStrain ~ BurgerRate(DecayTime,e2,eta,StressFixed.MPa,tau), data = CreepM$`2` , start = list(e2 = 10,eta = 100,tau = 1))
summary(Test2.fit.rate)
Test2.Pre.rate <- predict(Test2.fit.rate)
plot(CreepM$`2`$DecayTime,CreepM$`2`$DStrain)
lines(CreepM$`2`$DecayTime,Test2.Pre.rate,col = "red", lty = 2, lwd = 3)

##折腾折光指数
       {
      1.3640 20
       1.3715 25
       1.3803 30.4
       1.3952 40.1
       1.4110 50
       1.4263 60.5
       1.4306 63.2
       1.4433 72
       zg <- c(1.3640,1.3715,1.3803,1.3952,1.4110,1.4263,1.4306,1.4433)
       nd <- c(20,25,30.4,40.1,50,60.5,63.2,72)
       qx <- data.frame(zg, nd)
       
       ggplot(ckd, aes(x=zgall, y=ndall)) +
            geom_line(size = 1,)+ 
            scale_x_continuous(
            name = "Refractive Index",
            limits = c(1.42, 1.46),
           breaks = seq(1.425, 1.46, by = 0.005)) +
           scale_y_continuous(
           name = "Concentration (%)",
           limits = c(60,80),
           breaks = seq(60, 80, by = 1)) + 
            theme(
             panel.grid.major=element_line(size=.5,linetype =1,),
             panel.grid.minor=element_line(size=.5,linetype =3,),
             axis.line = element_line(size = .5),
             plot.title = element_text(hjust = 0.5,size = 22),
             axis.title.x=element_text(size=18,hjust=0.5),
             axis.title.y=element_text(size=18,hjust=0.5),
             axis.text.x=element_text(size=16),
             axis.text.y=element_text(size=14),
             panel.border = element_rect(colour = "black",linetype = 1,size = 1),
             )       
          }



ggplot(CreepM$'2', aes(x=DecayTime,y=Strain)) +
     geom_line(aes(x=DecayTime,y=Test2.Pre), size = 2, color = "red")+ 
     geom_point(size = 4,shape =17, color = "black", alpha = .5)+
     annotate("text", x = 20, y = 0.5,label = "paste(italic(R) ^ 2, \" = .75\")", parse = TRUE)+
     scale_x_continuous(
     name = "Time (min)",
    breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(
    name = "Strain (%)",
    limits = c(0,2.5),
    breaks = seq(0, 2.5, by = .5)) + 
     theme(
      panel.background = element_rect(fill='transparent', color='black'),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      )


ggplot(CreepM$'2', aes(x=DecayTime,y=Strain)) +
     geom_line(aes(x=DecayTime,y=Test2.Pre), size = 2, color = "red")+ 
     geom_point(size = 4,shape =17, color = "black", alpha = .5)+
     geom_text(aes(label = gongshi2), parse = TRUE)+
     scale_x_continuous(
     name = "Time (min)",
    breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(
    name = "Strain (%)",
    limits = c(0,2.5),
    breaks = seq(0, 2.5, by = .5)) + 
     theme(
      panel.background = element_rect(fill='transparent', color='black'),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      )

Burger <- function(t,e1,e2,eta,sigma,tau)
{
  (sigma/e1) + (sigma/e2) * (1-exp(-t/tau)) + ((sigma*t)/eta)
}

STrain <- Burger(CreepM$`2`$DecayTime,)

ggplot(EQ_HT_Creep, aes(x = Time, y = Strain, color = Sample, shape = Test)) + 
     geom_point(size = 2)+
     xlab("Time (min)") +
     ylab("Strain (%)") + 
     theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 12)
      )

DL <- 0.1223
DV <- 1.2364
S <- (DL/DV)*10^-6
SS <- S/1000
EQ_HT_Creep$StressFixed.Pa <- EQ_HT_Creep$StaticForce/SS ##换算成Pa
EQ_HT_Creep$StressFixed.MPa <- EQ_HT_Creep$StressFixed.Pa/(10^6) ##换算成MPa

ggplot(CreepSample$`100`, aes(x = Time, y = Strain, color = Test, shape = Test)) + 
     geom_point(size = 4)+
     geom_line()+
     xlab("Time (min)") +
     ylab("Strain (%)") + 
     theme(
      panel.background = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=18,hjust=0.5),
      axis.title.y=element_text(size=18,hjust=0.5),
      axis.text.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      legend.key.size = unit(2,"lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = c(0.8,0.8)
      )
Creep100 <- filter(CreepSample$`100`, Test == 1)
Creep120 <- filter(CreepSample$`120`, Test == 2)
Creep140 <- filter(CreepSample$`140`, Test == 1)
Creep160 <- filter(CreepSample$`160`, Test == 2)
Creep170 <- filter(CreepSample$`170`, Test == 2)
Creep180 <- filter(CreepSample$`180`, Test == 1)
CreepAll <- rbind(Creep100,Creep120,Creep140,Creep160,Creep170,Creep180)
CreepAll <- CreepAll[,-2]

ggplot(CreepAll, aes(x = Time, y = Strain, color = Sample)) + 
     geom_point(size = 3, shape = 17, alpha = .8)+
     ggtitle("PAN Fibers High Temperature \n Creep & Recovery Behavior")+
     scale_x_continuous(
     name = "Time (min)",
     limits = c(3,68),
    breaks = seq(5, 65, by = 5)) +
    scale_y_continuous(
    name = "Strain (%)",
    limits = c(-1,25),
    breaks = seq(0, 25, by = 5)) +
    guide_legend(title = "Temperature")+
     theme(
      panel.background = element_rect(fill='transparent', color='black'),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=20,hjust=0.5),
      axis.title.y=element_text(size=20,hjust=0.5),
      axis.text.x=element_text(size=20),
      axis.text.y=element_text(size=20),
      legend.key=element_rect(fill='transparent'),
      legend.key.size = unit(2,"lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 20),
      legend.position = c(0.1,0.75),
      legend.title = element_text(size = 20)
      )

CreepMain <- filter(CreepAll, StrainRecovery == 0)

ggplot(CreepMain, aes(x = DecayTime, y = Strain, color = Sample)) + 
     geom_point(size = 3, shape = 17, alpha = .8)+
     ggtitle("PAN Fibers High Temperature \n Creep & Recovery Behavior")+
     scale_x_continuous(
     name = "Time (min)",
     limits = c(0,30),
    breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(
    name = "Strain (%)",
    limits = c(-1,20),
    breaks = seq(0, 20, by = 5)) +
     theme(
      panel.background = element_rect(fill='transparent', color='black'),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=20,hjust=0.5),
      axis.title.y=element_text(size=20,hjust=0.5),
      axis.text.x=element_text(size=20),
      axis.text.y=element_text(size=20),
      legend.key=element_rect(fill='transparent'),
      legend.key.size = unit(2,"lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 20),
      legend.position = c(0.1,0.7),
      legend.title = element_text(size = 20)
      )

ggplot(CreepMain, aes(x = DecayTime, y = Strain, color = Sample)) + 
     geom_point(size = 3, shape = 17, alpha = .8)+
     ggtitle("PAN Fibers High Temperature \n Creep Behavior")+
     scale_x_continuous(
     name = "Time (min)",
     limits = c(0,1),
    breaks = seq(0, 1, by = .1)) +
    scale_y_continuous(
    name = "Strain (%)",
    limits = c(-1,2),
    breaks = seq(0, 2, by = .5)) +
     theme(
      panel.background = element_rect(fill='transparent', color='black'),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=20,hjust=0.5),
      axis.title.y=element_text(size=20,hjust=0.5),
      axis.text.x=element_text(size=20),
      axis.text.y=element_text(size=20),
      legend.key=element_rect(fill='transparent'),
      legend.key.size = unit(2,"lines"),
      legend.background = element_blank(),
      legend.text = element_text(size = 20),
      legend.position = c(0.1,0.75),
      legend.title = element_text(size = 20)
      )

ggplot(CreepMain, aes(x=DecayTime,y=DStrain, color=Sample))+
      geom_point(size = 4, shape = 17, alpha = .8)+
      ggtitle("PAN Fibers High Temperature \n Creep Rate Curve")+
     scale_x_continuous(
     name = "Time (min)",
     limits = c(0,30),
    breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(
    name = bquote(Creep ~~ Rate ~~ (min^-1)),
    limits = c(0,50),
    breaks = seq(0, 50, by = 5)) +
     theme(
      panel.background = element_rect(fill='transparent', color='black'),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5,size = 22),
      axis.title.x=element_text(size=20,hjust=0.5),
      axis.title.y=element_text(size=20,hjust=0.5),
      axis.text.x=element_text(size=20),
      axis.text.y=element_text(size=20),
      legend.key=element_rect(fill='transparent'),
      legend.key.size = unit(8,"mm"),
      legend.background = element_blank(),
      legend.text = element_text(size = 22),
      legend.position = c(0.7,0.7),
      legend.title = element_text(size = 20)
      )
##系统中中文字体列表
InconsolataN-Bold.otf
InconsolataN-Regular.otf
Inconsolatazi4-Bold.otf
Inconsolatazi4-Regular.otf
NotoSansCJK-Black.otf
NotoSansCJK-DemiLight.otf
NotoSansCJK-Light.otf
NotoSansCJK-Medium.otf
NotoSansCJK-Regular.otf
NotoSansCJK-Thin.otf
SourceHanSansCN-Bold.otf
SourceHanSansCN-ExtraLight.ot
SourceHanSansCN-Heavy.otf
SourceHanSansCN-Light.otf
SourceHanSansCN-Medium.otf
SourceHanSansCN-Normal.otf
SourceHanSansCN-Regular.otf








