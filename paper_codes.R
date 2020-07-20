library(nCov2019)
library(chinamap)
library(sp)
library(mapproj)
library(sf)
library(dplyr)
library(ggplot2)
x = get_nCov2019()
daily_newadd_data=summary(x,by='today')#2020-01-20
#dates= seq(from=as.Date("2020-01-20"), to=as.Date("2020-06-30"), by=1)
#生成序列日期，原始数据的日期格式，不利于作图，修改日期
#第二次修改，按照上面画图，x轴的刻度太大，因此，按照天数来画图
dates=1:nrow(daily_newadd_data)
daily_newadd_data$date=dates
#这样之后，就不用axis函数来完成月份标注




#########fig1a全国范围每日新增数据的基本图形1###########################################
attach(daily_newadd_data)
lwdN=1.8
png(file = "F:/2019-nCoV/figures/fig1a.png", width = 400, height =350)
par(ps=9,mar=c(2.5,3,0.5, 0.2),mgp=c(1.5,0.5,0))
#mgp是坐标轴标题、刻度值和轴线与绘图边框的距离
plot(date,confirm,type='o',
     col='orange',xlab='时间',
     ylab='单日新增人数',lwd=lwdN,pch=20,cex.axis=1.5,xaxt="n",cex.lab=2)
grid(nx=10,ny=NA)#网格
#重新设置x轴的刻度，原来的刻度是1，2，3,4,5,6，没有显示出月份
axis(side=1,at=c(12,41,72,102,133),labels=c('二月','三月','四月','五月','六月'))
points(date,suspect,col='yellow',pch=20)
lines(date,suspect,col='yellow',lwd=lwdN)
points(date,heal,col='blue',pch=20)
lines(date,heal,col='blue',lwd=lwdN)
points(date,dead*10,col='red',pch=20,cex=1.5)
lines(date,dead*10,col='red',lwd=lwdN)
legend("topright", bg='lightgray',                                 
legend=c("每日新增死亡人数","每日新增确诊人数","每日新增疑似病例人数","每日新增治愈人数"),        
col=c("red","orange","yellow","blue"),pch=c(20,20,20,20),                 
lwd=lwdN,cex=2)

points(24,15153,pch=16,col='orange',cex=2)
points(17,5328,pch=16,col='yellow',cex=2)
points(39,3626,pch=16,col='blue',cex=2)
points(24,2540,pch=16,col='red',cex=2)

rect(-2.96,14720,79.96,15679.68,col='lightgray')
rect(1.44,5550.7,79.6,6610,col='lightgray')
rect(47.67,3266.722,126.2,4325.961,col='lightgray')
rect(49.137,2042,123.62,3068.115,col='lightgray')
text(64.34-25,15099.48+100,labels='2020-02-12,15153例',cex=2)
text(30.69+10,5857+200,labels='2020-02-05,5328例',cex=2)
text(77.14663+10,3776,labels='2020-02-27,3626例',cex=2)
text(82.14663+5,2540,labels='2020-02-12,254例',cex=2)
arrows(24,2540,51.1,2567.7,angle=25,length=0.1)
dev.off()
#######################################################################################

dev.new()
####################fig1b死亡率与治愈率###################################################
attach(daily_newadd_data)
lwdN=1.8
png(file = "F:/2019-nCoV/figures/fig1b.png", width = 400, height = 350)
par(ps=9,mar=c(2.5,3,0.5, 0.2),mgp=c(1.5,0.5,0))
#mgp是坐标轴标题、刻度值和轴线与绘图边框的距离
plot(date,healRate,type='o',
     col='blue',xlab='时间',
     ylab='单日死亡率与治愈率',lwd=lwdN,pch=20,cex.axis=1.5,xaxt="n",cex.lab=2)
grid(nx=10,ny=NA)#网格
#重新设置x轴的刻度，原来的刻度是1，2，3,4,5,6，没有显示出月份
axis(side=1,at=c(12,41,72,102,133),labels=c('二月','三月','四月','五月','六月'))
points(date,as.numeric(deadRate)*10,col='red',pch=20)
lines(date,as.numeric(deadRate)*10,col='red',lwd=lwdN)
legend("topright", bg='lightgray',                                 
legend=c("单日死亡率","单日治愈率"),        
col=c("red","blue"),pch=c(20,20),                 
lwd=lwdN,cex=2)
dev.off()
#######################################################################################




###################fig2(a-d)绘制地图热图##################################################
library(chinamap)
library(sp)
library(mapproj)
library(sf)
x = get_nCov2019()
cn = get_map_china()
HD = load_nCov2019()
#可以绘制某一天的全国疫情形势，修改date，颜色可调整
p1=plot(x, region='china',
     chinamap=cn, continuous_scale=T,
     font.size=4,date='2020-06-30',palette='OrRd')

#可以通过参数palette='Blues'，'Reds'修改颜色，默认是红色
#这些所有颜色都来自RColorBrewer packages,通过display.brewer.all()可查看
#library(RColorBrewer),display.brewer.all(),brewer.pal.info
#下面绘制南海诸岛，一般的R map包都没有这个部分，目前没有找到，因此要单独绘制

########################
library("maptools")
library(rgdal)
library(ggplot2)
library(sp)
#fork 九段线
l9<-rgdal::readOGR("D:/BaiduNetdiskDownload/R中国地图模板/SouthSea/九段线.shp")
#data reading
china_map=rgdal::readOGR("D:/BaiduNetdiskDownload/R中国地图模板/china/bou2_4p.shp")
x <- china_map@data #读取行政信息
xs <- data.frame(x,id=seq(0:924)-1) #含岛屿共925个形状
china_map1 <- fortify(china_map) #转化为数据框
library(plyr)
china_map_data <- join(china_map1, xs, type = "full")
 #合并两个数据框
china_data <- join(china_map_data, x, type="full") 
#绘制小图,南海诸岛
p2<-ggplot()+
  geom_polygon(data=china_data,aes(x=long,y=lat,group=group),color="grey40",fill="white")+ #绘制分省图
  geom_line(data=l9,aes(x=long,y=lat,group=group),color="red",size=0.5)+ #9段线
  coord_cartesian(xlim=c(105,125),ylim=c(3,30))+ #缩小显示范围在南部区域
  theme(
    aspect.ratio = 1.25, #调节长宽比
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA,color="grey20",linetype=1,size=0.8),
    plot.margin=unit(c(0,0,0,0),"mm"))
#######################
#将这个部分绘制好后，使用ggplot合并地图
library(grid) #ggplot也是grid图
png(file = "F:/2019-nCoV/figures/fig3a.png", width = 400, height = 350)
vie <- viewport(width=0.15,height=0.10,x=0.72,y=0.33) #定义小图的绘图区域
p1#运行大图
print(p2,vp=vie) #在p1上按上述格式增加小图
dev.off()
####################################################################################



####################fig3(a-d)目前全国总体形势#########################################
#fig3a:腾讯下载最新地图
#fig3b：总体趋势图
x = get_nCov2019()
library(ggplot2)
#png(file = "F:/2019-nCoV/figures/fig3b.png", width = 400, height = 350)
ggplot(summary(x), aes(as.Date(date, "%m.%d"), as.numeric(confirm))) +
  geom_col(fill='gray') + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  labs(caption = paste("accessed date:", "2020-06-30"))
dev.off()
dev.new()
#fig3c:各省份情况1
library(forcats)
library(ggplot2)
d = x[]#x['湖北',]
d$confirm=as.numeric(d$confirm)#数值化
d$name = fct_reorder(d$name, d$confirm)#确诊数和地名映射
#png(file = "F:/2019-nCoV/figures/fig3c.png", width = 400, height = 350)
ggplot(d, aes(name, confirm)) + 
  geom_col(fill='steelblue') + coord_flip() +
  geom_text(aes(y = confirm+2, label=confirm), hjust=0) +
  theme_minimal(base_size=14) + 
  scale_y_continuous(expand=c(0,10)) +
  xlab(NULL) + ylab(NULL)
#dev.off()
#dev.new()
#fig3d：各省份情况2library(ggplot2)
require(ggrepel)
HD = load_nCov2019()
d2 <- summary(HD)[,1:5]
#png(file = "F:/2019-nCoV/figures/fig3d.png", width = 400, height = 350)
ggplot(d2,
       aes(time, as.numeric(cum_confirm), group=province, color=province)) +
  geom_point() + geom_line() +
  geom_text_repel(aes(label=province), data=d2[d2$time == '2020-06-27', ], hjust=1) +
  theme_minimal(base_size = 14) + theme(legend.position='none') +
  xlab(NULL) + ylab(NULL) + scale_y_log10()
#dev.off()
#################################################################################


###################再生数的估计###################################################
#基本再生数R0
#基于earlyR的R0估计，MLE估计
library(incidence)
library(earlyR)
#数据收集2019-12-01----2020-01-24封城前的再生数
inc=incidence(c(1, rep(11,3),rep(15,2),17,18,19,rep(20,5),
             rep(21,4),rep(22,3),rep(23,8),24,rep(25,3),rep(26,2),
             rep(27,2),rep(31,3),rep(32,3),rep(33,1),47,rep(48,17),
            rep(49,59),rep(50,77),rep(51,72),rep(52,105),rep(53,69),
            rep(54,105),rep(55,180)))
#计算不同时间段的R0
as.data.frame(inc)
R0 = get_R(inc,si_mean=7.5,si_sd=3.4);R0$lambda
#代际分布为gamma分布，均值是7.5，标准差是3.4
png(file = "F:/2019-nCoV/figures/fig4a.png", width = 400, height = 350)
par(ps=10,mar=c(2.5,3,0.5, 0.2),mgp=c(1.5,0.5,0))
plot(R0,cex.axis=1.5,cex.lab=2)#画图
#下面用R0包计算R0
dev.off()
#####fig4b#####
days=c("第15天","第20天","第25天","第30天","第50天","第54天")
R=c(1.952,2.222,1.942,1.211,3.333,2.812)
png(file = "F:/2019-nCoV/figures/fig4b.png", width = 400, height = 350)
bar=barplot(R,names.arg=days,
        col=c("gray","gray","gray","gray","gray","red"),
        ylim=c(0,4),ylab='基本再生数R0')
textt=c('1.952','2.222','1.942','1.211','3.333','封城日：2.812')
text(bar, R+0.2, textt, xpd = TRUE, col = "blue",cex=1.5)
dev.off()



library(R0)
GTn = generation.time(type='gamma', val=c(7.5,3.4),step=1)
temp = seq.Date(from = as.Date("2019/12/01",format = "%Y/%m/%d"),
       by = "day", length.out = nrow(inc))
inc_frame=as.data.frame(inc)
R0=estimate.R(inc_frame$counts,t=temp,GT=GTn, methods=c( "ML"))
#methods,ML, TD, SB

###估计瞬时再生数
library(EpiEstim)
x = get_nCov2019()
daily_newadd_data=summary(x,by='today')#2020-01-20
attach(daily_newadd_data)
Rt=estimate_R(incid =confirm,
           method = "parametric_si",
           config = make_config(mean_si = 7.5,std_si = 3.4,
           t_start = 2:(length(confirm)- 14),
           t_end = ( 2+ 14):length(confirm)))
plot(Rt,legend=T)#给出一个总图
png(file = "F:/2019-nCoV/figures/fig5a.png", width = 400, height = 350)
plot(Rt,what='R',legend=F)
dev.off()
#what = c("all", "incid", "R", "SI")
#计算lambad值
png(file = "F:/2019-nCoV/figures/fig5b.png", width = 400, height = 350)
lambda=overall_infectivity(incid =confirm,si_distr=Rt$si_distr)
plot(lambda, type = "s", xlab = "time (days)", ylab = "Infectivity")
grid(nx=10,ny=NA)
dev.off()


####################人员流动性分析##############################################
library(ggplot2)
library(forcats)
Wout_01_23=read.table('F:/2019-nCoV/datas/Wout0123.csv',sep=',',header=T)
paste("D",1:23,sep="")
names(Wout_01_23)[3:25]=paste("D",1:23,sep="")
head(Wout_01_23,n=100L)
Wout_average=read.table('F:/2019-nCoV/datas/Wu_to_province.csv',sep=',',header=T)
#对数值四舍五入4位
Wout_average$average_out=round(Wout_average$average_out,digit=1)
Wout_nohubei=subset(Wout_average,province!='鄂')
Wout_nohubei =Wout_nohubei[order(-Wout_nohubei$average_out),]
#png(file = "F:/2019-nCoV/figures/fig6a.png", width = 400, height = 350)
par(ps=10,mar=c(2.5,3,0.5, 0.2),mgp=c(1.5,0.5,0))
bar=barplot(Wout_nohubei$average_out,names.arg=Wout_nohubei$province,
            horiz=F,cex.names=1.5,col='lightblue',ylim=c(0,6))
box()
grid(nx=20,ny=NA)
text(bar, Wout_nohubei$average+0.12, factor(Wout_nohubei$average), 
xpd = TRUE, col = "blue",cex=1.5)
dev.off()
dev.new()
#考察14天以后及2.6（01-23---02-06）的累积增长病例，提取2月6号的数据
library(nCov2019)
HD = load_nCov2019()
countryHD=summary(HD)
provinceHD=HD['湖北',]
data_0206=provinceHD[282:298,]#0206湖北
Data_0206=countryHD[586:619,]#0206全国
#将全国的数据除湖北之外的迁出率与14天后累积病例合并成数据框
Confirm_0206=subset(Data_0206,province!='湖北')[,c('province','cum_confirm')]
Wout=c(0.455217391,0.172173913,1.831304348,0.94087,1.28,0.279130435,
       0.636086957,0.512173913,0.612173913,0.023043478,5.033043478,
       0.145217391,0.363913043,0,0.100434783,0,0,0.650869565,0.205217,
       1.623478261,0.45173913,0.097391304,2.179565217,1.91173913,
       1.025217391,0.280869565,3.220869565,0,0,0.160869565,1.24348,
       0.005217391,0)
Wout=round(Wout,digit=2)
Confirm_0206[,3]=Wout
names(Confirm_0206)[3]='Ave_Wout'#Confirm_0206
province=c('陕','黑','粤','沪','京','滇','冀','鲁','闽','吉','豫','晋',
           '黔','宁','甘','蒙','台','川','津','皖','桂','新','苏','赣','浙',
           '琼','湘','澳','藏','辽','渝','青','港')
Confirm_0206$province=province
Confirm_0206=Confirm_0206[order(-Confirm_0206$Ave_Wout),]
            
attach(Confirm_0206)
#png(file = "F:/2019-nCoV/figures/fig6a.png", width = 400, height = 350)
par(ps=10,mar=c(2.5,3,0.5, 0.2),mgp=c(1.5,0.5,0))
bar=barplot(cbind(Ave_Wout,cum_confirm/100)~province,data=Confirm_0206,
col=c('red','lightblue'), 
legend = c('武汉迁往省外的平均迁出率（2020-01-01---2020-01-23）',
         '武汉封城14天后的累积确诊病例（2020-02-06）'),ylim=c(0,16),
           cex.names=1.5)
box()
grid(nx=20,ny=NA)
text(bar, Ave_Wout+0.5, factor(Ave_Wout), 
xpd = TRUE, col = "black",cex=1.5)
text(bar, cum_confirm/100+Ave_Wout+0.5, factor(cum_confirm), 
xpd = TRUE, col = "black",cex=1.5)
#dev.off()
detach(Confirm_0206)


#将湖北的数据迁出率与14天后累积病例合并成数据框
confirm_0206=subset(data_0206,city!='武汉')[,c('city','cum_confirm')]
Win_ave=c(3.812608696,1.835652174,2.749130435,3.869130435,4.480869565,
          3.013478261,12.91913043,6.093043478,12.20434783,5.143043478,
          2.93173913,1.843913043,2.713043478,1.135652174,1.839130435,0.038695652)
Win_ave=round(Win_ave,digit=2)
confirm_0206[,3]=Win_ave
names(confirm_0206)[3]='Ave_Win'#confirm_0206
confirm_0206=confirm_0206[order(-confirm_0206$Ave_Win),]
            
attach(confirm_0206)
#png(file = "F:/2019-nCoV/figures/fig6b.png", width = 400, height = 350)
par(ps=10,mar=c(2.5,3,0.5, 0.2),mgp=c(1.5,0.5,0))
bar=barplot(cbind(Ave_Win,cum_confirm/100)~city,data=confirm_0206,
col=c('red','lightblue'), legend = c('武汉迁往省内的平均迁出率（2020-01-01---2020-01-23）',
'武汉封城14天后的累积确诊病例（2020-02-06）'),ylim=c(0,40),cex.names=1.6)
box()
grid(nx=20,ny=NA)
text(bar, Ave_Win+1, factor(Ave_Win), 
xpd = TRUE, col = "black",cex=1.5)
text(bar, cum_confirm/100+Ave_Win+1, factor(cum_confirm), 
xpd = TRUE, col = "black",cex=1.5)
#dev.off()
detach(confirm_0206)
#计算相关性
cor(confirm_0206$Ave_Win,confirm_0206$cum_confirm/100)
cor(Confirm_0206$Ave_Wout,Confirm_0206$cum_confirm/100)

dcor(confirm_0206$Ave_Win,confirm_0206$cum_confirm/100)
dcor(Confirm_0206$Ave_Wout,Confirm_0206$cum_confirm/100)

#评均迁徙数据，重新做图6a及6b，认为下面的信息更能完美体现客观事实。
#将平均迁入及迁出，流动强度，14天后确诊人数合在一起
#武汉省外迁徙情况
wu_ave_outin=read.table('F:/2019-nCoV/datas/wuhan_average_outin1.csv',sep=',',header=T)
wu_ave_outin[,2:4]=round(wu_ave_outin[,2:4],digit=2)
wu_ave_outin=wu_ave_outin[order(-wu_ave_outin$FM),]
attach(wu_ave_outin)
#png(file = "F:/2019-nCoV/figures/fig6a.png", width = 400, height = 350)
par(ps=10,mar=c(2.5,3,0.5, 0.2),mgp=c(1.5,0.5,0))
bar=barplot(cbind(Wu_Out,In_Wu,cum_confirm14/100)~province,data=wu_ave_outin,
col=c('red','lightblue','gray'), 
legend = c('武汉迁出目的地的平均迁出率（2020-01-01---2020-01-23）',
           '迁入武汉来源地的平均迁入率（2020-01-01---2020-01-23）',
           '武汉封城14天后的累积确诊病例（2020-02-06）'),ylim=c(0,25),
           cex.names=1.5)
box()
grid(nx=20,ny=NA)
#text(bar, Wu_Out+2, factor(Wu_Out), 
#xpd = TRUE, col = "black",cex=1.5)
text(bar, cum_confirm14/100+FM+0.5, factor(cum_confirm14), 
xpd = TRUE, col = "blue",cex=1.5)
#dev.off()
detach(wu_ave_outin)

#武汉省内迁徙情况
hubei_city=read.table('F:/2019-nCoV/datas/wuhan_average_outin2.csv',sep=',',header=T)
hubei_city[,2:4]=round(hubei_city[,2:4],digit=2)
hubei_city=hubei_city[order(-hubei_city$cityFM),]
attach(hubei_city)
#png(file = "F:/2019-nCoV/figures/fig6a.png", width = 400, height = 350)
par(ps=10,mar=c(2.5,3,0.5, 0.2),mgp=c(1.5,0.5,0))
bar=barplot(cbind(wu_out_city,city_In_wu,city_cum_confirm14/100)~city,data=hubei_city,
col=c('red','lightblue','gray'), 
legend = c('武汉迁出省内目的地的平均迁出率（2020-01-01---2020-01-23）',
           '迁入武汉省内来源地的平均迁入率（2020-01-01---2020-01-23）',
           '武汉封城14天后的累积确诊病例（2020-02-06）'),ylim=c(0,60),
           cex.names=1.5)
box()
grid(nx=20,ny=NA)
#text(bar, Wu_Out+1, factor(Wu_Out), 
#xpd = TRUE, col = "black",cex=1.5)
text(bar, city_cum_confirm14/100+cityFM+1.5, factor(city_cum_confirm14), 
xpd = TRUE, col = "blue",cex=1.5)
#dev.off()
detach(hubei_city)

#计算相关系数
#省外
library(energy)
cor(wu_ave_outin$Wu_Out,wu_ave_outin$cum_confirm14/100)
cor(wu_ave_outin$In_Wu,wu_ave_outin$cum_confirm14/100)
cor(wu_ave_outin$FM,wu_ave_outin$cum_confirm14/100)

dcor(wu_ave_outin$Wu_Out,wu_ave_outin$cum_confirm14/100)
dcor(wu_ave_outin$In_Wu,wu_ave_outin$cum_confirm14/100)
dcor(wu_ave_outin$FM,wu_ave_outin$cum_confirm14/100)

#省内
cor(hubei_city$wu_out_city,hubei_city$city_cum_confirm14)
cor(hubei_city$city_In_wu,hubei_city$city_cum_confirm14)
cor(hubei_city$cityFM,hubei_city$city_cum_confirm14)
 
dcor(hubei_city$wu_out_city,hubei_city$city_cum_confirm14)
dcor(hubei_city$city_In_wu,hubei_city$city_cum_confirm14)
dcor(hubei_city$cityFM,hubei_city$city_cum_confirm14)

###########################Poisson regression##########################
#全国地级市数据
covid=read.table('F:/2019-nCoV/datas/DATANew.csv',sep=',',header=T)
#factor(province)31个province，factor(city)320个city nrow(covid)
#head(covid)
#标准化数据,用不着标准化建模
#covid_Norm=as.data.frame(scale(as.matrix(covid[,10:length(covid)])))
#head(covid_Norm)
attach(covid)
#ks.test(newconfirm,'ppois', lambda=1.5) 
#首先对天气数据建模
covidweather=glm(newconfirm~minT+maxT+H+windv+windc+P+
                 visibility+rain
                ,family = poisson(), data = covid)
summary(covidweather)

#过度离势检验
library(qcc)
qcc.overdispersion.test(newconfirm, type = "poisson")
#拟poisson回归
quasi_covidweather=glm(newconfirm~minT+maxT+H+windv+windc+P+
                 visibility+rain
                ,family = quasipoisson(), data = covid)
summary(quasi_covidweather)
#step_p <- step(quasi_covidweather,direction="both")#逐步变量选择

#剔除不相关变量建模
quasi_covidweather1=glm(newconfirm~maxT+minT+P+
                 visibility
                ,family = quasipoisson(), data = covid)
summary(quasi_covidweather1)

#约简模型检验
library(lmtest)
waldtest(quasi_covidweather1, quasi_covidweather,test='Chisq')

#融合模型
quasi_covidinte=glm(newconfirm~cum_confirm+cum_heal+cum_dead+
                                 minT+maxT+H+windv+windc+P+
                                visibility+rain+wu_out+in_wu+
                                longtitude+latitude
                ,family = quasipoisson(), data = covid)
summary(quasi_covidinte)

#约简模型
quasi_covidinte1=glm(newconfirm~cum_confirm+#cum_dead+
                                H+windv+P+
                                visibility+rain+wu_out+in_wu+
                                latitude
                ,family = quasipoisson(), data = covid)
summary(quasi_covidinte1)
waldtest(quasi_covidinte1, quasi_covidinte,test='Chisq')
############################################END#####################################






       
































