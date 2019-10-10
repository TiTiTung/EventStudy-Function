x <- c("tidyverse","xts","highcharter","TTR","quantmod","eventstudies","devtools")
install.packages(x)
# install eventstudies for (R 3.5.0)
devtools::install_github("nipfpmf/eventstudies", ref="v1.2")


{ library(xts)
  library(tidyverse)
  library(highcharter)
  require(TTR)
  library(eventstudies)}


#============================================
#     事件期、估計期設定
#============================================

# 估計期長度 (使用者自訂)
esday=50
# 事件期長度 (使用者自訂)
evday=51


#==========================================================================
#     資料讀取、資料讀取
#==========================================================================
# 讀取所有股市資料
sret = read.csv(file="Total_stockdata.csv",header=TRUE)

# 讓欄位名稱變成股票代碼 => 符合phys2eventtime()的要求
sret <- spread(sret,name,adjclose)

# 處理NA值, 若na，以之後第一筆非NA的數值補上
# 注意：每檔股票前後仍可能有NA值
# sret <- na.locf(sret) 不能用
# 轉換為xts
sret <- xts(sret[,-1], as.Date(sret$when))
# 已經先算出報酬率了
sret=diff(log(sret))
sret=sret[-1,]

# 讀取事件資料，注意檔案路徑或格式
# 轉換資料格式為 dataframe
edate <- edate <- read.csv("DIDI7.csv",header=TRUE) %>%
  transform(name = as.character(name), when = as.Date(when)) %>% 
  na.omit()
# 把發生事件的股票名稱改成"大盤"名稱 <- mdate
mdate=edate
mdate$name="Mindex"

#==========================================================================
#     跑事件研究法
#==========================================================================
# 轉換一般股價資料變為事件資料，mk為市場報酬率資料
es=phys2eventtime(z=sret,events=edate,width=(esday+evday))

# es$outcomes包含成功與失敗的事件，此處為了篩選成功的事件
edate <- edate[es$outcomes=="success",]
# 大盤必須經過經過篩選成功事件後，才跑事件研究法函數
mdate <- mdate[es$outcomes=="success",]
    
# 轉換大盤股價資料變為事件資料
mk=phys2eventtime(z=sret,events=mdate)

#抓取需要的window
es.w=window(es$z.e,start=-(esday+evday), end=evday)
mk.w=window(mk$z.e,start=-(esday+evday), end=evday)

#==========================================================================
#     計算異常報酬率 AR
#==========================================================================
AR <- {}

for (i in 1:ncol(es.w)){
  # 為了估計"貝塔"，需讀取每一檔股票與大盤的估計期(1:esday)，來進行估計
  y=es.w[1:esday,i]
  x=mk.w[1:esday,i]
  # 跑單回歸
  fit1=lm(y~x)
  coef1=coef(fit1)
  temp=coredata(es.w[(esday+1):(esday+2*evday+1),i]-(mk.w[(esday+1):(esday+2*evday+1),i]*coef1[2]+coef1[1]))
  # Market Return
  #temp=coredata(es.w[(esday+1):(esday+2*evday+1),i]-mk.w[seq((esday+1),(esday+2*evday+1),i),i])
  AR=cbind(AR, as.numeric(temp))
}

#==========================================================================
#     計算 AAR
#==========================================================================
# "行"是事件日，"列"是股票
# 每一行的rowMeans，就是AAR
AAR=rowMeans(AR)

#==========================================================================
#     計算 CAAR
#==========================================================================
# 需要利用迴圈，算出每一行(事件日)的累計報異常酬率CAR
CAR <- rbind({},AR[1,])

for (i in 2:(2*evday+1)){
    CAR=rbind(CAR, CAR[(i-1),] + AR[i,])
  }

# 上面所算出來的是CAR，每一檔股票的累計報酬率CAR
# 再將每季報酬率平均(rowMeans)，就是"累計平均異常報酬率"CAAR
CAAR=rowMeans(CAR)

#====================================================================================================
#==========================================================================
#    繪圖
#==========================================================================
# par(mfrow=c(1,2)) 
# png("AAR.png", bg="transparent", width=800, height=600)
plot(AAR, col=1, type="l", xaxt="n",  xlab="window", main="Average Abnormal Returns")
axis(1, at=seq(1,(2*evday+1),by=10), label=seq(-evday,evday,by=10))
# dev.off()
plot((CAAR), col=1, type="l", xaxt="n", xlab="window", main="Cumulative Average Abnormal Returns")
axis(1, at=seq(1,(2*evday+1)), label=seq(-evday,evday))





#==========================================================================
#    存檔程序
#==========================================================================
# 把剛跑完的事件研究法存成RDS檔
# 如果跑很多各事件研究法，先存成多個檔案比較不會搞混
saveRDS(es,"short_stock.rds")
saveRDS(mk,"short_mindex.rds")

es <- readRDS("short_stock.rds")
mk <- readRDS("short_mindex.rds")

#==========================================================================
#    Excel畫圖方便，用spread把一行CAAR，改成一列
#==========================================================================
CAAR_save <- as.data.frame(AAR)
CAAR_save$z <- 1:length(as.vector(AAR))
CAAR_save <- spread(CAAR_save,z,AAR)

write.csv(CAAR_save,"CAAR_save.csv")

#==========================================================================
#    存取AR
#==========================================================================
AR_save <- as.data.frame(AR)
# 將AR的colume's name，改成那檔股票的代碼
colnames(AR_save) <- edate$name

write.csv(AR_save,"AR_save.csv")







#==========================================================================
#     相關檢定
#==========================================================================
# 普通橫剖面法
VAAR=rowSums((AR-t(matrix(AAR,nrow=ncol(AR),ncol=nrow(AR),byrow = TRUE)))^2)/(ncol(AR)*(ncol(AR)-1))
t=AAR/sqrt(VAAR)

VCAAR=rowSums((CAR-t(matrix(CAAR,nrow=ncol(AR),ncol=nrow(AR),byrow = TRUE)))^2)/(ncol(AR)*(ncol(AR)-1))
# 另一種寫法
# VCCAR=var(t(CAR))/ncol(AR)
ct=CAAR/sqrt(VCAAR)
#==========================================================================
#     相關檢定
#==========================================================================