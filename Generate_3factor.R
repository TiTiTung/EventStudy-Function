x <- c("tidyverse","xts","highcharter","TTR","quantmod","eventstudies","devtools")
install.packages(x)
# install eventstudies for (R 3.5.0)
devtools::install_github("nipfpmf/eventstudies", ref="v1.2")


{ library(xts)
  library(tidyverse)
  library(highcharter)
  require(TTR)
  library(eventstudies)
  library(magrittr)}


#============================================
#     事件期、估計期設定
#============================================

# 估計期長度 (使用者自訂)
esday <- 100
# 事件期長度 (使用者自訂)
evday <- 10


#==========================================================================
#     資料讀取、資料讀取
#==========================================================================
# 讀取所有股市資料
sret <- read.csv(file="Total_stockdata_Biotechnology.csv",header=TRUE) %>% 
  spread(name,adjclose) 

# 轉換為xts
sret <- xts(sret[,-1], as.Date(sret$when)) %>% 
  log() %>% 
  diff() %>% 
  .[-1,]
  
  #==========================================================================
  # 讀取所有size資料
  size <- read.csv(file="Total_size_Biotechnology.csv",header=TRUE) %>%
    transform(size = as.numeric(size)) %>% 
    spread(name,size)
  
  size <- xts(size[,-1], as.Date(size$when))
  #**************************************************************************
  # 讀取所有size資料
  book_m <- read.csv(file="Total_book_market_Biotechnology.csv",header=TRUE) %>%
    transform(book_market = as.numeric(book_market))%>% 
    spread(name,book_market)
  
  book_m <- xts(book_m[,-1], as.Date(book_m$when)) %>% 
    log()
  #==========================================================================
      
# 讀取事件資料，注意檔案路徑或格式
# 轉換資料格式為 dataframe
edate <- read.csv("Event__Hao Ding.csv",header=TRUE) %>%
  transform(name = as.character(name), when = as.Date(when)) %>% 
  na.omit()
      
edate %<>% filter(name != "1799" & name != "4157")

edate %<>% filter(name != "4107" & name != "4174" & name != "4911")

# 把發生事件的股票名稱改成"大盤"名稱 <- mdate
mdate <- edate
mdate$name <- "Mindex"

#==========================================================================
#     跑事件研究法
#==========================================================================
# 轉換一般股價資料變為事件資料，mk為市場報酬率資料
es <- phys2eventtime(z=sret,events=edate,width=(esday+evday))

# es$outcomes包含成功與失敗的事件，此處為了篩選成功的事件
edate <- edate[es$outcomes=="success",]
# 大盤必須經過經過篩選成功事件後，才跑事件研究法函數
mdate <- mdate[es$outcomes=="success",]

# 轉換大盤股價資料變為事件資料
mk <- phys2eventtime(z=sret,events=mdate)

#抓取需要的window
es.w <- window(es$z.e,start=-(esday+evday), end=evday)
mk.w <- window(mk$z.e,start=-(esday+evday), end=evday)

  #==========================================================================
  stock_size <- phys2eventtime(z=size,events=edate)
  size.w <- window(stock_size$z.e,start=-(esday+evday), end=evday)
  #**************************************************************************
  stock_book_m <- phys2eventtime(z=book_m,events=edate,width=(esday+evday))
  book_m.w <- window(stock_book_m$z.e,start=-(esday+evday), end=evday)
  #==========================================================================
        
#==========================================================================
#     計算異常報酬率 AR
#==========================================================================
#先建立一個空AR
AR <- matrix(0, evday*2+1, ncol(es.w))

for (i in seq_along(es.w[1,])){
  # 為了估計"貝塔"，需讀取每一檔股票與大盤的估計期(1:esday)，來進行估計
  y = es.w[1:esday,i]
  x = mk.w[1:esday,i]
  z = size.w[1:esday,i]
  k = book_m.w[1:esday,i]
  # 跑回歸
  #fit1 <- lm(y~x)
  fit1 <- lm(y~x)
  coef1 = coef(fit1)
  temp = coredata(
                es.w[(esday+1):(esday+2*evday+1),i] -
                   (
                     mk.w[(esday+1):(esday+2*evday+1),i]*coef1["x"]+
                     size.w[(esday+1):(esday+2*evday+1),i]*coef1["z"]+
                     book_m.w[(esday+1):(esday+2*evday+1),i]*coef1["k"]+
                    coef1[1]
                   )
               )
  
  AR[,i] = as.numeric(temp)
}

#==========================================================================
#     計算 AAR
#==========================================================================
# "行"是事件日，"列"是股票
# 每一行的rowMeans，就是AAR
AAR <- rowMeans(AR)

#==========================================================================
#     計算 CAAR
#==========================================================================
# 需要利用迴圈，算出每一行(事件日)的累計報異常酬率CAR
CAR <- rbind({},AR[1,])
        
for (i in 2:(2*evday+1)){ CAR=rbind(CAR, CAR[(i-1),] + AR[i,]) }
# 上面所算出來的是CAR，每一檔股票的累計報酬率CAR
# 再將每季報酬率平均(rowMeans)，就是"累計平均異常報酬率"CAAR
CAAR <- rowMeans(CAR)


#==========================================================================
#==========================================================================
#    繪圖
#==========================================================================
# par(mfrow=c(1,2)) 
# png("AAR.png", bg="transparent", width=800, height=600)
plot(AAR, col=1, type="l", xaxt="n",  xlab="window", main="Average Abnormal Returns")
axis(1, at=seq(1,(2*evday+1),by=10), label=seq(-evday,evday,by=10))
# dev.off()
plot((CAAR), col=1, type="l", xaxt="n", xlab="window", main="多因子")
axis(1, at=seq(1,(2*evday+1)), label=seq(-evday,evday))
#==========================================================================
#   highchart 繪圖
#==========================================================================
hc <- highchart() %>% 
  hc_title(text = "Three Factor Model") %>% 
  hc_subtitle(text = "Cumulative Average Abnormal Returns") %>% 
  hc_add_series(ken$ax,name="ax") %>% 
  hc_add_series(ken$az,name="az") %>% 
  hc_add_series(ken$ak,name="ak") %>% 
  hc_add_series(ken$xz,name="xz") %>% 
  hc_add_series(ken$xk,name="xk") %>% 
  hc_add_series(ken$zk,name="zk") %>% 
  hc_add_series(ken$xzk,name="xzk") %>% 
  hc_add_theme(hc_theme_google())

hc

hc <- highchart() %>% 
  hc_xAxis(categories = ken$X)%>% 
  hc_add_series(a*100,name="一般化酖因子",type = "line" ) %>% 
  hc_add_series(s*100,name="調整後單因子",type = "line" ) %>% 
  hc_add_series(rm*100,name="rm",type = "line" ) %>% 
  hc_add_series(rs*100,name="rs",type = "line" ) %>% 
  hc_add_series(CAAR*100,name="調整後單因子!!!!!!!!!",type = "line" ) %>% 
  hc_add_theme(hc_theme_google())

hc 
ken$x=-10:10
ken <- read.csv("ken.csv",header=TRUE)

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
x=as.data.frame(size.w)
y=as.data.frame(es.w)
z=as.data.frame(book_m.w)
edate2 <- edate[stock_book_m$outcomes=="success",]
edate$name[!edate$name[!is.na(edate$name)]%in%edate2$name[!is.na(edate2$name)]]
#==========================================================================



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