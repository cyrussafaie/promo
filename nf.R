nf=read.csv("cust_details_pre.csv")
str(nf)
#install.packages("ggplot2")
#library(ggplot2)
names(nf)
nf.qty=t(nf[,c(13,30:56,11,57:83)])
colnames(nf.qty)=nf[,6]


# qty index
qty.index=1:28

# count index
cnt.index=29:56

qtycont.div.index=56:84

#unique(nf$pim_prod_desc_lng)
#dim(nf.qty)

#names(nf.qty[,1])

# scales for testing
plot(scale(nf.qty[qty.index,1]),type="b", main ="CHICKEN, BREAST DOUBLE-LOBE RANDOM JUMBO BONELESS-SKINLESS RAW REF CVP")
lines(scale(nf.qty[cnt.index,1]), type = "l", col=3)

# per cust qty calculated
new.rows=nf.qty[qty.index,]/nf.qty[cnt.index,]
row.names(new.rows)=c('qty_per_cust','qty_per_cust_pre1' 
                      ,'qty_per_cust_pre2 ' 
                      ,'qty_per_cust_pre3 ' 
                      ,'qty_per_cust_pre4 ' 
                      ,'qty_per_cust_pre5 ' 
                      ,'qty_per_cust_pre6 ' 
                      ,'qty_per_cust_pre7 ' 
                      ,'qty_per_cust_pre8 ' 
                      ,'qty_per_cust_pre9 ' 
                      ,'qty_per_cust_pre10' 
                      ,'qty_per_cust_pre11 ' 
                      ,'qty_per_cust_pre12 ' 
                      ,'qty_per_cust_pre13 ' 
                      ,'qty_per_cust_pre14 ' 
                      ,'qty_per_cust_pre15 ' 
                      ,'qty_per_cust_pre16 ' 
                      ,'qty_per_cust_pre17 ' 
                      ,'qty_per_cust_pre18 ' 
                      ,'qty_per_cust_pre19 ' 
                      ,'qty_per_cust_pre20 ' 
                      ,'qty_per_cust_pre21 ' 
                      ,'qty_per_cust_pre22 ' 
                      ,'qty_per_cust_pre23 ' 
                      ,'qty_per_cust_pre24 ' 
                      ,'qty_per_cust_pre25 ' 
                      ,'qty_per_cust_pre26 ' 
                      ,'qty_per_cust_pre27 ' 
)

for (i in qty.index) {
        combo=cbind(nf.qty[qty.index,i]
                    ,nf.qty[cnt.index,i]
                    ,new.rows[,i])
        print(i)
        print(head(combo))
        
}

# adding vase per customer to the data
nf.all=rbind(nf.qty, new.rows)
dim(nf.all)


plot(nf.qty[qty.index,1],type="b", main ="CHICKEN, BREAST DOUBLE-LOBE RANDOM JUMBO BONELESS-SKINLESS RAW REF CVP")
plot(nf.qty[,2],type="b", main ="TOMATO, WHOLE IN JUICE PEELED CANNED")
plot(nf.qty[,3],type="b", main ="SUGAR, WHITE EXTRA FINE CANE")
plot(nf.qty[,4],type="b", main ="ONION, YELLOW JUMBO 3+ BAG FRESH REF")
plot(nf.qty[,5],type="b", main ="BUTTER, SOLID GRADE AA UNSALTED REF")
plot(nf.qty[,6],type="b", main ="BUN, HAMBURGER 4 SLICED TFF BAKED FROZEN")
plot(nf.qty[,7],type="b", main ="POTATO, FRENCH-FRY SWEET 3/8 STRAIGHT-CUT SKIN-ON FROZEN HOUSE")
plot(nf.qty[,8],type="b", main ="BUTTER, SOLID GRADE AA SALTED REF")             
plot(nf.qty[,9],type="b", main ="BUN, HOT DOG 6 SLICED TFF BAKED FROZEN")
plot(nf.qty[,10],type="b", main = "CHEESE, AMERICAN SLICED 160 COUNT TFF PROCESSED YELLOW REF")             


plot(scale(nf.qty[cnt.index,1]), type = "l", col=3)
lines(scale(nf.qty[cnt.index,2]), type = "l", col=4)

row.names(nf.all)

matplot(y=scale(nf.all[qty.index,1:10]),type = "l")

matplot(nf.all[cnt.index,],type = "l")      
matplot(scale(nf.all[qtycont.div.index,]),type = "l")            

#setInternet2(TRUE) # only for R versions older than 3.3.0
#installr::updateR() # updating R.

sessionInfo() 
version

dim(nf.qty)

#matplot(nf[,2],type='l')

x=nf.all[qty.index,1]    
x=ts(x)
x1=ts(arrange(as.data.frame(x[2:7]), -row_number()))
plot(x1)
#cx <- cumsum(x)
class(x1)
x

#install.packages("forecast")
library(forecast)

ets.model.qty=ets(x1)
predict(ets.model.qty,5)
summary(ets.model.qty)


arima.qty=auto.arima(x1)
plot(forecast(arima.qty,5))

ss=forecast::ma(x1,order = 4)
accuracy(ss,x1[2:27])

moving_average = forecast(ss, 5)

plot(x1)



data(wineind)
plot(wineind)
sm <-forecast::ma(wineind,order = 12)

sm3 <-forecast::ma(wineind,order = 9)
lines(sm3,col="green")
lines(sm,col=4)


mas <- function(arr, n=15){
        res = arr
        for(i in n:length(arr)){
                res[i] = mean(arr[(i-n):i])
        }
        res
}

mas(x1,4)



prd=forecast::ma(x1,order=2)
forecast(prd,h=1)

cbind(x1,mas(x1,3),forecast::ma(x1,order=3),SMA(x1))

mean(x1[24:27])

library(TTR)
SMA(x1)

(x1[1]*1+x1[2]*2+x1[3]*3+x1[4]*4)/10

library(genasis)
genwhisker(as.vector(1:10))
?genwhisker
# simple exponential - models level
fit <- HoltWinters(x1, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit2 <- HoltWinters(x1, gamma=FALSE)

library(dplyr)
x=arrange(as.data.frame(x), -row_number())
ts.x=ts(x1)

names(quantile(x1))
qr.x1=quantile(x1)

###################################################################
###################################################################
#Function for extracting qty baseline
###################################################################
###################################################################

qty.baseline=function(Pre1to4,qtyPre1to6,Pre1)
{
        require(forecast)
        qtyPre1to4=quantile(Pre1to4) 
        trimean=(qtyPre1to4[1]+2*qtyPre1to4[2]+qtyPre1to4[3])/4
        ma2.mod=forecast::ma(maPre1to6,order=2)
        ma2=forecast(ma2.mod,h=1)$mean
        baseline=mean(trimean,ma2,Pre1)
        return(baseline)
}





library()


??nzr




rownames(x)
# predictive accuracy
library(forecast)
accuracy(fit)

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))



