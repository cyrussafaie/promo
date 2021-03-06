#Cyrus Safaie#Promo effectiveness

version
Sys.info()
sessionInfo() 
dir()

##############################
##############################
#data reading 
##############################
##############################




# export from NEtezza DB, SQL script-nf20160824

# total item level info for regular customers (those who purchase pre and post and during promo)
nf7=read.csv("regular_customers.csv")
str(nf7)
names(nf7)
dim(nf7)

#col index
relevent.index=c(2,6,29:62)
nf.7.var=nf7[,relevent.index]
names(nf.7.var)
dim(nf.7.var)

# qty index
qty.index=1:12
length(qty.index)

# count index
cnt.index=13:24
length(cnt.index)

# 
#qty per count
#qty.cnt.index=24:30
#length(qty.cnt.index)


nf.7.var.t=t(nf.7.var[,-c(1,2)])
colnames(nf.7.var.t)=nf.7.var[,2]
#colnames(nf.7.var.t)=strtrim(nf.7.var[,2],15)
var.names=rownames(nf.7.var.t)
nf.7.var.t

###################################################################
###################################################################
#splitting the data for qty, cnt and qty per cust
###################################################################
#################################       ##################################

qty.data=nf.7.var.t[qty.index,]
cnt.data=nf.7.var.t[cnt.index,]
#qty.per.cnt.data=nf.7.var.t[qty.cnt.index,]


# basic trend view fro volume and customr change 
plot.ts(cbind(qty.data[,1:5],cnt.data[,1:5]), main="Top 1-5 items in NF7 - qty on left and customer count on right (pre6 to post3)",cex.lab=0.4,type='b')
plot.ts(cbind(qty.data[,6:10],cnt.data[,6:10]), main="Top 5-10 items in NF7 - qty on left and customer count on right (pre6 to post3)",cex.lab=0.4,type='b')

# top 10 qty per cust
plot.ts(qty.per.cnt.data[,1:10], main="Top 10 items in NF7 - qty per cust (pre6 to post3)",cex.lab=0.4)

###################################################################
###################################################################
#Function for extracting baseline for customer
###################################################################
###################################################################

baseline_qty=function(dat)
{
        trimean=vector("numeric",dim(dat)[2])
        
        for (i in 1:dim(dat)[2]){
                
                qtyPre1to4=quantile(dat[3:6,i]) 
                trimean[i]=as.vector((qtyPre1to4[2]+2*qtyPre1to4[3]+qtyPre1to4[4])/4)
        }   
        
        ma2.result=vector("numeric",dim(dat)[2])
        
        for (i in 1:dim(dat)[2]){
                
                require(forecast)
                ma2.mod=forecast::ma(dat[1:6,i],order=2)
                ma2=forecast(ma2.mod,h=1)$mean[1]
                ma2.result[i]=ma2
                
        }
        
        baseline_amount=rowMeans(cbind(trimean,ma2.result,pre1=dat[6,],pre2=dat[5,]))
        promo.vs.baseline=cbind(baseline=round(baseline_amount,0)
                                
                                ,pre6=round(dat[1,],0)
                                ,pre5=round(dat[2,],0)
                                ,pre4=round(dat[3,],0)
                                ,pre3=round(dat[4,],0)
                                ,pre2=round(dat[5,],0)
                                ,pre1=round(dat[6,],0)
                                ,promo=round(dat[7,],0)
                                
                                ,promo_lift=round(dat[7,]-baseline_amount,0)
                                ,promo_lift_BP=round(10000*(dat[7,]/baseline_amount-1),0)
                                
                                ,post1=round(dat[8,],0) 
                                ,post2=round(dat[9,],0)
                                ,post3=round(dat[10,],0)
                                ,post4=round(dat[11,],0)
                                ,post5=round(dat[12,],0)
                                
                                ,post1_lift=round(dat[8,]-baseline_amount,0) 
                                ,post2_lift=round(dat[9,]-baseline_amount,0)
                                ,post3_lift=round(dat[10,]-baseline_amount,0)
                                ,post4_lift=round(dat[11,]-baseline_amount,0)
                                ,post5_lift=round(dat[12,]-baseline_amount,0)
                                
                                ,lift_PromoPlusPost5=round(dat[7,]+dat[8,]+dat[9,]+dat[10,]+dat[11,]+dat[12,]-6*baseline_amount,0)
                                ,lift_PromoPlusPost5_BP=round(10000*(dat[7,]+dat[8,]+dat[9,]+dat[10,]+dat[11,]+dat[12,]-6*baseline_amount)/baseline_amount) )
        
        promo.vs.baseline=as.matrix(promo.vs.baseline)
        
        return(promo.vs.baseline)
        suppressWarnings()
}

suppressWarnings(baseline_qty(qty.data))
write.csv(suppressWarnings(baseline_qty(qty.data)),"RegCust_baseline_cases_20160928.csv")
saveRDS(qty.data, "RegCust_baseline_cases_20160928.rds")
###################################################################
###################################################################
#Function for extracting baseline for customer
###################################################################
###################################################################


#dim(baseline_qty(qty.data))



#suppressWarnings(baseline_qty(cnt.data))

#library(plyr)
#colwise(fun)(d)
#?plot.ts




