############################################################# ICLUDE LIBARY #############################################################
require(imputeTS)
require(Ckmeans.1d.dp)
require(zoo)
############################################################# LOAD INPUT #############################################################
df <- read.table("W289953_Q11_RD_StackWAL.txt",fill = TRUE,header = TRUE,sep = ",")
############################################################# PREPARE INPUT #############################################################
df$Ch <- factor(df$Ch,levels=unique(df$Ch))

df$WAL_SHOffset_filtered <- df$WAL_SHOffset
df$WAL_SHOffset_filtered[df$WAL_SHOffset>=300|df$WAL_SHOffset<=-200]=NA

df$R_filtered <- df$Wedge.Reader
df$R_filtered[df$WAL_SHOffset>=300|df$WAL_SHOffset<=-200]=NA

BarNo <- unique(df$RowID)

############################################################# Parameters Setting #############################################################
nSeg = c(6)
MA.size = 9

############################################################# Pre Allocate result table(rdf) #############################################################
rdf <- data.frame()

############################################################# LOOPING BAR #############################################################
for(j in 1:length(BarNo)){
#-------------------------------------------------------------------------------------------------------------------------------------  
  ######################################################### PREPARE INPUT
  ## Selected Current RowID
  selIdx <- df$RowID==BarNo[j]
  sdf <- df[selIdx,]
  
  ## Selected Columns Rdt[R,Ch] (as.data.frame == as table? )
  R <- as.data.frame(split(sdf$R_filtered,sdf$Ch))
  Offset <- as.data.frame(split(sdf$WAL_SHOffset_filtered,sdf$Ch))
  ######################################################### LOOPING CH
  ## Loop segmentation using k-means for each channels 
  for(i in 1:length(unique(sdf$Ch))){
#-------------------------------------------------------------------------------------------------------------------------------------  
#-------------------------------------------------------------------------------------------------------------------------------------  
  
   if(all(is.na(Offset[,i]))){next()} # if nodata continue next ch
    
   off <- na.locf(Offset[,i], option = "locf", na.remaining = "rev")
   off.diff <- c(diff(off)[1],diff(off))
   off.diff.ma <- rollmean(off.diff,k=MA.size,fill=NA,align = "left")
   var.in <- na.locf(off.diff.ma, option = "locf", na.remaining = "rev")
   
   # if(all(is.na(R[,i]))){next()}
   # r <- na.locf(R[,i], option = "locf", na.remaining = "rev")
   # r.diff <- c(diff(r)[1],diff(r))
   # r.diff.ma <- rollmean(r.diff,k=MA.size,fill=NA,align = "left")
   # var.in <- na.locf(r.diff.ma, option = "locf", na.remaining = "rev")
   # var.in <- r
   
   if(sum((var.in-max(var.in))^2)==0){next()}
    res <- Cksegs.1d.dp(var.in,k=nSeg,method="quadratic")
    ## plot
    # par(mfrow=c(1,2))
    # plot(res , xlab="index",main=sprintf("Bar = %d, Ch = %d, Seg = %d",BarNo[j],i,length(res$centers)))
    # plot(c(1:length(y.diff)),y,col=res$cluster)
    tdf <- data.frame(RowID=rep(BarNo[j],
                      length(res$cluster)),
                      Ch=rep(i,length(res$cluster)),
                      Idx=1:length(res$cluster),
                      Seg=res$cluster,
                      Offset=off,
                      R=r,
                      Diff=r.diff,
                      Diff.MA=r.diff.ma)
    rdf <- rbind(rdf,tdf)
    
#-------------------------------------------------------------------------------------------------------------------------------------  
#-------------------------------------------------------------------------------------------------------------------------------------  
  }

#-------------------------------------------------------------------------------------------------------------------------------------  
}

############################################################# SAVE RESULT #############################################################
write.csv(rdf,file=sprintf("resultsR_nSeg%d_MA%d.csv",max(nSeg),MA.size),row.names = FALSE)


############################################################# BACK UP SYNTAX #############################################################

# par(mfrow=c(1,2))
# plot(y.diff,ylim = c(range(y.diff)))
# plot(,ylim = c(range(y.diff)))


# ## sample data
# y <- 1:900
# y[1:300] = 0
# y[301:350]= c(301:350)*(200-0)/(350-301)-1229
# y[351:600]=200
# y[601:650]=c(601:650)*(200-100)/(601-650)+1427
# y[651:900]=100
# 
# par(mfrow=c(1,2))
# plot(y,col='blue')
# plot(diff(y),col='red')
# 
# res <- Cksegs.1d.dp(y,k=5,method="quadratic")
# y.diff <- c(diff(y)[1],diff(y))
# res.diff <- Cksegs.1d.dp(y.diff ,k=5,method="quadratic")
# 
# par(mfrow=c(1,2))
# plot(res , xlab="index",main=sprintf("Normal Seg = %d",length(res$centers)))
# plot(res.diff, xlab="index" ,main=sprintf("Diff Seg = %d",length(res.diff$centers)))
# 
# par(mfrow=c(1,2))
# plot(res.diff, xlab="index" ,main=sprintf("Diff Seg = %d",length(res.diff$centers)))
# plot(c(1:900),y,col=res.diff$cluster)

