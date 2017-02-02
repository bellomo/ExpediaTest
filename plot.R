library(lattice)
library(gdata)
library(RColorBrewer)
library(jsonlite)
library(beanplot)
library(gplots)

#################################################################################
# load raw data
#################################################################################
data = read.csv("Expedia/expedia_casestudy_20170127.csv", 
                   header = TRUE, sep = ",", dec = ".")

#################################################################################
# load data from table
#################################################################################
jdata = fromJSON("Expedia/myjson.json")

#################################################################################
# all transactions plot
#################################################################################
pdf("Expedia/plots/All_transactions.pdf")
hist(data$bkgs,col = "gray", xlab = "booking value", 
     main = "All transactions", breaks = 100,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
dev.off()

#################################################################################
# mean booking value per market
#################################################################################
bkgs_mean = aggregate(data$bkgs, by=list(market=data$mkt),FUN=mean)
bkgs_mean = bkgs_mean[order(bkgs_mean$x,decreasing = TRUE),]
pdf("Expedia/plots/mean_bkg_value_markets.pdf")
plot(bkgs_mean[,2:2],xlab = "Markets", ylab = "Mean booking value",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
dev.off()
d = head(bkgs_mean, n=10)
rownames(d) <- c(1:(dim(d)[1]))
colnames(d) <- c("Market", "Average bkg value")
nbgks = c()
for (m in d$Market) {
  nbgks = c(nbgks, length(data[data$mkt==m,3:3]))
}
d$`Number of bkgs` <- nbgks
d[,-1] <- round(d[,-1],digits = 2)
pdf("Expedia/plots/table_markets_tails.pdf")
textplot(d)
dev.off()

#################################################################################
# mean booking value per partner
#################################################################################
affiliate_mean = aggregate(data$bkgs, by=list(affiliate_id=data$affiliate_id),FUN=mean)
pdf("Expedia/plots/mean_bkg_value_partners.pdf")
plot(affiliate_mean[,2:2],xlab = "Partners", ylab = "Mean booking value",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
dev.off()

#################################################################################
# total booking value per market (% of total booking value)
#################################################################################
totbkgs_per_mkt = aggregate(data$bkgs, by=list(mkt=data$mkt),FUN=sum)
totbkgs_per_mkt <- totbkgs_per_mkt[order(totbkgs_per_mkt$x, decreasing = TRUE),]
listTop10Markets = (head(totbkgs_per_mkt, n=10))[,1:1]

# first print the table with top markets per mean 
d = bkgs_mean[bkgs_mean$market %in% listTop10Markets,]
rownames(d) <- c(1:(dim(d)[1]))
colnames(d) <- c("Market", "Average bkg value")
nbgks = c()
for (m in d$Market) {
  nbgks = c(nbgks, length(data[data$mkt==m,3:3]))
}
d$`Number of bkgs` <- nbgks
d[,-1] <- round(d[,-1],digits = 2)
pdf("Expedia/plots/table_markets_top10n.pdf")
textplot(d)
dev.off()

# then per volume in % 
totbkgs_per_mkt[,2:2] = 100*totbkgs_per_mkt[,2:2]/sum(totbkgs_per_mkt[,2:2])
cat("Top 10 markets (% of booking value) \n")
print(head(totbkgs_per_mkt, n=10))
d = head(totbkgs_per_mkt, n=10)
rownames(d) <- c(1:(dim(d)[1]))
colnames(d) <- c("Market", "Bkg value [%]")
d[,-1] <- round(d[,-1],digits = 2)
pdf("Expedia/plots/table_markets_top10.pdf")
textplot(d)
dev.off()

#################################################################################
# total booking value per partner (% of total booking value)
#################################################################################
totbkgs_per_affiliate = aggregate(data$bkgs, by=list(affiliate_id=data$affiliate_id),FUN=sum)
totbkgs_per_affiliate <- totbkgs_per_affiliate[order(totbkgs_per_affiliate$x, decreasing = TRUE),]
totbkgs_per_affiliate[,2:2] = 100*totbkgs_per_affiliate[,2:2]/sum(totbkgs_per_affiliate[,2:2])
cat("\nTop 10 parterns (% of booking value) \n")
print(head(totbkgs_per_affiliate, n=10))
d = head(totbkgs_per_affiliate, n=10)
rownames(d) <- c(1:(dim(d)[1]))
colnames(d) <- c("Partner", "Bkg value [%]")
d[,-1] <- round(d[,-1],digits = 2)
pdf("Expedia/plots/table_partners_top10.pdf")
textplot(d)
dev.off()

#################################################################################
# Compare similar partners for top10 ones
#################################################################################
listTop10Partners = (head(totbkgs_per_affiliate, n=10))[,1:1]
for(p in listTop10Partners) {
  jp = jdata[jdata$partner_name==p,]
  pdf(paste("Expedia/plots/beanplot_top",which(p==listTop10Partners),"partner.pdf",sep=""), width = 14)
  beanplot(data[data$affiliate_id==jp$partner_name & data$mkt==names(jp$top10markets[[1]])[1],3:3],
           data[data$affiliate_id==jp$similar_partners[[1]][1] & data$mkt==names(jp$top10markets[[1]])[1],3:3],
           data[data$affiliate_id==jp$similar_partners[[1]][2] & data$mkt==names(jp$top10markets[[1]])[1],3:3],
           data[data$affiliate_id==jp$similar_partners[[1]][3] & data$mkt==names(jp$top10markets[[1]])[1],3:3],
           data[data$affiliate_id==jp$similar_partners[[1]][4] & data$mkt==names(jp$top10markets[[1]])[1],3:3],
           main = paste("Top ",which(p==listTop10Partners)," partner: booking value distribution across similar partners on top market",sep=""),
           xlab = "Partners", ylab = "Booking value",     
           col = c("#CAB2D6", "#33A02C", "#B2DF8A"),
           innerborder = "black",
           log="", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5,
           ylim = c(-0.1, 1.2),
           names = c(paste(jp$partner_name," (top",which(p==listTop10Partners),")",sep=""),
                     jp$similar_partners[[1]][1],
                     jp$similar_partners[[1]][2],
                     jp$similar_partners[[1]][3],
                     jp$similar_partners[[1]][4]))
  dev.off()
}

##########################################################################################
# look for top10 partners on top10 markets by booking value and check who performs better
##########################################################################################

expbkgs_per_mkt_per_affiliate = data.frame("mkt"=character(),"affiliate_id"=character(),"mean"=numeric(),"sigma"=numeric(),stringsAsFactors=FALSE)
totbkgs_per_mkt_per_affiliate = aggregate(data$bkgs, by=list(mkt=data$mkt,affiliate_id=data$affiliate_id),FUN=sum)
for(m in listTop10Markets) {
  topP = totbkgs_per_mkt_per_affiliate[totbkgs_per_mkt_per_affiliate$mkt==m,2:3]
  topP = topP[order(topP$x,decreasing = TRUE),]
  listTop10Partners = (head(topP,n=10))$affiliate_id
  pdf(paste("Expedia/plots/beanplot_top",which(m==listTop10Markets),"_market_top5partners.pdf",sep=""), width = 14)
  beanplot(data[data$affiliate_id==listTop10Partners[1] & data$mkt==m,3:3],
           data[data$affiliate_id==listTop10Partners[2] & data$mkt==m,3:3],
           data[data$affiliate_id==listTop10Partners[3] & data$mkt==m,3:3],
           data[data$affiliate_id==listTop10Partners[4] & data$mkt==m,3:3],
           data[data$affiliate_id==listTop10Partners[5] & data$mkt==m,3:3],
           main = paste("Top ",which(m==listTop10Markets)," market: booking value distribution for top 5 partners",sep=""),
           xlab = "Partners", ylab = "Booking value",     
           col = c("#CAB2D6", "#33A02C", "#B2DF8A"),
           log="", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5,
           ylim = c(-0.1, 1.2),
           innerborder = "black",
           names = c(as.character(listTop10Partners[1]),
                     as.character(listTop10Partners[2]),
                     as.character(listTop10Partners[3]),
                     as.character(listTop10Partners[4]),
                     as.character(listTop10Partners[5])))
  dev.off()
  
  # compute the expected booking value for 1000 rooms averaging on 1000 bootstrap samples
  listTop10Partners = topP$affiliate_id
  #cat("market : ",m,"\n")
  for(p in listTop10Partners) {
    x = data[data$affiliate_id==p & data$mkt==m,3:3]
    if(length(x)>50) {
      #cat("    partner : ",p," nBKG=",length(x),"\n")
      d = density(x)
      sums = c()
      for(j in 1:1000) {
        newx <- sample(x, 1000, replace=TRUE) + rnorm(1000, 0, d$bw)
        #newx <- sample(x, length(x), replace=TRUE) + rnorm(length(x), 0, d$bw)
        sums = c(sums, sum(newx))        
      }
      #cat("    average exp bkg for 1000 rooms = ",mean(sums)," +- ",sd(sums),"\n")
      mymean = mean(sums)#*1000/length(x)
      mysd = sd(sums)#*1000/length(x)
      expbkgs_per_mkt_per_affiliate[nrow(expbkgs_per_mkt_per_affiliate)+1,] = c(as.character(m), as.character(p), mymean, mysd)
    }
  }
}


##########################################################################################
# Order per expected booking rates for 1000 rooms
##########################################################################################
expbkgs_per_mkt_per_affiliate$lval = (as.numeric(expbkgs_per_mkt_per_affiliate$mean) - 2*as.numeric(expbkgs_per_mkt_per_affiliate$sigma))
expbkgs_per_mkt_per_affiliate$rval = (as.numeric(expbkgs_per_mkt_per_affiliate$mean) + 2*as.numeric(expbkgs_per_mkt_per_affiliate$sigma))
listTop10Markets = (head(totbkgs_per_mkt, n=10))[,1:1]
for(m in listTop10Markets) {
  topP = expbkgs_per_mkt_per_affiliate[expbkgs_per_mkt_per_affiliate$mkt==m,2:6]
  topP = topP[order(topP$mean,decreasing = TRUE),]
  listTop10Partners = (head(topP,n=10))$affiliate_id
  pdf(paste("Expedia/plots/beanplot_top",which(m==listTop10Markets),"_market_top5partners_exp.pdf",sep=""), width = 14)
  beanplot(data[data$affiliate_id==listTop10Partners[1] & data$mkt==m,3:3],
           data[data$affiliate_id==listTop10Partners[2] & data$mkt==m,3:3],
           data[data$affiliate_id==listTop10Partners[3] & data$mkt==m,3:3],
           data[data$affiliate_id==listTop10Partners[4] & data$mkt==m,3:3],
           data[data$affiliate_id==listTop10Partners[5] & data$mkt==m,3:3],
           main = paste("Top ",which(m==listTop10Markets)," market: booking value distribution for top 5 partners",sep=""),
           xlab = "Partners", ylab = "Booking value",     
           col = c("#CAB2D6", "#33A02C", "#B2DF8A"),
           log="", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5,
           ylim = c(-0.1, 1.2),
           innerborder = "black",
           names = c(as.character(listTop10Partners[1]),
                     as.character(listTop10Partners[2]),
                     as.character(listTop10Partners[3]),
                     as.character(listTop10Partners[4]),
                     as.character(listTop10Partners[5])))
  dev.off()
  
  rownames(topP) <- c(1:(dim(topP)[1]))
  colnames(topP) <- c("Partner", "Mean", "Sigma", "Low Bkg", "High Bkg")
  topPP = topP[,c("Partner","Low Bkg","High Bkg")]
  topPP[,-1] <- round(topPP[,-1],digits = 1)
  pdf(paste("Expedia/plots/table_partners_exp_top",which(m==listTop10Markets),"market.pdf",sep=""))
  textplot(head(topPP,n=10))
  dev.off()
  
}




########
listTop10Markets = (head(totbkgs_per_mkt, n=10))[,1:1]
for(m in listTop10Markets) {
  topP = totbkgs_per_mkt_per_affiliate[totbkgs_per_mkt_per_affiliate$mkt==m,2:3]
  topP = topP[order(topP$x,decreasing = TRUE),]
  listTop10Partners = (head(topP,n=10))$affiliate_id
  topP = expbkgs_per_mkt_per_affiliate[expbkgs_per_mkt_per_affiliate$mkt==m,2:6]
  topP = topP[topP$affiliate_id %in% listTop10Partners,]
  rownames(topP) <- c(1:(dim(topP)[1]))
  colnames(topP) <- c("Partner", "Mean", "Sigma", "Low Bkg", "High Bkg")
  topPP = topP[,c("Partner","Low Bkg","High Bkg")]
  topPP[,-1] <- round(topPP[,-1],digits = 1)
  pdf(paste("Expedia/plots/table_partners_tot_top",which(m==listTop10Markets),"market.pdf",sep=""))
  textplot(head(topPP,n=10))
  dev.off()
}




##########################################################################################
# Plots booking value distribiutions for all partners in top 1st market
##########################################################################################
#m = listTop10Markets[1]
#topP = totbkgs_per_mkt_per_affiliate[totbkgs_per_mkt_per_affiliate$mkt==m,2:3]
#listTop10Partners = topP$affiliate_id
#for(p in listTop10Partners) {
#  bkg = data[data$affiliate_id==p & data$mkt==m,3:3]
#  if(length(bkg)>50) {
#    pdf(paste("Expedia/plots/beanplot_top",which(m==listTop10Markets),"_market_partner_",p,".pdf",sep=""), width = 7)
#    beanplot(bkg,
#             ylab = "Booking value",     
#             col = c("#CAB2D6", "#33A02C", "#B2DF8A"),
#             log="", 
#             cex.lab=1.5, cex.axis=1.5, cex.main=1.5,
#             ylim = c(-0.1, 1.2),
#             innerborder = "black",
#             names = c(as.character(p)))
#    dev.off()
#  }
#}
