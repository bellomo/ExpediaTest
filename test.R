library(lattice)
library(gdata)
library(RColorBrewer)
#################################################################################
# configuration flags
#################################################################################

# Set to 1(0) to load(compute) the partner similarity matrices
loadSim = 1

# Set to 1(0) to write(not-write) the JSON file
writeJSON = 0

#################################################################################
# load the data
#################################################################################
data = read.csv("Expedia/expedia_casestudy_20170127.csv", header = TRUE, sep = ",", dec = ".")

# list of partners
affiliate_id_list = unique(data$affiliate_id)

# total number of partners
num_of_affiliates = length(affiliate_id_list)

### TEST num_of_affiliates = 10

# list of markets
mkt_list = unique(data$mkt)

# total number of markets
num_of_mkts = length(mkt_list)

#################################################################################
# Top 10 markers per partner
#################################################################################
# total booking value per partner and per market
totbkgs_per_id_per_mkt = aggregate(data$bkgs, by=list(affiliate_id=data$affiliate_id,mkt=data$mkt),FUN=sum)

# number of markets per partner (used later)
num_of_mkt_per_partner = aggregate(totbkgs_per_id_per_mkt$mkt, by=list(affiliate_id=totbkgs_per_id_per_mkt$affiliate_id), FUN=length)

#################################################################################
# Search for similar partners
#################################################################################

if (loadSim==1) {

# load the derived data frames
load("Expedia/affiliates_similarity_matrix.Rda")
load("Expedia/affiliates_shared_markets_matrix.Rda")

}else{

# A similarity score is assigned to each pair of partners.  
# The score is defined as the number of common markets with similar booking value distributions
# divided by the total number of unique markters among the pair of partners.
# The similarity between booking value distributions is defined based on the 
# p-value from the Mann-Whitney-Wilcoxon test evaluated at 95% CL

# define a dataframe to store the similary score
affiliates_sim <- data.frame("affiliate_id1" = integer(), "affiliate_id2" = integer(), "score" = numeric())
affiliates_shamkt <- data.frame("affiliate_id1" = integer(), "affiliate_id2" = integer(), "score" = numeric())

# split data per partern
data_split = split(data, f = data$affiliate_id)

# loop over partners
cat("Computing similarity matrix ...\n")
for (i in 1:num_of_affiliates) {
  cat("Analysing data for partner ",i," ...\n")
  data_split_i = data_split[[i]]
  data_i = data_split_i[,2:3] 
  mkt_i = split(data_i, f = data_i$mkt, drop = TRUE)

  # loop over other partners
  for (j in i:num_of_affiliates) {
    data_split_j = data_split[[j]]
    data_j = data_split_j[,2:3] 
    mkt_j = split(data_j, f = data_j$mkt, drop = TRUE)

    # define the score for current partner
    score = 0
    # compute the list of common markets among the two partners
    mkts = intersect(data_i[,1], data_j[,1])

    if( length(mkts) > 0 ) {      
      for(m in 1:length(mkts)) {
        bkg_split_i = mkt_i[[mkts[m]]]
        bkg_split_j = mkt_j[[mkts[m]]]
        bkg_i = bkg_split_i[,2:2]
        bkg_j = bkg_split_j[,2:2]
      
        # neglects markets with less than 5 bookings
        if (length(bkg_i) > 5 & length(bkg_j) > 5) {
          Wtest = wilcox.test(bkg_i,bkg_j)
          if (Wtest$p.value > 0.05) {
            score = score + 1
          }
        }      
      } # end of loop over markets
    } # if common markets exist
    # store score in the data frame
    affiliates_sim[nrow(affiliates_sim)+1,] <- c(i, j, score)
    affiliates_shamkt[nrow(affiliates_shamkt)+1,] <- c(i, j, length((mkts)))    
  } # end of loop over other partners
} # end of loop over partners

save(affiliates_sim,file="Expedia/affiliates_similarity_matrix.Rda")
save(affiliates_shamkt,file="Expedia/affiliates_shared_markets_matrix.Rda")
 
}

#################################################################################
# build matrices for partner vs partner comparisons 
#################################################################################

# fraction of shared markets
shaMktMatrix = matrix(data=0, nrow = num_of_affiliates, ncol=num_of_affiliates)
# fraction of markets with similar booking value distributions among pair of partners 
simShaMatrix = matrix(data=0, nrow = num_of_affiliates, ncol=num_of_affiliates)
# fraction of markets with similar booking value distributions among pair of partners 
# this is the quantity used to define when two partners are similar.
simMatrix = matrix(data=0, nrow = num_of_affiliates, ncol=num_of_affiliates)
for( i in 1:num_of_affiliates) {
  num_of_mkt_i = num_of_mkt_per_partner[num_of_mkt_per_partner$affiliate_id==affiliate_id_list[i],2:2]
  for( j in i:num_of_affiliates) {
    num_of_mkt_j = num_of_mkt_per_partner[num_of_mkt_per_partner$affiliate_id==affiliate_id_list[j],2:2]
    num_of_shared_mkt_i_j = affiliates_shamkt[affiliates_shamkt$affiliate_id1==i & affiliates_shamkt$affiliate_id2==j,3:3]    
    num_of_similar_mkt_i_j = affiliates_sim[affiliates_sim$affiliate_id1==i & affiliates_sim$affiliate_id2==j,3:3]
    num_of_unique_mkt_i_j = num_of_mkt_i + num_of_mkt_j - num_of_shared_mkt_i_j
    
    shaMktMatrix[i,j] = num_of_shared_mkt_i_j / num_of_unique_mkt_i_j
    simShaMatrix[i,j] = num_of_similar_mkt_i_j / num_of_shared_mkt_i_j
    simMatrix[i,j]    = num_of_similar_mkt_i_j / num_of_unique_mkt_i_j
  }
}

# symmetrise the matrices
lowerTriangle(shaMktMatrix) <- upperTriangle(shaMktMatrix, byrow=TRUE)    
lowerTriangle(simShaMatrix) <- upperTriangle(simShaMatrix, byrow=TRUE)    
lowerTriangle(simMatrix) <- upperTriangle(simMatrix, byrow=TRUE)    

# data frame for final similarity score
affiliates_simfinal <- data.frame("affiliate_id1" = integer(), "affiliate_id2" = integer(), "score" = numeric())
for( i in 1:num_of_affiliates) {
  for( j in 1:num_of_affiliates) {
    affiliates_simfinal[nrow(affiliates_simfinal)+1,] <- c(i, j, simMatrix[i,j])
  }
}    

#################################################################################
# Write the JSON file
#################################################################################

if( writeJSON==1) {
library(jsonlite)
sink("Expedia/myjson.json")
djson = data.frame(factor(), data.frame(), list())
for (i in 1:num_of_affiliates) {
  # compute the top 10 markets
  mkt_i = totbkgs_per_id_per_mkt[totbkgs_per_id_per_mkt$affiliate_id == affiliate_id_list[i], 2:3]
  mkt_i = mkt_i[order(mkt_i$x, decreasing = TRUE),]
  mkt_i_top10 = head(mkt_i, n=10)

  # compute the top 10 most similar partners
  tmp = affiliates_simfinal[affiliates_simfinal$affiliate_id1==i,2:3]
  affiliate_i_top10 = (head(tmp[order(tmp$score,decreasing = TRUE),1:1],n=11))[2:11]
  
  # build a data frame to hold the info
  djson_i <- data.frame(partner_name = c(as.character(affiliate_id_list[i])), row.names = i)
  tmp <- data.frame(as.list(mkt_i_top10$x))
  tmp <- setNames(tmp, as.character(mkt_i_top10$mkt))
  djson_i$top10markets <- list(tmp)
  djson_i$similar_partners <- list(as.character(affiliate_id_list[affiliate_i_top10]))
  
  # append to final dataframe
  djson = rbind(djson,djson_i)
}
cat(toJSON(djson, pretty=TRUE))
sink()
}

#################################################################################
# save plots and tables for similarity matrices
#################################################################################

rgb.palette <- colorRampPalette(brewer.pal(9, "Blues"), space = "rgb")

# matrices for similarity among partners
pdf("Expedia/plots/shared_market_matrix.pdf")
print(levelplot(shaMktMatrix, main="shared market matrix", xlab="partner id", ylab="partner id", 
          col.regions=rgb.palette(120), cuts=num_of_affiliates))
dev.off()

# matrices for similarity among partners
pdf("Expedia/plots/shared_similarity_matrix.pdf")
print(levelplot(simShaMatrix, main="shared similarity matrix", xlab="partner id", ylab="partner id", 
          col.regions=rgb.palette(120), cuts=num_of_affiliates))
dev.off()

# matrices for similarity among partners
pdf("Expedia/plots/global_similarity_matrix.pdf")
print(levelplot(simMatrix, main="global similarity matrix", xlab="partner id", ylab="partner id", 
          col.regions=rgb.palette(120), cuts=num_of_affiliates))
dev.off()



