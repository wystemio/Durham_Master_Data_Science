wholesale = read.csv('~/Documents/DDS/DS/DVUL/Assignment2/wholesale.csv')
View(wholesale)
summary(wholesale)
boxplot(wholesale)

sapply(wholesale, function(x) sum(is.na(x)))


### PCA
library(psych)
pairs.panels(wholesale,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

sd <- apply(wholesale, 2, sd)
sd

S <- cor(wholesale)
eigdec <- eigen(S)
eigdec

eig <- eigdec$values
eig
sum(eig)

pc_loading <- eigdec$vectors
rownames(pc_loading) <- colnames(wholesale)
pc_loading

# Variances in percentage
eig <- eigdec$values
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig2 <- data.frame(eig = eig, variance = variance,
                   cumvariance = cumvar)
eig2 

barplot(eig2[, 2], names.arg=1:nrow(eig2),
        main = "Scree plot",
        xlab = "Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig2), eig2[, 2],
      type="b", pch=19, col = "red")

pc_score <- as.matrix(scale(wholesale))%*% pc_loading
colnames(pc_score) <- paste0("PC", 1:6)
pc_score[1:6,]

library(psych)
pairs.panels(pc_score,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

cor(wholesale, pc_score[,1:4])
t(t(pc_loading)*sqrt(eig))

cos2 <- (cor(wholesale, pc_score[,1:4]))^2
cos2

comp.cos2 <- apply(cos2, 2, sum)
comp.cos2 # same as the corresponding eigenvalues

contrib2 <- function(cos2, comp.cos2){cos2*100/comp.cos2}
contrib <- t(apply(cos2,1, contrib2, comp.cos2))
contrib

#######################################################################

names1 = c("Grocery", "Detergents_Paper","Milk", "Delicassen","Frozen","Fresh")
barplot(contrib[order(contrib[, 1],decreasing = T), 1], names.arg=names1, 
        main = "Contribution of variables to PC1",
        xlab = " ",
        ylab = "Percentage of variances",
        col ="steelblue",las=2,cex.names=0.7)
abline(h=25, col="red",lty=3, lwd =1)


barplot(contrib[order(contrib[, 1],decreasing = T), 2], names.arg=names1, 
        main = "Contribution of variables to PC2",
        xlab = " ",
        ylab = "Percentage of variances",
        col ="steelblue",las=2,cex.names=0.7)
abline(h=25, col="red",lty=3, lwd =1)


barplot(contrib[order(contrib[, 1],decreasing = T), 3], names.arg=names1, 
        main = "Contribution of variables to PC3",
        xlab = " ",
        ylab = "Percentage of variances",
        col ="steelblue",las=2,cex.names=0.7)
abline(h=25, col="red",lty=3, lwd =1)

barplot(contrib[order(contrib[, 1],decreasing = T), 4], names.arg=names1, 
        main = "Contribution of variables to PC4",
        xlab = " ",
        ylab = "Percentage of variances",
        col ="steelblue",las=2,cex.names=0.7)
abline(h=25, col="red",lty=3, lwd =1)

##############################################################
ws.scaled <- scale(wholesale)

# K-medoids

#K-medoids

fviz_nbclust(ws.scaled, clara, method = "silhouette")+
  labs(title = "K-medoids")

library(cluster)
# k=2
pam.res <- clara(ws.scaled, k=2)
names(pam.res)

pam.res$medoids
pam.res$i.med
fviz_cluster(pam.res, wholesale, ellipse.type = "norm")

#k=3
pam.res <- clara(ws.scaled, k=3)
names(pam.res)

pam.res$medoids
pam.res$i.med
fviz_cluster(pam.res, wholesale, ellipse.type = "norm")

###################

# Hierarchical clustering
# Compute distances and hierarchical clustering
# k = 4
dd <- dist(ws.scaled, method = "euclidean")
hc <- hclust(dd, method = "complete")
hc

dendros <- as.dendrogram(hc)
plot(dendros, main = "Wholesale data - Complete linkage",
     ylab = "Height")
abline(h=0.2, lty = 2, col="red")
abline(h=0.428, lty = 2, col="blue")#point 116 and 142 are merged
Hs <- hc$height[(length(hc$height)-4):length(hc$height)]
abline(h=Hs, col=3, lty=2)


fviz_cluster(list(data=ws.scaled, cluster=cutree(hc, 4)), ellipse.type = "norm")

fviz_dend(hc, k = 4, # Cut in three groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800","#FF4500"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray() # Change theme
)

hcut <- cutree(hc, k = 4)
table(hcut)

# install.packages("igraph")
library(igraph)
fviz_dend(hc, k = 4, k_colors = "jco", type = "phylogenic", repel = TRUE)
