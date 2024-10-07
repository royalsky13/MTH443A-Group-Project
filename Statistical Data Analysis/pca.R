setwd("~/Desktop/Parkinson")
library(scatterplot3d)
dat <- read.csv("parkinson.csv")
df <- dat[, -c(1,18)]
for (j in 1:dim(df)[2])
{
  if(is.numeric(df[ ,j]) == FALSE) df[ ,j] <- as.numeric(df[ ,j])
}
std.df <- scale(df)
covariancematrix <- cov(std.df)
eigenvaluesvector <- eigen(covariancematrix)
sortindice <- order(eigenvaluesvector$values, decreasing = TRUE)
sorteigenvalues <- eigenvaluesvector$values[sortindice]
sorteigenvector <- eigenvaluesvector$vectors[ ,sortindice]
par(mfrow=c(1,1), mar = c(5, 4, 2, 1))
# First plot: Eigenvalues
plot(eigenvaluesvector$values, type = "b", xlab = "Number of Principal Components", 
     ylab = "Eigenvalue", pch=16, col="darkblue", lwd=2, cex.lab =1.2)
abline(v = 6, col = "red", lty=2, lwd=2)

# Second plot: Variance Explained
plot(eigenvaluesvector$values / sum(eigenvaluesvector$values), type = "b", 
     xlab = "Number of Principal Components", ylab = "Proportion of Variance", 
     pch=16, col="darkgreen", lwd=2, cex.lab =1.2)
abline(v = 6, col = "red", lty=2, lwd=2)

# Third plot: Cumulative Variance Explained
plot(cumsum(eigenvaluesvector$values) / sum(eigenvaluesvector$values), type = "b", 
     xlab = "Number of Principal Components", ylab = "Cumulative Variance", 
     pch=16, col="darkorange", lwd=2, cex.lab = 1.2)
abline(v = 6, col = "red", lty=2, lwd=2)
# From the plot, 6 PCs explains 91.6% of total variability.
n.pcomponent <- 6
selectedcomponents <-sorteigenvector[ ,1:n.pcomponent]

# Reduced data (Dimensionality Reduction)
reduceddata<- std.df %*% selectedcomponents

# Importance of Components
pca_result <- prcomp(df, scale. = TRUE)
summary(pca_result)

# Projection into visualisable plane
tags.reduceddata <- cbind(dat[18], reduceddata[,1],reduceddata[,2], reduceddata[,3] )
colnames(tags.reduceddata ) <- c("Status", "PC1", "PC2", "PC3")
colors <- ifelse(dat[,18] == 0, "blue", "red")
s3d <- scatterplot3d(tags.reduceddata$PC1,tags.reduceddata$PC2, tags.reduceddata$PC3,
                     xlab = "PC1", ylab = "PC2", zlab = "PC3",
                     color = colors,
                     pch = 16, angle=60)
#45, 135, 225, and 315
legend("topleft", 
       legend = c("Healthy", "Parkinson"),  # Labels for legend
       col = c("blue", "red"),  # Colors corresponding to the labels
       pch = 16,  # Shape of the points in the legend
       title = "Status", cex=1.2)  # Title for the legend

