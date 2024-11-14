setwd("~/Desktop/Parkinson")
library(scatterplot3d)
library(reshape2)
library(aplpack)
dat <- read.csv("parkinson.csv")
df <- dat[, -c(1,18)]
for (j in 1:dim(df)[2])
{
  if(is.numeric(df[ ,j]) == FALSE) df[ ,j] <- as.numeric(df[ ,j])
}
std.df <- scale(df, center = TRUE, scale = TRUE)
covariancematrix <- cov(std.df)
eigenvaluesvector <- eigen(covariancematrix)
sortindice <- order(eigenvaluesvector$values, decreasing = TRUE)
sorteigenvalues <- eigenvaluesvector$values[sortindice]
sorteigenvector <- eigenvaluesvector$vectors[ ,sortindice]
par(mfrow=c(1,1), mar = c(5, 4, 2, 1))

# First plot: Eigenvalues
plot(eigenvaluesvector$values, type = "b", xlab = "Number of Principal Components", 
     ylab = "Eigenvalue", pch=16, col="darkblue", lwd=2, cex.lab =1.2)
abline(v = 8, col = "red", lty=2, lwd=2)

# Second plot: Cumulative Variance Explained
plot(cumsum(eigenvaluesvector$values) / sum(eigenvaluesvector$values), type = "b", 
     xlab = "Number of Principal Components", ylab = "Cumulative Variance", 
     pch=16, col="darkorange", lwd=2, cex.lab = 1.2)
abline(v = 8, col = "red", lty=2, lwd=2)

# From the plot, 8 PCs explains 95.76% of total variability.
n.pcomponent <- 8
selectedcomponents <-sorteigenvector[ ,1:n.pcomponent]

# Reduced data (Dimensionality Reduction)
reduceddata <- std.df %*% selectedcomponents # selected eigenvector

# Importance of Components
pca_result <- prcomp(df, scale. = TRUE)
summary(pca_result)

# Projection into visualisable plane
tags.reduceddata <- cbind(dat[18], reduceddata[,1], reduceddata[,2], reduceddata[,3] )
colnames(tags.reduceddata ) <- c("Status", "PC1", "PC2", "PC3")
colors <- ifelse(dat[,18] == 0, "blue", "red")
s3d <- scatterplot3d(tags.reduceddata$PC1,tags.reduceddata$PC2, tags.reduceddata$PC3,
                     xlab = "PC1", ylab = "PC2", zlab = "PC3",
                     color = colors,
                     pch = 16, angle=60)
# 45, 135, 225, and 315
legend("topleft", 
       legend = c("Healthy", "Parkinson"),  # Labels for legend
       col = c("blue", "red"),  
       pch = 16,  
       title = "Status", cex=1.2)  

# Outlier Detection 
## 2-D Plane
par(mfrow = c(1,1))
par(mar =  c(5.1, 4.1, 4.1, 2.1))
plot(x = reduceddata[,1], y = reduceddata[,2], xlab = "PC1", ylab = "PC2", 
     col="blue", pch = 16, cex.lab = 1)
tags <- dat$name
outlier <- c('phon_R01_S35_4', 'phon_R01_S24_4', 'phon_R01_S24_6', 
             'phon_R01_S35_6', 'phon_R01_S35_7')
outlier_indices <- which(tags %in% outlier)
text(x = reduceddata[outlier_indices, 1] + 0.5, 
     y = reduceddata[outlier_indices, 2], 
     labels = tags[outlier_indices], 
     pos = 3, cex = 0.6, col = "red")

## Chernoff faces
faces(reduceddata, face.type = 1,
      labels = dat$name, cex = 0.6)

# Outliers removed after detection
outlier <- c('phon_R01_S35_4', 'phon_R01_S24_4', 'phon_R01_S24_6', 
             'phon_R01_S35_6', 'phon_R01_S35_7')
df.outliers.removed <- dat[!dat$name %in% outlier, ]
write.csv(df.outliers.removed,"parkinson_no_outlier.csv", row.names = FALSE)








