dat <- read.csv("Parkinsson disease.csv")
# Renaming the columns for ease of 
colnames(dat) <- c("name","avg_fre", "max_fre", "min_fre", "var_fre1", "var_fre2", 
                   "var_fre3", "var_fre4", "var_fre5", "var_amp1", "var_amp2", 
                   "var_amp3", "var_amp4", "var_amp5", "var_amp6", "NHR", "HNR", 
                   "status", "RPDE", "DFA", "spread1", "spread2", "D2", "PPE")

dim(dat) # 195  X 24: 195 obs of 24 variables
head(dat, 3)
# Checking for missing values
any(is.na(dat)) # FALSE
# Finding numeric and non numeric columns
sapply(dat, is.numeric) # only name col is FALSE
# Information of the dataset
str(dat) # Status col is binary of 0 and 1 (hence integer) 
# The rest col's are all numeric except name col which is character type
# Convert name column into categorical type
dat$name <- as.factor(dat$name)
# Statistical Summary
summary(dat[-1])
# Duplicate Entries
sum(duplicated(dat[-1])) # sum is 0

# Histogram of features except status and name column
dat.hist <- dat[,-c(1,18)]
par(mfrow=c(2, 11))  # Adjust the rows and columns as needed
for (col in colnames( dat.hist )) {
  hist(dat.hist[,col], main = paste("Histogram of", col), xlab = col, 
       col = "lightblue", border = "black")
}
par(mfrow=c(1, 1))

# Correlated columns
library(corrplot)
cor_matrix <- cor(dat.hist)
corrplot(cor_matrix, method = "circle", type = "full", 
         order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45, 
         diag = FALSE)
# Highly positively correlated columns
# criteria : abs(cor_matrix) >0.9
# (spread1, PPE), (var_amp5,var_amp1,var_amp3,var_amp6,var_amp2), (var_freq2, var_freq4, var_freq1, var_freq3, var_freq5) )
# (NHR,var_freq1, var_freq3, var_freq5)