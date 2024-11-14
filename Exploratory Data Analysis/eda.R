setwd("~/Desktop/Parkinson")
library(ggplot2)
library(reshape2)
dat <- read.csv("Parkinsson disease.csv")
# Renaming the columns for ease of 
colnames(dat) <- c("name","avg_fre", "max_fre", "min_fre", "var_fre1", "var_fre2", 
                   "var_fre3", "var_fre4", "var_fre5", "var_amp1", "var_amp2", 
                   "var_amp3", "var_amp4", "var_amp5", "var_amp6", "NHR", "HNR", 
                   "status", "RPDE", "DFA", "spread1", "spread2", "D2", "PPE")
# Pie Chart
class.proportion <- c(mean(dat$status) * 100, 100 - (mean(dat$status) * 100))
labels <- c("Parkinson", "Healthy")
# Custom colors
colors <- c("lightblue", "lightgreen")
# Creating labels with percentages
labels_with_percent <- paste(labels, round(class.proportion, 1), "%")
# Plotting pie chart with the updated labels
pie(class.proportion, labels = labels_with_percent, col = colors)
legend("topright", labels_with_percent,
       fill = colors)

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


# Highly positively correlated columns
# criteria : abs(cor_matrix) >0.9
# (spread1, PPE), (var_amp5,var_amp1,var_amp3,var_amp6,var_amp2), (var_freq2, var_freq4, var_freq1, var_freq3, var_freq5) )
# (NHR,var_freq1, var_freq3, var_freq5)
cor_matrix <- round(cor(dat[,-c(1,18)]), 1)

# Melt the correlation matrix into long format for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Heatmap
ggplot(data = melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3) +
  labs(x = "Features", y = "Features")

df_long <- melt(dat[, -1], id.vars = "status")  # Exclude the 'name' column

# Boxplot
ggplot(df_long, aes(x = as.factor(status), y = value, fill = as.factor(status))) + 
  geom_boxplot() + 
  facet_wrap(~ variable, scales = "free", ncol = 4) +  # Create a 5x5 grid
  theme_minimal() + 
  labs(x = "Status", y = "Value", fill = "Status") +  # Adding fill legend title
  theme(axis.text.x = element_text(angle = 0, hjust = 1), 
        strip.text = element_text(size = 12),  # Increase facet label size if needed
        legend.position = "bottom", 
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines
scale_fill_brewer(palette = "Set2")  # Change color palette as needed

# Pairplot
dat$status <- as.factor(dat$status)
pairs(dat[, c('var_fre1', 'var_fre2', 'var_fre3', 'var_fre4', 'var_fre5')],
      col = dat$status,  # Color by 'status'
      pch = 19)


write.csv(dat,"parkinson.csv", row.names = FALSE)