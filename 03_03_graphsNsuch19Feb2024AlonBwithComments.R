# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(pacman, party, rio, tidyverse)
# pacman: for loading/unloading packages
# party: for decision trees
# rio: for importing data
# tidyverse: for so many reasons

# Define the file path to the Excel file
file_path <- "data/PersonalityBig5_Frank.xlsx"

# Load the readxl library for reading Excel files
library(readxl)

# Read the Excel file into a dataframe
dfBig5 <- read_excel(file_path)

# Display the column names of the dataframe
colnames(dfBig5)

# Convert Variable1 and Variable2 to numeric type
dfBig5$Variable1 <- as.numeric(as.character(dfBig5$Variable1))
dfBig5$Variable2 <- as.numeric(as.character(dfBig5$Variable2))

# Display the column names of the dataframe
colnames(dfBig5)

# Replace 'CorrectVariable1' and 'CorrectVariable2' with the actual column names from your dataframe
dfBig5$CorrectVariable1 <- as.numeric(as.character(dfBig5$CorrectVariable1))
dfBig5$CorrectVariable2 <- as.numeric(as.character(dfBig5$CorrectVariable2))

# Convert "Extrav" and "Neurot" columns to numeric if they are not already
dfBig5$Extrav <- as.numeric(as.character(dfBig5$Extrav))
dfBig5$Neurot <- as.numeric(as.character(dfBig5$Neurot))

# Calculate Pearson correlation coefficient between Extraversion and Neuroticism
pearson_cor <- cor(dfBig5$Extrav, dfBig5$Neurot, method = "pearson")
print(pearson_cor)

# Load the ggplot2 library for data visualization
library(ggplot2)

# Calculate Pearson correlation coefficient again for later use
pearson_cor <- cor(dfBig5$Extrav, dfBig5$Neurot, method = "pearson")

# Create a scatter plot of Extraversion vs. Neuroticism with a linear regression line
plot <- ggplot(dfBig5, aes(x = Extrav, y = Neurot)) +
  geom_point(alpha = 0.5) +  # Add points with some transparency
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line without the standard error
  theme_minimal() +  # Use a minimal theme for the plot
  labs(title = "Scatter plot of Extraversion vs. Neuroticism",
       x = "Extraversion",
       y = "Neuroticism") +
  annotate("text", x = Inf, y = Inf, label = paste("Pearson r:", round(pearson_cor, 2)), 
           hjust = 1.1, vjust = 2, size = 5, color = "red")  # Annotate plot with Pearson correlation

# Display the scatter plot
print(plot)

# Calculate Spearman's Rank Correlation Coefficient
#pearson_cor <- cor(dfBig5$Extrav (name of the variable), dfBig5$Neurot (name of the variable), method = "pearson")
#print(pearson_cor)
spearman_cor <- cor(dfBig5$Extrav, dfBig5$Neurot, method = "spearman")

# Create a scatter plot of Extraversion vs. Neuroticism with Spearman's correlation coefficient annotated
plot_spearman <- ggplot(dfBig5, aes(x = Extrav, y = Neurot)) +
  geom_point(alpha = 0.5, color = "darkgreen") +  # Adjust point color for visual distinction
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +  # Linear model fit for reference
  theme_minimal() +  # Minimal theme
  labs(title = "Scatter Plot of Extraversion vs. Neuroticism (Spearman's Rank Correlation)",
       x = "Extraversion",
       y = "Neuroticism") +
  annotate("text", x = Inf, y = Inf, label = paste("Spearman's rho:", round(spearman_cor, 2)), 
           hjust = 1.1, vjust = 2, size = 5, color = "red")  # Annotating plot with Spearman's correlation coefficient

# Display the scatter plot with Spearman's correlation coefficient
print(plot_spearman)

# Create a histogram of Extraversion scores
ggplot(dfBig5, aes(x = Extrav)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Extraversion Scores", x = "Extraversion Score", y = "Frequency")

# Create a density plot of Neuroticism scores
ggplot(dfBig5, aes(x = Neurot)) +
  geom_density(fill = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Neuroticism Scores", x = "Neuroticism Score", y = "Density")

# Create histograms of Openness scores grouped by English Nativeness
ggplot(dfBig5, aes(x = Open)) +
  geom_histogram(binwidth = 1, fill = "green", color = "white") +
  facet_wrap(~engnat) +
  theme_minimal() +
  labs(title = "Openness Scores by English Nativeness", x = "Openness Score", y = "Frequency")

# Calculate correlation matrix
df_subset <- dfBig5[, c("Extrav", "Neurot")]
cor_matrix <- cor(df_subset, use = "complete.obs")
# Use ggplot2 or pheatmap to visualize. Use package called pheatmap
install.packages("pheatmap")
library(pheatmap)
pheatmap(cor_matrix, display_numbers = TRUE)


# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)