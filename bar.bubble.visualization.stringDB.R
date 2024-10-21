#this script is for creating bar and bubble plot for stringDB results
library("data.table")
library("openxlsx")
library("readxl")
library("tidyverse")
library("ggplot2")
setwd("/Users/teestanaskar/Dropbox/Teesta/Placenta/Human.Placenta/bothsex/Proteomics/stringdb/")
#load stringDB results
data = read.xlsx("MS_unique_control/string_results_unique_control_MS.xlsx", sheet = 2)
# Calculate min and max for FDR
fdr_min <- min(data$false.discovery.rate)
fdr_max <- max(data$false.discovery.rate)

# Create the plot with customized label font and color
ggplot(data, aes(x = signal, y = term.description)) + 
  
  # Create the bar plot, colored by FDR
  geom_bar(aes(fill = false.discovery.rate), stat = "identity", width = 0.4) + 
  
  # Add bubble plot with x position adjusted to place bubbles at the right edge of the bars
  geom_point(aes(x = signal + 0.02, size = observed.gene.count, fill = false.discovery.rate), 
             color = "black", shape = 21) +  # Position bubbles at the right end of bars
  
  # Customize the bubble size range
  scale_size_continuous(range = c(5, 15)) +
  
  # Use a gradient color scale for both bar and bubble fill based on FDR
  scale_fill_gradient(low = "green", high = "blue", name = "False Discovery Rate (FDR)",
                      limits = c(fdr_min, fdr_max), 
                      breaks = seq(fdr_min, fdr_max, length.out = 4),
                      labels = round(seq(fdr_min, fdr_max, length.out = 4), 3)) +
  
  # Customize axis labels and plot title
  labs(x = "Signal Peptide Score", y = "Pathways", 
       title = "Bar and Bubble Plot for Signal Peptide Scores",
       size = "Gene Count") +
  
  # Customize font and color for labels
  theme(axis.title.x = element_text(size = 14, color = "black", face = "bold"),  # X-axis label
        axis.title.y = element_text(size = 14, color = "black", face = "bold"),  # Y-axis label
        plot.title = element_text(size = 16, color = "black", face = "bold"),     # Plot title
        legend.title = element_text(size = 8, color = "black"),               # Legend title
        legend.text = element_text(size = 8, color = "black"),                    # Legend text
        axis.text.x = element_text(size = 10, color = "black", face = "bold"),                    # X-axis text
        axis.text.y = element_text(size = 10, color = "black", face = "bold")) +                   # Y-axis text                      # Customize major grid lines
  # Rotate x-axis text for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot with customized dimensions to avoid being cut off
ggsave("bar_bubble_plot_unique_controlMS.png", width = 12, height = 8, dpi = 300)
