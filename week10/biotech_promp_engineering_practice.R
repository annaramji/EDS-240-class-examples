library(tidyverse)
library(here)
biotech_unedited <- read_csv(here("week10", "data", "ChatGPT_Practice_Dataset.xlsx - Sheet1.csv"))

View(biotech_unedited)

# scenario 1:
#Our Sales Representatives have surpassed their annual budget resulting in us operating with a negative balance. We want to know whose performance and compensation are not commensurate so we can fire them. We need to let go of enough people; such that, our total budget is reduced by $200,000 per year. 

#How do we strike a balance between recommending who to lay-off and who to keep? What should we consider? 
# Performance
# Salary
# Position (i.e., you can’t fire all biochemists even if they all suck)

head(biotech_unedited)


colnames(biotech_unedited) <- c("Info")  # Set column name

# -------- Split the columns  -----------
biotech_split <- strsplit(biotech_unedited$Info, ",")
biotech_df <- data.frame(do.call(rbind, biotech_split))

# -------- Rename the columns ----------
# assigning column names 
colnames(biotech_df) <- c("UniqueLabel", "Name", "Position", "State", "Salary", "Years_Worked", "Performance_Last_Year", "Performance_2_Years_Ago")

# ------ Convert Columns to Appropriate Data Types ---------

biotech_df$Salary <- as.numeric(biotech_df$Salary)
biotech_df$Years_Worked <- as.numeric(biotech_df$Years_Worked)
biotech_df$Performance_Last_Year <- as.numeric(biotech_df$Performance_Last_Year)
biotech_df$Performance_2_Years_Ago <- as.numeric(biotech_df$Performance_2_Years_Ago)


# ------ Add average performance column ----------

biotech_df$Average_Performance <- rowMeans(biotech_df[, c("Performance_Last_Year", "Performance_2_Years_Ago")], na.rm = TRUE)


# ----------- Calculate Total Current Expenses -----------
# Calculate the total current company expenses.

total_expenses <- sum(biotech_df$Salary)


# ----- Determine Employees to Terminate ------------
#. manually inspected which employees to remove... 
labels_to_remove <- c("Employee_38", "Employee_4", "Employee_22")
# Remove rows with specified labels
# note: Sevan's naming, not mine :)
biotech_df_byebye <- biotech_df[!biotech_df$Name %in% labels_to_remove, ]
# Show the resulting data frame
View(biotech_df_byebye)

# I personally disagree with this method.



# moving on

# ===== Scenario 2 =============
# Across all of their offices, the clients would like to know which positions are paid the most on average. 
#How do we figure out which positions are paid significantly  more? 
  
# Plot strategies
# Boxplot / histogram / density plot / (lots of options…)
# What about statistical analysis?



  
# plotting
# Histogram
histogram_plot <- ggplot(biotech_df, aes(x = Salary, fill = Position)) +
  geom_histogram(binwidth = 10000, color = "white", alpha = 0.8) +
  facet_wrap(~Position, scales = "free_y", ncol = 3) +
  labs(title = "Salary Distribution by Position (Histogram)",
       x = "Salary",
       y = "Frequency") +
  theme_minimal()

# Show histogram
print(histogram_plot)



ggplot(biotech_df_byebye, aes(x = Position, y = Salary)) +
  geom_boxplot() +
  labs(x = "Position", y = "Salary") +
  coord_flip() +
  theme_bw()


# -------- statistical tests ------------
# Perform ANOVA test
anova_result <- aov(Salary ~ Position, data = biotech_df)

# Summary of ANOVA test
summary(anova_result)
# shows no significance across all
# we want to run pair-wise

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Summary of Tukey's HSD test
print(tukey_result)

# also shows no significant difference between groups