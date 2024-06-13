 # Practice 1

#Load e1071 library
install.packages('e1071')
library(e1071)

# Question 1

# Read CSV file
electric_vehicle <- read.csv("D:/UMKC/Spring 2024/CS5590/electric.vehicle.data.csv")

View(electric_vehicle) #data frame

#Summary statistics
summary(electric_vehicle$electric_range)


#Skewness and kurtosis
range <- electric_vehicle$electric_range
skewness(range)
kurtosis(range)

# Question 2

#Frequency Distribution
electric_vehicle$electric_range_cat = ifelse(range <= 100,"0-100",
                                             ifelse(range <= 200, "101-200", ">200"))
table(electric_vehicle$electric_range_cat)

# Question 3

table_result <- table(electric_vehicle$electric_vehicle_type, electric_vehicle$clean_alternative_fuel_vehicle_eligibility) #cross-tabulation

#Create a new data frame from the table result
electric_vehicle_data <- data.frame(
  Electric_Vehicle_Type = c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)"),
  Clean_Alternative_Fuel_Vehicle_Eligible = c(46903, 15246),
  Not_eligible_due_to_low_battery_range = c(8, 17485)
)
electric_vehicle_data

# Calculate the percentage across rows
percentage <- prop.table(as.matrix(electric_vehicle_data[, -1]), margin = 1) * 100
percentage

# Create a data frame with the percentage values
percentage <- data.frame(
  Clean_Alternative_Fuel_Vehicle_Eligible = c(99.98, 46.56),
  Not_eligible_due_to_low_battery_range = c(0.02, 53.44)
)
percentage

# Add row names
rownames(percentage) <- c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)")
percentage

# Transpose the data frame
percentage <- t(percentage)
percentage

# Create clustered bar plot
barplot(percentage, beside = TRUE, legend.text = TRUE, args.legend = list(x = "topright"), 
        main = "Percentage of Electric Vehicle Types by Eligibility",
        xlab = "Electric Vehicle Type", ylab = "Percentage",
        col = c("blue", "red"),width=1, ylim = c(0, 100))


# Question 4

#Create a new data frame from two columns: electric_vehicle_type and electric_range in electric_vehicles
new_df <- data.frame(Column1=electric_vehicle$electric_vehicle_type, Column2=electric_vehicle$electric_range)

# Sort the data frame based on values in Column1
new_df <- new_df[order(new_df$Column1), ]
new_df

#Factor column 1
new_df$Column1<-factor(new_df$Column1, levels=c("Battery Electric Vehicle (BEV)","Plug-in Hybrid Electric Vehicle (PHEV)"))
new_df

#Create a box plot
boxplot(Column2 ~ Column1, data = new_df, xlab = "Electric Vehicle Type", ylab = "Electric Range", main = "Box Plot of electric range for BEV and PHEV")

# Question 5

#Create a new data frame from two columns: clean_alternative_fuel_vehicle_eligibility and electric_range in electric_vehicles
new_df_01 <- data.frame(Column1=electric_vehicle$clean_alternative_fuel_vehicle_eligibility, Column2=electric_vehicle$electric_range)

#Sort the data frame based on values in Column1
new_df_01 <- new_df_01[order(new_df_01$Column1), ]
new_df_01

avg_by_category <- aggregate(Column2 ~ Column1, data = new_df_01, FUN = mean)
avg_by_category

#Plot bar plot
barplot(avg_by_category$Column2, names.arg = avg_by_category$Column1,
        xlab = "Eligibility criteria", ylab = "Average electric range",
        main = "Average electric range by eligibility criteria",
        col = "skyblue", 
        ylim = c(0, max(avg_by_category$Column2) * 1.5) # Adjust y-axis limits 
)

# Question 6

#Create a new data frame containing all the "Battery Electric Vehicle (BEV)" and its values from new_df
new_df_02 <- new_df[new_df$Column1 == "Battery Electric Vehicle (BEV)", ]

#Calculate mean and standard deviation
mean_value <- mean(new_df_02$Column2)
sd_value <- sd(new_df_02$Column2)

#Calculate lower and upper bounds of 3-standard deviation range
lower_bound <- mean_value - 3 * sd_value
upper_bound <- mean_value + 3 * sd_value

#Count the number of vehicles within the 3-standard deviation range
within_range <- sum(new_df_02$Column2 >= lower_bound & new_df_02$Column2 <= upper_bound)

nrow(new_df_02)
#Calculate the percentage
percentage_within_range <- (within_range / nrow(new_df_02)) * 100
percentage_within_range

#Create histogram
hist(new_df_02$Column2, 
     main = " Histogram of the electric range for BEVs",
     xlab = "Electric Range",
     ylab = "Frequency",
     col = "skyblue", 
     border = "black",
     ylim = c(0, 20000)
)

