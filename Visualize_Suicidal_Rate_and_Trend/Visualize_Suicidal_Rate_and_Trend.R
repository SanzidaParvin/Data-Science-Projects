# Name: Sanzida Parvin
# Read the data
suicide = read.csv(file = "C:/Users/...../suicide.csv")

# Show the first few rows of the dataset
head(suicide)
names(suicide)

# Change the column name to read it properly
colnames(suicide)[which(names(suicide) == "ï..country")] <- "country"
names(suicide)

# Filtering out the USA data only
USA_data = suicide[(which(suicide$country == "United States")), ]
USA_data[1:5,]

# Make a data frame
USA_data = data.frame(USA_data[,1:6])
USA_data[1:5,]
attach(USA_data)

#?aggregate
# Compute Summary Statistics of Data Subsets. aggregate function Splits the data into subsets, 
# computes summary statistics for each, and returns the result in a convenient form.
num <- aggregate(suicides_no ~ year, USA_data, sum)

# scatter plot of the summary statistics
plot(num, main = "Yearly Suicide Rate in USA", xlab = "Year", ylab = "Number of Suicides", 
     col = "blue", lwd = 2, ylim = c(25000,45000))

# bar plot of the summary statistics
barplot(height = num$suicides_no, names.arg = num$year, main = "Yearly Suicide Rate in USA", 
        xlab = "Year", ylab = "Number of Suicides", col = "gray87")

# scatter plot with line of the summary statistics
plot(num$year,num$suicides_no,type = "l", main = "Yearly Suicide Rate in USA", 
     xlab = "Year", ylab = "Number of Suicides", col = "red", lwd = 2, ylim = c(25000,45000))

# Showing the trend between male
male = USA_data[which(USA_data$sex == "male"),]
num_male = aggregate(male$suicides_no ~ male$year, male, sum)
num_male
plot(num_male, col = "blue", lwd = 2)

# Showing the trend between female
female = USA_data[which(USA_data$sex == "female"),]
num_female = aggregate(female$suicides_no ~ female$year, female, sum)
num_female
plot(num_female, col = "red", lwd = 2)

# Compare the trends between male and female
plot(num_male$`male$year`, num_male$`male$suicides_no`,type = "l", col = "blue", lwd = 2, main = "Yearly Suicide Rate in USA",
     xlab = "Year", ylab = "Number of Suicides", ylim = c(5000,45000))
lines(num_female$`female$year`,num_female$`female$suicides_no`,type = "l", col = "red", lwd = 2)
legend("topleft", c("Male", "Female"), col = c("blue", "red"), lty = 1)







