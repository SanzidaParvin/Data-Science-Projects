# Name: Sanzida Parvin
# Load the dataset
loans_df = read.csv("C:/Users/...../kiva_loans.csv")
colnames(loans_df)
head(loans_df)

dim(loans_df)
# total number of observations
num_row = length(loans_df$id); num_row
summary(loans_df)

# find out the missing values and the percentage of missing values
# Data type of the variables
sapply(loans_df, class)

missing_id = length(which(is.na(loans_df$id))); missing_id
missing_funded_amount = length(which(is.na(loans_df$funded_amount))); missing_funded_amount
# no missing value in funded amount (it is fully funded or partially funded)

missing_loan_amount = length(which(is.na(loans_df$loan_amount))); missing_loan_amount
# no missing value in loan amount

missing_activity = length(which(loans_df$activity=="")); missing_activity
missing_sector = length(which(loans_df$sector=="")); missing_sector

#-------------------------------
# Order the most frequent sectors
tbl_sector = table(loans_df$sector); tbl_sector
ordered_sector <- tbl_sector[order(tbl_sector)]; ordered_sector
names(ordered_sector)
op <- par(mar = c(5,5,4,2) + 0.1) # 'bottom', 'left', 'top', 'right'.
barplot(ordered_sector, names.arg = names(ordered_sector), cex.names=0.7, axisnames = TRUE, xlim = c(0,200000),
        main = "Sector Wise Distribution of Loans", xlab = "Count", col=heat.colors(15), horiz = TRUE,  las = 1, space = .5)
#--------------------------------
  
missing_use = length(which(loans_df$use=="")); missing_use
per_missing_use = round(((missing_use / num_row) * 100),2); cat("Percentage of missing value in use is ", per_missing_use)

missing_country_code = length(which(loans_df$country_code=="")); missing_country_code
per_missing_country_code = round(((missing_country_code / num_row) * 100),3); cat("Percentage of missing value in country_code is ", per_missing_country_code)

#----------------------
# Most frequent countries
tbl_country = table(loans_df$country); tbl_country
ordered_country <- tbl_country[order(tbl_country)]; ordered_country
top = head(ordered_country,20);top

names(ordered_country)
op <- par(mar = c(5,6,4,2) + 0.1) # 'bottom', 'left', 'top', 'right'.
barplot(ordered_country, names.arg = names(ordered_country), cex.names=0.7, 
        main = "Country Wise Distribution of Loans", xlab = "Count", col=rainbow(20), horiz = TRUE,  las = 1, space = .5)

# ----------------------------
missing_country = length(which(loans_df$country=="")); missing_country
missing_region = length(which(loans_df$region=="")); missing_region
per_missing_region = round(((missing_region / num_row) * 100),2); cat("Percentage of missing value in region is ", per_missing_region)

missing_currency = length(which(loans_df$currency=="")); missing_currency
missing_partner_id = length(which(is.na(loans_df$partner_id))); missing_partner_id
per_missing_partner_id = round(((missing_partner_id / num_row) * 100),2); cat("Percentage of missing value in partner_id is ", per_missing_partner_id)

missing_posted_time = length(which(loans_df$posted_time=="")); missing_posted_time
missing_disbursed_time = length(which(loans_df$disbursed_time=="")); missing_disbursed_time
per_missing_disbursed_time = round(((missing_disbursed_time / num_row) * 100),2); cat("Percentage of missing value in disbursed_time is ", per_missing_disbursed_time)

missing_funded_time = length(which(loans_df$funded_time=="")); missing_funded_time  # check this one again
per_missing_funded_time = round(((missing_funded_time / num_row) * 100),2); cat("Percentage of missing value in funded_time is ", per_missing_funded_time)

missing_term_in_months = length(which(is.na(loans_df$term_in_months))); missing_term_in_months
missing_lender_count = length(which(is.na(loans_df$lender_count))); missing_lender_count

missing_tags = length(which(loans_df$tags=="")); missing_tags  # have to deal with it
per_missing_tags = round(((missing_tags / num_row) * 100),2); cat("Percentage of missing value in tags is ", per_missing_tags)

missing_borrower_genders = length(which(loans_df$borrower_genders=="")); missing_borrower_genders
per_missing_borrower_genders = round(((missing_borrower_genders / num_row) * 100),2); cat("Percentage of missing value in borrower_genders is ", per_missing_borrower_genders)

#####################
no_miss_gender = loans_df$borrower_genders[which(loans_df$borrower_genders != "")]
# Number of male, female and both borrower
tbl_gender = table(no_miss_gender); tbl_gender
ordered_gender <- tbl_gender[order(tbl_gender)]; ordered_gender
# Percentage of borrower gender
ordered_gender <- (tbl_gender[order(tbl_gender)])/1000; ordered_gender

library("viridis")
# Borrower's gender
barplot(ordered_gender, names.arg = names(ordered_gender), main = "Gender Wise Distribution", 
        ylim = c(0,500),xlab = "Gender", ylab = "Count (thousands)", col=viridis(3), space = .1)

missing_repayment_interval = length(which(loans_df$repayment_interval=="")); missing_repayment_interval

#------------------
no_miss_interval = loans_df$repayment_interval[which(loans_df$repayment_interval != "")]
# Repayment intervals
tbl_repayment = table(no_miss_interval); tbl_repayment
tbl_repayment <- tbl_repayment/1000; tbl_repayment

barplot(tbl_repayment, names.arg = names(tbl_repayment), main = "Repayment Interval", 
        ylim = c(0,350),xlab = "Interval", ylab = "Count (thousands)", col=rainbow(4), space = .1)

#--------------------

missing_date = length(which(loans_df$date=="")); missing_date

missing_list = data.frame("Variables" = c("id","funded_amount","loan_amount","activity","sector","use","country_code","country","region",
                                          "currency","partner_id","posted_time","disbursed_time","funded_time","term_in_months",
                                          "lender_count","tags","borrower_genders","repayment_interval","date"),
                          "Missing_values" = c(missing_id,missing_funded_amount,missing_loan_amount,missing_activity,missing_sector,
                                                missing_use,missing_country_code,missing_country,missing_region,missing_currency,
                                                missing_partner_id,missing_posted_time,missing_disbursed_time,missing_funded_time,
                                                missing_term_in_months,missing_lender_count,missing_tags,missing_borrower_genders,
                                                missing_repayment_interval,missing_date),
                          "Percentage" = c(0,0,0,0,0,per_missing_use,per_missing_country_code,0,per_missing_region,0,per_missing_partner_id,
                                           0,per_missing_disbursed_time,per_missing_funded_time,0,0,per_missing_tags,
                                           per_missing_borrower_genders,0,0)); missing_list

#install.packages("ggplot2")
library(ggplot2)

# According to Kiva, all loans should be disburshed fully within 30 days
# find out partially or unfunded loans
sapply(loans_df, class)
loans_df$diff_funded_amt = loans_df$loan_amount - loans_df$funded_amount  # add extra column on amount difference
num_amt_default = length(which(loans_df$diff_funded_amt > 0)); num_amt_default
per_amt_default = round(((num_amt_default / num_row) * 100),2); per_amt_default

not_funded = length(which(loans_df$diff_funded_amt == loans_df$loan_amount)); not_funded
per_not_funded = round(((not_funded / num_row) * 100),2); per_not_funded

# convert date columns into date class
loans_df$posted_time = as.Date(as.character(loans_df$posted_time))
class(loans_df$posted_time)
loans_df$disbursed_time = as.Date(as.character(loans_df$disbursed_time))
class(loans_df$disbursed_time)

loans_df$funded_time = as.Date(as.character(loans_df$funded_time))
class(loans_df$funded_time)

# Add extra column on expiration status
loans_df$expired_loan = loans_df$funded_time - loans_df$posted_time

tot_expired = length(which(loans_df$expired_loan > 30)); tot_expired
percentage_of_expired_loan = round(((tot_expired/length(loans_df$posted_time))*100),2); percentage_of_expired_loan

# Total number and percentage of funding category. According to Kiva loans should be fully distributed to the borrowers.
loan_default = data.frame("Default_Category" = c("Partially Funded", "Not Funded", "Time Expired"),
                             "Total_Number" = c(num_amt_default,not_funded,tot_expired),
                             "Percentage" = c(per_amt_default,per_not_funded,percentage_of_expired_loan)); loan_default


ggplot(data=loan_default, aes(x=Default_Category, y=Total_Number, fill = Default_Category)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=Total_Number), vjust=1.6, color="black", size=3.5) + 
  labs(title="Number of Loan Defaults by Category") + 
  theme_minimal()

# loans that were partially or not funded as well as the funded time was greater than 30 days were considered as expired/bad loans
dim(loans_df)
colnames(loans_df)
#head(loans_df)

# Define Good and bad loans. According to Kiva the loans that are not fully funded or expired are treated as loan default.
loans_df$loan_status <- "good"
loans_df$loan_status[which((loans_df$diff_funded_amt > 0) | (loans_df$expired_loan > 30))] <- "bad"
head(loans_df)

# Percentage of good and bad loans
percentage_bad_loan = round(((length(which(loans_df$loan_status == "bad")) / length(loans_df$loan_status)) * 100),2); percentage_bad_loan
# total percentage of bad loan is 20.45%

loans_df$fund_status <- "fully_funded"
loans_df$fund_status[which((loans_df$diff_funded_amt == loans_df$loan_amount))] <- "not_funded"
loans_df$fund_status[which((loans_df$diff_funded_amt > 0) & (loans_df$diff_funded_amt != loans_df$loan_amount))] <- "partially_funded"

#-----------------
tbl_fund = table(loans_df$fund_status); tbl_fund
df_fund = data.frame(tbl_fund)
tbl_fund <- (tbl_fund)/1000; tbl_fund

# Funding Status
barplot(tbl_fund, names.arg = names(tbl_fund), main = "Distribution of Funding Status",
        ylim = c(0,600),xlab = "Funding Status", ylab = "Count (thousands)", col=viridis(3), space = .1)

colnames(loans_df)
# drop diff_funded_amt column
loans_df = loans_df[-21] 

loans_df$expiration <- "not_expired"
loans_df$expiration[which(loans_df$expired_loan > 30)] <- "expired"

# -----------------
# Status of loan expiration
tbl_exp = (table(loans_df$expiration))/1000; tbl_exp
barplot(tbl_exp, names.arg = names(tbl_exp), main = "Distribution of Expired Loans",
        ylim = c(0,600),xlab = "Status", ylab = "Count (thousands)", col=rainbow(3), space = .1)

unique(loans_df$expiration)
colnames(loans_df)
loans_df = loans_df[-21] # drop expired_loan column
colnames(loans_df)

#-------------
df = data.frame(loans_df$country,loans_df$fund_status)
head(df)
names(df)[1] <- "country"
names(df)[2] <- "fund_status"
head(df)

tbl_df = data.frame(table(df));tbl_df
colnames(tbl_df)

# Frequency of funding status against country
as = data.frame(tbl_df[order(c(tbl_df$Freq,tbl_df$fund_status[tbl_df$country]), decreasing = TRUE),])
head(as,20)

df2 = data.frame(loans_df$sector, loans_df$fund_status)
head(df2)

# Frequency of funding status against sector
names(df2)[c(1,2)] <- c("sector","fund_status")
head(df2)
tbl_df2 = data.frame(table(df2));tbl_df2
ord_tbl_df2 = data.frame(tbl_df2[order(c(tbl_df2$Freq, tbl_df2$sector[tbl_df2$fund_status]), decreasing = TRUE),])
ord_tbl_df2 = na.omit(ord_tbl_df2)
head(ord_tbl_df2,20)

loans_df2 = loans_df
colnames(loans_df2)

# Remove those variables which has unique value in almost every single row. Also remove the redundent and unnecessary variables
loans_df2 = loans_df2[-c(4,6,7,9,10,12,13,14,17,20)]
#loans_df2$tags <- NULL
colnames(loans_df2)
sapply(loans_df2, class)

# Convert the datatypes as factor
loans_df2$sector = as.factor(loans_df2$sector)
loans_df2$country = as.factor(loans_df2$country)
loans_df2$borrower_genders = as.factor(loans_df2$borrower_genders)
loans_df2$repayment_interval = as.factor(loans_df2$repayment_interval)
loans_df2$loan_status = as.factor(loans_df2$loan_status)
loans_df2$fund_status = as.factor(loans_df2$fund_status)
loans_df2$expiration = as.factor(loans_df2$expiration)
sapply(loans_df2, class)

# Remove missing vaues
length(which(is.na(loans_df2)))
length(which(loans_df2 ==""))

loans_df2 = loans_df2[complete.cases(loans_df2),]
loans_df2 = na.omit(loans_df2)

length(which(is.na(loans_df2)))
length(which(loans_df2 ==""))

colnames(loans_df2)
length(which(is.na(loans_df2$partner_id)))
length(which(loans_df2$borrower_genders == ""))

filtered_data = loans_df2[which(loans_df2$borrower_genders !=""), ]
length(which(filtered_data ==""))
length(which(is.na(filtered_data)))
#summary(filtered_data)
dim(filtered_data)
head(filtered_data)

# Filter the dataframe with necessary variables for analysis
final_data = filtered_data[,c(1,2,3,5,6,7,8,9,10,11)]
head(final_data)
colnames(final_data)

# Split the dataset into test and training datasets by 20 80 ratio
#install.packages("caTools")
require(caTools)
set.seed(2)   #  set seed to ensure the same random numbers generated every time
sample = sample.split(final_data,SplitRatio = 0.80) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_data = subset(final_data,sample == TRUE) # creates a training dataset with rows which are marked as TRUE (80%)
test_data = subset(final_data, sample==FALSE)  # 20%

colnames(training_data)
# Fit the logistic regression model with training data
loan_model = glm(loan_status ~., data = training_data, family = "binomial", maxit = 100)

summary(loan_model)
# Predicted probability with test data
predprob = predict(loan_model, newdata = test_data, type="response")
pred_goodbad = cut(predprob, breaks = c(-Inf, 0.5, Inf), labels=c("Bad", "Good"))

# Contingency table to display the frequency distribution
contin_table1 = table(test_data$loan_status, pred_goodbad)
addmargins(contin_table1)

# predict the overall accuracy level
proportion1 = sum(diag(contin_table1)) / sum(contin_table1)  
print(paste('Overall Accuracy(Correctly predicted outcomes):',round((proportion1*100),2)))

TP = contin_table1[1,1]
FN = contin_table1[2,1]
TN = contin_table1[2,2]
FP = contin_table1[1,2]

true_bad = TP/(TP+FN)
print(paste('Correctly predicted bad loans: ', round((true_bad*100),2)))



