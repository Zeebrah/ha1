#Preparation
setwd(
  "/Users/ivanloktaev/Yandex.Disk.localized/Uni/4\ Курс/Data\ Science/ha1 materials"
)
p3_data <- read.csv("hw1p3.csv", stringsAsFactors = FALSE)
library(data.table)

#1 loops
miss_loop <- data.frame(variable = names(p3_data), num_miss = 0)
for (i in 1:231) {
  miss_loop[i, 2] <- sum(is.na(p3_data[, i]))
}
# Now, miss_loop is a data frame containting
# Names of variables in first column
# And respective number of missing values in the 2nd one

#1 apply
miss_apply <- data.frame(variable = names(p3_data), num_miss = 0)
miss_na_matrix <- apply(p3_data, 2, is.na)
# Boolean matrix of wheather particular value is NA
miss_apply[, 2] <- apply(miss_na_matrix, 2, sum)
# Same result as before, now table is miss_apply

#1 data.table
# First convert the data.frame into data.table
p3_data_table <- as.data.table(p3_data)
miss_dtable <- p3_data_table[,lapply(.SD, is.na)][,lapply(.SD, sum)]

#2 Preparation
# First we clean our data set of NAs in exp_by_1996
# so that the mean function do not freak out
p3_data_clean <- as.data.table(p3_data)
p3_data_clean <- as.data.frame(p3_data_clean[!is.na(p3_data_clean$exp_by_1996),])

# Now, we can be sure that set is clean of missing values using algorithm from #1
# miss_apply_clean <- data.frame(variable = names(p3_data), num_miss = 0)
# miss_na_matrix_clean <- apply(p3_data_clean, 2, is.na)
# miss_apply_clean[, 2] <- apply(miss_na_matrix_clean, 2, sum)

# Since we  need to average out by ar not a month we convert
# year and month to just year
p3_data_clean$date_of_birth <-
  substr(as.character(p3_data_clean$date_of_birth), rep(1, 98990), 4)

unique_birth_dates <- unique(p3_data_clean$date_of_birth)
p3_data_m <- p3_data_clean[p3_data_clean$gender == "M",]
p3_data_f <- p3_data_clean[p3_data_clean$gender == "F",]
sorted_unique_dates <-
  sort(unique(p3_data_clean$date_of_birth), decreasing = FALSE)



# # By far the easiest way to calculate the means by factors,
# # Unfortunately, we are not given a choice here
# mean_agg <- as.data.frame(aggregate(
#   p3_data_clean$exp_by_1996,
#   by = list(p3_data_clean$date_of_birth, p3_data_clean$gender),
#   FUN = mean
# ))

#2 loops
# Calculating means for female subsample and every birth month
mean_f_loop <-
  vector("numeric", length(unique(p3_data_clean$date_of_birth)))
for (i in 1:length(unique_birth_dates)) {
  mean_f_loop[i] <-
    mean(p3_data_f$exp_by_1996[p3_data_f$date_of_birth ==
                                 sorted_unique_dates[i]])
}

# Calculating means for male subsample and every birth month
mean_m_loop <-
  vector("numeric", length(unique(p3_data_clean$date_of_birth)))
for (i in 1:length(unique_birth_dates)) {
  mean_m_loop[i] <-
    mean(p3_data_f$exp_by_1996[p3_data_m$date_of_birth ==
                                 sorted_unique_dates[i]])
}

# Binding a output from two groups into a matrix
mean_loop <- matrix(c(mean_f_loop, mean_m_loop), ncol = 2)
# Converting to data.frame to give names
mean_loop <- as.data.frame(mean_loop)
mean_loop$date_of_birth <- sorted_unique_dates
# Formating the names to be more logical
names(mean_loop) <- c("F", "M", "Year")

#2 apply
# Converting the year and month of birth to just year of birth (again, just in case)
p3_data_clean$date_of_birth <-
  substr(as.character(p3_data_clean$date_of_birth), rep(1, 98990), 4)

# Calculating the mean by year and gender
mean_apply <- tapply(
  p3_data_clean$exp_by_1996,
  INDEX = list(p3_data_clean$date_of_birth, p3_data_clean$gender),
  FUN = mean
)

#2 data.table
#First we clean the table from missing values
p3_data_table_clean<-p3_data_table[!is.na(p3_data_table$exp_by_1996),]

#3 Graphs
# Trim the months of each year to create a plot (again)
trimmed_dates <-
  substr(as.character(sorted_unique_dates), rep(1, 252), 4)
# To preserve old layout parameters
old_par <- par()

# For females
plot(
  x = 1950:1970,
  y = mean_apply[, 1],
  type = "b",
  col = "blue",
  xlab = "Year of Birth",
  ylab = "Work experience before 1996",
  pch = 16
)
par(new = TRUE)
# For males
points(
  x = 1950:1970,
  y = mean_apply[, 2],
  type = "b",
  col = "red",
  pch = 17
)
legend(
  x = 1952,
  y = 15.1,
  legend = c("Females", "Males"),
  col = c("blue", "red"),
  pch = c(16, 17),
  bty = "n"
)

#4 loop
# We have to go back to using non-clean data, since we look for missing values

# Predefine a table for our inquiery
miss_dummie_loop <-
  data.frame(individual_id = c(1:100000),
             num_dummies_miss = 0)
# I will introduce special version of p3_data that does not have any missing values
# in gender, exp_by_1996 ao that our numbers are not skewd
p3_data_twisted <- p3_data
p3_data_twisted$gender = 0
p3_data_twisted$exp_by_1996 = 0
p3_data_twisted$date_of_birth = 0

#This loop is painfully slow but appears to work correctly anyways
for (i in 1:100000) {
  miss_dummie_loop[i, 2] <- sum(is.na(p3_data_twisted[i,]))
}

#4 apply
miss_dummie_apply <- 
  data.frame(individual_id = c(1:100000),
             num_dummies_miss = 0)

miss_dummie_apply_list <- as.data.frame(apply(X = p3_data[, 4:231], MARGIN = 1, FUN = is.na))
miss_dummie_apply[, 2] <- apply(X = miss_dummie_apply_list, MARGIN = 2, FUN = sum)

#4 data.frame

#5
# Only do this step if #4 loop was skipped
p3_data_twisted <- p3_data
p3_data_twisted$gender = 0
p3_data_twisted$exp_by_1996 = 0
p3_data_twisted$date_of_birth = 0

# Predefine the variable
tot_exp <- data.frame(individual = 1:100000, Total_WE = NA)
# Slow but does the job anyways
for (i in 1:100000){
  if (miss_dummie_apply[i,2] > 0){
    next
  }
    tot_exp[i,2] <- sum(p3_data_twisted[i,])
}

#6
tot_exp_by <- function(id,by){
  #Several mundane steps to transform a datae form easy to usable form
  by_1 <- as.numeric(substr(as.character(by), 1,4))
  by_2 <- as.numeric(substr(as.character(by), 5,6)) 
  # What the heart of the function actually uses
  by_used <- 12*c((by_1 - c(1996))) + by_2
  exp_vector <-as.numeric(p3_data[id, 4:231])
  mths_worked_by <- sum(exp_vector[1:by_used])
  return(mths_worked_by)
}

#7
tot_exp_age <- function(id, age){
  current_year <- age + as.numeric(substr(as.character(p3_data$date_of_birth[id]), 1,4))
  current_month <- as.numeric(substr(as.character(p3_data$date_of_birth[id]), 5,6))
  if (current_year > 2014 | current_year < 1996){
    return("FIELD_NOT_FOUND")
  }
  current_date <- as.numeric(paste(current_year,current_month,sep = ""))
  tot_exp_age <- 12*p3_data$exp_by_1996[id] + tot_epx_by(i,by = current_date)
  return(tot_exp_age)
}
# For checks
# tot_exp_age(2,79)

#8
# Predefine output data.frame
# Again, slow but surely works as I intended
tot_exp_by_201112 <- data.frame(individual = 1:100000, Total_WE_by_201112 = NA)
for (i in 1:100000){
  tot_exp_by_201112[i,2] <- tot_exp_by(i,201112)
}

#9
# Predefine output data.frame
# Again, slow but surely works as I intended
tot_exp_age_60 <- data.frame(individual = 1:100000, Total_WE_by_60 = NA)
for (i in 1:100000){
  tot_exp_age_60[i,2] <- tot_exp_age(i,60)
}
tot_exp_age_60
