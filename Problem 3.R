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
#miss_dtable
#miss_na_matrix_dtable <- p3_data.table[,sum(is.na(p3_data.table)) ,]
#is.na(p3_data.table)

#2 Preparation
# First, we clean our data set
p3_data_clean <- na.omit(p3_data)
# Now, we can be sure that set is clean of missing values using algorithm from #1
#
# miss_apply_clean <- data.frame(variable = names(p3_data), num_miss = 0)
# miss_na_matrix_clean <- apply(p3_data_clean, 2, is.na)
# miss_apply_clean[, 2] <- apply(miss_na_matrix_clean, 2, sum)

unique_birth_dates <- unique(p3_data$date_of_birth)
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
mean_f <-
  vector("numeric", length(unique(p3_data_clean$date_of_birth)))
for (i in 1:length(unique_birth_dates)) {
  mean_f[i] <- mean(p3_data_f$exp_by_1996[p3_data_f$date_of_birth ==
                                            sorted_unique_dates[i]])
}

# Calculating means for male subsample and every birth month
mean_m <-
  vector("numeric", length(unique(p3_data_clean$date_of_birth)))
for (i in 1:length(unique_birth_dates)) {
  mean_m[i] <- mean(p3_data_f$exp_by_1996[p3_data_m$date_of_birth ==
                                            sorted_unique_dates[i]])
}

# Binding a vector 
mean_fm <- c(mean_f, mean_m)

# Formating the output as a data table for an ease of use
mean_loop <-
  data.frame(
    date_of_birth = c(sorted_unique_dates, sorted_unique_dates),
    gender = rep(c("F", "M"), c(252, 252)),
    means = mean_fm
  )

#2 apply
mean_apply <-
  data.frame(
    date_of_birth = c(sorted_unique_dates, sorted_unique_dates),
    gender = rep(c("F", "M"), c(252, 252)),
    means = 0
  )
mean_apply <- as.data.frame(lapply(
  p3_data_clean$exp_by_1996,
  INDEX = p3_data_clean$date_of_birth,
  FUN = mean
))

