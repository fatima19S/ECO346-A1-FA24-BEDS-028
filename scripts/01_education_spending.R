
persons <- read.csv("data/pslm_person_subset.csv")

households <- read.csv("data/pslm_hh_subset (1).csv")
head(persons)
head(households)

# Step 2: Merge the datasets
data <- merge(
  persons,
  households[, c("hhid", "hhweight", "prov_code", "urban_rural")],
  by = "hhid"
)

# Step 3: Select rural Khyber Pakhtunkhwa
kp_rural <- subset(
  data,
  prov_code == 1 & urban_rural == 1
)

# Step 4: Select students with education spending
students <- subset(
  kp_rural,
  !is.na(ed_total) & ed_total > 0 &
    !is.na(hhweight) & hhweight > 0
)

# Step 5: Count the students
nrow(students)

# Step 6: Look at education expenditure
summary(students$ed_total)

# Step 7: Create a function for weighted percentiles

weighted_quantile <- function(x, w, p) {
  
  # Arrange expenditure from smallest to largest
  order_index <- order(x)
  
  x <- x[order_index]
  w <- w[order_index]
  
  # Calculate cumulative weight
  cumulative_weight <- cumsum(w) / sum(w)
  
  # Find the required percentile
  position <- which(cumulative_weight >= p)[1]
  
  return(x[position])
}


# Step 8: Calculate the unweighted median

unweighted_median <- median(students$ed_total)


# Step 9: Calculate the weighted median

weighted_median <- weighted_quantile(
  students$ed_total,
  students$hhweight,
  0.50
)


# Step 10: Calculate the weighted 10th percentile

p10 <- weighted_quantile(
  students$ed_total,
  students$hhweight,
  0.10
)


# Step 11: Calculate the weighted 90th percentile

p90 <- weighted_quantile(
  students$ed_total,
  students$hhweight,
  0.90
)


# Step 12: Calculate the 90th/10th percentile ratio

ratio <- p90 / p10


# Step 13: Count students outside the graph range

excluded <- sum(
  students$ed_total < 100 |
    students$ed_total > 1000000
)


# Step 14: Display the results

cat("\nQUESTION 2 RESULTS\n")

cat("Number of students:", nrow(students), "\n")

cat("Unweighted median:", round(unweighted_median), "\n")

cat("Weighted median:", round(weighted_median), "\n")

cat("10th percentile:", round(p10), "\n")

cat("90th percentile:", round(p90), "\n")

cat("90th/10th ratio:", round(ratio, 2), "\n")

cat("Excluded students:", excluded, "\n")