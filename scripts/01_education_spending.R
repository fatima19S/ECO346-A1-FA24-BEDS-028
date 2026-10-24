
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


# Step 15: Load graph package

library(ggplot2)

# Step 16: Keep spending from Rs 100 to Rs 1,000,000

plot_students <- subset(
  students,
  ed_total >= 100 & ed_total <= 1000000
)

# Step 17: Convert spending to log10 scale

plot_students$log_spending <- log10(
  plot_students$ed_total
)

# Step 18: Create weighted histogram bins

bin_edges <- seq(2, 6, by = 0.1)

bin_id <- cut(
  plot_students$log_spending,
  breaks = bin_edges,
  include.lowest = TRUE,
  labels = FALSE
)

# Add weights of students in each bin

bin_weights <- tapply(
  plot_students$hhweight,
  factor(bin_id, levels = 1:40),
  sum
)

bin_weights[is.na(bin_weights)] <- 0

# Step 19: Calculate density per log10 unit

bin_density <- as.numeric(bin_weights) /
  (sum(plot_students$hhweight) * 0.1)

hist_data <- data.frame(
  midpoint = head(bin_edges, -1) + 0.05,
  density = bin_density
)

# Step 20: Prepare labels

median_label <- paste0(
  "Median: Rs ",
  format(round(weighted_median),
         big.mark = ",")
)

p90_label <- paste0(
  "90th percentile: Rs ",
  format(round(p90),
         big.mark = ",")
)

caption_text <- paste0(
  "Source: PSLM 2019-20. Rural KP students, ",
  "n = ", nrow(students),
  ". Weighted by hhweight. ",
  "Excluded from plotted range: ", excluded, "."
)

# Step 21: Draw the histogram

p <- ggplot(
  hist_data,
  aes(x = midpoint, y = density)
) +
  
  geom_col(
    width = 0.1,
    fill = "grey55",
    color = "white"
  ) +
  
  geom_vline(
    xintercept = log10(weighted_median),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  geom_vline(
    xintercept = log10(p90),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  annotate(
    "label",
    x = log10(weighted_median),
    y = max(hist_data$density) * 0.90,
    label = median_label,
    size = 3
  ) +
  
  annotate(
    "label",
    x = log10(p90),
    y = max(hist_data$density) * 0.70,
    label = p90_label,
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 2:6,
    labels = c(
      "Rs 100",
      "Rs 1,000",
      "Rs 10,000",
      "Rs 100,000",
      "Rs 1,000,000"
    ),
    limits = c(2, 6)
  ) +
  
  labs(
    title = paste0(
      "Median annual education spending in rural KP is Rs ",
      format(round(weighted_median), big.mark = ",")
    ),
    x = "Annual education spending (rupees, log scale)",
    y = "Weighted density per log10 unit",
    caption = caption_text
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

# Step 22: Display graph

print(p)

# Step 23: Save graph as SVG

ggsave(
  filename = "figures/education_spending.svg",
  plot = p,
  width = 11,
  height = 6,
  units = "in"
)

# Check that histogram density is normalized
sum(hist_data$density) * 0.1

