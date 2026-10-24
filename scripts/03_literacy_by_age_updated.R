
# QUESTION 3 - LITERACY BY AGE AND SEX
# Variant 4: Rural Khyber Pakhtunkhwa

library(ggplot2)

# Step 1: Load data
persons <- read.csv("data/pslm_person_subset.csv")
households <- read.csv("data/pslm_hh_subset (1).csv")

# Step 2: Merge datasets
data <- merge(
  persons,
  households[, c("hhid", "hhweight", "prov_code", "urban_rural")],
  by = "hhid"
)

# Step 3: Select rural KP
kp_rural <- subset(
  data,
  prov_code == 1 & urban_rural == 1
)

# Step 4: Select the literacy sample
literacy <- subset(
  kp_rural,
  !is.na(age) & age >= 10 & age <= 64 &
    literate %in% c(0, 1) &
    sex %in% c(1, 2) &
    !is.na(hhweight) & hhweight > 0
)

# Step 5: Create five-year age groups
literacy$band_mid <- 12 + 5 * floor(
  (literacy$age - 10) / 5
)

# Step 6: Identify men and women
literacy$gender <- ifelse(
  literacy$sex == 1, "Men", "Women"
)

# Step 7: Weighted literacy calculations
literacy$weighted_lit <- literacy$literate *
  literacy$hhweight

band_table <- aggregate(
  cbind(weighted_lit, hhweight) ~ band_mid + gender,
  data = literacy,
  FUN = sum
)

# Step 8: Count people in each group
counts <- aggregate(
  literate ~ band_mid + gender,
  data = literacy,
  FUN = length
)

names(counts)[3] <- "n"

band_table <- merge(
  band_table,
  counts,
  by = c("band_mid", "gender")
)

# Step 9: Calculate literacy rates
band_table$rate <- 100 *
  band_table$weighted_lit / band_table$hhweight

# Step 10: Add age-band labels
band_table$age_band <- paste0(
  band_table$band_mid - 2,
  "-",
  band_table$band_mid + 2
)

# Display the results
print(band_table[, c(
  "age_band", "gender", "n", "rate"
)])



# Step 11: Identify groups with fewer than 30 people

band_table$thin <- ifelse(
  band_table$n < 30,
  "Thin",
  "Standard"
)

# Step 12: Find the largest male-female literacy gap

men <- subset(
  band_table,
  gender == "Men",
  select = c(band_mid, age_band, rate)
)

women <- subset(
  band_table,
  gender == "Women",
  select = c(band_mid, rate)
)

gap_table <- merge(
  men,
  women,
  by = "band_mid",
  suffixes = c("_men", "_women")
)

gap_table$gap <- abs(
  gap_table$rate_men - gap_table$rate_women
)

max_gap <- gap_table[
  which.max(gap_table$gap),
]

cat("Largest gap:", max_gap$age_band, "\n")
cat("Gap:", round(max_gap$gap, 1),
    "percentage points\n")

# Step 13: Prepare labels for the line endings

end_labels <- do.call(
  rbind,
  lapply(
    split(band_table, band_table$gender),
    function(d) d[which.max(d$band_mid), , drop = FALSE]
  )
)

# Step 14: Create the improved literacy graph

p_literacy <- ggplot(
  band_table,
  aes(
    x = band_mid,
    y = rate,
    color = gender,
    group = gender
  )
) +
  
  geom_line(linewidth = 1) +
  
  geom_point(
    aes(shape = thin),
    size = 3,
    stroke = 1
  ) +
  
  scale_shape_manual(
    values = c("Standard" = 16, "Thin" = 1)
  ) +
  
  # Mark the largest gap
  geom_segment(
    data = max_gap,
    aes(
      x = band_mid,
      xend = band_mid,
      y = rate_men,
      yend = rate_women
    ),
    inherit.aes = FALSE,
    color = "grey30",
    linetype = "dotted",
    linewidth = 0.8
  ) +
  
  annotate(
    "label",
    x = max_gap$band_mid,
    y = (max_gap$rate_men + max_gap$rate_women) / 2,
    label = paste0(
      "Largest gap: ",
      round(max_gap$gap, 1),
      " pp"
    ),
    size = 3,
    fill = "white"
  ) +
  
  # Label each series directly at its end
  geom_text(
    data = end_labels,
    aes(
      x = band_mid + 1.5,
      y = rate,
      label = gender,
      color = gender
    ),
    inherit.aes = FALSE,
    hjust = 0,
    fontface = "bold",
    size = 4
  ) +
  
  scale_x_continuous(
    breaks = seq(12, 62, by = 5),
    labels = paste0(
      seq(10, 60, by = 5),
      "-",
      seq(14, 64, by = 5)
    ),
    limits = c(11, 75)
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = paste0(
      "Largest gender literacy gap occurs at ages ",
      max_gap$age_band
    ),
    x = "Age group (years)",
    y = "Weighted literacy rate",
    caption = paste(
      "Source: PSLM 2019-20. Rural KP, ages 10-64.",
      "Weighted by hhweight.",
      "Hollow markers indicate fewer than 30 observations."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.caption = element_text(
      size = 8,
      hjust = 0
    )
  )

# Step 15: Display the graph

print(p_literacy)

# Step 16: Save as SVG

ggsave(
  filename = "figures/literacy_by_age.svg",
  plot = p_literacy,
  width = 11,
  height = 7
)

# Step 17: Display required results

print(
  subset(
    band_table,
    age_band %in% c("15-19", "55-59"),
    select = c(age_band, gender, n, rate)
  )
)

cat(
  "Largest gender gap age band:",
  max_gap$age_band,
  "\n"
)
