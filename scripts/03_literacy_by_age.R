
# QUESTION 3 - LITERACY BY AGE AND SEX
# Variant 4: Rural Khyber Pakhtunkhwa

library(ggplot2)

# Step 1: Load the datasets

persons <- read.csv("data/pslm_person_subset.csv")

households <- read.csv("data/pslm_hh_subset (1).csv")

# Step 2: Merge the datasets

stopifnot(!anyDuplicated(households$hhid))

data <- merge(
  persons,
  households[, c(
    "hhid", "hhweight",
    "prov_code", "urban_rural"
  )],
  by = "hhid"
)

# Step 3: Select the literacy sample

literacy_sample <- subset(
  data,
  prov_code == 1 &
    urban_rural == 1 &
    age >= 10 &
    age <= 64 &
    !is.na(age) &
    !is.na(literate) &
    sex %in% c(1, 2)
)

# Check weights before calculating rates

if (any(
  is.na(literacy_sample$hhweight) |
  !is.finite(literacy_sample$hhweight) |
  literacy_sample$hhweight <= 0
)) {
  stop("Invalid household weights found.")
}

# Step 4: Create age bands
# Ages 10-14, 15-19, ... 60-64

literacy_sample$band_start <-
  (literacy_sample$age %/% 5) * 5

literacy_sample$age_mid <-
  literacy_sample$band_start + 2

literacy_sample$sex_name <- factor(
  literacy_sample$sex,
  levels = c(1, 2),
  labels = c("Men", "Women")
)

# Step 5: Calculate weighted literacy rates

literacy_sample$lit_weight <-
  literacy_sample$literate *
  literacy_sample$hhweight

literacy_sample$person_count <- 1

band_table <- aggregate(
  cbind(
    n = person_count,
    total_weight = hhweight,
    literate_weight = lit_weight
  ) ~ band_start + sex_name,
  data = literacy_sample,
  FUN = sum
)

band_table$rate <- 100 *
  band_table$literate_weight /
  band_table$total_weight

band_table$age_mid <- band_table$band_start + 2

band_table$band <- paste0(
  band_table$band_start, "-",
  band_table$band_start + 4
)

# Step 6: Identify thin cells
# Fewer than 30 observations

band_table$marker <- ifelse(
  band_table$n < 30,
  "Thin",
  "Normal"
)

# Step 7: Find the largest literacy gap

men <- subset(
  band_table,
  sex_name == "Men",
  select = c(band_start, rate)
)

women <- subset(
  band_table,
  sex_name == "Women",
  select = c(band_start, rate)
)

gap_table <- merge(
  men,
  women,
  by = "band_start",
  suffixes = c("_men", "_women")
)

gap_table$gap <- abs(
  gap_table$rate_men -
    gap_table$rate_women
)

largest_gap <- gap_table[
  which.max(gap_table$gap),
]

gap_band <- paste0(
  largest_gap$band_start, "-",
  largest_gap$band_start + 4
)

gap_mid <- largest_gap$band_start + 2

# Step 8: Direct labels for end of lines

end_labels <- band_table[
  !duplicated(
    band_table$sex_name,
    fromLast = TRUE
  ),
]

# Order correctly first
band_table <- band_table[
  order(band_table$sex_name,
        band_table$age_mid),
]

end_labels <- do.call(
  rbind,
  lapply(
    split(band_table, band_table$sex_name),
    function(x) x[which.max(x$age_mid), ]
  )
)

# Step 9: Create the literacy graph

p3 <- ggplot(
  band_table,
  aes(
    x = age_mid,
    y = rate,
    color = sex_name,
    group = sex_name
  )
) +
  geom_line(linewidth = 0.9) +
  
  geom_point(
    aes(shape = marker),
    size = 3,
    stroke = 1
  ) +
  
  scale_shape_manual(
    values = c(Normal = 16, Thin = 1),
    guide = "none"
  ) +
  
  scale_color_manual(
    values = c(
      Men = "#315B75",
      Women = "#A45B45"
    ),
    guide = "none"
  ) +
  
  geom_segment(
    inherit.aes = FALSE,
    x = gap_mid,
    xend = gap_mid,
    y = largest_gap$rate_men,
    yend = largest_gap$rate_women,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  annotate(
    "label",
    x = 38,
    y = 96,
    label = paste0(
      "Largest gap: ages ", gap_band,
      " (", round(largest_gap$gap, 1),
      " percentage points)"
    ),
    size = 3.2
  ) +
  
  geom_text(
    data = end_labels,
    aes(
      x = age_mid + 2,
      label = sex_name
    ),
    hjust = 0,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_x_continuous(
    breaks = seq(12, 62, 5),
    labels = paste0(
      seq(10, 60, 5), "-",
      seq(14, 64, 5)
    ),
    limits = c(10, 72)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = paste0(
      "Largest gender literacy gap in rural KP: ages ",
      gap_band
    ),
    x = "Age group",
    y = "Weighted literacy rate",
    caption = paste0(
      "Source: PSLM 2019-20. ",
      "Rural KP residents aged 10-64 ",
      "with reported literacy. ",
      "Weighted by hhweight. ",
      "Hollow markers indicate fewer than ",
      "30 unweighted observations."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      size = 8,
      hjust = 0
    )
  )

# Step 10: Display graph

print(p3)

# Step 11: Save as SVG

ggsave(
  "figures/literacy_by_age.svg",
  plot = p3,
  width = 11,
  height = 7
)

# Step 12: Display the band table

print(
  band_table[, c(
    "band", "sex_name", "n", "rate"
  )],
  row.names = FALSE
)

# Step 13: Report requested age groups

cat("\nLiteracy rates: ages 15-19 and 55-59\n")

print(
  subset(
    band_table,
    band %in% c("15-19", "55-59"),
    select = c(band, sex_name, n, rate)
  )
)

cat("\nLargest gap age band:", gap_band, "\n")

cat(
  "Gap in percentage points:",
  round(largest_gap$gap, 1), "\n"
)

cat("\nQuestion 3 graph saved successfully.\n")


# Create separate tables for men and women

men <- subset(
  band_table,
  sex_name == "Men",
  select = c(band_start, n, rate)
)

women <- subset(
  band_table,
  sex_name == "Women",
  select = c(band_start, n, rate)
)

# Rename the columns
names(men) <- c("band_start", "men_n", "men_rate")
names(women) <- c("band_start", "women_n", "women_rate")

# Join both tables
literacy_table <- merge(
  men, women,
  by = "band_start"
)

# Arrange from youngest to oldest
literacy_table <- literacy_table[
  order(literacy_table$band_start),
]

# Create the final display table
final_table <- data.frame(
  "Age group" = paste0(
    literacy_table$band_start, "-",
    literacy_table$band_start + 4
  ),
  "Men (n)" = literacy_table$men_n,
  "Men literacy (%)" = sprintf("%.1f", literacy_table$men_rate),
  "Women (n)" = literacy_table$women_n,
  "Women literacy (%)" = sprintf("%.1f", literacy_table$women_rate),
  check.names = FALSE
)

# Display the finished table
print(final_table, row.names = FALSE)

# Generate a Markdown table for GitHub README
header <- paste0(
  "| Age group | Men (n) | Men literacy (%) | ",
  "Women (n) | Women literacy (%) |"
)

separator <- "|---|---:|---:|---:|---:|"

rows <- apply(final_table, 1, function(x) {
  paste0("| ", paste(x, collapse = " | "), " |")
})

markdown_table <- paste(
  c(header, separator, rows),
  collapse = "\n"
)

cat(markdown_table)
