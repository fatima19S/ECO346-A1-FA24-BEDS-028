
# QUESTION 4 - DEFAULT LITERACY GRAPH
# Variant 4: Rural Khyber Pakhtunkhwa

# Step 1: Load the graph package
library(ggplot2)

# Step 2: Load the datasets
persons <- read.csv("data/pslm_person_subset.csv")

households <- read.csv("data/pslm_hh_subset (1).csv")

# Step 3: Merge the datasets
data <- merge(
  persons,
  households[, c(
    "hhid",
    "hhweight",
    "prov_code",
    "urban_rural"
  )],
  by = "hhid"
)

# Step 4: Select rural Khyber Pakhtunkhwa
kp_rural <- subset(
  data,
  prov_code == 1 & urban_rural == 1
)

# Step 5: Select the literacy sample
literacy <- subset(
  kp_rural,
  !is.na(age) &
    age >= 10 &
    age <= 64 &
    literate %in% c(0, 1) &
    sex %in% c(1, 2) &
    !is.na(hhweight) &
    hhweight > 0
)

# Step 6: Check the sample
nrow(literacy)

# Step 7: Create the default scatterplot
# No additional styling or customization

default_plot <- ggplot(
  literacy,
  aes(x = age, y = literate)
) +
  geom_point()

# Step 8: Display the default graph
print(default_plot)

# Step 9: Save the graph as SVG
ggsave(
  filename = "figures/default.svg",
  plot = default_plot,
  width = 9,
  height = 6
)

# Step 10: Check whether the graph was saved
file.exists("figures/default.svg")
