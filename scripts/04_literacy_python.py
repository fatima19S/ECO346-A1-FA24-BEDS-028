
# QUESTION 5 - PYTHON VERIFICATION
# ECO346 Assignment 1
# Variant 4: Rural Khyber Pakhtunkhwa

import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path

# -----------------------------------
# STEP 1: FIND PROJECT FOLDER
# -----------------------------------

project = Path(__file__).resolve().parent.parent

# -----------------------------------
# STEP 2: LOAD DATASETS
# -----------------------------------

persons = pd.read_csv(
    project / "data/pslm_person_subset.csv",
    dtype={"hhid": str}
)

household_file = project / "data/pslm_hh_subset (1).csv"

if not household_file.exists():
    household_file = project / "data/pslm_hh_subset.csv"

households = pd.read_csv(
    household_file,
    dtype={"hhid": str}
)

print("Persons:", len(persons))
print("Households:", len(households))

# -----------------------------------
# STEP 3: MERGE DATASETS
# -----------------------------------

data = pd.merge(
    persons,
    households[[
        "hhid", "hhweight",
        "prov_code", "urban_rural"
    ]],
    on="hhid",
    how="inner",
    validate="many_to_one"
)

# Ensure the required variables are numeric
for column in [
    "age", "sex", "literate", "ed_total"
]:
    if column in data.columns:
        data[column] = pd.to_numeric(
            data[column], errors="coerce"
        )

for column in [
    "hhweight", "prov_code", "urban_rural"
]:
    data[column] = pd.to_numeric(
        data[column], errors="coerce"
    )

# -----------------------------------
# STEP 4: SELECT RURAL KP
# -----------------------------------

kp_rural = data[
    (data["prov_code"] == 1) &
    (data["urban_rural"] == 1)
].copy()

# -----------------------------------
# STEP 5: SELECT LITERACY SAMPLE
# -----------------------------------

literacy = kp_rural[
    kp_rural["age"].between(10, 64) &
    kp_rural["literate"].isin([0, 1]) &
    kp_rural["sex"].isin([1, 2]) &
    (kp_rural["hhweight"] > 0) &
    kp_rural["hhweight"].notna()
].copy()

print("Literacy sample:", len(literacy))

# -----------------------------------
# STEP 6: CREATE AGE BANDS
# -----------------------------------

literacy["band_mid"] = (
    12 + 5 * ((literacy["age"] - 10) // 5)
).astype(int)

literacy["gender"] = literacy["sex"].map({
    1: "Men",
    2: "Women"
})

# -----------------------------------
# STEP 7: CALCULATE WEIGHTED LITERACY
# -----------------------------------

literacy["weighted_lit"] = (
    literacy["literate"] *
    literacy["hhweight"]
)

# -----------------------------------
# STEP 8: CREATE BAND TABLE
# -----------------------------------

band_table = literacy.groupby(
    ["band_mid", "gender"],
    as_index=False
).agg(
    n=("literate", "size"),
    weighted_lit=("weighted_lit", "sum"),
    total_weight=("hhweight", "sum")
)

# Calculate weighted literacy percentage
band_table["rate"] = (
    100 * band_table["weighted_lit"] /
    band_table["total_weight"]
)

# Add age-group labels
band_table["age_band"] = (
    (band_table["band_mid"] - 2).astype(str)
    + "-"
    + (band_table["band_mid"] + 2).astype(str)
)

# -----------------------------------
# STEP 9: DISPLAY RESULTS
# -----------------------------------

print("\nPYTHON LITERACY TABLE\n")

print(
    band_table[
        ["age_band", "gender", "n", "rate"]
    ].round({"rate": 1}).to_string(index=False)
)

# -----------------------------------
# STEP 10: FIND LARGEST GENDER GAP
# -----------------------------------

gap_table = band_table.pivot(
    index="band_mid",
    columns="gender",
    values="rate"
).dropna()

gap_table["gap"] = abs(
    gap_table["Men"] - gap_table["Women"]
)

gap_mid = int(gap_table["gap"].idxmax())
largest_gap = gap_table.loc[gap_mid, "gap"]

gap_band = f"{gap_mid - 2}-{gap_mid + 2}"

print("\nLargest gender gap:", gap_band)
print("Gap:", round(largest_gap, 1),
      "percentage points")

# -----------------------------------
# STEP 11: CHECK WOMEN AGED 15-19
# -----------------------------------

women_15_19 = band_table[
    (band_table["band_mid"] == 17) &
    (band_table["gender"] == "Women")
]

r = women_15_19["rate"].iloc[0]

print("\nWomen aged 15-19:")
print(f"Python literacy rate: {r:.1f}%")

# -----------------------------------
# STEP 12: CREATE LITERACY GRAPH
# -----------------------------------

fig, ax = plt.subplots(figsize=(11, 7))

colors = {
    "Men": "#315d78",
    "Women": "#b3664f"
}

for gender in ["Men", "Women"]:

    group = band_table[
        band_table["gender"] == gender
    ].sort_values("band_mid")

    color = colors[gender]

    # Draw connecting line
    ax.plot(
        group["band_mid"],
        group["rate"],
        color=color,
        linewidth=2
    )

    # Solid markers for normal groups
    normal = group[group["n"] >= 30]

    ax.scatter(
        normal["band_mid"],
        normal["rate"],
        color=color,
        s=48,
        zorder=3
    )

    # Hollow markers for thin groups
    thin = group[group["n"] < 30]

    ax.scatter(
        thin["band_mid"],
        thin["rate"],
        facecolors="none",
        edgecolors=color,
        s=55,
        linewidths=1.5,
        zorder=4
    )

    # Label each line directly
    last = group.iloc[-1]

    ax.text(
        last["band_mid"] + 1.5,
        last["rate"],
        gender,
        color=color,
        fontsize=11,
        fontweight="bold",
        va="center"
    )

# -----------------------------------
# STEP 13: ANNOTATE LARGEST GAP
# -----------------------------------

male_rate = gap_table.loc[gap_mid, "Men"]
female_rate = gap_table.loc[gap_mid, "Women"]

ax.vlines(
    gap_mid,
    female_rate,
    male_rate,
    colors="black",
    linestyles="dashed",
    linewidth=1.2
)

ax.text(
    0.5,
    0.94,
    f"Largest gap: ages {gap_band} "
    f"({largest_gap:.1f} percentage points)",
    transform=ax.transAxes,
    ha="center",
    va="center",
    fontsize=10,
    bbox=dict(
        facecolor="white",
        edgecolor="gray",
        boxstyle="round,pad=0.3"
    )
)

# -----------------------------------
# STEP 14: FORMAT GRAPH
# -----------------------------------

midpoints = list(range(12, 63, 5))

age_labels = [
    f"{mid-2}-{mid+2}"
    for mid in midpoints
]

ax.set_xticks(midpoints)
ax.set_xticklabels(
    age_labels,
    rotation=45,
    ha="right"
)

ax.set_xlim(10, 73)
ax.set_ylim(0, 100)

ax.set_yticks(range(0, 101, 20))
ax.set_yticklabels([
    f"{v}%" for v in range(0, 101, 20)
])

ax.set_xlabel("Age group")
ax.set_ylabel("Weighted literacy rate")

ax.set_title(
    f"Largest gender literacy gap in rural KP: "
    f"ages {gap_band}",
    fontsize=14,
    fontweight="bold",
    pad=18
)

ax.grid(
    True,
    alpha=0.25
)

ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)

# Source and sample description
fig.text(
    0.1,
    0.035,
    "Source: PSLM 2019-20. Rural KP residents "
    "aged 10-64 with reported literacy. "
    "Weighted by hhweight. "
    "Hollow markers indicate fewer than "
    "30 observations.",
    fontsize=8
)

fig.subplots_adjust(
    left=0.10,
    right=0.91,
    top=0.88,
    bottom=0.20
)

# -----------------------------------
# STEP 15: SAVE GRAPH AS SVG
# -----------------------------------

output = project / "figures/literacy_by_age_python.svg"

output.parent.mkdir(
    parents=True,
    exist_ok=True
)

fig.savefig(
    output,
    format="svg"
)

print("\nGraph saved successfully!")
print(output)

plt.show()
