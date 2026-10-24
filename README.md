# FA24-BED-028
## Question 3: Literacy by Age and Sex.


| Age group | Men (n) | Men literacy (%) | Women (n) | Women literacy (%) |
|---|---:|---:|---:|---:|
| 10-14 | 1439 | 81.5 | 1210 | 54.6 |
| 15-19 | 1162 | 85.1 |  985 | 52.6 |
| 20-24 |  768 | 79.5 |  906 | 33.3 |
| 25-29 |  580 | 70.6 |  826 | 30.0 |
| 30-34 |  499 | 68.4 |  588 | 24.8 |
| 35-39 |  421 | 67.5 |  546 | 21.0 |
| 40-44 |  357 | 60.1 |  449 | 13.6 |
| 45-49 |  385 | 57.0 |  392 | 10.5 |
| 50-54 |  278 | 45.1 |  384 | 6.7 |
| 55-59 |  255 | 43.0 |  308 | 2.9 |
| 60-64 |  272 | 39.2 |  190 | 3.6 |




### Interpretation

Literacy rates differ considerably between men and women in rural Khyber Pakhtunkhwa. Among people aged 15–19, male literacy is 85.1%, compared with 52.6% for females. The rates are lower among older age groups, especially women. The largest gender gap occurs at ages 45–49, reaching 46.6 percentage points. These differences may reflect unequal educational opportunities across generations. However, the graph cannot establish the causes of these inequalities.

### Question 4: Graph Makeover

The default scatterplot shows individual literacy observations against age, but many points overlap because literacy has only two values, 0 and 1. This makes it difficult to identify clear patterns.

I improved the graph in three main ways. First, I used weighted conditional means for five-year age groups to reduce overplotting and make literacy trends easier to understand. Second, I used separate lines for men and women, allowing gender differences to be compared using position on a common scale. Third, I added direct labels and highlighted the largest gender gap, improving visual clarity and reducing unnecessary data-ink. Hollow markers also identify groups with fewer observations.

However, the redesigned graph hides individual literacy outcomes and differences within each age group because it presents average literacy rates rather than individual observations.


### Question 5: R and Python Comparison

Both R and Python produced identical weighted literacy rates to one decimal place. For women aged 15–19, the rate was 52.6% in both programs. One difference was the default axis formatting in ggplot2 and Matplotlib. To ensure consistency, I manually set the vertical axis from 0% to 100%, used the same five-year age groups, and applied identical household survey weights. This allowed the graphs to present comparable results.