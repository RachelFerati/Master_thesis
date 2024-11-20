Beyond Stories: Exploring the Influence of Emotional Indicators on the
Subjective Quality Rating of Stories
================
Rachel Ferati
2024-11-20

-   <a href="#results-dataset" id="toc-results-dataset">Results dataset</a>
    -   <a href="#1-data-organization" id="toc-1-data-organization">1. Data
        organization</a>
    -   <a href="#2-mean-and-sd-of-the-dataset"
        id="toc-2-mean-and-sd-of-the-dataset">2. Mean and SD of the dataset</a>
    -   <a href="#3-comparisons" id="toc-3-comparisons">3. Comparisons</a>
    -   <a href="#4-correlation-table" id="toc-4-correlation-table">4.
        Correlation table</a>
    -   <a href="#5-distribution" id="toc-5-distribution">5. Distribution</a>
    -   <a href="#6mann-whitney-u-test"
        id="toc-6mann-whitney-u-test">6.Mann-Whitney U Test</a>
    -   <a href="#7-linear-mixed_effects-model"
        id="toc-7-linear-mixed_effects-model">7. Linear Mixed_effects Model</a>
    -   <a href="#8-common-story" id="toc-8-common-story">8. Common story</a>

# Results dataset

#### Libraries

``` r
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(tinytex)
library(ggcorrplot)
library(corrplot)
library(Hmisc)
library(tidyverse)
library(lme4)
library(psych)
library(sjPlot)
```

#### Set options

## 1. Data organization

### Importation and cleaning

#### Import dataset from Qualtrics

``` r
Data_raw <- read_csv("Data_raw.csv")
View(Data_raw)
```

#### Remove 2 first lines of the dataset

``` r
Data_clear_large <- Data_raw[-c(1, 2), ]
View(Data_clear_large)
```

#### Add Category (high or low) to the large table

#### Define prefixes for high and low stories

``` r
high_prefixes <- c("BEJEDI08", "MAANAU12", "SADJSO02", "SAERBE09")
low_prefixes <- c("VEYVEM11", "CLCHJO01", "ANJETI02", "COHEIG03")

Data_clear_large <- Data_clear_large %>%
  rename_with(~ paste0("high_",.), starts_with(high_prefixes)) %>% 
  rename_with(~ paste0("low_",.), starts_with(low_prefixes))

head(Data_clear_large)
```

    ## # A tibble: 6 × 84
    ##   StartDate EndDate Status Progress Duration (in seconds…¹ Finished RecordedDate
    ##   <chr>     <chr>   <chr>  <chr>    <chr>                  <chr>    <chr>       
    ## 1 2024-09-… 2024-0… 0      100      2104                   1        2024-09-17 …
    ## 2 2024-09-… 2024-0… 0      100      1081                   1        2024-09-17 …
    ## 3 2024-09-… 2024-0… 0      100      4061                   1        2024-09-18 …
    ## 4 2024-09-… 2024-0… 0      100      915                    1        2024-09-23 …
    ## 5 2024-09-… 2024-0… 0      100      2786                   1        2024-09-23 …
    ## 6 2024-09-… 2024-0… 0      100      1269                   1        2024-09-24 …
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 77 more variables: ResponseId <chr>, DistributionChannel <chr>,
    ## #   UserLanguage <chr>, Q_RecaptchaScore <chr>, Age <chr>, Genre <chr>,
    ## #   Education <chr>, Groupe <chr>, high_BEJEDI08_1 <chr>,
    ## #   high_BEJEDI08_L_1 <chr>, high_BEJEDI08_L_2 <chr>, high_BEJEDI08_L_3 <chr>,
    ## #   high_BEJEDI08_L_4 <chr>, high_BEJEDI08_L_5 <chr>, high_MAANAU12_1 <chr>,
    ## #   high_MAANAU12_L_1 <chr>, high_MAANAU12_L_2 <chr>, …

``` r
View(Data_clear_large)
```

#### Define prefixes and suffixes for high and low stories for exploratory questions

``` r
high_exploratory_prefixes <- c("high_BEJEDI08_L", "high_MAANAU12_L", "high_SADJSO02_L", "high_SAERBE09_L")
low_exploratory_prefixes <- c("low_VEYVEM11_L", "low_CLCHJO01_L", "low_ANJETI02_L", "low_COHEIG03_L")

Data_clear_large <- Data_clear_large %>%
  rename_with(~ paste0("exp_", .), starts_with(high_exploratory_prefixes)) %>% 
  rename_with(~ paste0("exp_", .), starts_with(low_exploratory_prefixes))

head(Data_clear_large)
```

    ## # A tibble: 6 × 84
    ##   StartDate EndDate Status Progress Duration (in seconds…¹ Finished RecordedDate
    ##   <chr>     <chr>   <chr>  <chr>    <chr>                  <chr>    <chr>       
    ## 1 2024-09-… 2024-0… 0      100      2104                   1        2024-09-17 …
    ## 2 2024-09-… 2024-0… 0      100      1081                   1        2024-09-17 …
    ## 3 2024-09-… 2024-0… 0      100      4061                   1        2024-09-18 …
    ## 4 2024-09-… 2024-0… 0      100      915                    1        2024-09-23 …
    ## 5 2024-09-… 2024-0… 0      100      2786                   1        2024-09-23 …
    ## 6 2024-09-… 2024-0… 0      100      1269                   1        2024-09-24 …
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 77 more variables: ResponseId <chr>, DistributionChannel <chr>,
    ## #   UserLanguage <chr>, Q_RecaptchaScore <chr>, Age <chr>, Genre <chr>,
    ## #   Education <chr>, Groupe <chr>, high_BEJEDI08_1 <chr>,
    ## #   exp_high_BEJEDI08_L_1 <chr>, exp_high_BEJEDI08_L_2 <chr>,
    ## #   exp_high_BEJEDI08_L_3 <chr>, exp_high_BEJEDI08_L_4 <chr>,
    ## #   exp_high_BEJEDI08_L_5 <chr>, high_MAANAU12_1 <chr>, …

``` r
View(Data_clear_large)
```

``` r
high_exploratory_prefixes <- c("exp_high_BEJEDI08_L", "exp_high_MAANAU12_L", "exp_high_SADJSO02_L", "exp_high_SAERBE09_L")
low_exploratory_prefixes <- c("exp_low_VEYVEM11_L", "exp_low_CLCHJO01_L", "exp_low_ANJETI02_L", "exp_low_COHEIG03_L")

Data_clear_large <- Data_clear_large %>%
  rename_with(~ paste0(.,"_high"), starts_with(high_exploratory_prefixes)) %>% 
  rename_with(~ paste0(.,"_low"), starts_with(low_exploratory_prefixes))

head(Data_clear_large)
```

    ## # A tibble: 6 × 84
    ##   StartDate EndDate Status Progress Duration (in seconds…¹ Finished RecordedDate
    ##   <chr>     <chr>   <chr>  <chr>    <chr>                  <chr>    <chr>       
    ## 1 2024-09-… 2024-0… 0      100      2104                   1        2024-09-17 …
    ## 2 2024-09-… 2024-0… 0      100      1081                   1        2024-09-17 …
    ## 3 2024-09-… 2024-0… 0      100      4061                   1        2024-09-18 …
    ## 4 2024-09-… 2024-0… 0      100      915                    1        2024-09-23 …
    ## 5 2024-09-… 2024-0… 0      100      2786                   1        2024-09-23 …
    ## 6 2024-09-… 2024-0… 0      100      1269                   1        2024-09-24 …
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 77 more variables: ResponseId <chr>, DistributionChannel <chr>,
    ## #   UserLanguage <chr>, Q_RecaptchaScore <chr>, Age <chr>, Genre <chr>,
    ## #   Education <chr>, Groupe <chr>, high_BEJEDI08_1 <chr>,
    ## #   exp_high_BEJEDI08_L_1_high <chr>, exp_high_BEJEDI08_L_2_high <chr>,
    ## #   exp_high_BEJEDI08_L_3_high <chr>, exp_high_BEJEDI08_L_4_high <chr>,
    ## #   exp_high_BEJEDI08_L_5_high <chr>, high_MAANAU12_1 <chr>, …

``` r
View(Data_clear_large)
```

------------------------------------------------------------------------

### Long table

#### Convert from wide to long in Data_clear based on high and low stories

``` r
Data_clear_long <- pivot_longer(
  Data_clear_large, 
  cols = starts_with(c("high_", "low_")),
  names_to = "Stories",
  values_to = "Grades"
)
```

#### Move “Stories” and “Grades” to the first two columns

``` r
Data_clear_long <- Data_clear_long %>%
  select(Stories, Grades, everything())

print(Data_clear_long)
```

    ## # A tibble: 208 × 78
    ##    Stories       Grades StartDate EndDate Status Progress Duration (in seconds…¹
    ##    <chr>         <chr>  <chr>     <chr>   <chr>  <chr>    <chr>                 
    ##  1 high_BEJEDI0… <NA>   2024-09-… 2024-0… 0      100      2104                  
    ##  2 high_MAANAU1… <NA>   2024-09-… 2024-0… 0      100      2104                  
    ##  3 high_SAERBE0… 5      2024-09-… 2024-0… 0      100      2104                  
    ##  4 high_SADJSO0… 5      2024-09-… 2024-0… 0      100      2104                  
    ##  5 low_ANJETI02… <NA>   2024-09-… 2024-0… 0      100      2104                  
    ##  6 low_CLCHJO01… <NA>   2024-09-… 2024-0… 0      100      2104                  
    ##  7 low_COHEIG03… 5      2024-09-… 2024-0… 0      100      2104                  
    ##  8 low_VEYVEM11… 5      2024-09-… 2024-0… 0      100      2104                  
    ##  9 high_BEJEDI0… <NA>   2024-09-… 2024-0… 0      100      1081                  
    ## 10 high_MAANAU1… <NA>   2024-09-… 2024-0… 0      100      1081                  
    ## # ℹ 198 more rows
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 71 more variables: Finished <chr>, RecordedDate <chr>, ResponseId <chr>,
    ## #   DistributionChannel <chr>, UserLanguage <chr>, Q_RecaptchaScore <chr>,
    ## #   Age <chr>, Genre <chr>, Education <chr>, Groupe <chr>,
    ## #   exp_high_BEJEDI08_L_1_high <chr>, exp_high_BEJEDI08_L_2_high <chr>,
    ## #   exp_high_BEJEDI08_L_3_high <chr>, exp_high_BEJEDI08_L_4_high <chr>, …

``` r
View(Data_clear_long)
```

#### Add the column “Category” to the dataset

``` r
Data_clear_long <- Data_clear_long %>%
  mutate(
    Category = case_when(
      grepl("^(high_BEJEDI08|high_MAANAU12|high_SADJSO02|high_SAERBE09)", Stories) ~ "high",
      grepl("^(low_VEYVEM11|low_CLCHJO01|low_ANJETI02|low_COHEIG03)", Stories) ~ "low",
    )
  )%>%
  select(1:2, Category, everything())  # Moves Category to the 3rd position

Data_clear_long$Category<- as.factor(Data_clear_long$Category)

View(Data_clear_long)
```

------------------------------------------------------------------------

### Exclusion of participants

``` r
Data_clear_large <- Data_clear_large %>%
  filter(Attention == 3)
```

``` r
Data_clear_long <- Data_clear_long %>%
  filter(Attention == 3)
```

## 2. Mean and SD of the dataset

#### Convert columns in Data_clear_large to numeric

``` r
Data_clear_large[, 12:84] <- lapply(Data_clear_large[, 12:84], function(x) as.numeric(as.character(x)))
```

------------------------------------------------------------------------

### AGE

``` r
summary(Data_clear_large$Age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   18.00   23.25   29.00   32.82   37.50   63.00

``` r
sum(is.na(Data_clear_large$Age))
```

    ## [1] 0

#### Calculate Mean and SD for AGE

``` r
mean_age <- mean(Data_clear_large$Age, na.rm = TRUE)
sd_age <- sd(Data_clear_large$Age, na.rm = TRUE)

cat("Mean Age:", mean_age, "\n")
```

    ## Mean Age: 32.81818

``` r
cat("Standard Deviation of Age:", sd_age, "\n")
```

    ## Standard Deviation of Age: 12.80118

#### Frequency distribution of Age

``` r
age_distribution <- table(Data_clear_large$Age)
print(age_distribution)
```

    ## 
    ## 18 20 23 24 27 31 33 39 42 45 56 59 63 
    ##  1  1  4  2  3  3  2  1  1  1  1  1  1

#### Percentage of Age

``` r
frequency_table_age <- table(Data_clear_large$Age)

percentage_table_age <- prop.table(frequency_table_age) * 100

percentage_age_df <- as.data.frame(percentage_table_age)
colnames(percentage_age_df) <- c("Age", "Percentage")

print(percentage_age_df)
```

    ##    Age Percentage
    ## 1   18   4.545455
    ## 2   20   4.545455
    ## 3   23  18.181818
    ## 4   24   9.090909
    ## 5   27  13.636364
    ## 6   31  13.636364
    ## 7   33   9.090909
    ## 8   39   4.545455
    ## 9   42   4.545455
    ## 10  45   4.545455
    ## 11  56   4.545455
    ## 12  59   4.545455
    ## 13  63   4.545455

#### Visual of Age distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Number of participants") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

``` r
ggplot(Data_clear_large, aes(x = Age)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Age Density Distribution", x = "Age", y = "Density") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### GENDER

``` r
summary(Data_clear_large$Genre)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   1.591   2.000   3.000

``` r
sum(is.na(Data_clear_large$Genre))
```

    ## [1] 0

#### Calculate Mean and SD of Genre

``` r
mean_genre <- mean(Data_clear_large$Genre, na.rm = TRUE)
sd_genre <- sd(Data_clear_large$Genre, na.rm = TRUE)

cat("Mean Genre:", mean_genre, "\n")
```

    ## Mean Genre: 1.590909

``` r
cat("Standard Deviation of Genre:", sd_genre, "\n")
```

    ## Standard Deviation of Genre: 0.5903261

#### Frequency distribution of Genre

``` r
genre_distribution <- table(Data_clear_large$Genre)

print(genre_distribution)
```

    ## 
    ##  1  2  3 
    ## 10 11  1

#### Percentage of Gender

``` r
frequency_table_gender <- table(Data_clear_large$Genre)

percentage_table_gender <- prop.table(frequency_table_gender) * 100

percentage_gender_df <- as.data.frame(percentage_table_gender)
colnames(percentage_gender_df) <- c("Gender", "Percentage")

print(percentage_gender_df)
```

    ##   Gender Percentage
    ## 1      1  45.454545
    ## 2      2  50.000000
    ## 3      3   4.545455

#### Visual of Gender distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Genre), y = (..count..)/sum(..count..), fill = factor(Genre))) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("1" = "lightgoldenrod1", "2" = "lightblue1", "3" = "lightsalmon"),
                    labels = c("Female", "Male", "Non-binary")) + 
  labs(title = "Gender Distribution", x = "Gender", y = "Percentage") +
  scale_x_discrete(labels = c("1" = "Female", "2" = "Male", "3" = "Non-binary")) +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### EDUCATION

``` r
summary(Data_clear_large$Education)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.000   3.000   5.000   5.136   7.000   8.000

#### Calculate Mean and SD of Education

``` r
mean_education <- mean(Data_clear_large$Education, na.rm = TRUE)
sd_education <- sd(Data_clear_large$Education, na.rm = TRUE)

cat("Mean Education:", mean_education, "\n")
```

    ## Mean Education: 5.136364

``` r
cat("Standard Deviation of Education:", sd_education, "\n")
```

    ## Standard Deviation of Education: 2.14466

#### Frequency distribution of Education

``` r
education_distribution <- table(Data_clear_large$Education)

print(education_distribution)
```

    ## 
    ## 2 3 5 7 8 
    ## 3 5 4 8 2

#### Percentage of Education

``` r
frequency_table_education <- table(Data_clear_large$Education)

percentage_table_education <- prop.table(frequency_table_education) * 100

percentage_education_df <- as.data.frame(percentage_table_education)
colnames(percentage_education_df) <- c("Level of Education", "Percentage")

print(percentage_education_df)
```

    ##   Level of Education Percentage
    ## 1                  2  13.636364
    ## 2                  3  22.727273
    ## 3                  5  18.181818
    ## 4                  7  36.363636
    ## 5                  8   9.090909

#### Visual of Education distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Education), fill = factor(Education))) +
  geom_bar() +
  scale_y_continuous(breaks = seq(1, 8, by = 1), limits = c(0, 8)) +
  scale_fill_manual(values = c("1" = "lightgoldenrod1", "2" = "lightblue1", "3" = "brown3", "4" = "red", "5" = "orange", "6" = "blue", "7" = "lightgoldenrod1", "8" = "lightpink"),
                    labels = c("High school", "Professional","Bachelor", "Master", "Else")) +
  scale_x_discrete(labels = c("1" = "Secondary", "2" = "High school", "3" = "Professional", "4" = "Federal", "5" = "Bachelor", "7" = "Master", "8" = "Else")) +
  labs(title = "Education Level Distribution", x = "Education Level", y = "Number of participants", fill = "Education") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-28-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### SIMILAR LIVING EXPERIENCE (Vie)

``` r
summary(Data_clear_large$Vie)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   1.000   1.364   2.000   2.000

#### Calculate Mean and SD of Vie

``` r
mean_vie <- mean(Data_clear_large$Vie, na.rm = TRUE)
sd_vie <- sd(Data_clear_large$Vie, na.rm = TRUE)

cat("Mean Vie:", mean_vie, "\n")
```

    ## Mean Vie: 1.363636

``` r
cat("Standard Deviation of Vie:", sd_vie, "\n")
```

    ## Standard Deviation of Vie: 0.492366

#### Frequency distribution of Vie

``` r
vie_distribution <- table(Data_clear_large$Vie)

print(vie_distribution)
```

    ## 
    ##  1  2 
    ## 14  8

#### Percentage of Similar living experience

``` r
frequency_table_vie <- table(Data_clear_large$Vie)

percentage_table_vie <- prop.table(frequency_table_vie) * 100

percentage_vie_df <- as.data.frame(percentage_table_vie)
colnames(percentage_vie_df) <- c("Similar living experience", "Percentage")

print(percentage_vie_df)
```

    ##   Similar living experience Percentage
    ## 1                         1   63.63636
    ## 2                         2   36.36364

#### Visual of Vie distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Vie), fill = factor(Vie))) +
  geom_bar() +
  scale_fill_manual(values = c("1" = "lightcoral", "2" = "lightblue1"),
                    labels = c("Yes", "No")) +
  labs(title = "Similar Life Experience Distribution", x = "Vie", y = "Number of participants") +
  scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-33-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### PARTICIPATION

``` r
summary(Data_clear_large$Participation)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   1.682   2.000   2.000

#### Calculate Mean and SD of Participation

``` r
mean_participation <- mean(Data_clear_large$Participation, na.rm = TRUE)
sd_participation <- sd(Data_clear_large$Participation, na.rm = TRUE)

cat("Mean Participation:", mean_participation, "\n")
```

    ## Mean Participation: 1.681818

``` r
cat("Standard Deviation of Participation:", sd_participation, "\n")
```

    ## Standard Deviation of Participation: 0.4767313

#### Frequency distribution of Participation

``` r
participation_distribution <- table(Data_clear_large$Participation)

print(participation_distribution)
```

    ## 
    ##  1  2 
    ##  7 15

#### Percentage of participation in the previous experiment

``` r
frequency_table_participation <- table(Data_clear_large$Participation)

percentage_table_participation <- prop.table(frequency_table_participation) * 100

percentage_participation_df <- as.data.frame(percentage_table_participation)
colnames(percentage_participation_df) <- c("Participation in previous experiment", "Percentage")

print(percentage_participation_df)
```

    ##   Participation in previous experiment Percentage
    ## 1                                    1   31.81818
    ## 2                                    2   68.18182

#### Visual of Participation distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Participation), fill = factor(Participation))) +
  geom_bar() +
  scale_fill_manual(values = c("1" = "lightcoral", "2" = "lightblue1"),
                    labels = c("Yes", "No")) +
  labs(title = "Previous Participation Distribution", x = "Participation", y = "Number of participants") +
  scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-38-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### HABITS

#### Habits - BOOKS

``` r
summary(Data_clear_large$Habitudes_1)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.500   3.818   5.750   6.000

#### Calculate mean and SD of Habits for books

``` r
mean_habits_books <- mean(Data_clear_large$Habitudes_1, na.rm = TRUE)
sd_habits_books <- sd(Data_clear_large$Habitudes_1, na.rm = TRUE)

cat("Mean Books habits:", mean_habits_books, "\n")
```

    ## Mean Books habits: 3.818182

``` r
cat("Standard Deviation of Books habits:", sd_habits_books, "\n")
```

    ## Standard Deviation of Books habits: 1.735796

#### Frequency distribution habits of books

``` r
habits_books_distribution <- table(Data_clear_large$Habitudes_1)

print(habits_books_distribution)
```

    ## 
    ## 1 2 3 4 5 6 
    ## 1 6 4 2 3 6

#### Percentage of books habits

``` r
frequency_table_books <- table(Data_clear_large$Habitudes_1)

percentage_table_books <- prop.table(frequency_table_books) * 100

percentage_books_df <- as.data.frame(percentage_table_books)
colnames(percentage_books_df) <- c("Grade", "Percentage")

print(percentage_books_df)
```

    ##   Grade Percentage
    ## 1     1   4.545455
    ## 2     2  27.272727
    ## 3     3  18.181818
    ## 4     4   9.090909
    ## 5     5  13.636364
    ## 6     6  27.272727

#### Visual of Books habits distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Habitudes_1), fill = factor(Habitudes_1))) +
  geom_bar() +
  scale_fill_manual(values = c("1" = "lightgoldenrod1", "2" = "lightblue1", "3" = "brown3", "4" = "lightseagreen", "5" = "orange", "6" = "lightpink"),
                    labels = c("Never", "Once a month", "Multiple times a month", "Once a week", "Multiple times a week", "Everyday")) +
  scale_x_discrete(labels = c("1" = "Never", "2" = "Once a month", "3" = "Multiple times a month", "4" = "Once a week", "5" = "Multiple times a week", "6" = "Everyday")) +
  labs(title = "Frequency of Reading Habits Grades", x = "Book habits grade", y = "Number of participants") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-43-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

#### Habits - AUDIOBOOKS

``` r
summary(Data_clear_large$Habitudes_2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   2.318   3.000   5.000

#### Calculate mean and SD of Habits for audiobooks/podcasts

``` r
mean_habits_audiobooks <- mean(Data_clear_large$Habitudes_2, na.rm = TRUE)
sd_habits_audiobooks <- sd(Data_clear_large$Habitudes_2, na.rm = TRUE)

cat("Mean Audiobooks habits:", mean_habits_audiobooks, "\n")
```

    ## Mean Audiobooks habits: 2.318182

``` r
cat("Standard Deviation of Audiobooks habits:", sd_habits_audiobooks, "\n")
```

    ## Standard Deviation of Audiobooks habits: 1.427164

#### Frequency distribution habits of audiobooks

``` r
habits_audiobooks_distribution <- table(Data_clear_large$Habitudes_2)

print(habits_audiobooks_distribution)
```

    ## 
    ## 1 2 3 4 5 
    ## 8 7 2 2 3

#### Percentage of audiobooks habits

``` r
frequency_table_audiobooks <- table(Data_clear_large$Habitudes_2)

percentage_table_audiobooks <- prop.table(frequency_table_audiobooks) * 100

percentage_audiobooks_df <- as.data.frame(percentage_table_audiobooks)
colnames(percentage_audiobooks_df) <- c("Grade", "Percentage")

print(percentage_audiobooks_df)
```

    ##   Grade Percentage
    ## 1     1  36.363636
    ## 2     2  31.818182
    ## 3     3   9.090909
    ## 4     4   9.090909
    ## 5     5  13.636364

#### Visual of audiobooks habits distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Habitudes_2), fill = factor(Habitudes_2))) +
  geom_bar() +
    scale_fill_manual(values = c("1" = "lightgoldenrod1", "2" = "lightblue1", "3" = "brown3", "4" = "lightseagreen", "5" = "orange", "6" = "lightpink"),
                    labels = c("Never", "Once a month", "Multiple times a month", "Once a week", "Multiple times a week", "Everyday")) +
  scale_x_discrete(labels = c("1" = "Never", "2" = "Once a month", "3" = "Multiple times a month", "4" = "Once a week", "5" = "Multiple times a week", "6" = "Everyday")) +
  labs(title = "Frequency of Audiobooks/podcasts Habits Grades", x = "Audioooks/podcasts habits grade", y = "Number of participants") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-48-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

#### Habits - TELEVISION

``` r
summary(Data_clear_large$Habitudes_3)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   3.000   4.000   5.000   4.818   5.000   6.000

#### Calculate mean and SD of Habits for television

``` r
mean_habits_television <- mean(Data_clear_large$Habitudes_3, na.rm = TRUE)
sd_habits_television <- sd(Data_clear_large$Habitudes_3, na.rm = TRUE)

cat("Mean television habits:", mean_habits_television, "\n")
```

    ## Mean television habits: 4.818182

``` r
cat("Standard Deviation of television habits:", sd_habits_television, "\n")
```

    ## Standard Deviation of television habits: 0.7950061

#### Frequency distribution habits of television

``` r
habits_television_distribution <- table(Data_clear_large$Habitudes_3)

print(habits_television_distribution)
```

    ## 
    ##  3  4  5  6 
    ##  1  6 11  4

#### Percentage of television habits

``` r
frequency_table_television <- table(Data_clear_large$Habitudes_3)

percentage_table_television <- prop.table(frequency_table_television) * 100

percentage_television_df <- as.data.frame(percentage_table_television)
colnames(percentage_television_df) <- c("Grade", "Percentage")

print(percentage_television_df)
```

    ##   Grade Percentage
    ## 1     3   4.545455
    ## 2     4  27.272727
    ## 3     5  50.000000
    ## 4     6  18.181818

#### Visual of television habits distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Habitudes_3), fill = factor(Habitudes_3))) +
  geom_bar() +
  scale_fill_manual(values = c("3" = "brown3", "4" = "lightseagreen", "5" = "orange", "6" = "lightpink"),
                    labels = c("Multiple times a month", "Once a week", "Multiple times a week", "Everyday")) +
  scale_x_discrete(labels = c("1" = "Never", "2" = "Once a month", "3" = "Multiple times a month", "4" = "Once a week", "5" = "Multiple times a week", "6" = "Everyday")) +
  labs(title = "Frequency of Watching Television Habits Grades", x = "Television habits grade", y = "Nb of participants") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-53-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

#### Habits - SOCIALMEDIA

``` r
summary(Data_clear_large$Habitudes_4)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.000   5.000   5.000   5.045   6.000   6.000

#### Calculate mean and SD of Habits for social media

``` r
mean_habits_socialmedia <- mean(Data_clear_large$Habitudes_4, na.rm = TRUE)
sd_habits_socialmedia <- sd(Data_clear_large$Habitudes_4, na.rm = TRUE)

cat("Mean socialmedia habits:", mean_habits_socialmedia, "\n")
```

    ## Mean socialmedia habits: 5.045455

``` r
cat("Standard Deviation of socialmedia habits:", sd_habits_socialmedia, "\n")
```

    ## Standard Deviation of socialmedia habits: 1.214095

#### Frequency distribution habits of television

``` r
habits_socialmedia_distribution <- table(Data_clear_large$Habitudes_4)

print(habits_socialmedia_distribution)
```

    ## 
    ##  2  3  5  6 
    ##  1  3  8 10

#### Percentage of television habits

``` r
frequency_table_socialmedia <- table(Data_clear_large$Habitudes_4)

percentage_table_socialmedia <- prop.table(frequency_table_socialmedia) * 100

percentage_socialmedia_df <- as.data.frame(percentage_table_socialmedia)
colnames(percentage_socialmedia_df) <- c("Grade", "Percentage")

print(percentage_socialmedia_df)
```

    ##   Grade Percentage
    ## 1     2   4.545455
    ## 2     3  13.636364
    ## 3     5  36.363636
    ## 4     6  45.454545

#### Visual of socialmedia habits distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Habitudes_4), fill = factor(Habitudes_4))) +
  geom_bar() +
  scale_fill_manual(values = c("2" = "lightblue1", "3" = "brown3", "5" = "orange", "6" = "lightpink"),
                    labels = c("Once a month", "Multiple times a month", "Multiple times a week", "Everyday")) +
  scale_x_discrete(labels = c("1" = "Never", "2" = "Once a month", "3" = "Multiple times a month", "4" = "Once a week", "5" = "Multiple times a week", "6" = "Everyday")) +
  labs(title = "Frequency of Social media Habit Grades", x = "Social media habits grade", y = "Nb of participants") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-58-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

#### Habits - VIDEO GAMES

``` r
summary(Data_clear_large$Habitudes_5)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   1.000   2.045   2.750   6.000

#### Calculate mean and SD of Habits for video games

``` r
mean_habits_videogames <- mean(Data_clear_large$Habitudes_5, na.rm = TRUE)
sd_habits_videogames <- sd(Data_clear_large$Habitudes_5, na.rm = TRUE)

cat("Mean videogames habits:", mean_habits_videogames, "\n")
```

    ## Mean videogames habits: 2.045455

``` r
cat("Standard Deviation of videogames habits:", sd_habits_videogames, "\n")
```

    ## Standard Deviation of videogames habits: 1.526817

#### Frequency distribution habits of video games

``` r
habits_videogames_distribution <- table(Data_clear_large$Habitudes_5)

print(habits_videogames_distribution)
```

    ## 
    ##  1  2  3  5  6 
    ## 12  4  3  2  1

#### Percentage of television habits

``` r
frequency_table_videogames <- table(Data_clear_large$Habitudes_5)

percentage_table_videogames <- prop.table(frequency_table_videogames) * 100

percentage_videogames_df <- as.data.frame(percentage_table_videogames)
colnames(percentage_videogames_df) <- c("Grade", "Percentage")

print(percentage_videogames_df)
```

    ##   Grade Percentage
    ## 1     1  54.545455
    ## 2     2  18.181818
    ## 3     3  13.636364
    ## 4     5   9.090909
    ## 5     6   4.545455

#### Visual of videogames habits distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Habitudes_5), fill = factor(Habitudes_5))) +
  geom_bar() +
  scale_fill_manual(values = c("1" = "lightgoldenrod1", "2" = "lightblue1", "3" = "brown3", "5" = "orange", "6" = "lightpink"),
                    labels = c("Never", "Once a month", "Multiple times a month", "Multiple times a week", "Everyday")) +
  scale_x_discrete(labels = c("1" = "Never", "2" = "Once a month", "3" = "Multiple times a month", "4" = "Once a week", "5" = "Multiple times a week", "6" = "Everyday")) +
  labs(title = "Frequency of Video games Habit Grades", x = "Video games habits grade", y = "Nb of participants") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-63-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

#### Habits - ORAL STORIES

``` r
summary(Data_clear_large$Habitudes_6)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.250   5.000   4.091   5.750   6.000

#### Calculate mean and SD of Habits for Oral stories

``` r
mean_habits_oralstories <- mean(Data_clear_large$Habitudes_6, na.rm = TRUE)
sd_habits_oralstories  <- sd(Data_clear_large$Habitudes_6, na.rm = TRUE)

cat("Mean oralstories habits:", mean_habits_oralstories , "\n")
```

    ## Mean oralstories habits: 4.090909

``` r
cat("Standard Deviation of oralstories habits:", sd_habits_oralstories , "\n")
```

    ## Standard Deviation of oralstories habits: 1.849301

#### Frequency distribution habits of oral stories

``` r
habits_oralstories_distribution <- table(Data_clear_large$Habitudes_6)

print(habits_oralstories_distribution)
```

    ## 
    ## 1 2 3 4 5 6 
    ## 3 3 2 1 7 6

#### Percentage of oral stories habits

``` r
frequency_table_oralstories <- table(Data_clear_large$Habitudes_6)

percentage_table_oralstories <- prop.table(frequency_table_oralstories) * 100

percentage_oralstories_df <- as.data.frame(percentage_table_oralstories)
colnames(percentage_oralstories_df) <- c("Grade", "Percentage")

print(percentage_oralstories_df)
```

    ##   Grade Percentage
    ## 1     1  13.636364
    ## 2     2  13.636364
    ## 3     3   9.090909
    ## 4     4   4.545455
    ## 5     5  31.818182
    ## 6     6  27.272727

#### Visual of oralstories habits distribution in the dataset

``` r
ggplot(Data_clear_large, aes(x = factor(Habitudes_6), fill = factor(Habitudes_6))) +
  geom_bar() +
  scale_fill_manual(values = c("1" = "lightgoldenrod1", "2" = "lightblue1", "3" = "brown3", "4" = "lightseagreen", "5" = "orange", "6" = "lightpink"),
                    labels = c("Never", "Once a month", "Multiple times a month", "Once a week", "Multiple times a week", "Everyday")) +
  scale_x_discrete(labels = c("1" = "Never", "2" = "Once a month", "3" = "Multiple times a month", "4" = "Once a week", "5" = "Multiple times a week", "6" = "Everyday")) +
  labs(title = "Frequency of Oral stories Habit Grades", x = "Oral stories habits grade", y = "Nb of participants") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-68-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### STORIES WITH HIGH GRANULARITY

#### Select columns that start with “high\_” and convert to numeric

``` r
stories_high <- Data_clear_large %>%
  select(starts_with("high_")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_high))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column starting with “high\_” (for high Stories)

``` r
mean_values <- colMeans(stories_high, na.rm = TRUE)
sd_values <- apply(stories_high, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_high_stories <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_high_stories)
```

    ##                          Column     Mean       SD
    ## high_BEJEDI08_1 high_BEJEDI08_1 3.375000 2.133910
    ## high_MAANAU12_1 high_MAANAU12_1 3.125000 0.834523
    ## high_SAERBE09_1 high_SAERBE09_1 3.428571 1.157868
    ## high_SADJSO02_1 high_SADJSO02_1 4.214286 1.251373

``` r
View(results_high_stories)
```

------------------------------------------------------------------------

### STORIES WITH HIGH GRANULARITY OVERALL

#### Calculate overall mean and standard deviation for high stories across all participants

``` r
Data_clear_large[, grep("^high_", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("^high_", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))


Overall_Mean_Grade <- mean(unlist(Data_clear_large[, grep("^high_", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade <- sd(unlist(Data_clear_large[, grep("^high_", names(Data_clear_large))]), na.rm = TRUE)


overall_results_high_stories <- list(
  Overall_Mean_Grade = Overall_Mean_Grade,
  Overall_SD_Grade = Overall_SD_Grade
)

print(overall_results_high_stories)
```

    ## $Overall_Mean_Grade
    ## [1] 3.613636
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.384565

------------------------------------------------------------------------

### STORIES WITH LOW GRANULARITY

#### Select columns that starts with “low\_” and convert to numeric

``` r
stories_low <- Data_clear_large %>%
  select(starts_with("low_")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_low))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column starting with “low\_” (for low Stories)

``` r
mean_values <- colMeans(stories_low, na.rm = TRUE)
sd_values <- apply(stories_low, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_low_stories <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_low_stories)
```

    ##                        Column     Mean       SD
    ## low_ANJETI02_1 low_ANJETI02_1 2.875000 1.457738
    ## low_CLCHJO01_1 low_CLCHJO01_1 2.625000 1.407886
    ## low_COHEIG03_1 low_COHEIG03_1 3.214286 1.121714
    ## low_VEYVEM11_1 low_VEYVEM11_1 3.785714 1.188313

``` r
View(results_low_stories)
```

------------------------------------------------------------------------

### STORIES WITH LOW GRANULARITY OVERALL

#### Calculate overall mean and standard deviation for low stories across all participants

``` r
Data_clear_large[, grep("^low_", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("^low_", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_low <- mean(unlist(Data_clear_large[, grep("^low_", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_low <- sd(unlist(Data_clear_large[, grep("^low_", names(Data_clear_large))]), na.rm = TRUE)

overall_results_low_stories <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_low,
  Overall_SD_Grade = Overall_SD_Grade_low
)

print(overall_results_low_stories)
```

    ## $Overall_Mean_Grade
    ## [1] 3.227273
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.29154

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Entertainment (\_L_1\_high)

#### Select columns that end with “\_L_1\_high” and convert to numeric

``` r
stories_Entertainment_high <- Data_clear_large %>%
  select(ends_with("_L_1_high")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Entertainment_high))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_1\_high”

``` r
mean_values <- colMeans(stories_Entertainment_high, na.rm = TRUE)
sd_values <- apply(stories_Entertainment_high, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Entertainment_high <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Entertainment_high)
```

    ##                                                Column Mean        SD
    ## exp_high_BEJEDI08_L_1_high exp_high_BEJEDI08_L_1_high  3.0 1.5118579
    ## exp_high_MAANAU12_L_1_high exp_high_MAANAU12_L_1_high  2.5 0.7559289
    ## exp_high_SAERBE09_L_1_high exp_high_SAERBE09_L_1_high  3.0 1.0377490
    ## exp_high_SADJSO02_L_1_high exp_high_SADJSO02_L_1_high  3.5 1.0919284

``` r
View(results_stories_Entertainment_high)
```

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Entertainment (\_L_1\_high) OVERALL

#### Calculate overall mean and standard deviation for Entertainment in high stories across all participants

``` r
Data_clear_large[, grep("_L_1_high$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_1_high$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Entertainment_high <- mean(unlist(Data_clear_large[, grep("_L_1_high$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Entertainment_high <- sd(unlist(Data_clear_large[, grep("_L_1_high$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Entertainment_high <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Entertainment_high,
  Overall_SD_Grade = Overall_SD_Grade_Entertainment_high
)

print(overall_results_Entertainment_high)
```

    ## $Overall_Mean_Grade
    ## [1] 3.068182
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.128855

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Emotionality (\_L_2\_high)

#### Select columns that end with “\_L_2\_high” and convert to numeric

``` r
stories_Emotionality_high <- Data_clear_large %>%
  select(ends_with("_L_2_high")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Emotionality_high))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_2\_high”

``` r
mean_values <- colMeans(stories_Emotionality_high, na.rm = TRUE)
sd_values <- apply(stories_Emotionality_high, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Emotionality_high <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Emotionality_high)
```

    ##                                                Column     Mean        SD
    ## exp_high_BEJEDI08_L_2_high exp_high_BEJEDI08_L_2_high 3.625000 1.5059406
    ## exp_high_MAANAU12_L_2_high exp_high_MAANAU12_L_2_high 3.375000 1.0606602
    ## exp_high_SAERBE09_L_2_high exp_high_SAERBE09_L_2_high 3.214286 0.8925824
    ## exp_high_SADJSO02_L_2_high exp_high_SADJSO02_L_2_high 3.714286 1.0690450

``` r
View(results_stories_Emotionality_high)
```

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Emotionality (\_L_2\_high) OVERALL

#### Calculate overall mean and standard deviation for Emotionality in high stories across all participants

``` r
Data_clear_large[, grep("_L_2_high$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_2_high$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Emotionality_high <- mean(unlist(Data_clear_large[, grep("_L_2_high$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Emotionality_high <- sd(unlist(Data_clear_large[, grep("_L_2_high$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Emotionality_high <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Emotionality_high,
  Overall_SD_Grade = Overall_SD_Grade_Emotionality_high
)

print(overall_results_Emotionality_high)
```

    ## $Overall_Mean_Grade
    ## [1] 3.477273
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.088815

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Memorable (\_L_3\_high)

#### Select columns that end with “\_L_3\_high” and convert to numeric

``` r
stories_Memorable_high <- Data_clear_large %>%
  select(ends_with("_L_3_high")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Memorable_high))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_3\_high”

``` r
mean_values <- colMeans(stories_Memorable_high, na.rm = TRUE)
sd_values <- apply(stories_Memorable_high, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Memorable_high <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Memorable_high)
```

    ##                                                Column     Mean        SD
    ## exp_high_BEJEDI08_L_3_high exp_high_BEJEDI08_L_3_high 3.375000 1.5059406
    ## exp_high_MAANAU12_L_3_high exp_high_MAANAU12_L_3_high 3.000000 0.7559289
    ## exp_high_SAERBE09_L_3_high exp_high_SAERBE09_L_3_high 2.785714 0.8017837
    ## exp_high_SADJSO02_L_3_high exp_high_SADJSO02_L_3_high 3.428571 1.0894096

``` r
View(results_stories_Memorable_high)
```

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Memorable (\_L_3\_high) OVERALL

#### Calculate overall mean and standard deviation for Memorable in high stories across all participants

``` r
Data_clear_large[, grep("_L_3_high$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_3_high$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Memorable_high <- mean(unlist(Data_clear_large[, grep("_L_3_high$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Memorable_high <- sd(unlist(Data_clear_large[, grep("_L_3_high$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Memorable_high <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Memorable_high,
  Overall_SD_Grade = Overall_SD_Grade_Memorable_high
)

print(overall_results_Memorable_high)
```

    ## $Overall_Mean_Grade
    ## [1] 3.136364
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.047498

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Originality (\_L_4\_high)

#### Select columns that end with “\_L_4\_high” and convert to numeric

``` r
stories_Originality_high <- Data_clear_large %>%
  select(ends_with("_L_4_high")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Originality_high))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_4\_high”

``` r
mean_values <- colMeans(stories_Originality_high, na.rm = TRUE)
sd_values <- apply(stories_Originality_high, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Originality_high <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Originality_high)
```

    ##                                                Column     Mean        SD
    ## exp_high_BEJEDI08_L_4_high exp_high_BEJEDI08_L_4_high 3.250000 0.8864053
    ## exp_high_MAANAU12_L_4_high exp_high_MAANAU12_L_4_high 3.000000 1.0690450
    ## exp_high_SAERBE09_L_4_high exp_high_SAERBE09_L_4_high 2.357143 0.8418974
    ## exp_high_SADJSO02_L_4_high exp_high_SADJSO02_L_4_high 3.285714 1.2043876

``` r
View(results_stories_Originality_high)
```

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Originality (\_L_4\_high) OVERALL

#### Calculate overall mean and standard deviation for Originality in high stories across all participants

``` r
Data_clear_large[, grep("_L_4_high$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_4_high$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Originality_high <- mean(unlist(Data_clear_large[, grep("_L_4_high$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Originality_high <- sd(unlist(Data_clear_large[, grep("_L_4_high$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Originality_high <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Originality_high,
  Overall_SD_Grade = Overall_SD_Grade_Originality_high
)

print(overall_results_Originality_high)
```

    ## $Overall_Mean_Grade
    ## [1] 2.931818
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.06526

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Engagement (\_L_5\_high)

#### Select columns that end with “\_L_5\_high” and convert to numeric

``` r
stories_Engagement_high <- Data_clear_large %>%
  select(ends_with("_L_5_high")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Engagement_high))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_5\_high”

``` r
mean_values <- colMeans(stories_Engagement_high, na.rm = TRUE)
sd_values <- apply(stories_Engagement_high, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Engagement_high <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Engagement_high)
```

    ##                                                Column     Mean        SD
    ## exp_high_BEJEDI08_L_5_high exp_high_BEJEDI08_L_5_high 3.375000 1.5979898
    ## exp_high_MAANAU12_L_5_high exp_high_MAANAU12_L_5_high 2.375000 0.5175492
    ## exp_high_SAERBE09_L_5_high exp_high_SAERBE09_L_5_high 2.642857 1.0818178
    ## exp_high_SADJSO02_L_5_high exp_high_SADJSO02_L_5_high 3.142857 1.1673206

``` r
View(results_stories_Engagement_high)
```

------------------------------------------------------------------------

### STORIES HIGH GRANULARITY - Engagement (\_L_5\_high) OVERALL

#### Calculate overall mean and standard deviation for Engagement in high stories across all participants

``` r
Data_clear_large[, grep("_L_5_high$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_5_high$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Engagement_high <- mean(unlist(Data_clear_large[, grep("_L_5_high$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Engagement_high <- sd(unlist(Data_clear_large[, grep("_L_5_high$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Engagement_high <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Engagement_high,
  Overall_SD_Grade = Overall_SD_Grade_Engagement_high
)

print(overall_results_Engagement_high)
```

    ## $Overall_Mean_Grade
    ## [1] 2.886364
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.16571

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Entertainment (\_L_1\_low)

#### Select columns that end with “\_L_1\_low” and convert to numeric

``` r
stories_Entertainment_low <- Data_clear_large %>%
  select(ends_with("_L_1_low")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Entertainment_low))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_1\_low”

``` r
mean_values <- colMeans(stories_Entertainment_low, na.rm = TRUE)
sd_values <- apply(stories_Entertainment_low, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Entertainment_low <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Entertainment_low)
```

    ##                                            Column     Mean        SD
    ## exp_low_ANJETI02_L_1_low exp_low_ANJETI02_L_1_low 2.125000 1.2464235
    ## exp_low_CLCHJO01_L_1_low exp_low_CLCHJO01_L_1_low 2.125000 1.1259916
    ## exp_low_COHEIG03_L_1_low exp_low_COHEIG03_L_1_low 2.571429 1.2224997
    ## exp_low_VEYVEM11_L_1_low exp_low_VEYVEM11_L_1_low 3.357143 0.7449463

``` r
View(results_stories_Entertainment_low)
```

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Entertainment (\_L_1\_low) OVERALL

#### Calculate overall mean and standard deviation for Entertainment in low stories across all participants

``` r
Data_clear_large[, grep("_L_1_low$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_1_low$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Entertainment_low <- mean(unlist(Data_clear_large[, grep("_L_1_low$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Entertainment_low <- sd(unlist(Data_clear_large[, grep("_L_1_low$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Entertainment_low <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Entertainment_low,
  Overall_SD_Grade = Overall_SD_Grade_Entertainment_low
)

print(overall_results_Entertainment_low)
```

    ## $Overall_Mean_Grade
    ## [1] 2.659091
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.160256

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Emotionality (\_L_2\_low)

#### Select columns that end with “\_L_2\_high” and convert to numeric

``` r
stories_Emotionality_low <- Data_clear_large %>%
  select(ends_with("_L_2_low")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Emotionality_low))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_2\_low”

``` r
mean_values <- colMeans(stories_Emotionality_low, na.rm = TRUE)
sd_values <- apply(stories_Emotionality_low, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Emotionality_low <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Emotionality_low)
```

    ##                                            Column     Mean        SD
    ## exp_low_ANJETI02_L_2_low exp_low_ANJETI02_L_2_low 3.375000 1.4078860
    ## exp_low_CLCHJO01_L_2_low exp_low_CLCHJO01_L_2_low 2.375000 1.6850180
    ## exp_low_COHEIG03_L_2_low exp_low_COHEIG03_L_2_low 2.857143 0.9492623
    ## exp_low_VEYVEM11_L_2_low exp_low_VEYVEM11_L_2_low 3.285714 1.1387288

``` r
View(results_stories_Emotionality_low)
```

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Emotionality (\_L_2\_low) OVERALL

#### Calculate overall mean and standard deviation for Emotionality in low stories across all participants

``` r
Data_clear_large[, grep("_L_2_low$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_2_low$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Emotionality_low <- mean(unlist(Data_clear_large[, grep("_L_2_low$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Emotionality_low <- sd(unlist(Data_clear_large[, grep("_L_2_low$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Emotionality_low <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Emotionality_low,
  Overall_SD_Grade = Overall_SD_Grade_Emotionality_low
)

print(overall_results_Emotionality_low)
```

    ## $Overall_Mean_Grade
    ## [1] 3
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.257535

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Memorable (\_L_3\_low)

#### Select columns that end with “\_L_3\_low” and convert to numeric

``` r
stories_Memorable_low <- Data_clear_large %>%
  select(ends_with("_L_3_low")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Memorable_low))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_3\_low”

``` r
mean_values <- colMeans(stories_Memorable_low, na.rm = TRUE)
sd_values <- apply(stories_Memorable_low, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Memorable_low <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Memorable_low)
```

    ##                                            Column     Mean        SD
    ## exp_low_ANJETI02_L_3_low exp_low_ANJETI02_L_3_low 2.375000 1.0606602
    ## exp_low_CLCHJO01_L_3_low exp_low_CLCHJO01_L_3_low 2.250000 1.0350983
    ## exp_low_COHEIG03_L_3_low exp_low_COHEIG03_L_3_low 2.642857 1.0082081
    ## exp_low_VEYVEM11_L_3_low exp_low_VEYVEM11_L_3_low 2.857143 0.9492623

``` r
View(results_stories_Memorable_low)
```

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Memorable (\_L_3\_low) OVERALL

#### Calculate overall mean and standard deviation for Memorable in low stories across all participants

``` r
Data_clear_large[, grep("_L_3_low$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_3_low$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Memorable_low <- mean(unlist(Data_clear_large[, grep("_L_3_low$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Memorable_low <- sd(unlist(Data_clear_large[, grep("_L_3_low$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Memorable_low <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Memorable_low,
  Overall_SD_Grade = Overall_SD_Grade_Memorable_low
)

print(overall_results_Memorable_low)
```

    ## $Overall_Mean_Grade
    ## [1] 2.590909
    ## 
    ## $Overall_SD_Grade
    ## [1] 0.9957627

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Originality (\_L_4\_low)

#### Select columns that end with “\_L_4\_low” and convert to numeric

``` r
stories_Originality_low <- Data_clear_large %>%
  select(ends_with("_L_4_low")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Originality_low))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_4\_low”

``` r
mean_values <- colMeans(stories_Originality_low, na.rm = TRUE)
sd_values <- apply(stories_Originality_low, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Originality_low <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Originality_low)
```

    ##                                            Column     Mean        SD
    ## exp_low_ANJETI02_L_4_low exp_low_ANJETI02_L_4_low 2.625000 1.1877349
    ## exp_low_CLCHJO01_L_4_low exp_low_CLCHJO01_L_4_low 2.375000 0.7440238
    ## exp_low_COHEIG03_L_4_low exp_low_COHEIG03_L_4_low 2.357143 1.0818178
    ## exp_low_VEYVEM11_L_4_low exp_low_VEYVEM11_L_4_low 2.928571 1.0716117

``` r
View(results_stories_Originality_low)
```

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Originality (\_L_4\_low) OVERALL

#### Calculate overall mean and standard deviation for Originality in low stories across all participants

``` r
Data_clear_large[, grep("_L_4_low$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_4_low$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Originality_low <- mean(unlist(Data_clear_large[, grep("_L_4_low$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Originality_low <- sd(unlist(Data_clear_large[, grep("_L_4_low$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Originality_low <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Originality_low,
  Overall_SD_Grade = Overall_SD_Grade_Originality_low
)

print(overall_results_Originality_low)
```

    ## $Overall_Mean_Grade
    ## [1] 2.590909
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.041425

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Engagement (\_L_5\_low)

#### Select columns that end with “\_L_5\_low” and convert to numeric

``` r
stories_Engagement_low <- Data_clear_large %>%
  select(ends_with("_L_5_low")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
```

#### Check for any NA values introduced by coercion

``` r
na_count <- sum(is.na(stories_Engagement_low))
if (na_count > 0) {
  cat("Warning: NA values were introduced during conversion. Count:", na_count, "\n")
}
```

    ## Warning: NA values were introduced during conversion. Count: 44

#### Calculate mean and SD for each column ending with “\_L_5\_low”

``` r
mean_values <- colMeans(stories_Engagement_low, na.rm = TRUE)
sd_values <- apply(stories_Engagement_low, 2, sd, na.rm = TRUE)
```

#### Combine the results into a data frame for better readability

``` r
results_stories_Engagement_low <- data.frame(
  Column = names(mean_values),
  Mean = mean_values,
  SD = sd_values
)

print(results_stories_Engagement_low)
```

    ##                                            Column     Mean        SD
    ## exp_low_ANJETI02_L_5_low exp_low_ANJETI02_L_5_low 2.250000 1.0350983
    ## exp_low_CLCHJO01_L_5_low exp_low_CLCHJO01_L_5_low 2.000000 0.9258201
    ## exp_low_COHEIG03_L_5_low exp_low_COHEIG03_L_5_low 2.428571 1.2838815
    ## exp_low_VEYVEM11_L_5_low exp_low_VEYVEM11_L_5_low 2.928571 0.9972490

``` r
View(results_stories_Engagement_low)
```

------------------------------------------------------------------------

### STORIES LOW GRANULARITY - Engagement (\_L_5\_low) OVERALL

#### Calculate overall mean and standard deviation for Engagement in low stories across all participants

``` r
Data_clear_large[, grep("_L_5_low$", names(Data_clear_large))] <- 
  lapply(Data_clear_large[, grep("_L_5_low$", names(Data_clear_large))], 
         function(x) as.numeric(as.character(x)))

Overall_Mean_Grade_Engagement_low <- mean(unlist(Data_clear_large[, grep("_L_5_low$", names(Data_clear_large))]), na.rm = TRUE)
Overall_SD_Grade_Engagement_low <- sd(unlist(Data_clear_large[, grep("_L_5_low$", names(Data_clear_large))]), na.rm = TRUE)

overall_results_Engagement_low <- list(
  Overall_Mean_Grade = Overall_Mean_Grade_Engagement_low,
  Overall_SD_Grade = Overall_SD_Grade_Engagement_low
)

print(overall_results_Engagement_low)
```

    ## $Overall_Mean_Grade
    ## [1] 2.477273
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.109968

## 3. Comparisons

### Comparison between OVERALL grades HIGH STORIES and LOW STORIES

``` r
comparison_results_overall_highvslow <- data.frame(
  Story_Type = c("High Stories", "Low Stories"),
  Overall_Mean_Grade = c(overall_results_high_stories$Overall_Mean_Grade, 
                         overall_results_low_stories$Overall_Mean_Grade),
  Overall_SD_Grade = c(overall_results_high_stories$Overall_SD_Grade, 
                       overall_results_low_stories$Overall_SD_Grade)
)
```

#### Create the bar plot comparing overall mean grades

``` r
ggplot(comparison_results_overall_highvslow, aes(x = Story_Type, y = Overall_Mean_Grade, fill = Story_Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Overall_Mean_Grade - Overall_SD_Grade, 
                    ymax = Overall_Mean_Grade + Overall_SD_Grade), 
                width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Comparison of Overall Mean Grades for High and Low Stories",
       x = "Story Type",
       y = "Overall Mean Grade") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-130-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### Comparison between OVERALL ENTERTAINMENT grades HIGH STORIES and LOW STORIES

``` r
comparison_results_overall_Entertainment_highvslow <- data.frame(
  Story_Type = c("High Stories", "Low Stories"),
  Overall_Mean_Grade = c(overall_results_Entertainment_high$Overall_Mean_Grade, 
                         overall_results_Entertainment_low$Overall_Mean_Grade),
  Overall_SD_Grade = c(overall_results_Entertainment_high$Overall_SD_Grade, 
                       overall_results_Entertainment_low$Overall_SD_Grade)
)
```

#### Create the bar plot comparing overall mean grades for Entertainment

``` r
ggplot(comparison_results_overall_Entertainment_highvslow, aes(x = Story_Type, y = Overall_Mean_Grade, fill = Story_Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Overall_Mean_Grade - Overall_SD_Grade, 
                    ymax = Overall_Mean_Grade + Overall_SD_Grade), 
                width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Comparison of Overall Mean Grades for Entertainment for High and Low Stories",
       x = "Story Type",
       y = "Overall Mean Grade Entertainment") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-132-1.png" style="display: block; margin: auto;" />

``` r
View(comparison_results_overall_Entertainment_highvslow)
```

------------------------------------------------------------------------

### Comparison between OVERALL EMOTIONALITY grades HIGH STORIES and LOW STORIES

``` r
comparison_results_overall_Emotionality_highvslow <- data.frame(
  Story_Type = c("High Stories", "Low Stories"),
  Overall_Mean_Grade = c(overall_results_Emotionality_high$Overall_Mean_Grade, 
                         overall_results_Emotionality_low$Overall_Mean_Grade),
  Overall_SD_Grade = c(overall_results_Emotionality_high$Overall_SD_Grade, 
                       overall_results_Emotionality_low$Overall_SD_Grade)
)
```

#### Create the bar plot comparing overall mean grades for Emotionality

``` r
ggplot(comparison_results_overall_Emotionality_highvslow, aes(x = Story_Type, y = Overall_Mean_Grade, fill = Story_Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Overall_Mean_Grade - Overall_SD_Grade, 
                    ymax = Overall_Mean_Grade + Overall_SD_Grade), 
                width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Comparison of Overall Mean Grades for Emotionality for High and Low Stories",
       x = "Story Type",
       y = "Overall Mean Grade Emotionality") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-134-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### Comparison between OVERALL MEMORABLE grades HIGH STORIES and LOW STORIES

``` r
comparison_results_overall_Memorable_highvslow <- data.frame(
  Story_Type = c("High Stories", "Low Stories"),
  Overall_Mean_Grade = c(overall_results_Memorable_high$Overall_Mean_Grade, 
                         overall_results_Memorable_low$Overall_Mean_Grade),
  Overall_SD_Grade = c(overall_results_Memorable_high$Overall_SD_Grade, 
                       overall_results_Memorable_low$Overall_SD_Grade)
)
```

#### Create the bar plot comparing overall mean grades for Emotionality

``` r
ggplot(comparison_results_overall_Memorable_highvslow, aes(x = Story_Type, y = Overall_Mean_Grade, fill = Story_Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Overall_Mean_Grade - Overall_SD_Grade, 
                    ymax = Overall_Mean_Grade + Overall_SD_Grade), 
                width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Comparison of Overall Mean Grades for Memorable for High and Low Stories",
       x = "Story Type",
       y = "Overall Mean Grade Memorable") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-136-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### Comparison between OVERALL ORIGINALITY grades HIGH STORIES and LOW STORIES

``` r
comparison_results_overall_Originality_highvslow <- data.frame(
  Story_Type = c("High Stories", "Low Stories"),
  Overall_Mean_Grade = c(overall_results_Originality_high$Overall_Mean_Grade, 
                         overall_results_Originality_low$Overall_Mean_Grade),
  Overall_SD_Grade = c(overall_results_Originality_high$Overall_SD_Grade, 
                       overall_results_Originality_low$Overall_SD_Grade)
)
```

#### Create the bar plot comparing overall mean grades for Emotionality

``` r
ggplot(comparison_results_overall_Originality_highvslow, aes(x = Story_Type, y = Overall_Mean_Grade, fill = Story_Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Overall_Mean_Grade - Overall_SD_Grade, 
                    ymax = Overall_Mean_Grade + Overall_SD_Grade), 
                width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Comparison of Overall Mean Grades for Originality for High and Low Stories",
       x = "Story Type",
       y = "Overall Mean Grade Originality") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-138-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### Comparison between OVERALL ENGAGEMENT grades HIGH STORIES and LOW STORIES

``` r
comparison_results_overall_Engagement_highvslow <- data.frame(
  Story_Type = c("High Stories", "Low Stories"),
  Overall_Mean_Grade = c(overall_results_Engagement_high$Overall_Mean_Grade, 
                         overall_results_Engagement_low$Overall_Mean_Grade),
  Overall_SD_Grade = c(overall_results_Engagement_high$Overall_SD_Grade, 
                       overall_results_Engagement_low$Overall_SD_Grade)
)
```

#### Create the bar plot comparing overall mean grades for Emotionality

``` r
ggplot(comparison_results_overall_Engagement_highvslow, aes(x = Story_Type, y = Overall_Mean_Grade, fill = Story_Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Overall_Mean_Grade - Overall_SD_Grade, 
                    ymax = Overall_Mean_Grade + Overall_SD_Grade), 
                width = 0.2, position = position_dodge(0.7)) +
  labs(title = "Comparison of Overall Mean Grades for Engagement for High and Low Stories",
       x = "Story Type",
       y = "Overall Mean Grade Engagement") +
  theme_minimal()
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-140-1.png" style="display: block; margin: auto;" />

## 4. Correlation table

### Correlation table with Grades and Category

``` r
correlation_data <- Data_clear_long[, c("Age", "Genre", "Education", "Vie", "Participation", "Grades", "Category")]

correlation_data$Category <- as.numeric(correlation_data$Category)
correlation_data$Age <- as.numeric(correlation_data$Age)
correlation_data$Genre <- as.numeric(correlation_data$Genre)
correlation_data$Education <- as.numeric(correlation_data$Education)
correlation_data$Vie <- as.numeric(correlation_data$Vie)
correlation_data$Participation <- as.numeric(correlation_data$Participation)
correlation_data$Grades <- as.numeric(correlation_data$Grades)

correlation_data <- na.omit(correlation_data)
```

``` r
correlation_results = cor(correlation_data)
print(correlation_results)
```

    ##                       Age       Genre   Education          Vie Participation
    ## Age            1.00000000 -0.06702426  0.30101322 -0.102337868    0.34120040
    ## Genre         -0.06702426  1.00000000  0.45989626  0.044681660   -0.14613223
    ## Education      0.30101322  0.45989626  1.00000000 -0.094290807    0.27733021
    ## Vie           -0.10233787  0.04468166 -0.09429081  1.000000000   -0.09221389
    ## Participation  0.34120040 -0.14613223  0.27733021 -0.092213889    1.00000000
    ## Grades        -0.07490601 -0.05691185 -0.24345578  0.009633164   -0.13182543
    ## Category       0.00000000  0.00000000  0.00000000  0.000000000    0.00000000
    ##                     Grades   Category
    ## Age           -0.074906007  0.0000000
    ## Genre         -0.056911853  0.0000000
    ## Education     -0.243455778  0.0000000
    ## Vie            0.009633164  0.0000000
    ## Participation -0.131825434  0.0000000
    ## Grades         1.000000000 -0.1444261
    ## Category      -0.144426091  1.0000000

``` r
correlation_results_pvalue <- rcorr(as.matrix(correlation_data))
correlation_results_pvalue
```

    ##                 Age Genre Education   Vie Participation Grades Category
    ## Age            1.00 -0.07      0.30 -0.10          0.34  -0.07     0.00
    ## Genre         -0.07  1.00      0.46  0.04         -0.15  -0.06     0.00
    ## Education      0.30  0.46      1.00 -0.09          0.28  -0.24     0.00
    ## Vie           -0.10  0.04     -0.09  1.00         -0.09   0.01     0.00
    ## Participation  0.34 -0.15      0.28 -0.09          1.00  -0.13     0.00
    ## Grades        -0.07 -0.06     -0.24  0.01         -0.13   1.00    -0.14
    ## Category       0.00  0.00      0.00  0.00          0.00  -0.14     1.00
    ## 
    ## n= 88 
    ## 
    ## 
    ## P
    ##               Age    Genre  Education Vie    Participation Grades Category
    ## Age                  0.5350 0.0044    0.3427 0.0011        0.4879 1.0000  
    ## Genre         0.5350        0.0000    0.6793 0.1743        0.5984 1.0000  
    ## Education     0.0044 0.0000           0.3822 0.0089        0.0223 1.0000  
    ## Vie           0.3427 0.6793 0.3822           0.3928        0.9290 1.0000  
    ## Participation 0.0011 0.1743 0.0089    0.3928               0.2208 1.0000  
    ## Grades        0.4879 0.5984 0.0223    0.9290 0.2208               0.1794  
    ## Category      1.0000 1.0000 1.0000    1.0000 1.0000        0.1794

``` r
correlation_results_pvalue$r
```

    ##                       Age       Genre   Education          Vie Participation
    ## Age            1.00000000 -0.06702426  0.30101322 -0.102337868    0.34120040
    ## Genre         -0.06702426  1.00000000  0.45989626  0.044681660   -0.14613223
    ## Education      0.30101322  0.45989626  1.00000000 -0.094290807    0.27733021
    ## Vie           -0.10233787  0.04468166 -0.09429081  1.000000000   -0.09221389
    ## Participation  0.34120040 -0.14613223  0.27733021 -0.092213889    1.00000000
    ## Grades        -0.07490601 -0.05691185 -0.24345578  0.009633164   -0.13182543
    ## Category       0.00000000  0.00000000  0.00000000  0.000000000    0.00000000
    ##                     Grades   Category
    ## Age           -0.074906007  0.0000000
    ## Genre         -0.056911853  0.0000000
    ## Education     -0.243455778  0.0000000
    ## Vie            0.009633164  0.0000000
    ## Participation -0.131825434  0.0000000
    ## Grades         1.000000000 -0.1444261
    ## Category      -0.144426091  1.0000000

``` r
correlation_results_pvalue$P
```

    ##                       Age        Genre    Education       Vie Participation
    ## Age                    NA 5.349605e-01 4.374788e-03 0.3427316   0.001141183
    ## Genre         0.534960485           NA 6.551394e-06 0.6793395   0.174290331
    ## Education     0.004374788 6.551394e-06           NA 0.3822119   0.008898860
    ## Vie           0.342731607 6.793395e-01 3.822119e-01        NA   0.392829667
    ## Participation 0.001141183 1.742903e-01 8.898860e-03 0.3928297            NA
    ## Grades        0.487927532 5.984199e-01 2.227362e-02 0.9290207   0.220838463
    ## Category      1.000000000 1.000000e+00 1.000000e+00 1.0000000   1.000000000
    ##                   Grades  Category
    ## Age           0.48792753 1.0000000
    ## Genre         0.59841991 1.0000000
    ## Education     0.02227362 1.0000000
    ## Vie           0.92902068 1.0000000
    ## Participation 0.22083846 1.0000000
    ## Grades                NA 0.1794289
    ## Category      0.17942890        NA

``` r
correlation_matrix <- correlation_results_pvalue$r

ggcorrplot(correlation_matrix, hc.order = FALSE, type = "lower", lab = TRUE)
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-142-1.png" style="display: block; margin: auto;" />

``` r
tab_corr(correlation_data,
         triangle = "lower")
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
 
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
Age
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
Genre
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
Education
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
Vie
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
Participation
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
Grades
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
Category
</th>
</tr>
<tr>
<td style="font-style:italic;">
Age
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
Genre
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.067<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
Education
</td>
<td style="padding:0.2cm; text-align:center;">
0.301<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.460<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
Vie
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.102<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.045<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.094<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
Participation
</td>
<td style="padding:0.2cm; text-align:center;">
0.341<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.146<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.277<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.092<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
Grades
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.075<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.057<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.243<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.010<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.132<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
Category
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.000<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.000<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.000<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.000<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.000<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.144<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td colspan="8" style="border-bottom:double black; border-top:1px solid black; font-style:italic; font-size:0.9em; text-align:right;">
Computed correlation used pearson-method with listwise-deletion.
</td>
</tr>
</table>

------------------------------------------------------------------------

## 5. Distribution

### Distribution of HIGH STORIES GRADES

``` r
high_grades_distribution <- Data_clear_large %>%
  select("high_BEJEDI08_1", "high_MAANAU12_1", "high_SAERBE09_1", "high_SADJSO02_1")

par(mfrow = c(2, 2))
for (col in names(high_grades_distribution)) {
  hist(high_grades_distribution[[col]], breaks = 10, main = paste("Histogram of", col), xlab = "Grades", col = "lightblue")
}
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-144-1.png" style="display: block; margin: auto;" />

``` r
summary_stats_high_grades <- summary(high_grades_distribution)
print(summary_stats_high_grades)
```

    ##  high_BEJEDI08_1 high_MAANAU12_1 high_SAERBE09_1 high_SADJSO02_1
    ##  Min.   :1.000   Min.   :2.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:1.000   1st Qu.:2.750   1st Qu.:3.000   1st Qu.:4.000  
    ##  Median :4.000   Median :3.000   Median :3.000   Median :4.000  
    ##  Mean   :3.375   Mean   :3.125   Mean   :3.429   Mean   :4.214  
    ##  3rd Qu.:5.000   3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.:5.000  
    ##  Max.   :6.000   Max.   :4.000   Max.   :5.000   Max.   :6.000  
    ##  NA's   :14      NA's   :14      NA's   :8       NA's   :8

#### Shapiro-Wilk normality test

``` r
shapiro_results_high_grades <- lapply(high_grades_distribution, shapiro.test)

shapiro_results_high_grades
```

    ## $high_BEJEDI08_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.81726, p-value = 0.04366
    ## 
    ## 
    ## $high_MAANAU12_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.83521, p-value = 0.06724
    ## 
    ## 
    ## $high_SAERBE09_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.90185, p-value = 0.12
    ## 
    ## 
    ## $high_SADJSO02_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.85469, p-value = 0.02574

``` r
# Higher than 0.05 = normally distributed
# BEJEDI08 and SADJSO02 are NOT normally distributed
```

------------------------------------------------------------------------

### Distribution of LOW STORIES GRADES

``` r
low_grades_distribution <- Data_clear_large %>%
  select("low_ANJETI02_1", "low_CLCHJO01_1", "low_COHEIG03_1", "low_VEYVEM11_1")

par(mfrow = c(2, 2))
for (col in names(low_grades_distribution)) {
  hist(low_grades_distribution[[col]], breaks = 10, main = paste("Histogram of", col), xlab = "Grades", col = "lightblue")
}
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-147-1.png" style="display: block; margin: auto;" />

``` r
summary_stats_low_grades <- summary(low_grades_distribution)
print(summary_stats_low_grades)
```

    ##  low_ANJETI02_1  low_CLCHJO01_1  low_COHEIG03_1  low_VEYVEM11_1 
    ##  Min.   :1.000   Min.   :1.000   Min.   :2.000   Min.   :2.000  
    ##  1st Qu.:1.750   1st Qu.:1.750   1st Qu.:2.250   1st Qu.:3.000  
    ##  Median :3.000   Median :2.500   Median :3.000   Median :4.000  
    ##  Mean   :2.875   Mean   :2.625   Mean   :3.214   Mean   :3.786  
    ##  3rd Qu.:4.000   3rd Qu.:3.250   3rd Qu.:3.750   3rd Qu.:4.750  
    ##  Max.   :5.000   Max.   :5.000   Max.   :5.000   Max.   :6.000  
    ##  NA's   :14      NA's   :14      NA's   :8       NA's   :8

#### Shapiro-Wilk normality test

``` r
shapiro_results_low_grades <- lapply(low_grades_distribution, shapiro.test)

shapiro_results_low_grades
```

    ## $low_ANJETI02_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.93, p-value = 0.5161
    ## 
    ## 
    ## $low_CLCHJO01_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.93444, p-value = 0.5573
    ## 
    ## 
    ## $low_COHEIG03_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.8227, p-value = 0.00966
    ## 
    ## 
    ## $low_VEYVEM11_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.93591, p-value = 0.3684

``` r
# Higher than 0.05 = normally distributed
# COHEIG03 is NOT normally distributed
```

------------------------------------------------------------------------

### Distribution of Exploratory questions

``` r
grades_distribution_Exploration <- Data_clear_large %>%
  select(matches("_high$|_low$"))

par(mfrow = c(2, 2))
for (col in names(grades_distribution_Exploration)) {
  hist(grades_distribution_Exploration[[col]], breaks = 10, main = paste("Histogram of", col), xlab = "Grades", col = "lightblue")
}
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-1.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-2.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-3.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-4.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-5.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-6.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-7.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-8.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-9.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-150-10.png" style="display: block; margin: auto;" />

``` r
summary_stats_grades_Exploration <- summary(grades_distribution_Exploration)
print(summary_stats_grades_Exploration)
```

    ##  exp_high_BEJEDI08_L_1_high exp_high_BEJEDI08_L_2_high
    ##  Min.   :1.00               Min.   :1.000             
    ##  1st Qu.:1.75               1st Qu.:2.750             
    ##  Median :3.50               Median :4.000             
    ##  Mean   :3.00               Mean   :3.625             
    ##  3rd Qu.:4.00               3rd Qu.:5.000             
    ##  Max.   :5.00               Max.   :5.000             
    ##  NA's   :14                 NA's   :14                
    ##  exp_high_BEJEDI08_L_3_high exp_high_BEJEDI08_L_4_high
    ##  Min.   :1.000              Min.   :2.00              
    ##  1st Qu.:2.000              1st Qu.:3.00              
    ##  Median :4.000              Median :3.00              
    ##  Mean   :3.375              Mean   :3.25              
    ##  3rd Qu.:4.250              3rd Qu.:3.25              
    ##  Max.   :5.000              Max.   :5.00              
    ##  NA's   :14                 NA's   :14                
    ##  exp_high_BEJEDI08_L_5_high exp_high_MAANAU12_L_1_high
    ##  Min.   :1.000              Min.   :2.0               
    ##  1st Qu.:2.500              1st Qu.:2.0               
    ##  Median :4.000              Median :2.0               
    ##  Mean   :3.375              Mean   :2.5               
    ##  3rd Qu.:4.250              3rd Qu.:3.0               
    ##  Max.   :5.000              Max.   :4.0               
    ##  NA's   :14                 NA's   :14                
    ##  exp_high_MAANAU12_L_2_high exp_high_MAANAU12_L_3_high
    ##  Min.   :2.000              Min.   :2.00              
    ##  1st Qu.:2.750              1st Qu.:2.75              
    ##  Median :3.500              Median :3.00              
    ##  Mean   :3.375              Mean   :3.00              
    ##  3rd Qu.:4.000              3rd Qu.:3.25              
    ##  Max.   :5.000              Max.   :4.00              
    ##  NA's   :14                 NA's   :14                
    ##  exp_high_MAANAU12_L_4_high exp_high_MAANAU12_L_5_high exp_low_ANJETI02_L_1_low
    ##  Min.   :2.00               Min.   :2.000              Min.   :1.000           
    ##  1st Qu.:2.00               1st Qu.:2.000              1st Qu.:1.000           
    ##  Median :3.00               Median :2.000              Median :2.000           
    ##  Mean   :3.00               Mean   :2.375              Mean   :2.125           
    ##  3rd Qu.:3.25               3rd Qu.:3.000              3rd Qu.:3.000           
    ##  Max.   :5.00               Max.   :3.000              Max.   :4.000           
    ##  NA's   :14                 NA's   :14                 NA's   :14              
    ##  exp_low_ANJETI02_L_2_low exp_low_ANJETI02_L_3_low exp_low_ANJETI02_L_4_low
    ##  Min.   :1.000            Min.   :1.000            Min.   :1.000           
    ##  1st Qu.:2.750            1st Qu.:1.750            1st Qu.:1.750           
    ##  Median :3.500            Median :2.500            Median :3.000           
    ##  Mean   :3.375            Mean   :2.375            Mean   :2.625           
    ##  3rd Qu.:4.250            3rd Qu.:3.000            3rd Qu.:3.250           
    ##  Max.   :5.000            Max.   :4.000            Max.   :4.000           
    ##  NA's   :14               NA's   :14               NA's   :14              
    ##  exp_low_ANJETI02_L_5_low exp_low_CLCHJO01_L_1_low exp_low_CLCHJO01_L_2_low
    ##  Min.   :1.00             Min.   :1.000            Min.   :1.000           
    ##  1st Qu.:1.75             1st Qu.:1.000            1st Qu.:1.000           
    ##  Median :2.00             Median :2.000            Median :2.000           
    ##  Mean   :2.25             Mean   :2.125            Mean   :2.375           
    ##  3rd Qu.:3.00             3rd Qu.:3.000            3rd Qu.:2.750           
    ##  Max.   :4.00             Max.   :4.000            Max.   :5.000           
    ##  NA's   :14               NA's   :14               NA's   :14              
    ##  exp_low_CLCHJO01_L_3_low exp_low_CLCHJO01_L_4_low exp_low_CLCHJO01_L_5_low
    ##  Min.   :1.00             Min.   :1.000            Min.   :1               
    ##  1st Qu.:1.75             1st Qu.:2.000            1st Qu.:1               
    ##  Median :2.00             Median :2.500            Median :2               
    ##  Mean   :2.25             Mean   :2.375            Mean   :2               
    ##  3rd Qu.:3.00             3rd Qu.:3.000            3rd Qu.:3               
    ##  Max.   :4.00             Max.   :3.000            Max.   :3               
    ##  NA's   :14               NA's   :14               NA's   :14              
    ##  exp_high_SAERBE09_L_1_high exp_high_SAERBE09_L_2_high
    ##  Min.   :1                  Min.   :1.000             
    ##  1st Qu.:2                  1st Qu.:3.000             
    ##  Median :3                  Median :3.000             
    ##  Mean   :3                  Mean   :3.214             
    ##  3rd Qu.:4                  3rd Qu.:3.750             
    ##  Max.   :4                  Max.   :5.000             
    ##  NA's   :8                  NA's   :8                 
    ##  exp_high_SAERBE09_L_3_high exp_high_SAERBE09_L_4_high
    ##  Min.   :1.000              Min.   :1.000             
    ##  1st Qu.:2.250              1st Qu.:2.000             
    ##  Median :3.000              Median :2.000             
    ##  Mean   :2.786              Mean   :2.357             
    ##  3rd Qu.:3.000              3rd Qu.:3.000             
    ##  Max.   :4.000              Max.   :4.000             
    ##  NA's   :8                  NA's   :8                 
    ##  exp_high_SAERBE09_L_5_high exp_high_SADJSO02_L_1_high
    ##  Min.   :1.000              Min.   :1.0               
    ##  1st Qu.:2.000              1st Qu.:3.0               
    ##  Median :2.500              Median :4.0               
    ##  Mean   :2.643              Mean   :3.5               
    ##  3rd Qu.:3.750              3rd Qu.:4.0               
    ##  Max.   :4.000              Max.   :5.0               
    ##  NA's   :8                  NA's   :8                 
    ##  exp_high_SADJSO02_L_2_high exp_high_SADJSO02_L_3_high
    ##  Min.   :1.000              Min.   :1.000             
    ##  1st Qu.:3.000              1st Qu.:3.000             
    ##  Median :4.000              Median :3.500             
    ##  Mean   :3.714              Mean   :3.429             
    ##  3rd Qu.:4.000              3rd Qu.:4.000             
    ##  Max.   :5.000              Max.   :5.000             
    ##  NA's   :8                  NA's   :8                 
    ##  exp_high_SADJSO02_L_4_high exp_high_SADJSO02_L_5_high exp_low_COHEIG03_L_1_low
    ##  Min.   :1.000              Min.   :1.000              Min.   :1.000           
    ##  1st Qu.:2.250              1st Qu.:2.250              1st Qu.:2.000           
    ##  Median :3.500              Median :3.000              Median :2.500           
    ##  Mean   :3.286              Mean   :3.143              Mean   :2.571           
    ##  3rd Qu.:4.000              3rd Qu.:4.000              3rd Qu.:3.000           
    ##  Max.   :5.000              Max.   :5.000              Max.   :5.000           
    ##  NA's   :8                  NA's   :8                  NA's   :8               
    ##  exp_low_COHEIG03_L_2_low exp_low_COHEIG03_L_3_low exp_low_COHEIG03_L_4_low
    ##  Min.   :1.000            Min.   :1.000            Min.   :1.000           
    ##  1st Qu.:2.000            1st Qu.:2.000            1st Qu.:1.250           
    ##  Median :3.000            Median :3.000            Median :2.500           
    ##  Mean   :2.857            Mean   :2.643            Mean   :2.357           
    ##  3rd Qu.:3.750            3rd Qu.:3.000            3rd Qu.:3.000           
    ##  Max.   :4.000            Max.   :4.000            Max.   :4.000           
    ##  NA's   :8                NA's   :8                NA's   :8               
    ##  exp_low_COHEIG03_L_5_low exp_low_VEYVEM11_L_1_low exp_low_VEYVEM11_L_2_low
    ##  Min.   :1.000            Min.   :2.000            Min.   :1.000           
    ##  1st Qu.:1.250            1st Qu.:3.000            1st Qu.:3.000           
    ##  Median :2.000            Median :3.500            Median :3.000           
    ##  Mean   :2.429            Mean   :3.357            Mean   :3.286           
    ##  3rd Qu.:3.000            3rd Qu.:4.000            3rd Qu.:4.000           
    ##  Max.   :5.000            Max.   :4.000            Max.   :5.000           
    ##  NA's   :8                NA's   :8                NA's   :8               
    ##  exp_low_VEYVEM11_L_3_low exp_low_VEYVEM11_L_4_low exp_low_VEYVEM11_L_5_low
    ##  Min.   :1.000            Min.   :1.000            Min.   :1.000           
    ##  1st Qu.:2.000            1st Qu.:2.250            1st Qu.:2.000           
    ##  Median :3.000            Median :3.000            Median :3.000           
    ##  Mean   :2.857            Mean   :2.929            Mean   :2.929           
    ##  3rd Qu.:3.750            3rd Qu.:4.000            3rd Qu.:4.000           
    ##  Max.   :4.000            Max.   :4.000            Max.   :4.000           
    ##  NA's   :8                NA's   :8                NA's   :8

#### Shapiro-Wilk normality test

``` r
shapiro_results_grades_Exploration <- lapply(grades_distribution_Exploration, shapiro.test)

shapiro_results_grades_Exploration
```

    ## $exp_high_BEJEDI08_L_1_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.89107, p-value = 0.2394
    ## 
    ## 
    ## $exp_high_BEJEDI08_L_2_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.87142, p-value = 0.1556
    ## 
    ## 
    ## $exp_high_BEJEDI08_L_3_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.87142, p-value = 0.1556
    ## 
    ## 
    ## $exp_high_BEJEDI08_L_4_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.82604, p-value = 0.05399
    ## 
    ## 
    ## $exp_high_BEJEDI08_L_5_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.83379, p-value = 0.06499
    ## 
    ## 
    ## $exp_high_MAANAU12_L_1_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.7238, p-value = 0.004174
    ## 
    ## 
    ## $exp_high_MAANAU12_L_2_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.91163, p-value = 0.3657
    ## 
    ## 
    ## $exp_high_MAANAU12_L_3_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.84891, p-value = 0.09288
    ## 
    ## 
    ## $exp_high_MAANAU12_L_4_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.85995, p-value = 0.1199
    ## 
    ## 
    ## $exp_high_MAANAU12_L_5_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.6412, p-value = 0.0004791
    ## 
    ## 
    ## $exp_low_ANJETI02_L_1_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.7792, p-value = 0.01707
    ## 
    ## 
    ## $exp_low_ANJETI02_L_2_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.93444, p-value = 0.5573
    ## 
    ## 
    ## $exp_low_ANJETI02_L_3_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.91163, p-value = 0.3657
    ## 
    ## 
    ## $exp_low_ANJETI02_L_4_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.87483, p-value = 0.1679
    ## 
    ## 
    ## $exp_low_ANJETI02_L_5_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.91728, p-value = 0.4082
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_1_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.88212, p-value = 0.1973
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_2_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.74988, p-value = 0.008144
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_3_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.91728, p-value = 0.4082
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_4_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.79758, p-value = 0.02697
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_5_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.8015, p-value = 0.0297
    ## 
    ## 
    ## $exp_high_SAERBE09_L_1_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.8316, p-value = 0.01261
    ## 
    ## 
    ## $exp_high_SAERBE09_L_2_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.78862, p-value = 0.003622
    ## 
    ## 
    ## $exp_high_SAERBE09_L_3_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.84982, p-value = 0.02209
    ## 
    ## 
    ## $exp_high_SAERBE09_L_4_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.88898, p-value = 0.07813
    ## 
    ## 
    ## $exp_high_SAERBE09_L_5_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.87277, p-value = 0.0459
    ## 
    ## 
    ## $exp_high_SADJSO02_L_1_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.89236, p-value = 0.08744
    ## 
    ## 
    ## $exp_high_SADJSO02_L_2_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.85288, p-value = 0.02431
    ## 
    ## 
    ## $exp_high_SADJSO02_L_3_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.90812, p-value = 0.148
    ## 
    ## 
    ## $exp_high_SADJSO02_L_4_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.92169, p-value = 0.2326
    ## 
    ## 
    ## $exp_high_SADJSO02_L_5_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.93607, p-value = 0.3703
    ## 
    ## 
    ## $exp_low_COHEIG03_L_1_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.92276, p-value = 0.2409
    ## 
    ## 
    ## $exp_low_COHEIG03_L_2_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.88237, p-value = 0.06283
    ## 
    ## 
    ## $exp_low_COHEIG03_L_3_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.8953, p-value = 0.09641
    ## 
    ## 
    ## $exp_low_COHEIG03_L_4_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.87277, p-value = 0.0459
    ## 
    ## 
    ## $exp_low_COHEIG03_L_5_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.89955, p-value = 0.1111
    ## 
    ## 
    ## $exp_low_VEYVEM11_L_1_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.77272, p-value = 0.002341
    ## 
    ## 
    ## $exp_low_VEYVEM11_L_2_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.93243, p-value = 0.3299
    ## 
    ## 
    ## $exp_low_VEYVEM11_L_3_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.88237, p-value = 0.06283
    ## 
    ## 
    ## $exp_low_VEYVEM11_L_4_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.84279, p-value = 0.01775
    ## 
    ## 
    ## $exp_low_VEYVEM11_L_5_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.86246, p-value = 0.03293

``` r
# Higher than 0.05 = normally distributed
# BEJEDI08_L_1_high, BEJEDI08_L_4_high, MAANAU12_L_1_high, MAANAU12_L_5_high, ANJETI02_L_1_low, CLCHJO01_L_2_low, SAERBE09_L_1_high, SAERBE09_L_2_high, SAERBE09_L_3_high, SADJSO02_L_2_high, COHEIG03_L_4_low, VEYVEM11_L_1_low and VEYVEM11_L_4_low are NOT NORMALLY DISTRIBUTED
```

## 6.Mann-Whitney U Test

### Mann-Whitney U Test for every participants

``` r
high_ratings <- unlist(Data_clear_large %>% select(starts_with("high_")))
low_ratings <- unlist(Data_clear_large %>% select(starts_with("low_")))

mann_whitney_test <- wilcox.test(high_ratings, low_ratings, paired = TRUE)

print(mann_whitney_test)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  high_ratings and low_ratings
    ## V = 288, p-value = 0.1182
    ## alternative hypothesis: true location shift is not equal to 0

### Mann-Whitney U Test for Group A (1) and Group B (2)

``` r
groupe_A_data <- Data_clear_large %>% filter(Groupe == 1)
groupe_B_data <- Data_clear_large %>% filter(Groupe == 2)

# Groupe A (1)
high_ratings_groupeA<- unlist(groupe_A_data %>% select(starts_with("high_")))
low_ratings_groupeA <- unlist(groupe_A_data %>% select(starts_with("low_")))

mann_whitney_groupeA <- wilcox.test(high_ratings_groupeA, low_ratings_groupeA, paired = TRUE)

# Groupe B (2)
high_ratings_groupeB <- unlist(groupe_B_data %>% select(starts_with("high_")))
low_ratings_groupeB <- unlist(groupe_B_data %>% select(starts_with("low_")))
mann_whitney_groupeB <- wilcox.test(high_ratings_groupeB, low_ratings_groupeB, paired = TRUE)

# Print results
print(mann_whitney_groupeA)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  high_ratings_groupeA and low_ratings_groupeA
    ## V = 53, p-value = 0.2663
    ## alternative hypothesis: true location shift is not equal to 0

``` r
print(mann_whitney_groupeB)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  high_ratings_groupeB and low_ratings_groupeB
    ## V = 99, p-value = 0.2855
    ## alternative hypothesis: true location shift is not equal to 0

## 7. Linear Mixed_effects Model

### Linear Mixed_effects Model for high and low stories

``` r
Data_clear_long$Grades <- as.numeric(as.character(Data_clear_long$Grades))
Data_clear_long$Age <- as.numeric(Data_clear_long$Age)
Data_clear_long$Genre <- as.factor(Data_clear_long$Genre)
Data_clear_long$Education <- as.factor(Data_clear_long$Education)
Data_clear_long$Vie <- as.factor(Data_clear_long$Vie)
Data_clear_long$Participation <- as.factor(Data_clear_long$Participation)

model_grade_category <- lmer(Grades ~ Category + Age + Genre + Education + Vie + Participation + (1 | ResponseId), data = Data_clear_long)

summary(model_grade_category)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Grades ~ Category + Age + Genre + Education + Vie + Participation +  
    ##     (1 | ResponseId)
    ##    Data: Data_clear_long
    ## 
    ## REML criterion at convergence: 276.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.34041 -0.54453  0.02359  0.55049  2.39668 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.6852   0.8277  
    ##  Residual               1.1149   1.0559  
    ## Number of obs: 88, groups:  ResponseId, 22
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     4.29374    1.02311   4.197
    ## Categorylow    -0.38636    0.22511  -1.716
    ## Age             0.04436    0.02838   1.563
    ## Genre2          0.67148    0.60230   1.115
    ## Genre3          0.86067    1.19413   0.721
    ## Education3     -2.23624    0.87738  -2.549
    ## Education5     -2.23160    0.97444  -2.290
    ## Education7     -2.05761    0.88473  -2.326
    ## Education8     -3.95358    1.61891  -2.442
    ## Vie2           -0.28528    0.51232  -0.557
    ## Participation2 -0.56548    0.57881  -0.977
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.110                                                        
    ## Age         -0.608  0.000                                                 
    ## Genre2      -0.219  0.000  0.365                                          
    ## Genre3      -0.195  0.000  0.202  0.244                                   
    ## Education3  -0.156  0.000 -0.475 -0.433 -0.133                            
    ## Education5  -0.391  0.000 -0.279 -0.314 -0.328  0.645                     
    ## Education7  -0.155  0.000 -0.350 -0.622 -0.181  0.744  0.618              
    ## Education8   0.282  0.000 -0.735 -0.618 -0.233  0.678  0.481  0.697       
    ## Vie2        -0.481  0.000  0.089 -0.149  0.029  0.173  0.411  0.194  0.024
    ## Participtn2 -0.321  0.000 -0.206  0.078  0.139  0.138  0.305 -0.093 -0.006
    ##             Vie2  
    ## Categorylow       
    ## Age               
    ## Genre2            
    ## Genre3            
    ## Education3        
    ## Education5        
    ## Education7        
    ## Education8        
    ## Vie2              
    ## Participtn2  0.237

### Conclusion for Linear Mixed_effects Model for high and low stories

The model suggests that Category (high vs. low stories) does not have a
significant effect on grades, though there is a trend towards
low-category stories receiving slightly lower ratings.

Age, Genre, and Vie do not have significant effects on the grades.

Education is the most influential factor, with higher education levels
(particularly Education8) associated with lower ratings.

Participation does not significantly impact the grades either.

------------------------------------------------------------------------

### Linear Mixed_effects Model for Entertainment grades

#### Long table for Entertainment grades

``` r
Data_clear_long_Entertainment <- pivot_longer(
  Data_clear_large, 
  cols = ends_with(c("_L_1_high", "_L_1_low")),
  names_to = "Stories",
  values_to = "Grades"
)
```

#### Move “Question” and “Response” to the first two columns

``` r
Data_clear_long_Entertainment <- Data_clear_long_Entertainment %>%
  select(Stories, Grades, everything())

print(Data_clear_long_Entertainment)
```

    ## # A tibble: 176 × 78
    ##    Stories       Grades StartDate EndDate Status Progress Duration (in seconds…¹
    ##    <chr>          <dbl> <chr>     <chr>   <chr>  <chr>    <chr>                 
    ##  1 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  2 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  3 exp_high_SAE…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  4 exp_high_SAD…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  5 exp_low_ANJE…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  6 exp_low_CLCH…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  7 exp_low_COHE…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  8 exp_low_VEYV…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  9 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## 10 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## # ℹ 166 more rows
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 71 more variables: Finished <chr>, RecordedDate <chr>, ResponseId <chr>,
    ## #   DistributionChannel <chr>, UserLanguage <chr>, Q_RecaptchaScore <chr>,
    ## #   Age <dbl>, Genre <dbl>, Education <dbl>, Groupe <dbl>,
    ## #   high_BEJEDI08_1 <dbl>, exp_high_BEJEDI08_L_2_high <dbl>,
    ## #   exp_high_BEJEDI08_L_3_high <dbl>, exp_high_BEJEDI08_L_4_high <dbl>, …

``` r
View(Data_clear_long_Entertainment)
```

#### Add the column “Category” to the dataset

``` r
Data_clear_long_Entertainment <- Data_clear_long_Entertainment %>%
  mutate(
    Category = case_when(
      grepl("(exp_high_BEJEDI08|exp_high_MAANAU12|exp_high_SADJSO02|exp_high_SAERBE09)", Stories) ~ "high",
      grepl("^(exp_low_VEYVEM11|exp_low_CLCHJO01|exp_low_ANJETI02|exp_low_COHEIG03)", Stories) ~ "low",
    )
  )%>%
  select(1:2, Category, everything())  # Moves Category to the 3rd position

Data_clear_long_Entertainment$Category<- as.factor(Data_clear_long_Entertainment$Category)
Data_clear_long_Entertainment$Grades <- as.numeric(as.character(Data_clear_long_Entertainment$Grades))
Data_clear_long_Entertainment$Age <- as.numeric(Data_clear_long_Entertainment$Age)
Data_clear_long_Entertainment$Genre <- as.factor(Data_clear_long_Entertainment$Genre)
Data_clear_long_Entertainment$Education <- as.factor(Data_clear_long_Entertainment$Education)
Data_clear_long_Entertainment$Vie <- as.factor(Data_clear_long_Entertainment$Vie)
Data_clear_long_Entertainment$Participation <- as.factor(Data_clear_long_Entertainment$Participation)

View(Data_clear_long_Entertainment)
```

``` r
Data_clear_long_Entertainment$Grades <- as.numeric(as.character(Data_clear_long_Entertainment$Grades))

model_Entertainment_category <- lmer(Grades ~ Category + Age + Genre + Education + Vie + Participation + (1 | ResponseId), data = Data_clear_long_Entertainment)

summary(model_Entertainment_category)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Grades ~ Category + Age + Genre + Education + Vie + Participation +  
    ##     (1 | ResponseId)
    ##    Data: Data_clear_long_Entertainment
    ## 
    ## REML criterion at convergence: 251.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7943 -0.6707  0.1081  0.5763  2.5645 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.6999   0.8366  
    ##  Residual               0.7741   0.8798  
    ## Number of obs: 88, groups:  ResponseId, 22
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     3.69877    0.98353   3.761
    ## Categorylow    -0.40909    0.18758  -2.181
    ## Age             0.01943    0.02732   0.711
    ## Genre2          0.27238    0.57987   0.470
    ## Genre3          1.06162    1.14968   0.923
    ## Education3     -1.41558    0.84471  -1.676
    ## Education5     -1.50277    0.93816  -1.602
    ## Education7     -1.36469    0.85179  -1.602
    ## Education8     -2.32468    1.55864  -1.491
    ## Vie2            0.08386    0.49325   0.170
    ## Participation2 -0.26501    0.55726  -0.476
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.095                                                        
    ## Age         -0.609  0.000                                                 
    ## Genre2      -0.219  0.000  0.365                                          
    ## Genre3      -0.195  0.000  0.202  0.244                                   
    ## Education3  -0.156  0.000 -0.475 -0.433 -0.133                            
    ## Education5  -0.392  0.000 -0.279 -0.314 -0.328  0.645                     
    ## Education7  -0.155  0.000 -0.350 -0.622 -0.181  0.744  0.618              
    ## Education8   0.283  0.000 -0.735 -0.618 -0.233  0.678  0.481  0.697       
    ## Vie2        -0.482  0.000  0.089 -0.149  0.029  0.173  0.411  0.194  0.024
    ## Participtn2 -0.321  0.000 -0.206  0.078  0.139  0.138  0.305 -0.093 -0.006
    ##             Vie2  
    ## Categorylow       
    ## Age               
    ## Genre2            
    ## Genre3            
    ## Education3        
    ## Education5        
    ## Education7        
    ## Education8        
    ## Vie2              
    ## Participtn2  0.237

### Conclusion of Linear Mixed_effects Model for Entertainment grades

Category (high vs. low stories) has a marginally significant effect on
entertainment ratings, with low-category stories receiving slightly
lower ratings.

Age, Genre, Education, Vie, and Participation do not have significant
effects on the grade given by participants to the entertainment
question.

The Education variable shows a trend where participants with higher
education levels tend to rate the entertainment value of the stories
lower, but these effects are not statistically significant.

Overall, the model suggests that factors like age, genre, education, and
participation do not strongly influence entertainment ratings, but the
Category of the story (high vs. low) may have a small impact.

------------------------------------------------------------------------

### Linear Mixed_effects Model for Emotionality grades

#### Long table for Emotional grades

``` r
Data_clear_long_Emotionality <- pivot_longer(
  Data_clear_large, 
  cols = ends_with(c("_L_2_high", "_L_2_low")),
  names_to = "Stories",
  values_to = "Grades"
)
```

#### Move “Question” and “Response” to the first two columns

``` r
Data_clear_long_Emotionality <- Data_clear_long_Emotionality %>%
  select(Stories, Grades, everything())

print(Data_clear_long_Emotionality)
```

    ## # A tibble: 176 × 78
    ##    Stories       Grades StartDate EndDate Status Progress Duration (in seconds…¹
    ##    <chr>          <dbl> <chr>     <chr>   <chr>  <chr>    <chr>                 
    ##  1 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  2 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  3 exp_high_SAE…      3 2024-09-… 2024-0… 0      100      2104                  
    ##  4 exp_high_SAD…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  5 exp_low_ANJE…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  6 exp_low_CLCH…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  7 exp_low_COHE…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  8 exp_low_VEYV…      5 2024-09-… 2024-0… 0      100      2104                  
    ##  9 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## 10 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## # ℹ 166 more rows
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 71 more variables: Finished <chr>, RecordedDate <chr>, ResponseId <chr>,
    ## #   DistributionChannel <chr>, UserLanguage <chr>, Q_RecaptchaScore <chr>,
    ## #   Age <dbl>, Genre <dbl>, Education <dbl>, Groupe <dbl>,
    ## #   high_BEJEDI08_1 <dbl>, exp_high_BEJEDI08_L_1_high <dbl>,
    ## #   exp_high_BEJEDI08_L_3_high <dbl>, exp_high_BEJEDI08_L_4_high <dbl>, …

``` r
View(Data_clear_long_Emotionality)
```

#### Add the column “Category” to the dataset

``` r
Data_clear_long_Emotionality <- Data_clear_long_Emotionality %>%
  mutate(
    Category = case_when(
      grepl("(exp_high_BEJEDI08|exp_high_MAANAU12|exp_high_SADJSO02|exp_high_SAERBE09)", Stories) ~ "high",
      grepl("^(exp_low_VEYVEM11|exp_low_CLCHJO01|exp_low_ANJETI02|exp_low_COHEIG03)", Stories) ~ "low",
    )
  )%>%
  select(1:2, Category, everything())  # Moves Category to the 3rd position

Data_clear_long_Emotionality$Category<- as.factor(Data_clear_long_Emotionality$Category)
Data_clear_long_Emotionality$Grades <- as.numeric(as.character(Data_clear_long_Emotionality$Grades))
Data_clear_long_Emotionality$Age <- as.numeric(Data_clear_long_Emotionality$Age)
Data_clear_long_Emotionality$Genre <- as.factor(Data_clear_long_Emotionality$Genre)
Data_clear_long_Emotionality$Education <- as.factor(Data_clear_long_Emotionality$Education)
Data_clear_long_Emotionality$Vie <- as.factor(Data_clear_long_Emotionality$Vie)
Data_clear_long_Emotionality$Participation <- as.factor(Data_clear_long_Emotionality$Participation)

View(Data_clear_long_Emotionality)
```

``` r
Data_clear_long_Emotionality$Grades <- as.numeric(as.character(Data_clear_long_Emotionality$Grades))

model_Emotionality_category <- lmer(Grades ~ Category + Age + Genre + Education + Vie + Participation + (1 | ResponseId), data = Data_clear_long_Emotionality)

summary(model_Entertainment_category)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Grades ~ Category + Age + Genre + Education + Vie + Participation +  
    ##     (1 | ResponseId)
    ##    Data: Data_clear_long_Entertainment
    ## 
    ## REML criterion at convergence: 251.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7943 -0.6707  0.1081  0.5763  2.5645 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.6999   0.8366  
    ##  Residual               0.7741   0.8798  
    ## Number of obs: 88, groups:  ResponseId, 22
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     3.69877    0.98353   3.761
    ## Categorylow    -0.40909    0.18758  -2.181
    ## Age             0.01943    0.02732   0.711
    ## Genre2          0.27238    0.57987   0.470
    ## Genre3          1.06162    1.14968   0.923
    ## Education3     -1.41558    0.84471  -1.676
    ## Education5     -1.50277    0.93816  -1.602
    ## Education7     -1.36469    0.85179  -1.602
    ## Education8     -2.32468    1.55864  -1.491
    ## Vie2            0.08386    0.49325   0.170
    ## Participation2 -0.26501    0.55726  -0.476
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.095                                                        
    ## Age         -0.609  0.000                                                 
    ## Genre2      -0.219  0.000  0.365                                          
    ## Genre3      -0.195  0.000  0.202  0.244                                   
    ## Education3  -0.156  0.000 -0.475 -0.433 -0.133                            
    ## Education5  -0.392  0.000 -0.279 -0.314 -0.328  0.645                     
    ## Education7  -0.155  0.000 -0.350 -0.622 -0.181  0.744  0.618              
    ## Education8   0.283  0.000 -0.735 -0.618 -0.233  0.678  0.481  0.697       
    ## Vie2        -0.482  0.000  0.089 -0.149  0.029  0.173  0.411  0.194  0.024
    ## Participtn2 -0.321  0.000 -0.206  0.078  0.139  0.138  0.305 -0.093 -0.006
    ##             Vie2  
    ## Categorylow       
    ## Age               
    ## Genre2            
    ## Genre3            
    ## Education3        
    ## Education5        
    ## Education7        
    ## Education8        
    ## Vie2              
    ## Participtn2  0.237

### Conclusion for Linear Mixed_effects Model for Emotionality grades

Category (low vs high): The Categorylow effect is negative, suggesting
that lower emotionality stories might receive lower ratings, but this is
not statistically significant.

Age, Genre, Education, Vie, and Participation: None of these variables
show strong, statistically significant effects on emotionality ratings
in this model.

------------------------------------------------------------------------

### Linear Mixed_effects Model for Memorable grades

#### Long table for Memorable grades

``` r
Data_clear_long_Memorable <- pivot_longer(
  Data_clear_large, 
  cols = ends_with(c("_L_3_high", "_L_3_low")),
  names_to = "Stories",
  values_to = "Grades"
)
```

#### Move “Question” and “Response” to the first two columns

``` r
Data_clear_long_Memorable <- Data_clear_long_Memorable %>%
  select(Stories, Grades, everything())

print(Data_clear_long_Memorable)
```

    ## # A tibble: 176 × 78
    ##    Stories       Grades StartDate EndDate Status Progress Duration (in seconds…¹
    ##    <chr>          <dbl> <chr>     <chr>   <chr>  <chr>    <chr>                 
    ##  1 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  2 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  3 exp_high_SAE…      3 2024-09-… 2024-0… 0      100      2104                  
    ##  4 exp_high_SAD…      3 2024-09-… 2024-0… 0      100      2104                  
    ##  5 exp_low_ANJE…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  6 exp_low_CLCH…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  7 exp_low_COHE…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  8 exp_low_VEYV…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  9 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## 10 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## # ℹ 166 more rows
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 71 more variables: Finished <chr>, RecordedDate <chr>, ResponseId <chr>,
    ## #   DistributionChannel <chr>, UserLanguage <chr>, Q_RecaptchaScore <chr>,
    ## #   Age <dbl>, Genre <dbl>, Education <dbl>, Groupe <dbl>,
    ## #   high_BEJEDI08_1 <dbl>, exp_high_BEJEDI08_L_1_high <dbl>,
    ## #   exp_high_BEJEDI08_L_2_high <dbl>, exp_high_BEJEDI08_L_4_high <dbl>, …

``` r
View(Data_clear_long_Memorable)
```

#### Add the column “Category” to the dataset

``` r
Data_clear_long_Memorable <- Data_clear_long_Memorable %>%
  mutate(
    Category = case_when(
      grepl("(exp_high_BEJEDI08|exp_high_MAANAU12|exp_high_SADJSO02|exp_high_SAERBE09)", Stories) ~ "high",
      grepl("^(exp_low_VEYVEM11|exp_low_CLCHJO01|exp_low_ANJETI02|exp_low_COHEIG03)", Stories) ~ "low",
    )
  )%>%
  select(1:2, Category, everything())  # Moves Category to the 3rd position

Data_clear_long_Memorable$Category<- as.factor(Data_clear_long_Memorable$Category)
Data_clear_long_Memorable$Grades <- as.numeric(as.character(Data_clear_long_Memorable$Grades))
Data_clear_long_Memorable$Age <- as.numeric(Data_clear_long_Memorable$Age)
Data_clear_long_Memorable$Genre <- as.factor(Data_clear_long_Memorable$Genre)
Data_clear_long_Memorable$Education <- as.factor(Data_clear_long_Memorable$Education)
Data_clear_long_Memorable$Vie <- as.factor(Data_clear_long_Memorable$Vie)
Data_clear_long_Memorable$Participation <- as.factor(Data_clear_long_Memorable$Participation)

View(Data_clear_long_Memorable)
```

``` r
Data_clear_long_Memorable$Grades <- as.numeric(as.character(Data_clear_long_Memorable$Grades))

model_Memorable_category <- lmer(Grades ~ Category + Age + Genre + Education + Vie + Participation + (1 | ResponseId), data = Data_clear_long_Memorable)

summary(model_Memorable_category)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Grades ~ Category + Age + Genre + Education + Vie + Participation +  
    ##     (1 | ResponseId)
    ##    Data: Data_clear_long_Memorable
    ## 
    ## REML criterion at convergence: 241.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.73785 -0.64574 -0.06207  0.64016  2.43410 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.3707   0.6088  
    ##  Residual               0.7224   0.8499  
    ## Number of obs: 88, groups:  ResponseId, 22
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     3.70370    0.77437   4.783
    ## Categorylow    -0.54545    0.18121  -3.010
    ## Age             0.02605    0.02146   1.214
    ## Genre2          0.23455    0.45550   0.515
    ## Genre3          0.75573    0.90308   0.837
    ## Education3     -1.31044    0.66353  -1.975
    ## Education5     -1.28594    0.73694  -1.745
    ## Education7     -1.00098    0.66909  -1.496
    ## Education8     -2.28477    1.22433  -1.866
    ## Vie2           -0.28083    0.38745  -0.725
    ## Participation2 -0.54053    0.43774  -1.235
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.117                                                        
    ## Age         -0.608  0.000                                                 
    ## Genre2      -0.219  0.000  0.365                                          
    ## Genre3      -0.195  0.000  0.202  0.244                                   
    ## Education3  -0.156  0.000 -0.475 -0.433 -0.133                            
    ## Education5  -0.391  0.000 -0.279 -0.314 -0.328  0.645                     
    ## Education7  -0.155  0.000 -0.350 -0.622 -0.181  0.744  0.618              
    ## Education8   0.282  0.000 -0.735 -0.618 -0.233  0.678  0.481  0.697       
    ## Vie2        -0.481  0.000  0.089 -0.149  0.029  0.173  0.411  0.194  0.024
    ## Participtn2 -0.321  0.000 -0.206  0.078  0.139  0.138  0.305 -0.093 -0.006
    ##             Vie2  
    ## Categorylow       
    ## Age               
    ## Genre2            
    ## Genre3            
    ## Education3        
    ## Education5        
    ## Education7        
    ## Education8        
    ## Vie2              
    ## Participtn2  0.237

### Conclusion for Linear Mixed_effects Model for Memorable grades

Category (low vs high): The significant negative effect for Categorylow
(-0.50000) suggests that stories rated as less memorable tend to get
lower scores, which is a statistically significant effect.

Age, Genre, Education, Vie, and Participation: None of these factors
show strong, statistically significant effects on memorability ratings
in this model.

------------------------------------------------------------------------

### Linear Mixed_effects Model for Originality grades

#### Long table for Originality grades

``` r
Data_clear_long_Originality <- pivot_longer(
  Data_clear_large, 
  cols = ends_with(c("_L_4_high", "_L_4_low")),
  names_to = "Stories",
  values_to = "Grades"
)
```

#### Move “Question” and “Response” to the first two columns

``` r
Data_clear_long_Originality <- Data_clear_long_Originality %>%
  select(Stories, Grades, everything())

print(Data_clear_long_Originality)
```

    ## # A tibble: 176 × 78
    ##    Stories       Grades StartDate EndDate Status Progress Duration (in seconds…¹
    ##    <chr>          <dbl> <chr>     <chr>   <chr>  <chr>    <chr>                 
    ##  1 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  2 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  3 exp_high_SAE…      3 2024-09-… 2024-0… 0      100      2104                  
    ##  4 exp_high_SAD…      2 2024-09-… 2024-0… 0      100      2104                  
    ##  5 exp_low_ANJE…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  6 exp_low_CLCH…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  7 exp_low_COHE…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  8 exp_low_VEYV…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  9 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## 10 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## # ℹ 166 more rows
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 71 more variables: Finished <chr>, RecordedDate <chr>, ResponseId <chr>,
    ## #   DistributionChannel <chr>, UserLanguage <chr>, Q_RecaptchaScore <chr>,
    ## #   Age <dbl>, Genre <dbl>, Education <dbl>, Groupe <dbl>,
    ## #   high_BEJEDI08_1 <dbl>, exp_high_BEJEDI08_L_1_high <dbl>,
    ## #   exp_high_BEJEDI08_L_2_high <dbl>, exp_high_BEJEDI08_L_3_high <dbl>, …

``` r
View(Data_clear_long_Originality)
```

#### Add the column “Category” to the dataset

``` r
Data_clear_long_Originality <- Data_clear_long_Originality %>%
  mutate(
    Category = case_when(
      grepl("(exp_high_BEJEDI08|exp_high_MAANAU12|exp_high_SADJSO02|exp_high_SAERBE09)", Stories) ~ "high",
      grepl("^(exp_low_VEYVEM11|exp_low_CLCHJO01|exp_low_ANJETI02|exp_low_COHEIG03)", Stories) ~ "low",
    )
  )%>%
  select(1:2, Category, everything())  # Moves Category to the 3rd position

Data_clear_long_Originality$Category<- as.factor(Data_clear_long_Originality$Category)
Data_clear_long_Originality$Grades <- as.numeric(as.character(Data_clear_long_Originality$Grades))
Data_clear_long_Originality$Age <- as.numeric(Data_clear_long_Originality$Age)
Data_clear_long_Originality$Genre <- as.factor(Data_clear_long_Originality$Genre)
Data_clear_long_Originality$Education <- as.factor(Data_clear_long_Originality$Education)
Data_clear_long_Originality$Vie <- as.factor(Data_clear_long_Originality$Vie)
Data_clear_long_Originality$Participation <- as.factor(Data_clear_long_Originality$Participation)

View(Data_clear_long_Originality)
```

``` r
Data_clear_long_Originality$Grades <- as.numeric(as.character(Data_clear_long_Originality$Grades))

model_Originality_category <- lmer(Grades ~ Category + Age + Genre + Education + Vie + Participation + (1 | ResponseId), data = Data_clear_long_Originality)

summary(model_Originality_category)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Grades ~ Category + Age + Genre + Education + Vie + Participation +  
    ##     (1 | ResponseId)
    ##    Data: Data_clear_long_Originality
    ## 
    ## REML criterion at convergence: 238.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.66656 -0.62968 -0.04228  0.52996  2.87949 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.4303   0.6559  
    ##  Residual               0.6799   0.8246  
    ## Number of obs: 88, groups:  ResponseId, 22
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)     4.450758   0.807268   5.513
    ## Categorylow    -0.340909   0.175796  -1.939
    ## Age            -0.006763   0.022396  -0.302
    ## Genre2          0.228476   0.475291   0.481
    ## Genre3         -0.289453   0.942328  -0.307
    ## Education3     -0.970527   0.692366  -1.402
    ## Education5     -0.835303   0.768961  -1.086
    ## Education7     -0.956263   0.698168  -1.370
    ## Education8     -1.292174   1.277530  -1.011
    ## Vie2           -0.232321   0.404291  -0.575
    ## Participation2 -0.698049   0.456758  -1.528
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.109                                                        
    ## Age         -0.608  0.000                                                 
    ## Genre2      -0.219  0.000  0.365                                          
    ## Genre3      -0.195  0.000  0.202  0.244                                   
    ## Education3  -0.156  0.000 -0.475 -0.433 -0.133                            
    ## Education5  -0.391  0.000 -0.279 -0.314 -0.328  0.645                     
    ## Education7  -0.155  0.000 -0.350 -0.622 -0.181  0.744  0.618              
    ## Education8   0.282  0.000 -0.735 -0.618 -0.233  0.678  0.481  0.697       
    ## Vie2        -0.481  0.000  0.089 -0.149  0.029  0.173  0.411  0.194  0.024
    ## Participtn2 -0.321  0.000 -0.206  0.078  0.139  0.138  0.305 -0.093 -0.006
    ##             Vie2  
    ## Categorylow       
    ## Age               
    ## Genre2            
    ## Genre3            
    ## Education3        
    ## Education5        
    ## Education7        
    ## Education8        
    ## Vie2              
    ## Participtn2  0.237

### Conclusion for Linear Mixed_effects Model for Originality grades

Category (low vs. high originality): The Categorylow variable has a
marginally significant effect. Low stories are rated lower in
originality compared to high stories, but the effect is just on the edge
of statistical significance with a t-value of -1.912.

Age, Genre, Education, Vie, and Participation: None of these factors
show strong, statistically significant effects on originality ratings.

------------------------------------------------------------------------

### Linear Mixed_effects Model for Engagement grades

#### Long table for Engagement grades

``` r
Data_clear_long_Engagement <- pivot_longer(
  Data_clear_large, 
  cols = ends_with(c("_L_5_high", "_L_5_low")),
  names_to = "Stories",
  values_to = "Grades"
)
```

#### Move “Question” and “Response” to the first two columns

``` r
Data_clear_long_Engagement <- Data_clear_long_Engagement %>%
  select(Stories, Grades, everything())

print(Data_clear_long_Engagement)
```

    ## # A tibble: 176 × 78
    ##    Stories       Grades StartDate EndDate Status Progress Duration (in seconds…¹
    ##    <chr>          <dbl> <chr>     <chr>   <chr>  <chr>    <chr>                 
    ##  1 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  2 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  3 exp_high_SAE…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  4 exp_high_SAD…      3 2024-09-… 2024-0… 0      100      2104                  
    ##  5 exp_low_ANJE…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  6 exp_low_CLCH…     NA 2024-09-… 2024-0… 0      100      2104                  
    ##  7 exp_low_COHE…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  8 exp_low_VEYV…      4 2024-09-… 2024-0… 0      100      2104                  
    ##  9 exp_high_BEJ…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## 10 exp_high_MAA…     NA 2024-09-… 2024-0… 0      100      1081                  
    ## # ℹ 166 more rows
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 71 more variables: Finished <chr>, RecordedDate <chr>, ResponseId <chr>,
    ## #   DistributionChannel <chr>, UserLanguage <chr>, Q_RecaptchaScore <chr>,
    ## #   Age <dbl>, Genre <dbl>, Education <dbl>, Groupe <dbl>,
    ## #   high_BEJEDI08_1 <dbl>, exp_high_BEJEDI08_L_1_high <dbl>,
    ## #   exp_high_BEJEDI08_L_2_high <dbl>, exp_high_BEJEDI08_L_3_high <dbl>, …

``` r
View(Data_clear_long_Engagement)
```

#### Add the column “Category” to the dataset

``` r
Data_clear_long_Engagement <- Data_clear_long_Engagement %>%
  mutate(
    Category = case_when(
      grepl("(exp_high_BEJEDI08|exp_high_MAANAU12|exp_high_SADJSO02|exp_high_SAERBE09)", Stories) ~ "high",
      grepl("^(exp_low_VEYVEM11|exp_low_CLCHJO01|exp_low_ANJETI02|exp_low_COHEIG03)", Stories) ~ "low",
    )
  )%>%
  select(1:2, Category, everything())  # Moves Category to the 3rd position

Data_clear_long_Engagement$Category<- as.factor(Data_clear_long_Engagement$Category)
Data_clear_long_Engagement$Grades <- as.numeric(as.character(Data_clear_long_Engagement$Grades))
Data_clear_long_Engagement$Age <- as.numeric(Data_clear_long_Engagement$Age)
Data_clear_long_Engagement$Genre <- as.factor(Data_clear_long_Engagement$Genre)
Data_clear_long_Engagement$Education <- as.factor(Data_clear_long_Engagement$Education)
Data_clear_long_Engagement$Vie <- as.factor(Data_clear_long_Engagement$Vie)
Data_clear_long_Engagement$Participation <- as.factor(Data_clear_long_Engagement$Participation)

View(Data_clear_long_Engagement)
```

``` r
Data_clear_long_Engagement$Grades <- as.numeric(as.character(Data_clear_long_Engagement$Grades))

model_Engagement_category <- lmer(Grades ~ Category + Age + Genre + Education + Vie + Participation + (1 | ResponseId), data = Data_clear_long_Engagement)

summary(model_Engagement_category)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Grades ~ Category + Age + Genre + Education + Vie + Participation +  
    ##     (1 | ResponseId)
    ##    Data: Data_clear_long_Engagement
    ## 
    ## REML criterion at convergence: 256.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2840 -0.6189 -0.0484  0.5642  2.6008 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.4265   0.6530  
    ##  Residual               0.8818   0.9391  
    ## Number of obs: 88, groups:  ResponseId, 22
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     3.45707    0.83909   4.120
    ## Categorylow    -0.40909    0.20021  -2.043
    ## Age             0.01898    0.02325   0.816
    ## Genre2          0.27022    0.49343   0.548
    ## Genre3          0.79870    0.97829   0.816
    ## Education3     -1.60317    0.71879  -2.230
    ## Education5     -1.23781    0.79831  -1.551
    ## Education7     -1.20517    0.72481  -1.663
    ## Education8     -2.47999    1.32629  -1.870
    ## Vie2            0.23908    0.41972   0.570
    ## Participation2 -0.29173    0.47419  -0.615
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.119                                                        
    ## Age         -0.608  0.000                                                 
    ## Genre2      -0.219  0.000  0.365                                          
    ## Genre3      -0.194  0.000  0.202  0.244                                   
    ## Education3  -0.156  0.000 -0.475 -0.433 -0.133                            
    ## Education5  -0.391  0.000 -0.279 -0.314 -0.328  0.645                     
    ## Education7  -0.154  0.000 -0.350 -0.622 -0.181  0.744  0.618              
    ## Education8   0.282  0.000 -0.735 -0.618 -0.233  0.678  0.481  0.697       
    ## Vie2        -0.481  0.000  0.089 -0.149  0.029  0.173  0.411  0.194  0.024
    ## Participtn2 -0.320  0.000 -0.206  0.078  0.139  0.138  0.305 -0.093 -0.006
    ##             Vie2  
    ## Categorylow       
    ## Age               
    ## Genre2            
    ## Genre3            
    ## Education3        
    ## Education5        
    ## Education7        
    ## Education8        
    ## Vie2              
    ## Participtn2  0.237

### Conclusion for Linear Mixed_effects Model for Engagement grades

Category (low vs. high engagement): The Categorylow variable has a
borderline significant effect on engagement, suggesting that low stories
tend to be rated lower for engagement than high stories. However, the
effect is not very strong, with a t-value close to the threshold for
statistical significance.

Age, Genre, Education, Vie, and Participation: None of these predictors
show strong, statistically significant effects on engagement ratings.

## 8. Common story

``` r
# Merge the columns that start with "RIRO12_1" and so on
Data_clear_large <- Data_clear_large %>%
  mutate(RIRO12_1 = coalesce(select(., starts_with("RIRO12_1"))[[1]], select(., starts_with("RIRO12_1"))[[2]]))

Data_clear_large <- Data_clear_large %>%
  mutate(RIRO12_L_1 = coalesce(select(., starts_with("RIRO12_L_1"))[[1]], select(., starts_with("RIRO12_L_1"))[[2]]))

Data_clear_large <- Data_clear_large %>%
  mutate(RIRO12_L_2 = coalesce(select(., starts_with("RIRO12_L_2"))[[1]], select(., starts_with("RIRO12_L_2"))[[2]]))

Data_clear_large <- Data_clear_large %>%
  mutate(RIRO12_L_3 = coalesce(select(., starts_with("RIRO12_L_3"))[[1]], select(., starts_with("RIRO12_L_3"))[[2]]))

Data_clear_large <- Data_clear_large %>%
  mutate(RIRO12_L_4 = coalesce(select(., starts_with("RIRO12_L_4"))[[1]], select(., starts_with("RIRO12_L_4"))[[2]]))

Data_clear_large <- Data_clear_large %>%
  mutate(RIRO12_L_5 = coalesce(select(., starts_with("RIRO12_L_5"))[[1]], select(., starts_with("RIRO12_L_5"))[[2]]))

# View the updated dataset
head(Data_clear_large)
```

    ## # A tibble: 6 × 90
    ##   StartDate EndDate Status Progress Duration (in seconds…¹ Finished RecordedDate
    ##   <chr>     <chr>   <chr>  <chr>    <chr>                  <chr>    <chr>       
    ## 1 2024-09-… 2024-0… 0      100      2104                   1        2024-09-17 …
    ## 2 2024-09-… 2024-0… 0      100      1081                   1        2024-09-17 …
    ## 3 2024-09-… 2024-0… 0      100      4061                   1        2024-09-18 …
    ## 4 2024-09-… 2024-0… 0      100      915                    1        2024-09-23 …
    ## 5 2024-09-… 2024-0… 0      100      2786                   1        2024-09-23 …
    ## 6 2024-09-… 2024-0… 0      100      1578                   1        2024-09-24 …
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 83 more variables: ResponseId <chr>, DistributionChannel <chr>,
    ## #   UserLanguage <chr>, Q_RecaptchaScore <chr>, Age <dbl>, Genre <dbl>,
    ## #   Education <dbl>, Groupe <dbl>, high_BEJEDI08_1 <dbl>,
    ## #   exp_high_BEJEDI08_L_1_high <dbl>, exp_high_BEJEDI08_L_2_high <dbl>,
    ## #   exp_high_BEJEDI08_L_3_high <dbl>, exp_high_BEJEDI08_L_4_high <dbl>,
    ## #   exp_high_BEJEDI08_L_5_high <dbl>, high_MAANAU12_1 <dbl>, …

``` r
#Deleting useless columns
Data_clear_large <- Data_clear_large %>%
  select(-c(40:45, 70:75))
```

``` r
common_story_distribution <- Data_clear_large %>%
  select(starts_with("RIRO12"))

par(mfrow = c(2, 2))
for (col in names(common_story_distribution)) {
  hist(common_story_distribution[[col]], breaks = 10, main = paste("Histogram of", col), xlab = "Grades", col = "lightblue")
}
```

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-177-1.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-177-2.png" style="display: block; margin: auto;" />

``` r
shapiro_results_common_story <- lapply(common_story_distribution, shapiro.test)

shapiro_results_common_story
```

    ## $RIRO12_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.86546, p-value = 0.00645
    ## 
    ## 
    ## $RIRO12_L_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.80573, p-value = 0.000616
    ## 
    ## 
    ## $RIRO12_L_2
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.89283, p-value = 0.02142
    ## 
    ## 
    ## $RIRO12_L_3
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.87895, p-value = 0.01154
    ## 
    ## 
    ## $RIRO12_L_4
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.84866, p-value = 0.003214
    ## 
    ## 
    ## $RIRO12_L_5
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.85245, p-value = 0.003752

``` r
# Higher than 0.05 = normally distributed
```

``` r
# Calculate the variance of the grades
variance_riro12 <- var(Data_clear_large$RIRO12_1)
print(variance_riro12)
```

    ## [1] 1.160173
