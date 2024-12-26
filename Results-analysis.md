Beyond Stories: Exploring the Influence of Emotional Indicators on the
Subjective Quality Rating of Stories
================
Rachel Ferati
2024-12-26

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

    ## # A tibble: 216 × 78
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
    ## # ℹ 206 more rows
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
    ##   18.00   23.50   27.00   32.48   36.00   63.00

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

    ## Mean Age: 32.47826

``` r
cat("Standard Deviation of Age:", sd_age, "\n")
```

    ## Standard Deviation of Age: 12.61265

#### Frequency distribution of Age

``` r
age_distribution <- table(Data_clear_large$Age)
print(age_distribution)
```

    ## 
    ## 18 20 23 24 25 27 31 33 39 42 45 56 59 63 
    ##  1  1  4  2  1  3  3  2  1  1  1  1  1  1

#### Percentage of Age

``` r
frequency_table_age <- table(Data_clear_large$Age)

percentage_table_age <- prop.table(frequency_table_age) * 100

percentage_age_df <- as.data.frame(percentage_table_age)
colnames(percentage_age_df) <- c("Age", "Percentage")

print(percentage_age_df)
```

    ##    Age Percentage
    ## 1   18   4.347826
    ## 2   20   4.347826
    ## 3   23  17.391304
    ## 4   24   8.695652
    ## 5   25   4.347826
    ## 6   27  13.043478
    ## 7   31  13.043478
    ## 8   33   8.695652
    ## 9   39   4.347826
    ## 10  42   4.347826
    ## 11  45   4.347826
    ## 12  56   4.347826
    ## 13  59   4.347826
    ## 14  63   4.347826

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
    ##   1.000   1.000   2.000   1.609   2.000   3.000

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

    ## Mean Genre: 1.608696

``` r
cat("Standard Deviation of Genre:", sd_genre, "\n")
```

    ## Standard Deviation of Genre: 0.5830274

#### Frequency distribution of Genre

``` r
genre_distribution <- table(Data_clear_large$Genre)

print(genre_distribution)
```

    ## 
    ##  1  2  3 
    ## 10 12  1

#### Percentage of Gender

``` r
frequency_table_gender <- table(Data_clear_large$Genre)

percentage_table_gender <- prop.table(frequency_table_gender) * 100

percentage_gender_df <- as.data.frame(percentage_table_gender)
colnames(percentage_gender_df) <- c("Gender", "Percentage")

print(percentage_gender_df)
```

    ##   Gender Percentage
    ## 1      1  43.478261
    ## 2      2  52.173913
    ## 3      3   4.347826

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
    ##    2.00    3.00    5.00    5.13    7.00    8.00

#### Calculate Mean and SD of Education

``` r
mean_education <- mean(Data_clear_large$Education, na.rm = TRUE)
sd_education <- sd(Data_clear_large$Education, na.rm = TRUE)

cat("Mean Education:", mean_education, "\n")
```

    ## Mean Education: 5.130435

``` r
cat("Standard Deviation of Education:", sd_education, "\n")
```

    ## Standard Deviation of Education: 2.095544

#### Frequency distribution of Education

``` r
education_distribution <- table(Data_clear_large$Education)

print(education_distribution)
```

    ## 
    ## 2 3 5 7 8 
    ## 3 5 5 8 2

#### Percentage of Education

``` r
frequency_table_education <- table(Data_clear_large$Education)

percentage_table_education <- prop.table(frequency_table_education) * 100

percentage_education_df <- as.data.frame(percentage_table_education)
colnames(percentage_education_df) <- c("Level of Education", "Percentage")

print(percentage_education_df)
```

    ##   Level of Education Percentage
    ## 1                  2  13.043478
    ## 2                  3  21.739130
    ## 3                  5  21.739130
    ## 4                  7  34.782609
    ## 5                  8   8.695652

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
    ##   1.000   1.000   1.000   1.391   2.000   2.000

#### Calculate Mean and SD of Vie

``` r
mean_vie <- mean(Data_clear_large$Vie, na.rm = TRUE)
sd_vie <- sd(Data_clear_large$Vie, na.rm = TRUE)

cat("Mean Vie:", mean_vie, "\n")
```

    ## Mean Vie: 1.391304

``` r
cat("Standard Deviation of Vie:", sd_vie, "\n")
```

    ## Standard Deviation of Vie: 0.4990109

#### Frequency distribution of Vie

``` r
vie_distribution <- table(Data_clear_large$Vie)

print(vie_distribution)
```

    ## 
    ##  1  2 
    ## 14  9

#### Percentage of Similar living experience

``` r
frequency_table_vie <- table(Data_clear_large$Vie)

percentage_table_vie <- prop.table(frequency_table_vie) * 100

percentage_vie_df <- as.data.frame(percentage_table_vie)
colnames(percentage_vie_df) <- c("Similar living experience", "Percentage")

print(percentage_vie_df)
```

    ##   Similar living experience Percentage
    ## 1                         1   60.86957
    ## 2                         2   39.13043

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
    ##   1.000   1.000   2.000   1.696   2.000   2.000

#### Calculate Mean and SD of Participation

``` r
mean_participation <- mean(Data_clear_large$Participation, na.rm = TRUE)
sd_participation <- sd(Data_clear_large$Participation, na.rm = TRUE)

cat("Mean Participation:", mean_participation, "\n")
```

    ## Mean Participation: 1.695652

``` r
cat("Standard Deviation of Participation:", sd_participation, "\n")
```

    ## Standard Deviation of Participation: 0.470472

#### Frequency distribution of Participation

``` r
participation_distribution <- table(Data_clear_large$Participation)

print(participation_distribution)
```

    ## 
    ##  1  2 
    ##  7 16

#### Percentage of participation in the previous experiment

``` r
frequency_table_participation <- table(Data_clear_large$Participation)

percentage_table_participation <- prop.table(frequency_table_participation) * 100

percentage_participation_df <- as.data.frame(percentage_table_participation)
colnames(percentage_participation_df) <- c("Participation in previous experiment", "Percentage")

print(percentage_participation_df)
```

    ##   Participation in previous experiment Percentage
    ## 1                                    1   30.43478
    ## 2                                    2   69.56522

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
    ##    1.00    2.00    4.00    3.87    5.50    6.00

#### Calculate mean and SD of Habits for books

``` r
mean_habits_books <- mean(Data_clear_large$Habitudes_1, na.rm = TRUE)
sd_habits_books <- sd(Data_clear_large$Habitudes_1, na.rm = TRUE)

cat("Mean Books habits:", mean_habits_books, "\n")
```

    ## Mean Books habits: 3.869565

``` r
cat("Standard Deviation of Books habits:", sd_habits_books, "\n")
```

    ## Standard Deviation of Books habits: 1.713697

#### Frequency distribution habits of books

``` r
habits_books_distribution <- table(Data_clear_large$Habitudes_1)

print(habits_books_distribution)
```

    ## 
    ## 1 2 3 4 5 6 
    ## 1 6 4 2 4 6

#### Percentage of books habits

``` r
frequency_table_books <- table(Data_clear_large$Habitudes_1)

percentage_table_books <- prop.table(frequency_table_books) * 100

percentage_books_df <- as.data.frame(percentage_table_books)
colnames(percentage_books_df) <- c("Grade", "Percentage")

print(percentage_books_df)
```

    ##   Grade Percentage
    ## 1     1   4.347826
    ## 2     2  26.086957
    ## 3     3  17.391304
    ## 4     4   8.695652
    ## 5     5  17.391304
    ## 6     6  26.086957

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
    ##   1.000   1.000   2.000   2.261   3.000   5.000

#### Calculate mean and SD of Habits for audiobooks/podcasts

``` r
mean_habits_audiobooks <- mean(Data_clear_large$Habitudes_2, na.rm = TRUE)
sd_habits_audiobooks <- sd(Data_clear_large$Habitudes_2, na.rm = TRUE)

cat("Mean Audiobooks habits:", mean_habits_audiobooks, "\n")
```

    ## Mean Audiobooks habits: 2.26087

``` r
cat("Standard Deviation of Audiobooks habits:", sd_habits_audiobooks, "\n")
```

    ## Standard Deviation of Audiobooks habits: 1.421184

#### Frequency distribution habits of audiobooks

``` r
habits_audiobooks_distribution <- table(Data_clear_large$Habitudes_2)

print(habits_audiobooks_distribution)
```

    ## 
    ## 1 2 3 4 5 
    ## 9 7 2 2 3

#### Percentage of audiobooks habits

``` r
frequency_table_audiobooks <- table(Data_clear_large$Habitudes_2)

percentage_table_audiobooks <- prop.table(frequency_table_audiobooks) * 100

percentage_audiobooks_df <- as.data.frame(percentage_table_audiobooks)
colnames(percentage_audiobooks_df) <- c("Grade", "Percentage")

print(percentage_audiobooks_df)
```

    ##   Grade Percentage
    ## 1     1  39.130435
    ## 2     2  30.434783
    ## 3     3   8.695652
    ## 4     4   8.695652
    ## 5     5  13.043478

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
    ##   3.000   4.000   5.000   4.739   5.000   6.000

#### Calculate mean and SD of Habits for television

``` r
mean_habits_television <- mean(Data_clear_large$Habitudes_3, na.rm = TRUE)
sd_habits_television <- sd(Data_clear_large$Habitudes_3, na.rm = TRUE)

cat("Mean television habits:", mean_habits_television, "\n")
```

    ## Mean television habits: 4.73913

``` r
cat("Standard Deviation of television habits:", sd_habits_television, "\n")
```

    ## Standard Deviation of television habits: 0.8643122

#### Frequency distribution habits of television

``` r
habits_television_distribution <- table(Data_clear_large$Habitudes_3)

print(habits_television_distribution)
```

    ## 
    ##  3  4  5  6 
    ##  2  6 11  4

#### Percentage of television habits

``` r
frequency_table_television <- table(Data_clear_large$Habitudes_3)

percentage_table_television <- prop.table(frequency_table_television) * 100

percentage_television_df <- as.data.frame(percentage_table_television)
colnames(percentage_television_df) <- c("Grade", "Percentage")

print(percentage_television_df)
```

    ##   Grade Percentage
    ## 1     3   8.695652
    ## 2     4  26.086957
    ## 3     5  47.826087
    ## 4     6  17.391304

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
    ##   2.000   5.000   5.000   5.087   6.000   6.000

#### Calculate mean and SD of Habits for social media

``` r
mean_habits_socialmedia <- mean(Data_clear_large$Habitudes_4, na.rm = TRUE)
sd_habits_socialmedia <- sd(Data_clear_large$Habitudes_4, na.rm = TRUE)

cat("Mean socialmedia habits:", mean_habits_socialmedia, "\n")
```

    ## Mean socialmedia habits: 5.086957

``` r
cat("Standard Deviation of socialmedia habits:", sd_habits_socialmedia, "\n")
```

    ## Standard Deviation of socialmedia habits: 1.202764

#### Frequency distribution habits of television

``` r
habits_socialmedia_distribution <- table(Data_clear_large$Habitudes_4)

print(habits_socialmedia_distribution)
```

    ## 
    ##  2  3  5  6 
    ##  1  3  8 11

#### Percentage of television habits

``` r
frequency_table_socialmedia <- table(Data_clear_large$Habitudes_4)

percentage_table_socialmedia <- prop.table(frequency_table_socialmedia) * 100

percentage_socialmedia_df <- as.data.frame(percentage_table_socialmedia)
colnames(percentage_socialmedia_df) <- c("Grade", "Percentage")

print(percentage_socialmedia_df)
```

    ##   Grade Percentage
    ## 1     2   4.347826
    ## 2     3  13.043478
    ## 3     5  34.782609
    ## 4     6  47.826087

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
    ##   1.000   1.000   1.000   2.087   3.000   6.000

#### Calculate mean and SD of Habits for video games

``` r
mean_habits_videogames <- mean(Data_clear_large$Habitudes_5, na.rm = TRUE)
sd_habits_videogames <- sd(Data_clear_large$Habitudes_5, na.rm = TRUE)

cat("Mean videogames habits:", mean_habits_videogames, "\n")
```

    ## Mean videogames habits: 2.086957

``` r
cat("Standard Deviation of videogames habits:", sd_habits_videogames, "\n")
```

    ## Standard Deviation of videogames habits: 1.504933

#### Frequency distribution habits of video games

``` r
habits_videogames_distribution <- table(Data_clear_large$Habitudes_5)

print(habits_videogames_distribution)
```

    ## 
    ##  1  2  3  5  6 
    ## 12  4  4  2  1

#### Percentage of television habits

``` r
frequency_table_videogames <- table(Data_clear_large$Habitudes_5)

percentage_table_videogames <- prop.table(frequency_table_videogames) * 100

percentage_videogames_df <- as.data.frame(percentage_table_videogames)
colnames(percentage_videogames_df) <- c("Grade", "Percentage")

print(percentage_videogames_df)
```

    ##   Grade Percentage
    ## 1     1  52.173913
    ## 2     2  17.391304
    ## 3     3  17.391304
    ## 4     5   8.695652
    ## 5     6   4.347826

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
    ##    1.00    2.50    5.00    4.13    5.50    6.00

#### Calculate mean and SD of Habits for Oral stories

``` r
mean_habits_oralstories <- mean(Data_clear_large$Habitudes_6, na.rm = TRUE)
sd_habits_oralstories  <- sd(Data_clear_large$Habitudes_6, na.rm = TRUE)

cat("Mean oralstories habits:", mean_habits_oralstories , "\n")
```

    ## Mean oralstories habits: 4.130435

``` r
cat("Standard Deviation of oralstories habits:", sd_habits_oralstories , "\n")
```

    ## Standard Deviation of oralstories habits: 1.816699

#### Frequency distribution habits of oral stories

``` r
habits_oralstories_distribution <- table(Data_clear_large$Habitudes_6)

print(habits_oralstories_distribution)
```

    ## 
    ## 1 2 3 4 5 6 
    ## 3 3 2 1 8 6

#### Percentage of oral stories habits

``` r
frequency_table_oralstories <- table(Data_clear_large$Habitudes_6)

percentage_table_oralstories <- prop.table(frequency_table_oralstories) * 100

percentage_oralstories_df <- as.data.frame(percentage_table_oralstories)
colnames(percentage_oralstories_df) <- c("Grade", "Percentage")

print(percentage_oralstories_df)
```

    ##   Grade Percentage
    ## 1     1  13.043478
    ## 2     2  13.043478
    ## 3     3   8.695652
    ## 4     4   4.347826
    ## 5     5  34.782609
    ## 6     6  26.086957

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## high_BEJEDI08_1 high_BEJEDI08_1 3.333333 2.000000
    ## high_MAANAU12_1 high_MAANAU12_1 3.333333 1.000000
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
    ## [1] 3.630435
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.372082

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## low_ANJETI02_1 low_ANJETI02_1 2.777778 1.394433
    ## low_CLCHJO01_1 low_CLCHJO01_1 2.555556 1.333333
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
    ## [1] 3.173913
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.287622

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

    ## Warning: NA values were introduced during conversion. Count: 46

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

    ##                                                Column     Mean        SD
    ## exp_high_BEJEDI08_L_1_high exp_high_BEJEDI08_L_1_high 3.000000 1.4142136
    ## exp_high_MAANAU12_L_1_high exp_high_MAANAU12_L_1_high 2.555556 0.7264832
    ## exp_high_SAERBE09_L_1_high exp_high_SAERBE09_L_1_high 3.000000 1.0377490
    ## exp_high_SADJSO02_L_1_high exp_high_SADJSO02_L_1_high 3.500000 1.0919284

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
    ## [1] 3.065217
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.103573

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_high_BEJEDI08_L_2_high exp_high_BEJEDI08_L_2_high 3.666667 1.4142136
    ## exp_high_MAANAU12_L_2_high exp_high_MAANAU12_L_2_high 3.444444 1.0137938
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
    ## [1] 3.5
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.069787

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_high_BEJEDI08_L_3_high exp_high_BEJEDI08_L_3_high 3.333333 1.4142136
    ## exp_high_MAANAU12_L_3_high exp_high_MAANAU12_L_3_high 3.111111 0.7817360
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
    ## [1] 3.152174
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.032094

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_high_BEJEDI08_L_4_high exp_high_BEJEDI08_L_4_high 3.333333 0.8660254
    ## exp_high_MAANAU12_L_4_high exp_high_MAANAU12_L_4_high 2.888889 1.0540926
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
    ## [1] 2.934783
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.062537

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_high_BEJEDI08_L_5_high exp_high_BEJEDI08_L_5_high 3.222222 1.5634719
    ## exp_high_MAANAU12_L_5_high exp_high_MAANAU12_L_5_high 2.444444 0.5270463
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
    ## [1] 2.869565
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.147145

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_low_ANJETI02_L_1_low exp_low_ANJETI02_L_1_low 2.000000 1.2247449
    ## exp_low_CLCHJO01_L_1_low exp_low_CLCHJO01_L_1_low 2.000000 1.1180340
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
    ## [1] 2.586957
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.184644

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_low_ANJETI02_L_2_low exp_low_ANJETI02_L_2_low 3.333333 1.3228757
    ## exp_low_CLCHJO01_L_2_low exp_low_CLCHJO01_L_2_low 2.222222 1.6414763
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
    ## [1] 2.956522
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.264147

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_low_ANJETI02_L_3_low exp_low_ANJETI02_L_3_low 2.333333 1.0000000
    ## exp_low_CLCHJO01_L_3_low exp_low_CLCHJO01_L_3_low 2.111111 1.0540926
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
    ## [1] 2.543478
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.004579

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_low_ANJETI02_L_4_low exp_low_ANJETI02_L_4_low 2.555556 1.1303883
    ## exp_low_CLCHJO01_L_4_low exp_low_CLCHJO01_L_4_low 2.333333 0.7071068
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
    ## [1] 2.565217
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.025284

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

    ## Warning: NA values were introduced during conversion. Count: 46

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
    ## exp_low_ANJETI02_L_5_low exp_low_ANJETI02_L_5_low 2.111111 1.0540926
    ## exp_low_CLCHJO01_L_5_low exp_low_CLCHJO01_L_5_low 2.000000 0.8660254
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
    ## [1] 2.434783
    ## 
    ## $Overall_SD_Grade
    ## [1] 1.108596

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
    ## Age            1.00000000 -0.08465733  0.30021457 -0.132195012    0.31673040
    ## Genre         -0.08465733  1.00000000  0.45292029  0.081513915   -0.12248296
    ## Education      0.30021457  0.45292029  1.00000000 -0.094495843    0.27262010
    ## Vie           -0.13219501  0.08151391 -0.09449584  1.000000000   -0.05050763
    ## Participation  0.31673040 -0.12248296  0.27262010 -0.050507627    1.00000000
    ## Grades        -0.06445361 -0.06453496 -0.23755945 -0.007975048   -0.13687902
    ## Category       0.00000000  0.00000000  0.00000000  0.000000000    0.00000000
    ##                     Grades   Category
    ## Age           -0.064453607  0.0000000
    ## Genre         -0.064534957  0.0000000
    ## Education     -0.237559452  0.0000000
    ## Vie           -0.007975048  0.0000000
    ## Participation -0.136879024  0.0000000
    ## Grades         1.000000000 -0.1709012
    ## Category      -0.170901235  1.0000000

``` r
correlation_results_pvalue <- rcorr(as.matrix(correlation_data))
correlation_results_pvalue
```

    ##                 Age Genre Education   Vie Participation Grades Category
    ## Age            1.00 -0.08      0.30 -0.13          0.32  -0.06     0.00
    ## Genre         -0.08  1.00      0.45  0.08         -0.12  -0.06     0.00
    ## Education      0.30  0.45      1.00 -0.09          0.27  -0.24     0.00
    ## Vie           -0.13  0.08     -0.09  1.00         -0.05  -0.01     0.00
    ## Participation  0.32 -0.12      0.27 -0.05          1.00  -0.14     0.00
    ## Grades        -0.06 -0.06     -0.24 -0.01         -0.14   1.00    -0.17
    ## Category       0.00  0.00      0.00  0.00          0.00  -0.17     1.00
    ## 
    ## n= 92 
    ## 
    ## 
    ## P
    ##               Age    Genre  Education Vie    Participation Grades Category
    ## Age                  0.4224 0.0036    0.2091 0.0021        0.5416 1.0000  
    ## Genre         0.4224        0.0000    0.4398 0.2448        0.5411 1.0000  
    ## Education     0.0036 0.0000           0.3703 0.0086        0.0226 1.0000  
    ## Vie           0.2091 0.4398 0.3703           0.6326        0.9399 1.0000  
    ## Participation 0.0021 0.2448 0.0086    0.6326               0.1932 1.0000  
    ## Grades        0.5416 0.5411 0.0226    0.9399 0.1932               0.1034  
    ## Category      1.0000 1.0000 1.0000    1.0000 1.0000        0.1034

``` r
correlation_results_pvalue$r
```

    ##                       Age       Genre   Education          Vie Participation
    ## Age            1.00000000 -0.08465733  0.30021457 -0.132195012    0.31673040
    ## Genre         -0.08465733  1.00000000  0.45292029  0.081513915   -0.12248296
    ## Education      0.30021457  0.45292029  1.00000000 -0.094495843    0.27262010
    ## Vie           -0.13219501  0.08151391 -0.09449584  1.000000000   -0.05050763
    ## Participation  0.31673040 -0.12248296  0.27262010 -0.050507627    1.00000000
    ## Grades        -0.06445361 -0.06453496 -0.23755945 -0.007975048   -0.13687902
    ## Category       0.00000000  0.00000000  0.00000000  0.000000000    0.00000000
    ##                     Grades   Category
    ## Age           -0.064453607  0.0000000
    ## Genre         -0.064534957  0.0000000
    ## Education     -0.237559452  0.0000000
    ## Vie           -0.007975048  0.0000000
    ## Participation -0.136879024  0.0000000
    ## Grades         1.000000000 -0.1709012
    ## Category      -0.170901235  1.0000000

``` r
correlation_results_pvalue$P
```

    ##                       Age        Genre    Education       Vie Participation
    ## Age                    NA 4.223539e-01 3.642461e-03 0.2090599   0.002097696
    ## Genre         0.422353927           NA 5.808181e-06 0.4398476   0.244773538
    ## Education     0.003642461 5.808181e-06           NA 0.3702598   0.008560433
    ## Vie           0.209059917 4.398476e-01 3.702598e-01        NA   0.632555013
    ## Participation 0.002097696 2.447735e-01 8.560433e-03 0.6325550            NA
    ## Grades        0.541597004 5.410858e-01 2.259689e-02 0.9398572   0.193231075
    ## Category      1.000000000 1.000000e+00 1.000000e+00 1.0000000   1.000000000
    ##                   Grades  Category
    ## Age           0.54159700 1.0000000
    ## Genre         0.54108584 1.0000000
    ## Education     0.02259689 1.0000000
    ## Vie           0.93985725 1.0000000
    ## Participation 0.19323108 1.0000000
    ## Grades                NA 0.1033508
    ## Category      0.10335079        NA

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
-0.085<span style="vertical-align:super;font-size:0.8em;"></span>
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
0.300<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.453<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
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
-0.132<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.082<span style="vertical-align:super;font-size:0.8em;"></span>
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
0.317<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.122<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.273<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.051<span style="vertical-align:super;font-size:0.8em;"></span>
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
-0.064<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.065<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.238<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.008<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.137<span style="vertical-align:super;font-size:0.8em;"></span>
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
-0.171<span style="vertical-align:super;font-size:0.8em;"></span>
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
    ##  1st Qu.:1.000   1st Qu.:3.000   1st Qu.:3.000   1st Qu.:4.000  
    ##  Median :3.000   Median :3.000   Median :3.000   Median :4.000  
    ##  Mean   :3.333   Mean   :3.333   Mean   :3.429   Mean   :4.214  
    ##  3rd Qu.:5.000   3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.:5.000  
    ##  Max.   :6.000   Max.   :5.000   Max.   :5.000   Max.   :6.000  
    ##  NA's   :14      NA's   :14      NA's   :9       NA's   :9

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
    ## W = 0.8526, p-value = 0.07958
    ## 
    ## 
    ## $high_MAANAU12_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.91655, p-value = 0.3644
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
    ##  1st Qu.:2.000   1st Qu.:2.000   1st Qu.:2.250   1st Qu.:3.000  
    ##  Median :3.000   Median :2.000   Median :3.000   Median :4.000  
    ##  Mean   :2.778   Mean   :2.556   Mean   :3.214   Mean   :3.786  
    ##  3rd Qu.:4.000   3rd Qu.:3.000   3rd Qu.:3.750   3rd Qu.:4.750  
    ##  Max.   :5.000   Max.   :5.000   Max.   :5.000   Max.   :6.000  
    ##  NA's   :14      NA's   :14      NA's   :9       NA's   :9

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
    ## W = 0.93756, p-value = 0.5565
    ## 
    ## 
    ## $low_CLCHJO01_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.92181, p-value = 0.4074
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
    ##  Min.   :1                  Min.   :1.000             
    ##  1st Qu.:2                  1st Qu.:3.000             
    ##  Median :3                  Median :4.000             
    ##  Mean   :3                  Mean   :3.667             
    ##  3rd Qu.:4                  3rd Qu.:5.000             
    ##  Max.   :5                  Max.   :5.000             
    ##  NA's   :14                 NA's   :14                
    ##  exp_high_BEJEDI08_L_3_high exp_high_BEJEDI08_L_4_high
    ##  Min.   :1.000              Min.   :2.000             
    ##  1st Qu.:2.000              1st Qu.:3.000             
    ##  Median :4.000              Median :3.000             
    ##  Mean   :3.333              Mean   :3.333             
    ##  3rd Qu.:4.000              3rd Qu.:4.000             
    ##  Max.   :5.000              Max.   :5.000             
    ##  NA's   :14                 NA's   :14                
    ##  exp_high_BEJEDI08_L_5_high exp_high_MAANAU12_L_1_high
    ##  Min.   :1.000              Min.   :2.000             
    ##  1st Qu.:2.000              1st Qu.:2.000             
    ##  Median :4.000              Median :2.000             
    ##  Mean   :3.222              Mean   :2.556             
    ##  3rd Qu.:4.000              3rd Qu.:3.000             
    ##  Max.   :5.000              Max.   :4.000             
    ##  NA's   :14                 NA's   :14                
    ##  exp_high_MAANAU12_L_2_high exp_high_MAANAU12_L_3_high
    ##  Min.   :2.000              Min.   :2.000             
    ##  1st Qu.:3.000              1st Qu.:3.000             
    ##  Median :4.000              Median :3.000             
    ##  Mean   :3.444              Mean   :3.111             
    ##  3rd Qu.:4.000              3rd Qu.:4.000             
    ##  Max.   :5.000              Max.   :4.000             
    ##  NA's   :14                 NA's   :14                
    ##  exp_high_MAANAU12_L_4_high exp_high_MAANAU12_L_5_high exp_low_ANJETI02_L_1_low
    ##  Min.   :2.000              Min.   :2.000              Min.   :1               
    ##  1st Qu.:2.000              1st Qu.:2.000              1st Qu.:1               
    ##  Median :3.000              Median :2.000              Median :1               
    ##  Mean   :2.889              Mean   :2.444              Mean   :2               
    ##  3rd Qu.:3.000              3rd Qu.:3.000              3rd Qu.:3               
    ##  Max.   :5.000              Max.   :3.000              Max.   :4               
    ##  NA's   :14                 NA's   :14                 NA's   :14              
    ##  exp_low_ANJETI02_L_2_low exp_low_ANJETI02_L_3_low exp_low_ANJETI02_L_4_low
    ##  Min.   :1.000            Min.   :1.000            Min.   :1.000           
    ##  1st Qu.:3.000            1st Qu.:2.000            1st Qu.:2.000           
    ##  Median :3.000            Median :2.000            Median :3.000           
    ##  Mean   :3.333            Mean   :2.333            Mean   :2.556           
    ##  3rd Qu.:4.000            3rd Qu.:3.000            3rd Qu.:3.000           
    ##  Max.   :5.000            Max.   :4.000            Max.   :4.000           
    ##  NA's   :14               NA's   :14               NA's   :14              
    ##  exp_low_ANJETI02_L_5_low exp_low_CLCHJO01_L_1_low exp_low_CLCHJO01_L_2_low
    ##  Min.   :1.000            Min.   :1                Min.   :1.000           
    ##  1st Qu.:1.000            1st Qu.:1                1st Qu.:1.000           
    ##  Median :2.000            Median :2                Median :2.000           
    ##  Mean   :2.111            Mean   :2                Mean   :2.222           
    ##  3rd Qu.:3.000            3rd Qu.:3                3rd Qu.:2.000           
    ##  Max.   :4.000            Max.   :4                Max.   :5.000           
    ##  NA's   :14               NA's   :14               NA's   :14              
    ##  exp_low_CLCHJO01_L_3_low exp_low_CLCHJO01_L_4_low exp_low_CLCHJO01_L_5_low
    ##  Min.   :1.000            Min.   :1.000            Min.   :1               
    ##  1st Qu.:1.000            1st Qu.:2.000            1st Qu.:1               
    ##  Median :2.000            Median :2.000            Median :2               
    ##  Mean   :2.111            Mean   :2.333            Mean   :2               
    ##  3rd Qu.:3.000            3rd Qu.:3.000            3rd Qu.:3               
    ##  Max.   :4.000            Max.   :3.000            Max.   :3               
    ##  NA's   :14               NA's   :14               NA's   :14              
    ##  exp_high_SAERBE09_L_1_high exp_high_SAERBE09_L_2_high
    ##  Min.   :1                  Min.   :1.000             
    ##  1st Qu.:2                  1st Qu.:3.000             
    ##  Median :3                  Median :3.000             
    ##  Mean   :3                  Mean   :3.214             
    ##  3rd Qu.:4                  3rd Qu.:3.750             
    ##  Max.   :4                  Max.   :5.000             
    ##  NA's   :9                  NA's   :9                 
    ##  exp_high_SAERBE09_L_3_high exp_high_SAERBE09_L_4_high
    ##  Min.   :1.000              Min.   :1.000             
    ##  1st Qu.:2.250              1st Qu.:2.000             
    ##  Median :3.000              Median :2.000             
    ##  Mean   :2.786              Mean   :2.357             
    ##  3rd Qu.:3.000              3rd Qu.:3.000             
    ##  Max.   :4.000              Max.   :4.000             
    ##  NA's   :9                  NA's   :9                 
    ##  exp_high_SAERBE09_L_5_high exp_high_SADJSO02_L_1_high
    ##  Min.   :1.000              Min.   :1.0               
    ##  1st Qu.:2.000              1st Qu.:3.0               
    ##  Median :2.500              Median :4.0               
    ##  Mean   :2.643              Mean   :3.5               
    ##  3rd Qu.:3.750              3rd Qu.:4.0               
    ##  Max.   :4.000              Max.   :5.0               
    ##  NA's   :9                  NA's   :9                 
    ##  exp_high_SADJSO02_L_2_high exp_high_SADJSO02_L_3_high
    ##  Min.   :1.000              Min.   :1.000             
    ##  1st Qu.:3.000              1st Qu.:3.000             
    ##  Median :4.000              Median :3.500             
    ##  Mean   :3.714              Mean   :3.429             
    ##  3rd Qu.:4.000              3rd Qu.:4.000             
    ##  Max.   :5.000              Max.   :5.000             
    ##  NA's   :9                  NA's   :9                 
    ##  exp_high_SADJSO02_L_4_high exp_high_SADJSO02_L_5_high exp_low_COHEIG03_L_1_low
    ##  Min.   :1.000              Min.   :1.000              Min.   :1.000           
    ##  1st Qu.:2.250              1st Qu.:2.250              1st Qu.:2.000           
    ##  Median :3.500              Median :3.000              Median :2.500           
    ##  Mean   :3.286              Mean   :3.143              Mean   :2.571           
    ##  3rd Qu.:4.000              3rd Qu.:4.000              3rd Qu.:3.000           
    ##  Max.   :5.000              Max.   :5.000              Max.   :5.000           
    ##  NA's   :9                  NA's   :9                  NA's   :9               
    ##  exp_low_COHEIG03_L_2_low exp_low_COHEIG03_L_3_low exp_low_COHEIG03_L_4_low
    ##  Min.   :1.000            Min.   :1.000            Min.   :1.000           
    ##  1st Qu.:2.000            1st Qu.:2.000            1st Qu.:1.250           
    ##  Median :3.000            Median :3.000            Median :2.500           
    ##  Mean   :2.857            Mean   :2.643            Mean   :2.357           
    ##  3rd Qu.:3.750            3rd Qu.:3.000            3rd Qu.:3.000           
    ##  Max.   :4.000            Max.   :4.000            Max.   :4.000           
    ##  NA's   :9                NA's   :9                NA's   :9               
    ##  exp_low_COHEIG03_L_5_low exp_low_VEYVEM11_L_1_low exp_low_VEYVEM11_L_2_low
    ##  Min.   :1.000            Min.   :2.000            Min.   :1.000           
    ##  1st Qu.:1.250            1st Qu.:3.000            1st Qu.:3.000           
    ##  Median :2.000            Median :3.500            Median :3.000           
    ##  Mean   :2.429            Mean   :3.357            Mean   :3.286           
    ##  3rd Qu.:3.000            3rd Qu.:4.000            3rd Qu.:4.000           
    ##  Max.   :5.000            Max.   :4.000            Max.   :5.000           
    ##  NA's   :9                NA's   :9                NA's   :9               
    ##  exp_low_VEYVEM11_L_3_low exp_low_VEYVEM11_L_4_low exp_low_VEYVEM11_L_5_low
    ##  Min.   :1.000            Min.   :1.000            Min.   :1.000           
    ##  1st Qu.:2.000            1st Qu.:2.250            1st Qu.:2.000           
    ##  Median :3.000            Median :3.000            Median :3.000           
    ##  Mean   :2.857            Mean   :2.929            Mean   :2.929           
    ##  3rd Qu.:3.750            3rd Qu.:4.000            3rd Qu.:4.000           
    ##  Max.   :4.000            Max.   :4.000            Max.   :4.000           
    ##  NA's   :9                NA's   :9                NA's   :9

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
    ## W = 0.91152, p-value = 0.3267
    ## 
    ## 
    ## $exp_high_BEJEDI08_L_2_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.86668, p-value = 0.1133
    ## 
    ## 
    ## $exp_high_BEJEDI08_L_3_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.91152, p-value = 0.3267
    ## 
    ## 
    ## $exp_high_BEJEDI08_L_4_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.87282, p-value = 0.1318
    ## 
    ## 
    ## $exp_high_BEJEDI08_L_5_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.87783, p-value = 0.1489
    ## 
    ## 
    ## $exp_high_MAANAU12_L_1_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.76301, p-value = 0.007671
    ## 
    ## 
    ## $exp_high_MAANAU12_L_2_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.89178, p-value = 0.2081
    ## 
    ## 
    ## $exp_high_MAANAU12_L_3_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.83798, p-value = 0.05485
    ## 
    ## 
    ## $exp_high_MAANAU12_L_4_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.82489, p-value = 0.03913
    ## 
    ## 
    ## $exp_high_MAANAU12_L_5_high
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.65474, p-value = 0.0004194
    ## 
    ## 
    ## $exp_low_ANJETI02_L_1_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.75055, p-value = 0.005502
    ## 
    ## 
    ## $exp_low_ANJETI02_L_2_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.93644, p-value = 0.5449
    ## 
    ## 
    ## $exp_low_ANJETI02_L_3_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.91655, p-value = 0.3644
    ## 
    ## 
    ## $exp_low_ANJETI02_L_4_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.89939, p-value = 0.2485
    ## 
    ## 
    ## $exp_low_ANJETI02_L_5_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.88886, p-value = 0.1942
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_1_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.84448, p-value = 0.06476
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_2_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.72202, p-value = 0.002564
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_3_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.88886, p-value = 0.1942
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_4_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.8054, p-value = 0.02353
    ## 
    ## 
    ## $exp_low_CLCHJO01_L_5_low
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.82304, p-value = 0.03729
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
    ## V = 342, p-value = 0.05893
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
    ## V = 77.5, p-value = 0.1085
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

### Mann-Whitney U Test Exploratory questions

#### Entertainment

``` r
high_ratings_Entertainment <- unlist(Data_clear_large %>% select(ends_with("_1_high")))
low_ratings_Entertainment <- unlist(Data_clear_large %>% select(ends_with("_1_low")))

mann_whitney_test_Entertainment <- wilcox.test(high_ratings_Entertainment, low_ratings_Entertainment, paired = TRUE)

print(mann_whitney_test_Entertainment)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  high_ratings_Entertainment and low_ratings_Entertainment
    ## V = 372, p-value = 0.01177
    ## alternative hypothesis: true location shift is not equal to 0

#### Emotionality

``` r
high_ratings_Emotionality <- unlist(Data_clear_large %>% select(ends_with("_2_high")))
low_ratings_Emotionality <- unlist(Data_clear_large %>% select(ends_with("_2_low")))

mann_whitney_test_Emotionality <- wilcox.test(high_ratings_Emotionality, low_ratings_Emotionality, paired = TRUE)

print(mann_whitney_test_Emotionality)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  high_ratings_Emotionality and low_ratings_Emotionality
    ## V = 368, p-value = 0.01629
    ## alternative hypothesis: true location shift is not equal to 0

#### Memorability

``` r
high_ratings_Memorability <- unlist(Data_clear_large %>% select(ends_with("_3_high")))
low_ratings_Memorability <- unlist(Data_clear_large %>% select(ends_with("_3_low")))

mann_whitney_test_Memorability <- wilcox.test(high_ratings_Memorability, low_ratings_Memorability, paired = TRUE)

print(mann_whitney_test_Memorability)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  high_ratings_Memorability and low_ratings_Memorability
    ## V = 473.5, p-value = 0.001492
    ## alternative hypothesis: true location shift is not equal to 0

#### Originality

``` r
high_ratings_Originality <- unlist(Data_clear_large %>% select(ends_with("_4_high")))
low_ratings_Originality <- unlist(Data_clear_large %>% select(ends_with("_4_low")))

mann_whitney_test_Originality <- wilcox.test(high_ratings_Originality, low_ratings_Originality, paired = TRUE)

print(mann_whitney_test_Originality)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  high_ratings_Originality and low_ratings_Originality
    ## V = 229, p-value = 0.01969
    ## alternative hypothesis: true location shift is not equal to 0

#### Engagement

``` r
high_ratings_Engagement <- unlist(Data_clear_large %>% select(ends_with("_5_high")))
low_ratings_Engagement <- unlist(Data_clear_large %>% select(ends_with("_5_low")))

mann_whitney_test_Engagement <- wilcox.test(high_ratings_Engagement, low_ratings_Engagement, paired = TRUE)

print(mann_whitney_test_Engagement)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  high_ratings_Engagement and low_ratings_Engagement
    ## V = 371, p-value = 0.03437
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
    ## REML criterion at convergence: 289.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.35808 -0.59934 -0.04963  0.55515  2.34998 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.6084   0.780   
    ##  Residual               1.1317   1.064   
    ## Number of obs: 92, groups:  ResponseId, 23
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     4.28920    0.94884   4.520
    ## Categorylow    -0.45652    0.22182  -2.058
    ## Age             0.04425    0.02728   1.622
    ## Genre2          0.69059    0.56532   1.222
    ## Genre3          0.84383    1.14292   0.738
    ## Education3     -2.23251    0.84335  -2.647
    ## Education5     -2.17244    0.85200  -2.550
    ## Education7     -2.07006    0.84680  -2.445
    ## Education8     -3.97531    1.55017  -2.564
    ## Vie2           -0.25654    0.45477  -0.564
    ## Participation2 -0.53062    0.50694  -1.047
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.117                                                        
    ## Age         -0.639  0.000                                                 
    ## Genre2      -0.171  0.000  0.380                                          
    ## Genre3      -0.230  0.000  0.201  0.273                                   
    ## Education3  -0.154  0.000 -0.475 -0.450 -0.131                            
    ## Education5  -0.320  0.000 -0.294 -0.455 -0.318  0.697                     
    ## Education7  -0.188  0.000 -0.355 -0.619 -0.192  0.750  0.728              
    ## Education8   0.269  0.000 -0.741 -0.616 -0.244  0.684  0.574  0.694       
    ## Vie2        -0.426  0.000  0.108 -0.258  0.072  0.176  0.299  0.252  0.065
    ## Participtn2 -0.240  0.000 -0.214 -0.014  0.197  0.138  0.161 -0.059  0.035
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
    ## Participtn2  0.093

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

    ## # A tibble: 184 × 78
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
    ## # ℹ 174 more rows
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
    ## REML criterion at convergence: 263.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.82156 -0.78614  0.06435  0.62529  2.50328 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.6434   0.8021  
    ##  Residual               0.7756   0.8807  
    ## Number of obs: 92, groups:  ResponseId, 23
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)     3.845293   0.917950   4.189
    ## Categorylow    -0.478261   0.183632  -2.604
    ## Age             0.019763   0.026441   0.747
    ## Genre2          0.218408   0.547927   0.399
    ## Genre3          1.109201   1.107758   1.001
    ## Education3     -1.426103   0.817400  -1.745
    ## Education5     -1.669911   0.825785  -2.022
    ## Education7     -1.329520   0.820752  -1.620
    ## Education8     -2.263303   1.502476  -1.506
    ## Vie2            0.002665   0.440775   0.006
    ## Participation2 -0.363492   0.491341  -0.740
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.100                                                        
    ## Age         -0.640  0.000                                                 
    ## Genre2      -0.172  0.000  0.380                                          
    ## Genre3      -0.230  0.000  0.201  0.273                                   
    ## Education3  -0.154  0.000 -0.475 -0.450 -0.131                            
    ## Education5  -0.321  0.000 -0.294 -0.455 -0.318  0.697                     
    ## Education7  -0.188  0.000 -0.355 -0.619 -0.192  0.750  0.728              
    ## Education8   0.269  0.000 -0.741 -0.616 -0.244  0.684  0.574  0.694       
    ## Vie2        -0.427  0.000  0.108 -0.258  0.072  0.176  0.299  0.252  0.065
    ## Participtn2 -0.241  0.000 -0.214 -0.014  0.197  0.138  0.161 -0.059  0.035
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
    ## Participtn2  0.093

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

    ## # A tibble: 184 × 78
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
    ## # ℹ 174 more rows
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

summary(model_Emotionality_category)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Grades ~ Category + Age + Genre + Education + Vie + Participation +  
    ##     (1 | ResponseId)
    ##    Data: Data_clear_long_Emotionality
    ## 
    ## REML criterion at convergence: 272.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3286 -0.5196  0.1152  0.5433  2.0327 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.4479   0.6692  
    ##  Residual               0.9185   0.9584  
    ## Number of obs: 92, groups:  ResponseId, 23
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     3.81688    0.82762   4.612
    ## Categorylow    -0.54348    0.19983  -2.720
    ## Age             0.02557    0.02378   1.075
    ## Genre2          0.15600    0.49287   0.317
    ## Genre3          1.90602    0.99644   1.913
    ## Education3     -1.45723    0.73526  -1.982
    ## Education5     -1.03919    0.74280  -1.399
    ## Education7     -0.96598    0.73828  -1.308
    ## Education8     -1.69659    1.35149  -1.255
    ## Vie2           -0.14912    0.39648  -0.376
    ## Participation2 -0.32620    0.44197  -0.738
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.121                                                        
    ## Age         -0.638  0.000                                                 
    ## Genre2      -0.171  0.000  0.380                                          
    ## Genre3      -0.229  0.000  0.201  0.273                                   
    ## Education3  -0.154  0.000 -0.475 -0.450 -0.131                            
    ## Education5  -0.320  0.000 -0.294 -0.455 -0.318  0.697                     
    ## Education7  -0.188  0.000 -0.355 -0.619 -0.192  0.750  0.728              
    ## Education8   0.269  0.000 -0.741 -0.616 -0.244  0.684  0.574  0.694       
    ## Vie2        -0.426  0.000  0.108 -0.258  0.072  0.176  0.299  0.252  0.065
    ## Participtn2 -0.240  0.000 -0.214 -0.014  0.197  0.138  0.161 -0.059  0.035
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
    ## Participtn2  0.093

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

    ## # A tibble: 184 × 78
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
    ## # ℹ 174 more rows
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
    ## REML criterion at convergence: 253.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6854 -0.6331 -0.1039  0.6559  2.3721 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.3284   0.5731  
    ##  Residual               0.7350   0.8573  
    ## Number of obs: 92, groups:  ResponseId, 23
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     3.67814    0.71989   5.109
    ## Categorylow    -0.60870    0.17876  -3.405
    ## Age             0.02588    0.02068   1.252
    ## Genre2          0.26213    0.42853   0.612
    ## Genre3          0.73142    0.86637   0.844
    ## Education3     -1.30507    0.63929  -2.041
    ## Education5     -1.20054    0.64584  -1.859
    ## Education7     -1.01895    0.64191  -1.587
    ## Education8     -2.31613    1.17508  -1.971
    ## Vie2           -0.23935    0.34473  -0.694
    ## Participation2 -0.49022    0.38428  -1.276
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.124                                                        
    ## Age         -0.638  0.000                                                 
    ## Genre2      -0.171  0.000  0.380                                          
    ## Genre3      -0.229  0.000  0.201  0.273                                   
    ## Education3  -0.154  0.000 -0.475 -0.450 -0.131                            
    ## Education5  -0.320  0.000 -0.294 -0.455 -0.318  0.697                     
    ## Education7  -0.188  0.000 -0.355 -0.619 -0.192  0.750  0.728              
    ## Education8   0.268  0.000 -0.741 -0.616 -0.244  0.684  0.574  0.694       
    ## Vie2        -0.426  0.000  0.108 -0.258  0.072  0.176  0.299  0.252  0.065
    ## Participtn2 -0.240  0.000 -0.214 -0.014  0.197  0.138  0.161 -0.059  0.035
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
    ## Participtn2  0.093

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

    ## # A tibble: 184 × 78
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
    ## # ℹ 174 more rows
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
    ## REML criterion at convergence: 249.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6454 -0.6289 -0.1418  0.5245  2.8525 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.3829   0.6188  
    ##  Residual               0.6854   0.8279  
    ## Number of obs: 92, groups:  ResponseId, 23
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     4.47966    0.74811   5.988
    ## Categorylow    -0.36956    0.17263  -2.141
    ## Age            -0.00672    0.02151  -0.312
    ## Genre2          0.22145    0.44580   0.497
    ## Genre3         -0.28326    0.90129  -0.314
    ## Education3     -0.97190    0.66505  -1.461
    ## Education5     -0.85707    0.67187  -1.276
    ## Education7     -0.95168    0.66778  -1.425
    ## Education8     -1.28418    1.22244  -1.051
    ## Vie2           -0.24289    0.35862  -0.677
    ## Participation2 -0.71087    0.39976  -1.778
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.115                                                        
    ## Age         -0.639  0.000                                                 
    ## Genre2      -0.171  0.000  0.380                                          
    ## Genre3      -0.230  0.000  0.201  0.273                                   
    ## Education3  -0.154  0.000 -0.475 -0.450 -0.131                            
    ## Education5  -0.320  0.000 -0.294 -0.455 -0.318  0.697                     
    ## Education7  -0.188  0.000 -0.355 -0.619 -0.192  0.750  0.728              
    ## Education8   0.269  0.000 -0.741 -0.616 -0.244  0.684  0.574  0.694       
    ## Vie2        -0.426  0.000  0.108 -0.258  0.072  0.176  0.299  0.252  0.065
    ## Participtn2 -0.240  0.000 -0.214 -0.014  0.197  0.138  0.161 -0.059  0.035
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
    ## Participtn2  0.093

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

    ## # A tibble: 184 × 78
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
    ## # ℹ 174 more rows
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
    ## REML criterion at convergence: 266.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.26946 -0.66013 -0.04333  0.57543  2.58364 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  ResponseId (Intercept) 0.4010   0.6332  
    ##  Residual               0.8625   0.9287  
    ## Number of obs: 92, groups:  ResponseId, 23
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)     3.60909    0.78975   4.570
    ## Categorylow    -0.43478    0.19365  -2.245
    ## Age             0.01939    0.02269   0.855
    ## Genre2          0.20311    0.47021   0.432
    ## Genre3          0.85787    0.95063   0.902
    ## Education3     -1.61625    0.70146  -2.304
    ## Education5     -1.44564    0.70865  -2.040
    ## Education7     -1.16144    0.70434  -1.649
    ## Education8     -2.40367    1.28936  -1.864
    ## Vie2            0.13812    0.37825   0.365
    ## Participation2 -0.41418    0.42165  -0.982
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ctgryl Age    Genre2 Genre3 Edctn3 Edctn5 Edctn7 Edctn8
    ## Categorylow -0.123                                                        
    ## Age         -0.638  0.000                                                 
    ## Genre2      -0.171  0.000  0.380                                          
    ## Genre3      -0.229  0.000  0.201  0.273                                   
    ## Education3  -0.154  0.000 -0.475 -0.450 -0.131                            
    ## Education5  -0.320  0.000 -0.294 -0.455 -0.318  0.697                     
    ## Education7  -0.188  0.000 -0.355 -0.619 -0.192  0.750  0.728              
    ## Education8   0.268  0.000 -0.741 -0.616 -0.244  0.684  0.574  0.694       
    ## Vie2        -0.426  0.000  0.108 -0.258  0.072  0.176  0.299  0.252  0.065
    ## Participtn2 -0.240  0.000 -0.214 -0.014  0.197  0.138  0.161 -0.059  0.035
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
    ## Participtn2  0.093

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

<img src="Results-analysis_files/figure-gfm/unnamed-chunk-182-1.png" style="display: block; margin: auto;" /><img src="Results-analysis_files/figure-gfm/unnamed-chunk-182-2.png" style="display: block; margin: auto;" />

``` r
shapiro_results_common_story <- lapply(common_story_distribution, shapiro.test)

shapiro_results_common_story
```

    ## $RIRO12_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.85722, p-value = 0.003688
    ## 
    ## 
    ## $RIRO12_L_1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.79513, p-value = 0.0003205
    ## 
    ## 
    ## $RIRO12_L_2
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.89302, p-value = 0.0182
    ## 
    ## 
    ## $RIRO12_L_3
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.88312, p-value = 0.01153
    ## 
    ## 
    ## $RIRO12_L_4
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.85539, p-value = 0.003413
    ## 
    ## 
    ## $RIRO12_L_5
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  X[[i]]
    ## W = 0.84417, p-value = 0.002137

``` r
# Higher than 0.05 = normally distributed
```

``` r
# Calculate the variance of the grades
variance_riro12 <- var(Data_clear_large$RIRO12_1)
print(variance_riro12)
```

    ## [1] 1.110672
