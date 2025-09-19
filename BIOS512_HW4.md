# Homework 04
For questions 2-6, please use hw4.zip, which contains a data base of patient/hopsital data.

## Question 1
*For this question, you can either import these tables into R and do each join, or create the tables we expect to see in a Markdown cell.*   
Please see the tables below.  


R
library(tidyverse)

table_a <- tibble(
  SKU = c(102345, 104567, 108912, 109876, 112233),
  Fruit = c("Apple", "Orange", "Mango", "Blueberry", "Watermelon"),
  Color = c("Red", "Orange", "Yellow", "Blue", "Green"),
  Price = c(1.20, 1.40, 1.70, 3.50, 4.40),
  In_Stock = c("Yes", "Yes", "No", "Yes", "No")
)

table_b <- tibble(
  SKU = c(102345, 105432, 106789, 104567, 107654),
  Fruit = c("Apple", "Banana", "Grape", "Orange", "Pear"),
  Color = c("Red", "Yellow", "Purple", "Orange", "Green"),
  Sale_Price = c(1.00, 0.50, 2.00, 1.20, 1.10),
  Number_in_Stock = c(50, 120, 0, 75, 0)
)


    ── [1mAttaching core tidyverse packages[22m ──────────────────────── tidyverse 2.0.0 ──
    [32m✔[39m [34mdplyr    [39m 1.1.4     [32m✔[39m [34mreadr    [39m 2.1.5
    [32m✔[39m [34mforcats  [39m 1.0.0     [32m✔[39m [34mstringr  [39m 1.5.1
    [32m✔[39m [34mggplot2  [39m 3.5.1     [32m✔[39m [34mtibble   [39m 3.2.1
    [32m✔[39m [34mlubridate[39m 1.9.3     [32m✔[39m [34mtidyr    [39m 1.3.1
    [32m✔[39m [34mpurrr    [39m 1.0.2     
    ── [1mConflicts[22m ────────────────────────────────────────── tidyverse_conflicts() ──
    [31m✖[39m [34mdplyr[39m::[32mfilter()[39m masks [34mstats[39m::filter()
    [31m✖[39m [34mdplyr[39m::[32mlag()[39m    masks [34mstats[39m::lag()
    [36mℹ[39m Use the conflicted package ([3m[34m<http://conflicted.r-lib.org/>[39m[23m) to force all conflicts to become errors


What would the result be if you did...  
a) Left join 
table_left_join <- table_a %>% left_join(table_b, by=c('SKU','Fruit','Color')) ## keep all table a, attach table b when present 

b) Right join  
table_right_join <- table_a %>% right_join(table_b, by=c('SKU','Fruit','Color')) ## keep all table b, attach all table a when present

c) Inner join  
table_inner_join <- table_left_join %>% inner_join(table_right_join, by=c('SKU','Fruit','Color','Price','In_Stock','Sale_Price','Number_in_Stock')) ## keeping only rows with matching keys (just apple an orange bc they have the same values in each table)

d) Full join  
table_full_join <- table_a %>% full_join(table_b, by=c('SKU','Fruit','Color')) ## keep all table a and table b (overlap anything that's the same)

e) Semi join  
table_semi_join <- table_a %>% semi_join(table_b, by='SKU') ## keeping everyone from table a with at least one value in table b 

f) Anti join  
table_anti_join <- table_a %>% anti_join(table_b, by='SKU') ## keeping everyone in SKU with no values in table b 

## Question 2
Inspect the data sets in our database!  
a) Import them.
demographics <- read_csv("/Users/ssagar/Desktop/UNC/BIOS512/BIOS512_assignments/hw4/demographics.csv")
full <- read_csv("/Users/ssagar/Desktop/UNC/BIOS512/BIOS512_assignments/hw4/full.csv")
hospitals <- read_csv("/Users/ssagar/Desktop/UNC/BIOS512/BIOS512_assignments/hw4/hospitals.csv")
patient_names <- read_csv("/Users/ssagar/Desktop/UNC/BIOS512/BIOS512_assignments/hw4/patient_names.csv")
treatment_info <- read_csv("/Users/ssagar/Desktop/UNC/BIOS512/BIOS512_assignments/hw4/treatment_info.csv")

b) Check out the columns and their variable types using one of R's tibble summary functions.
glimpse(demographics)
glimpse(full)
glimpse(hospitals)
glimpse(patient_names)
glimpse(treatment_info)

## Question 3
Using the `full.csv` data set from our database, **pivot longer** by making all of the variables the same type.
Use both `patient_ID` and `name` as ID variables. After pivoting, get a `tally` for number of observations per 
`patient ID`/`name`. (*Hint: We did this in lecture 5!*)  

full_longer <- full %>% pivot_longer(cols = -c(patient_id, name),
names_to = "variable", values_to = "value", values_transform = function(x) ifelse(is.na(x), NA, as.character(x)))

full_longer %>% group_by(patient_id, name) %>% tally()

## Question 4
Pivot longer by making one column per data type. Use both `patient_ID` and `name` as ID variables.
After pivoting, get a `tally` for number of each type of observation per `patient ID`/`name`.  


**Helpful Hints:**  
1. You're performing 3 seperate pivots with careful column selection then joining them after!  
2. After each pivot, add the code below to create a unique row number:  
```
%>%
group_by(patient_id, name) %>%
  mutate(row = row_number()) %>%
  ungroup()
```
3. To create the tally, add what is below after your grouping statement:   
```
%>%
summarise(
    n_chr  = sum(!is.na(value_chr)),
    n_num  = sum(!is.na(value_num)),
    n_date = sum(!is.na(value_date)),
    .groups = "drop"
```
##pivoting character columns 
full_long_chr <- full %>% select(patient_id, name, where(is.character)) %>% pivot_longer(
  cols = -c(patient_id,name),
  names_to = "variable_chr",
  values_to = "value_chr"
) %>%
group_by(patient_id, name) %>%
mutate(row=row_number()) %>%
ungroup()


##pivoting numerical columns
full_long_num <- full %>% select(patient_id, name, where(is.numeric)) %>% pivot_longer(
  cols = -c(patient_id,name),
  names_to="variable_num",
  values_to="value_num"
  ) %>%
group_by(patient_id, name) %>%
mutate(row = row_number()) %>%
ungroup()

## pivoting date columns 
full_long_date <- full %>%
  select(patient_id, name, where(lubridate::is.Date)) %>%
  pivot_longer(
    cols = -c(patient_id, name),
    names_to = "variable_date",
    values_to = "value_date"
  ) %>%
  group_by(patient_id, name) %>%
  mutate(row = row_number()) %>%
  ungroup()

## join all 3 pivots
full_long_combined <- full_long_chr %>%
  full_join(full_long_num, by=c("patient_id", "name", "row")) %>%
  full_join(full_long_date, by=c("patient_id", "name", "row"))

df_tally <- full_long_combined %>% 
  group_by(patient_id, name) %>% 
  summarise(
    n_chr = sum(!is.na(value_chr)),
    n_num = sum(!is.na(value_num)),
    n_date = sum(!is.na(value_date)),
    .groups="drop"
  )

view(df_tally)

## Question 5
Match patient names to the name of the hospital they were treated at.  
*Hint: You'll need `patient_names.csv` and `hospitals.csv`.*

hospitals_by_patient <- patient_names %>% left_join(hospitals, by='hospital_id')

## Question 6

Using joins, create a table that shows `patient_id`, `name`, `age`, `gender`, `condition`, and `treatment`.   
*Hint: You'll need `patient_names.csv`, `demographics.csv`, and `treatment_info.csv`.*
##patient_names: patient_id, name, hospital_id, condition_id
##demographics: patient_id, age, gender, race, ethnicity
##treatment_info: condition_id, condition, treatment, department 
##start with patient names, merge by patient_id with demographics and select name 

patient_demographics <- patient_names %>% left_join(demographics, by='patient_id') 
patient_demo_tx <- patient_demographics %>% left_join(treatment_info, by="condition_id")
patient_demo_tx_final <- patient_demo_tx %>% select(patient_id, name, age, gender, condition, treatment)
print(patient_demo_tx_final)

## Question 7
Let's revisit the NOFORC workshop.  
Below is what we completed in class on 9/9.  
**Please note: This contains the skimr library. Make sure you install that package! See the link for instructions: https://github.com/rjenki/BIOS512#adding-packages-to-installr-later.**  

install.packages("skimr")

# Load UFO sightings data from a GitHub CSV
df <- read_csv("https://raw.githubusercontent.com/Vincent-Toups/bios512/refs/heads/main/nuforc_workshop/nuforc_sightings.csv")

# Read column names
names(df)

# Count the occurrences of each unique 'shape' value
unique_vals <- df$shape %>% table()

# Sort the counts of shapes in descending order and get the names
unique_vals %>% sort(decreasing = T) %>% names()

# Store column names in a vector
column_names <- names(df)

# Total number of rows in the dataset
n_total <- nrow(df)

# Loop over each column to get basic summary stats
for(col in column_names) {
  values <- df[[col]];        # Extract column
  n_na <- sum(is.na(values))  # Count number of NA values
    
  unique_vals <- values %>% table() %>% sort(decreasing = T)  # Count unique values and sort them by frequency
  n_unique <- length(unique_vals)
    
  cat(sprintf("%s:\n", col))  # Print column name
  cat(sprintf("\tnumber of NA values %d (%0.2f %%)\n", n_na, 100*n_na/n_total)) # Print number and percent of NA values
  if(n_unique < 150) cat(sprintf("\t\t%s\n", names(unique_vals) %>% paste(collapse=", "))) # If column has fewer than 150 unique values, print them all
  cat(sprintf("\tnumber of unique values %d (%0.2f %%)\n", length(unique_vals), # Print number and percent of unique values
    100*length(unique_vals)/n_total))
}

# Count number of reports per state and sort ascending
df %>% group_by(state) %>% tally() %>% arrange(n)

# Extract the 'occurred' column as a vector
df %>% pull(occurred)

# Helper function: nth(n) returns a function that extracts the nth element of a vector
nth <- function(n) function(a) a[n]

# Custom function to parse date strings by splitting on - / space : characters
parse_date <- function(s){
                          space_split <- s %>% str_split("[-/ :]")
                          tibble(d1 = Map(nth(1), space_split) %>% as.character(),
                                      d2 = Map(nth(2), space_split) %>% as.character(),
                                      d3 = Map(nth(3), space_split) %>% as.character(),
                                      d4 = Map(nth(4), space_split) %>% as.character(),
                                      d5 = Map(nth(5), space_split) %>% as.character())
                          }

# Apply the parsing function to the 'occurred' column
date_stuff <- parse_date(df %>% pull(occurred))
head(date_stuff, 10)

# Histogram of the second component of the split date (likely month)
ggplot (date_stuff, aes(d2))+ geom_bar() + labs(x = "Month", y = "Count")

# Install and load the skimr package for a nicer summary
library(skimr)

# Quick summary of the dataset
skim_output <- skimr::skim(df)

# Count occurrences for categorical columns
df %>% count(country, sort = TRUE)
df %>% count(state, sort = TRUE)
df %>% count(shape, sort = TRUE)

# Convert 'occurred' and 'reported' to proper date-time format using lubridate
df <- df %>%
  mutate(
  occurred = lubridate::mdy_hm(occurred, quiet = TRUE),
  reported = lubridate::mdy_hm(reported, quiet = TRUE)
  )

# Plot UFO sightings per year
df %>%
  filter(!is.na(occurred)) %>%
  count(year = lubridate::year(occurred)) %>%
  ggplot(aes(year, n)) +
  geom_line() +
    labs(title = "UFO Sightings per Year", x = "Year", y = "Number of Reports")

##For the columns that have a low (relative to this dataset, which has ~150,000 observation) number of unique values, create a table that lists these unique values in ascending order.
# Set threshold for "low number of unique values"
threshold <- 100

# finding
low_cols <- df %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "unique_count") %>%
  filter(unique_count < threshold)

# list the unique values in ascending order
low_values <- map_df(
  low_cols$column,
  function(colname) {
    values <- df %>%
      select(all_of(colname)) %>%
      distinct() %>%
      mutate(across(everything(), as.character)) %>% 
      arrange(across(everything())) %>%
      mutate(column = colname) %>%
      rename(value = 1)
    return(values)
  }
)
print(low_values)

## Question 8
##Make a plot of number of UFO sightings by state (United States only). You can filter out states that only have one observation.

# counting UFO sightings by state, filtering out states that only have 1 observation
ufo_by_state <- df %>%
  filter(country=="USA") %>%
  filter(nchar(state) == 2, str_detect(state, "^[A-Z]{2}$")) %>%  # Filter valid US state codes (2 uppercase letters)
  group_by(state) %>%
  tally(name = "sightings") %>%
  filter(sightings > 1) %>%
  arrange(desc(sightings))

# actual plot 
ggplot(ufo_by_state, aes(x = reorder(state, sightings), y = sightings)) +
  geom_col(fill = "dodgerblue", width=0.7) +
  coord_flip() +
  labs(
    title = "Number of UFO Sightings by U.S. State",
    x = "State",
    y = "Number of Sightings"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  # smaller state labels
    plot.title = element_text(size = 14, face = "bold")  # optional: better title
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
