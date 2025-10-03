# Homework 6
This homework builds on the effective visualization workshop with the Star Trek data. Below is what we completed in class. Output is suppressed for readability, but you can remove the supression on your code if you'd like.

library(dplyr)
library(ggplot2)
invisible({
suppressPackageStartupMessages(library(tidyverse))

# Get the data.
dialogs <- read_csv(
  "https://raw.githubusercontent.com/Vincent-Toups/bios512/fcbc65a2696c7cff80d0f6ed1dd5c97abf0ef800/effective-visualization/source_data/tng.csv",
  show_col_types = FALSE
)
head(dialogs, 10) # Showing first 10 observations

# Checkout the data.
names(dialogs)
dialogs %>% group_by(character) %>% tally() %>% arrange(desc(n))
dialogs %>% mutate(dialog_length=str_length(dialog)) %>% group_by(character) %>% summarize(mean_dialog_length = mean(dialog_length)) %>% arrange(desc(mean_dialog_length))

# Fix weird data.
dialogs %>% filter(character=="BEVERLY'S")

dialogs_fixed <- dialogs %>%
  mutate(
    character = str_replace_all(character, "'S.*$", ""),
    character = str_replace_all(character, " VOICE", ""),
    character = str_replace_all(character, "\\.", ""),
    character = str_replace_all(character, "'", ""),
    character = str_replace_all(character, "S COM", ""),
    character = str_replace_all(character, " COM", ""),
    dialog_length = str_length(dialog)
  ) %>%
  filter(character %in% unlist(str_split("PICARD RIKER DATA TROI BEVERLY WORF WESLEY GEORDI", " ")))

dialogs_fixed %>% group_by(character) %>% summarize(mean_dialog_length = mean(dialog_length), std_dialog_length=sd(dialog_length)) %>% arrange(desc(mean_dialog_length))

dialog_len_per_ep <- dialogs_fixed %>% group_by(character, episode_number) %>% summarize(mean_dialog_length = mean(dialog_length), std_dialog_length=sd(dialog_length), .groups = "drop") %>% arrange(desc(mean_dialog_length))

dialog_len_per_ep

# Plot the data.
ggplot(dialogs_fixed) + geom_density(aes(x=dialog_length))

for_factor <- dialog_len_per_ep %>% group_by(character) %>% summarise(m=mean(mean_dialog_length)) %>% arrange(desc(m))
ggplot(dialog_len_per_ep, aes(factor(character,for_factor$character), mean_dialog_length)) + geom_boxplot()

dialog_len_per_ep <- dialogs_fixed %>% 
    group_by(character, episode_number) %>% 
    summarize(mean_dialog_length = mean(dialog_length), dialog_count=n(), .groups = "drop") %>% 
    arrange(desc(mean_dialog_length))

ggplot(dialog_len_per_ep, aes(dialog_count, mean_dialog_length)) + geom_point(aes(color=character)) + facet_wrap(~character)
})
```

## Question 1
In class, we left off on the plot below, which shows the distribution of dialog count by mean dialog length, 
where each point represents an episode. Interpret these results. How can we tell the character's role in the story by their plot?

```
ggplot(dialog_len_per_ep, aes(dialog_count, mean_dialog_length)) + 
    geom_point(aes(color=character)) + 
    facet_wrap(~character)
```

Each point represents an episode, and shows the mean amount of time that each character speaks per episode. 
You can tell the character's role by seeing the distribution of their dialogue length and how long they speak, 
on average, per episode. For some characters, most of the mean dialog length was concentrated around 0 (Beverly, Troi)
but some have a more even distribution and speak a different amount each time (Picard, Riker).

## Question 2
#### a) Compare Beverly's mean dialog per episode vs. mean dialog count per episode from season 1 (episodes 102-126) to season 3
(episodes 149-174) in a table.  
*Hints*:  
-   First, use `filter()` to get - 1) the dialog from only Beverly's character and 2) the episodes within the ranges given.
-   Then, add a season variable using `mutate()` with `case_when()`.
-   To create the means per episode, after your `mutate()` step, you'll need to `group_by()` season and episode number, then you can 
do your `summarize()` step to get the means by episode. At the end of the `summary()` statement (inside the parenthesis), add `.groups="drop"`. 
-   Then, to get the mean of means, you'll do the same as above, but only grouping by season. 
```
# filtering for beverly and relevant episodes
beverly_summary <- dialog_len_per_ep %>%
  filter(character == "BEVERLY",
         (episode_number >= 102 & episode_number <= 126) |
         (episode_number >= 149 & episode_number <= 174)) %>%

#adding season variable 
  mutate(season = case_when(
    episode_number >= 102 & episode_number <= 126 ~ "Season 1",
    episode_number >= 149 & episode_number <= 174 ~ "Season 3"
  ))

# grouping by season, getting mean of means 
beverly_means_by_season <- beverly_summary %>%
  group_by(season) %>%
  summarize(
    mean_dialogue_count = mean(dialog_count),
    mean_dialogue_length = mean(mean_dialog_length),
    .groups = "drop"
  )

# view result
print(beverly_means_by_season)

#### b) In class, we talked about this character saying the actress has stated that after she was fired and rehired, the writers
## began giving her storylines that made her feel like a male character. How is this reflected in our table?

They made her talk for longer, but the amount of times she talked per episode was a lot less. Men tend to speak for a long time in shows, 
but the amount of times they converse is often less. 

## Question 3
Let's compare the vocabulary richness (unique words / total words) of each character. 
#### a) Tokenize dialog into words, remove punctuation, convert to lowercase. Then filter out the stop words in the list below (from https://gist.github.com/sebleier/554280).
*Hint*: Here's a template for that this step should look like:

stop_words <- c(
  "i","me","my","myself","we","our","ours","ourselves","you","your","yours","yourself",
  "yourselves","he","him","his","himself","she","her","hers","herself","it","its","itself",
  "they","them","their","theirs","themselves","what","which","who","whom","this","that",
  "these","those","am","is","are","was","were","be","been","being","have","has","had",
  "having","do","does","did","doing","a","an","the","and","but","if","or","because","as",
  "until","while","of","at","by","for","with","about","against","between","into","through",
  "during","before","after","above","below","to","from","up","down","in","out","on","off",
  "over","under","again","further","then","once","here","there","when","where","why","how",
  "all","any","both","each","few","more","most","other","some","such","no","nor","not",
  "only","own","same","so","than","too","very","s","t","can","will","just","don","should","now"
)

tokens <- dialogs_fixed %>%
# Split each dialog into words
  mutate(word_list = str_split(dialog, "\\s+")) %>%
  
# Unnest the list column so each word is a row
  unnest(word_list) %>%
  
# Clean words
  mutate(
    word = str_remove_all(word_list, "[[:punct:]]"),  # Remove punctuation
    word = str_to_lower(word)                         # Convert to lowercase
  ) %>%
  
# Remove empty strings and stopwords
  filter(word != "", !word %in% stop_words)

#### b) Count unique words per character. Print a summary table with the following columns: character, total words, unique words, and vocabulary richness.  
*Hint*: Group by character, then use `summarize()` to get what you want. You'll use `n_distinct()` to get the unique word counts. 
Arrange in descending value of vocabulary richness (unique words/total words).

# summarizing total and unique words per character
vocab_summary <- tokens %>%
  group_by(character) %>%
  summarize(
    total_words = n(),
    unique_words = n_distinct(word),
    vocabulary_richness = unique_words / total_words,
    .groups = "drop"
  ) %>%
  arrange(desc(vocabulary_richness))

# View the summary
print(vocab_summary)

#### c) Plot total words versus vocab richness. 
-   Use the character names as the "points".
    -   *Hint*: Use `geom_text()` to add the character names as the points.
-   Do not include a legend.
    -   *Hint*: Use `theme()` to remove the legend.
-   Add a title and axis titles.
    -   *Hint*: Use `labs()` to add titles.

ggplot(vocab_summary, aes(x = total_words, y = vocabulary_richness)) +
  geom_text(aes(label = character), size = 3, check_overlap = TRUE) +
  labs(
    title = "Vocabulary Richness vs Total Words Spoken",
    x = "Total Words (Excluding Stopwords)",
    y = "Vocabulary Richness (Unique Words / Total Words)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

#### d) Interpret these results. 

Picard talks the most, but the their vocabulary richness is very low. In other words, they talk a lot, yet the diversity and uniqueness
of their vocabulary is very limited. Wesley, on the other hand, talks the least but their vocabulary is the most rich, meaning they say the 
most amount of unique words in comparison to the total words they speak. 

## Question 4
#### a) Find what episode Wesley left the show as a main character and state it explicitly. Meaning, find the first significant gap where he is not found in more than two episodes in a row. 
*Hint*: It's after season 3 (ended at episode 174), so you can filter out seasons 1-3 and print Wesley's dialog count per episode. Then, scan the table for the gap. 

wesley_counts <- dialogs_fixed %>%
  filter(episode_number > 174, character == "WESLEY") %>%
  count(episode_number, name = "wesley_lines")

print(wesley_counts)

Wesley left the show as a main character on episode 183. 

#### b) After Wesley leaves the main cast, in which episodes does he make cameo appearances?

He makes a cameo appearance at episode 206, 219, 263, and 272. 

#### c) Dig back into the data. Print:
-   Wesley's last piece of dialog before he left the main cast.
-   Wesley's last piece of dialog ever.  
  
*Hint*: To do this, you'll need to filter the `dialogs_fixed` data set to Welsey's lines and the episode number, and use `slice_tail(n = 1)` to get the last observation.

# filter so it's only showing wesley's lines 
wesley_lines <- dialogs_fixed %>%
  filter(character == "WESLEY") %>%
  select(episode_number, dialog) %>%
  arrange(episode_number)

# data frame for wesley's last line as main character
last_main_line <- wesley_lines %>%
  filter(episode_number <= 183) %>%
  slice_tail(n = 1)

# data frame for wesley's last line ever
last_ever_line <- wesley_lines %>%
  slice_tail(n = 1)

# printing results 
cat("Wesley’s LAST LINE as a main cast member (episode", last_main_line$episode_number, "):\n",
    last_main_line$dialog, "\n\n")

cat("Wesley’s FINAL LINE ever on the show (episode", last_ever_line$episode_number, "):\n",
    last_ever_line$dialog, "\n")

## Question 5
Create a heatmap with `dialog_len_per_ep` showing mean dialog length per episode for each character. Sort the characters on the y-axis by their overall mean dialog length, with the lowest on top using a factor. Add a title and an axis title. 
*Hints*:
For the factor:
##computing overall mean (mean of mean) dialog length per character (`group_by()` then `summarize()`), and arrange the overall mean in ascending order. Add `pull(character)` to the end of this step so that you can use character as a factor in the next step. Store all of this in a new tibble.

char_order <- dialog_len_per_ep %>%
  group_by(character) %>%
  summarize(overall_mean_length = mean(mean_dialog_length), .groups = "drop") %>%
  arrange(overall_mean_length) %>%
  pull(character)
  
##converting factor to order 

dialog_len_ordered <- dialog_len_per_ep %>%
  mutate(character = factor(character, levels = rev(char_order)))

## plotting 

ggplot(dialog_len_ordered, aes(x = episode_number, y = character, fill = mean_dialog_length)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +  # Optional, looks nice
  labs(
    title = "Mean Dialog Length per Episode by Character",
    x = "Episode Number",
    y = "Character",
    fill = "Mean Dialog\nLength"
  ) +
  theme_minimal()
  
