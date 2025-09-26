# Homework 5
This homework requires `wine.csv`, and the `tidyverse` and `Rtsne` packages. Install them if you haven't already!  
See the following link for how to add new packages to Binder: https://github.com/rjenki/BIOS512?tab=readme-ov-file#adding-packages-to-installr-later.   
**For readability and easier processing, please make each question part a different code chunk.**



install.packages("Rtsne")
library(tidyverse)
library(Rtsne)
library(ggplot2)
library(dplyr)
library(tibble)

    ── [1mAttaching core tidyverse packages[22m ──────────────────────── tidyverse 2.0.0 ──
    [32m✔[39m [34mdplyr    [39m 1.1.2     [32m✔[39m [34mreadr    [39m 2.1.4
    [32m✔[39m [34mforcats  [39m 1.0.0     [32m✔[39m [34mstringr  [39m 1.5.0
    [32m✔[39m [34mggplot2  [39m 3.4.2     [32m✔[39m [34mtibble   [39m 3.2.1
    [32m✔[39m [34mlubridate[39m 1.9.2     [32m✔[39m [34mtidyr    [39m 1.3.0
    [32m✔[39m [34mpurrr    [39m 1.0.1     
    ── [1mConflicts[22m ────────────────────────────────────────── tidyverse_conflicts() ──
    [31m✖[39m [34mdplyr[39m::[32mfilter()[39m masks [34mstats[39m::filter()
    [31m✖[39m [34mdplyr[39m::[32mlag()[39m    masks [34mstats[39m::lag()
    [36mℹ[39m Use the conflicted package ([3m[34m<http://conflicted.r-lib.org/>[39m[23m) to force all conflicts to become errors


## Question 1  

#### a) Import your data.  
#### b) Check out the columns present using one of R's data frame summary.  
#### c) Get summary statistics on the numeric variables.  

wine <- read_csv("/Users/ssagar/Desktop/UNC/BIOS512/BIOS512_assignments/wine.csv")
glimpse(wine)
summary(wine)

## Question 2

#### a) Scale and center your data  
## Scaling and centering reduces the impact of consistently occuring spikes. centering makes sense because 
## PCA is a rotation and you want to rotate around the centroid of the data 
*Hint:* Use a `mutate()` statement across all columns **except class** with `function(x) as.numeric(scale(x))`.

wine_scaled <- wine %>%
  mutate(across(.cols = -class, .fns = ~ as.numeric(scale(.x))))
summary(wine_scaled)

#### b) Based on what you saw in the summary statistic table from the imported data, why would scaling 
## and centering this data be helpful before we perform PCA?

We need to scale and center the data to reduce the impact of spikes in our data. PCA is a rotation, and we 
want to rotate around the center of our data to reduce the variance in our data and summarize our data into
information that is helpful. Scaling and centering the data puts all our variables on the same scale so that
variables with larger variances are less of a problem. Centering ensures that the mean of all variables is 0, 
which rotates our principal components around one origin. 

## Question 3

#### a) Perform PCA
pca_input <- wine_scaled %>% select(-class)
r <- prcomp(pca_input);
summary(r);

#### b) How much of the total variance is explained by PC1? PC2? What function do we use to see that information?
PC1: 36.20%
PC2: 19.21% 

We can use summary() to see the total variance. 

#### c) Why are we doing PCA first?

We are doing PCA first because we are reducing the dimensionality of our data by grouping 
it so that we can simplify it while still retaining as much information as possible. Eventually, we want to visualize
our data, so doing PCA beforehand helps us summarize our data so it is easier and more meaningful to visualize, especially 
when we have large, complex datasets. 

#### d) What is the rotation matrix? Print it explicitly.  
*Hint:* Check the notes for a simple way to do this!

r$rotation

#### e) Plot PC1 vs. PC2, using the wine class as labels for coloring.  
*Hint:* You'll first need a data set with only PC1 and PC2, then add back the class variable from your scaled data set with a `mutate()` statement. Then, you can use `color = factor(class)` in your `ggplot` statement.

pc_df <- as.data.frame(r$x[, 1:2])
pc_df <- pc_df %>%
  mutate(class = wine_scaled$class)
ggplot(pc_df, aes(x = PC1, y = PC2, color = factor(class))) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA: PC1 vs PC2", x = "PC1", y = "PC2", color = "Class") +
  theme_minimal()

#### f) What do you see after plotting PC1 vs. PC2? What does this mean in context of wine classes?
I see wine split up into 3 classes. The first class (Class 1) is mostly negative for PCA1 and PCA2. The second
class (Class 2) is mostly positive for PC2 but distributed evenly from -2.5 to 2.5 for PC1. The third class (Class 3)
is mostly negative for PC2 and mostly concentrated around 2.5 for Class 3. In the context of wine classes, there is little overlap
between each class, which demonstrates that PC1 and PC2 captures the variation separating the classes. The points closer together 
show wines that are similar to each other, and the points farther away from each other are different. The fact that most of the classes
don't overlap shows that the classes are pretty distinct from one another in terms of the data we have, like alcohol, ash, magnesiu, flavinoids,
etc. 

#### g) Give an example of data where PCA would fail. You can describe the data or do a simulation.  
*Hint:* Our notes have a few examples!

If there is low variance in my dataset, and all my data is really similar to each other, PCA might fail because 
the principal components would have low variance and would not differentiate between each other. My data would not rotate around
anything because all aspects of it would be the same. 

#### h) Explain the difference between vector space and manifold, and how these terms apply to what we did/will do with T-SNE.

A vector space is a combination of a set and field for which certain elements are true and in
which you can add vectors together. Vectors can be described with linear coordinates. A manifold is a set which has the property that it resembles a 
vector space locally but can be curved or twisted. T-SNE assumes that your data lies on a manifold, not a flat vector space, but that our data is 
low-dimensional. So, we will be plotting in 2D.

## Question 4
#### a) Perform T-SNE
##Set `seed = 123`.  
##*Hint: Subset your PCA results to PC1–PC10, add the class variable back in, remove duplicates, then perform T-SNE.

set.seed(123)
pca_scores <- as.data.frame(r$x[, 1:10])
pca_scores$class <- wine_scaled$class 
pca_unique <- pca_scores %>% distinct()

tsne_input <- pca_unique %>% select(-class)
tsne_result <- Rtsne(tsne_input, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

tsne_df <- as.data.frame(tsne_result$Y)
colnames(tsne_df) <- c("TSNE1", "TSNE2")
tsne_df$class <- pca_unique$class

#### b) Plot the results in 2D
##*Hint: Convert your T-SNE results to a tibble and add back the class variable from your scaled data set using a `mutate()` statement. Then, you can use `color = factor(class)` in your `ggplot` statement.

colnames(tsne_result$Y) <- c("TSNE1", "TSNE2")
tsne_df <- as_tibble(tsne_result$Y) %>% mutate(class = pca_unique$class)

ggplot(tsne_df, aes(x = TSNE1, y = TSNE2, color = factor(class))) +
  geom_point(alpha = 0.7) +
  labs(
    title = "T-SNE Results (2D)",
    x = "T-SNE 1",
    y = "T-SNE 2",
    color = "Class"
  ) + theme_minimal()

#### c) Why didn't we stop at PCA?

T-SNE is essentially untangling what we rotated using PCA in non-linear manifolds, so our data is distributed in a more 
understandable visual format with a clear relationship. T-SNE has clearer class separation, and also helps us think about the probability
of two points in our dataset being close to each other or far apart. It simplifies our data, but on a non-linear scale.

#### d) What other types of data does this workflow make sense for?

I could see this workflow being useful when creating some sort of composite score from a lot of different
factors, something we do a lot in epidemiology. For example, if we are trying to gain information about a variable 
like wealth, we could survey people and ask a bunch of different questions about their assets, family, income, etc. 
This data would probably be on a bunch of different scales and the columns we would get would tell lots of different stories,
but when we do a PCA, we could scale and center then split our data up to get a better understanding of variance in our dataset and 
create a distribution of wealth across our cohort. We could then use T-SNE to visualize our data on a non-linear scale. 



