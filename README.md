# dcproj_2_import_clean_wwc
---
title: "dcproj2_importclean_wwc"
author: "John Griffiths"
date: "November 28, 2019"
output: word_document
---
##1. Importing data part 1

What an exciting FIFA Women's World Cup! Congratulations to the US Women's National Team and to all the national teams who made the tournament such a great event!

footballSource: Wikimedia Commons

Now that the tournament is over, our boss at Crunching Numbers needs to know which match/stadium had the highest attendance during the women's world cup. Thankfully, one of our trusty interns, Bob, already has the data for us. After chatting with Bob about his football (soccer in the US) obsession, we learned that he collected the data by either watching the games or reading the FIFA Women's World Cup match reports. He manually entered the data into a flat file (.csv) and emailed it to us.

Let's read in the file and explore the raw data!

```{r, echo=TRUE}
# In this project code scaffolding will only be given for functions that were not 
# explained in the prerequisite courses. Look at the hints if you need help.

# Load the packages
library(readr)
library(dplyr)

# Read in the data from the datasets folder
wwc_raw<- read_csv("datasets/2019_WWCFIFA_summary.csv")

# Check the dimensions and structure of the data
dim(wwc_raw)
class(wwc_raw)
summary(wwc_raw)
str(wwc_raw)
glimpse(wwc_raw)
head(wwc_raw)
```
##2. Importing data part 2
Looking at the outputs, we notice a few things about the data. First, we have some NAs to address. Second, most of the columns are of type character. One of the differences between read_csv() (readr) and read.csv() (utils) is how character strings are treated. With read.csv() strings are coerced to factors, while in read_csv() strings are not coerced. Let's import the data again, but this time, we will assign data types to the Round, Date, and Venue columns.



Instructor's note: My apologies for several incorrect spellings. Stade Oceane should be Stade Océane. The correct spellings of the names of the referees are Stéphanie Frappart, Katalin Kulcsár, and Claudia Umpiérrez. Accents are not currently supported in Jupyter Notebook outputs on DataCamp and had to be removed from the data file.
```{r, echo = TRUE}
# Read in the data specifying column types
wwc_raw <- read_csv("datasets/2019_WWCFIFA_summary.csv",
                col_types = cols(
                                Round = col_factor(),
                                Date = col_date(format = "%m/%d/%y"),
                                Venue = col_factor() 
                                  )
                 )

# Look at the summary and structure of the data
summary(wwc_raw)
glimpse(wwc_raw)

# Print the dataset
wwc_raw
```           
##3. Removing rows of NA
We have 55 observations (rows) of 13 variables (columns). Hmmm, we know there were 52 games - why the extra rows? Also Round and Attendance each have three NA, and Date and Venue each have four NA. It looks like we have a few things to fix.

Rows of NA
Missing data values
Multiple values in one column (look at Score and PKS)
Column headers are a mix of upper- and lowercase letters
The last issue is more of a preference. Having all the column names in the same case will make typing easier.

Great hustle on Bob's part for collecting the information we need, but he gets a yellow card for inserting the rows of NA. Let's start cleaning the data by putting the column names in lowercase and removing the rows of NA.
```{r, echo = TRUE}
# load the package
library(tidyr)

# Remove rows of NA
wwc_1  <- wwc_raw  %>% 
 rename_all(tolower)  %>% 
 filter(!is.na(round))
# Could also use: drop_na(round)

# Get the dimensions and inspect the first 10 and last 10 rows
dim(wwc_1)
head(wwc_1, n = 10)
```

##4. Replacing NA
Excellent! We now have 52 rows. Each row corresponds to a match in the tournament. But, it looks like there are a couple NA still lurking about in date and venue. Using colSums() and is.na() we can check to see how many NA are in each column.

colSums output



We only have the one NA in date and one NA in venue. We would expect the notes column to have several NA, but what about pks? pks is the column for penalty kicks. It has 51 NA. A good guess would be that only one match ended in penalty kicks, and according to Bob, that is correct. We'll deal with the double data in score and pks shortly. Now we are going to clean date and venue.
```{r, echo = TRUE}
# Housekeeping
wwc_2  <- wwc_1

# Find, view, and replace NA in column date
index_date  <- which(is.na(wwc_2$date))
wwc_2[index_date, ]
wwc_2$date[index_date] <- "2019-06-09"

# Find, view, and replace NA in column venue
index_venue  <- which(is.na(wwc_2$venue))
wwc_2[index_venue, ]
wwc_2$venue[index_venue]  <- "Groupama Stadium"
```

##5. separate() and replace_na()
All right! The data are looking good, but it is a good idea to get the two data points in score and two data points in pks into their own columns for future data sleuthing.

For this task we're going to employ the functionality of separate(), mutate(), and replace_na(). Look back at the directions in Task 4 to see an example of how mutate() and replace_na() are used together. We'll use the pipe operator, %, to pipe the functions together for readability.
```{r, echo = TRUE}
# Separate columns and replace NA 
wwc_3  <- wwc_2 %>%
  separate(score, c("home_score", "away_score"), sep = "-", convert = TRUE) %>%
  separate(pks, c("home_pks", "away_pks"), sep = "-", convert = TRUE) %>%
  mutate(home_pks = replace_na(home_pks, 0), away_pks = replace_na(away_pks, 0))

# Print the data
wwc_3
```

##6. Plotting for outliers
We corrected the NA in the date and venue columns, and separated the score and pks columns to have one score per column.

Now we can take a look at attendance and find the information the boss wants. Let's plot the data to see if there are any outliers.
```{r, echo = TRUE}
# Housekeeping for plot size
options(repr.plot.width=6, repr.plot.height=4)

# Load the package
library(ggplot2)

# Make a boxplot of attendance by venue and add the point data
ggplot(wwc_3, aes(x = venue, y = attendance))+
  geom_boxplot()+
  geom_jitter(color = "red", size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
```
##7. What to do with the outlier?
Whoa! What’s up with the attendance for Groupama Stadium? One data point is almost 600,000 (6e+05) while all the other data points are less than 100,000. That does not seem right.

After chatting with Bob about the outlier and checking the match report, we learned that Bob mistakenly added an extra 0. We can fix that! Let's summarize the attendance by the venue, fix the outlier, and create a new summary table with the updated data.

For fun, add scale_y_continuous(labels = scales::comma) as the last line of the call to ggplot() in Task 6 and see how the y-axis value labels change.
```{r, echo = TRUE}
# Summarize the number of games, and min and max attendance for each venue
wwc_3  %>% 
  group_by(venue) %>%
  summarize(nb_of_games = n(), 
           min_attendance = min(attendance),
           max_attendance = max(attendance))

# Correct the outlier
wwc_4  <- wwc_3  %>% 
  mutate(attendance = replace(attendance, which(attendance == 579000), 57900))

# Print an updated summary table 
wwc_venue_summary <- wwc_4  %>% 
  group_by(venue) %>%
  summarize(nb_of_games = n(), 
           min_attendance = min(attendance),
           max_attendance = max(attendance))

wwc_venue_summary
```

##8. A pretty boxplot
Let's make a boxplot of the attendance by venue again. The first outlier was extreme and might have masked other problems.

This time we will clean up the plot by setting the theme, adding a title, a subtitle, x and y labels, and we will flip the axes to make it easier to read the venue names. We are also going to angle the x-axis text so the numbers will fit within the plot space. And to be fancy, we are going to reorder venue by the attendance within the plot aesthetics so the venues display in descending order of mean attendance.
```{r, echo = TRUE}
# Housekeeping for plot size
options(repr.plot.width=6, repr.plot.height=4)

# Prettier boxplot of attendance data by venue
wwc_4  %>% 
  ggplot(aes(x = forcats::fct_reorder(venue, attendance), y = attendance)) +
    geom_boxplot()+
    geom_jitter(color = "red", size = 0.5)+
    coord_flip() +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, , hjust = 1)) +
    labs(title = "distribution of attendance by stadium",
         subtitle = "2019 FIFA Women's World Cup",
         x = "Stadium", 
         y = "Attendance")
```
##9. A pretty line plot
Looks good!

That outlier for Stade de Nice looks a little suspect, but we checked the match reports, and the attendance numbers are correct. The suspect match was France vs. Norway - the home nation favorite drew a large crowd wherever they played!

Now that the data are clean, let's plot the attendance at each stadium over time so we can tell the boss which stadium had the highest attendance during the tournament.
```{r, echo = TRUE}
# Housekeeping for plot size
options(repr.plot.width=6, repr.plot.height=4)

# Line plot of attendance over time
wwc_4  %>% 
  ggplot(aes(x = date, y = attendance, color = venue)) +
  geom_line()+
  theme_minimal() +
  theme(legend.position = "bottom",
       legend.text = element_text(size = 8)) +
  guides(col = guide_legend(nrow = 3)) +
  labs(title = "Stadium attendance during the tournament",
       subtitle = "2019 FIFA Women's World Cup",
       x = "Date", 
       y = "Attendance",
      color = "") 
```
##10. Wrap up
Congratulations! You've made it to the end of the project.

Great job! We have a couple of beautiful plots to give our boss. With a little more code, we can answer her questions:

What match had the highest attendance?
In what stadium was the match with the highest attendance played?
```{r, echo = TRUE}
wwc_4 %>%
    arrange(desc(attendance))

# What match had the higest attendance?
# A: wk = SMIF, England vs. USA
# B: wk = FIN, USA vs. Netherlands
# C: wk = SMIF, Netherlands vs. Sweden

ans_1  <- "B"

# In what stadium was the match with the highest attendance played?
# A: Groupama Stadium
# B: Parc des Princes
# C: Stade des Alpes

ans_2  <- "A"
