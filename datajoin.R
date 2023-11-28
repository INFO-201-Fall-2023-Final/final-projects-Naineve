library(dplyr)
library(stringr)

more_df <- read.csv("mxmh_survey_results(1).csv") 
genre_df <- read.csv("data_by_genres.csv")

more_df$Fav.genre <- tolower(more_df$Fav.genre)


df <- merge(x=genre_df, y=more_df, 
            by.x = "genres", by.y = "Fav.genre",
            all.y = TRUE)

df <- mutate(df, negative.mental.level = Anxiety + Depression + Insomnia + OCD)

df <- df %>%
  mutate(Age_category = case_when(
    Age < 20 ~ "under 20",
    Age >= 20 & Age < 30 ~ "20 - 30",
    Age >= 30 & Age < 40 ~ "30 - 40",
    Age >= 40 & Age < 50 ~ "40 - 50",
    Age >= 50 ~ "above 50"
  ))

df_grouped <- group_by(df,genres) 
summarise_df <- summarize(df_grouped, avg_neg_mental = mean(negative.mental.level, na.rm = TRUE))


# Export the csv file
write.csv(x = df,file = "newData.csv")



