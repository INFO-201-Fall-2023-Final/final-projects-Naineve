library(dplyr)
library(stringr)
library(ggplot2)

more_df <- read.csv("mxmh_survey_results(1).csv") 
genre_df <- read.csv("data_by_genres.csv")

genre_names <- colnames(df)[25:40]
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

choices<- c("under 20", "20 - 30", "30 - 40", "40 - 50", "above 50")
numeric_values <- c(1, 2, 3, 4, 5)
reordered_age <- choices[order(numeric_values)]

scatter <- ggplot(df, aes(x = Hours.per.day, y = negative.mental.level, color = genres)) +
  geom_point() +
  labs(
    x = "The time of listensing music",
    y = "The negative level of mental health(higher, worse)",
    title = "Avg. Carbon footprint per Capita vs Avg. Median Household Income",
    caption = "time of listening versus negative level of mental level"
  )

plot(scatter)

df_new <- df[df$Music.effects != "",]

counts <- table(df_new$Music.effects)
category_percentages <- prop.table(counts) * 100
data <- data.frame(Category = names(counts), Count = as.numeric(counts))

pie_chart <- ggplot(data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", category_percentages)), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +  # Convert the bar chart into a pie chart
  theme_void() +  # Remove unnecessary elements
  labs(title = "Proportion table of the reported effects on mental health")
column_mapping <- c(
  "Frequency..Classical." = "Classical",
  "Frequency..Country." = "Country",
  "Frequency..EDM." = "EDM",
  "Frequency..Folk." = "Folk",
  "Frequency..Gospel." = "Gospel",
  "Frequency..Hip.hop." = "Hip-hop",
  "Frequency..Jazz." = "Jazz",
  "Frequency..K.pop." = "K-pop",
  "Frequency..Latin." = "Latin",
  "Frequency..Lofi." = "Lofi",
  "Frequency..Metal." = "Metal",
  "Frequency..Pop." = "Pop",
  "Frequency..R.B." = "R&B",
  "Frequency..Rap." = "Rap",
  "Frequency..Rock." = "Rock",
  "Frequency..Video.game.music." = "Video game"
)
colnames(df)[25:40] <- column_mapping[names(df)[25:40]]





