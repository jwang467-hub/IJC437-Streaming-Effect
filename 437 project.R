# Packages + Data

library(tidyverse)

# Make sure the df has been read in and year is numerical
df <- read_csv("billboard_24years_lyrics_spotify.csv") %>%
  mutate(year = as.numeric(year))

# 1) do not include duration_ms when dropping drop_na
eda_summary <- df %>%
  drop_na(year, danceability, energy, valence) %>%
  group_by(year) %>%
  summarise(
    Danceability = mean(danceability, na.rm = TRUE),
    Energy = mean(energy, na.rm = TRUE),
    Valence = mean(valence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -year, names_to = "Feature", values_to = "Value")

# 2) 
ggplot(eda_summary, aes(x = year, y = Value, color = Feature)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(size = 1.6, na.rm = TRUE) +
  facet_wrap(~Feature, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 2018, linetype = "dashed", alpha = 0.6) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Figure 1: Macro-Trends of Music Features (2000–2023)",
    subtitle = "Yearly averages of Spotify audio features",
    x = "Year",
    y = "Average value"
  )


# 2) Helper functions for lyrics

count_words <- function(text) {
  # Count the number of words
  if (is.na(text)) return(NA_integer_)
  str_count(text, "\\w+")
}

repetition_score <- function(text) {
  # repetition = 1 - unique/total
  if (is.na(text)) return(NA_real_)
  words <- str_extract_all(tolower(text), "\\w+")[[1]]
  if (length(words) == 0) return(NA_real_)
  1 - (length(unique(words)) / length(words))
}


# RQ1: Download vs Streaming (lyric length proxy) + Wilcoxon

df_rq1 <- df %>%
  drop_na(year, lyrics) %>%
  mutate(
    word_count = map_int(lyrics, count_words),
    Era = case_when(
      year >= 2000 & year <= 2010 ~ "Download Era",
      year >= 2018 & year <= 2023 ~ "Streaming Era",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(Era, word_count) %>%
  filter(word_count > 0)

print("（RQ1）")
print(table(df_rq1$Era))

lyric_stats <- df_rq1 %>%
  group_by(Era) %>%
  summarise(
    Median_Words = median(word_count),
    Mean_Words = mean(word_count),
    SD = sd(word_count),
    .groups = "drop"
  )
print(lyric_stats)

lyric_test <- wilcox.test(word_count ~ Era, data = df_rq1)
print(lyric_test)

ggplot(df_rq1, aes(x = Era, y = word_count, fill = Era)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.15, width = 0.6) +
  coord_cartesian(ylim = c(0, 800)) +
  theme_minimal() +
  labs(
    title = "RQ1: Lyric Length (Word Count) in Download vs Streaming Era",
    subtitle = paste("Wilcoxon p-value:", 
                     format.pval(lyric_test$p.value)),
    x = "Era", 
    y = "Word Count"
  ) +
  stat_compare_means(method = "wilcox.test", 
                     label = "p.signif", 
                     label.x = 1.5) +
  theme(legend.position = "none")


# RQ2: Year vs lyrical repetitiveness (Spearman) + trend plot

df_rq2 <- df %>%
  drop_na(year, lyrics) %>%
  mutate(rep_score = map_dbl(lyrics, repetition_score)) %>%
  drop_na(rep_score)

# Aggregate by 'year' and then calculate the correlation
df_rq2_year <- df_rq2 %>%
  group_by(year) %>%
  summarise(
    mean_rep = mean(rep_score),
    n = n(),
    .groups = "drop"
  )

cor_test <- cor.test(df_rq2_year$year, df_rq2_year$mean_rep,
                     method = "spearman", 
                     exact = FALSE)
print(cor_test)

ggplot(df_rq2_year, aes(x = year, y = mean_rep)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "RQ2: Trend of Lyrical Repetitiveness (2000–2023)",
    subtitle = paste0("Spearman rho test p-value: ", 
                      format.pval(cor_test$p.value)),
    x = "Year", 
    y = "Mean repetition score (1 - unique/total)"
  )


# RQ3: Valence trend over years (Linear regression on yearly mean)

df_rq3 <- df %>%
  drop_na(year, valence) %>%
  group_by(year) %>%
  summarise(mean_valence = mean(valence),
            n = n(),
            .groups = "drop")

rq3_model <- lm(mean_valence ~ year, data = df_rq3)
summary(rq3_model)

ggplot(df_trend, aes(x = year, y = mean_valence)) +
  geom_line(color = "grey70") + 
  geom_point(color = "steelblue", size = 2) + 
  geom_smooth(method = "lm", color = "red", fill = "pink") + 
  theme_minimal() +
  labs(title = "RQ3: The 24-Year Decline of Musical Happiness (Valence)",
       subtitle = "Linear regression showing the downward emotional trend",
       x = "Year", 
       y = "Average Valence Score")
