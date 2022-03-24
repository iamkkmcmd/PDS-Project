# Cleaning R-studio
rm(list = ls())
graphics.off()

# Load the libraries
pacman::p_load(dplyr, ggplot2, tidyr, tidytext, lubridate, stringr)

# Prilimary Plots ------------------------------------------------------------------------

# Setting working directory and load the clean data file
setwd("C:/Users/iamkkmcmd/Desktop/PDS Project/data/")
source("C:/Users/iamkkmcmd/Desktop/PDS Project/r_files/functions.R")
def <- read.csv("./data/clean_data.csv")

# Plot-1: Age distribution of respondents ------------------------------------------------
plt1 <- def %>% 
    ggplot(aes(x = sex, y = age, shape = sex, col = sex, size = 0.5)) +
    geom_jitter() +
    scale_shape_manual(values = c("Male" = "\u2642", "Female" = "\u2640")) +
    geom_crossbar(data = aggregate(age~sex, def, mean), 
                  aes(ymin = age, ymax = age),
                  width = 0.5, size = 0.2) +
    geom_text(data = def %>% group_by(sex) %>% summarise(avg_age = mean(age)),
              aes(label = paste("Mean = ",round(avg_age,0)), y = 47)) +
    geom_text(data = def %>% 
                  group_by(sex) %>% 
                  summarise(count = n()) %>% 
                  mutate(percent = paste0(round(count*100/sum(count), 2),"%")),
              aes(label = percent, y = 45)) +
    labs(x = "Sex", y = "Age",
         title = "Age Distribution of Respondents",
         col = "Sex")  + custom_theme('none')

ggsave("./figure/fig_12_wordclode.png",p,"png",dpi = 1500,width = 5,height = 5)


# Some summary is used inside plot
def %>% 
    group_by(sex) %>% 
    summarise(count = n()) %>% 
    mutate(percent = paste0(round(count/sum(count), 2),"%"))
def %>% group_by(sex) %>% summarise(avg_age = mean(age))

# Plot-2: Scatterplot of height and weight ----------------------------------------
def %>% 
    ggplot(aes(height, weight, col = sex)) +
    geom_point() + geom_rug() +
    facet_wrap(.~sex) +
    geom_point(data = def %>% 
                   group_by(sex) %>% 
                   summarise(avg_height = mean(height),
                             avg_weight = mean(weight)),
               aes(x = avg_height, y = avg_weight), size = 2, shape = 15, col = "black") +
    geom_text(data = def %>% 
                  group_by(sex) %>% 
                  summarise(corr = cor(height, weight)),
              aes(label = paste("Cor = ", round(corr,2)), x = 160, y = 100)) +
    labs(x = "Height (in cm)", y = "Weight (in Kg)", col = "Gender",
         title = "Scatter Plot of Height and Weight") +
    custom_theme("top") 
# Summary
def %>% 
    group_by(sex) %>% 
    summarise(corr = cor(height, weight))


# Plot-3: Gender-wise Work Preferences vs Weight ------------------------------------
def %>% 
    ggplot(aes(x = work, y = weight, fill = sex)) +
    geom_violin() + geom_sina(alpha = 0.5) +
    labs(x = "Work", y = "Weight", fill = "Sex",
         title = "Gender-wise Work Preferences vs Weight") +
    custom_theme()


# Plot-4: Density Plot of Diet Habits -----------------------------------------------
df_density <- def %>% 
    select(fruit,veg,cook,spend) %>% 
    mutate(id = 1:nrow(def)) %>% 
    relocate(id, .before = "fruit") %>% 
    gather(key, value, -c(id,spend))
p <- df_density %>% 
    filter(spend < 15000) %>% 
    ggplot(aes(x = spend)) +
    geom_density(aes(fill = key), alpha = 0.4) +
    facet_wrap(~value) + 
    scale_x_discrete(limits = seq(0,10000,2500), labels = c("","2.5K","5K","7.5K","10K")) +
    labs(x = "Amount of spending money", y = "Density",
         title = "Density plot of expenditure for fitness",
         subtitle = "(According to Diet Habits)", fill = "Diet Habit:   ",
         caption = "*Facet by number of meal/snacks contain corresponding diet habits") +
    custom_theme()
ggsave("./figure/fig4_density_plot_of_diet_habits.png",p,"png",dpi = 1500,width = 7,height = 5)

p <- def %>% 
  filter(income < 100000) %>% 
  ggplot(aes(x = income)) +
  geom_density(aes(fill = sex), alpha = 0.4) +
  scale_x_discrete(limits = seq(0,100000,25000), labels = c("0","25K","50K","75K","10K")) +
  labs(x = "Monthly Income", y = "Density", fill = "Sex:  ",
       title = "Gender-wise Density plot of monthly income") +
  custom_theme()
ggsave("./figure/fig4.1_density_plot_of_monthly_income.png",p,"png",dpi = 1500,width = 7,height = 5)


# Plot-5: Boxplot of Weights --------------------------------------------------------
def %>% 
    ggplot(aes(meal, weight)) +
    geom_boxplot(color = "mediumpurple3", size = 0.8, alpha = 0.65, outlier.color = "red") +
    labs(x = "Number of Meal", y = "Weight",
         title = "Boxplot of Weights", subtitle = "(By number of meal)") +
    custom_theme()


# Plot-6: Histogram of Expenditure for Fitness --------------------------------------
p <- def %>% 
    filter(spend < 5000) %>% 
    ggplot(aes(x = spend)) + 
    geom_histogram(aes(y = ..density..), binwidth = 250, fill = "slateblue") + 
    geom_density(kernel = "gaussian", col = "tomato4", size = 1) +
    labs(x = "Spend", y = "Probability", 
         title = "Histogram of Expenditure for Fitness") + 
    custom_theme()
ggsave("./figure/fig6_histogram_of_expenditure_for_fitness.png",p,"png",dpi = 1500,width = 5,height = 5)

# Plot-7.1: Donut Chart of Exercise Preferences ---------------------------------------
def$exercise <- str_replace_all(def$exercise,"Do not interested","Not interested")
exercises <- unique(trimws(unlist(strsplit(def$exercise, ','))))
mat <- t(sapply(def$exercise, function(x){ifelse(exercises %in% x, 1, 0)}))
rownames(mat) <- paste("Person", 1:nrow(def))
colnames(mat) <- exercises
df <- data.frame(exercises,freq = colSums(mat), row.names = NULL)
df <- df %>% mutate(r_freq = freq/sum(freq),
                    ymax = cumsum(r_freq),
                    ymin = lag(ymax, default = 0),
                    pos = (ymax + ymin)/2,
                    label = paste0(exercises,'-', round(r_freq*100,0), '%'))
df %>% 
    ggplot(aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = exercises)) +
    geom_rect() + xlim(c(2,4)) +
    coord_polar(theta = "y") +
    geom_label(x = 3.5, aes(y = pos, label = label), size = 3.2) +
    custom_theme("none") + 
    annotate("text", x = 2, y = 0.5, label = "EXERCISE", size = 10) +
    labs(x = " ", y = "", title = "Donut Chart of Exercise Preferences") +
    theme(axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank()) -> g
# ggsave("../figure/fig7_donut_chart_of_exercise_preferences.png",g,"png",dpi = 1500, height = 8, width = 8)


## Plot-7.2: Density of Walkers' Age -------------------------------------------------
def %>% 
    filter(exercise == "Walk") %>% 
    ggplot(aes(age)) +
    labs(x = "Age", y = "Density",
         title = "Density of Walkers' Age") +
    geom_density(fill = "#c21dbf", alpha = 0.5, size = 1,inherit.aes = T) +
    custom_theme() 

# Plot-8.1: Fondness of Fast-Food & Healthiness ---------------------------------------
def %>% 
    ggplot(aes(x = phy_ff, y = phy_health, col = sex)) +
    geom_jitter() +
    scale_x_discrete(labels = 1:10, limits = 1:10) +
    scale_y_discrete(labels = 1:10, limits = 1:10) +
    labs(x = "Rating of liking fast food",
         y = "Rating of healthiness",
         title = "Fondness of Fast-Food & Healthiness", col = "Sex") +
    custom_theme()

## Plot-8.2: Fondness of Fast-Food & Healthiness -------------------------------------
def %>% 
    filter(phy_ff <= 5 & phy_health <= 5) %>% 
    ggplot(aes(x = phy_ff, y = phy_health, col = gymtime)) +
    geom_jitter() +
    scale_x_discrete(labels = 1:5, limits = 1:5) +
    scale_y_discrete(labels = 1:5, limits = 1:5) +
    labs(x = "Rating of liking fast food",
         y = "Rating of healthiness",
         title = "Fondness of Fast-Food & Healthiness", col = "Gym-time") +
    custom_theme() -> g

ggsave("../figure/fig8_2_clustering.png",g,"png",dpi = 1500)

# Plot-9: Bar Chart of Ratings of this study ----------------------------------------
def %>% 
    group_by(rate, sex) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(rate, count, fill = sex)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(x = "Ratings", y = 'Frequency', fill = "Sex",
         title = "Bar Chart of Ratings of this study",
         subtitle = "(Grouped by gender)") +
    custom_theme()

# Plot-10: Weight & Healthiness According to Gym-time -------------------------------
def %>% 
    filter(gymtime != "More than 3 hours") %>% 
    ggplot(aes(x = factor(phy_health), y = weight, fill = gymtime)) +
    geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", shape = 5, colour = NA) +
    facet_wrap(~gymtime) + 
    scale_x_discrete(labels = 1:10, limits = 1:10) +
    labs(y = "Weight (in Kg.)",
         x = "Rating of healthiness",
         title = "Weight & Healthiness According to Gym-time", fill = "Gym-time") +
    custom_theme() 



# Plot-trying: Likart Scale plot ----------------------------------------------------
likart <- def %>% 
    select(starts_with('phy')) %>%
    mutate(id = 1:nrow(def)) %>% 
    relocate(id, .before = phy_ff) %>% 
    pivot_longer(-id)
likart %>% 
    # filter(id %in% sample(1:nrow(def), 15, replace = FALSE)) %>% 
    ggplot(aes(x = value, y = name, fill = name, group = id)) +
    geom_line(aes(col = factor(id))) +
    geom_point() + coord_flip()


likart %>% glimpse()
likart <- likart %>% filter(key == "phy_ff")
likart1 <-  t(sapply(likart$value, function(x){ifelse(1:10 %in% x, x, 0)}))
colnames(likart1) <- 1:10
likart <- cbind(likart,likart1)

likart %>%
    select(-c("key")) %>% 
    gather('id','value',2:11) %>% 
    ggplot(aes(x = '',y = value)) + geom_line()

sapply(def[,6:9], function(x){ifelse(x %in% 1:10, 1, 0)})

sapply(starwars$films, function(x){ifelse(unique(unlist(starwars$films)) %in% x, 1, 0)})

ggplot(def, aes(height, weight)) + geom_density2d()

# ggsave("../figure/fig_7_2_age_distribution_of_walkers.png",p,"png",dpi = 1500)



install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)

install.packages("tm")
library(tm)
#Create a vector containing only the text
text <- def$review
# Create a corpus  
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

tweets_words <-  docs %>%
  select(text) %>%
  unnest_tokens(word, text)
words <- tweets_words %>% count(word, sort=TRUE)

p <- wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))
wordcloud2(data=df, size=1.6, color='random-dark')
