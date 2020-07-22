## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(42)
data <- read_csv("netflix_titles.csv")


## ---------------------------------------------------------------------------------------------------------------------------------
glimpse(data)


## ---------------------------------------------------------------------------------------------------------------------------------
n_distinct(data$country)


## ---------------------------------------------------------------------------------------------------------------------------------
sum_type <- data %>% group_by(type) %>% 
    count(.)
sum_type


## ----warning=FALSE----------------------------------------------------------------------------------------------------------------
tidy_countries <- data %>% 
    separate(
        country, 
        into = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"), 
        sep = ",", 
        convert = TRUE
    )

longer_countries <- tidy_countries %>% 
    pivot_longer(
        c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"),
        names_to = "country_sect",
        values_to = "country_long"
    ) %>% 
    drop_na(country_long)


## ---------------------------------------------------------------------------------------------------------------------------------
print(
    paste(
        "O dataset possui", 
        nrow(filter(data, is.na(data$country))), 
        "filmes/séries sem país definido", 
        sep = " "
    )
)


## ---------------------------------------------------------------------------------------------------------------------------------
count_country <- longer_countries %>% 
    select(country_long) %>%
    mutate(country_long = trimws(country_long)) %>% 
    group_by(country_long) %>% 
    count(country_long) %>%
    arrange(desc(n))

count_country_fit <- count_country %>% drop_na(country_long) %>% arrange(desc(n))
top_countries <- head(count_country_fit, n = 20)
top_countries


## ---------------------------------------------------------------------------------------------------------------------------------
ggplot(top_countries, aes(x = reorder(country_long, -n), y = n)) +
    geom_bar(aes(fill = n), stat = "identity") +
    scale_fill_gradient(low = "orange", high = "red") +
    theme_minimal() +
    labs(x = "", y = "") +
    theme(axis.text.x = element_text(angle = 90), legend.position = "none")


## ---------------------------------------------------------------------------------------------------------------------------------
sum_rating <- data %>% group_by(rating) %>% 
    count(.) %>% 
    arrange(desc(n))

sapply(names(sum_rating)[-1], function(x) {
    sum_rating[paste0("percentage")] <<- sum_rating[x] / sum(sum_rating[x])
})
sum_rating

ggplot(sum_rating, aes(x = "", y = n, fill = rating)) +
    geom_bar(stat = "identity", width = 0.5, color = "white") +
    theme_void() +
    coord_polar("y", start = 0) +
    labs(title = "Classificação do Conteúdo")


## ----warning=FALSE----------------------------------------------------------------------------------------------------------------
year <- data %>% group_by(release_year) %>% count(.) %>% arrange(release_year)

## Removeremos 2020 pois ainda é ano em aberto

year_fit <- year %>% filter(release_year != "2020")

ggplot(year_fit, aes(x = release_year, y = n)) +
    geom_smooth(formula = y ~ x, method = "loess", se = FALSE, lty = "dashed") +
    geom_line(lwd = 1.5, alpha = 0.5) +
    theme_minimal() +
    labs(title = "Catálogo de Lançamentos por Ano", x = "", y = "")


## ---------------------------------------------------------------------------------------------------------------------------------
movies <- data %>%
    filter(type == "Movie")

head(movies)


## ---------------------------------------------------------------------------------------------------------------------------------
movies$duration <- as.numeric(gsub(" min", "", movies$duration))


## ---------------------------------------------------------------------------------------------------------------------------------
mean_duration <- movies %>% 
    group_by(release_year) %>%
    summarise(mean = mean(duration)) 

ggplot(mean_duration, aes(release_year, mean)) +
    geom_point(alpha = 0.5, shape = 4) +
    geom_smooth(formula = y ~ x, method = "loess", se = FALSE) +
    labs(title = "Duração Média dos Filmes", x = "", y = "minutos") +
    theme_minimal()

