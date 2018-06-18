library(tidyverse)

A <- read_tsv("shootings.tsv",
              col_types =
                  cols(
                      date = col_datetime(),
                      location = col_character(),
                      deaths = col_integer(),
                      injuries = col_integer(),
                      description = col_character()
                  ))

B <- A %>%
    filter(date > as.Date(max(A$date))-365*40) %>%
    mutate( year = format(date, "%Y") ) %>%
    group_by(year) %>%
    summarise(deaths=sum(deaths), injuries=sum(injuries)) %>%
    gather(type, count, deaths, injuries)

g <- ggplot(B, aes(x=year, y=count, fill=factor(type)))
g <- g + geom_col()
g <- g + theme(
             plot.caption = element_text(color = alpha("black", 0.75))
         )
g <- g + labs(
             x = "Year",
             y = "Casualties",
             title = "School shootings in the United States",
             caption = "Source: https://en.wikipedia.org/wiki/List_of_school_shootings_in_the_United_States"
         )
g <- g + scale_fill_discrete(name="Type")

ggsave("shootings.png", g, width = 18, height = 8, dpi = 125)
