###############
### PAKIETY ###
###############

### PAKIETY W R ###

# INSTALOWANIE
# install.packages('dplyr')

# AKTUALIZACJA
# update.packages()

# ??ADOWANIE
library(dplyr)

library(tidyverse)

?stats::filter

# ZARZ??DZANIE
search()
detach('package:dplyr')

##############
### TIBBLE ###
##############

library(dplyr)

setwd("~/23Z/testowy")

readRDS('./diamonds1.rds') -> data; head(data)
readr::read_rds('./diamonds1.rds') -> data; head(data)

tictoc::tic()
readr::write_rds(data, './test0.rds')
tictoc::toc()
tictoc::tic()
readr::write_rds(data, './test1.rds', compress = 'xz')
tictoc::toc()

tictoc::tic()
readr::read_rds('./test0.rds') -> dt0
tictoc::toc()
tictoc::tic()
readr::read_rds('./test1.rds') -> dt1
tictoc::toc()

data
str(data)

tibble::as_tibble(data) -> data

data
tibble::glimpse(data)

################
### MAGRITTR ###
################

library(magrittr)

# PRZECHOWYWANIE WYNIKÓW PO??REDNICH
data$carat -> res
cut(
    res, breaks = seq(min(data$carat), max(data$carat), length.out = 10),
    include.lowest = TRUE
) -> res

table(res, useNA = 'ifany') -> res
prop.table(res) -> res
round(res, 5) -> res
res

round(prop.table(table(res, useNA = 'ifany')), 5)


# PIPELINE (ctr + shift + M)
res1 <- data$carat %>%
    cut(
        breaks = seq(min(data$carat), max(data$carat), length.out = 10),
        include.lowest = TRUE
    ) %>%
    table(useNA = 'ifany') %>%
    prop.table() %>%
    round(5)

1 %>% sum(2) # <=> sum(1, 2)

x <- 1

x <- x %>% sum(2); x

# MAGRITTR

# x %<>% fun() <=> x <- x %>% fun()

x %<>% sum(2); x

# %T>%

v <- 1:10

v %T>% 
    plot(1:10) %>% 
    sum() %>% 
    print()

my_fun1 <- function(x, y = 0) {
    x + y * 2 -> z
    
    return(z)
}

1 %>% my_fun1(5, .)
1 %>% my_fun1(5)

# R Native Pipe (4.1.0)

data$carat |>
    cut(
        breaks = seq(min(data$carat), max(data$carat), length.out = 10),
        include.lowest = TRUE
    ) |>
    table(useNA = 'ifany') |>
    prop.table() |>
    round(5)

# wybór innego argumentu funkcji wywo??ywanej w pipeline

1 |> (function(x) my_fun1(5, x))()

# Skrócone definiowanie funkcji w R 4.1.0

1 |> (\(x) my_fun1(5, x))()

########################
### DPLYR - OPERACJE ###
########################

### FILTER ###

data %>% dfilter(cut == 'Ideal' | price > quantile(price, probs = 0.9))

### SELECT ###

data %>% select(cut, carat, price)

data %>% select(-x, -y, -z)

data %>% select(cut) %>% distinct()

data %>% pull(carat) %>% range()

# TIDYSELECT

data %>% dplyr::select(tidyselect::contains("c")) %>% head()

### MUTATE, TRANSMUTE, IF_ELSE ###

data %>% mutate(ppc = price / carat, size = x * y * z, pps = price / size,
                .after = carat)

data %>% transmute(price, carat, ppc = price / carat) %>% head()

data %>% transmute(price, below330 = if_else(price < 330, "TAK", "NIE"))

### ARRANGE ###

data %>% arrange(table)
data %>% arrange(table, depth)
data %>% arrange(desc(table))

### SUMMARISE ###

data %>%
    summarise(
        carat_min = min(carat),
        carat_avg = mean(carat),
        carat_max = max(carat),
        price_avg = mean(price)
    )

### GROUP_BY + SUMMARISE ###

data %>%
    group_by(clarity) %>%
    summarise(price = mean(price))

data %>%
    group_by(clarity, color) %>%
    summarise(price = mean(price)) %>%
    ungroup()

data %>%
    group_by(clarity, color) %>%
    summarise(price = mean(price), .groups = 'drop')

### GROUP_BY + TALLY ###

data %>% 
    group_by(color) %>%
    tally()

## COUNT, n()

data %>% count(clarity)

data %>% 
    group_by(clarity) %>% 
    summarise(n = n())

data %>% 
    group_by(clarity) %>%
    tally()

##############################
### DPLYR - ????CZENIE TABEL ###
##############################

### BIND ROWS ###

data %>% slice(1:5) -> data_part1; data_part1
data %>% slice(6:10) -> data_part2; data_part2

bind_rows(data_part1, data_part2)

### BIND COLS ###

data %>% slice(1:10) %>% select(carat:color) -> data_part1; data_part1
data %>% slice(1:10) %>% select(clarity:price) -> data_part2; data_part2

bind_cols(data_part1, data_part2)

### JOIN ###

data %>% mutate(id = 1:n()) %>% slice(1:10) -> tmp
tmp %>% select(id, carat:table) %>% slice(1:4, 6:9) -> diam_descr; diam_descr
tmp %>% select(ids = id, price) %>% slice(2:6, 8:10) -> diam_price; diam_price
diam_descr %>% full_join(diam_price, by = c('id' = 'ids'))

# inner_join left_join right_join full_join
