require(dplyr)
require(stringr)
require(stringdist)
require(tidyr)

rem_comm <- function(string){
  return(as.numeric(str_replace(string, '<1', '0')))
}

items <- read.csv('data/tacoItems.csv', stringsAsFactors = FALSE) %>%
  mutate_at(vars(-c('itemName', 'menuType')), rem_comm)

prices <- read.csv('data/tacoPrices.csv', stringsAsFactors = FALSE) %>%
    mutate(price = as.numeric(str_remove(price, '\\$')))

matches <- items %>%
  inner_join(prices, by='itemName') %>%
  select(itemName)



dropSplit <- function(df, delim){
  df %>%
    mutate(old = itemName) %>%
    separate(itemName, c('prefix','B'), sep=delim) %>%
    filter(!is.na(B)) %>%
    select(prefix) %>%
    unique()
}

# 4 matches if we look at prefix
prefMatches <- dropSplit(items, '-') %>%
  inner_join(dropSplit(prices, '\\('), by='prefix')

# Investigate those (pick up 20 obs)
reg <- paste(prefMatches$prefix, collapse = '|')
items %>%
  filter(grepl(pattern=reg, itemName))


# Cartesian product and get string distances (probs not the best way to do this)
diff_items <- items %>% filter(!itemName %in% matches$itemName)
diff_prices <- prices %>% filter(!itemName %in% matches$itemName)

allBetween <- expand.grid(diff_items$itemName, diff_prices$itemName) %>%
  mutate(distance = stringdist(Var1, Var2))
