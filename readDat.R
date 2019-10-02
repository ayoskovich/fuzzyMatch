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

View(items %>%
       inner_join(prices, by='itemName'), title='Matches')

View(items %>%
  anti_join(prices, by='itemName'), title='Diffs')

one <- items %>%
       mutate(old = itemName) %>%
       separate(itemName, c("A", "B"), sep='-') %>%
       filter(!is.na(B))

View(items %>%
  filter(str_count(itemName, '-') == TRUE), title='All with -')

# Cartesian product and get string distances (probs not the best way to do this)
allSelf <- expand.grid(items$itemName, items$itemName) %>%
  mutate(distance = stringdist(Var1, Var2))

allBetween <- expand.grid(items$itemName, prices$itemName) %>%
  mutate(distance = stringdist(Var1, Var2))
