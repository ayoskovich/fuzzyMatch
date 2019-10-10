require(dplyr)
require(stringr)
require(stringdist)
require(tidyr)
require(fuzzyjoin)

rem_comm <- function(string){
  return(as.numeric(str_replace(string, '<1', '0')))
}

items <- read.csv('data/tacoItems.csv', stringsAsFactors = FALSE) %>%
  mutate_at(vars(-c('itemName', 'menuType')), rem_comm) %>%
  filter(!(menuType %in% c('cantina menu', 'cantina beer, wine, and spirits',
                           'las vegas cantina', '16 oz fountain drinks',
                           '20 oz fountain drinks', '30 oz fountain drinks',
                           'hot sauce packets', 'freezies'))) %>%
  select(itemName, menuType)

prices <- read.csv('data/tacoPrices.csv', stringsAsFactors = FALSE) %>%
  mutate(price = as.numeric(str_remove(price, '\\$'))) %>%
  filter(!(genGroup %in% c('combo meals', 'drinks'))) %>%
  select(itemName, genGroup)

# Using package, not very good
View(items %>%
  stringdist_left_join(prices, by='itemName'))

# Remove matches
rem_matches <- function(){
  matches <- items %>%
    inner_join(prices, by='itemName')
  small_items <- items %>%
    filter(!(itemName %in% matches$itemName))
  small_prices <- prices %>%
    filter(!(itemName %in% matches$itemName))
  
  # Cartesian product and get string distances
  allBetween <- expand.grid(small_items$itemName, small_prices$itemName) %>%
    mutate(distance = stringdist(Var1, Var2)) %>%
    group_by(Var1) %>%
    arrange(distance) %>%
    filter(row_number(distance) == 1)
  
  return(allBetween)
}

View(rem_matches())
