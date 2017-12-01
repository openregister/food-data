# Explore what's in the food-types.json download

library(tidyverse)
library(stringr)
library(here)

x <- read_csv(here("lists", "food-types.csv"))
y <- x

# Most fields have NAs, but not all
map_lgl(y, anyNA)

# `skos:notation` is the same as the id code
y %>%
  mutate(id = str_sub(`@id`, -6L, -2L)) %>%
  select(`@id`, `id`, `skos:notation`) %>%
  filter(id != `skos:notation`)

# Everything is either a FootType or a Concept
y %>%
  mutate(type = str_split(`rdf:type`, ":"),
         type = map_chr(type, ~ .x[length(.x)])) %>%
  count(type, sort = TRUE)


count(y, `terms-foodtype:termType`)
count(y, `terms-foodtype:lastVersion`)
count(y, `terms-foodtype:state`)
count(y, `terms-foodtype:validFrom`)
count(y, `skos:notation`)
count(y, `skos:inScheme`)
count(y, `rdf:type`)
count(y, `skos:prefLabel`)
