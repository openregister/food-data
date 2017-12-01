# Explore food types from the RDF SPARQL JENA queries

library(tidyverse)
library(here)

x <- read_tsv(here("lists", "query-result.tsv"), quote = "")

# Everything only has one parent in each hierarchy, I think
count(x, `?notation`, sort = TRUE)

# Only 194 have different ingred parents from biomo parents.
filter(x, `?biomo_notation` != `?ingred_notation`) %>%
  glimpse()

# No NAs
filter(x, is.na(`?biomo_notation`))
filter(x, is.na(`?ingred_notation`))
filter(x, is.na(`?expo_notation`))
filter(x, is.na(`?pest_notation`))
filter(x, is.na(`?racsource_notation`))
filter(x, is.na(`?report_notation`))

y <- read_tsv(here("lists", "query-result3.tsv"), quote = "")

# Everything still only has one parent in each hierarchy.  See query-result2.tsv
# for a parent with many children in the same hierarchy.  Query2 is the inverse
# of Query1 -- it gets everything and its children, rather than its parent.
count(y, `?notation`, sort = TRUE)

filter(y, `?biomo_notation` != `?ingred_notation`) %>%
  glimpse()

z <- read_tsv(here("lists", "query-result2.tsv"), quote = "")

z %>%
  distinct(`?biomo_notation`)
x %>%
  distinct(`?notation`)

# optional-result is the result of a query where all values apart from
# `?notation` are optional, so the thing in each row isn't necessarily part of
# any hierarchy.
optional <- read_tsv(here("lists", "optional-result.tsv"), quote = "")

filter(optional, !is.na(`?biomo_notation`))
filter(optional, !is.na(`?ingred_notation`))
filter(optional, is.na(`?biomo_notation`), !is.na(`?ingred_notation`))
