library(tidyverse)
library(readxl)
library(here)
library(stringr)

source_data <- read_csv(here("lists", "approved-food-establishments.csv"))

# * Species: comma-delimited concatenation of all subsequent columns, only
#   applies to SH (slaughterhouse) under Part A Sections I and II.
# * AuctionHall: this and subsequent columns are 'yes' if an auction hall is
#   approved in any category, but it could be in more than one, e.g. Potts
#   (Bakers) Ltd has a packaging plant approved in two categories
# * Remarks: not a thing
# * `PartA(AllSections)`: comma-delimited concatenation of previous columns, even
#   when those columns are themselves comma-delimited.
# * `PartB(AllSections)`: only has sprouts in it
#   http://data.food.gov.uk/codes/business/eu-approved-establishments/categories/_B
# * CompetentAuthority: It's per AppNo, but multiple approvals per AppNo, so a
#   separate register?
# * Nothing under PartASectionXVIHoney: expected
# * Nothing under Reefervessel: expected
# * Nothing under Tannery: expected
# * AddressWitheld: 'blank' and 'No' are equivalent
# * DH 003 and MD 018 shouldn't be in there.

# * What's the relationship between species, and parts and sections?

# Melt into one row per AppNo, Part and Section
facilities <-
  source_data %>%
  select(AppNo, starts_with("Part"), -`PartA(AllSections)`, -`PartB(AllSections)`) %>%
  gather(category, facilities, -AppNo) %>%
  filter(!is.na(facilities)) %>%
  arrange(AppNo, category, facilities) %>%
  mutate(part = str_sub(category, 5L, 5L),
         section = str_extract(str_sub(category, 13L), "[0-9A-Z]+(?=[A-Z])"),
         # Sections actually include the Part letter in their ID, e.g. these two
         # are different
         # http://data.food.gov.uk/codes/business/eu-approved-establishments/categories/_A-I
         # http://data.food.gov.uk/codes/business/eu-approved-establishments/categories/_B-I
         section = paste(part, section, sep = "-"),
         facility = str_split(facilities, ", ")) %>%
  unnest(facility) %>%
  mutate(facility = str_extract(facility, "[[:alnum:]]+(?= )")) %>%
  select(-category, -facilities)

# Melt into one row per AppNo and species
species <-
  source_data %>%
  select(AppNo, Poultry:WildLandMammalsOtherThanWildUngulatesAndWildLagomorphs) %>%
  gather(species, value, -AppNo) %>%
  filter(!is.na(value)) %>%
  select(-value) # all values are "Yes"

# Create a species register.  This will need manual tweaking to rename
# "WildBirds" to "Wild Birds" etc.
species %>%
  distinct(species) %>%
  transmute(name = species,
            `approved-food-species` = row_number() + 10L,
            `start-date` = NA,
            `end-date` = NA) %>%
  select(`approved-food-species`, everything()) %>%
  write_tsv(here("data", "approved-food-species.tsv")) %>%
  print(n = Inf)

# Combine facilities and species into a one-row-per-approval form, with
# cardinality:n columns for facility and species.

species_nested <-
  species %>%
  group_by(AppNo) %>%
  summarise(species = list(species))

facilities_nested <-
  facilities %>%
  group_by(AppNo) %>%
  summarise(facility = list(facility))

only_facility <- anti_join(facilities_nested, species_nested, by = "AppNo")
only_species <- anti_join(species_nested, facilities_nested, by = "AppNo")
both <- inner_join(species_nested, facilities_nested, by = "AppNo")

# What facilities never appear against species?  A few.
facility_without_species <-
  full_join(facilities, species, by = "AppNo") %>%
  filter(is.na(species)) %>%
  count(facility, section, sort = TRUE)

facility_with_species <-
  inner_join(facilities, species, by = "AppNo") %>%
  count(facility, section, sort = TRUE)

anti_join(facility_without_species, facility_with_species, by = c("facility", "section"))

# What facilities only appear with species?  None.
anti_join(facility_with_species, facility_without_species, by = c("facility", "section"))

# Crosstab facilities by species to see what combinations exist
full_join(facilities, species) %>%
  distinct(facility, section, species) %>%
  mutate(dummy = "x") %>%
  spread(species, dummy) %>%
  print(n = Inf)
