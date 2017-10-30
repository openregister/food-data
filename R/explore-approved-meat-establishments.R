library(tidyverse)
library(here)

establishments <- read_csv(here("lists", "approved-meat-establishment.csv"))
metadata <- read_csv(here("lists", "approved-meat-establishment-metadata.csv"))

map(select(establishments, -1:-4, -AuditDate), unique)

# There's multiple activities per approval (space delimiter), some
# TradingNames have more than one approval, even more than one per town.
count(establishments, ApprovalNumber, sort = TRUE)
count(establishments, TradingName, sort = TRUE)
count(establishments, TradingName, Town, sort = TRUE)
count(establishments, Activity, sort = TRUE)

# James Burden has two approvals in London for CS.  Presumably different sites,
# though not obvious from the internet.
filter(establishments, TradingName == "James Burden Ltd") %>%
  select(ApprovalNumber, Activity, Town, County, Country, AuditDate, Outcome)

establishments %>%
  select(ApprovalNumber, Activity, 7:26) %>%
  gather("key", "value", 3:22) %>%
  filter(value != "No") %>%
  select(-value) %>%
  group_by(ApprovalNumber, Activity) %>%
  mutate(key = list(key))

# Scotland isn't included
count(establishments, Country)

# Could they use company number?
# Could they use local-authority instead of town and county?
# Where are non-approved meat establishments?
# Does the approval number expire on the next due date?
# Is the audit date the same as the approval date?
# There are mixed cases in the Outcome column

# Potential registers:
# * approved-meat-establishments
# * meat-establishment-activity
# * meat-establishment-operation
# * meat-establishment-assessment
# * meat-establishment-outcome
# * meat-establishment-criterion
# Activity is cardinality='n'

# * approved-meat-establishments
#   * approved-meat-establishment
#   * company
#   * local-authority
#   * meat-establishment-activities
#   * meat-establishment-operations
#   * meat-establishment-outcome
#   * meat-establishment-audit-frequency
#   * meat-establishment-audit-date
#   * meat-establishment-next-audit-due-date
#   * start-date
#   * end-date

# * meat-establishment-activity
#   * meat-establishment-activity
#   * start-date
#   * end-date

# * meat-establishment-operation
#   * meat-establishment-operation
#   * start-date
#   * end-date

# * meat-establishment-assessment
#   * meat-establishment-assessment (the Q number? No, might change)
#   * meat-establishment-criterion
#   * text
#   * start-date
#   * end-date

# * meat-establishment-criterion
#   * meat-establishment-criterion
#   * name
#   * start-date
#   * end-date

# * meat-establishment-outcome
#   * meat-establishment-outcome
#   * name
#   * start-date
#   * end-date

`meat-establishment-activity` <-
  tibble(`meat-establishment-activity` = 1:3,
         name = c("Cutting Plant", "Cold Store", "Slaughterhouse"),
         `start-date` = NA,
         `end-date` = NA) %>%
  write_tsv(here("data", "meat-establishment-activity.tsv"), na = "") %>%
  mutate(Activity = c("CP", "CS", "SH")) # for joining

`meat-establishment-outcome` <-
  establishments$Outcome %>%
  tolower() %>% # resolves inconsistencies
  unique() %>%
  data_frame(name = .) %>%
  arrange(name) %>%
  mutate(`meat-establishment-outcome` = row_number(),
         `start-date` = NA,
         `end-date` = NA) %>%
  select(`meat-establishment-outcome`, everything()) %>%
  write_tsv(here("data", "meat-establishment-outcome.tsv"), na = "")

`meat-establishment-operation` <-
  tribble(~acronym, ~name,
  "RSL","Red Meat Slaughterhouse",
  "PSL","Poultry Meat Slaughterhouse",
  "GHE","Game Handling Establishment",
  "RCP","Red Meat Cutting Plant",
  "PCP","Poultry Meat Cutting Plant",
  "MIN","Minced Meat Preparations Establishment",
  "PRO","Processing Plant",
  "MSM","Mechanically Separated Meat",
  "CLS","Cold Store",
  "WMA","Wholesale Market",
  "Sheep","Approved to remove spinal cord in sheep and goats over 12 months of age or with a permanent incisor erupted is specified risk material (SRM)",
  "BSETesting","Testing required in BSE susceptible animals",
  "VC","Cutting Plants approved to remove vertebral column in cattle older than 30 months",
  "WGP","Wild Game Cutting",
  "MPrep","Meat Preparations Establishment",
  "RTE","Ready To Eat Meats",
  "Fish","Fishery Products",
  "Wrap","Re-Wrapping Establishment",
  "Dairy","Dairy Products",
  "Egg","Egg Products") %>%
  mutate(`meat-establishment-operation` = row_number() + 10,
         `start-date` = NA,
         `end-date` = NA) %>%
  select(`meat-establishment-operation`, name, `start-date`, `end-date`, acronym)
write_tsv(select(`meat-establishment-operation`, -acronym),
          here("data", "meat-establishment-operation.tsv"),
          na = "")

establishments %>%
  select(`approved-meat-establishment` = ApprovalNumber,
         `company` = TradingName,
         `local-authority` = County,
         Activity,
         RSL:Egg,
         `meat-establishment-audit-date` = AuditDate,
         `meat-establishment-audit-frequency` = AuditFrequency,
         Outcome,
         `meat-establishment-next-audit-due-date` = NextAuditDueDate) %>%
  mutate(Activity = map(Activity, ~ unlist(str_split(.x, " "))),
         Outcome = tolower(Outcome),
         `meat-establishment-audit-date` = lubridate::dmy(`meat-establishment-audit-date`),
         `meat-establishment-next-audit-due-date` = lubridate::dmy(`meat-establishment-next-audit-due-date`)) %>%
  unnest(Activity) %>%
  gather("operation", "yesno", RSL:Egg) %>%
  filter(yesno == "Yes") %>%
  select(-yesno) %>%
  left_join(select(`meat-establishment-outcome`, `meat-establishment-outcome`, name),
            by = c("Outcome" = "name")) %>%
  select(-Outcome) %>%
  left_join(select(`meat-establishment-activity`, `meat-establishment-activity`, Activity),
            by = c("Activity")) %>%
  select(-Activity) %>%
  left_join(select(`meat-establishment-operation`, `meat-establishment-operation`, acronym),
            by = c("operation" = "acronym")) %>%
  select(-operation) %>%
  write_tsv(here("data", "approved-meat-establishment.tsv"), na = "")
