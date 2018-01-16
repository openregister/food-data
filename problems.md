# Problems

A list of problems noticed in the source data.

*  An AppNo can have a Slaughterhouse in two different sections, which makes
    it difficult to tell which species applies to which sections of
    slaughterhouse.
*  It isn't possible to map one species to one category (and therefore one
    facility), because 'ratite' and 'Farmed Land Mammals ...' each occur in more
    than one category.
*  An AppNo (approval) is granted to either: the combination of a section and a
    facility, or the combination of a section, a slaughterhouse, and a species.
    Unfortunately it isn't possible to tell which species applies to which
    slaughterhouse in which section, where there is more than one, at least for
    species 'ratite' and 'Farmed Land Mammals' (see above) so AppNos will have
    to be recorded against both sections A-I and A-II.
*  To model this, Paul created a table of combinations of section and facility
    (activity), to which I would add species.
    [here](https://github.com/openregister-attic/food-data/blob/master/approvals/data/food-premises-category.tsv),
    but another option is to add the AppNo to that table, create a meaningless
    key, and make that the main table of approvals.  That option (table of
    approvals, meaningless key) requires that we find out which species maps to
    which facility.
*  Some slaughterhouses don't have any species.

```r
slaughterhouse_without_species
# # A tibble: 3 x 5
#   AppNo part  section facility Species
#   <chr> <chr> <chr>   <chr>    <chr>
# 1 2660  A     A-I     SH       <NA>
# 2 2669  A     A-II    SH       <NA>
# 3 5388  A     A-II    SH       <NA>
```
