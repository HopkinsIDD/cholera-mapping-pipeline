## if the taxonomy directory is taxonomy-verified and you want to pull the whole taxonomy
df <- read_taxonomy_data()

## You can manually specify the taxonomy directory
df <- read_taxonomy_data('taxonomy-working/working-entry1')

## You can specify which columns you want
df <- read_taxonomy_data('taxonomy-verified', c('who_region','ISO_A1', 'sCh', 'cCh'))

## You can filter the data by passing strings (as in dplyr)
## **Note that these only have access to fields in the description file

df <- read_taxonomy_data('taxonomy-verified', NULL, "who_region %in% c('EMR','AFR')", 'sCh > 0')
