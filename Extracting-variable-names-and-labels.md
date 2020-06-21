Extracting variable names and labels
================

This helper function uses regular expressions (`str_replace()`,
`str_remove()`, etc.) to standardize as many label names as possible.
This means getting rid of everything in the column label that changes
year-to-year (sometimes there’s metadata about what year it is, what
form it’s on), as well as reformatting when there are differences like
extra spaces or capitalization.

*NOTE: It’ll be worth it to do a big refactoring of this function\! I’ve
already lost information about what specific examples led me to come up
with these patterns :( I also want to make sure not to take for granted
that there’s a specific ordering that matters, and that these pattern
matches are kind of of different types.*

``` r
create_standard_helpful_name = function(unedited_name = unedited_name) {
  # everything to uppercase
  helpful_name = str_to_upper(unedited_name)
  
  # take away alphanumerics at the beginning if they fit the pattern ###C##CC
  helpful_name = str_remove(helpful_name, "^[0-9]{3}C[0-9]{2}[A-Z]{2}")
  
  # remove anything before a delineation character like : | ; ! and (R)
  #    (might be blank before special char, from removal in line above)
  helpful_name = str_remove(helpful_name, "^.*[:|\\||;|!]\\s*")
  helpful_name = str_remove(helpful_name, "[A-z0-9]+\\(R\\)")
  
  # for the specific format that goes `BYyy ALPHANUM varname` strip to just varname
  helpful_name = str_remove(helpful_name, "^BY[0-9]{2} [A-z0-9]+ ")
  
  # for the one weird case in 1997 where `= "caseid"` was left in the name
  helpful_name = str_remove(helpful_name, '\\= \\"')
  helpful_name = str_remove(helpful_name, '\\"$')
  
  # 'H'/ "H"/ and H/ are used interchangeably, so convert everything to no quotes
  helpful_name = str_replace(helpful_name, "\\'H\\'\\/", "H/")
  helpful_name = str_replace(helpful_name, '\\"H\\"\\/', "H/")
  
  # strip extra spaces from around equal signs
  helpful_name = str_replace_all(helpful_name, " = ", "=")
  
  # standardize the way that /LAST 30 DAYS and /LAST 12 MO is written wrt. spaces and cutting off DAY
  helpful_name = str_replace(helpful_name, "\\/LAST ", "\\/LAST")
  helpful_name = str_replace(helpful_name, "30 DA", "30DA")
  helpful_name = str_replace(helpful_name, "30D[A]?$", "30DAY")
  helpful_name = str_replace(helpful_name, "12 MO", "12MO")
  helpful_name = str_replace(helpful_name, "12M[O]?", "12MO")
  helpful_name = str_replace(helpful_name, "LIFETIM[E]?", "LIFETIME")
  helpful_name = str_replace(helpful_name, "SED\\/BARB\\/30DAY", "SED\\/BARB\\/LAST30DAY")
  helpful_name = str_replace(helpful_name, "SED\\/BARB\\/12MO", "SED\\/BARB\\/LAST12MO")
  helpful_name = str_replace(helpful_name, "SED\\/BARB\\/LIFE(TIME)?", "SED\\/BARB\\/LIFETIME")
  
  # standardize the way that MSA/NON-MSA = 0 etc. is written
  helpful_name = str_replace_all(helpful_name, "SMSA", "MSA")
  helpful_name = str_replace(helpful_name, "MSA(=1)?\\/NON[ |\\-]MSA", "MSA/NON-MSA")
  helpful_name = str_replace(helpful_name, "072 LARGE MSA=1/NOT=0", "LARGE MSA=1/NOT=0")
  
  # standardize the way that a bunch more random things are written
  helpful_name = str_replace(helpful_name, "R XPCTS BE? OFFCR", "R XPCTS BE OFFCR")
  helpful_name = str_replace(helpful_name, "R'S HSHLD MOTHE?R", "R'S HSHLD MOTHER")
  helpful_name = str_replace(helpful_name, "R'S HSHLD FATHE?R", "R'S HSHLD FATHER")
  helpful_name = str_replace(helpful_name, "R[ |']ATTND", "R'ATTND")
  helpful_name = str_replace(helpful_name, "R[ |']POL", "R'POL")
  helpful_name = str_replace(helpful_name, "RLGN IMP R'?S LF", "RLGN IMP R'S LF")
  helpful_name = str_replace(helpful_name, "YEAR OF ADMIN.*", "YEAR OF ADMIN")
  
  helpful_name = str_replace(helpful_name, "R'S HSHLD SPOUSE?", "R'S HSHLD SPOUSE")
  helpful_name = str_replace(helpful_name, "R'S HSHLD RELTVS?", "R'S HSHLD RELTVS")
  helpful_name = str_replace(helpful_name, "R'S HSHLD NONRLT?", "R'S HSHLD NONRLT")
  helpful_name = str_replace(helpful_name, "R'S HSHLD GRPR[T|N][T|N]?", "R'S HSHLD GRPRNT")
  helpful_name = str_replace(helpful_name, "R'S HSHLD CHLDRN?", "R'S HSHLD CHLDRN")
  helpful_name = str_replace(helpful_name, "SCH REG-4 CAT", "SCHL RGN-4 CAT")
  
  # if something starts with RS, convert to R'S.
  helpful_name = str_replace(helpful_name, "^R[S|s]", "R'S")
  
  # for two years, respondent ID was listed as ARCHIVE ID. Other years there was an extra space. Standardize
  helpful_name = str_replace(helpful_name, "ARCHIVE ID|R'S  ID-SERIAL #", "R'S ID-SERIAL #")
  
  # at some point, sampling weight switched to archive weight 
  helpful_name = str_replace(helpful_name, "ARCHIVE WEIGHT", "SAMPLING WEIGHT")
  
  # trim whitespace off of either end
  helpful_name = str_trim(helpful_name)
  
  helpful_name
}
```

This function creates a database for a given year. The database maps
from the `helpful_name` (something descriptive, like “age”) to
information about how to find that question’s data: an entry for every
year it was asked, along with which file its data was stored in, and
what the variable name was that year.

First, I identify the variable name and variable *label* from each
column. This is informaton stored in SAS that I think gets lots if we
convert from CSV, but the `haven` package preserves it for us. The label
isn’t guarunteed to be consistent year-to-year, but it’s still pretty
helpful in providing a standardized meaningful name for each question
asked. Then, I use the helper function above to standardize.

*NOTE: There are two important pieces of information I don’t store
here:* *1-how are the answers coded. This could be especially
problematic if different coding was used from year to year for the same
question.* *2-what form(s) a question was asked on. I’m using file
instead. If I need to figure it out by form, I’ll probably have to do
some parsing of the original label*

*Also: I don’t yet know what to do when a question appears in multiple
files.*

*I’ll come back to these questions/problems later\!*

``` r
create_mapping = function(path = path,
                    year = year,
                    file_number = file_number
                    ) {
  
  mapping = tibble(
    helpful_name = character(),
    unedited_name = character(),
    year = numeric(),
    variable_name = character(),
    file_number = numeric()
  )
  
  file_name = str_c(path, "y", year, "_", file_number,".sas7bdat")
  this_year_data = read_sas(data_file = file_name)
  
  for (col in 1:ncol(this_year_data)) {
    unedited_name = attr(this_year_data[[col]], "label")
    helpful_name = create_standard_helpful_name(unedited_name)
    
    mapping = add_row(mapping,
                    helpful_name = helpful_name,
                    unedited_name = unedited_name,
                    year = year,
                    variable_name = colnames(this_year_data)[col],
                    file_number = file_number
                    )
  }
  mapping
}

grade12_file1_mapping = tibble()

for (year in 1990:2018) {
  grade12_file1_mapping = rbind(grade12_file1_mapping,
                                create_mapping(path = "~/Documents/Code/MTF/MTFData/12th_grade/",
                                               year = year,
                                               file_number = 1
                                               )
  )
}

knitr::kable(head(grade12_file1_mapping))
```

| helpful\_name    | unedited\_name        | year | variable\_name | file\_number |
| :--------------- | :-------------------- | ---: | :------------- | -----------: |
| YEAR OF ADMIN    | 902 :YEAR OF ADMINST  | 1990 | V1             |            1 |
| FORM ID          | 902 :FORM ID          | 1990 | V3             |            1 |
| R’S ID-SERIAL \# | 902 :R’S ID-SERIAL \# | 1990 | V4             |            1 |
| SAMPLING WEIGHT  | 902 :SAMPLING WEIGHT  | 1990 | V5             |            1 |
| SCHL RGN-4 CAT   | 902 :SCHL RGN-4 CAT   | 1990 | V13            |            1 |
| SELF-REP/NOT=0   | 902 :SELF-REP/NOT=0   | 1990 | V16            |            1 |

I used this code iteratively to clean up the databases and create the
regular expression rules above. Arranging and grouping by `helpful_name`
and adding a `count` column to see how many rows had that exact
`helpful_name` let me check that everything that *should* share a
standardized `helpful_name` actually does.

``` r
grade12_file1_mapping = grade12_file1_mapping %>% 
  arrange(., helpful_name) %>% 
  group_by(., helpful_name) %>% 
  mutate(., count = n()) %>% 
  arrange(., desc(count))

knitr::kable(head(grade12_file1_mapping))
```

| helpful\_name | unedited\_name       | year | variable\_name | file\_number | count |
| :------------ | :------------------- | ---: | :------------- | -----------: | ----: |
| \# SIBLINGS   | 90C07AB :\# SIBLINGS | 1990 | V49            |            1 |    29 |
| \# SIBLINGS   | 912C07AB:\# SIBLINGS | 1991 | V49            |            1 |    29 |
| \# SIBLINGS   | 922C07ab:\# SIBLINGS | 1992 | V49            |            1 |    29 |
| \# SIBLINGS   | 93C07R :\# SIBLINGS  | 1993 | V49            |            1 |    29 |
| \# SIBLINGS   | 94C07R ;\# SIBLINGS  | 1994 | V49            |            1 |    29 |
| \# SIBLINGS   | 95C07R ;\# SIBLINGS  | 1995 | V49            |            1 |    29 |

# Next steps

### Assumptions I made that I’d like to verify:

  - If a question was asked on multiple forms, the answer is guarunteed
    to be the same on each form/in each file, for every participant ()

### Questions for the lab:

  - Is this duplicating information that’s easy to find elsewhere?
  - Is this helpful enough to…
      - Create and clean up years before 1990?
      - Create for 8th/10th grades?
      - Put more effort into making more robust?
      - Have someone else “validate”?
  - Are there standardzed lab scales/measures/operationalizations worth
    working in?

### Dreams for a final product:

  - Website with drop-downs, as well as something to use either in R or
    from the command line
  - Print out a warning/message that tells you what years your desired
    columns existed for (and ideally, if anything changed with the
    coding, but that might be too much)
  - Standard formats for missing vs. not asked
