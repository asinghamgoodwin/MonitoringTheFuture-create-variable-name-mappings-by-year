Extracting variable names and labels
================

Using the `haven` package (part of the tidyverse), read in SAS files for
12th grade by year.

``` r
get_helpful_name = function(path = path,
                            year = year,
                            file_number = file_number,
                            variable = variable) {
  file_name = str_c(path, "y", year, "_", file_number,".sas7bdat")
  this_year_data = read_sas(data_file = file_name)
  
  attr(this_year_data[[variable]], "label")
  #this_year_data
}

get_helpful_name(path = "~/Documents/Code/MTF/MTFData/12th_grade/",
                 year = 2018,
                 file_number = 1,
                 variable = 5) #can pass a variable name or a column number into this function
```

    ## [1] "ARCHIVE WEIGHT"

Create a fake tibble in the format I want my resulting tibble to be in,
to practice with it.

``` r
fake = tibble(
  helpful_name = c("age", "age", "age"),
  year = 2016:2018,
  variable_name = c("XYZ", "XXX", "ZZZ"),
  file_number = c(1, 1, 2)
)

knitr::kable(fake)
```

| helpful\_name | year | variable\_name | file\_number |
| :------------ | ---: | :------------- | -----------: |
| age           | 2016 | XYZ            |            1 |
| age           | 2017 | XXX            |            1 |
| age           | 2018 | ZZZ            |            2 |

``` r
knitr::kable(filter(fake, helpful_name == "age", year == 2017))
```

| helpful\_name | year | variable\_name | file\_number |
| :------------ | ---: | :------------- | -----------: |
| age           | 2017 | XXX            |            1 |

Create an empty tibble to fill in with real information:

``` r
grade12_file1_mapping = tibble(
  helpful_name = character(),
  year = numeric(),
  variable_name = character(),
  file_number = numeric()
)
```

A function to add mapping information into the database, year by year:

``` r
add_year = function(path = path,
                    table = table,
                    year = year,
                    file_number = file_number
                    ) {
  
  file_name = str_c(path, "y", year, "_", file_number,".sas7bdat")
  this_year_data = read_sas(data_file = file_name)
  
  for (col in 1:ncol(this_year_data)) {
    year_abbr = str_sub(as.character(year), start = 3)
    helpful_name = attr(this_year_data[[col]], "label")
    helpful_name = str_replace(helpful_name, str_c("^", year_abbr), "yy")
    helpful_name = str_replace(helpful_name, str_c("^BY", year_abbr, " "), "BYyy ")

    table = add_row(table,
                    helpful_name = helpful_name,
                    year = year,
                    variable_name = colnames(this_year_data)[col],
                    file_number = file_number
                    )
  }
  table
}

new_table = add_year(path = "~/Documents/Code/MTF/MTFData/12th_grade/",
                    table = grade12_file1_mapping,
                    year = 2018,
                    file_number = 1)

two_years = add_year(path = "~/Documents/Code/MTF/MTFData/12th_grade/",
                    table = new_table,
                    year = 2017,
                    file_number = 1)

two_years = two_years %>% 
  arrange(., helpful_name) %>% 
  group_by(., helpful_name) %>% 
  mutate(., count = n())

knitr::kable(two_years)
```

| helpful\_name                                 | year | variable\_name  | file\_number | count |
| :-------------------------------------------- | ---: | :-------------- | -----------: | ----: |
| ARCHIVE WEIGHT                                | 2018 | ARCHIVE\_WT     |            1 |     2 |
| ARCHIVE WEIGHT                                | 2017 | ARCHIVE\_WT     |            1 |     2 |
| BYyy 00030 R’S SEX                            | 2018 | V2150           |            1 |     2 |
| BYyy 00030 R’S SEX                            | 2017 | V2150           |            1 |     2 |
| BYyy 00050 R SPD \>TIM R-URB                  | 2018 | V2152           |            1 |     2 |
| BYyy 00050 R SPD \>TIM R-URB                  | 2017 | V2152           |            1 |     2 |
| BYyy 00060 R NOT MARRIED                      | 2018 | V2153           |            1 |     2 |
| BYyy 00060 R NOT MARRIED                      | 2017 | V2153           |            1 |     2 |
| BYyy 00090 R’S HSHLD FATHER                   | 2018 | V2155           |            1 |     2 |
| BYyy 00090 R’S HSHLD FATHER                   | 2017 | V2155           |            1 |     2 |
| BYyy 00100 R’S HSHLD MOTHER                   | 2018 | V2156           |            1 |     2 |
| BYyy 00100 R’S HSHLD MOTHER                   | 2017 | V2156           |            1 |     2 |
| BYyy 00110 R’S HSHLD BR/SR                    | 2018 | V2157           |            1 |     2 |
| BYyy 00110 R’S HSHLD BR/SR                    | 2017 | V2157           |            1 |     2 |
| BYyy 00310 FATHR EDUC LEVEL                   | 2018 | V2163           |            1 |     2 |
| BYyy 00310 FATHR EDUC LEVEL                   | 2017 | V2163           |            1 |     2 |
| BYyy 00320 MOTHR EDUC LEVEL                   | 2018 | V2164           |            1 |     2 |
| BYyy 00320 MOTHR EDUC LEVEL                   | 2017 | V2164           |            1 |     2 |
| BYyy 00330 MOTH PD JB R YNG                   | 2018 | V2165           |            1 |     2 |
| BYyy 00330 MOTH PD JB R YNG                   | 2017 | V2165           |            1 |     2 |
| BYyy 00340 R’S POLTL PRFNC                    | 2018 | V2166           |            1 |     2 |
| BYyy 00340 R’S POLTL PRFNC                    | 2017 | V2166           |            1 |     2 |
| BYyy 00350 R’POL BLF RADCL                    | 2018 | V2167           |            1 |     2 |
| BYyy 00350 R’POL BLF RADCL                    | 2017 | V2167           |            1 |     2 |
| BYyy 00370 R’ATTND REL SVC                    | 2018 | V2169           |            1 |     2 |
| BYyy 00370 R’ATTND REL SVC                    | 2017 | V2169           |            1 |     2 |
| BYyy 00380 RLGN IMP R’S LF                    | 2018 | V2170           |            1 |     2 |
| BYyy 00380 RLGN IMP R’S LF                    | 2017 | V2170           |            1 |     2 |
| BYyy 00390 WHEN R XPCT GRAD                   | 2018 | V2171           |            1 |     2 |
| BYyy 00390 WHEN R XPCT GRAD                   | 2017 | V2171           |            1 |     2 |
| BYyy 00400 R’S HS PROGRAM                     | 2018 | V2172           |            1 |     2 |
| BYyy 00400 R’S HS PROGRAM                     | 2017 | V2172           |            1 |     2 |
| BYyy 00410 RT SF SCH AB\>AVG                  | 2018 | V2173           |            1 |     2 |
| BYyy 00410 RT SF SCH AB\>AVG                  | 2017 | V2173           |            1 |     2 |
| BYyy 00420 RT SF INTELL\>AVG                  | 2018 | V2174           |            1 |     2 |
| BYyy 00420 RT SF INTELL\>AVG                  | 2017 | V2174           |            1 |     2 |
| BYyy 00430 \#DA/4W SC MS ILL                  | 2018 | V2175           |            1 |     2 |
| BYyy 00430 \#DA/4W SC MS ILL                  | 2017 | V2175           |            1 |     2 |
| BYyy 00440 \#DA/4W SC MS CUT                  | 2018 | V2176           |            1 |     2 |
| BYyy 00440 \#DA/4W SC MS CUT                  | 2017 | V2176           |            1 |     2 |
| BYyy 00450 \#DA/4W SC MS OTH                  | 2018 | V2177           |            1 |     2 |
| BYyy 00450 \#DA/4W SC MS OTH                  | 2017 | V2177           |            1 |     2 |
| BYyy 00460 \#DA/4W SKP CLASS                  | 2018 | V2178           |            1 |     2 |
| BYyy 00460 \#DA/4W SKP CLASS                  | 2017 | V2178           |            1 |     2 |
| BYyy 00470 R HS GRADE/D=1                     | 2018 | V2179           |            1 |     2 |
| BYyy 00470 R HS GRADE/D=1                     | 2017 | V2179           |            1 |     2 |
| BYyy 00480 R WL DO VOC/TEC                    | 2018 | V2180           |            1 |     2 |
| BYyy 00480 R WL DO VOC/TEC                    | 2017 | V2180           |            1 |     2 |
| BYyy 00490 R WL DO ARMD FC                    | 2018 | V2181           |            1 |     2 |
| BYyy 00490 R WL DO ARMD FC                    | 2017 | V2181           |            1 |     2 |
| BYyy 00500 R WL DO 2YR CLG                    | 2018 | V2182           |            1 |     2 |
| BYyy 00500 R WL DO 2YR CLG                    | 2017 | V2182           |            1 |     2 |
| BYyy 00510 R WL DO 4YR CLG                    | 2018 | V2183           |            1 |     2 |
| BYyy 00510 R WL DO 4YR CLG                    | 2017 | V2183           |            1 |     2 |
| BYyy 00520 R WL DO GRD/PRF                    | 2018 | V2184           |            1 |     2 |
| BYyy 00520 R WL DO GRD/PRF                    | 2017 | V2184           |            1 |     2 |
| BYyy 00530 R WNTDO VOC/TEC                    | 2018 | V2185           |            1 |     2 |
| BYyy 00530 R WNTDO VOC/TEC                    | 2017 | V2185           |            1 |     2 |
| BYyy 00540 R WNTDO ARMD FC                    | 2018 | V2186           |            1 |     2 |
| BYyy 00540 R WNTDO ARMD FC                    | 2017 | V2186           |            1 |     2 |
| BYyy 00550 R WNTDO 2YR CLG                    | 2018 | V2187           |            1 |     2 |
| BYyy 00550 R WNTDO 2YR CLG                    | 2017 | V2187           |            1 |     2 |
| BYyy 00560 R WNTDO 4YR CLG                    | 2018 | V2188           |            1 |     2 |
| BYyy 00560 R WNTDO 4YR CLG                    | 2017 | V2188           |            1 |     2 |
| BYyy 00570 R WNTDO GRD/PRF                    | 2018 | V2189           |            1 |     2 |
| BYyy 00570 R WNTDO GRD/PRF                    | 2017 | V2189           |            1 |     2 |
| BYyy 00580 R WNTDO NONE                       | 2018 | V2190           |            1 |     2 |
| BYyy 00580 R WNTDO NONE                       | 2017 | V2190           |            1 |     2 |
| BYyy 00590 HRS/W WRK SCHYR                    | 2018 | V2191           |            1 |     2 |
| BYyy 00590 HRS/W WRK SCHYR                    | 2017 | V2191           |            1 |     2 |
| BYyy 00600 R$/AVG WEEK JOB                    | 2018 | V2192           |            1 |     2 |
| BYyy 00600 R$/AVG WEEK JOB                    | 2017 | V2192           |            1 |     2 |
| BYyy 00610 R$/AVG WEEK OTH                    | 2018 | V2193           |            1 |     2 |
| BYyy 00610 R$/AVG WEEK OTH                    | 2017 | V2193           |            1 |     2 |
| BYyy 00620 \#X/AV WK GO OUT                   | 2018 | V2194           |            1 |     2 |
| BYyy 00620 \#X/AV WK GO OUT                   | 2017 | V2194           |            1 |     2 |
| BYyy 00630 \#X DATE 3+/WK                     | 2018 | V2195           |            1 |     2 |
| BYyy 00630 \#X DATE 3+/WK                     | 2017 | V2195           |            1 |     2 |
| BYyy 00640 DRIVE\>200 MI/WK                   | 2018 | V2196           |            1 |     2 |
| BYyy 00640 DRIVE\>200 MI/WK                   | 2017 | V2196           |            1 |     2 |
| BYyy 00650 \#X/12MO R TCKTD                   | 2018 | V2197           |            1 |     2 |
| BYyy 00650 \#X/12MO R TCKTD                   | 2017 | V2197           |            1 |     2 |
| BYyy 00660 \#TCKTS AFT DRNK                   | 2018 | V2198           |            1 |     2 |
| BYyy 00660 \#TCKTS AFT DRNK                   | 2017 | V2198           |            1 |     2 |
| BYyy 00670 \#TCKTS AFT MARJ                   | 2018 | V2199           |            1 |     2 |
| BYyy 00670 \#TCKTS AFT MARJ                   | 2017 | V2199           |            1 |     2 |
| BYyy 00680 \#TCKTS AFT OTDG                   | 2018 | V2200           |            1 |     2 |
| BYyy 00680 \#TCKTS AFT OTDG                   | 2017 | V2200           |            1 |     2 |
| BYyy 00690 \#ACCIDNTS/12 MO                   | 2018 | V2201           |            1 |     2 |
| BYyy 00690 \#ACCIDNTS/12 MO                   | 2017 | V2201           |            1 |     2 |
| BYyy 00700 \#ACDTS AFT DRNK                   | 2018 | V2202           |            1 |     2 |
| BYyy 00700 \#ACDTS AFT DRNK                   | 2017 | V2202           |            1 |     2 |
| BYyy 00710 \#ACDTS AFT MARJ                   | 2018 | V2203           |            1 |     2 |
| BYyy 00710 \#ACDTS AFT MARJ                   | 2017 | V2203           |            1 |     2 |
| BYyy 00720 \#ACDTS AFT OTDG                   | 2018 | V2204           |            1 |     2 |
| BYyy 00720 \#ACDTS AFT OTDG                   | 2017 | V2204           |            1 |     2 |
| BYyy 00730 R’S BRANCH SERV                    | 2018 | V2205           |            1 |     2 |
| BYyy 00730 R’S BRANCH SERV                    | 2017 | V2205           |            1 |     2 |
| BYyy 00740 R XPCTS B OFFCR                    | 2018 | V2206           |            1 |     2 |
| BYyy 00740 R XPCTS B OFFCR                    | 2017 | V2206           |            1 |     2 |
| BYyy 00750 R XPCTS MLTR CR                    | 2018 | V2207           |            1 |     2 |
| BYyy 00750 R XPCTS MLTR CR                    | 2017 | V2207           |            1 |     2 |
| BYyy 00760 EVR SMK CIG,REGL                   | 2018 | V2101           |            1 |     2 |
| BYyy 00760 EVR SMK CIG,REGL                   | 2017 | V2101           |            1 |     2 |
| BYyy 00780 \#CIGS SMKD/30DAY                  | 2018 | V2102           |            1 |     2 |
| BYyy 00780 \#CIGS SMKD/30DAY                  | 2017 | V2102           |            1 |     2 |
| BYyy 00790 EVER DRINK                         | 2018 | V2103           |            1 |     2 |
| BYyy 00790 EVER DRINK                         | 2017 | V2103           |            1 |     2 |
| BYyy 00810 \#X DRNK/LIFETIME                  | 2018 | V2104           |            1 |     2 |
| BYyy 00810 \#X DRNK/LIFETIME                  | 2017 | V2104           |            1 |     2 |
| BYyy 00820 \#X DRNK/LAST12MO                  | 2018 | V2105           |            1 |     2 |
| BYyy 00820 \#X DRNK/LAST12MO                  | 2017 | V2105           |            1 |     2 |
| BYyy 00830 \#X DRNK/LAST30DAY                 | 2018 | V2106           |            1 |     2 |
| BYyy 00830 \#X DRNK/LAST30DAY                 | 2017 | V2106           |            1 |     2 |
| BYyy 00840 \#X DRK ENF FL HI                  | 2018 | V2107           |            1 |     2 |
| BYyy 00840 \#X DRK ENF FL HI                  | 2017 | V2107           |            1 |     2 |
| BYyy 00850 5+DRK ROW/LST 2W                   | 2018 | V2108           |            1 |     2 |
| BYyy 00850 5+DRK ROW/LST 2W                   | 2017 | V2108           |            1 |     2 |
| BYyy 00860 \#XMJ+HS/LIFETIME                  | 2018 | V2115           |            1 |     2 |
| BYyy 00860 \#XMJ+HS/LIFETIME                  | 2017 | V2115           |            1 |     2 |
| BYyy 00870 \#XMJ+HS/LAST12MO                  | 2018 | V2116           |            1 |     2 |
| BYyy 00870 \#XMJ+HS/LAST12MO                  | 2017 | V2116           |            1 |     2 |
| BYyy 00880 \#XMJ+HS/LAST30DAY                 | 2018 | V2117           |            1 |     2 |
| BYyy 00880 \#XMJ+HS/LAST30DAY                 | 2017 | V2117           |            1 |     2 |
| BYyy 00890 \#X LSD/LIFETIME                   | 2018 | V2118           |            1 |     2 |
| BYyy 00890 \#X LSD/LIFETIME                   | 2017 | V2118           |            1 |     2 |
| BYyy 00900 \#X LSD/LAST 12MO                  | 2018 | V2119           |            1 |     2 |
| BYyy 00900 \#X LSD/LAST 12MO                  | 2017 | V2119           |            1 |     2 |
| BYyy 00910 \#X LSD/LAST 30DAY                 | 2018 | V2120           |            1 |     2 |
| BYyy 00910 \#X LSD/LAST 30DAY                 | 2017 | V2120           |            1 |     2 |
| BYyy 00920 \#X PSYD/LIFETIME                  | 2018 | V2121           |            1 |     2 |
| BYyy 00920 \#X PSYD/LIFETIME                  | 2017 | V2121           |            1 |     2 |
| BYyy 00930 \#X PSYD/LAST12MO                  | 2018 | V2122           |            1 |     2 |
| BYyy 00930 \#X PSYD/LAST12MO                  | 2017 | V2122           |            1 |     2 |
| BYyy 00940 \#X PSYD/LAST30DAY                 | 2018 | V2123           |            1 |     2 |
| BYyy 00940 \#X PSYD/LAST30DAY                 | 2017 | V2123           |            1 |     2 |
| BYyy 00950 \#X COKE/LIFETIME                  | 2018 | V2124           |            1 |     2 |
| BYyy 00950 \#X COKE/LIFETIME                  | 2017 | V2124           |            1 |     2 |
| BYyy 00960 \#X COKE/LAST12MO                  | 2018 | V2125           |            1 |     2 |
| BYyy 00960 \#X COKE/LAST12MO                  | 2017 | V2125           |            1 |     2 |
| BYyy 00970 \#X COKE/LAST30DAY                 | 2018 | V2126           |            1 |     2 |
| BYyy 00970 \#X COKE/LAST30DAY                 | 2017 | V2126           |            1 |     2 |
| BYyy 00980 \#X AMPH/LIFETIME                  | 2018 | V2127           |            1 |     2 |
| BYyy 00980 \#X AMPH/LIFETIME                  | 2017 | V2127           |            1 |     2 |
| BYyy 00990 \#X AMPH/LAST12MO                  | 2018 | V2128           |            1 |     2 |
| BYyy 00990 \#X AMPH/LAST12MO                  | 2017 | V2128           |            1 |     2 |
| BYyy 01000 \#X AMPH/LAST30DAY                 | 2018 | V2129           |            1 |     2 |
| BYyy 01000 \#X AMPH/LAST30DAY                 | 2017 | V2129           |            1 |     2 |
| BYyy 01040 \#X SED/BARB/LIFETIME              | 2017 | V2133           |            1 |     1 |
| BYyy 01042 \#X SED/BARB/LIFETIME              | 2018 | V2133           |            1 |     1 |
| BYyy 01050 \#X SED/BARB/LAST12MO              | 2017 | V2134           |            1 |     1 |
| BYyy 01052 \#X SED/BARB/LAST12MO              | 2018 | V2134           |            1 |     1 |
| BYyy 01060 \#X SED/BARB/LAST30DAY             | 2017 | V2135           |            1 |     1 |
| BYyy 01062 \#X SED/BARB/LAST30DAY             | 2018 | V2135           |            1 |     1 |
| BYyy 01070 \#X TRQL/LIFETIME                  | 2018 | V2136           |            1 |     2 |
| BYyy 01070 \#X TRQL/LIFETIME                  | 2017 | V2136           |            1 |     2 |
| BYyy 01080 \#X TRQL/LAST12MO                  | 2018 | V2137           |            1 |     2 |
| BYyy 01080 \#X TRQL/LAST12MO                  | 2017 | V2137           |            1 |     2 |
| BYyy 01090 \#X TRQL/LAST30DAY                 | 2018 | V2138           |            1 |     2 |
| BYyy 01090 \#X TRQL/LAST30DAY                 | 2017 | V2138           |            1 |     2 |
| BYyy 01100 \#X H/LIFETIME                     | 2018 | V2139           |            1 |     2 |
| BYyy 01100 \#X H/LIFETIME                     | 2017 | V2139           |            1 |     2 |
| BYyy 01110 \#X H/LAST 12MO                    | 2018 | V2140           |            1 |     2 |
| BYyy 01110 \#X H/LAST 12MO                    | 2017 | V2140           |            1 |     2 |
| BYyy 01120 \#X H/LAST 30DAY                   | 2018 | V2141           |            1 |     2 |
| BYyy 01120 \#X H/LAST 30DAY                   | 2017 | V2141           |            1 |     2 |
| BYyy 01130 \#X NARC/LIFETIME                  | 2018 | V2142           |            1 |     2 |
| BYyy 01130 \#X NARC/LIFETIME                  | 2017 | V2142           |            1 |     2 |
| BYyy 01140 \#X NARC/LAST12MO                  | 2018 | V2143           |            1 |     2 |
| BYyy 01140 \#X NARC/LAST12MO                  | 2017 | V2143           |            1 |     2 |
| BYyy 01150 \#X NARC/LAST30DAY                 | 2018 | V2144           |            1 |     2 |
| BYyy 01150 \#X NARC/LAST30DAY                 | 2017 | V2144           |            1 |     2 |
| BYyy 01160 \#X INHL/LIFETIME                  | 2018 | V2145           |            1 |     2 |
| BYyy 01160 \#X INHL/LIFETIME                  | 2017 | V2145           |            1 |     2 |
| BYyy 01170 \#X INHL/LAST12MO                  | 2018 | V2146           |            1 |     2 |
| BYyy 01170 \#X INHL/LAST12MO                  | 2017 | V2146           |            1 |     2 |
| BYyy 01180 \#X INHL/LAST30DAY                 | 2018 | V2147           |            1 |     2 |
| BYyy 01180 \#X INHL/LAST30DAY                 | 2017 | V2147           |            1 |     2 |
| BYyy 22260 \#X CRACK/LIFETIME                 | 2018 | V2459           |            1 |     2 |
| BYyy 22260 \#X CRACK/LIFETIME                 | 2017 | V2459           |            1 |     2 |
| BYyy 22270 \#X CRACK/LAST12MO                 | 2018 | V2460           |            1 |     2 |
| BYyy 22270 \#X CRACK/LAST12MO                 | 2017 | V2460           |            1 |     2 |
| BYyy 22280 \#X CRACK/LAST30DAY                | 2018 | V2461           |            1 |     2 |
| BYyy 22280 \#X CRACK/LAST30DAY                | 2017 | V2461           |            1 |     2 |
| BYyy 22320 \#XOTH COKE/LIFE                   | 2018 | V2042           |            1 |     2 |
| BYyy 22320 \#XOTH COKE/LIFE                   | 2017 | V2042           |            1 |     2 |
| BYyy 22330 \#XOTH COKE/12MO                   | 2018 | V2043           |            1 |     2 |
| BYyy 22330 \#XOTH COKE/12MO                   | 2017 | V2043           |            1 |     2 |
| BYyy 22340 \#XOTH COKE/30DAY                  | 2018 | V2044           |            1 |     2 |
| BYyy 22340 \#XOTH COKE/30DAY                  | 2017 | V2044           |            1 |     2 |
| BYyy 22660 \#X MDMA/LIFETIME                  | 2018 | V2032           |            1 |     2 |
| BYyy 22660 \#X MDMA/LIFETIME                  | 2017 | V2032           |            1 |     2 |
| BYyy 22670 \#X MDMA/LAST12MO                  | 2018 | V2033           |            1 |     2 |
| BYyy 22670 \#X MDMA/LAST12MO                  | 2017 | V2033           |            1 |     2 |
| BYyy 22680 \#X MDMA/LAST30DAY                 | 2018 | V2034           |            1 |     2 |
| BYyy 22680 \#X MDMA/LAST30DAY                 | 2017 | V2034           |            1 |     2 |
| BYyy 22690 \#X STRD/LIFETIME                  | 2018 | V2493           |            1 |     2 |
| BYyy 22690 \#X STRD/LIFETIME                  | 2017 | V2493           |            1 |     2 |
| BYyy 22700 \#X STRD/LAST12MO                  | 2018 | V2494           |            1 |     2 |
| BYyy 22700 \#X STRD/LAST12MO                  | 2017 | V2494           |            1 |     2 |
| BYyy 22710 \#X STRD/LAST30DAY                 | 2018 | V2495           |            1 |     2 |
| BYyy 22710 \#X STRD/LAST30DAY                 | 2017 | V2495           |            1 |     2 |
| BYyy 25020 \#XDRUNK/LIFETIME                  | 2018 | V2020           |            1 |     2 |
| BYyy 25020 \#XDRUNK/LIFETIME                  | 2017 | V2020           |            1 |     2 |
| BYyy 25030 \#XDRUNK/LAST12MO                  | 2018 | V2021           |            1 |     2 |
| BYyy 25030 \#XDRUNK/LAST12MO                  | 2017 | V2021           |            1 |     2 |
| BYyy 25040 \#XDRUNK/LAST30DAY                 | 2018 | V2022           |            1 |     2 |
| BYyy 25040 \#XDRUNK/LAST30DAY                 | 2017 | V2022           |            1 |     2 |
| BYyy 29630 \#X H LIF USE NDL                  | 2018 | V2510           |            1 |     2 |
| BYyy 29630 \#X H LIF USE NDL                  | 2017 | V2510           |            1 |     2 |
| BYyy 29640 \#X H 12MO USE NDL                 | 2018 | V2511           |            1 |     2 |
| BYyy 29640 \#X H 12MO USE NDL                 | 2017 | V2511           |            1 |     2 |
| BYyy 29650 \#X H 30D USE NDL                  | 2018 | V2512           |            1 |     2 |
| BYyy 29650 \#X H 30D USE NDL                  | 2017 | V2512           |            1 |     2 |
| BYyy 29660 \#X H LIF W/O NDL                  | 2018 | V2513           |            1 |     2 |
| BYyy 29660 \#X H LIF W/O NDL                  | 2017 | V2513           |            1 |     2 |
| BYyy 29670 \#X H 12MO W/O NDL                 | 2018 | V2514           |            1 |     2 |
| BYyy 29670 \#X H 12MO W/O NDL                 | 2017 | V2514           |            1 |     2 |
| BYyy 29680 \#X H 30D W/O NDL                  | 2018 | V2515           |            1 |     2 |
| BYyy 29680 \#X H 30D W/O NDL                  | 2017 | V2515           |            1 |     2 |
| BYyy 30800 \#X METHAMPH/LIFE                  | 2018 | V2029           |            1 |     2 |
| BYyy 30800 \#X METHAMPH/LIFE                  | 2017 | V2029           |            1 |     2 |
| BYyy 30810 \#X METHAMPH/12MO                  | 2018 | V2030           |            1 |     2 |
| BYyy 30810 \#X METHAMPH/12MO                  | 2017 | V2030           |            1 |     2 |
| BYyy 30820 \#X METHAMPH/30DAY                 | 2018 | V2031           |            1 |     2 |
| BYyy 30820 \#X METHAMPH/30DAY                 | 2017 | V2031           |            1 |     2 |
| BYyy 31060 \#X KETAMINE/12MO                  | 2018 | V2912           |            1 |     2 |
| BYyy 31060 \#X KETAMINE/12MO                  | 2017 | V2912           |            1 |     2 |
| BYyy 31160 \#X ANDRO/12MO                     | 2018 | V2918           |            1 |     2 |
| BYyy 31160 \#X ANDRO/12MO                     | 2017 | V2918           |            1 |     2 |
| BYyy 31170 \#X CREATINE/12MO                  | 2018 | V2919           |            1 |     2 |
| BYyy 31170 \#X CREATINE/12MO                  | 2017 | V2919           |            1 |     2 |
| BYyy 31180 \#X RITALIN/12MO                   | 2018 | V2909           |            1 |     2 |
| BYyy 31180 \#X RITALIN/12MO                   | 2017 | V2909           |            1 |     2 |
| BYyy 31310 \#X OXYCONTN/12MO                  | 2018 | V2907           |            1 |     2 |
| BYyy 31310 \#X OXYCONTN/12MO                  | 2017 | V2907           |            1 |     2 |
| BYyy 31320 \#X VICODIN/12MO                   | 2018 | V2908           |            1 |     2 |
| BYyy 31320 \#X VICODIN/12MO                   | 2017 | V2908           |            1 |     2 |
| BYyy 31670 \#X COUGHMED/12MO                  | 2018 | V2920           |            1 |     2 |
| BYyy 31670 \#X COUGHMED/12MO                  | 2017 | V2920           |            1 |     2 |
| BYyy 32450 \#X ADDERALL/12MO                  | 2018 | V2307           |            1 |     2 |
| BYyy 32450 \#X ADDERALL/12MO                  | 2017 | V2307           |            1 |     2 |
| BYyy 32500 \#X SALVIA/12MO                    | 2018 | V2305           |            1 |     2 |
| BYyy 32500 \#X SALVIA/12MO                    | 2017 | V2305           |            1 |     2 |
| BYyy 32690 \#X ALC BVRG W-CAFF/12MO           | 2018 | V2009           |            1 |     2 |
| BYyy 32690 \#X ALC BVRG W-CAFF/12MO           | 2017 | V2009           |            1 |     2 |
| BYyy 32700 \#X SYNTHETIC MJ/12MO              | 2018 | V2003           |            1 |     2 |
| BYyy 32700 \#X SYNTHETIC MJ/12MO              | 2017 | V2003           |            1 |     2 |
| BYyy 32730 \#X BATH SALTS/12MO                | 2018 | V2922           |            1 |     2 |
| BYyy 32730 \#X BATH SALTS/12MO                | 2017 | V2922           |            1 |     2 |
| BYyy 33710 \#X E-CIG/30DAY                    | 2018 | V2546           |            1 |     2 |
| BYyy 33710 \#X E-CIG/30DAY                    | 2017 | V2546           |            1 |     2 |
| BYyy 33720 \#X LRG CIGAR/30DAY                | 2018 | V2547           |            1 |     2 |
| BYyy 33720 \#X LRG CIGAR/30DAY                | 2017 | V2547           |            1 |     2 |
| BYyy 33730 \#X FLVD SML CIGAR/30DAY           | 2018 | V2548           |            1 |     2 |
| BYyy 33730 \#X FLVD SML CIGAR/30DAY           | 2017 | V2548           |            1 |     2 |
| BYyy 33740 \#X REG SML CIGAR/30DAY            | 2018 | V2549           |            1 |     2 |
| BYyy 33740 \#X REG SML CIGAR/30DAY            | 2017 | V2549           |            1 |     2 |
| BYyy 33750 \#X SYNTH MJ/30DAY                 | 2017 | V2550           |            1 |     1 |
| BYyy 33980 \#X HASH/OIL/12MO                  | 2018 | V2927           |            1 |     2 |
| BYyy 33980 \#X HASH/OIL/12MO                  | 2017 | V2927           |            1 |     2 |
| BYyy 34040 \#X PWD ALCOHOL/12MO               | 2018 | V2929           |            1 |     2 |
| BYyy 34040 \#X PWD ALCOHOL/12MO               | 2017 | V2929           |            1 |     2 |
| BYyy 34050 \#X TOB HOOKAH/30DAY               | 2018 | V2564           |            1 |     2 |
| BYyy 34050 \#X TOB HOOKAH/30DAY               | 2017 | V2564           |            1 |     2 |
| BYyy 34230 EVER VAPE                          | 2018 | V2566           |            1 |     2 |
| BYyy 34230 EVER VAPE                          | 2017 | V2566           |            1 |     2 |
| BYyy 34240 \#X VAPE NIC/LIFETIME              | 2018 | V2567           |            1 |     2 |
| BYyy 34240 \#X VAPE NIC/LIFETIME              | 2017 | V2567           |            1 |     2 |
| BYyy 34250 \#X VAPE NIC/LAST12MO              | 2018 | V2568           |            1 |     2 |
| BYyy 34250 \#X VAPE NIC/LAST12MO              | 2017 | V2568           |            1 |     2 |
| BYyy 34260 \#X VAPE NIC/LAST30DAY             | 2018 | V2569           |            1 |     2 |
| BYyy 34260 \#X VAPE NIC/LAST30DAY             | 2017 | V2569           |            1 |     2 |
| BYyy 34270 \#X VAPE MJ/LIFETIME               | 2018 | V2570           |            1 |     2 |
| BYyy 34270 \#X VAPE MJ/LIFETIME               | 2017 | V2570           |            1 |     2 |
| BYyy 34280 \#X VAPE MJ/LAST12MO               | 2018 | V2571           |            1 |     2 |
| BYyy 34280 \#X VAPE MJ/LAST12MO               | 2017 | V2571           |            1 |     2 |
| BYyy 34290 \#X VAPE MJ/LAST30DAY              | 2018 | V2572           |            1 |     2 |
| BYyy 34290 \#X VAPE MJ/LAST30DAY              | 2017 | V2572           |            1 |     2 |
| BYyy 34300 \#X VAPE FLAV/LIFETIME             | 2018 | V2573           |            1 |     2 |
| BYyy 34300 \#X VAPE FLAV/LIFETIME             | 2017 | V2573           |            1 |     2 |
| BYyy 34310 \#X VAPE FLAV/LAST12MO             | 2018 | V2574           |            1 |     2 |
| BYyy 34310 \#X VAPE FLAV/LAST12MO             | 2017 | V2574           |            1 |     2 |
| BYyy 34320 \#X VAPE FLAV/LAST30DAY            | 2018 | V2575           |            1 |     2 |
| BYyy 34320 \#X VAPE FLAV/LAST30DAY            | 2017 | V2575           |            1 |     2 |
| BYyy 34370 \# DAYS VAPE NIC/30DAY             | 2018 | V2930           |            1 |     2 |
| BYyy 34370 \# DAYS VAPE NIC/30DAY             | 2017 | V2930           |            1 |     2 |
| BYyy 34390 \# DAYS SMKLESS/30DAY              | 2018 | V2576           |            1 |     2 |
| BYyy 34390 \# DAYS SMKLESS/30DAY              | 2017 | V2576           |            1 |     2 |
| BYyy 34810 R’S HSHLD OTHER                    | 2018 | V2577           |            1 |     1 |
| BYyy 80001 YEAR OF ADMIN (4-DIGITS)           | 2018 | V1              |            1 |     2 |
| BYyy 80001 YEAR OF ADMIN (4-DIGITS)           | 2017 | V1              |            1 |     2 |
| BYyy 80003 FORM ID                            | 2018 | V3              |            1 |     2 |
| BYyy 80003 FORM ID                            | 2017 | V3              |            1 |     2 |
| BYyy 80013 SCHL RGN-4 CAT                     | 2018 | V13             |            1 |     2 |
| BYyy 80013 SCHL RGN-4 CAT                     | 2017 | V13             |            1 |     2 |
| BYyy 80017 SMSA/NON-SMSA=0                    | 2018 | V17             |            1 |     2 |
| BYyy 80017 SMSA/NON-SMSA=0                    | 2017 | V17             |            1 |     2 |
| CASE IDENTIFICATION NUMBER                    | 2018 | CASEID          |            1 |     2 |
| CASE IDENTIFICATION NUMBER                    | 2017 | CASEID          |            1 |     2 |
| LARGE MSA = 1/NOT = 0                         | 2018 | V16             |            1 |     2 |
| LARGE MSA = 1/NOT = 0                         | 2017 | V16             |            1 |     2 |
| R’S ID-SERIAL \#                              | 2018 | RESPONDENT\_ID  |            1 |     2 |
| R’S ID-SERIAL \#                              | 2017 | RESPONDENT\_ID  |            1 |     2 |
| yy1C01(R):AGE \<\>18 DICHOTOMY                | 2018 | RESPONDENT\_AGE |            1 |     2 |
| yy1C01(R):AGE \<\>18 DICHOTOMY                | 2017 | RESPONDENT\_AGE |            1 |     2 |
| yy1C04(R):Rs RACE B/W/H                       | 2018 | V2151           |            1 |     2 |
| yy1C04(R):Rs RACE B/W/H                       | 2017 | V2151           |            1 |     2 |
| yy1C07(R):\# SIBLINGS                         | 2018 | V49             |            1 |     2 |
| yy1C07(R):\# SIBLINGS                         | 2017 | V49             |            1 |     2 |
| yy2B18A:\#X INHL/LIFETIME (dicotomous recode) | 2018 | V2145D          |            1 |     2 |
| yy2B18A:\#X INHL/LIFETIME (dicotomous recode) | 2017 | V2145D          |            1 |     2 |
| yy2B18B:\#X INHL/LAST12MO (dicotomous recode) | 2018 | V2146D          |            1 |     2 |
| yy2B18B:\#X INHL/LAST12MO (dicotomous recode) | 2017 | V2146D          |            1 |     2 |
| yy2B18C:\#X INHL/LAST30DA (dicotomous recode) | 2018 | V2147D          |            1 |     2 |
| yy2B18C:\#X INHL/LAST30DA (dicotomous recode) | 2017 | V2147D          |            1 |     2 |
| yy2R:\#X SED/BARB/12MO (dicotomous recode)    | 2018 | V2134D          |            1 |     2 |
| yy2R:\#X SED/BARB/12MO (dicotomous recode)    | 2017 | V2134D          |            1 |     2 |
| yy2R:\#X SED/BARB/30DA (dicotomous recode)    | 2018 | V2135D          |            1 |     2 |
| yy2R:\#X SED/BARB/30DA (dicotomous recode)    | 2017 | V2135D          |            1 |     2 |
| yy2R:\#X SED/BARB/LIFE (dicotomous recode)    | 2018 | V2133D          |            1 |     2 |
| yy2R:\#X SED/BARB/LIFE (dicotomous recode)    | 2017 | V2133D          |            1 |     2 |
| yy6B01 :EVR SMK CIG,REGL (dicotomous recode)  | 2018 | V2101D          |            1 |     2 |
| yy6B01 :EVR SMK CIG,REGL (dicotomous recode)  | 2017 | V2101D          |            1 |     2 |
| yy6B02 :\#CIGS SMKD/30DAY (dicotomous recode) | 2018 | V2102D          |            1 |     2 |
| yy6B02 :\#CIGS SMKD/30DAY (dicotomous recode) | 2017 | V2102D          |            1 |     2 |
| yy6B22A:\#X ALC/LIF SIPS (dicotomous recode)  | 2018 | V2104D          |            1 |     2 |
| yy6B22A:\#X ALC/LIF SIPS (dicotomous recode)  | 2017 | V2104D          |            1 |     2 |
| yy6B22B:\#X ALC/ANN SIPS (dicotomous recode)  | 2018 | V2105D          |            1 |     2 |
| yy6B22B:\#X ALC/ANN SIPS (dicotomous recode)  | 2017 | V2105D          |            1 |     2 |
| yy6B22C:\#X ALC/30D SIPS (dicotomous recode)  | 2018 | V2106D          |            1 |     2 |
| yy6B22C:\#X ALC/30D SIPS (dicotomous recode)  | 2017 | V2106D          |            1 |     2 |
| yy6B25A:\#XMJ+HS/LIFETIME (dicotomous recode) | 2018 | V2115D          |            1 |     2 |
| yy6B25A:\#XMJ+HS/LIFETIME (dicotomous recode) | 2017 | V2115D          |            1 |     2 |
| yy6B25B:\#XMJ+HS/LAST12MO (dicotomous recode) | 2018 | V2116D          |            1 |     2 |
| yy6B25B:\#XMJ+HS/LAST12MO (dicotomous recode) | 2017 | V2116D          |            1 |     2 |
| yy6B25C:\#XMJ+HS/LAST30DA (dicotomous recode) | 2018 | V2117D          |            1 |     2 |
| yy6B25C:\#XMJ+HS/LAST30DA (dicotomous recode) | 2017 | V2117D          |            1 |     2 |
| yy6B26A:\#X LSD/LIFETIME (dicotomous recode)  | 2018 | V2118D          |            1 |     2 |
| yy6B26A:\#X LSD/LIFETIME (dicotomous recode)  | 2017 | V2118D          |            1 |     2 |
| yy6B26B:\#X LSD/LAST 12MO (dicotomous recode) | 2018 | V2119D          |            1 |     2 |
| yy6B26B:\#X LSD/LAST 12MO (dicotomous recode) | 2017 | V2119D          |            1 |     2 |
| yy6B26C:\#X LSD/LAST 30DA (dicotomous recode) | 2018 | V2120D          |            1 |     2 |
| yy6B26C:\#X LSD/LAST 30DA (dicotomous recode) | 2017 | V2120D          |            1 |     2 |
| yy6B27A:\#X PSYD/LIFETIME (dicotomous recode) | 2018 | V2121D          |            1 |     2 |
| yy6B27A:\#X PSYD/LIFETIME (dicotomous recode) | 2017 | V2121D          |            1 |     2 |
| yy6B27B:\#X PSYD/LAST12MO (dicotomous recode) | 2018 | V2122D          |            1 |     2 |
| yy6B27B:\#X PSYD/LAST12MO (dicotomous recode) | 2017 | V2122D          |            1 |     2 |
| yy6B27C:\#X PSYD/LAST30DA (dicotomous recode) | 2018 | V2123D          |            1 |     2 |
| yy6B27C:\#X PSYD/LAST30DA (dicotomous recode) | 2017 | V2123D          |            1 |     2 |
| yy6B28A:\#X AMPH/LIFETIME (dicotomous recode) | 2018 | V2127D          |            1 |     2 |
| yy6B28A:\#X AMPH/LIFETIME (dicotomous recode) | 2017 | V2127D          |            1 |     2 |
| yy6B28B:\#X AMPH/LAST12MO (dicotomous recode) | 2018 | V2128D          |            1 |     2 |
| yy6B28B:\#X AMPH/LAST12MO (dicotomous recode) | 2017 | V2128D          |            1 |     2 |
| yy6B28C:\#X AMPH/LAST30DA (dicotomous recode) | 2018 | V2129D          |            1 |     2 |
| yy6B28C:\#X AMPH/LAST30DA (dicotomous recode) | 2017 | V2129D          |            1 |     2 |
| yy6B32A:\#X TRQL/LIFETIME (dicotomous recode) | 2018 | V2136D          |            1 |     2 |
| yy6B32A:\#X TRQL/LIFETIME (dicotomous recode) | 2017 | V2136D          |            1 |     2 |
| yy6B32B:\#X TRQL/LAST12MO (dicotomous recode) | 2018 | V2137D          |            1 |     2 |
| yy6B32B:\#X TRQL/LAST12MO (dicotomous recode) | 2017 | V2137D          |            1 |     2 |
| yy6B32C:\#X TRQL/LAST30DA (dicotomous recode) | 2018 | V2138D          |            1 |     2 |
| yy6B32C:\#X TRQL/LAST30DA (dicotomous recode) | 2017 | V2138D          |            1 |     2 |
| yy6B35A:\#X NARC/LIFETIME (dicotomous recode) | 2018 | V2142D          |            1 |     2 |
| yy6B35A:\#X NARC/LIFETIME (dicotomous recode) | 2017 | V2142D          |            1 |     2 |
| yy6B35B:\#X NARC/LAST12MO (dicotomous recode) | 2018 | V2143D          |            1 |     2 |
| yy6B35B:\#X NARC/LAST12MO (dicotomous recode) | 2017 | V2143D          |            1 |     2 |
| yy6B35C:\#X NARC/LAST30DA (dicotomous recode) | 2018 | V2144D          |            1 |     2 |
| yy6B35C:\#X NARC/LAST30DA (dicotomous recode) | 2017 | V2144D          |            1 |     2 |
