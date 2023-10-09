Project 2
================
Yvette Callender
2023-10-09

- [Packages used in this project](#packages-used-in-this-project)
- [Introduction](#introduction)
- [Accessing Data](#accessing-data)
  - [Downloading data](#downloading-data)
  - [Function 1 for HPV Vaccination Coverage: Regional level differences
    between genders with user modification of
    year](#function-1-for-hpv-vaccination-coverage-regional-level-differences-between-genders-with-user-modification-of-year)
  - [Function 2 for HPV Vaccination Coverage: State level differences
    between genders with user modification of
    year](#function-2-for-hpv-vaccination-coverage-state-level-differences-between-genders-with-user-modification-of-year)
  - [Function 3 for Vaccination Coverage for Vaccines other than HPV:
    Regional Level
    Differences](#function-3-for-vaccination-coverage-for-vaccines-other-than-hpv-regional-level-differences)
  - [Function 4 for Vaccination Coverage for Vaccines other than HPV:
    State Level
    Differences](#function-4-for-vaccination-coverage-for-vaccines-other-than-hpv-state-level-differences)
  - [Function 5 for Vaccination Coverage 2018-2022 for Vaccines other
    than HPV or Varicella: State Level
    Differences](#function-5-for-vaccination-coverage-2018-2022-for-vaccines-other-than-hpv-or-varicella-state-level-differences)
  - [Summary of Functions](#summary-of-functions)
- [Exploratory Data Analysis](#exploratory-data-analysis)
  - [Comparing Vaccine Coverage for Tdap and Meningococcal
    Conjugate](#comparing-vaccine-coverage-for-tdap-and-meningococcal-conjugate)
  - [Vaccine Coverage for Meningococcal Conjugate by State
    Requirements](#vaccine-coverage-for-meningococcal-conjugate-by-state-requirements)
  - [Vaccine Coverage for Meningococcal Conjugate in States without
    Mandates](#vaccine-coverage-for-meningococcal-conjugate-in-states-without-mandates)
  - [Vaccine Coverage for Vaccines by
    Region](#vaccine-coverage-for-vaccines-by-region)
  - [Coverage for HPV Vaccines by State and
    Insurance](#coverage-for-hpv-vaccines-by-state-and-insurance)
  - [Summary of Data Analysis](#summary-of-data-analysis)
- [Summary for Project](#summary-for-project)

# Packages used in this project

``` r
library(tidyverse)
library(jsonlite)
library(ggplot2)
```

# Introduction

The API chosen for investigation was data from the Center for Disease
Control relating to [Vaccination Coverage among
Adolescents](https://data.cdc.gov/Teen-Vaccinations/Vaccination-Coverage-among-Adolescents-13-17-Years/ee48-w5t6).
The dataset contains columns for vaccine, dose, geography type
(national, regional, state, and local area), geography name, survey
year, dimension type, dimension, estimated 95%CI, and sample size.
Dimension types include age, race/ethnicity, insurance coverage, poverty
levels, proximity to an urban area, and overall. This suggests areas of
inquiry regarding things like:

- geographical variation in vaccination coverage at regional level or
  state level  
- geographical variation in vaccination coverage differences due to
  race/ethnicity or insurance coverage at the state level  
- variation in vaccination coverage based on the vaccine in question

The vaccines covered by the database include:

- HPV (human papillomavirus)  
- Menigococcal conjugate  
- T or Td (tetanus or tetanus/diptheria)  
- Tdap (tetanus, diptheria, and pertussis) (Replaced Td in 2005)
- Varicella (chicken pox)  
- Hep A (hepatitis A)  
- MMR (measles, mumps, rubella)  
- HepB (hepatitis B)

Parental attitudes may vary a great deal between these vaccines due to
several factors such as perception of the threat being protected against
(chicken pox not as serious as diptheria), whether the threat is
associated with sexual behavior or drug use (HPV or HepB), and whether
the vaccine has received particular attention from the anti-vaccination
activists (MMR). The HPV vaccine may also be perceived as more valuable
to children who are assigned female at birth than those assigned male at
birth.

For HPV, the database uses the `dose` variable as way to track
differential treatment of “male” and “female” children.

In this project, functions were developed to help users query the
dataset about the following:

- HPV Vaccination Coverage: Regional level coverage estimates and
  differences in coverage between genders  
- HPV Vaccination Coverage: State level coverage estimates and
  differences in coverage between genders  
- Vaccination for Vaccines other than HPV: Regional level differences in
  coverage estimates with the option of changing vaccine  
- Vaccination for Vaccines other than HPV: State level differences in
  coverage estimates with the option of changing vaccine  
- Vaccination for Tdap or Meningococcal Conjugate: State level
  differences in coverage estimates with the option of changing
  dimension or vaccine

Geographical information in the database is presented at a state level
and at a regional level. The regions are listed
[here](https://www.hhs.gov/about/agencies/iea/regional-offices/index.html).
They are:

- 1: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, and
  Vermont  
- 2: New Jersey, New York, Puerto Rico, and the Virgin Islands  
- 3: Delaware, District of Columbia, Maryland, Pennsylvania, Virginia,
  and West Virginia  
- 4: Alabama, Florida, Georgia, Kentucky, Mississippi, North Carolina,
  South Carolina, and Tennessee
- 5: Illinois, Indiana, Michigan, Minnesota, Ohio, and Wisconsin  
- 6: Arkansas, Louisiana, New Mexico, Oklahoma, and Texas  
- 7: Iowa, Kansas, Missouri, and Nebraska  
- 8: Colorado, Montana, North Dakota, South Dakota, Utah, and Wyoming
- 9: Arizona, California, Hawaii, Nevada, American Samoa, Commonwealth
  of the Northern Mariana Islands, Federated States of Micronesia, Guam,
  Marshall Islands, and Republic of Palau  
- 10:Alaska, Idaho, Oregon, and Washington

These regions are shown visually here.

``` r
#Get mapping data for states
map_data_1 <- map_data("state")

#Add HHS region info for states
map_data_2 <- as_tibble(map_data_1) %>%  
    mutate("HHS_region" =
          (if_else(region %in% c('connecticut', 'maine', 'massachusetts', 
                                'new hampshire', 'rhode island', 'vermont'),"1",
          if_else(region %in% c('new jersey', 'new york', "puerto rico", "u.s. virgin islands"),"2",
          if_else(region %in% c('delaware','district of columbia', 'maryland', 'pennsylvania', 'virginia',
                                'west virginia'),"3",
          if_else(region %in% c('alabama', 'florida','georgia','kentucky', 'mississippi','north carolina',
                                'south carolina','tennessee'),"4",
          if_else(region %in% c('illinois','indiana', 'michigan', 'minnesota', 'ohio', 
                                'wisconsin'),"5",
          if_else(region %in% c('arkansas', 'louisiana', 'oklahoma', 'new mexico','texas'),"6",
          if_else(region %in% c('iowa', 'kansas','missouri', 'nebraska'),"7",
          if_else(region %in% c('colorado', 'montana','north dakota', 'south dakota',  
                                'utah', 'wyoming'),"8",
          if_else(region %in% c('arizona', 'california', 'hawaii', 'nevada', "guam") ,"9",
          if_else(region %in% c('alaska', 'idaho', 'oregon', 
                                'washington') ,"10","error"))))))))))))

#Make plot
ggplot(map_data_2, aes (x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = HHS_region), color = "black") +
#Removes lat and long labels and grid
    theme(axis.text = element_blank(),                  
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          rect = element_blank())+
#Add title
    labs(title = "Figure 1. HHS Regions for Contiental United States") +
#Change legend title
    guides(fill = guide_legend(title = "HHS Regions")) +
#Reorder legend
      scale_fill_discrete(breaks = c('1','2','3','4','5','6','7','8','9','10')) 
```

![](Project-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Accessing Data

## Downloading data

The dataset for [Vaccination Coverage among
Adolescents](https://data.cdc.gov/Teen-Vaccinations/Vaccination-Coverage-among-Adolescents-13-17-Years/ee48-w5t6)
was converted from a JSON object to and R object using `fromJSON` which
is in the `jsonlite` package. The dataset currently contains \>25,000
lines (which is greater than the default for from JSON), so the function
call needed an explicit limit.

``` r
vaccine_API <- fromJSON("https://data.cdc.gov/resource/ee48-w5t6.json?$limit=50000") %>% as_tibble

#Verify the number of rows is less than 50000 (that the limit was sufficient to retrieve the complete dataset).

nrow(vaccine_API)
```

    ## [1] 25033

``` r
vaccine_API
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
  </script>

</div>

## Function 1 for HPV Vaccination Coverage: Regional level differences between genders with user modification of year

The function `HPV_gender_eval_function` selects the rows related to HPV
vaccination at the regional level or national. The user may specify a
specific year. Possible values for years are each year of `2006` through
`2022`. The function selects rows corresponding to the coverage
estimates for up to date vaccination of each of males and females in
13-17 year age range.

``` r
HPV_gender_eval_function <- function(year_of_interest = "2022" ){
  
    #Check input type
    possible_years <-c ('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015',
                                    '2016','2017','2018','2019','2020','2021','2022')

    out_message <- "Year out of range, choose a year between 2006 and 2022 (inclusive)."
    
    if (!(year_of_interest %in% possible_years)) stop (out_message)
         
    #Select vaccine=HPV and National/Regional Level
    vaccine_API %>% filter(vaccine == "HPV" & geography_type == "HHS Regions/National") %>%
    #Filter Year based on user request
    filter(year_season == year_of_interest) %>%                              
    filter((dose == "Up-to-Date, Females")| (dose == "Up-to-Date, Males")) %>%  
    filter(dimension == "13-17 Years")
}
```

A demonstration of the function (`HPV_gender_eval_function`) for the
year 2021 is given below.

``` r
demo_function_1 <- HPV_gender_eval_function(year_of_interest = "2021")

demo_function_1
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["vaccine"],"name":[1],"type":["chr"],"align":["left"]},{"label":["dose"],"name":[2],"type":["chr"],"align":["left"]},{"label":["geography_type"],"name":[3],"type":["chr"],"align":["left"]},{"label":["geography"],"name":[4],"type":["chr"],"align":["left"]},{"label":["year_season"],"name":[5],"type":["chr"],"align":["left"]},{"label":["dimension_type"],"name":[6],"type":["chr"],"align":["left"]},{"label":["dimension"],"name":[7],"type":["chr"],"align":["left"]},{"label":["coverage_estimate"],"name":[8],"type":["chr"],"align":["left"]},{"label":["_95_ci"],"name":[9],"type":["chr"],"align":["left"]},{"label":["population_sample_size"],"name":[10],"type":["chr"],"align":["left"]}],"data":[{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"United States","5":"2021","6":"Age","7":"13-17 Years","8":"63.8","9":"61.5 to 65.9","10":"8423"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 1","5":"2021","6":"Age","7":"13-17 Years","8":"73.9","9":"68.2 to 78.8","10":"898"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 2","5":"2021","6":"Age","7":"13-17 Years","8":"66.0","9":"59.6 to 71.8","10":"400"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 3","5":"2021","6":"Age","7":"13-17 Years","8":"71.0","9":"66.9 to 74.9","10":"1546"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 4","5":"2021","6":"Age","7":"13-17 Years","8":"58.3","9":"53.3 to 63.3","10":"1023"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 5","5":"2021","6":"Age","7":"13-17 Years","8":"66.9","9":"62.7 to 70.7","10":"991"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 6","5":"2021","6":"Age","7":"13-17 Years","8":"56.0","9":"50.1 to 61.8","10":"1022"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 7","5":"2021","6":"Age","7":"13-17 Years","8":"66.2","9":"60.5 to 71.4","10":"535"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 8","5":"2021","6":"Age","7":"13-17 Years","8":"65.0","9":"59.2 to 70.3","10":"840"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 9","5":"2021","6":"Age","7":"13-17 Years","8":"65.7","9":"56.3 to 74.1","10":"574"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"Region 10","5":"2021","6":"Age","7":"13-17 Years","8":"64.3","9":"58.4 to 69.9","10":"594"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"United States","5":"2021","6":"Age","7":"13-17 Years","8":"59.8","9":"57.6 to 61.8","10":"9579"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 1","5":"2021","6":"Age","7":"13-17 Years","8":"69.5","9":"64.0 to 74.5","10":"1025"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 2","5":"2021","6":"Age","7":"13-17 Years","8":"56.6","9":"50.6 to 62.4","10":"473"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 3","5":"2021","6":"Age","7":"13-17 Years","8":"64.6","9":"60.5 to 68.6","10":"1792"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 4","5":"2021","6":"Age","7":"13-17 Years","8":"55.0","9":"50.5 to 59.4","10":"1240"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 5","5":"2021","6":"Age","7":"13-17 Years","8":"57.8","9":"53.5 to 61.9","10":"1069"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 6","5":"2021","6":"Age","7":"13-17 Years","8":"51.7","9":"46.3 to 57.0","10":"1162"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 7","5":"2021","6":"Age","7":"13-17 Years","8":"58.5","9":"53.0 to 63.7","10":"619"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 8","5":"2021","6":"Age","7":"13-17 Years","8":"65.0","9":"59.7 to 70.0","10":"949"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 9","5":"2021","6":"Age","7":"13-17 Years","8":"68.5","9":"59.2 to 76.6","10":"599"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"Region 10","5":"2021","6":"Age","7":"13-17 Years","8":"67.7","9":"61.9 to 73.0","10":"651"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

A demonstration of the results from this function call transformed into
a user-friendly table is given below.

``` r
demo_function_1a <- demo_function_1 %>%select(geography, dose, coverage_estimate) %>% 
    #recast as numeric to allow calculation of difference
    mutate (coverage_estimate = as.numeric(coverage_estimate)) %>% 
    pivot_wider(names_from = dose, values_from = (coverage_estimate)) 

#Rename columns for ease of reference in mutate 
colnames(demo_function_1a) <-c ("geography",  "cov_est_female", "cov_est_male")

#Add a column for difference between genders
demo_function_1a <- mutate(demo_function_1a, "Diff" = cov_est_female-cov_est_male)

demo_function_1a
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["geography"],"name":[1],"type":["chr"],"align":["left"]},{"label":["cov_est_female"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["cov_est_male"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Diff"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"United States","2":"63.8","3":"59.8","4":"4.0"},{"1":"Region 1","2":"73.9","3":"69.5","4":"4.4"},{"1":"Region 2","2":"66.0","3":"56.6","4":"9.4"},{"1":"Region 3","2":"71.0","3":"64.6","4":"6.4"},{"1":"Region 4","2":"58.3","3":"55.0","4":"3.3"},{"1":"Region 5","2":"66.9","3":"57.8","4":"9.1"},{"1":"Region 6","2":"56.0","3":"51.7","4":"4.3"},{"1":"Region 7","2":"66.2","3":"58.5","4":"7.7"},{"1":"Region 8","2":"65.0","3":"65.0","4":"0.0"},{"1":"Region 9","2":"65.7","3":"68.5","4":"-2.8"},{"1":"Region 10","2":"64.3","3":"67.7","4":"-3.4"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[11],"max":[11]},"pages":{}}}
  </script>

</div>

*Considered on a national level, the difference between male and female
vaccination rates is less than 5%. But in Regions 2 and 5, the
difference seems “higher” at 9%, suggesting this might be worth further
investigation.*

## Function 2 for HPV Vaccination Coverage: State level differences between genders with user modification of year

The function `HPV_gender_eval_function_state` selects the rows related
to HPV vaccination at the state/territory level . It makes use of the
`state.name` dataset built into R combined with the territories with
entries in the dataset (District of Columbia, Guam, Puerto Rico, and
U.S. Virgin Islands). Additionally The user may specify a specific year.
Possible values for years are each year of `2006` through `2022`. The
function selects rows corresponding to the coverage estimates for
up-to-date vaccination of each of males and females in 13-17 year age
range.

``` r
HPV_gender_eval_function_state <- function(year_of_interest = "2022" ){
  
    #Check input type
    possible_years <-c ('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015',
                                    '2016','2017','2018','2019','2020','2021','2022')

    out_message <- "Year out of range, choose a year between 2006 and 2022 (inclusive)."
    
    if (!(year_of_interest %in% possible_years)) stop (out_message)
    
    #function actions
    vaccine_API %>% filter((vaccine == "HPV") & 
        (geography %in% state.name | 
         geography %in% c("District of Columbia", "Guam", "Puerto Rico", 
                          "U.S. Virgin Islands","United States"))) %>%
    #Filter Year based on user request
    filter(year_season == year_of_interest) %>%    
    filter((dose == "Up-to-Date, Females")| (dose == "Up-to-Date, Males")) %>%  
    filter(dimension == "13-17 Years")
}
```

A demonstration of the function (`HPV_gender_eval_function_state`) for
the year 2021 is given below.

``` r
demo_function_2 <- HPV_gender_eval_function_state(year_of_interest = "2021")
demo_function_2
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["vaccine"],"name":[1],"type":["chr"],"align":["left"]},{"label":["dose"],"name":[2],"type":["chr"],"align":["left"]},{"label":["geography_type"],"name":[3],"type":["chr"],"align":["left"]},{"label":["geography"],"name":[4],"type":["chr"],"align":["left"]},{"label":["year_season"],"name":[5],"type":["chr"],"align":["left"]},{"label":["dimension_type"],"name":[6],"type":["chr"],"align":["left"]},{"label":["dimension"],"name":[7],"type":["chr"],"align":["left"]},{"label":["coverage_estimate"],"name":[8],"type":["chr"],"align":["left"]},{"label":["_95_ci"],"name":[9],"type":["chr"],"align":["left"]},{"label":["population_sample_size"],"name":[10],"type":["chr"],"align":["left"]}],"data":[{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Alabama","5":"2021","6":"Age","7":"13-17 Years","8":"58.2","9":"47.9 to 67.8","10":"135"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Alaska","5":"2021","6":"Age","7":"13-17 Years","8":"60.5","9":"49.5 to 70.6","10":"121"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Arizona","5":"2021","6":"Age","7":"13-17 Years","8":"63.1","9":"52.7 to 72.4","10":"127"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Arkansas","5":"2021","6":"Age","7":"13-17 Years","8":"58.9","9":"49.4 to 67.7","10":"149"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"California","5":"2021","6":"Age","7":"13-17 Years","8":"67.0","9":"54.8 to 77.3","10":"146"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Colorado","5":"2021","6":"Age","7":"13-17 Years","8":"68.6","9":"57.5 to 77.9","10":"135"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Connecticut","5":"2021","6":"Age","7":"13-17 Years","8":"67.1","9":"57.0 to 75.8","10":"159"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Delaware","5":"2021","6":"Age","7":"13-17 Years","8":"68.8","9":"59.0 to 77.1","10":"144"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"District of Columbia","5":"2021","6":"Age","7":"13-17 Years","8":"81.2","9":"73.3 to 87.2","10":"197"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Florida","5":"2021","6":"Age","7":"13-17 Years","8":"49.1","9":"36.9 to 61.5","10":"127"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Georgia","5":"2021","6":"Age","7":"13-17 Years","8":"66.6","9":"55.6 to 76.0","10":"118"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Guam","5":"2021","6":"Age","7":"13-17 Years","8":"52.9","9":"40.8 to 64.7","10":"94"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Hawaii","5":"2021","6":"Age","7":"13-17 Years","8":"69.0","9":"59.0 to 77.5","10":"126"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Idaho","5":"2021","6":"Age","7":"13-17 Years","8":"63.7","9":"53.6 to 72.7","10":"153"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Illinois","5":"2021","6":"Age","7":"13-17 Years","8":"64.8","9":"57.3 to 71.7","10":"282"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Indiana","5":"2021","6":"Age","7":"13-17 Years","8":"62.0","9":"51.4 to 71.6","10":"122"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Iowa","5":"2021","6":"Age","7":"13-17 Years","8":"70.3","9":"56.4 to 81.3","10":"92"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Kansas","5":"2021","6":"Age","7":"13-17 Years","8":"70.6","9":"60.0 to 79.4","10":"140"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Kentucky","5":"2021","6":"Age","7":"13-17 Years","8":"48.9","9":"37.2 to 60.7","10":"111"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Louisiana","5":"2021","6":"Age","7":"13-17 Years","8":"60.7","9":"50.0 to 70.4","10":"129"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Maine","5":"2021","6":"Age","7":"13-17 Years","8":"67.8","9":"57.9 to 76.4","10":"137"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Maryland","5":"2021","6":"Age","7":"13-17 Years","8":"73.8","9":"67.4 to 79.4","10":"389"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Massachusetts","5":"2021","6":"Age","7":"13-17 Years","8":"78.1","9":"66.8 to 86.4","10":"115"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Michigan","5":"2021","6":"Age","7":"13-17 Years","8":"68.9","9":"58.9 to 77.4","10":"179"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Minnesota","5":"2021","6":"Age","7":"13-17 Years","8":"69.5","9":"58.0 to 78.9","10":"135"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Mississippi","5":"2021","6":"Age","7":"13-17 Years","8":"32.6","9":"23.9 to 42.6","10":"146"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Missouri","5":"2021","6":"Age","7":"13-17 Years","8":"62.7","9":"53.1 to 71.5","10":"156"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Montana","5":"2021","6":"Age","7":"13-17 Years","8":"49.5","9":"39.6 to 59.3","10":"135"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Nebraska","5":"2021","6":"Age","7":"13-17 Years","8":"63.2","9":"52.5 to 72.7","10":"147"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Nevada","5":"2021","6":"Age","7":"13-17 Years","8":"54.9","9":"44.6 to 64.8","10":"175"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"New Hampshire","5":"2021","6":"Age","7":"13-17 Years","8":"77.0","9":"67.6 to 84.4","10":"133"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"New Jersey","5":"2021","6":"Age","7":"13-17 Years","8":"63.0","9":"51.3 to 73.4","10":"122"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"New Mexico","5":"2021","6":"Age","7":"13-17 Years","8":"62.9","9":"53.5 to 71.3","10":"168"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"New York","5":"2021","6":"Age","7":"13-17 Years","8":"67.5","9":"59.9 to 74.3","10":"278"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"North Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"72.8","9":"60.0 to 82.7","10":"122"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"North Dakota","5":"2021","6":"Age","7":"13-17 Years","8":"68.6","9":"56.0 to 78.9","10":"87"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Ohio","5":"2021","6":"Age","7":"13-17 Years","8":"69.0","9":"57.2 to 78.8","10":"129"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Oklahoma","5":"2021","6":"Age","7":"13-17 Years","8":"55.1","9":"44.5 to 65.4","10":"114"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Oregon","5":"2021","6":"Age","7":"13-17 Years","8":"63.3","9":"52.6 to 72.8","10":"129"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Pennsylvania","5":"2021","6":"Age","7":"13-17 Years","8":"68.2","9":"61.4 to 74.3","10":"434"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Puerto Rico","5":"2021","6":"Age","7":"13-17 Years","8":"68.6","9":"59.3 to 76.6","10":"175"},{"1":"HPV","2":"Up-to-Date, Females","3":"HHS Regions/National","4":"United States","5":"2021","6":"Age","7":"13-17 Years","8":"63.8","9":"61.5 to 65.9","10":"8423"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Rhode Island","5":"2021","6":"Age","7":"13-17 Years","8":"80.1","9":"68.4 to 88.2","10":"138"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"South Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"61.0","9":"50.1 to 71.0","10":"123"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"South Dakota","5":"2021","6":"Age","7":"13-17 Years","8":"78.3","9":"70.7 to 84.4","10":"202"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Tennessee","5":"2021","6":"Age","7":"13-17 Years","8":"64.4","9":"54.2 to 73.5","10":"141"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Texas","5":"2021","6":"Age","7":"13-17 Years","8":"54.8","9":"46.6 to 62.7","10":"462"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"U.S. Virgin Islands","5":"2021","6":"Age","7":"13-17 Years","8":"45.7","9":"33.0 to 58.9","10":"113"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Utah","5":"2021","6":"Age","7":"13-17 Years","8":"62.5","9":"52.1 to 71.9","10":"144"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Vermont","5":"2021","6":"Age","7":"13-17 Years","8":"63.8","9":"55.8 to 71.1","10":"216"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Virginia","5":"2021","6":"Age","7":"13-17 Years","8":"73.5","9":"63.4 to 81.6","10":"259"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Washington","5":"2021","6":"Age","7":"13-17 Years","8":"65.5","9":"55.8 to 74.0","10":"191"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"West Virginia","5":"2021","6":"Age","7":"13-17 Years","8":"67.3","9":"57.2 to 76.0","10":"123"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Wisconsin","5":"2021","6":"Age","7":"13-17 Years","8":"66.9","9":"57.3 to 75.3","10":"144"},{"1":"HPV","2":"Up-to-Date, Females","3":"States/Local Areas","4":"Wyoming","5":"2021","6":"Age","7":"13-17 Years","8":"48.7","9":"38.2 to 59.3","10":"137"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Alabama","5":"2021","6":"Age","7":"13-17 Years","8":"66.6","9":"57.7 to 74.5","10":"169"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Alaska","5":"2021","6":"Age","7":"13-17 Years","8":"52.1","9":"42.2 to 61.8","10":"147"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Arizona","5":"2021","6":"Age","7":"13-17 Years","8":"60.3","9":"50.6 to 69.2","10":"169"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Arkansas","5":"2021","6":"Age","7":"13-17 Years","8":"54.7","9":"45.5 to 63.7","10":"165"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"California","5":"2021","6":"Age","7":"13-17 Years","8":"70.9","9":"58.7 to 80.7","10":"140"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Colorado","5":"2021","6":"Age","7":"13-17 Years","8":"69.8","9":"60.3 to 77.8","10":"163"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Connecticut","5":"2021","6":"Age","7":"13-17 Years","8":"65.8","9":"56.8 to 73.8","10":"181"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Delaware","5":"2021","6":"Age","7":"13-17 Years","8":"68.9","9":"59.7 to 76.9","10":"166"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"District of Columbia","5":"2021","6":"Age","7":"13-17 Years","8":"77.7","9":"68.3 to 84.9","10":"212"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Florida","5":"2021","6":"Age","7":"13-17 Years","8":"48.8","9":"38.1 to 59.6","10":"185"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Georgia","5":"2021","6":"Age","7":"13-17 Years","8":"55.4","9":"43.9 to 66.3","10":"142"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Guam","5":"2021","6":"Age","7":"13-17 Years","8":"54.9","9":"44.1 to 65.2","10":"131"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Hawaii","5":"2021","6":"Age","7":"13-17 Years","8":"69.6","9":"59.4 to 78.2","10":"132"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Idaho","5":"2021","6":"Age","7":"13-17 Years","8":"59.5","9":"50.4 to 68.0","10":"181"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Illinois","5":"2021","6":"Age","7":"13-17 Years","8":"59.6","9":"52.3 to 66.6","10":"296"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Indiana","5":"2021","6":"Age","7":"13-17 Years","8":"48.7","9":"39.1 to 58.5","10":"146"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Iowa","5":"2021","6":"Age","7":"13-17 Years","8":"62.0","9":"49.5 to 73.0","10":"107"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Kansas","5":"2021","6":"Age","7":"13-17 Years","8":"58.3","9":"47.9 to 67.9","10":"160"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Kentucky","5":"2021","6":"Age","7":"13-17 Years","8":"64.8","9":"54.2 to 74.2","10":"129"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Louisiana","5":"2021","6":"Age","7":"13-17 Years","8":"67.1","9":"57.9 to 75.1","10":"174"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Maine","5":"2021","6":"Age","7":"13-17 Years","8":"55.5","9":"45.9 to 64.8","10":"156"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Maryland","5":"2021","6":"Age","7":"13-17 Years","8":"70.2","9":"63.9 to 75.8","10":"468"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Massachusetts","5":"2021","6":"Age","7":"13-17 Years","8":"72.0","9":"61.0 to 80.9","10":"151"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Michigan","5":"2021","6":"Age","7":"13-17 Years","8":"60.7","9":"50.4 to 70.2","10":"179"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Minnesota","5":"2021","6":"Age","7":"13-17 Years","8":"62.0","9":"52.4 to 70.8","10":"161"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Mississippi","5":"2021","6":"Age","7":"13-17 Years","8":"32.8","9":"24.3 to 42.6","10":"157"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Missouri","5":"2021","6":"Age","7":"13-17 Years","8":"56.0","9":"46.6 to 65.0","10":"184"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Montana","5":"2021","6":"Age","7":"13-17 Years","8":"55.5","9":"46.2 to 64.5","10":"155"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Nebraska","5":"2021","6":"Age","7":"13-17 Years","8":"60.7","9":"50.9 to 69.7","10":"168"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Nevada","5":"2021","6":"Age","7":"13-17 Years","8":"58.0","9":"47.6 to 67.8","10":"158"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"New Hampshire","5":"2021","6":"Age","7":"13-17 Years","8":"67.8","9":"58.2 to 76.1","10":"149"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"New Jersey","5":"2021","6":"Age","7":"13-17 Years","8":"47.0","9":"37.2 to 57.0","10":"170"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"New Mexico","5":"2021","6":"Age","7":"13-17 Years","8":"53.1","9":"44.2 to 61.9","10":"169"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"New York","5":"2021","6":"Age","7":"13-17 Years","8":"61.5","9":"53.9 to 68.5","10":"303"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"North Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"62.9","9":"52.2 to 72.5","10":"144"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"North Dakota","5":"2021","6":"Age","7":"13-17 Years","8":"75.6","9":"63.8 to 84.4","10":"104"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Ohio","5":"2021","6":"Age","7":"13-17 Years","8":"55.6","9":"43.4 to 67.2","10":"125"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Oklahoma","5":"2021","6":"Age","7":"13-17 Years","8":"58.2","9":"47.9 to 67.8","10":"126"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Oregon","5":"2021","6":"Age","7":"13-17 Years","8":"70.6","9":"61.2 to 78.5","10":"165"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Pennsylvania","5":"2021","6":"Age","7":"13-17 Years","8":"69.2","9":"62.9 to 74.9","10":"495"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Puerto Rico","5":"2021","6":"Age","7":"13-17 Years","8":"65.9","9":"56.5 to 74.1","10":"175"},{"1":"HPV","2":"Up-to-Date, Males","3":"HHS Regions/National","4":"United States","5":"2021","6":"Age","7":"13-17 Years","8":"59.8","9":"57.6 to 61.8","10":"9579"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Rhode Island","5":"2021","6":"Age","7":"13-17 Years","8":"86.3","9":"77.1 to 92.2","10":"116"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"South Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"63.2","9":"52.9 to 72.3","10":"140"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"South Dakota","5":"2021","6":"Age","7":"13-17 Years","8":"71.3","9":"63.1 to 78.4","10":"243"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Tennessee","5":"2021","6":"Age","7":"13-17 Years","8":"48.8","9":"39.3 to 58.4","10":"174"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Texas","5":"2021","6":"Age","7":"13-17 Years","8":"48.3","9":"41.0 to 55.6","10":"528"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"U.S. Virgin Islands","5":"2021","6":"Age","7":"13-17 Years","8":"45.3","9":"33.3 to 57.8","10":"132"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Utah","5":"2021","6":"Age","7":"13-17 Years","8":"60.2","9":"49.9 to 69.7","10":"160"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Vermont","5":"2021","6":"Age","7":"13-17 Years","8":"69.5","9":"62.6 to 75.6","10":"272"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Virginia","5":"2021","6":"Age","7":"13-17 Years","8":"56.7","9":"46.4 to 66.4","10":"307"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Washington","5":"2021","6":"Age","7":"13-17 Years","8":"70.1","9":"60.1 to 78.5","10":"158"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"West Virginia","5":"2021","6":"Age","7":"13-17 Years","8":"46.1","9":"36.6 to 55.9","10":"144"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Wisconsin","5":"2021","6":"Age","7":"13-17 Years","8":"59.9","9":"50.7 to 68.4","10":"162"},{"1":"HPV","2":"Up-to-Date, Males","3":"States/Local Areas","4":"Wyoming","5":"2021","6":"Age","7":"13-17 Years","8":"47.4","9":"36.8 to 58.4","10":"124"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

A demonstration of the results from this function call transformed into
a user-friendly table is given below.

``` r
demo_function_2a <- demo_function_2 %>% select(geography, dose, coverage_estimate) %>% 
    mutate (coverage_estimate = as.numeric(coverage_estimate)) %>%
    pivot_wider(names_from = dose, values_from = coverage_estimate)

#Rename columns for ease of reference in mutate 
colnames(demo_function_2a) <-c ("geography",  "cov_est_female", "cov_est_male")

#Add a column for difference between genders
demo_function_2a <- mutate(demo_function_2a, "Diff" = cov_est_female-cov_est_male)
demo_function_2a
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["geography"],"name":[1],"type":["chr"],"align":["left"]},{"label":["cov_est_female"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["cov_est_male"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Diff"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"Alabama","2":"58.2","3":"66.6","4":"-8.4"},{"1":"Alaska","2":"60.5","3":"52.1","4":"8.4"},{"1":"Arizona","2":"63.1","3":"60.3","4":"2.8"},{"1":"Arkansas","2":"58.9","3":"54.7","4":"4.2"},{"1":"California","2":"67.0","3":"70.9","4":"-3.9"},{"1":"Colorado","2":"68.6","3":"69.8","4":"-1.2"},{"1":"Connecticut","2":"67.1","3":"65.8","4":"1.3"},{"1":"Delaware","2":"68.8","3":"68.9","4":"-0.1"},{"1":"District of Columbia","2":"81.2","3":"77.7","4":"3.5"},{"1":"Florida","2":"49.1","3":"48.8","4":"0.3"},{"1":"Georgia","2":"66.6","3":"55.4","4":"11.2"},{"1":"Guam","2":"52.9","3":"54.9","4":"-2.0"},{"1":"Hawaii","2":"69.0","3":"69.6","4":"-0.6"},{"1":"Idaho","2":"63.7","3":"59.5","4":"4.2"},{"1":"Illinois","2":"64.8","3":"59.6","4":"5.2"},{"1":"Indiana","2":"62.0","3":"48.7","4":"13.3"},{"1":"Iowa","2":"70.3","3":"62.0","4":"8.3"},{"1":"Kansas","2":"70.6","3":"58.3","4":"12.3"},{"1":"Kentucky","2":"48.9","3":"64.8","4":"-15.9"},{"1":"Louisiana","2":"60.7","3":"67.1","4":"-6.4"},{"1":"Maine","2":"67.8","3":"55.5","4":"12.3"},{"1":"Maryland","2":"73.8","3":"70.2","4":"3.6"},{"1":"Massachusetts","2":"78.1","3":"72.0","4":"6.1"},{"1":"Michigan","2":"68.9","3":"60.7","4":"8.2"},{"1":"Minnesota","2":"69.5","3":"62.0","4":"7.5"},{"1":"Mississippi","2":"32.6","3":"32.8","4":"-0.2"},{"1":"Missouri","2":"62.7","3":"56.0","4":"6.7"},{"1":"Montana","2":"49.5","3":"55.5","4":"-6.0"},{"1":"Nebraska","2":"63.2","3":"60.7","4":"2.5"},{"1":"Nevada","2":"54.9","3":"58.0","4":"-3.1"},{"1":"New Hampshire","2":"77.0","3":"67.8","4":"9.2"},{"1":"New Jersey","2":"63.0","3":"47.0","4":"16.0"},{"1":"New Mexico","2":"62.9","3":"53.1","4":"9.8"},{"1":"New York","2":"67.5","3":"61.5","4":"6.0"},{"1":"North Carolina","2":"72.8","3":"62.9","4":"9.9"},{"1":"North Dakota","2":"68.6","3":"75.6","4":"-7.0"},{"1":"Ohio","2":"69.0","3":"55.6","4":"13.4"},{"1":"Oklahoma","2":"55.1","3":"58.2","4":"-3.1"},{"1":"Oregon","2":"63.3","3":"70.6","4":"-7.3"},{"1":"Pennsylvania","2":"68.2","3":"69.2","4":"-1.0"},{"1":"Puerto Rico","2":"68.6","3":"65.9","4":"2.7"},{"1":"United States","2":"63.8","3":"59.8","4":"4.0"},{"1":"Rhode Island","2":"80.1","3":"86.3","4":"-6.2"},{"1":"South Carolina","2":"61.0","3":"63.2","4":"-2.2"},{"1":"South Dakota","2":"78.3","3":"71.3","4":"7.0"},{"1":"Tennessee","2":"64.4","3":"48.8","4":"15.6"},{"1":"Texas","2":"54.8","3":"48.3","4":"6.5"},{"1":"U.S. Virgin Islands","2":"45.7","3":"45.3","4":"0.4"},{"1":"Utah","2":"62.5","3":"60.2","4":"2.3"},{"1":"Vermont","2":"63.8","3":"69.5","4":"-5.7"},{"1":"Virginia","2":"73.5","3":"56.7","4":"16.8"},{"1":"Washington","2":"65.5","3":"70.1","4":"-4.6"},{"1":"West Virginia","2":"67.3","3":"46.1","4":"21.2"},{"1":"Wisconsin","2":"66.9","3":"59.9","4":"7.0"},{"1":"Wyoming","2":"48.7","3":"47.4","4":"1.3"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

*States that have more than a 15% difference (absolute) in coverage
rates (with a higher percent of females being vaccinated) include New
Jersey, Tennessee, Virginia, and West Virginia. At the other extreme,
Kentucky has more than a 15% difference (absolute) in coverage with a
higher percent of males vaccinated. The quick look at the data suggests
that further study of the gender differences in HPV vaccine coverage
correlated with states might be interesting.*

## Function 3 for Vaccination Coverage for Vaccines other than HPV: Regional Level Differences

The function `vaccines_regional_eval_function` selects the rows related
tovaccination with vaccines other than HPV at the regional level.
Varicella data is also being excluded because the compliance data is
separated into entries for teens with just vaccinations, those with a
history of the disease, and those with both.

The user can select a `year_of_interest` to use in the analysis.
Possible values for years are each year of `2006` through `2022`. A
default value of 2022 is provided for `year_of_interest`.

The user can select a `vaccine_of_interest`. Possible values include:

- “Meningococcal Conjugate”  
- “Td or Tdap”  
- “Tdap”  
- “Hep A”  
- “MMR”  
- “HepB”

A default value of NULL is provided for `vaccine_of_interest`; this will
return data for all rows with the vaccines equal to any in the above
list.

``` r
vaccines_regional_eval_function <- function(vaccine_of_interest = NULL, year_of_interest = "2022" ){
  
    #Check input type-year 
    possible_years <- c('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015',
                                    '2016','2017','2018','2019','2020','2021','2022')

    out_message <- "Year out of range, choose a year between 2006 and 2022 (inclusive)."
    
    if (!(year_of_interest %in% possible_years)) stop (out_message)      
  
    #Check input type-vaccines
    possible_vaccines <- c("Meningococcal Conjugate", "Td or Tdap", "Tdap", "Hep A", "MMR", "HepB", NULL)  

    out_message <- "Choices for vaccine are 'Meningococcal Conjugate', 'Td or Tdap', 'Tdap', 'Hep A', 'MMR', 'HepB' or NULL."

    if (!(is.null(vaccine_of_interest))) {
        if (!(vaccine_of_interest %in% possible_vaccines)) stop (out_message)
}

    #functions actions on dataset
    filtered_API <- vaccine_API %>% 
        filter(vaccine != "HPV" & vaccine != "Varicella"  #Select vaccine=not HPV or Varicella
                & geography_type == "HHS Regions/National") %>%  #Select National/Regional Level
        #Filter Year based on user request
        filter(year_season == year_of_interest) %>%
        #Filter age to allow comparison between vaccines
        filter (dimension =="13-17 Years")%>% 
        #Recast coverage_estimate to numeric 
        mutate(coverage_estimate = as.numeric(coverage_estimate)) 

    # filter for vaccine_of_interest if it is present, otherwise return all
    if (!is.null(vaccine_of_interest)) {
        filtered_API <- filtered_API %>% 
          filter(vaccine == vaccine_of_interest)
    }
    return(filtered_API)
}
```

A demonstration of the function (`vaccines_regional_eval_function`) is
given below.

``` r
demo_function_3 <- vaccines_regional_eval_function(year_of_interest = "2021")
```

Presentation of the results from this function call transformed into a
user-friendly table is given below.

``` r
demo_function_3a <- demo_function_3 %>%
    select(geography, vaccine, coverage_estimate) %>% 
    pivot_wider(names_from = vaccine, values_from = coverage_estimate)
demo_function_3a
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["geography"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Meningococcal Conjugate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Td or Tdap"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Tdap"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Hep A"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["MMR"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["HepB"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"United States","2":"89.0","3":"92.2","4":"89.6","5":"85.0","6":"92.2","7":"92.3"},{"1":"Region 1","2":"91.9","3":"95.7","4":"93.8","5":"86.6","6":"96.5","7":"95.8"},{"1":"Region 2","2":"93.2","3":"90.7","4":"87.9","5":"81.5","6":"92.8","7":"94.6"},{"1":"Region 3","2":"91.4","3":"92.0","4":"89.7","5":"85.8","6":"93.4","7":"92.1"},{"1":"Region 4","2":"84.9","3":"93.5","4":"91.0","5":"81.9","6":"95.1","7":"95.1"},{"1":"Region 5","2":"91.6","3":"93.3","4":"89.4","5":"84.9","6":"94.2","7":"94.1"},{"1":"Region 6","2":"89.9","3":"90.7","4":"88.1","5":"84.6","6":"86.6","7":"86.5"},{"1":"Region 7","2":"90.4","3":"92.0","4":"90.2","5":"82.6","6":"92.5","7":"93.1"},{"1":"Region 8","2":"88.2","3":"93.1","4":"91.6","5":"89.3","6":"94.8","7":"93.7"},{"1":"Region 9","2":"87.5","3":"91.0","4":"89.2","5":"88.5","6":"89.0","7":"89.9"},{"1":"Region 10","2":"84.6","3":"89.4","4":"86.9","5":"89.4","6":"92.3","7":"91.2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[11],"max":[11]},"pages":{}}}
  </script>

</div>

*Based on this function call, it appears that vaccination coverage for
Hepatitis A is lower than the other vaccines.  
[Immunization requiredments for secondary
school](https://www.immunize.org/laws/hepa.asp) only include Hepatitis A
in about half the states. This might be related. Looking at the state
data should include could show if lower rates of vaccination for
Hepatitis A are observed only in states without a vaccination
requirement.*

## Function 4 for Vaccination Coverage for Vaccines other than HPV: State Level Differences

The function `vaccines_state_eval_function` selects the rows related to
vaccination with vaccines other than HPV at the state level. Varicella
data is also being excluded because the compliance data is separated
into entries for teens with just vaccinations, those with a history of
the disease and those with both.

The user can select a `year_of_interest` to use in the analysis.
Possible values for years are each year of `2006` through `2022`. A
default value of 2022 is provided for `year_of_interest`.

The user can select a `vaccine_of_interest`. Possible values include:

- “Meningococcal Conjugate”  
- “Td or Tdap”  
- “Tdap”  
- “Hep A”  
- “MMR”  
- “HepB”

A default value of NULL is provided for `vaccine_of_interest` this will
return data for all rows with the vaccines equal to any in the above
list.

The user can select a `state_or_territory_of_interest`. Possible values
include the fifty states as well as the territories with data present in
the dataset (District of Columbia, Guam, Puerto Rico, and U.S. Virgin
Islands.) A default value of NULL is provided for `state_of_interest`;
this will return data for all the states and territories.

``` r
vaccines_state_eval_function <- function(vaccine_of_interest = NULL, state_of_interest=NULL, year_of_interest = "2022" ){
  
    #Check input type-year 
    possible_years <-c ('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015',
                                    '2016','2017','2018','2019','2020','2021','2022')

    out_message <- "Year out of range, choose a year between 2006 and 2022 (inclusive)."
    
    if (!(year_of_interest %in% possible_years)) stop (out_message)      
  
    #Check input type-vaccines
    possible_vaccines <- c("Meningococcal Conjugate", "Td or Tdap", "Tdap", "Hep A", "MMR", "HepB" )  

    out_message <- "Choices for vaccine are 'Meningococcal Conjugate', 'Td or Tdap', 'Tdap', 'Hep A', 'MMR', 'HepB' or NULL."
  
    if (!(is.null(vaccine_of_interest))) {
        if (!(vaccine_of_interest %in% possible_vaccines)) stop (out_message)}
    

  
    #Check input type-states
    possible_nonstates <- c ("District of Columbia", "Guam", "Puerto Rico", "U.S. Virgin Islands") 

    out_message <- "Choices for state include the fifty states as well as 'District of Columbia', 'Guam', 'Puerto Rico', and 'U.S. Virgin Islands'."
  
    if (!(is.null(state_of_interest))) {
    if (!(state_of_interest %in% state.name | state_of_interest %in% possible_nonstates)) stop (out_message)
    }
  
    #Function operations on dataset
    filtered_API <- vaccine_API %>% 
        #Select rows where vaccine is not HPV or Varicella and geography is a state or territory
        filter(vaccine != "HPV" & vaccine != "Varicella" &         
              (geography %in% state.name | 
               geography %in% c ("District of Columbia", "Guam", "Puerto Rico", "U.S. Virgin Islands"))) %>%
        #Filter Year based on user input
        filter(year_season == year_of_interest) %>% 
        #Filter age to allow easy comparison between vaccines
        filter (dimension == "13-17 Years") %>%                    
        #Recast coverage_estimate to numeric 
        mutate(coverage_estimate = as.numeric(coverage_estimate)) 

    # filter for vaccine_of_interest if it is present, otherwise return all
    if (!is.null(vaccine_of_interest)) {
        filtered_API <- filtered_API %>% 
          filter(vaccine == vaccine_of_interest)
    }

    # filter for state_of_interest if it is present, otherwise return all
    if (!is.null(state_of_interest)) {
        filtered_API <- filtered_API %>% 
          filter(geography == state_of_interest)
    }

    return(filtered_API)
}
```

A demonstration of the function (`vaccines_state_eval_function`) is
given below.

``` r
demo_function_4 <- vaccines_state_eval_function(year_of_interest = "2021", state_of_interest = "North Carolina")

demo_function_4
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["vaccine"],"name":[1],"type":["chr"],"align":["left"]},{"label":["dose"],"name":[2],"type":["chr"],"align":["left"]},{"label":["geography_type"],"name":[3],"type":["chr"],"align":["left"]},{"label":["geography"],"name":[4],"type":["chr"],"align":["left"]},{"label":["year_season"],"name":[5],"type":["chr"],"align":["left"]},{"label":["dimension_type"],"name":[6],"type":["chr"],"align":["left"]},{"label":["dimension"],"name":[7],"type":["chr"],"align":["left"]},{"label":["coverage_estimate"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["_95_ci"],"name":[9],"type":["chr"],"align":["left"]},{"label":["population_sample_size"],"name":[10],"type":["chr"],"align":["left"]}],"data":[{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"93.3","9":"87.6 to 96.5","10":"266"},{"1":"Td or Tdap","2":">=1 Dose","3":"States/Local Areas","4":"North Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"95.5","9":"89.9 to 98.1","10":"266"},{"1":"Tdap","2":">=1 Dose","3":"States/Local Areas","4":"North Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"94.5","9":"88.9 to 97.4","10":"266"},{"1":"Hep A","2":">=2 Doses","3":"States/Local Areas","4":"North Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"90.8","9":"83.6 to 95.0","10":"266"},{"1":"MMR","2":">=2 Doses","3":"States/Local Areas","4":"North Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"95.3","9":"88.8 to 98.1","10":"266"},{"1":"HepB","2":">=3 Doses","3":"States/Local Areas","4":"North Carolina","5":"2021","6":"Age","7":"13-17 Years","8":"95.5","9":"88.9 to 98.2","10":"266"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

## Function 5 for Vaccination Coverage 2018-2022 for Vaccines other than HPV or Varicella: State Level Differences

The function `vaccines_4_year_function` selects the rows related to
vaccination with vaccines other than HPV and Varicella at the state
level. The user can select a `dimension_type_of_interest`. Possible
values are:  
- “Insurance Coverage”  
- “Poverty”  
- “Race/Ethnicity”  
- “Urbanicity”  
- “Overall”

The complete list of dimension types are only available for Tdap and
Meningococcal Conjugate. For other vaccines, only `Overall` is
available. The default value for `dimension_type_of_interest` is
“Overall”.

The user can select a `vaccine_of_interest`. Possible values include:  
- “Meningococcal Conjugate”  
- “Td or Tdap”  
- “Tdap”  
- “Hep A”  
- “MMR”  
- “HepB”

A default value of NULL is provided for `vaccine_of_interest`; this will
return data for all rows with the vaccines equal to any in the above
list.

``` r
vaccines_4_year_function <- function(dimension_type_of_interest="Overall", vaccine_of_interest = NULL){
  
    #Check input type-vaccines
    possible_vaccines <- c("Meningococcal Conjugate", "Td or Tdap", "Tdap", "Hep A", "MMR", "HepB" )  

    out_message <- "Choices for vaccine are 'Meningococcal Conjugate', 'Td or Tdap', 'Tdap', 'Hep A', 'MMR', 'HepB' or NULL."
  
    if (!(is.null(vaccine_of_interest))) {
        if (!(vaccine_of_interest %in% possible_vaccines)) stop (out_message)}

    #Check input type-dimension_type
    possible_dimensions <- c("Insurance Coverage", "Poverty", "Race/Ethnicity ", "Urbanicity", "Overall")

    out_message <- 'Choices for dimension type are Insurance Coverage", "Poverty", "Race/Ethnicity ", "Urbanicity", "Overall".'
  
    if (!(dimension_type_of_interest %in% possible_dimensions)) stop (out_message)
    
    #Function actions on dataset
    filtered_API <- vaccine_API %>% 
    #select the 4-year season
    filter(year_season == "2018-2022") %>%                         
    #remove HPV and varicella
    filter(vaccine != "HPV" & vaccine != "Varicella") %>% 
    #select the dimension_type of interest
    filter(dimension_type == dimension_type_of_interest) %>%       
    #remove metropolitan areas
    filter(geography %in% state.name |                            
        geography %in% c("District of Columbia", "Guam", "Puerto Rico", "U.S. Virgin Islands", "United States")) %>%
    #Recast coverage_estimate to numeric 
    mutate(coverage_estimate = as.numeric(coverage_estimate))      
    
    # filter for vaccine_of_interest if it is present, otherwise return all
    if (!is.null(vaccine_of_interest)) {
        filtered_API <- filtered_API %>% 
                        filter(vaccine == vaccine_of_interest)
    }
    return(filtered_API)
}
```

A demonstration of the function (`vaccines_4_year_function`) is given
below.

``` r
demo_function_5 <- vaccines_4_year_function(dimension_type_of_interest = "Insurance Coverage", 
                                            vaccine_of_interest = "Meningococcal Conjugate")
demo_function_5
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["vaccine"],"name":[1],"type":["chr"],"align":["left"]},{"label":["dose"],"name":[2],"type":["chr"],"align":["left"]},{"label":["geography_type"],"name":[3],"type":["chr"],"align":["left"]},{"label":["geography"],"name":[4],"type":["chr"],"align":["left"]},{"label":["year_season"],"name":[5],"type":["chr"],"align":["left"]},{"label":["dimension_type"],"name":[6],"type":["chr"],"align":["left"]},{"label":["dimension"],"name":[7],"type":["chr"],"align":["left"]},{"label":["coverage_estimate"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["_95_ci"],"name":[9],"type":["chr"],"align":["left"]},{"label":["population_sample_size"],"name":[10],"type":["chr"],"align":["left"]}],"data":[{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Alabama","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"76.7","9":"66.4 to 84.6","10":"138"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Alaska","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"80.8","9":"74.6 to 85.9","10":"316"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Arizona","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"83.5","9":"73.3 to 90.3","10":"119"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Arkansas","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"94.8","9":"89.5 to 97.5","10":"223"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"California","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"84.0","9":"64.3 to 93.8","10":"132"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Colorado","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"88.4","9":"81.7 to 92.8","10":"133"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Connecticut","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"91.0","9":"83.2 to 95.4","10":"211"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Delaware","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"89.8","9":"77.7 to 95.7","10":"72"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"District of Columbia","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"95.5","9":"80.1 to 99.1","10":"44"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Florida","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"74.7","9":"62.2 to 84.1","10":"147"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Georgia","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"95.3","9":"89.5 to 98.0","10":"158"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Hawaii","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"79.5","9":"70.6 to 86.3","10":"157"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Idaho","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"82.5","9":"68.7 to 91.1","10":"94"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Illinois","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"88.1","9":"78.5 to 93.7","10":"101"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Indiana","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"85.9","9":"69.1 to 94.4","10":"41"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Iowa","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"95.2","9":"90.1 to 97.8","10":"103"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Kansas","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"84.5","9":"79.1 to 88.7","10":"316"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Kentucky","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"87.3","9":"75.6 to 93.9","10":"78"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Louisiana","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"91.6","9":"81.8 to 96.4","10":"67"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Maine","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"82.8","9":"70.7 to 90.6","10":"83"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Maryland","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"94.1","9":"86.6 to 97.5","10":"180"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Massachusetts","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"96.0","9":"82.6 to 99.2","10":"67"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Michigan","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"93.4","9":"85.1 to 97.2","10":"72"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Minnesota","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"78.8","9":"54.9 to 91.9","10":"49"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Mississippi","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"61.1","9":"48.6 to 72.3","10":"105"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Missouri","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"84.1","9":"72.0 to 91.6","10":"85"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Montana","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"80.6","9":"72.0 to 87.0","10":"150"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Nebraska","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"98.5","9":"95.9 to 99.5","10":"89"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Nevada","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"86.1","9":"76.7 to 92.1","10":"124"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Hampshire","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"88.3","9":"75.3 to 94.9","10":"72"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Jersey","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"91.3","9":"78.3 to 96.8","10":"47"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Mexico","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"87.3","9":"79.0 to 92.7","10":"103"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New York","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"95.7","9":"93.0 to 97.4","10":"365"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Carolina","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"89.5","9":"81.8 to 94.2","10":"153"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Dakota","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"95.9","9":"91.8 to 98.0","10":"148"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Ohio","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"78.6","9":"59.6 to 90.1","10":"65"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Oklahoma","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"78.1","9":"70.8 to 84.1","10":"217"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Oregon","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"80.7","9":"70.4 to 88.0","10":"122"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Pennsylvania","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"90.4","9":"83.3 to 94.6","10":"305"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"HHS Regions/National","4":"United States","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"86.6","9":"84.9 to 88.2","10":"7253"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Rhode Island","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"91.5","9":"76.3 to 97.3","10":"54"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"South Carolina","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"83.5","9":"73.1 to 90.4","10":"98"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"South Dakota","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"91.8","9":"82.1 to 96.4","10":"135"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Tennessee","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"76.3","9":"65.6 to 84.4","10":"123"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Texas","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"89.3","9":"85.4 to 92.2","10":"705"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Utah","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"91.4","9":"80.9 to 96.4","10":"103"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Vermont","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"95.2","9":"87.9 to 98.2","10":"78"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Virginia","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"79.7","9":"70.7 to 86.5","10":"296"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Washington","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"83.0","9":"73.0 to 89.9","10":"143"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"West Virginia","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"96.1","9":"89.3 to 98.6","10":"99"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Wisconsin","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"68.6","9":"51.8 to 81.7","10":"55"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Wyoming","5":"2018-2022","6":"Insurance Coverage","7":"Other","8":"76.4","9":"64.6 to 85.2","10":"113"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Alabama","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"84.1","9":"80.9 to 86.9","10":"806"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Alaska","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"73.5","9":"69.5 to 77.1","10":"794"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Arizona","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"87.4","9":"84.1 to 90.1","10":"826"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Arkansas","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"95.1","9":"93.1 to 96.5","10":"752"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"California","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"85.7","9":"82.1 to 88.7","10":"951"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Colorado","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"86.8","9":"83.9 to 89.3","10":"950"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Connecticut","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"95.3","9":"93.4 to 96.7","10":"1000"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Delaware","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"91.0","9":"88.3 to 93.2","10":"915"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"District of Columbia","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"91.0","9":"87.9 to 93.3","10":"1006"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Florida","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"82.4","9":"78.5 to 85.7","10":"892"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Georgia","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"95.1","9":"92.8 to 96.6","10":"886"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Hawaii","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"89.9","9":"87.3 to 92.0","10":"797"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Idaho","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"89.8","9":"86.9 to 92.1","10":"911"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Illinois","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"94.4","9":"92.9 to 95.6","10":"1944"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Indiana","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"93.0","9":"90.6 to 94.8","10":"847"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Iowa","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"94.7","9":"92.6 to 96.3","10":"769"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Kansas","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"82.2","9":"78.8 to 85.1","10":"970"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Kentucky","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"91.3","9":"88.4 to 93.6","10":"743"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Louisiana","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"90.6","9":"87.5 to 93.0","10":"756"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Maine","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"91.1","9":"88.6 to 93.1","10":"943"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Maryland","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"93.9","9":"92.0 to 95.4","10":"2139"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Massachusetts","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"94.2","9":"91.9 to 95.9","10":"1143"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Michigan","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"94.4","9":"92.0 to 96.0","10":"1070"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Minnesota","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"92.1","9":"89.7 to 94.0","10":"1092"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Mississippi","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"57.1","9":"52.1 to 61.9","10":"617"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Missouri","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"88.3","9":"85.7 to 90.6","10":"1032"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Montana","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"79.7","9":"76.2 to 82.8","10":"823"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Nebraska","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"86.7","9":"83.7 to 89.2","10":"1027"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Nevada","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"86.4","9":"83.3 to 89.1","10":"889"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Hampshire","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"90.7","9":"88.6 to 92.5","10":"1058"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Jersey","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"93.0","9":"90.9 to 94.7","10":"1192"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Mexico","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"83.4","9":"79.7 to 86.6","10":"721"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New York","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"95.4","9":"94.0 to 96.5","10":"1581"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Carolina","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"92.3","9":"89.4 to 94.5","10":"906"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Dakota","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"95.6","9":"93.7 to 97.0","10":"1015"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Ohio","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"92.5","9":"90.0 to 94.4","10":"951"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Oklahoma","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"72.4","9":"67.9 to 76.4","10":"632"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Oregon","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"84.6","9":"81.4 to 87.3","10":"858"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Pennsylvania","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"95.1","9":"93.6 to 96.3","10":"2218"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"HHS Regions/National","4":"United States","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"89.5","9":"88.9 to 90.0","10":"54111"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Rhode Island","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"97.7","9":"95.8 to 98.8","10":"867"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"South Carolina","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"83.5","9":"80.2 to 86.4","10":"805"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"South Dakota","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"91.2","9":"89.2 to 93.0","10":"1100"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Tennessee","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"83.5","9":"80.1 to 86.4","10":"814"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Texas","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"88.6","9":"86.7 to 90.2","10":"3327"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Utah","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"89.2","9":"86.6 to 91.3","10":"1130"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Vermont","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"91.2","9":"89.0 to 93.1","10":"1194"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Virginia","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"87.0","9":"84.0 to 89.4","10":"1860"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Washington","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"88.3","9":"85.3 to 90.8","10":"930"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"West Virginia","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"91.1","9":"88.4 to 93.2","10":"735"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Wisconsin","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"91.2","9":"88.9 to 93.0","10":"986"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Wyoming","5":"2018-2022","6":"Insurance Coverage","7":"Private Insurance Only","8":"69.6","9":"65.8 to 73.2","10":"941"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Alabama","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"87.2","9":"83.6 to 90.1","10":"521"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Alaska","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"76.1","9":"71.2 to 80.3","10":"474"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Arizona","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"90.0","9":"86.5 to 92.7","10":"476"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Arkansas","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"94.2","9":"91.5 to 96.1","10":"567"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"California","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"87.0","9":"82.7 to 90.3","10":"528"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Colorado","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"84.8","9":"80.0 to 88.6","10":"414"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Connecticut","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"91.8","9":"86.7 to 95.1","10":"273"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Delaware","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"86.6","9":"82.3 to 89.9","10":"497"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"District of Columbia","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"92.4","9":"89.2 to 94.7","10":"549"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Florida","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"81.2","9":"76.4 to 85.2","10":"503"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Georgia","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"94.0","9":"90.5 to 96.3","10":"430"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Hawaii","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"82.9","9":"78.2 to 86.8","10":"443"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Idaho","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"92.2","9":"88.9 to 94.6","10":"521"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Illinois","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"90.8","9":"88.4 to 92.7","10":"1112"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Indiana","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"91.1","9":"87.5 to 93.7","10":"467"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Iowa","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"91.0","9":"86.8 to 94.0","10":"373"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Kansas","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"85.5","9":"79.7 to 89.9","10":"290"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Kentucky","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"88.3","9":"84.4 to 91.4","10":"473"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Louisiana","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"89.8","9":"86.6 to 92.2","10":"748"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Maine","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"90.8","9":"87.5 to 93.3","10":"496"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Maryland","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"92.2","9":"88.5 to 94.8","10":"569"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Massachusetts","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"96.1","9":"92.5 to 98.0","10":"332"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Michigan","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"92.7","9":"88.6 to 95.3","10":"449"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Minnesota","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"92.0","9":"87.5 to 94.9","10":"348"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Mississippi","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"63.8","9":"58.9 to 68.4","10":"589"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Missouri","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"82.5","9":"77.6 to 86.6","10":"445"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Montana","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"73.8","9":"68.8 to 78.2","10":"481"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Nebraska","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"84.8","9":"79.6 to 88.9","10":"364"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Nevada","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"85.5","9":"81.1 to 89.1","10":"497"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Hampshire","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"83.9","9":"78.6 to 88.0","10":"361"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Jersey","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"90.8","9":"86.2 to 94.0","10":"362"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Mexico","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"83.9","9":"80.5 to 86.8","10":"794"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New York","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"93.9","9":"91.7 to 95.6","10":"897"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Carolina","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"92.2","9":"88.7 to 94.7","10":"471"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Dakota","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"93.4","9":"88.9 to 96.1","10":"267"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Ohio","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"89.3","9":"85.5 to 92.2","10":"451"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Oklahoma","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"84.9","9":"81.3 to 87.9","10":"659"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Oregon","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"84.0","9":"79.4 to 87.7","10":"430"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Pennsylvania","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"94.0","9":"91.7 to 95.7","10":"1213"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"HHS Regions/National","4":"United States","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"88.3","9":"87.5 to 89.0","10":"27403"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Rhode Island","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"95.2","9":"92.2 to 97.1","10":"508"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"South Carolina","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"84.2","9":"80.1 to 87.7","10":"523"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"South Dakota","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"93.0","9":"89.5 to 95.3","10":"381"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Tennessee","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"87.7","9":"83.3 to 91.0","10":"439"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Texas","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"88.2","9":"85.2 to 90.7","10":"2274"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Utah","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"90.4","9":"84.7 to 94.1","10":"218"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Vermont","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"90.2","9":"87.3 to 92.5","10":"690"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Virginia","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"81.8","9":"75.3 to 86.9","10":"319"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Washington","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"85.1","9":"80.9 to 88.5","10":"544"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"West Virginia","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"90.9","9":"87.7 to 93.4","10":"603"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Wisconsin","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"90.5","9":"87.0 to 93.2","10":"454"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Wyoming","5":"2018-2022","6":"Insurance Coverage","7":"Any Medicaid","8":"79.5","9":"73.7 to 84.3","10":"316"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Alaska","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"67.4","9":"48.2 to 82.1","10":"44"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Arizona","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"83.3","9":"71.6 to 90.8","10":"84"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Arkansas","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"90.9","9":"78.2 to 96.5","10":"63"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Colorado","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"74.5","9":"55.9 to 87.0","10":"37"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Connecticut","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"82.3","9":"59.4 to 93.7","10":"32"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Delaware","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"84.6","9":"71.6 to 92.3","10":"50"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Florida","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"72.9","9":"58.9 to 83.4","10":"98"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Georgia","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"88.3","9":"77.2 to 94.4","10":"79"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Idaho","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"76.9","9":"61.5 to 87.4","10":"58"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Illinois","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"85.5","9":"73.8 to 92.5","10":"73"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Indiana","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"95.6","9":"86.9 to 98.6","10":"58"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Kansas","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"63.2","9":"48.7 to 75.6","10":"76"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Kentucky","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"76.3","9":"54.2 to 89.8","10":"39"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Louisiana","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"93.8","9":"83.4 to 97.9","10":"38"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Maine","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"71.2","9":"53.5 to 84.2","10":"44"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Maryland","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"89.4","9":"66.6 to 97.3","10":"47"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Michigan","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"85.8","9":"65.4 to 95.1","10":"32"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Mississippi","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"55.4","9":"40.0 to 69.9","10":"61"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Missouri","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"78.0","9":"62.9 to 88.2","10":"66"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Nebraska","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"93.5","9":"83.4 to 97.6","10":"67"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Nevada","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"86.1","9":"75.6 to 92.5","10":"98"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Hampshire","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"74.5","9":"49.0 to 89.9","10":"33"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Jersey","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"92.7","9":"82.8 to 97.1","10":"41"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New Mexico","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"68.1","9":"47.7 to 83.4","10":"47"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"New York","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"82.9","9":"53.5 to 95.4","10":"36"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Carolina","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"90.5","9":"77.7 to 96.4","10":"70"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"North Dakota","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"82.4","9":"64.1 to 92.4","10":"48"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Oklahoma","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"74.9","9":"58.3 to 86.5","10":"48"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Pennsylvania","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"88.8","9":"71.9 to 96.1","10":"93"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"HHS Regions/National","4":"United States","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"79.9","9":"76.7 to 82.8","10":"2929"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Rhode Island","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"91.7","9":"75.1 to 97.6","10":"37"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"South Carolina","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"68.6","9":"53.8 to 80.4","10":"54"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"South Dakota","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"75.3","9":"57.8 to 87.2","10":"51"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Tennessee","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"71.5","9":"57.4 to 82.3","10":"66"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Texas","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"83.2","9":"76.4 to 88.3","10":"548"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Utah","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"90.5","9":"77.2 to 96.4","10":"63"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Virginia","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"82.8","9":"65.2 to 92.5","10":"53"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"West Virginia","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"85.7","9":"67.9 to 94.5","10":"30"},{"1":"Meningococcal Conjugate","2":">=1 Dose","3":"States/Local Areas","4":"Wyoming","5":"2018-2022","6":"Insurance Coverage","7":"Uninsured","8":"64.1","9":"49.3 to 76.5","10":"79"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

Presentation of the results from this function call transformed into a
user-friendly table is given below.

``` r
demo_function_5a <- demo_function_5 %>%
    select(geography, dimension, coverage_estimate) %>%
    pivot_wider(names_from = dimension, values_from = coverage_estimate)

demo_function_5a
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["geography"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Other"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Private Insurance Only"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Any Medicaid"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Uninsured"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"Alabama","2":"76.7","3":"84.1","4":"87.2","5":"NA"},{"1":"Alaska","2":"80.8","3":"73.5","4":"76.1","5":"67.4"},{"1":"Arizona","2":"83.5","3":"87.4","4":"90.0","5":"83.3"},{"1":"Arkansas","2":"94.8","3":"95.1","4":"94.2","5":"90.9"},{"1":"California","2":"84.0","3":"85.7","4":"87.0","5":"NA"},{"1":"Colorado","2":"88.4","3":"86.8","4":"84.8","5":"74.5"},{"1":"Connecticut","2":"91.0","3":"95.3","4":"91.8","5":"82.3"},{"1":"Delaware","2":"89.8","3":"91.0","4":"86.6","5":"84.6"},{"1":"District of Columbia","2":"95.5","3":"91.0","4":"92.4","5":"NA"},{"1":"Florida","2":"74.7","3":"82.4","4":"81.2","5":"72.9"},{"1":"Georgia","2":"95.3","3":"95.1","4":"94.0","5":"88.3"},{"1":"Hawaii","2":"79.5","3":"89.9","4":"82.9","5":"NA"},{"1":"Idaho","2":"82.5","3":"89.8","4":"92.2","5":"76.9"},{"1":"Illinois","2":"88.1","3":"94.4","4":"90.8","5":"85.5"},{"1":"Indiana","2":"85.9","3":"93.0","4":"91.1","5":"95.6"},{"1":"Iowa","2":"95.2","3":"94.7","4":"91.0","5":"NA"},{"1":"Kansas","2":"84.5","3":"82.2","4":"85.5","5":"63.2"},{"1":"Kentucky","2":"87.3","3":"91.3","4":"88.3","5":"76.3"},{"1":"Louisiana","2":"91.6","3":"90.6","4":"89.8","5":"93.8"},{"1":"Maine","2":"82.8","3":"91.1","4":"90.8","5":"71.2"},{"1":"Maryland","2":"94.1","3":"93.9","4":"92.2","5":"89.4"},{"1":"Massachusetts","2":"96.0","3":"94.2","4":"96.1","5":"NA"},{"1":"Michigan","2":"93.4","3":"94.4","4":"92.7","5":"85.8"},{"1":"Minnesota","2":"78.8","3":"92.1","4":"92.0","5":"NA"},{"1":"Mississippi","2":"61.1","3":"57.1","4":"63.8","5":"55.4"},{"1":"Missouri","2":"84.1","3":"88.3","4":"82.5","5":"78.0"},{"1":"Montana","2":"80.6","3":"79.7","4":"73.8","5":"NA"},{"1":"Nebraska","2":"98.5","3":"86.7","4":"84.8","5":"93.5"},{"1":"Nevada","2":"86.1","3":"86.4","4":"85.5","5":"86.1"},{"1":"New Hampshire","2":"88.3","3":"90.7","4":"83.9","5":"74.5"},{"1":"New Jersey","2":"91.3","3":"93.0","4":"90.8","5":"92.7"},{"1":"New Mexico","2":"87.3","3":"83.4","4":"83.9","5":"68.1"},{"1":"New York","2":"95.7","3":"95.4","4":"93.9","5":"82.9"},{"1":"North Carolina","2":"89.5","3":"92.3","4":"92.2","5":"90.5"},{"1":"North Dakota","2":"95.9","3":"95.6","4":"93.4","5":"82.4"},{"1":"Ohio","2":"78.6","3":"92.5","4":"89.3","5":"NA"},{"1":"Oklahoma","2":"78.1","3":"72.4","4":"84.9","5":"74.9"},{"1":"Oregon","2":"80.7","3":"84.6","4":"84.0","5":"NA"},{"1":"Pennsylvania","2":"90.4","3":"95.1","4":"94.0","5":"88.8"},{"1":"United States","2":"86.6","3":"89.5","4":"88.3","5":"79.9"},{"1":"Rhode Island","2":"91.5","3":"97.7","4":"95.2","5":"91.7"},{"1":"South Carolina","2":"83.5","3":"83.5","4":"84.2","5":"68.6"},{"1":"South Dakota","2":"91.8","3":"91.2","4":"93.0","5":"75.3"},{"1":"Tennessee","2":"76.3","3":"83.5","4":"87.7","5":"71.5"},{"1":"Texas","2":"89.3","3":"88.6","4":"88.2","5":"83.2"},{"1":"Utah","2":"91.4","3":"89.2","4":"90.4","5":"90.5"},{"1":"Vermont","2":"95.2","3":"91.2","4":"90.2","5":"NA"},{"1":"Virginia","2":"79.7","3":"87.0","4":"81.8","5":"82.8"},{"1":"Washington","2":"83.0","3":"88.3","4":"85.1","5":"NA"},{"1":"West Virginia","2":"96.1","3":"91.1","4":"90.9","5":"85.7"},{"1":"Wisconsin","2":"68.6","3":"91.2","4":"90.5","5":"NA"},{"1":"Wyoming","2":"76.4","3":"69.6","4":"79.5","5":"64.1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

## Summary of Functions

Five functions were developed to do the following:

- Function 1: Obtain regional data about differences in HPV vaccination
  coverage between genders; user can specify year  
- Function 2: Obtain state data about differences in HPV vaccination
  coverage between genders; user can specify year  
- Function 3: Obtain regional data about vaccination coverage for
  meningococcal disease, tetanus, diptheria, pertussis, hepatitis A,
  measles, mumps, rubella, and hepatitis B; the user can specify a year
  and a vaccine  
- Function 4: Obtain state data about vaccination coverage for
  meningococcal disease, tetanus, diptheria, pertussis, hepatitis A,
  measles, mumps, rubella, and hepatitis B; the user can specify a year
  , a vaccine, and a state  
- Function 5: Obtain state data about vaccination data for the Tdap and
  meningococcal vaccine for the 2018-2022 time-frame filtered by
  dimension (insurance coverage, poverty, race/ethnicity, urbanicity,
  and overall); the user may specify a vaccine and a dimension.

# Exploratory Data Analysis

## Comparing Vaccine Coverage for Tdap and Meningococcal Conjugate

While all states require Tdap vaccination for secondary school
attendance, only 35 states require vaccination with the meningococcal
conjugate for secondary school attendance. A possible line of inquiry is
does this difference lead to lower vaccine coverage for meningococcal
conjugate. Data will be obtained for this using Function 5,
`vaccines_4_year_function`. The function was called using all vaccines
and with overall for the dimension. The other vaccines were filtered out
and then the table pivoted to a wide format.

``` r
coverage_comparison_1 <- vaccines_4_year_function () %>%
    filter(vaccine == "Tdap" | vaccine == "Meningococcal Conjugate") %>%
    select (geography, vaccine, coverage_estimate) %>%
    pivot_wider(names_from = vaccine, values_from = coverage_estimate)

#Change columns names to eliminate illegal characters introduced in pivot operation

colnames(coverage_comparison_1) <- c("geography", "Meningococcal_conjugate", "Tdap")

coverage_comparison_1
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["geography"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Meningococcal_conjugate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Tdap"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Alabama","2":"84.4","3":"91.6"},{"1":"Alaska","2":"75.7","3":"82.4"},{"1":"Arizona","2":"87.8","3":"86.8"},{"1":"Arkansas","2":"94.5","3":"93.3"},{"1":"California","2":"85.7","3":"87.5"},{"1":"Colorado","2":"86.0","3":"90.8"},{"1":"Connecticut","2":"93.6","3":"94.5"},{"1":"Delaware","2":"89.0","3":"88.4"},{"1":"District of Columbia","2":"91.5","3":"87.1"},{"1":"Florida","2":"80.7","3":"91.5"},{"1":"Georgia","2":"94.4","3":"92.2"},{"1":"Hawaii","2":"85.8","3":"86.4"},{"1":"Idaho","2":"89.8","3":"88.2"},{"1":"Illinois","2":"92.7","3":"91.3"},{"1":"Indiana","2":"92.2","3":"91.6"},{"1":"Iowa","2":"93.3","3":"93.9"},{"1":"Kansas","2":"82.3","3":"89.5"},{"1":"Kentucky","2":"89.5","3":"87.5"},{"1":"Louisiana","2":"90.2","3":"92.9"},{"1":"Maine","2":"90.1","3":"91.0"},{"1":"Maryland","2":"93.3","3":"89.7"},{"1":"Massachusetts","2":"94.6","3":"93.7"},{"1":"Michigan","2":"93.6","3":"90.6"},{"1":"Minnesota","2":"91.3","3":"91.6"},{"1":"Mississippi","2":"60.6","3":"89.9"},{"1":"Missouri","2":"85.8","3":"85.4"},{"1":"Montana","2":"76.7","3":"88.5"},{"1":"Nebraska","2":"87.1","3":"90.8"},{"1":"Nevada","2":"86.1","3":"88.3"},{"1":"New Hampshire","2":"88.4","3":"94.8"},{"1":"New Jersey","2":"92.3","3":"89.3"},{"1":"New Mexico","2":"83.5","3":"87.4"},{"1":"New York","2":"94.8","3":"91.2"},{"1":"North Carolina","2":"92.0","3":"91.9"},{"1":"North Dakota","2":"94.8","3":"93.4"},{"1":"Ohio","2":"90.6","3":"91.8"},{"1":"Oklahoma","2":"79.1","3":"86.8"},{"1":"Oregon","2":"83.6","3":"88.0"},{"1":"Pennsylvania","2":"94.2","3":"92.8"},{"1":"United States","2":"88.5","3":"89.7"},{"1":"Rhode Island","2":"96.4","3":"95.2"},{"1":"South Carolina","2":"83.3","3":"91.0"},{"1":"South Dakota","2":"91.2","3":"91.5"},{"1":"Tennessee","2":"83.7","3":"90.2"},{"1":"Texas","2":"88.0","3":"84.9"},{"1":"Utah","2":"89.6","3":"91.0"},{"1":"Vermont","2":"90.9","3":"94.2"},{"1":"Virginia","2":"84.7","3":"89.7"},{"1":"Washington","2":"86.2","3":"89.1"},{"1":"West Virginia","2":"91.2","3":"90.1"},{"1":"Wisconsin","2":"89.7","3":"90.6"},{"1":"Wyoming","2":"72.4","3":"89.3"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

The summaries for the two vaccines were printed out.

``` r
#Print quantile summaries
knitr::kable(summary(coverage_comparison_1)[,2:3])
```

|     | Meningococcal_conjugate | Tdap          |
|:----|:------------------------|:--------------|
|     | Min. :60.60             | Min. :82.40   |
|     | 1st Qu.:85.45           | 1st Qu.:88.38 |
|     | Median :89.55           | Median :90.60 |
|     | Mean :87.83             | Mean :90.21   |
|     | 3rd Qu.:92.22           | 3rd Qu.:91.65 |
|     | Max. :96.40             | Max. :95.20   |

The data for vaccination with the meningococcal conjugate does appear
more spread out.

## Vaccine Coverage for Meningococcal Conjugate by State Requirements

To see if the spread in coverage for mengicoccal disease is related to
[requirements for secondary school
attendance](https://www.immunize.org/laws/menin_sec.asp), a column was
added to the data giving the regulatory status (as a categorical
variable) and then that was used for comparison. Since vaccination is
not mandated at a federal level, `United States` was also filtered out.

``` r
#States not requiring vaccination with meningococcal conjugate
mening_not_required <- c("Alabama","Alaska","California", "Colorado", "Florida", "Mississippi", "Montana",
                         "Nebraska", "New Hampshire", "Oklahoma", "Oregon", "South Carolina", "Tennessee",
                         "Washington", "Wyoming" )


meningococcal_regulation_analysis <- coverage_comparison_1 %>%
    #remove Tdap column
    select(!Tdap) %>%
    #remove US row
    filter(geography != "United States") %>%   
    #create new variable
    mutate(mening_req = if_else                
                        (geography %in% mening_not_required, 
                         "No", 
                         "Yes")) %>%
#reorder columns
    select(geography, mening_req, Meningococcal_conjugate) 

#Set the regulatory status to be a factor
meningococcal_regulation_analysis$mening_req <-as.factor(meningococcal_regulation_analysis$mening_req)

meningococcal_regulation_analysis
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["geography"],"name":[1],"type":["chr"],"align":["left"]},{"label":["mening_req"],"name":[2],"type":["fct"],"align":["left"]},{"label":["Meningococcal_conjugate"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Alabama","2":"No","3":"84.4"},{"1":"Alaska","2":"No","3":"75.7"},{"1":"Arizona","2":"Yes","3":"87.8"},{"1":"Arkansas","2":"Yes","3":"94.5"},{"1":"California","2":"No","3":"85.7"},{"1":"Colorado","2":"No","3":"86.0"},{"1":"Connecticut","2":"Yes","3":"93.6"},{"1":"Delaware","2":"Yes","3":"89.0"},{"1":"District of Columbia","2":"Yes","3":"91.5"},{"1":"Florida","2":"No","3":"80.7"},{"1":"Georgia","2":"Yes","3":"94.4"},{"1":"Hawaii","2":"Yes","3":"85.8"},{"1":"Idaho","2":"Yes","3":"89.8"},{"1":"Illinois","2":"Yes","3":"92.7"},{"1":"Indiana","2":"Yes","3":"92.2"},{"1":"Iowa","2":"Yes","3":"93.3"},{"1":"Kansas","2":"Yes","3":"82.3"},{"1":"Kentucky","2":"Yes","3":"89.5"},{"1":"Louisiana","2":"Yes","3":"90.2"},{"1":"Maine","2":"Yes","3":"90.1"},{"1":"Maryland","2":"Yes","3":"93.3"},{"1":"Massachusetts","2":"Yes","3":"94.6"},{"1":"Michigan","2":"Yes","3":"93.6"},{"1":"Minnesota","2":"Yes","3":"91.3"},{"1":"Mississippi","2":"No","3":"60.6"},{"1":"Missouri","2":"Yes","3":"85.8"},{"1":"Montana","2":"No","3":"76.7"},{"1":"Nebraska","2":"No","3":"87.1"},{"1":"Nevada","2":"Yes","3":"86.1"},{"1":"New Hampshire","2":"No","3":"88.4"},{"1":"New Jersey","2":"Yes","3":"92.3"},{"1":"New Mexico","2":"Yes","3":"83.5"},{"1":"New York","2":"Yes","3":"94.8"},{"1":"North Carolina","2":"Yes","3":"92.0"},{"1":"North Dakota","2":"Yes","3":"94.8"},{"1":"Ohio","2":"Yes","3":"90.6"},{"1":"Oklahoma","2":"No","3":"79.1"},{"1":"Oregon","2":"No","3":"83.6"},{"1":"Pennsylvania","2":"Yes","3":"94.2"},{"1":"Rhode Island","2":"Yes","3":"96.4"},{"1":"South Carolina","2":"No","3":"83.3"},{"1":"South Dakota","2":"Yes","3":"91.2"},{"1":"Tennessee","2":"No","3":"83.7"},{"1":"Texas","2":"Yes","3":"88.0"},{"1":"Utah","2":"Yes","3":"89.6"},{"1":"Vermont","2":"Yes","3":"90.9"},{"1":"Virginia","2":"Yes","3":"84.7"},{"1":"Washington","2":"No","3":"86.2"},{"1":"West Virginia","2":"Yes","3":"91.2"},{"1":"Wisconsin","2":"Yes","3":"89.7"},{"1":"Wyoming","2":"No","3":"72.4"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

This data set can be visualized using a violin plot.

``` r
#Violin plot comparing states with and without  requirements for secondary school attendance
#Make base layer 
graph_2 <- ggplot(meningococcal_regulation_analysis, aes(x = mening_req, y = Meningococcal_conjugate))

#Add layers
graph_2 + geom_violin(fill = 'gray') +
    labs( x = "Vaccination Required for Secondary School",
          y = "Vaccination Coverage Estimate",
          title = "Figure 2. Variation in Coverage Estimate for Meningococcal Disease",
          subtitle = "by if State Requires Vaccination for Secondary School Attendance")
```

![](Project-2_files/figure-gfm/violin%20plot-1.png)<!-- -->

The violin plots show a considerable difference in vaccination coverage
rates between states with and without a vaccination requirement for
secondary school.

It is interesting to consider if the parental choice to vaccinate
children is influenced by some other factor when vaccination is not
mandated for school attendance. The data allows evaluation using
insurance coverage, poverty, race/ethnicity, and urbanicity.

## Vaccine Coverage for Meningococcal Conjugate in States without Mandates

Analyses were performed to determine if coverage estimates for
meningococcal disease are affected by the factors: insurance coverage,
poverty, race/ethnicity, and urbanicity. Function 5 was used, selecting
for meningococcal conjugate and the appropriate dimension, working
through them consecutively.  
Data for states with vaccination requirements for secondary school were
filtered out. The dimension data was recast as factors, and box plots
were generated.

``` r
#Select data relating to insurance
insurance_comparison_1 <- vaccines_4_year_function(dimension_type_of_interest = "Insurance Coverage",
                                                   vaccine_of_interest = "Meningococcal Conjugate") %>%
    select(geography, dimension, coverage_estimate) %>%
    filter(geography %in% mening_not_required) 

#Recast dimension as factor
insurance_comparison_1$dimension <- as.factor(insurance_comparison_1$dimension)

#Make base layer 
graph_3 <- ggplot(insurance_comparison_1, aes(x = dimension, y = coverage_estimate))

#Add layers
graph_3 + geom_boxplot(fill = 'gray') +
    labs( x = "Insurance Coverage",
          y = "Vaccination Coverage Estimate",
          title = "Figure 3. Variation in Coverage Estimate for Meningococcal Disease",
          subtitle = "with Insurance in States without Vaccination Requirements for Secondary School Attendance")
```

![](Project-2_files/figure-gfm/compare%20menigococcal%20using%20insurance-1.png)<!-- -->

``` r
#Select data relating to poverty
poverty_comparison_1 <- vaccines_4_year_function(dimension_type_of_interest = "Poverty",
                                                   vaccine_of_interest = "Meningococcal Conjugate") %>%
    select(geography, dimension, coverage_estimate) %>%
    filter(geography %in% mening_not_required) 

#Recast dimension as factor
poverty_comparison_1$dimension <- as.factor(poverty_comparison_1$dimension)

#Make base layer 
graph_4 <- ggplot(poverty_comparison_1, aes(x = dimension, y = coverage_estimate))

#Add layers
graph_4 + geom_boxplot(fill = 'gray') +
    labs( x = "Poverty",
          y = "Vaccination Coverage Estimate",
          title = "Figure 4. Variation in Coverage Estimate for Meningococcal Disease",
          subtitle = "with Poverty Level in States without Vaccination Requirements for Secondary School Attendance")
```

![](Project-2_files/figure-gfm/compare%20menigococcal%20using%20different%20dimensions-1.png)<!-- -->

``` r
#Select data relating to race/ethnicity
ethnicity_comparison_1 <- vaccines_4_year_function(dimension_type_of_interest = "Race/Ethnicity ",
                                                   vaccine_of_interest = "Meningococcal Conjugate") %>%
    select(geography, dimension, coverage_estimate) %>%
    filter(geography %in% mening_not_required) 

#Recast dimension as factor
ethnicity_comparison_1$dimension <- as.factor(ethnicity_comparison_1$dimension)

#Make base layer 
graph_5 <- ggplot(ethnicity_comparison_1, aes(x = dimension, y = coverage_estimate))

#Add layers
graph_5 + geom_boxplot(fill = 'gray') +
    labs( x = "Race/Ethnicity",
          y = "Vaccination Coverage Estimate",
          title = "Figure 5. Variation in Coverage Estimate for Meningococcal Disease ",
          subtitle = "with Race/Ethnicity inStates without Vaccination Requirements for Secondary School Attendance")
```

![](Project-2_files/figure-gfm/compare%20menigococcal%20using%20race/ethnicity-1.png)<!-- -->

``` r
#Select data relating to urbanicity
urbanicity_comparison_1 <- vaccines_4_year_function(dimension_type_of_interest = "Urbanicity",
                                                   vaccine_of_interest = "Meningococcal Conjugate") %>%
    select(geography, dimension, coverage_estimate) %>%
    filter(geography %in% mening_not_required) 

#Recast dimension as factor
urbanicity_comparison_1$dimension <- as.factor(urbanicity_comparison_1$dimension)

#Make base layer 
graph_6 <- ggplot(urbanicity_comparison_1, aes(x = dimension, y = coverage_estimate))

#Add layers
graph_6 + geom_boxplot(fill = 'gray') +
    labs( x = "Urbanicity",
          y = "Vaccination Coverage Estimate",
          title = "Figure 6. Variation in Coverage Estimate for Meningococcal Disease ",
          subtitle = "with Urbanicity inStates without Vaccination Requirements for Secondary School Attendance")
```

![](Project-2_files/figure-gfm/compare%20menigococcal%20using%20urbanicity-1.png)<!-- -->

Comparing these four boxplots, it appears that of the factors studied-
insurance coverage has the largest impact on vaccination. Specifically
lower vaccination coverage is observed in the uninsured. The plots on
urbanicity suggest that living in a non-metropolitan statistical area
also lowers the rate of vaccination.

## Vaccine Coverage for Vaccines by Region

To determine which vaccines exhibited the most variation in vaccine
coverage, a regional analysis was performed. Function 3,
`vaccines_regional_eval_function` was used with the year set to 2021.
Data was then graphed as vaccination coverage versus region in Graph 7.

``` r
#Remove row for country level
vac_cov_region <- vaccines_regional_eval_function(year_of_interest = "2021") %>%
    filter(geography != "United States")

#Make base layer
graph_7 <-ggplot(data = vac_cov_region, aes(x = as.factor(geography), y = coverage_estimate))

#Build graph
graph_7 + geom_point(aes(color = as.factor(vaccine))) +
    geom_line(aes(color = as.factor(vaccine), group = as.factor(vaccine))) +
    theme(legend.position = "bottom") +
    labs( x = "Region",
          y = "Vaccination Coverage Estimate",
          title = "Figure 7. Variation in Coverage Estimate for Vaccines Across Regions",
          color = "vaccine") +
    guides(fill = guide_legend(title = "Vaccine")) +
    scale_x_discrete(limits = c('Region 1','Region 2','Region 3', 'Region 4','Region 5','Region 6',
                                'Region 7','Region 8','Region 9','Region 10'))
```

![](Project-2_files/figure-gfm/Vaccine%20Coverage%20by%20Region-1.png)<!-- -->

After inspection of Graph 7, Hepatitis A was chosen for further
examination. Function 3, `vaccines_state_eval_function` was used to
retrieve the data. To allow comparison with the regional data, a column
for region was added. To allow analysis based on the whether the state
requires a child to be vaccinated for hepatitis A to attend secondary
school, a column was added for that.

``` r
#Make vector of states with mandated Hep A vaccination for childcare or secondary school enrollment
hep_a_required <- c('Alaska', 'Arkansas', "Connecticut", 'District of Columbia', 'Georgia', 'Hawaii',
    'Idaho', 'Indiana', 'Kansas', 'Kentucky','Louisiana', 'Minnesota','Nevada','New Mexico', 
    'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 
    'South Dakota', 'Tennessee','Texas', 'Utah', 'Virginia')

#State with vaccination required in some counties
hep_a_required_county <- c("Arizona")

hepatitis_a <- vaccines_state_eval_function(vaccine_of_interest = "Hep A", 
                                            year_of_interest = "2021") %>%
    #Filter out territories due to lack of info about mandate
    filter((geography != "Guam") & (geography != "U.S. Virgin Islands") & (geography != "Puerto Rico")) %>%
    select(geography, coverage_estimate) %>% 
    #Add region
    mutate("HHS_geography" = (if_else(geography %in% c('Connecticut', 'Maine', 'Massachusetts', 
                                   'New Hampshire', 'Rhode Island', 'Vermont'),"1",
       if_else(geography %in% c('New Jersey', 'New York', "Puerto Rico", "U.S. Virgin Islands"),"2",
       if_else(geography %in% c('Delaware','District of Columbia', 'Maryland', 'Pennsylvania', 'Virginia',
                                'West Virginia'),"3",
       if_else(geography %in% c('Alabama', 'Florida','Georgia','Kentucky', 'Mississippi','North Carolina',
                                'South Carolina','Tennessee'),"4",
       if_else(geography %in% c('Illinois','Indiana', 'Michigan', 'Minnesota', 'Ohio', 
                                'Wisconsin'),"5",
       if_else(geography %in% c('Arkansas', 'Louisiana', 'Oklahoma', 'New Mexico','Texas'),"6",
       if_else(geography %in% c('Iowa', 'Kansas','Missouri', 'Nebraska'),"7",
       if_else(geography %in% c('Colorado', 'Montana','North Dakota', 'South Dakota',  
                                'Utah', 'Wyoming'),"8",
       if_else(geography %in% c('Arizona', 'California', 'Hawaii', 'Nevada', "Guam") ,"9",
       if_else(geography %in% c('Alaska', 'Idaho', 'Oregon', 
                                'Washington') ,"10","error")))))))))))) %>%
    #Add column with info about hepatitis vaccine mandate
    mutate("hep_a_required" = (if_else(geography %in% hep_a_required, 'yes', 
        (if_else(geography %in% hep_a_required_county, 'some', 'no'))))) %>%
    #Reorder columns
    select(geography, HHS_geography, hep_a_required, coverage_estimate)
    
hepatitis_a
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["geography"],"name":[1],"type":["chr"],"align":["left"]},{"label":["HHS_geography"],"name":[2],"type":["chr"],"align":["left"]},{"label":["hep_a_required"],"name":[3],"type":["chr"],"align":["left"]},{"label":["coverage_estimate"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"Alabama","2":"4","3":"no","4":"83.0"},{"1":"Alaska","2":"10","3":"yes","4":"89.0"},{"1":"Arizona","2":"9","3":"some","4":"85.9"},{"1":"Arkansas","2":"6","3":"yes","4":"84.2"},{"1":"California","2":"9","3":"no","4":"88.9"},{"1":"Colorado","2":"8","3":"no","4":"88.4"},{"1":"Connecticut","2":"1","3":"yes","4":"87.6"},{"1":"Delaware","2":"3","3":"no","4":"85.7"},{"1":"District of Columbia","2":"3","3":"yes","4":"84.2"},{"1":"Florida","2":"4","3":"no","4":"75.4"},{"1":"Georgia","2":"4","3":"yes","4":"88.9"},{"1":"Hawaii","2":"9","3":"yes","4":"89.5"},{"1":"Idaho","2":"10","3":"yes","4":"90.4"},{"1":"Illinois","2":"5","3":"no","4":"81.2"},{"1":"Indiana","2":"5","3":"yes","4":"90.6"},{"1":"Iowa","2":"7","3":"no","4":"86.5"},{"1":"Kansas","2":"7","3":"yes","4":"83.9"},{"1":"Kentucky","2":"4","3":"yes","4":"91.5"},{"1":"Louisiana","2":"6","3":"yes","4":"79.7"},{"1":"Maine","2":"1","3":"no","4":"84.5"},{"1":"Maryland","2":"3","3":"no","4":"85.6"},{"1":"Massachusetts","2":"1","3":"no","4":"87.6"},{"1":"Michigan","2":"5","3":"no","4":"84.6"},{"1":"Minnesota","2":"5","3":"yes","4":"91.2"},{"1":"Mississippi","2":"4","3":"no","4":"52.5"},{"1":"Missouri","2":"7","3":"no","4":"79.6"},{"1":"Montana","2":"8","3":"no","4":"81.4"},{"1":"Nebraska","2":"7","3":"no","4":"83.8"},{"1":"Nevada","2":"9","3":"yes","4":"90.1"},{"1":"New Hampshire","2":"1","3":"no","4":"84.1"},{"1":"New Jersey","2":"2","3":"no","4":"80.0"},{"1":"New Mexico","2":"6","3":"yes","4":"87.8"},{"1":"New York","2":"2","3":"no","4":"82.2"},{"1":"North Carolina","2":"4","3":"no","4":"90.8"},{"1":"North Dakota","2":"8","3":"yes","4":"96.9"},{"1":"Ohio","2":"5","3":"yes","4":"83.8"},{"1":"Oklahoma","2":"6","3":"yes","4":"91.4"},{"1":"Oregon","2":"10","3":"yes","4":"94.8"},{"1":"Pennsylvania","2":"3","3":"yes","4":"86.7"},{"1":"Rhode Island","2":"1","3":"yes","4":"85.9"},{"1":"South Carolina","2":"4","3":"yes","4":"68.4"},{"1":"South Dakota","2":"8","3":"yes","4":"87.2"},{"1":"Tennessee","2":"4","3":"yes","4":"91.4"},{"1":"Texas","2":"6","3":"yes","4":"84.3"},{"1":"Utah","2":"8","3":"yes","4":"94.3"},{"1":"Vermont","2":"1","3":"no","4":"79.7"},{"1":"Virginia","2":"3","3":"yes","4":"85.6"},{"1":"Washington","2":"10","3":"no","4":"86.2"},{"1":"West Virginia","2":"3","3":"no","4":"81.3"},{"1":"Wisconsin","2":"5","3":"no","4":"82.2"},{"1":"Wyoming","2":"8","3":"no","4":"71.1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

The following contingency table shows how the number of states with
vaccine requirements vary between regions.

``` r
#as_integer used to control order in table
cont_table_1 <- table(as.integer(hepatitis_a$HHS_geography), hepatitis_a$hep_a_required)

#label matrix dimensions more clearly
my_dimnames <- list (
    c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5", "Region 6", "Region 7", "Region 8", 
      "Region 9", "Region 10"),
    c("No Mandate", "Mandated in Some Areas", "Mandate"))

dimnames(cont_table_1) <- my_dimnames
```

``` r
#print table
knitr::kable(cont_table_1, caption = "Contingency Table for Regions and Mandates for Hepatitis A Vaccination")
```

|           | No Mandate | Mandated in Some Areas | Mandate |
|:----------|-----------:|-----------------------:|--------:|
| Region 1  |          4 |                      0 |       2 |
| Region 2  |          2 |                      0 |       0 |
| Region 3  |          3 |                      0 |       3 |
| Region 4  |          4 |                      0 |       4 |
| Region 5  |          3 |                      0 |       3 |
| Region 6  |          0 |                      0 |       5 |
| Region 7  |          3 |                      0 |       1 |
| Region 8  |          3 |                      0 |       3 |
| Region 9  |          1 |                      1 |       2 |
| Region 10 |          1 |                      0 |       3 |

Contingency Table for Regions and Mandates for Hepatitis A Vaccination

There is a great deal of variation between regions with regard to
mandated for vaccination against hepatitis A. While all states in Region
5 mandate vaccination (for childcare/secondary school enrollment), no
states in Region 2 do. A more detailed analysis was performed.

``` r
summary_table <-hepatitis_a %>% 
    group_by(hep_a_required,as.integer(HHS_geography)) %>%
    summarise(average_coverage_estimate = round(mean(coverage_estimate),1)) %>%
    pivot_wider(names_from = hep_a_required, values_from = average_coverage_estimate)

#Rename colnames for printing and for reference in arrange
colnames(summary_table) <- c("Region","No Mandate", "Mandated in Some Areas", "Mandate" )

#Arrange in Region order
summary_table <- summary_table %>% arrange(Region)

knitr::kable(summary_table,caption = "Coverage Estimates for Hepatitis A Vaccination")
```

| Region | No Mandate | Mandated in Some Areas | Mandate |
|-------:|-----------:|-----------------------:|--------:|
|      1 |       84.0 |                     NA |    86.8 |
|      2 |       81.1 |                     NA |      NA |
|      3 |       84.2 |                     NA |    85.5 |
|      4 |       75.4 |                     NA |    85.1 |
|      5 |       82.7 |                     NA |    88.5 |
|      6 |         NA |                     NA |    85.5 |
|      7 |       83.3 |                     NA |    83.9 |
|      8 |       80.3 |                     NA |    92.8 |
|      9 |       88.9 |                   85.9 |    89.8 |
|     10 |       86.2 |                     NA |    91.4 |

Coverage Estimates for Hepatitis A Vaccination

For each region, the averge value for estimated coverage is higher in
the states with a mandate than without. Returning to the data and
summarizing only based on the presence of a mandate yields the
following.

``` r
summary_table_short <- hepatitis_a %>% 
    group_by(hep_a_required) %>%
    summarise(average_coverage_estimate = round(mean(coverage_estimate),1)) %>%
    pivot_wider(names_from = hep_a_required, values_from = average_coverage_estimate)

#Rename colnames for printing
colnames(summary_table_short) <- c("No Mandate", "Mandated in Some Areas", "Mandate" )

knitr::kable(summary_table_short,caption="Coverage Estimates for Hepatitis A Vaccination as a Function of State Mandates")
```

| No Mandate | Mandated in Some Areas | Mandate |
|-----------:|-----------------------:|--------:|
|       81.9 |                   85.9 |    87.7 |

Coverage Estimates for Hepatitis A Vaccination as a Function of State
Mandates

This table shows that variation in whether states mandate vaccination
against heptatis A is a major source of the variable in coverage
estimates for this vaccine.

## Coverage for HPV Vaccines by State and Insurance

Vaccination against infection with human papillomavirus is mandated by
very few states/districts at this time [(Virginia, Hawaii, Rhode Island,
and District of Columbia)](https://www.immunize.org/laws/hpv.asp). An
analysis was performed to evaluate the extent of variablility in
coverage for HPV in the states/territories without a mandate. Function 2
from above, `HPV_gender_eval_function_state` was used setting the
`year_of_interest` to 2021. The data was filtered to include only
females and remove the data for the states with mandates. Data was then
analyzed using a boxplot.

``` r
#Create vector containing states with mandates
states_with_HPV_mandate <- c ('Virginia', 'Hawaii', 'Rhode Island', 'District of Columbia')

hpv_state <-  HPV_gender_eval_function_state(year_of_interest = "2021") %>% 
    filter(dose == "Up-to-Date, Females") %>%
    filter (!(geography %in% states_with_HPV_mandate)) %>%
    select(geography, coverage_estimate) %>%
    #recast variable to allow plotting
    mutate (coverage_estimate=as.numeric(coverage_estimate))   

#Make a boxplot.
graph_8 <- ggplot() +
           geom_boxplot(aes (y = hpv_state$coverage_estimate)) +
           labs(y = "Vaccination Coverage Estimate",
                title = "Figure 8. Variation in Coverage Estimate for HPV Infection",
                subtitle = "In States without Vaccination Requirements for Secondary School Attendance") +
           theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

graph_8
```

![](Project-2_files/figure-gfm/Vaccine%20Coverage%20for%20HPV-1.png)<!-- -->

There seems to be a good deal of variability in coverage for HPV among
the states without a vaccine mandate for HPV. The data was evaluated to
determine if there was a relationship between coverage estimates for the
state and percentage uninsured for the states. [Data for insurance
coverage](https://advisorsmith.com/data/the-most-and-least-uninsured-states-for-health-coverage/)
was used to divide the states into groups of ten. The ten states with
the highest uninsured percentage were assigned as group A. The next ten
as Group B, and so on. Similarly the states were ranked with regard to
coverage estimates with Group 1 being the the states with the lowest
vaccination coverage estimates.

``` r
#Set up vectors with insurance ranking data
insurance_group_a <- c('Texas', 'Oklahoma', 'Georgia', 'Florida',
                       'Mississippi', 'Wyoming', 'Alaska', 'Nevada', 'Arizona', 'North Carolina')
insurance_group_b <- c('Idaho', 'South Carolina', 'South Dakota', 'Tennessee', 'Missouri', 'New Mexico', 
                       'Alabama', 'Utah', 'Kansas', 'Arkansas')
insurance_group_c <- c('Louisiana', 'Indiana', 'Montana', 'Nebraska', 'Colorado', 'Maine', 'New Jersey', 
                       'Virginia', 'California', 'Illinois')
insurance_group_d <- c('Oregon', 'North Dakota', 'West Virginia', 'Delaware', 'Ohio', 'Washington', 
                       'Kentucky', 'New Hampshire', 'Maryland', 'Connecticut')
insurance_group_e <- c('Michigan', 'Pennsylvania', 'Wisconsin', 'New York', 'Iowa', 'Minnesota', 'Vermont', 
                      'Hawaii', 'Rhode Island', 'Massachusetts')

hpv_state_2 <- hpv_state %>% arrange(coverage_estimate) %>%
    filter (!(geography %in% c("U.S. Virgin Islands", "United States", "Puerto Rico", "Guam"))) %>%
    #Add variable for coverage estimate ranking
    mutate (coverage_rank_group = (trunc((row_number()-1)/10)+1))%>%
    #Add variable for ranking based on percent uninsured
    mutate(insurance_group = (if_else (geography %in% insurance_group_a, "A", 
                             (if_else (geography %in% insurance_group_b, "B", 
                             (if_else (geography %in% insurance_group_c, "C", 
                             (if_else (geography %in% insurance_group_d, "D", 
                             (if_else (geography %in% insurance_group_e, "E", "error")))))))))))
```

The dataset with rankings was explored using a contingency table.

``` r
cont_table_2 <- table(hpv_state_2$insurance_group, hpv_state_2$coverage_rank_group)

#make  meaningful labels for matrix output
my_dim_names_2 <- list(c('Ins Grp A-High % Unins', 'Ins Grp B', 'Ins Grp C', 
                          'Ins Grp D','Ins Grp E-Low % Unins'),
                       c("Grp 1-Low", "Group 2", "Group 3", "Group 4", "Grp 5-High")
                       )

dimnames(cont_table_2) <- my_dim_names_2

knitr::kable(cont_table_2, caption="Contingency Table for Ranking by %Uninsured and Coverage Estimate")
```

|                        | Grp 1-Low | Group 2 | Group 3 | Group 4 | Grp 5-High |
|:-----------------------|----------:|--------:|--------:|--------:|-----------:|
| Ins Grp A-High % Unins |         6 |       2 |       1 |       0 |          1 |
| Ins Grp B              |         2 |       4 |       2 |       0 |          2 |
| Ins Grp C              |         1 |       4 |       2 |       2 |          0 |
| Ins Grp D              |         1 |       0 |       3 |       4 |          2 |
| Ins Grp E-Low % Unins  |         0 |       0 |       2 |       4 |          2 |

Contingency Table for Ranking by %Uninsured and Coverage Estimate

There does appear to be a correlation between low coverage against HPV
infection in a state and its percent uninsured. Of the ten states with
the lowest vaccine coverage for HPV, six are in the lowest insurance
group and two more are in the second lowest. This was further
investigated by generating a summary table using the `insurance_group`
as a categorical variable.

``` r
summary_table_2 <-hpv_state_2 %>% 
    group_by(insurance_group) %>%
    summarise(average_coverage_estimate = round(mean(coverage_estimate),1)) 

#Rename colnames for printing 
colnames(summary_table_2) <- c("Insurance Group","Average Coverage Estimate")

knitr::kable(summary_table_2, caption="Summary Table for Average Coverage Extimate for HPV vs Percent Uninsured")
```

| Insurance Group | Average Coverage Estimate |
|:----------------|--------------------------:|
| A               |                      55.8 |
| B               |                      64.3 |
| C               |                      63.0 |
| D               |                      66.9 |
| E               |                      69.2 |

Summary Table for Average Coverage Extimate for HPV vs Percent Uninsured

There seems to be a relationship between the percent uninsured for a
state and its estimate for coverage against HPV infection. The group of
states with the highest percent uninsured of the population corresponds
to the lowest coverage rates. Similarly, the group of states with the
lowest percent uninsured of the population corresponds to the highest
coverage rates. The ordering for percent uninsured for a state was based
on the entire population; it would be interesting to do a similar
analysis using insurance information related only to children/teenagers
in the state.

## Summary of Data Analysis

The section of the database dealing with vaccination over a 4-year
period was queried and coverage estimates were compared for Tdap and
meningococcal conjugate. Coverage estimates for meningococcal conjugate
varied more than those for Tdap. While Tdap is mandated for enrollment
in secondary school in all states, the same is not true for
meningococcal conjugate. Coverage estimates for meningococcal conjugate
were compared for states with and without a mandate. Then the
variability in coverage in states without a mandate was analyzed as a
function of insurance coverage, poverty level, race/ethnicity, and
urbanicity. The data suggested that a lack of insurance led to lower
coverage estimates. Living in a non-metropolitan area also seems related
to lower rates of vaccine coverage.

Vaccine coverage estimates were compared across regions for vaccines.
The data for hepatitis A seemed the most variable. Its variability was
evaluated as a function of region and state mandates.

Coverage with regard to protection from HPV infection was evaluated for
correlation with percent uninsured for the states.

# Summary for Project

This project involved building functions to interact with data from the
Center for Disease Control relating to [Vaccination Coverage among
Adolescents](https://data.cdc.gov/Teen-Vaccinations/Vaccination-Coverage-among-Adolescents-13-17-Years/ee48-w5t6).
Five function were written:

- Function 1: Obtain regional data About differences in HPV vaccination
  coverage between genders; user can specify year  
- Function 2: Obtain state sata about differences in HPV vaccination
  coverage between genders; user can specify year  
- Function 3: Obtain regional data about vaccination coverage for
  meningococcal disease, tetanus, diptheria, pertussis, hepatitis A,
  measles, mumps, rubella, and hepatitis B; the user can specify a year
  and a vaccine  
- Function 4: Obtain state data about vaccination coverage for
  meningococcal disease, tetanus, diptheria, pertussis, hepatitis A,
  measles, mumps, rubella, and hepatitis B; the user can specify a year
  , a vaccine, and a state  
- Function 5: Obtain state data about vaccination data for the Tdap and
  meningococcal vaccine for the 2018-2022 time frame sorted by dimension
  (insurance coverage, poverty, race/ethnicity, urbanicity, and
  overall); the user may specify a vaccine and a dimension.

An exploratory data analysis was performed. This involved multiple
function calls, multiple contingency tables (for hepatitis A and HPV)
and multiple summary tables (for hepatitis A and HPV). Over the course
of the analysis several graphs were made. These included:

- boxplots across categorical variable  
- single boxplot (new)  
- violin plot (new)  
- scatter plot with line plot (new)  
- map (new).