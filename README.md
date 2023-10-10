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

![](C:\Users\Office\Desktop\NCSUFA~1\ST558-~1\README~1/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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
```

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

knitr::kable(demo_function_1a)
```

| geography     | cov_est_female | cov_est_male | Diff |
|:--------------|---------------:|-------------:|-----:|
| United States |           63.8 |         59.8 |  4.0 |
| Region 1      |           73.9 |         69.5 |  4.4 |
| Region 2      |           66.0 |         56.6 |  9.4 |
| Region 3      |           71.0 |         64.6 |  6.4 |
| Region 4      |           58.3 |         55.0 |  3.3 |
| Region 5      |           66.9 |         57.8 |  9.1 |
| Region 6      |           56.0 |         51.7 |  4.3 |
| Region 7      |           66.2 |         58.5 |  7.7 |
| Region 8      |           65.0 |         65.0 |  0.0 |
| Region 9      |           65.7 |         68.5 | -2.8 |
| Region 10     |           64.3 |         67.7 | -3.4 |

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
```

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
knitr::kable(demo_function_2a)
```

| geography            | cov_est_female | cov_est_male |  Diff |
|:---------------------|---------------:|-------------:|------:|
| Alabama              |           58.2 |         66.6 |  -8.4 |
| Alaska               |           60.5 |         52.1 |   8.4 |
| Arizona              |           63.1 |         60.3 |   2.8 |
| Arkansas             |           58.9 |         54.7 |   4.2 |
| California           |           67.0 |         70.9 |  -3.9 |
| Colorado             |           68.6 |         69.8 |  -1.2 |
| Connecticut          |           67.1 |         65.8 |   1.3 |
| Delaware             |           68.8 |         68.9 |  -0.1 |
| District of Columbia |           81.2 |         77.7 |   3.5 |
| Florida              |           49.1 |         48.8 |   0.3 |
| Georgia              |           66.6 |         55.4 |  11.2 |
| Guam                 |           52.9 |         54.9 |  -2.0 |
| Hawaii               |           69.0 |         69.6 |  -0.6 |
| Idaho                |           63.7 |         59.5 |   4.2 |
| Illinois             |           64.8 |         59.6 |   5.2 |
| Indiana              |           62.0 |         48.7 |  13.3 |
| Iowa                 |           70.3 |         62.0 |   8.3 |
| Kansas               |           70.6 |         58.3 |  12.3 |
| Kentucky             |           48.9 |         64.8 | -15.9 |
| Louisiana            |           60.7 |         67.1 |  -6.4 |
| Maine                |           67.8 |         55.5 |  12.3 |
| Maryland             |           73.8 |         70.2 |   3.6 |
| Massachusetts        |           78.1 |         72.0 |   6.1 |
| Michigan             |           68.9 |         60.7 |   8.2 |
| Minnesota            |           69.5 |         62.0 |   7.5 |
| Mississippi          |           32.6 |         32.8 |  -0.2 |
| Missouri             |           62.7 |         56.0 |   6.7 |
| Montana              |           49.5 |         55.5 |  -6.0 |
| Nebraska             |           63.2 |         60.7 |   2.5 |
| Nevada               |           54.9 |         58.0 |  -3.1 |
| New Hampshire        |           77.0 |         67.8 |   9.2 |
| New Jersey           |           63.0 |         47.0 |  16.0 |
| New Mexico           |           62.9 |         53.1 |   9.8 |
| New York             |           67.5 |         61.5 |   6.0 |
| North Carolina       |           72.8 |         62.9 |   9.9 |
| North Dakota         |           68.6 |         75.6 |  -7.0 |
| Ohio                 |           69.0 |         55.6 |  13.4 |
| Oklahoma             |           55.1 |         58.2 |  -3.1 |
| Oregon               |           63.3 |         70.6 |  -7.3 |
| Pennsylvania         |           68.2 |         69.2 |  -1.0 |
| Puerto Rico          |           68.6 |         65.9 |   2.7 |
| United States        |           63.8 |         59.8 |   4.0 |
| Rhode Island         |           80.1 |         86.3 |  -6.2 |
| South Carolina       |           61.0 |         63.2 |  -2.2 |
| South Dakota         |           78.3 |         71.3 |   7.0 |
| Tennessee            |           64.4 |         48.8 |  15.6 |
| Texas                |           54.8 |         48.3 |   6.5 |
| U.S. Virgin Islands  |           45.7 |         45.3 |   0.4 |
| Utah                 |           62.5 |         60.2 |   2.3 |
| Vermont              |           63.8 |         69.5 |  -5.7 |
| Virginia             |           73.5 |         56.7 |  16.8 |
| Washington           |           65.5 |         70.1 |  -4.6 |
| West Virginia        |           67.3 |         46.1 |  21.2 |
| Wisconsin            |           66.9 |         59.9 |   7.0 |
| Wyoming              |           48.7 |         47.4 |   1.3 |

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
knitr::kable(demo_function_3a)
```

| geography     | Meningococcal Conjugate | Td or Tdap | Tdap | Hep A |  MMR | HepB |
|:--------------|------------------------:|-----------:|-----:|------:|-----:|-----:|
| United States |                    89.0 |       92.2 | 89.6 |  85.0 | 92.2 | 92.3 |
| Region 1      |                    91.9 |       95.7 | 93.8 |  86.6 | 96.5 | 95.8 |
| Region 2      |                    93.2 |       90.7 | 87.9 |  81.5 | 92.8 | 94.6 |
| Region 3      |                    91.4 |       92.0 | 89.7 |  85.8 | 93.4 | 92.1 |
| Region 4      |                    84.9 |       93.5 | 91.0 |  81.9 | 95.1 | 95.1 |
| Region 5      |                    91.6 |       93.3 | 89.4 |  84.9 | 94.2 | 94.1 |
| Region 6      |                    89.9 |       90.7 | 88.1 |  84.6 | 86.6 | 86.5 |
| Region 7      |                    90.4 |       92.0 | 90.2 |  82.6 | 92.5 | 93.1 |
| Region 8      |                    88.2 |       93.1 | 91.6 |  89.3 | 94.8 | 93.7 |
| Region 9      |                    87.5 |       91.0 | 89.2 |  88.5 | 89.0 | 89.9 |
| Region 10     |                    84.6 |       89.4 | 86.9 |  89.4 | 92.3 | 91.2 |

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

knitr::kable(demo_function_4)
```

| vaccine                 | dose       | geography_type     | geography      | year_season | dimension_type | dimension   | coverage_estimate | \_95_ci      | population_sample_size |
|:------------------------|:-----------|:-------------------|:---------------|:------------|:---------------|:------------|------------------:|:-------------|:-----------------------|
| Meningococcal Conjugate | \>=1 Dose  | States/Local Areas | North Carolina | 2021        | Age            | 13-17 Years |              93.3 | 87.6 to 96.5 | 266                    |
| Td or Tdap              | \>=1 Dose  | States/Local Areas | North Carolina | 2021        | Age            | 13-17 Years |              95.5 | 89.9 to 98.1 | 266                    |
| Tdap                    | \>=1 Dose  | States/Local Areas | North Carolina | 2021        | Age            | 13-17 Years |              94.5 | 88.9 to 97.4 | 266                    |
| Hep A                   | \>=2 Doses | States/Local Areas | North Carolina | 2021        | Age            | 13-17 Years |              90.8 | 83.6 to 95.0 | 266                    |
| MMR                     | \>=2 Doses | States/Local Areas | North Carolina | 2021        | Age            | 13-17 Years |              95.3 | 88.8 to 98.1 | 266                    |
| HepB                    | \>=3 Doses | States/Local Areas | North Carolina | 2021        | Age            | 13-17 Years |              95.5 | 88.9 to 98.2 | 266                    |

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
```

Presentation of the results from this function call transformed into a
user-friendly table is given below.

``` r
demo_function_5a <- demo_function_5 %>%
    select(geography, dimension, coverage_estimate) %>%
    pivot_wider(names_from = dimension, values_from = coverage_estimate)

knitr::kable(demo_function_5a)
```

| geography            | Other | Private Insurance Only | Any Medicaid | Uninsured |
|:---------------------|------:|-----------------------:|-------------:|----------:|
| Alabama              |  76.7 |                   84.1 |         87.2 |        NA |
| Alaska               |  80.8 |                   73.5 |         76.1 |      67.4 |
| Arizona              |  83.5 |                   87.4 |         90.0 |      83.3 |
| Arkansas             |  94.8 |                   95.1 |         94.2 |      90.9 |
| California           |  84.0 |                   85.7 |         87.0 |        NA |
| Colorado             |  88.4 |                   86.8 |         84.8 |      74.5 |
| Connecticut          |  91.0 |                   95.3 |         91.8 |      82.3 |
| Delaware             |  89.8 |                   91.0 |         86.6 |      84.6 |
| District of Columbia |  95.5 |                   91.0 |         92.4 |        NA |
| Florida              |  74.7 |                   82.4 |         81.2 |      72.9 |
| Georgia              |  95.3 |                   95.1 |         94.0 |      88.3 |
| Hawaii               |  79.5 |                   89.9 |         82.9 |        NA |
| Idaho                |  82.5 |                   89.8 |         92.2 |      76.9 |
| Illinois             |  88.1 |                   94.4 |         90.8 |      85.5 |
| Indiana              |  85.9 |                   93.0 |         91.1 |      95.6 |
| Iowa                 |  95.2 |                   94.7 |         91.0 |        NA |
| Kansas               |  84.5 |                   82.2 |         85.5 |      63.2 |
| Kentucky             |  87.3 |                   91.3 |         88.3 |      76.3 |
| Louisiana            |  91.6 |                   90.6 |         89.8 |      93.8 |
| Maine                |  82.8 |                   91.1 |         90.8 |      71.2 |
| Maryland             |  94.1 |                   93.9 |         92.2 |      89.4 |
| Massachusetts        |  96.0 |                   94.2 |         96.1 |        NA |
| Michigan             |  93.4 |                   94.4 |         92.7 |      85.8 |
| Minnesota            |  78.8 |                   92.1 |         92.0 |        NA |
| Mississippi          |  61.1 |                   57.1 |         63.8 |      55.4 |
| Missouri             |  84.1 |                   88.3 |         82.5 |      78.0 |
| Montana              |  80.6 |                   79.7 |         73.8 |        NA |
| Nebraska             |  98.5 |                   86.7 |         84.8 |      93.5 |
| Nevada               |  86.1 |                   86.4 |         85.5 |      86.1 |
| New Hampshire        |  88.3 |                   90.7 |         83.9 |      74.5 |
| New Jersey           |  91.3 |                   93.0 |         90.8 |      92.7 |
| New Mexico           |  87.3 |                   83.4 |         83.9 |      68.1 |
| New York             |  95.7 |                   95.4 |         93.9 |      82.9 |
| North Carolina       |  89.5 |                   92.3 |         92.2 |      90.5 |
| North Dakota         |  95.9 |                   95.6 |         93.4 |      82.4 |
| Ohio                 |  78.6 |                   92.5 |         89.3 |        NA |
| Oklahoma             |  78.1 |                   72.4 |         84.9 |      74.9 |
| Oregon               |  80.7 |                   84.6 |         84.0 |        NA |
| Pennsylvania         |  90.4 |                   95.1 |         94.0 |      88.8 |
| United States        |  86.6 |                   89.5 |         88.3 |      79.9 |
| Rhode Island         |  91.5 |                   97.7 |         95.2 |      91.7 |
| South Carolina       |  83.5 |                   83.5 |         84.2 |      68.6 |
| South Dakota         |  91.8 |                   91.2 |         93.0 |      75.3 |
| Tennessee            |  76.3 |                   83.5 |         87.7 |      71.5 |
| Texas                |  89.3 |                   88.6 |         88.2 |      83.2 |
| Utah                 |  91.4 |                   89.2 |         90.4 |      90.5 |
| Vermont              |  95.2 |                   91.2 |         90.2 |        NA |
| Virginia             |  79.7 |                   87.0 |         81.8 |      82.8 |
| Washington           |  83.0 |                   88.3 |         85.1 |        NA |
| West Virginia        |  96.1 |                   91.1 |         90.9 |      85.7 |
| Wisconsin            |  68.6 |                   91.2 |         90.5 |        NA |
| Wyoming              |  76.4 |                   69.6 |         79.5 |      64.1 |

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

knitr::kable(coverage_comparison_1)
```

| geography            | Meningococcal_conjugate | Tdap |
|:---------------------|------------------------:|-----:|
| Alabama              |                    84.4 | 91.6 |
| Alaska               |                    75.7 | 82.4 |
| Arizona              |                    87.8 | 86.8 |
| Arkansas             |                    94.5 | 93.3 |
| California           |                    85.7 | 87.5 |
| Colorado             |                    86.0 | 90.8 |
| Connecticut          |                    93.6 | 94.5 |
| Delaware             |                    89.0 | 88.4 |
| District of Columbia |                    91.5 | 87.1 |
| Florida              |                    80.7 | 91.5 |
| Georgia              |                    94.4 | 92.2 |
| Hawaii               |                    85.8 | 86.4 |
| Idaho                |                    89.8 | 88.2 |
| Illinois             |                    92.7 | 91.3 |
| Indiana              |                    92.2 | 91.6 |
| Iowa                 |                    93.3 | 93.9 |
| Kansas               |                    82.3 | 89.5 |
| Kentucky             |                    89.5 | 87.5 |
| Louisiana            |                    90.2 | 92.9 |
| Maine                |                    90.1 | 91.0 |
| Maryland             |                    93.3 | 89.7 |
| Massachusetts        |                    94.6 | 93.7 |
| Michigan             |                    93.6 | 90.6 |
| Minnesota            |                    91.3 | 91.6 |
| Mississippi          |                    60.6 | 89.9 |
| Missouri             |                    85.8 | 85.4 |
| Montana              |                    76.7 | 88.5 |
| Nebraska             |                    87.1 | 90.8 |
| Nevada               |                    86.1 | 88.3 |
| New Hampshire        |                    88.4 | 94.8 |
| New Jersey           |                    92.3 | 89.3 |
| New Mexico           |                    83.5 | 87.4 |
| New York             |                    94.8 | 91.2 |
| North Carolina       |                    92.0 | 91.9 |
| North Dakota         |                    94.8 | 93.4 |
| Ohio                 |                    90.6 | 91.8 |
| Oklahoma             |                    79.1 | 86.8 |
| Oregon               |                    83.6 | 88.0 |
| Pennsylvania         |                    94.2 | 92.8 |
| United States        |                    88.5 | 89.7 |
| Rhode Island         |                    96.4 | 95.2 |
| South Carolina       |                    83.3 | 91.0 |
| South Dakota         |                    91.2 | 91.5 |
| Tennessee            |                    83.7 | 90.2 |
| Texas                |                    88.0 | 84.9 |
| Utah                 |                    89.6 | 91.0 |
| Vermont              |                    90.9 | 94.2 |
| Virginia             |                    84.7 | 89.7 |
| Washington           |                    86.2 | 89.1 |
| West Virginia        |                    91.2 | 90.1 |
| Wisconsin            |                    89.7 | 90.6 |
| Wyoming              |                    72.4 | 89.3 |

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

knitr::kable(meningococcal_regulation_analysis)
```

| geography            | mening_req | Meningococcal_conjugate |
|:---------------------|:-----------|------------------------:|
| Alabama              | No         |                    84.4 |
| Alaska               | No         |                    75.7 |
| Arizona              | Yes        |                    87.8 |
| Arkansas             | Yes        |                    94.5 |
| California           | No         |                    85.7 |
| Colorado             | No         |                    86.0 |
| Connecticut          | Yes        |                    93.6 |
| Delaware             | Yes        |                    89.0 |
| District of Columbia | Yes        |                    91.5 |
| Florida              | No         |                    80.7 |
| Georgia              | Yes        |                    94.4 |
| Hawaii               | Yes        |                    85.8 |
| Idaho                | Yes        |                    89.8 |
| Illinois             | Yes        |                    92.7 |
| Indiana              | Yes        |                    92.2 |
| Iowa                 | Yes        |                    93.3 |
| Kansas               | Yes        |                    82.3 |
| Kentucky             | Yes        |                    89.5 |
| Louisiana            | Yes        |                    90.2 |
| Maine                | Yes        |                    90.1 |
| Maryland             | Yes        |                    93.3 |
| Massachusetts        | Yes        |                    94.6 |
| Michigan             | Yes        |                    93.6 |
| Minnesota            | Yes        |                    91.3 |
| Mississippi          | No         |                    60.6 |
| Missouri             | Yes        |                    85.8 |
| Montana              | No         |                    76.7 |
| Nebraska             | No         |                    87.1 |
| Nevada               | Yes        |                    86.1 |
| New Hampshire        | No         |                    88.4 |
| New Jersey           | Yes        |                    92.3 |
| New Mexico           | Yes        |                    83.5 |
| New York             | Yes        |                    94.8 |
| North Carolina       | Yes        |                    92.0 |
| North Dakota         | Yes        |                    94.8 |
| Ohio                 | Yes        |                    90.6 |
| Oklahoma             | No         |                    79.1 |
| Oregon               | No         |                    83.6 |
| Pennsylvania         | Yes        |                    94.2 |
| Rhode Island         | Yes        |                    96.4 |
| South Carolina       | No         |                    83.3 |
| South Dakota         | Yes        |                    91.2 |
| Tennessee            | No         |                    83.7 |
| Texas                | Yes        |                    88.0 |
| Utah                 | Yes        |                    89.6 |
| Vermont              | Yes        |                    90.9 |
| Virginia             | Yes        |                    84.7 |
| Washington           | No         |                    86.2 |
| West Virginia        | Yes        |                    91.2 |
| Wisconsin            | Yes        |                    89.7 |
| Wyoming              | No         |                    72.4 |

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

![](C:\Users\Office\Desktop\NCSUFA~1\ST558-~1\README~1/figure-gfm/violin_plot-1.png)<!-- -->

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

![](C:\Users\Office\Desktop\NCSUFA~1\ST558-~1\README~1/figure-gfm/compare_menigococcal_using_insurance-1.png)<!-- -->

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

![](C:\Users\Office\Desktop\NCSUFA~1\ST558-~1\README~1/figure-gfm/compare_menigococcal_using_different_dimensions-1.png)<!-- -->

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
          subtitle = "with Race/Ethnicity in States without Vaccination Requirements for Secondary School Attendance")
```

![](C:\Users\Office\Desktop\NCSUFA~1\ST558-~1\README~1/figure-gfm/compare_menigococcal_using_race/ethnicity-1.png)<!-- -->

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
          subtitle = "with Urbanicity in States without Vaccination Requirements for Secondary School Attendance")
```

![](C:\Users\Office\Desktop\NCSUFA~1\ST558-~1\README~1/figure-gfm/compare_menigococcal_using_urbanicity-1.png)<!-- -->

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

![](C:\Users\Office\Desktop\NCSUFA~1\ST558-~1\README~1/figure-gfm/Vaccine_Coverage_by_Region-1.png)<!-- -->

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
    
knitr::kable(hepatitis_a)
```

| geography            | HHS_geography | hep_a_required | coverage_estimate |
|:---------------------|:--------------|:---------------|------------------:|
| Alabama              | 4             | no             |              83.0 |
| Alaska               | 10            | yes            |              89.0 |
| Arizona              | 9             | some           |              85.9 |
| Arkansas             | 6             | yes            |              84.2 |
| California           | 9             | no             |              88.9 |
| Colorado             | 8             | no             |              88.4 |
| Connecticut          | 1             | yes            |              87.6 |
| Delaware             | 3             | no             |              85.7 |
| District of Columbia | 3             | yes            |              84.2 |
| Florida              | 4             | no             |              75.4 |
| Georgia              | 4             | yes            |              88.9 |
| Hawaii               | 9             | yes            |              89.5 |
| Idaho                | 10            | yes            |              90.4 |
| Illinois             | 5             | no             |              81.2 |
| Indiana              | 5             | yes            |              90.6 |
| Iowa                 | 7             | no             |              86.5 |
| Kansas               | 7             | yes            |              83.9 |
| Kentucky             | 4             | yes            |              91.5 |
| Louisiana            | 6             | yes            |              79.7 |
| Maine                | 1             | no             |              84.5 |
| Maryland             | 3             | no             |              85.6 |
| Massachusetts        | 1             | no             |              87.6 |
| Michigan             | 5             | no             |              84.6 |
| Minnesota            | 5             | yes            |              91.2 |
| Mississippi          | 4             | no             |              52.5 |
| Missouri             | 7             | no             |              79.6 |
| Montana              | 8             | no             |              81.4 |
| Nebraska             | 7             | no             |              83.8 |
| Nevada               | 9             | yes            |              90.1 |
| New Hampshire        | 1             | no             |              84.1 |
| New Jersey           | 2             | no             |              80.0 |
| New Mexico           | 6             | yes            |              87.8 |
| New York             | 2             | no             |              82.2 |
| North Carolina       | 4             | no             |              90.8 |
| North Dakota         | 8             | yes            |              96.9 |
| Ohio                 | 5             | yes            |              83.8 |
| Oklahoma             | 6             | yes            |              91.4 |
| Oregon               | 10            | yes            |              94.8 |
| Pennsylvania         | 3             | yes            |              86.7 |
| Rhode Island         | 1             | yes            |              85.9 |
| South Carolina       | 4             | yes            |              68.4 |
| South Dakota         | 8             | yes            |              87.2 |
| Tennessee            | 4             | yes            |              91.4 |
| Texas                | 6             | yes            |              84.3 |
| Utah                 | 8             | yes            |              94.3 |
| Vermont              | 1             | no             |              79.7 |
| Virginia             | 3             | yes            |              85.6 |
| Washington           | 10            | no             |              86.2 |
| West Virginia        | 3             | no             |              81.3 |
| Wisconsin            | 5             | no             |              82.2 |
| Wyoming              | 8             | no             |              71.1 |

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

![](C:\Users\Office\Desktop\NCSUFA~1\ST558-~1\README~1/figure-gfm/Vaccine_Coverage_for_HPV-1.png)<!-- -->

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
