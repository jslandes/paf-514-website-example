---
layout: page
title: "Text/Sentiment Analysis"
output:
  md_document:
    variant: markdown_github
    preserve_yaml: true
---

This code through seeks to demonstrate how R could be used to analyze
open-ended survey responses from the Quality of Life Survey. It
specifically demonstrates the following:

-   How to use the vader package to calculate a VADER score (sentiment
    analysis)
-   How to use quanteda packages to identify frequently used words and
    compound words
-   How to create a packed bubble chart using word frequency and VADER
    scores

# Setup

We’ll start with 2 sets of data. `dat` comes from IRS Form 1023-EZ, an
application for tax exemption. You can find the data on the [IRS
website](https://www.irs.gov/charities-non-profits/exempt-organizations-form-1023-ez-approvals).
`ntees` is a list of IRS activity codes. The data is pulled from
Deondre’ Jones’ work on the [National Taxonomy of Exempt Entities (NTEE)
Codes
project](https://urbaninstitute.github.io/nccs-legacy/ntee/ntee.html).

``` r
# load packages
library(readr)
library(dplyr)
library(vader)
library(quanteda)
library(quanteda.textstats)
library(stringr)
library(packcircles)
library(ggplot2)
library(viridis)
```

``` r
# read in the data
dat <- read_csv("C:/Users/jcrav/Documents/Education/PAF 514/Module 7/Code Through/irs-2024.csv")
ntees <- read_csv("C:/Users/jcrav/Documents/Education/PAF 514/Module 7/Code Through/ntee.csv")
```

``` r
# prep list of zips for testing
spocozips <- c(99208, 99205, 99206, 99223, 99207, 99216, 99224, 99217, 
               99004, 99202, 99203, 99016, 99212, 99218, 99006, 99037, 
               99201, 99019, 99005, 99022, 99156, 99021, 99026, 99204, 
               99001, 99025, 99027, 99003, 99009, 99011, 99029, 99036, 
               99030, 99013, 99170, 99023, 99012, 99031, 99033, 99018, 
               99252, 99260, 99299, 99015, 99258, 99014, 99020, 99039, 
               99210, 99209, 99211, 99214, 99213, 99215, 99220, 99219, 
               99251, 99228, 99256)

# clean up
dat <- dat %>% 
  filter(State == "WA",
         Benefitofcollege == 0) %>% 
  select(Orgname1, Orgname2, City, State, Zip, Mission, Nteecode, 
         Orgpurposecharitable, Orgpurposereligious,
         Orgpurposeeducational, Orgpurposescientific,
         Orgpurposeliterary, Orgpurposepublicsafety,
         Orgpurposeamateursports, Orgpurposecrueltyprevention,
         Qualifyforexemption, Leginflno, Leginflyes) %>%
  mutate(spoco.yes = ifelse(Zip %in% spocozips, 1, 0))

dat <- dat[!duplicated(dat), ]

dat <- dat[-198, ] # this line causes an error in the VADER score

head(dat)
```

    ## # A tibble: 6 × 19
    ##   Orgname1      Orgname2 City  State Zip   Mission Nteecode Orgpurposecharitable
    ##   <chr>         <chr>    <chr> <chr> <chr> <chr>   <chr>                   <dbl>
    ## 1 WASHINGTON T… <NA>     RENT… WA    98055 The pu… N66                         0
    ## 2 FUR FRIENDS … <NA>     SELAH WA    98942 We are… D20                         0
    ## 3 CLALLAM BAY … <NA>     SEKIU WA    98326 Operat… S41                         1
    ## 4 GAISMA THEAT… <NA>     SEAT… WA    98144 To sup… A65                         0
    ## 5 THE TRANSGEN… CENTER … BELL… WA    98225 The Tr… P88                         1
    ## 6 MT ERIE FIRE… <NA>     ANAC… WA    98221 The pu… S80                         1
    ## # ℹ 11 more variables: Orgpurposereligious <dbl>, Orgpurposeeducational <dbl>,
    ## #   Orgpurposescientific <dbl>, Orgpurposeliterary <dbl>,
    ## #   Orgpurposepublicsafety <dbl>, Orgpurposeamateursports <dbl>,
    ## #   Orgpurposecrueltyprevention <dbl>, Qualifyforexemption <dbl>,
    ## #   Leginflno <dbl>, Leginflyes <dbl>, spoco.yes <dbl>

The IRS data lists the NTEE code for each organization, but we don’t
want to look a code up each time we need it. Instead, we’ll join the
data frames to create 2 new columns.

``` r
# 1st join to add NTEE subcategory names and descriptions
ntees <- ntees %>% 
  rename(Nteecode = Code,
         ntee.cat2 = Name, 
         ntee.desc = Definition)

dat <- merge(dat, ntees, by = "Nteecode", all.x = TRUE)

# 2nd join to add NTEE category names
dat <- dat %>% 
  mutate(ntee.catcode = substr(Nteecode, 1, 1))

ntees2 <- ntees %>% 
  select(Nteecode, ntee.cat2) %>% 
  filter(!grepl("\\d", Nteecode)) %>% 
  rename(ntee.catcode = Nteecode,
         ntee.cat1 = ntee.cat2)

dat <- merge(dat, ntees2, by = "ntee.catcode", all.x = TRUE)

rm(ntees, ntees2, spocozips)
```

Organization names are split into two fields. We need to join these
fields, but there’s no perfect way to do this. Some rows need a
`sep = " "` while others need a `sep = ""`. We won’t use a separator to
avoid making it a complex process. We’re not doing anything with the
names, so we don’t need it to be perfect. `dat$Orgname2` contains `NA`
for blank columns. When we join the data frames, we have to make sure
the code doesn’t append `NA` to existing `dat$Orgname1` text.

``` r
dat$orgname <- ifelse(!is.na(dat$Orgname2), 
                           paste(dat$Orgname1, dat$Orgname2, sep = ""), 
                           dat$Orgname1)

dat <- dat[, !(names(dat) %in% c("Orgname1", "Orgname2"))]
```

# VADER Scoring

ChatGPT is going to help explain VADER scores, but its answer has been
vetted using the resources below.

> VADER stands for Valence Aware Dictionary and sEntiment Reasoner, and
> it’s a tool used to measure how positive, negative, or neutral a piece
> of text is. It’s often used in social media, product reviews, or
> anywhere people write short, casual comments. VADER gives each piece
> of text a sentiment score based on words used and their context.

The `vader` package calculates the following scores, which all range
from 0 to 1:

-   `pos`: positive score
-   `neg`: negative score
-   `neu`: neutral score
-   `compound`: compound score (final VADER score)

We’ll use the three classes from Geeks for Geeks to interpret the
compound score.

-   `> 0.05`: Positive sentiment
-   `< -0.05`: Negative sentiment
-   `-0.05 to 0.05`: Neutral sentiment

<strong>VADER Score Resources</strong>:To better understand how VADER is
calculated, read [Vader: A Comprehensive Guide to Sentiment Analysis in
Python](https://medium.com/@rslavanyageetha/vader-a-comprehensive-guide-to-sentiment-analysis-in-python-c4f1868b0d2e)
by Lavanya Geetha and [Sentiment Analysis using VADER – Using
Python](https://www.geeksforgeeks.org/python-sentiment-analysis-using-vader/)
by Geeks for Geeks.

General set up is done, but we need some additional work before we’re
ready to create VADER scores. The `vader_df` function will produce a new
data frame that contains the mission statements it scores. We’ll need to
join the new data frame with `dat` using the mission statements as the
unique key. However, some organizations have exactly the same mission
but are unique organizations. (Think about a national nonprofit with
regional branches or kids sport leagues across a state.) We don’t want
to remove the duplicates, so we must make them unique to ensure the join
will work.

``` r
# calculate the number of duplicate missions
duplicates.before <- dat %>%
  count(Mission) %>%
  filter(n > 1)

print(duplicates.before)
```

    ##                                                                                                                                                                                                                       Mission
    ## 1 Organized and operated for the exclusive purposes of granting money to various public charitable organizations. The foundation will focus on organizations that aid the public in literacy, education, science, and health.
    ## 2        We are an organization of trumpeters, formed to promote communication among trumpet players across the state and to improve the artistic level of performance, teaching, and literature associated with the trumpet.
    ##   n
    ## 1 2
    ## 2 2

``` r
# make them unique
dat$Mission <- make.unique(as.character(dat$Mission))

# double-check that duplicates are gone
duplicates.after <- dat %>%
  count(Mission) %>%
  filter(n > 1)

print(duplicates.after)
```

    ## [1] Mission n      
    ## <0 rows> (or 0-length row.names)

``` r
# clean up the global environment
rm(duplicates.after, duplicates.before)
```

We’ve cleaned up our data. It’s important to note that we would normally
tidy our text data by making everything lower case and removing stop
words, but these things affect VADER scores. That’s why we’re doing the
sentiment analysis before the general text analysis. Using the `vader`
package is easy. Simply use `vader_df()` and specify your arguments.
There are only four. See comments in the code chunk for details.

<strong>NOTE</strong>: Calculating the VADER score takes several
minutes.

``` r
wa.vader <- vader_df(dat$Mission,  # what it should score
                     incl_nt = T,  # includes unusual instances of n't like yesn't
                     neu_set = T,  # includes neutral score
                     rm_qm = F)    # does not remove quotation marks

head(wa.vader)
```

    ##                                                                                                                                                                                                                                                       text
    ## 1 we are Community helping ourselves to stay connected, build trusted among ourselves, help another to build family of understands, Art and Cultural Events, Creating Events for Volunteering programs, giving hope to drop off kids, family's connection,
    ## 2                                           To create lasting solutions and assistance to poverty, hunger, social injustice, mental health, homelessness, climate adaptations, health equity to individuals, families and communities that is underserved.
    ## 3                                                            The mission of the Srikandi Initiative is to leverage digital technology, research, and education to sustain cultural traditions, promote diversity, and advance healthy, informed societies.
    ## 4                                                                        MISSION STATEMENT The Vithiya Foundation (THE FOUNDATION) translates and localizes high-quality popular culture from the anglophone world into Persian and for Iranian audiences.
    ## 5     Allegro Dance Boosters exists to encourage youth participation in dance. Dancers volunteer to perform in the community, we provide financial assistance for youth to participate in dance programs, and host community-building events for families.
    ## 6                                                                                                                                                                          We share donated clothing with residents at the Concrete schools and community.
    ##                                                                                                                   word_scores
    ## 1 {0, 0, 0, 1.2, 0, 0, 0, 0, 0, 2.1, 0, 0, 1.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.2, 0, 0, 0, 0, 1.4, 1.9, 0, -1.1, 0, 0, 0, 0}
    ## 2                                   {0, 1.1, 0, 0.7, 0, 0, 0, -2.3, -1, 0, -2.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    ## 3                                             {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.6, 0, 0, 0, 1.7, 0, 0}
    ## 4                                                     {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    ## 5                    {0, 0, 0, 0, 0, 2.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    ## 6                                                                                      {0, 1.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    ##   compound   pos   neu   neg but_count
    ## 1    0.908 0.333 0.622 0.045         0
    ## 2   -0.735 0.112 0.621 0.266         0
    ## 3    0.649 0.187 0.813 0.000         0
    ## 4    0.421 0.113 0.887 0.000         0
    ## 5    0.511 0.091 0.909 0.000         0
    ## 6    0.296 0.167 0.833 0.000         0

We have our scores! We need to join `dat` and `wa.vader` and create a
new data frame with only local county data. This is where we use
`dat$spocoyes`.

``` r
wa.vader <- wa.vader %>% 
  rename(Mission = text)

dat <- merge(dat, wa.vader, by = "Mission", all.x = TRUE)

rm(wa.vader, incl_nt, neu_set)

local.dat <- dat %>% 
  filter(spoco.yes == 1)
```

# Early Observations

It’s time to explore the data.

``` r
str(local.dat)
```

    ## 'data.frame':    139 obs. of  28 variables:
    ##  $ Mission                    : chr  "1. Provide a social organization for all those interested in the restoration, modification, and promotion of an"| __truncated__ "A nonprofit community sports program organized and operated for the purpose of youth athletics, baseball and so"| __truncated__ "A. Provide financial and physical assistance for destitute families, women, children, and the elderly living ex"| __truncated__ "ANIMAL RESCUE AND REHABILITATION CENTER" ...
    ##  $ ntee.catcode               : chr  "N" "N" "Q" "D" ...
    ##  $ Nteecode                   : chr  "N50" "N63" "Q33" "D20" ...
    ##  $ City                       : chr  "SPOKANE" "SPOKANE" "SPOKANE" "SPOKANE VALLEY" ...
    ##  $ State                      : chr  "WA" "WA" "WA" "WA" ...
    ##  $ Zip                        : chr  "99218" "99208" "99208" "99216" ...
    ##  $ Orgpurposecharitable       : num  1 0 1 0 1 1 1 1 0 1 ...
    ##  $ Orgpurposereligious        : num  0 0 1 0 0 0 0 0 1 0 ...
    ##  $ Orgpurposeeducational      : num  0 0 1 0 1 0 0 0 0 0 ...
    ##  $ Orgpurposescientific       : num  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ Orgpurposeliterary         : num  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ Orgpurposepublicsafety     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Orgpurposeamateursports    : num  0 1 0 0 0 0 0 0 0 0 ...
    ##  $ Orgpurposecrueltyprevention: num  0 0 0 1 0 0 0 0 0 0 ...
    ##  $ Qualifyforexemption        : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Leginflno                  : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Leginflyes                 : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ spoco.yes                  : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ntee.cat2                  : chr  "Recreational Clubs" "Baseball & Softball" "International Relief" "Animal Protection & Welfare" ...
    ##  $ ntee.desc                  : chr  "Organizations that make available to members and their guests and facilities for recreational activities, sport"| __truncated__ "Organizations such as Little Leagues that provide opportunities for people to learn and become competitive in b"| __truncated__ "Organizations that work to relieve poverty in developing countries by providing funds, technical assistance and"| __truncated__ "Organizations such as animal shelters that provide for the humane care, protection and control of animals and w"| __truncated__ ...
    ##  $ ntee.cat1                  : chr  "Recreation & Sports" "Recreation & Sports" "International, Foreign Affairs & National Security" "Animal-Related" ...
    ##  $ orgname                    : chr  "INLAND NORTHWEST BRONCO CLUB" "NORTH SPOKANE LL" "BASKETS OF LOVE AMERICA" "PAWS FOR LOVE" ...
    ##  $ word_scores                : chr  "{0, 0, 0, 0, 0, 0, 0, 0, 1.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}" "{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.7, 0, 0, 0, 0, 0, 0, 0, 0}" "{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}" "{0, 2.3, 0, 0, 0}" ...
    ##  $ compound                   : num  0.402 0.402 0 0.511 0.421 0.681 0.681 0.939 0.612 0.896 ...
    ##  $ pos                        : num  0.119 0.078 0 0.452 0.08 0.145 0.309 0.39 0.132 0.335 ...
    ##  $ neu                        : num  0.881 0.922 1 0.548 0.92 0.855 0.571 0.61 0.868 0.571 ...
    ##  $ neg                        : num  0 0 0 0 0 0 0.12 0 0 0.094 ...
    ##  $ but_count                  : num  0 0 0 0 0 0 0 0 0 0 ...

There are no surprises here; it’s all number and character fields, but
it’s good to know everything looks the way it should.

``` r
summary(local.dat)
```

    ##    Mission          ntee.catcode         Nteecode             City          
    ##  Length:139         Length:139         Length:139         Length:139        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##     State               Zip            Orgpurposecharitable Orgpurposereligious
    ##  Length:139         Length:139         Min.   :0.0000       Min.   :0.00000    
    ##  Class :character   Class :character   1st Qu.:1.0000       1st Qu.:0.00000    
    ##  Mode  :character   Mode  :character   Median :1.0000       Median :0.00000    
    ##                                        Mean   :0.7626       Mean   :0.08633    
    ##                                        3rd Qu.:1.0000       3rd Qu.:0.00000    
    ##                                        Max.   :1.0000       Max.   :1.00000    
    ##  Orgpurposeeducational Orgpurposescientific Orgpurposeliterary
    ##  Min.   :0.0000        Min.   :0.00000      Min.   :0.00000   
    ##  1st Qu.:0.0000        1st Qu.:0.00000      1st Qu.:0.00000   
    ##  Median :0.0000        Median :0.00000      Median :0.00000   
    ##  Mean   :0.4676        Mean   :0.05755      Mean   :0.05755   
    ##  3rd Qu.:1.0000        3rd Qu.:0.00000      3rd Qu.:0.00000   
    ##  Max.   :1.0000        Max.   :1.00000      Max.   :1.00000   
    ##  Orgpurposepublicsafety Orgpurposeamateursports Orgpurposecrueltyprevention
    ##  Min.   :0.00000        Min.   :0.0000          Min.   :0.00000            
    ##  1st Qu.:0.00000        1st Qu.:0.0000          1st Qu.:0.00000            
    ##  Median :0.00000        Median :0.0000          Median :0.00000            
    ##  Mean   :0.02158        Mean   :0.1007          Mean   :0.07914            
    ##  3rd Qu.:0.00000        3rd Qu.:0.0000          3rd Qu.:0.00000            
    ##  Max.   :1.00000        Max.   :1.0000          Max.   :1.00000            
    ##  Qualifyforexemption   Leginflno        Leginflyes         spoco.yes
    ##  Min.   :1           Min.   :0.0000   Min.   :0.000000   Min.   :1  
    ##  1st Qu.:1           1st Qu.:1.0000   1st Qu.:0.000000   1st Qu.:1  
    ##  Median :1           Median :1.0000   Median :0.000000   Median :1  
    ##  Mean   :1           Mean   :0.9928   Mean   :0.007194   Mean   :1  
    ##  3rd Qu.:1           3rd Qu.:1.0000   3rd Qu.:0.000000   3rd Qu.:1  
    ##  Max.   :1           Max.   :1.0000   Max.   :1.000000   Max.   :1  
    ##   ntee.cat2          ntee.desc          ntee.cat1           orgname         
    ##  Length:139         Length:139         Length:139         Length:139        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  word_scores           compound            pos              neu        
    ##  Length:139         Min.   :-0.9500   Min.   :0.0000   Min.   :0.2690  
    ##  Class :character   1st Qu.: 0.3180   1st Qu.:0.0825   1st Qu.:0.6920  
    ##  Mode  :character   Median : 0.5110   Median :0.1670   Median :0.7970  
    ##                     Mean   : 0.4686   Mean   :0.1803   Mean   :0.7904  
    ##                     3rd Qu.: 0.8180   3rd Qu.:0.2775   3rd Qu.:0.8970  
    ##                     Max.   : 0.9640   Max.   :0.4740   Max.   :1.0000  
    ##       neg            but_count      
    ##  Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.00000  
    ##  Mean   :0.02928   Mean   :0.02158  
    ##  3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :0.35900   Max.   :1.00000

Purpose notes

-   Religious mean is lower than I expected
-   Scientific and literary means are the same
-   Public safety has the smallest mean

Legislative influence notes

-   Very few nonprofits try to influence legislation
-   Columns might not be useful - explore more

Vader scores notes

-   Compound: most scores are positive with some outliers
-   Positive: summary stats are lower than I expected
-   Neutral: way higher than anticipated
-   Negative: lower than I thought; expected more issue based language

VADER is not meant for this kind of data, but it was still interesting
to use it. Let’s do some more exploration.

``` r
table(local.dat$ntee.cat1)
```

    ## 
    ##                                      Animal-Related 
    ##                                                  10 
    ##                          Arts, Culture & Humanities 
    ##                                                  14 
    ##              Civil Rights, Social Action & Advocacy 
    ##                                                   5 
    ##           Community Improvement & Capacity Building 
    ##                                                   1 
    ##                               Crime & Legal-Related 
    ##                                                   2 
    ##                                           Education 
    ##                                                  22 
    ##                                         Environment 
    ##                                                   3 
    ##                       Food, Agriculture & Nutrition 
    ##                                                   2 
    ##                                         Health Care 
    ##                                                   5 
    ##                                   Housing & Shelter 
    ##                                                   4 
    ##                                      Human Services 
    ##                                                  15 
    ##  International, Foreign Affairs & National Security 
    ##                                                   2 
    ##                 Mental Health & Crisis Intervention 
    ##                                                   7 
    ## Philanthropy, Voluntarism & Grantmaking Foundations 
    ##                                                   4 
    ##                           Public & Societal Benefit 
    ##                                                   6 
    ##       Public Safety, Disaster Preparedness & Relief 
    ##                                                   3 
    ##                                 Recreation & Sports 
    ##                                                  22 
    ##                                    Religion-Related 
    ##                                                   6 
    ##                                Science & Technology 
    ##                                                   1 
    ##                                   Youth Development 
    ##                                                   5

Education and Recreation & Sports tied for 1st place, followed by Human
Services then Arts, Culture, & Humanities.

``` r
table(local.dat$City)
```

    ## 
    ##  AIRWAY HEIGHTS       CHATTAROY          CHENEY         COLBERT       DEER PARK 
    ##               2               2               3               3               2 
    ##             ELK   FAIRCHILD AFB       FAIRFIELD      GREENACRES         LIBERTY 
    ##               1               2               1               1               1 
    ##    LIBERTY LAKE            MEAD    MEDICAL LAKE         NEWPORT NINE MILE FALLS 
    ##               4               2               2               4               2 
    ##   OTIS ORCHARDS         SPOKANE  SPOKANE VALLEY           TEKOA 
    ##               1              89              16               1

Most organizations are in Spokane and Spokane Valley.

``` r
table(local.dat$Leginflyes)
```

    ## 
    ##   0   1 
    ## 138   1

``` r
table(dat$Leginflyes)
```

    ## 
    ##    0    1 
    ## 1741   19

There are only 19 organizations that influence legislation, so this must
have a hyper specific definition. We can remove these columns from `dat`
and `local.dat`.

``` r
dat <- dat %>% 
  select(-Leginflyes, -Leginflno, -Qualifyforexemption)

local.dat <- local.dat %>% 
  select(-Leginflyes, -Leginflno, -Qualifyforexemption)
```

Let’s explore negatively scored mission statements to see what sets them
apart from others.

``` r
local.neg <- local.dat %>% 
  filter(compound < 0)

head(local.neg$Mission)
```

    ## [1] "Cherokee's Chance aims to provide assistance for livestock during times of need. Namely horses, mules, donkeys, goats, chickens, etc. Assistance may be provided in forms of temporary housing, medical assistance, refuge during natural disasters."     
    ## [2] "Sage Indigenous Law Center is a nonprofit community based legal aid office providing low income and underrepresented people with legal services including culturally appropriate consultations and representation."                                       
    ## [3] "The intent of Skyward Rescue & Rehabilitation is to take in and house neglected and/or abused exotic/non native birds (including but not limited to parrots). We check for medical and dietary issues, provide adoption services to screened individuals,"
    ## [4] "The mission of the Fairfield Firefighter's Assocation is to support fire prevention and protection services provided by the Fairfield Fire Department. Also, to provide its members a social, athletic, and recreational fellowship."                     
    ## [5] "The organization's mission is to provide support for traumatic brain injury survivors, families, and caregivers. Through the use of support groups and community."                                                                                        
    ## [6] "Thrive 4 Abundance is committed to raising awareness about Human Trafficking, providing advocacy and self-empowerment to those who have been affected by this crime within our community."

We can see some common issues here. When mission statements list issues
they’re working on or conditions they want to change, it lowers the
VADER score. Having mostly neutral words seems to make the positive and
negative words influence the compound score more extremely. ALL CAPS
WORDS get read negatively. Lastly, VADER scores some words negatively
because due to alternative definitions. See the trap shooting team
example.

``` r
trap_shooting <- 12

print(local.neg$Mission[trap_shooting])
```

    ## [1] "We will provide funds to our youth trap shooting team."

There is nothing negative here except that when “trap” and “shooting”
are read on an individual basis, they count as 2 negative words

# Basic Text Analysis

The sentiment analysis is done, so we can tidy and tokenize the mission
statements. First, we remove stopwords and white space and make
everything lower case.

``` r
local.dat$Mission <- tolower(local.dat$Mission)

corp <- corpus(local.dat, text_field="Mission")

corp <- corpus_trim(corp, what="sentences", min_ntoken=3)

tokens <- tokens(corp, what="word", remove_punct=TRUE)

head(tokens)
```

    ## Tokens consisting of 6 documents and 24 docvars.
    ## text1 :
    ##  [1] "1"            "provide"      "a"            "social"       "organization"
    ##  [6] "for"          "all"          "those"        "interested"   "in"          
    ## [11] "the"          "restoration" 
    ## [ ... and 9 more ]
    ## 
    ## text2 :
    ##  [1] "a"         "nonprofit" "community" "sports"    "program"   "organized"
    ##  [7] "and"       "operated"  "for"       "the"       "purpose"   "of"       
    ## [ ... and 23 more ]
    ## 
    ## text3 :
    ##  [1] "a"          "provide"    "financial"  "and"        "physical"  
    ##  [6] "assistance" "for"        "destitute"  "families"   "women"     
    ## [11] "children"   "and"       
    ## [ ... and 21 more ]
    ## 
    ## text4 :
    ## [1] "animal"         "rescue"         "and"            "rehabilitation"
    ## [5] "center"        
    ## 
    ## text5 :
    ##  [1] "applicant's"   "mission"       "is"            "to"           
    ##  [5] "foster"        "safer"         "and"           "healthier"    
    ##  [9] "communities"   "by"            "fostering"     "collaborative"
    ## [ ... and 21 more ]
    ## 
    ## text6 :
    ##  [1] "assist"      "homeless"    "vets"        "in"          "king"       
    ##  [6] "county"      "washington"  "find"        "sustainable" "housing"    
    ## [11] "that"        "will"       
    ## [ ... and 23 more ]

``` r
tokens <- tokens_remove(tokens, c(stopwords("english"), "nbsp" ), padding=F)
```

Mission statements are ready to be explored. We’ll use n-grams and take
a look at word stems. An n-gram is a “consecutive sequences of words,”
where the n is replaced with a number of your choice. For example, below
we use ngram2 (bigram) and ngram3 (trigram). Learn more about n-grams in
[Text Mining with R](https://www.tidytextmining.com/) by Julia Silge and
David Robinson.

``` r
ngram2 <- tokens_ngrams(tokens, n=2) %>% dfm()

ngram2 %>% textstat_frequency(n=10)
```

    ##               feature frequency rank docfreq group
    ## 1     provide_support         5    1       5   all
    ## 2       mental_health         5    1       5   all
    ## 3  organized_operated         4    3       4   all
    ## 4    student_athletes         4    3       4   all
    ## 5         high_school         4    3       4   all
    ## 6               501_c         3    6       3   all
    ## 7                 c_3         3    6       3   all
    ## 8       animal_rescue         3    6       2   all
    ## 9    support_services         3    6       3   all
    ## 10    local_community         3    6       3   all

I can see from this list that I should go in and do some clean up of
501c3 then come back and run this again.

``` r
ngram3 <- tokens_ngrams(tokens, n=3) %>% dfm()

ngram3 %>% textstat_frequency(n=10)
```

    ##                        feature frequency rank docfreq group
    ## 1                      501_c_3         3    1       3   all
    ## 2                section_501_c         2    2       2   all
    ## 3           sport_roller_derby         2    2       2   all
    ## 4                john_r_rogers         2    2       2   all
    ## 5                r_rogers_high         2    2       2   all
    ## 6           rogers_high_school         2    2       2   all
    ## 7  education_resources_support         2    2       2   all
    ## 8  committed_raising_awareness         2    2       2   all
    ## 9    families_communities_need         2    2       2   all
    ## 10         low_income_families         2    2       1   all

This reinforces the need to clean up 501c3.

``` r
# pull mission statements that contain "501" to find different version of 501c3
nonprof <- grep("501", local.dat$Mission, value = TRUE)

print(nonprof)
```

    ## [1] "a nonprofit community sports program organized and operated for the purpose of youth athletics, baseball and softball, for exemption under section 501(c)(3) as a charitable organization providing exercise, teaching teamwork and building community."  
    ## [2] "john r. rogers high school alumni association primarily provides scholarships and assistance to students, supporting social, educational and cultural needs of the general public. we will continue to actively accomplish tax-exempt 501(c)(3) purposes."
    ## [3] "the corporation is organized and operated exclusively for religious, charitable, educational, scientific, and literary purposes, within the meaning of section 501 (c)(3) of the internal revenue code of 1986, as amended."

``` r
# create a dictionary for force common compounds words into a single format
my_dictionary <- dictionary(list(five01_c_3 = c("501 c 3", "501 c 3", "501c3", "501(c)(3)", "501(c)3", "501 (c)(3)"),
                                 high_school = c("high school", "high-school", "highschool"),
                                 non_profit = c("non-profit", "non profit", "not-for-profit", "not for profit"),
                                 community_based = c("community-based", "community based"),
                                 under_represented = c("under-represented", "under represented", "underrepresented"),
                                 issue_driven = c("issue driven", "issue-driven"),
                                 high_quality = c("high-quality", "high quality"),
                                 action_oriented = c("action-oriented", "action oriented"),
                                 policy_focused = c("policy-focused", "policy focused"),
                                 intercultural = c("inter cultural", "inter-cultural"),
                                 healthcare = c("health-care", "health care"),
                                 volunteer_run = c("volunteer-run", "volunteer run"),
                                 underserved = c("under-served", "under served")))

# apply the dictionary
tokens <- tokens_compound(tokens, pattern=my_dictionary)

head(tokens)
```

    ## Tokens consisting of 6 documents and 24 docvars.
    ## text1 :
    ##  [1] "1"            "provide"      "social"       "organization" "interested"  
    ##  [6] "restoration"  "modification" "promotion"    "ford"         "broncos"     
    ## 
    ## text2 :
    ##  [1] "nonprofit" "community" "sports"    "program"   "organized" "operated" 
    ##  [7] "purpose"   "youth"     "athletics" "baseball"  "softball"  "exemption"
    ## [ ... and 10 more ]
    ## 
    ## text3 :
    ##  [1] "provide"     "financial"   "physical"    "assistance"  "destitute"  
    ##  [6] "families"    "women"       "children"    "elderly"     "living"     
    ## [11] "exclusively" "country"    
    ## [ ... and 8 more ]
    ## 
    ## text4 :
    ## [1] "animal"         "rescue"         "rehabilitation" "center"        
    ## 
    ## text5 :
    ##  [1] "applicant's"   "mission"       "foster"        "safer"        
    ##  [5] "healthier"     "communities"   "fostering"     "collaborative"
    ##  [9] "relationships" "within"        "community"     "specifically" 
    ## [ ... and 11 more ]
    ## 
    ## text6 :
    ##  [1] "assist"      "homeless"    "vets"        "king"        "county"     
    ##  [6] "washington"  "find"        "sustainable" "housing"     "improve"    
    ## [11] "life"        "health"     
    ## [ ... and 12 more ]

``` r
# re-run ngram2 and ngram3
ngram2 <- tokens_ngrams(tokens, n=2) %>% dfm()

ngram2 %>% textstat_frequency(n=10)
```

    ##               feature frequency rank docfreq group
    ## 1     provide_support         5    1       5   all
    ## 2       mental_health         5    1       5   all
    ## 3  organized_operated         4    3       4   all
    ## 4    student_athletes         4    3       4   all
    ## 5       animal_rescue         3    5       2   all
    ## 6    support_services         3    5       3   all
    ## 7     local_community         3    5       3   all
    ## 8    first_responders         3    5       3   all
    ## 9        roller_derby         3    5       2   all
    ## 10   community_events         3    5       3   all

This looks much better. Due to the limited data, there probably won’t be
much improvement to ngram3, but let’s take a gander anyway.

``` r
ngram3 <- tokens_ngrams(tokens, n=3) %>% dfm()

ngram3 %>% textstat_frequency(n=10)
```

    ##                           feature frequency rank docfreq group
    ## 1              sport_roller_derby         2    1       2   all
    ## 2                   john_r_rogers         2    1       2   all
    ## 3            r_rogers_high_school         2    1       2   all
    ## 4     education_resources_support         2    1       2   all
    ## 5     committed_raising_awareness         2    1       2   all
    ## 6       families_communities_need         2    1       2   all
    ## 7             low_income_families         2    1       1   all
    ## 8                1_provide_social         1    8       1   all
    ## 9     provide_social_organization         1    8       1   all
    ## 10 social_organization_interested         1    8       1   all

It seems like there just isn’t enough data for this to be of much value;
pulling from the statewide `dat` would probably show more, but we don’t
need it.

``` r
# find the top 20 wordstems
tokens %>% dfm() %>% dfm_wordstem() %>% topfeatures(20)
```

    ##    provid   support communiti      educ     organ      help   mission    purpos 
    ##        73        52        50        44        26        25        23        20 
    ##   student    assist    servic     activ    promot     youth     creat   program 
    ##        19        17        17        17        16        16        16        15 
    ##    foster  individu    famili    health 
    ##        15        15        14        14

Word stems are the common root of a word. “Win,” “winning,” and “winner”
all have the same root of “win.” This is a very useful function because
it means we don’t have to search for and add together different
variations of a word. “Organ” is a top stem, but let’s look deeper. Is
it organization or organic?

``` r
organ.results <- grep("^organ", tokens, value = TRUE)

organ.table <- as.data.frame(table(organ.results))

print(organ.table)
```

    ##    organ.results Freq
    ## 1   organization   14
    ## 2 organization's    2
    ## 3  organizations    4
    ## 4      organized    5
    ## 5      organizes    1

Word stems and n-grams would be useful for the Quality of Life Survey.
This code through only uses basic commands, but text analysis and mining
can be used to find co-occurrences and correlations or create topic
models. Our open-ended Quality of Life Survey question asks, “What is
the most important issue facing the Spokane area today?” Even this
simple application could make it significantly easier to generate a
frequency of issues.

# Packed Bubble Chart

I won’t lie…Tableau is <strong>absolutely</strong> the better choice for
visualizing text and sentiment analysis data. Rouder et al. put together
a helpful guide called [What to Do With All Those Open-Ended Responses?
Data Visualization Techniques for Survey
Researchers](https://www.surveypractice.org/article/25699-what-to-do-with-all-those-open-ended-responses-data-visualization-techniques-for-survey-researchers).
Figures 1 & 2 are of particular interest to us.

Figure 1 is a beautiful packed bubble chart that shows theme frequency
(bubble size) and valence (bubble color). I used this as a challenge to
see if I could do something similar in R. It’s…not the same quality as
something produced in Tableau, but it was incredibly easy to create. I
see value in using this chart type in the future, and Amy R. might have
practical Tableau insight for us. Check out Tableau’s [Build a Packed
Bubble
Chart](https://help.tableau.com/current/pro/desktop/en-us/buildexamples_bubbles.htm).

Figure 2 is a sunburst chart used to visualize a coding scheme. This has
a lot of potential as a tool for communicating methodology in our work.
Again, this can be achieved in Tableau way easier than in R. Check out
Tableau’s [Sunburst Diagram](https://exchange.tableau.com/products/1011)
and [Dynamic Sunburst Chart](https://exchange.tableau.com/products/668).

The last task is to create a packed bubble chart using NTEE category
frequency as the circle size and VADER compound score classifications as
the color.

``` r
# quick refresh on the fields we're working with
head(local.dat$ntee.cat1)
```

    ## [1] "Recreation & Sports"                               
    ## [2] "Recreation & Sports"                               
    ## [3] "International, Foreign Affairs & National Security"
    ## [4] "Animal-Related"                                    
    ## [5] "Public Safety, Disaster Preparedness & Relief"     
    ## [6] "Human Services"

``` r
summary(local.dat$compound)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -0.9500  0.3180  0.5110  0.4686  0.8180  0.9640

``` r
# calculate mean for each category and classify the means
bubble.dat <- local.dat %>% 
  select(ntee.cat1, compound) %>% 
  group_by(ntee.cat1) %>% 
  mutate(comp.mean = mean(compound),
         ntee.count = n()) %>% 
  ungroup() %>% 
  filter(ntee.count > 9) %>%   # filters it down to a manageable size
  arrange(desc(ntee.count), desc(comp.mean)) %>%
  mutate(vader.sentiment = case_when(comp.mean > 0.05 ~ "Positive",
                                     comp.mean < -0.05 ~ "Negative",
                                     comp.mean >= -0.05 & compound <= 0.05 ~ "Neutral")) %>% 
  select(-compound)

# remove duplicate rows
bubble.dat <- bubble.dat[!duplicated(bubble.dat), ]

# preview the data
print(bubble.dat)
```

    ## # A tibble: 5 × 4
    ##   ntee.cat1                  comp.mean ntee.count vader.sentiment
    ##   <chr>                          <dbl>      <int> <chr>          
    ## 1 Education                      0.446         22 Positive       
    ## 2 Recreation & Sports            0.372         22 Positive       
    ## 3 Human Services                 0.538         15 Positive       
    ## 4 Arts, Culture & Humanities     0.614         14 Positive       
    ## 5 Animal-Related                 0.568         10 Positive

This doesn’t generate enough diversity, so let’s clean up the data frame
and add fake sentiments later.

``` r
# remove comp.mean and adjust category name for fit (added based on issue with wrapping)
bubble.dat <- bubble.dat %>%
  select(-comp.mean) %>% 
  mutate(ntee.cat1 = recode(ntee.cat1, "Animal-Related" = "Animals"))
```

Now we need to prep the data, which means adding `x` and `y` coordinates
to place the circles. We also need to add fake `vader.sentiments`. Then
we simply use ggplot.

``` r
packing <- circleProgressiveLayout(bubble.dat$ntee.count, sizetype='area')

packing$radius <- 0.95*packing$radius

bubble.dat <- cbind(bubble.dat, packing)

dat.gg <- circleLayoutVertices(packing)

dat.gg$vader.sentiment <- case_when(dat.gg$id == 1 ~ "Negative",
                                    dat.gg$id == 2 ~ "Positive",
                                    dat.gg$id == 3 ~ "Neutral",
                                    dat.gg$id == 4 ~ "Positive",
                                    dat.gg$id == 5 ~ "Negative")

head(dat.gg)
```

    ##            x         y id vader.sentiment
    ## 1 -0.1323142 0.0000000  1        Negative
    ## 2 -0.2112952 0.6251988  1        Negative
    ## 3 -0.4432754 1.2111141  1        Negative
    ## 4 -0.8136788 1.7209306  1        Negative
    ## 5 -1.2992315 2.1226147  1        Negative
    ## 6 -1.8694244 2.3909271  1        Negative

``` r
# plot packed bubble chart 
packed.bubble.chart <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = vader.sentiment, fill=vader.sentiment), alpha = 1) +
  scale_fill_manual(values = viridis(nrow(bubble.dat))) +
  geom_text(data = bubble.dat, aes(x, y, label = str_wrap(ntee.cat1, width = 10)), color = "white", size = 4) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() + 
  coord_equal() +
  guides(fill = guide_legend(title = "VADER Sentiment")) +
  theme(legend.position = "bottom") + 
  labs(title = "Spokane County nonprofits",
       subtitle = "by NTEE category frequency and mission statement sentiment")

print(packed.bubble.chart)
```

![Packed bubble chart](https://jslandes.github.io/paf-514-website-example/assets/img/packedbubble.jpg)

# Conclusion

R could be very useful to us in analyzing open-ended survey responses.
It’s a less feasible choice for some qualitative data visualizations,
but R and Tableau can complement each other in many ways. We should
consider text mining for future surveys.
