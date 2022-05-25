Exercise 3 - Ranvir K
================

## Setting up data and environment

## We first need to do a few things before we can manipulate the data.

``` r
# set path for R to find our data
data_path <- "C:/Users/rkjg9/OneDrive/RAZER Desktop Backup/MBA/Summer 2022/ORGB 690 - People Analytics/Group Project/USPTO_data/"
```

## 1. Load data

We’ll load application data only here (you are welcome to load the other
three files as well). Because we are loading from a .parquet format
file, we’ll use library `arrow` and the functions `read_parquet()`. For
the rest of the files, we can use function `read_csv()` which comes with
a package `readr` (which is included in `tidyverse` set of packages, so
if we are loading `tidyverse` there is no need to also load `readr`).
Note that the path to the data file on my computer is defined above, in
the `data_path` variable.

``` r
library(arrow) # to be able to load data in the .parquet format
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
# read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

To inspect the top slice of the data, we can simply call it:

``` r
app_data_sample
```

    ## # A tibble: 2,018,477 × 16
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # … with 2,018,467 more rows, and 12 more variables:
    ## #   examiner_name_middle <chr>, examiner_id <dbl>, examiner_art_unit <dbl>,
    ## #   uspc_class <chr>, uspc_subclass <chr>, patent_number <chr>,
    ## #   patent_issue_date <date>, abandon_date <date>, disposal_type <chr>,
    ## #   appl_status_code <dbl>, appl_status_date <chr>, tc <dbl>

### Get gender for examiners

We’ll get gender based on the first name of the examiner, which is
recorded in the field `examiner_name_first`. We’ll use library `gender`
for that, relying on a modified version of their own
[example](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html).

Note that there are over 2 million records in the applications table –
that’s because there are many records for each examiner, as many as the
number of applications that examiner worked on during this time frame.
Our first step therefore is to get all *unique* names in a separate list
`examiner_names`. We will then guess gender for each one and will join
this table back to the original dataset. So, let’s get names without
repetition:

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
examiner_names
```

    ## # A tibble: 2,595 × 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # … with 2,585 more rows

Now let’s use function `gender()` as shown in the example for the
package to attach a gender and probability to each name and put the
results into the table `examiner_names_gender`

``` r
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
examiner_names_gender
```

    ## # A tibble: 1,822 × 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # … with 1,812 more rows

Finally, let’s join that table back to our original applications data
and discard the temporary tables we have just created to reduce clutter
in our environment.

``` r
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)
# joining gender back to the dataset
app_data_sample <- app_data_sample %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4551642 243.1    7582492 405.0  4950462 264.4
    ## Vcells 49542397 378.0   92452241 705.4 79858154 609.3

### Guess the examiner’s race

We’ll now use package `wru` to estimate likely race of an examiner. Just
like with gender, we’ll get a list of unique names first, only now we
are using surnames.

``` r
library(wru)
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_surnames
```

    ## # A tibble: 3,806 × 1
    ##    surname   
    ##    <chr>     
    ##  1 HOWARD    
    ##  2 YILDIRIM  
    ##  3 HAMILTON  
    ##  4 MOSHER    
    ##  5 BARR      
    ##  6 GRAY      
    ##  7 MCMILLIAN 
    ##  8 FORD      
    ##  9 STRZELECKA
    ## 10 KIM       
    ## # … with 3,796 more rows

We’ll follow the instructions for the package outlined here
<https://github.com/kosukeimai/wru>.

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
examiner_race
```

    ## # A tibble: 3,806 × 6
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198
    ## # … with 3,796 more rows

As you can see, we get probabilities across five broad US Census
categories: white, black, Hispanic, Asian and other. (Some of you may
correctly point out that Hispanic is not a race category in the US
Census, but these are the limitations of this package.)

Our final step here is to pick the race category that has the highest
probability for each last name and then join the table back to the main
applications table. See this example for comparing values across
columns: <https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/>.
And this one for `case_when()` function:
<https://dplyr.tidyverse.org/reference/case_when.html>.

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))
examiner_race
```

    ## # A tibble: 3,806 × 8
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333      0.643 white
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372      0.861 white
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309      0.702 white
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185      0.947 white
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271      0.827 white
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324      0.687 white
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463      0.574 black
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313      0.620 white
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318      0.666 white
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198      0.945 Asian
    ## # … with 3,796 more rows

Let’s join the data back to the applications table.

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)
app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4966356 265.3    7582492 405.0  7582492 405.0
    ## Vcells 53341739 407.0   92452241 705.4 90519288 690.7

### Examiner’s tenure

To figure out the timespan for which we observe each examiner in the
applications data, let’s find the first and the last observed date for
each examiner. We’ll first get examiner IDs and application dates in a
separate table, for ease of manipulation. We’ll keep examiner ID (the
field `examiner_id`), and earliest and latest dates for each application
(`filing_date` and `appl_status_date` respectively). We’ll use functions
in package `lubridate` to work with date and time values.

``` r
library(lubridate) # to work with dates
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates
```

    ## # A tibble: 2,018,477 × 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # … with 2,018,467 more rows

The dates look inconsistent in terms of formatting. Let’s make them
consistent. We’ll create new variables `start_date` and `end_date`.

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018)
```

Let’s now identify the earliest and the latest date for each examiner
and calculate the difference in days, which is their tenure in the
organization.

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )
examiner_dates
```

    ## # A tibble: 5,649 × 4
    ##    examiner_id earliest_date latest_date tenure_days
    ##          <dbl> <date>        <date>            <dbl>
    ##  1       59012 2004-07-28    2015-07-24         4013
    ##  2       59025 2009-10-26    2017-05-18         2761
    ##  3       59030 2005-12-12    2017-05-22         4179
    ##  4       59040 2007-09-11    2017-05-23         3542
    ##  5       59052 2001-08-21    2007-02-28         2017
    ##  6       59054 2000-11-10    2016-12-23         5887
    ##  7       59055 2004-11-02    2007-12-26         1149
    ##  8       59056 2000-03-24    2017-05-22         6268
    ##  9       59074 2000-01-31    2017-03-17         6255
    ## 10       59081 2011-04-21    2017-05-19         2220
    ## # … with 5,639 more rows

Joining back to the applications data.

``` r
app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  4980650 266.0   13601220  726.4  13601220  726.4
    ## Vcells 65720838 501.5  133307226 1017.1 133306908 1017.1

## 3. Descriptive statistics

Let’s look at distribution of gender, race and tenure, overall in the
organization, and by technology centers (TCs) and workgroups.

### Overall distributions of gender, race and tenure.

We can we can start with simple frequencies.

``` r
subset_app_data <- app_data_sample %>% 
  mutate(race = as_factor(race), gender = as_factor(gender)) %>% 
  select(gender, race, tenure_days) 
subset_app_data %>% 
  count(gender) %>% 
  mutate(pct = n/sum(n))
```

    ## # A tibble: 3 × 3
    ##   gender       n   pct
    ##   <fct>    <int> <dbl>
    ## 1 female  571227 0.283
    ## 2 male   1143391 0.566
    ## 3 <NA>    303859 0.151

``` r
subset_app_data %>% 
  count(race) %>% 
  mutate(pct = n/sum(n))
```

    ## # A tibble: 5 × 3
    ##   race           n      pct
    ##   <fct>      <int>    <dbl>
    ## 1 white    1279353 0.634   
    ## 2 black      90027 0.0446  
    ## 3 Asian     588988 0.292   
    ## 4 Hispanic   59657 0.0296  
    ## 5 other        452 0.000224

We can also use library `skimr` to skim the data quickly.

``` r
library(skimr)
subset_app_data %>%  
  skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 2018477    |
| Number of columns                                | 3          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 2          |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts                                        |
|:--------------|----------:|--------------:|:--------|---------:|:--------------------------------------------------|
| gender        |    303859 |          0.85 | FALSE   |        2 | mal: 1143391, fem: 571227                         |
| race          |         0 |          1.00 | FALSE   |        5 | whi: 1279353, Asi: 588988, bla: 90027, His: 59657 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |      sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|--------:|--------:|----:|-----:|-----:|-----:|-----:|:------|
| tenure_days   |         0 |             1 | 5531.37 | 1100.91 |  27 | 4962 | 6090 | 6336 | 6518 | ▁▁▁▂▇ |

### Plot tenure by gender and TCs

We will use `ggplot` package to plot our distributions. It’s the most
widely used and the most flexible plotting package. Watch a tutorial
here: <https://youtu.be/h29g21z0a68>.

First, we need to keep one observation per person, to avoid the same
person contributing to the plot multiple times because they appear in
the data for multiple applications.

``` r
person_level_data <- app_data_sample %>% 
  group_by(examiner_id) %>% 
  summarise(
    art_unit = min(examiner_art_unit, na.rm = TRUE),
    gender = min(gender, na.rm = TRUE),
    start_year = min(year(earliest_date), na.rm = TRUE),
    latest_date = max(latest_date, na.rm = TRUE),
    tenure_days = max(tenure_days, na.rm = TRUE)
  ) %>% 
  mutate(
    tc = floor(art_unit/100)*100,
    work_group = floor(art_unit/10)*10
  ) %>% 
  filter(!is.na(gender)) # dropping all records where we don't know the gender
person_level_data
```

    ## # A tibble: 4,849 × 8
    ##    examiner_id art_unit gender start_year latest_date tenure_days    tc
    ##          <dbl>    <dbl> <chr>       <dbl> <date>            <dbl> <dbl>
    ##  1       59012     1716 male         2004 2015-07-24         4013  1700
    ##  2       59025     2465 male         2009 2017-05-18         2761  2400
    ##  3       59040     1724 female       2007 2017-05-23         3542  1700
    ##  4       59052     2138 male         2001 2007-02-28         2017  2100
    ##  5       59055     2165 male         2004 2007-12-26         1149  2100
    ##  6       59056     2124 male         2000 2017-05-22         6268  2100
    ##  7       59081     2489 male         2011 2017-05-19         2220  2400
    ##  8       59086     2487 female       2010 2017-05-18         2527  2400
    ##  9       59096     1612 male         2000 2015-11-20         5800  1600
    ## 10       59117     2439 male         2009 2011-09-02          925  2400
    ## # … with 4,839 more rows, and 1 more variable: work_group <dbl>

We start by plotting boxplots for tenure. A simple boxplot for the
entire sample looks like this

``` r
ggplot(person_level_data) +
  geom_boxplot(aes(x = tenure_days, color = gender))
```

![](Exercise-3_files/figure-gfm/plot-gender-2-1.png)<!-- -->

Now let’s plot gender composition by TC. Note that I am conversing the
numerical variable `tc` into a factor variable, so that ggplot
understands it’s just a label for the Technology Center, not an actual
measure of something and so intelligently adjusts the x-axis.

``` r
ggplot(person_level_data) +
  geom_bar(
    aes(x=as_factor(tc), fill = gender), 
    position = position_stack()
    ) +
  xlab("Technology Center")
```

![](Exercise-3_files/figure-gfm/plot-gender-3-1.png)<!-- -->

### Tiling multiple plots

Let’s plot gender composition by work group for each TC. See this page
for documentation:
<https://ggplot2.tidyverse.org/reference/facet_wrap.html>

``` r
ggplot(person_level_data) +
  geom_bar(
    aes(x=as_factor(work_group), fill = gender), 
    position = position_stack()
    ) +
  xlab("Work group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + #rotate labels
  facet_wrap(vars(tc), scales = "free")
```

![](Exercise-3_files/figure-gfm/plot-gender-4-1.png)<!-- -->

## Creating Art Unit size variable and joining it to main table

``` r
art_unit_size = app_data_sample %>% group_by (examiner_art_unit) %>% count() %>% select(examiner_art_unit,art_unit_size = n)
app_data_sample = app_data_sample %>% left_join(art_unit_size,by = "examiner_art_unit")
```

## Creating Gender ratio variable in every art unit and joining it to the main table

``` r
art_unit_gender_size = app_data_sample %>% group_by(examiner_art_unit,gender) %>%
  count() %>%  filter(!is.na(gender)) %>% filter(gender == 'female')

Table_gender_size <- art_unit_gender_size %>% 
  left_join(art_unit_size, by = "examiner_art_unit")

Table_gender_size$female_to_male_ratio = Table_gender_size$n/(Table_gender_size$art_unit_size - Table_gender_size$n)

df = subset(Table_gender_size, select = c(female_to_male_ratio, examiner_art_unit))

app_data_sample = app_data_sample %>% left_join(df, by = "examiner_art_unit")
```

## 4. Linear Models

## Libraries for the regression

``` r
library(modelsummary)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(gt)
```

    ## 
    ## Attaching package: 'gt'

    ## The following object is masked from 'package:modelsummary':
    ## 
    ##     escape_latex

## OLS 1 - Using all variabes to explain Tenure_days

``` r
OLS_1 = lm(tenure_days ~ art_unit_size + as.factor(gender) + as.factor(race) + female_to_male_ratio  , data=app_data_sample)
modelsummary(OLS_1)
```

|                         |    Model 1    |
|:------------------------|:-------------:|
| (Intercept)             |   5066.715    |
|                         |    (2.739)    |
| art_unit_size           |     0.040     |
|                         |    (0.000)    |
| as.factor(gender)male   |    -26.295    |
|                         |    (1.821)    |
| as.factor(race)black    |    152.052    |
|                         |    (4.716)    |
| as.factor(race)Hispanic |   -306.718    |
|                         |    (4.894)    |
| as.factor(race)other    |   -576.222    |
|                         |   (50.113)    |
| as.factor(race)white    |    -4.036     |
|                         |    (1.948)    |
| female_to_male_ratio    |    210.896    |
|                         |    (2.194)    |
| Num.Obs.                |    1711242    |
| R2                      |     0.053     |
| R2 Adj.                 |     0.053     |
| AIC                     |  28712662.9   |
| BIC                     |  28712774.1   |
| Log.Lik.                | -14356322.456 |
| F                       |   13608.718   |
| RMSE                    |    1064.73    |

## OLS 2 - Using gender and art_unit_size to explain Tenure_days

``` r
OLS_2 = lm(tenure_days ~ art_unit_size + as.factor(gender) , data=app_data_sample)
modelsummary(OLS_2)
```

|                       |    Model 1    |
|:----------------------|:-------------:|
| (Intercept)           |   5175.248    |
|                       |    (2.161)    |
| art_unit_size         |     0.042     |
|                       |    (0.000)    |
| as.factor(gender)male |    -82.789    |
|                       |    (1.741)    |
| Num.Obs.              |    1714618    |
| R2                    |     0.045     |
| R2 Adj.               |     0.045     |
| AIC                   |  28785125.2   |
| BIC                   |  28785174.6   |
| Log.Lik.              | -14392558.603 |
| F                     |   39968.961   |
| RMSE                  |    1069.66    |

## OLS 3 - Using Gender Ratio and art_unit_size to explain tenure days

``` r
OLS_3 = lm(tenure_days ~ art_unit_size + female_to_male_ratio  , data=app_data_sample)
modelsummary(OLS_3)
```

|                      |    Model 1    |
|:---------------------|:-------------:|
| (Intercept)          |   5016.508    |
|                      |    (1.706)    |
| art_unit_size        |     0.041     |
|                      |    (0.000)    |
| female_to_male_ratio |    246.246    |
|                      |    (1.983)    |
| Num.Obs.             |    2014157    |
| R2                   |     0.054     |
| R2 Adj.              |     0.054     |
| AIC                  |  33816487.1   |
| BIC                  |  33816537.2   |
| Log.Lik.             | -16908239.573 |
| F                    |   57434.561   |
| RMSE                 |    1070.37    |

## OLS 4 - Using race, gender ratio & art unit size to explain Tenure_days

``` r
OLS_4 <- lm(tenure_days ~ female_to_male_ratio + as.factor(race) + art_unit_size  , data=app_data_sample)
modelsummary(OLS_4)
```

|                         |    Model 1    |
|:------------------------|:-------------:|
| (Intercept)             |   5018.805    |
|                         |    (1.974)    |
| female_to_male_ratio    |    242.993    |
|                         |    (1.991)    |
| as.factor(race)black    |    104.719    |
|                         |    (3.838)    |
| as.factor(race)Hispanic |   -301.570    |
|                         |    (4.593)    |
| as.factor(race)other    |   -569.473    |
|                         |   (50.293)    |
| as.factor(race)white    |    12.314     |
|                         |    (1.701)    |
| art_unit_size           |     0.040     |
|                         |    (0.000)    |
| Num.Obs.                |    2014157    |
| R2                      |     0.057     |
| R2 Adj.                 |     0.057     |
| AIC                     |  33810632.3   |
| BIC                     |  33810732.5   |
| Log.Lik.                | -16905308.167 |
| F                       |   20179.179   |
| RMSE                    |    1068.81    |

## Results:

### 1) Using the regression models I have run, it seems like the best fit model is OLS 4 which includes gender ratio, art unit size and race to explain the tenure days. Surprisingly, this model does not contain all variables and the model that contains all variables (OLS 1) seem to be less relevant in terms of explaining the tenure days. This tells me that including gender as well as gender ratio in the same mode is not only excessive and irrelevant but also impacts the model negatively as suggested by the lower adjusted r_squared value.

### 2) It should also be noted that the P value is the same for both OLS 1 and OLS 4, which means that both models give the same probability of the predicted tenure days being greater than the actual value. Also, this P value is quite low at 0.00000000000000022, which means that the prediction of tenure days has a very very low probability of being off from the actual tenure days.

### 3) It is also seen that OLS 4 has a residual standard error of 1069 on 2014150 degrees of freedom, while OLS 1 has a residual standard error of 1065 on 1711234 degrees of freedom. This means that OLS 1 fits the data set better as the data points in OLS 1 are more tightly packed along the trendline. However, it is to be noted that the difference between residual standard error of OLS 4 (1069) and the residual standard error of OLS 1 (1065) is extremely small and can be considered insignificant in determining which model is better.

### 4) Another observation is in terms of significance codes. In OLS 4, all the variables (gender ratio, art unit size and the different races) have a very high significance level, i.e all variables are strong predictors, whereas in OLS 1, the race “white” has a low significance code, which means it is not a strong contributor to the prediction. This is an interesting observation as I would expect them to be the same in both models. The explanation of this is something I could not come up with.

### 5) There is a lot more observations that are deleted/dropped by R from OLS 1 as compared to OLS 4 to perform the regressions. This is because in OLS 1, we have used gender as a variable to predict tenure days, and upon quick glance at the table, we can see that there are a lot of NA values in the gender column. This is what causes the large number of dropped data points by R.

### 6) To explain the intercept, I will use OLS 4, which is my choice as the best predictive model for tenure days. In OLS 4, the intercept value is 5019. This means that for a gender tatio of 0 (number of females = 0), race = 0 (all races except the one selected) and art_unit_size = 0 (hypothetical as no art unit exists with size 0), the predicted tenure days is 5019.

### 7) Finally, in order to explain the output of OLS 4 let us use an exmple. If we were to predict the Tenure days of a Hispanic Female in art unit 1600, the regression output predicts that the Tenure Days = 5019 + 243 \* (Gender ratio in art unit 1600) + 0.04047 \* (number of people in art unit 1600). this is equal to 5019 + (243 \* 0.278481) + (101 \* 0.0407) = 5090.78. Note that this equation does not take into consideration the gender of the person, this is because the model does not use gender as a variable for prediction, whereas it uses gender ratio, which incorporates the effect of gender in the model.

## Improvements:

### 1) We could split the data into 2 and run regressions with same variabels with different data sets to check if same r2

### 2) We could split the data into different TCs and run the regresion for each TC to see how the variables effect the tenure days in each TC

## Contributors to R code:

### Ranvir, Hima, Patrick, Adel, Shan
