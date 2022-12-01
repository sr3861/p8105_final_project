Tama - EDA
================
Shritama Ray
2022-12-01

### Import & Tidy Data:

``` r
#Load & select variables of interest

load("./data/04572-0001-Data.rda") 

federal_df <- da04572.0001%>% 
  select(V2288, V2289, V2290, V2291, MCMH_HIVTEST, MCMH_RSLT_HIV, DRUG_INJECT_CB, V2401, V2402, V2403, V2404, V2405, V2406, V2407, V2409, V2412, MCMH_MH_TREATMENT_PA, MCMH_MH_TREATMENT_AD, MCMH_MENTAL_HISTORY, V1197, CS_SENTENCEMTH,CH_PRIORSENTENCE, CH_PRIORSENTENCE_NUM, CH_CRIMHIST, CH_CRIMHIST_COLLAPSED, CH_NUMCAR,CH_NUMCAR1, CH_NUMCAR2, CH_NUMCAR3, CH_NUMCAR4, CH_NUMCAR5, MOST_SERIOUS_OFFENSE2, TYPEOFFENSE, V1056, V1057, V1060, V1061, V1325, V0001, V0014, AGE_CAT, V2982, V0005, EDUCATION, SES_INCOMEMTH, DRUG_ANY, DRUG_ANYREG, DRUG_ANYMTH, SES_PHYSSEXABUSED_EVER, SES_PARENTS_INCARCERATED, SES_FAMILY_INCARCERATED)

load("./data/04572-0002-Data.rda")
state_df =
  da04572.0002 %>%
  select(V2288, V2289, V2290, V2291, MCMH_HIVTEST, MCMH_RSLT_HIV,DRUG_INJECT_CB,
         V2401, V2402, V2403, V2404, V2405, V2406, V2407, V2409, V2412, MCMH_MH_TREATMENT_PA, MCMH_MH_TREATMENT_AD, MCMH_MENTAL_HISTORY, MCMH_WHEN_DISORDER, MCMH_WHEN_DISORDER2, MCMH_MHOSPYR, MCMH_MHOSPADM, MCMH_MHOSPYRADM, MCMH_SMI, V1197, CS_SENTENCEMTH, CH_PRIORSENTENCE, CH_PRIORSENTENCE_NUM, CH_CRIMHIST, CH_CRIMHIST_COLLAPSED, CH_NUMCAR, CH_NUMCAR1, CH_NUMCAR2, CH_NUMCAR3, CH_NUMCAR4, CH_NUMCAR5, MOST_SERIOUS_OFFENSE2, TYPEOFFENSE, V1056, V1057, V1060, V1061, V1325, V0001, V0014, AGE_CAT, V2982, V0005, EDUCATION, SES_INCOMEMTH, DRUG_ANY,DRUG_ANYREG,DRUG_ANYMTH, SES_PHYSSEXABUSED_EVER, SES_PARENTS_INCARCERATED, SES_FAMILY_INCARCERATED)
```

### Summary of HIV/Mental Health Diagnoses

``` r
#HIV Data Summary:

federal_df %>%
  group_by(V2288)%>%
  summarize(n_obs = n())
```

    ## # A tibble: 5 × 2
    ##   V2288          n_obs
    ##   <fct>          <int>
    ## 1 (1) Yes         2673
    ## 2 (2) No           298
    ## 3 (7) Don't know   398
    ## 4 (8) Refused        1
    ## 5 <NA>             316

``` r
#2.673/3,686 federal inmates have been tested for HIV

state_df %>%
  group_by(V2288)%>%
  summarize(n_obs = n())
```

    ## # A tibble: 5 × 2
    ##   V2288          n_obs
    ##   <fct>          <int>
    ## 1 (1) Yes         9641
    ## 2 (2) No          1696
    ## 3 (7) Don't know  1122
    ## 4 (8) Refused        6
    ## 5 <NA>            2034

``` r
#9,641/14,499 state inmates have been tested for HIV

federal_df %>%
  group_by(MCMH_RSLT_HIV)%>%
  summarize(n_obs = n())
```

    ## # A tibble: 3 × 2
    ##   MCMH_RSLT_HIV                                         n_obs
    ##   <fct>                                                 <int>
    ## 1 (0000001) At least one positive test                     35
    ## 2 (0000002) At least one negative and the other unknown  2909
    ## 3 (9999999) Missing                                       742

``` r
#35 federal inmates have had at least 1 positive HIV test

state_df %>%
  group_by(MCMH_RSLT_HIV)%>%
  summarize(n_obs = n())
```

    ## # A tibble: 3 × 2
    ##   MCMH_RSLT_HIV                                         n_obs
    ##   <fct>                                                 <int>
    ## 1 (0000001) At least one positive test                    193
    ## 2 (0000002) At least one negative and the other unknown 11399
    ## 3 (9999999) Missing                                      2907

``` r
#193 state inmates have had at least 1 positive HIV test

federal_df %>%
  group_by(MCMH_RSLT_HIV, MCMH_MENTAL_HISTORY)%>%
  summarize(n_obs = n())
```

    ## `summarise()` has grouped output by 'MCMH_RSLT_HIV'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 10 × 3
    ## # Groups:   MCMH_RSLT_HIV [3]
    ##    MCMH_RSLT_HIV                                         MCMH_MENTAL_HIS…¹ n_obs
    ##    <fct>                                                 <fct>             <int>
    ##  1 (0000001) At least one positive test                  (0000001) Yes         6
    ##  2 (0000001) At least one positive test                  (0000002) No         29
    ##  3 (0000002) At least one negative and the other unknown (0000001) Yes       628
    ##  4 (0000002) At least one negative and the other unknown (0000002) No       2261
    ##  5 (0000002) At least one negative and the other unknown (9999997) DK/ref…    14
    ##  6 (0000002) At least one negative and the other unknown (9999999) Blank       6
    ##  7 (9999999) Missing                                     (0000001) Yes       126
    ##  8 (9999999) Missing                                     (0000002) No        539
    ##  9 (9999999) Missing                                     (9999997) DK/ref…    29
    ## 10 (9999999) Missing                                     (9999999) Blank      48
    ## # … with abbreviated variable name ¹​MCMH_MENTAL_HISTORY

``` r
#6/35 federal inmates with HIV have been told they have some mental health diagnosis

state_df %>%
  group_by(MCMH_RSLT_HIV, MCMH_MENTAL_HISTORY)%>%
  summarize(n_obs = n())
```

    ## `summarise()` has grouped output by 'MCMH_RSLT_HIV'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 10 × 3
    ## # Groups:   MCMH_RSLT_HIV [3]
    ##    MCMH_RSLT_HIV                                         MCMH_MENTAL_HIS…¹ n_obs
    ##    <fct>                                                 <fct>             <int>
    ##  1 (0000001) At least one positive test                  (0000001) Yes        78
    ##  2 (0000001) At least one positive test                  (0000002) No        115
    ##  3 (0000002) At least one negative and the other unknown (0000001) Yes      3348
    ##  4 (0000002) At least one negative and the other unknown (0000002) No       8015
    ##  5 (0000002) At least one negative and the other unknown (9999997) DK/ref…    25
    ##  6 (0000002) At least one negative and the other unknown (9999999) Blank      11
    ##  7 (9999999) Missing                                     (0000001) Yes       698
    ##  8 (9999999) Missing                                     (0000002) No       2038
    ##  9 (9999999) Missing                                     (9999997) DK/ref…    65
    ## 10 (9999999) Missing                                     (9999999) Blank     106
    ## # … with abbreviated variable name ¹​MCMH_MENTAL_HISTORY

``` r
#6/35 federal inmates with HIV have been told they have some health diagnosis

state_df %>%
  group_by(MCMH_RSLT_HIV)%>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count))) %>%
  knitr::kable()
```

| MCMH_RSLT_HIV                                           | count |   percent |
|:--------------------------------------------------------|------:|----------:|
| \(0000001\) At least one positive test                  |   193 | 0.0133113 |
| \(0000002\) At least one negative and the other unknown | 11399 | 0.7861922 |
| \(9999999\) Missing                                     |  2907 | 0.2004966 |

### Relationships between HIV Status and Mental Health

``` r
#Are mental health disorders more prevalent among those with HIV?

#Federal:
federal_df %>%
  group_by(MCMH_RSLT_HIV, MCMH_MENTAL_HISTORY)%>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count))) %>%
  knitr::kable()
```

    ## `summarise()` has grouped output by 'MCMH_RSLT_HIV'. You can override using the
    ## `.groups` argument.

| MCMH_RSLT_HIV                                           | MCMH_MENTAL_HISTORY    | count |   percent |
|:--------------------------------------------------------|:-----------------------|------:|----------:|
| \(0000001\) At least one positive test                  | \(0000001\) Yes        |     6 | 0.1714286 |
| \(0000001\) At least one positive test                  | \(0000002\) No         |    29 | 0.8285714 |
| \(0000002\) At least one negative and the other unknown | \(0000001\) Yes        |   628 | 0.2158817 |
| \(0000002\) At least one negative and the other unknown | \(0000002\) No         |  2261 | 0.7772430 |
| \(0000002\) At least one negative and the other unknown | \(9999997\) DK/refused |    14 | 0.0048127 |
| \(0000002\) At least one negative and the other unknown | \(9999999\) Blank      |     6 | 0.0020626 |
| \(9999999\) Missing                                     | \(0000001\) Yes        |   126 | 0.1698113 |
| \(9999999\) Missing                                     | \(0000002\) No         |   539 | 0.7264151 |
| \(9999999\) Missing                                     | \(9999997\) DK/refused |    29 | 0.0390836 |
| \(9999999\) Missing                                     | \(9999999\) Blank      |    48 | 0.0646900 |

``` r
#NO; About 17% of inmates with HIV have a mental health disorder while 22% of inmates without HIV have a mental health disorder

#State:
state_df %>%
  group_by(MCMH_RSLT_HIV, MCMH_MENTAL_HISTORY)%>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count))) %>%
  knitr::kable()
```

    ## `summarise()` has grouped output by 'MCMH_RSLT_HIV'. You can override using the
    ## `.groups` argument.

| MCMH_RSLT_HIV                                           | MCMH_MENTAL_HISTORY    | count |   percent |
|:--------------------------------------------------------|:-----------------------|------:|----------:|
| \(0000001\) At least one positive test                  | \(0000001\) Yes        |    78 | 0.4041451 |
| \(0000001\) At least one positive test                  | \(0000002\) No         |   115 | 0.5958549 |
| \(0000002\) At least one negative and the other unknown | \(0000001\) Yes        |  3348 | 0.2937100 |
| \(0000002\) At least one negative and the other unknown | \(0000002\) No         |  8015 | 0.7031319 |
| \(0000002\) At least one negative and the other unknown | \(9999997\) DK/refused |    25 | 0.0021932 |
| \(0000002\) At least one negative and the other unknown | \(9999999\) Blank      |    11 | 0.0009650 |
| \(9999999\) Missing                                     | \(0000001\) Yes        |   698 | 0.2401101 |
| \(9999999\) Missing                                     | \(0000002\) No         |  2038 | 0.7010664 |
| \(9999999\) Missing                                     | \(9999997\) DK/refused |    65 | 0.0223598 |
| \(9999999\) Missing                                     | \(9999999\) Blank      |   106 | 0.0364637 |

``` r
#YES; About 40% of those diagnoses with HIV have a mental health disorder, while about 29% without HIV have a mental health disorder

#Is this difference significance?

#2 proportion z-test
z_test <- prop.test(x = c(78, 3348), n = c(193, 11399))

z_test$p.value
```

    ## [1] 0.001135034

``` r
# The p-value is 0.001135
```

At a significance level of 0.05, we can REJECT the null hypothesis and
conclude that there is a significant difference in the prevalence of
mental health disorders between HIV positive vs. HIV negative
individuals in the state prison system.

### Graphs:

``` r
federal_bar = 
  federal_df %>%
  group_by(MCMH_RSLT_HIV, MCMH_MENTAL_HISTORY)%>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count))) %>%
      filter(MCMH_RSLT_HIV != "(9999999) Missing",
           MCMH_MENTAL_HISTORY == "(0000001) Yes") %>%
  mutate(HIV_status = recode(MCMH_RSLT_HIV, 
      "(0000001) At least one positive test" = "Positive", 
      "(0000002) At least one negative and the other unknown" = "Negative")) %>%
  ggplot(aes(x = HIV_status, y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title="Federal",
        x ="HIV Status", y = "Percent with Mental Health Disorder") +
  ylim(0, 0.5)
```

    ## `summarise()` has grouped output by 'MCMH_RSLT_HIV'. You can override using the
    ## `.groups` argument.

``` r
state_bar = 
  state_df %>%
  group_by(MCMH_RSLT_HIV, MCMH_MENTAL_HISTORY)%>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count))) %>%
      filter(MCMH_RSLT_HIV != "(9999999) Missing",
           MCMH_MENTAL_HISTORY == "(0000001) Yes") %>%
  mutate(HIV_status = recode(MCMH_RSLT_HIV, 
      "(0000001) At least one positive test" = "Positive", 
      "(0000002) At least one negative and the other unknown" = "Negative")) %>%
  ggplot(aes(x = HIV_status, y = percent)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title="State",
        x ="HIV Status", y = "Percent with Mental Health Disorder") +
  ylim(0, 0.5)
```

    ## `summarise()` has grouped output by 'MCMH_RSLT_HIV'. You can override using the
    ## `.groups` argument.

``` r
#Mental Health Disorder Prevalence by HIV Status at the Federal & State Levels(2004)
federal_bar + state_bar
```

<img src="Tama---EDA_files/figure-gfm/graphs-1.png" width="90%" />
