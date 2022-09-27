Tidy Data
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

## `pivot_longer`

load the PULSE data

``` r
pulse_data =
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names()
```

Wide formate to long format …

``` r
pulse_data_tidy =
  pulse_data %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi"
  )
```

rewrite, combine, and extend (to add a mutate)

``` r
pulse_data =
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi"
  ) %>% 
  relocate(id, visit) %>% 
  mutate(visit = recode(visit, "bl" = "00m"))
```

## `pivot_wider`

Make up some data!

``` r
analysis_result =
  tibble(
    group = c("treatment", "treatment","placebo","placebo"),
    time = c("pre", "post","pre","post"),
    mean = c(4,8,3.5,4)
  )

analysis_result %>% 
  pivot_wider(
    names_from = "time",
    values_from = "mean"
    )
```

    ## # A tibble: 2 × 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4

## Binding rows

Using the LotR data.

First step: import each table.

``` r
fellowship_ring =
  readxl::read_excel("./data/LotR_Words.xlsx" , range = "B3:D6") %>% 
  mutate(movie = "fellowship_ring")

two_towers =
  readxl::read_excel("./data/LotR_Words.xlsx" , range = "F3:H6") %>% 
  mutate(movie = "two_towers")

return_king =
  readxl::read_excel("./data/LotR_Words.xlsx" , range = "J3:L6") %>% 
  mutate(movie = "return_king")
```

Bind all the rows together

``` r
lotr_tidy =
  bind_rows(fellowship_ring, two_towers, return_king) %>% 
  janitor::clean_names() %>% 
  relocate(movie) %>% 
  pivot_longer(
    female:male,
    names_to = "gender",
    values_to = "words"
  )
```

## Joining datasets

Import and clean the FAS datasets.

``` r
pups_df =
  read_csv("./data/FAS_pups.csv") %>% 
  janitor::clean_names() %>% 
  mutate(sex = recode(sex, `1` = "male",`2` = "female"))
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df =
  read_csv("./data/FAS_litters.csv") %>% 
  janitor::clean_names() %>% 
  relocate(litter_number) %>% 
  separate(group, into = c("dose","day_of_tx"), sep =3)
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Next up, time to join them!

``` r
fas_df =
  left_join(pups_df, litters_df, by = "litter_number") %>% 
  arrange(litter_number) %>% 
  relocate(litter_number, dose, day_of_tx)
```

## Learning Assessment

``` r
surv_os = read_csv("./data/surv_os.csv") %>% 
  janitor::clean_names() %>% 
  rename(id = what_is_your_uni, os = what_operating_system_do_you_use)
```

    ## Rows: 173 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): What is your UNI?, What operating system do you use?
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
surv_pr_git = read_csv("./data/surv_program_git.csv") %>% 
  janitor::clean_names() %>% 
  rename(
    id = what_is_your_uni,
    prog = what_is_your_degree_program,
    git_exp = which_most_accurately_describes_your_experience_with_git)
```

    ## Rows: 135 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): What is your UNI?, What is your degree program?, Which most accurat...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
left_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 175 × 4
    ##    id          os         prog  git_exp                                         
    ##    <chr>       <chr>      <chr> <chr>                                           
    ##  1 student_87  <NA>       MS    Pretty smooth: needed some work to connect Git,…
    ##  2 student_106 Windows 10 Other Pretty smooth: needed some work to connect Git,…
    ##  3 student_66  Mac OS X   MPH   Smooth: installation and connection with GitHub…
    ##  4 student_93  Windows 10 MS    Smooth: installation and connection with GitHub…
    ##  5 student_99  Mac OS X   MS    Smooth: installation and connection with GitHub…
    ##  6 student_115 Mac OS X   MS    Smooth: installation and connection with GitHub…
    ##  7 student_15  Windows 10 MPH   Pretty smooth: needed some work to connect Git,…
    ##  8 student_15  Windows 10 MPH   Pretty smooth: needed some work to connect Git,…
    ##  9 student_21  Windows 10 MPH   Pretty smooth: needed some work to connect Git,…
    ## 10 student_86  Mac OS X   <NA>  <NA>                                            
    ## # … with 165 more rows

``` r
inner_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 129 × 4
    ##    id          os         prog  git_exp                                         
    ##    <chr>       <chr>      <chr> <chr>                                           
    ##  1 student_87  <NA>       MS    Pretty smooth: needed some work to connect Git,…
    ##  2 student_106 Windows 10 Other Pretty smooth: needed some work to connect Git,…
    ##  3 student_66  Mac OS X   MPH   Smooth: installation and connection with GitHub…
    ##  4 student_93  Windows 10 MS    Smooth: installation and connection with GitHub…
    ##  5 student_99  Mac OS X   MS    Smooth: installation and connection with GitHub…
    ##  6 student_115 Mac OS X   MS    Smooth: installation and connection with GitHub…
    ##  7 student_15  Windows 10 MPH   Pretty smooth: needed some work to connect Git,…
    ##  8 student_15  Windows 10 MPH   Pretty smooth: needed some work to connect Git,…
    ##  9 student_21  Windows 10 MPH   Pretty smooth: needed some work to connect Git,…
    ## 10 student_59  Windows 10 MPH   Smooth: installation and connection with GitHub…
    ## # … with 119 more rows

``` r
anti_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 46 × 2
    ##    id          os                                     
    ##    <chr>       <chr>                                  
    ##  1 student_86  Mac OS X                               
    ##  2 student_91  Windows 10                             
    ##  3 student_24  Mac OS X                               
    ##  4 student_103 Mac OS X                               
    ##  5 student_163 Mac OS X                               
    ##  6 student_68  Other (Linux, Windows, 95, TI-89+, etc)
    ##  7 student_158 Mac OS X                               
    ##  8 student_19  Windows 10                             
    ##  9 student_43  Mac OS X                               
    ## 10 student_78  Mac OS X                               
    ## # … with 36 more rows

``` r
anti_join(surv_pr_git, surv_os)
```

    ## Joining, by = "id"

    ## # A tibble: 15 × 3
    ##    id         prog  git_exp                                                     
    ##    <chr>      <chr> <chr>                                                       
    ##  1 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  2 student_17 PhD   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  3 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  4 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  5 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  6 student_53 MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  7 <NA>       MS    "Smooth: installation and connection with GitHub was easy"  
    ##  8 student_80 PhD   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  9 student_16 MPH   "Smooth: installation and connection with GitHub was easy"  
    ## 10 student_98 MS    "Smooth: installation and connection with GitHub was easy"  
    ## 11 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ## 12 <NA>       MS    "What's \"Git\" ...?"                                       
    ## 13 <NA>       MS    "Smooth: installation and connection with GitHub was easy"  
    ## 14 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ## 15 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
