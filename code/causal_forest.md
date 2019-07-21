Causal Forest
================
Denis Maciel

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.0     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.1
    ## ✔ tidyr   0.8.3     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
source(here::here("code", "load_datasets.R"))
load_datasets()
```

``` r
df <- emergency %>% 
  left_join(descriptive) %>% 
  left_join(state_programs) %>% 
  left_join(survey0) %>% 
  mutate(medicaid = ohp_all_ever_firstn_30sep2009) # varibel used by Taubman as Medicaid
```

    ## Joining, by = "person_id"
    ## Joining, by = "person_id"
    ## Joining, by = "person_id"

## Feature Selection

Remove variables measured after the lottery. We want to be able to
predict emergency department visits only with information before the
lottery.

``` r
ind <- which(
  !stringr::str_detect(colnames(df), "_ed") |
    stringr::str_detect(colnames(df), "pre_ed") |
    stringr::str_detect(colnames(df), "num_visit_cens_ed")
) 

df <- df[ , ind]
```

Remove columns with more than 5% NAs.

``` r
ind <- which(
  map_dbl(df, function(col) sum(is.na(col)) / nrow(df)) < 0.05
)

df <- df[ , ind]
```

Calculate \# of unique values and type of columms

``` r
col_stats <- df %>% 
  map(function(col) 
    tibble(length = length(unique(col)), type = class(col))
  ) %>%
  reduce(bind_rows) %>% 
  mutate(col = colnames(df))

col_stats
```

    ## # A tibble: 62 x 3
    ##    length type    col              
    ##     <int> <chr>   <chr>            
    ##  1  24646 numeric person_id        
    ##  2      2 factor  any_visit_pre_ed 
    ##  3     24 numeric num_visit_cens_ed
    ##  4      2 factor  any_hosp_pre_ed  
    ##  5      2 factor  any_out_pre_ed   
    ##  6      2 factor  any_on_pre_ed    
    ##  7      2 factor  any_off_pre_ed   
    ##  8   1353 numeric num_edcnnp_pre_ed
    ##  9    287 numeric num_edcnpa_pre_ed
    ## 10   2049 numeric num_epct_pre_ed  
    ## # … with 52 more rows

Unclear meaning

``` r
df <- df %>% 
  select(-starts_with("tanf_"),
         -starts_with("snap_"))

# If the respondents ever participated in OHP before 2009, i.e. after lottery
df <- df %>% 
  select(-starts_with("ohp_"))
```

``` r
to_rmv <- c(
  "household_id", 
  "person_id",
  "draw_lottery", # The draw (from 1 to 8) one was picked should not be relevant
  "dt_retro_coverage", # Date where the coverage retroactively begins (probably coincides with lottery draw)
  "dt_notify_lottery", # Notification date about lottery 
  "postn_death", # Unclear 
  "zip_msa_list" # Has only one value across whole dataset
)

to_keep <- setdiff(colnames(df), to_rmv)

df <- df[, to_keep]
```

``` r
df <- df %>% 
  mutate(age_2009 = 2009 - birthyear_list) %>% 
  select(-birthyear_list)
```

## Drops all NAs

``` r
df <- drop_na(df)
```

## Mutate Variables

``` r
# Number of household in the list
df <- df %>% 
  mutate(numhh_list = dplyr::case_when(
    numhh_list == "signed self up" ~ 0L,
    numhh_list == "signed self up + 1 additional person" ~ 1L,
    numhh_list == "signed self up + 2 additional people" ~ 2L
  ))

# Treatment binary and numeric
df <- df %>% 
  mutate(treatment = case_when(
    treatment == "Selected" ~ 1L,
    treatment == "Not selected" ~ 0L
  ))

df <- df %>% 
  mutate(medicaid = case_when(
    medicaid == "Enrolled" ~ 1L,
    medicaid == "NOT enrolled" ~ 0L
  ))

stopifnot(
  all(!is.na(df$numhh_list))
)
stopifnot(
  all(!is.na(df$medicaid))
)
stopifnot(
  all(!is.na(df$treatment))
)
```

Train vs test and preprocesing

``` r
TRAIN_PROPORTION <- 0.8
ind <- sample(1:nrow(df), round(TRAIN_PROPORTION*nrow(df)), replace = FALSE)

train <- df[ind, ]
test <- df[-ind, ]

library(recipes)
```

    ## 
    ## Attaching package: 'recipes'

    ## The following object is masked from 'package:stringr':
    ## 
    ##     fixed

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
rec <- recipe(num_visit_cens_ed ~ ., data = train %>% select(-medicaid, -treatment)) %>% 
  step_dummy(all_nominal())

prepped_rec <- prep(rec, train)

X_train <- juice(prepped_rec)
X_test <- bake(prepped_rec, new_data = test)
```

## Fit Model

``` r
library(grf)

cf <- instrumental_forest(
  X = X_train,
  Y = train$num_visit_cens_ed,
  W = train$medicaid,
  Z = train$treatment,
  honesty = TRUE
)

# tree <- get_tree(cf, 1)
```

Predict on test set

``` r
pred_df <- predict(cf,
                   newdata = X_test,
                   estimate.variance = TRUE)
```

Estimated treatment effects ranked

``` r
p_rank <- pred_df %>% 
  arrange(predictions) %>% 
  mutate(idx = row_number()) %>% 
  ggplot(aes(idx, predictions)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Rank",
       y = "Estimated Treatment Effect") +
  # expand_limits(y = c(-5, 5)) +
  theme_light() 

ggsave(plot = p_rank, here::here("figs", "ete_p_rank.png"))
```

    ## Saving 7 x 5 in image

``` r
p_rank 
```

![](causal_forest_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Max and min causal effect

``` r
max(pred_df$predictions)
```

    ## [1] 0.6906325

``` r
min(pred_df$predictions)
```

    ## [1] -0.1409223

``` r
summary(pred_df$predictions)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -0.1409  0.4191  0.5468  0.4852  0.6068  0.6906

Estimated treatment effect with confidence intervals

``` r
plot_htes <- function(cf_preds, z = 1.96) {
  ggplot(cf_preds, aes(rank(cf_preds$predictions), cf_preds$predictions)) +
    geom_point() +
    labs(x = "Rank", y = "Estimated Treatment Effect") +
    theme_light() +
    geom_errorbar(aes(
      ymin = cf_preds$predictions + z * sqrt(cf_preds$variance.estimates),
      ymax = cf_preds$predictions - z * sqrt(cf_preds$variance.estimates))
    )
}

# pred_df %>% 
#   # sample_frac(size = 1/100) %>% 
#   plot_htes(cf_preds =., ci = TRUE)

pred_df_sampled <- sample_n(pred_df, 200)
p_rank_interval <- plot_htes(pred_df_sampled)
ggsave(plot = p_rank_interval, here::here("figs", "ete_p_rank_interval.png"))
```

    ## Saving 7 x 5 in image

``` r
p_rank_interval
```

![](causal_forest_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Histogram (geom\_col)

``` r
p_hist <- pred_df %>% 
  bind_cols(test) %>% 
  mutate(bins = cut(predictions, breaks = seq(-5, 5, 0.1))) %>% 
  count(bins, medicaid) %>% 
  ggplot(aes(bins, n)) +
  # ggplot(aes(bins, n, fill = factor(medicaid))) +
  geom_col() +
  labs(x = "Bins", y = "") +
  theme_light() 

ggsave(plot = p_hist, here::here("figs", "ete_histogram.png"))
```

    ## Saving 7 x 5 in image

``` r
p_hist
```

![](causal_forest_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Feature importance

``` r
feat_importance <- variable_importance(cf) %>% 
  as_tibble() %>% 
  rename(importance = V1) %>% 
  mutate(feature = colnames(X_train)) %>% 
  arrange(desc(importance))
```

    ## Warning: `as_tibble.matrix()` requires a matrix with column names or a `.name_repair` argument. Using compatibility `.name_repair`.
    ## This warning is displayed once per session.

``` r
feat_importance
```

    ## # A tibble: 44 x 2
    ##    importance feature             
    ##         <dbl> <chr>               
    ##  1     0.210  num_visit_cens_ed   
    ##  2     0.133  charg_tot_pre_ed    
    ##  3     0.119  ed_charg_tot_pre_ed 
    ##  4     0.0976 num_epct_pre_ed     
    ##  5     0.0867 num_ne_pre_ed       
    ##  6     0.0849 num_edcnpa_pre_ed   
    ##  7     0.0791 num_edcnnp_pre_ed   
    ##  8     0.0340 any_chron_pre_ed_Yes
    ##  9     0.0337 num_unclas_pre_ed   
    ## 10     0.0252 any_hosp_pre_ed_Yes 
    ## # … with 34 more rows

``` r
write_csv(feat_importance, here::here("figs", "feat_importance.csv"))

# xtable::xtable(feat_importance)
```
