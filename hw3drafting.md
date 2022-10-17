hw3drafting
================

## Problem 0

-   created repository and R project for HW3, created rmd file and
    rending to GitHub.
-   created a sub-directory/ data folder that has all the data set files
    to be used for this HW

``` r
library(tidyverse)
library(readxl)
library(dplyr)
library(ggridges)
library(patchwork)
library(p8105.datasets)
options(tibble.print_min = 5)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Problem 1

reading the data

``` r
data("instacart")

instacart = 
  instacart %>% 
  as_tibble(instacart)
```

To see the number of aisles, and the aisles the items were most ordered
from:

``` r
instacart %>% 
  count(aisle) %>% 
  arrange(desc(n))
```

    ## # A tibble: 134 × 2
    ##   aisle                           n
    ##   <chr>                       <int>
    ## 1 fresh vegetables           150609
    ## 2 fresh fruits               150473
    ## 3 packaged vegetables fruits  78493
    ## 4 yogurt                      55240
    ## 5 packaged cheese             41699
    ## # … with 129 more rows

To plot for the number of items ordered in each aisle:

``` r
instacart %>% 
  count(aisle) %>% 
  filter(n > 10000) %>% 
  mutate(aisle = fct_reorder(aisle, n)) %>% 
  ggplot(aes(x = aisle, y = n)) + 
  geom_point() + 
  labs(title = "Number of items ordered in each aisle") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

<img src="hw3drafting_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

To see the three most popular items in each of the aisles “baking
ingredients”, “dog food care”, and “packaged vegetables fruits”:

``` r
instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>%
  group_by(aisle) %>% 
  count(product_name) %>% 
  mutate(rank = min_rank(desc(n))) %>% 
  filter(rank < 4) %>% 
  arrange(desc(n)) %>%
  knitr::kable()
```

| aisle                      | product_name                                  |    n | rank |
|:---------------------------|:----------------------------------------------|-----:|-----:|
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |

To make table showing the mean hour of the day at which Pink Lady Apples
and Coffee Ice Cream are ordered on each day of the week

``` r
instacart %>%
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>%
  summarize(mean_hour = mean(order_hour_of_day)) %>%
  spread(key = order_dow, value = mean_hour) %>%
  knitr::kable(digits = 2)
```

| product_name     |     0 |     1 |     2 |     3 |     4 |     5 |     6 |
|:-----------------|------:|------:|------:|------:|------:|------:|------:|
| Coffee Ice Cream | 13.77 | 14.32 | 15.38 | 15.32 | 15.22 | 12.26 | 13.83 |
| Pink Lady Apples | 13.44 | 11.36 | 11.70 | 14.25 | 11.55 | 12.78 | 11.94 |

This data set contains 1384617 rows and 15 columns, with each row
representing a single product from an instacart order. Variables include
identifiers for user, order, and product; the order in which each
product was added to the cart. There are several order-level variables,
describing the day and time of the order, and number of days since prior
order. Then there are several item-specific variables, describing the
product name (e.g. Yogurt, Avocado), department (e.g. dairy and eggs,
produce), and aisle (e.g. yogurt, fresh fruits), and whether the item
has been ordered by this user in the past. In total, there are 39123
products found in 131209 orders from 131209 distinct users.

### Answers

-   There are 134 aisles, and the items were mostly ordered from ‘Fresh
    Vegetables’ and ‘Fresh Fruits’ aisles.
-   Graph plotted for number of items ordered in each aisle
-   The number of times the items were ordered in the aisle:
-   packaged vegetables fruits: organic baby spinach: 9784 times,
    organic raspberries: 5546 times, organic blueberries: 4966 times
-   baking ingredients: light brown sugar: 499 times, pure baking soda:
    387 times, cane sugar: 336 times
-   dog food care: snack sticks chicken and rice recipe dog treats: 30
    times, organic chicken and brown rice recipe: 28 times, small dog
    biscuits: 26 times
-   The mean hour of the day at which Coffee ice cream and Pink lady
    apples was delivered, displayed during the week.

## Problem 2

Loading and tidying the data, adding new variable

``` r
acc_ds = read_csv(
    "data/accel_data.csv") %>%
  janitor::clean_names() %>%
  pivot_longer (activity_1:activity_1440,
names_to = "minute_act",
names_prefix = "activity_",
values_to = "physical_activity") %>%
  mutate (minute_act = as.numeric (minute_act),
          wd_vs_wknd = case_when (day == "Monday" ~ "weekday", day == "Tuesday" ~ "weekday", day == "Wednesday" ~ "weekday", day == "Thursday" ~ "weekday", day == "Friday" ~ "weekday", day == "Saturday" ~ "weekend",day == "Sunday" ~ "weekend"))
```

|                                                  |        |
|:-------------------------------------------------|:-------|
| Name                                             | acc_ds |
| Number of rows                                   | 50400  |
| Number of columns                                | 6      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| character                                        | 2      |
| numeric                                          | 4      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| day           |         0 |             1 |   6 |   9 |     0 |        7 |          0 |
| wd_vs_wknd    |         0 |             1 |   7 |   7 |     0 |        2 |          0 |

**Variable type: numeric**

| skim_variable     | n_missing | complete_rate |   mean |     sd |  p0 |    p25 |   p50 |     p75 | p100 | hist  |
|:------------------|----------:|--------------:|-------:|-------:|----:|-------:|------:|--------:|-----:|:------|
| week              |         0 |             1 |   3.00 |   1.41 |   1 |   2.00 |   3.0 |    4.00 |    5 | ▇▇▇▇▇ |
| day_id            |         0 |             1 |  18.00 |  10.10 |   1 |   9.00 |  18.0 |   27.00 |   35 | ▇▇▇▇▇ |
| minute_act        |         0 |             1 | 720.50 | 415.70 |   1 | 360.75 | 720.5 | 1080.25 | 1440 | ▇▇▇▇▇ |
| physical_activity |         0 |             1 | 267.04 | 443.16 |   1 |   1.00 |  74.0 |  364.00 | 8982 | ▇▁▁▁▁ |

Describing the dataset: - The total number of observations/rows are
**50400** and the total number of variables/columns are **6** - The key
variables in this data set are **week, day_id, day, minute_act,
physical_activity, wd_vs_wknd**

Aggregating across minutes to create a total activity variable for each
day, and creating a table showing these totals

``` r
agg_acc_ds = acc_ds %>%
  group_by(week,day) %>%
  summarise (total_activity = sum(physical_activity)) %>%
  pivot_wider(names_from = day,
    values_from = total_activity) %>%
select ("week","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  
agg_acc_ds %>%
  knitr::kable()
```

| week |    Monday |  Tuesday | Wednesday | Thursday |   Friday | Saturday | Sunday |
|-----:|----------:|---------:|----------:|---------:|---------:|---------:|-------:|
|    1 |  78828.07 | 307094.2 |    340115 | 355923.6 | 480542.6 |   376254 | 631105 |
|    2 | 295431.00 | 423245.0 |    440962 | 474048.0 | 568839.0 |   607175 | 422018 |
|    3 | 685910.00 | 381507.0 |    468869 | 371230.0 | 467420.0 |   382928 | 467052 |
|    4 | 409450.00 | 319568.0 |    434460 | 340291.0 | 154049.0 |     1440 | 260617 |
|    5 | 389080.00 | 367824.0 |    445366 | 549658.0 | 620860.0 |     1440 | 138421 |

-   **according to this, the physical activity is increasing throughout
    the weekdays from Monday through Friday, is the least on Saturday,
    and increases again on Sundays.**

``` r
plot_acc_ds = acc_ds %>%
  mutate (hour = minute_act/60)

ggplot(plot_acc_ds, aes(x = hour, y = physical_activity, color = day)) + 
  geom_point (alpha = 0.5) + geom_line()
```

<img src="hw3drafting_files/figure-gfm/making plot-1.png" width="90%" />

``` r
  theme(legend.position = "none")+
  labs(
    title = "physical activity by hour for each day",
    x = "Hour",
    y = "Physical activity")
```

    ## List of 4
    ##  $ legend.position: chr "none"
    ##  $ x              : chr "Hour"
    ##  $ y              : chr "Physical activity"
    ##  $ title          : chr "physical activity by hour for each day"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

-   **According to this, the physical activity increases everyday after
    5:00 am and is maintained throughout the day till around 19:00 hrs.
    It peaks between about 19:00 to 22:00 hrs and then gradually
    subsides. The period from \~ 23:00 to 5:00 hrs in the morning is the
    lowest (implying the person sleeps during that time). The activity
    around 19:00 hrs is the highest for Wednesday.The activity between
    around 19:00 through 22:00 hrs is the highest for Fridays. Daytime
    activity \~10:00 - \~12:00 is high for Sunday.**

## Problem 3

Reading ny_noaa dataset

``` r
data("ny_noaa")

ny_noaa = ny_noaa %>%
 as_tibble (ny_noaa)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | ny_noaa |
| Number of rows                                   | 2595176 |
| Number of columns                                | 7       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 3       |
| Date                                             | 1       |
| numeric                                          | 3       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| id            |         0 |          1.00 |  11 |  11 |     0 |      747 |          0 |
| tmax          |   1134358 |          0.56 |   1 |   4 |     0 |      532 |          0 |
| tmin          |   1134420 |          0.56 |   1 |   4 |     0 |      548 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 1981-01-01 | 2010-12-31 | 1997-01-21 |    10957 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |     sd |  p0 | p25 | p50 | p75 |  p100 | hist  |
|:--------------|----------:|--------------:|------:|-------:|----:|----:|----:|----:|------:|:------|
| prcp          |    145838 |          0.94 | 29.82 |  78.18 |   0 |   0 |   0 |  23 | 22860 | ▇▁▁▁▁ |
| snow          |    381221 |          0.85 |  4.99 |  27.22 | -13 |   0 |   0 |   0 | 10160 | ▇▁▁▁▁ |
| snwd          |    591786 |          0.77 | 37.31 | 113.54 |   0 |   0 |   0 |   0 |  9195 | ▇▁▁▁▁ |

cleaning the data

``` r
ny_noaa_clean = ny_noaa %>%
  janitor::clean_names() %>%
  separate(col= date, into = c("year", "month", "day"), sep ='-', convert = TRUE) %>%
  mutate (month = month.abb[month],
    tmax = as.numeric(tmax),
          tmin = as.numeric(tmin),
          prcp = prcp/10) %>%
  select (id, year, month, day, everything ()) %>%
  mutate (tmax = tmax/10, tmin = tmin/10)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | ny_noaa_clean |
| Number of rows                                   | 2595176       |
| Number of columns                                | 9             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 2             |
| numeric                                          | 7             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| id            |         0 |             1 |  11 |  11 |     0 |      747 |          0 |
| month         |         0 |             1 |   3 |   3 |     0 |       12 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |     sd |     p0 |    p25 |    p50 |    p75 |  p100 | hist  |
|:--------------|----------:|--------------:|--------:|-------:|-------:|-------:|-------:|-------:|------:|:------|
| year          |         0 |          1.00 | 1996.50 |   9.19 | 1981.0 | 1988.0 | 1997.0 | 2005.0 |  2010 | ▆▆▅▅▇ |
| day           |         0 |          1.00 |   15.73 |   8.80 |    1.0 |    8.0 |   16.0 |   23.0 |    31 | ▇▇▇▇▆ |
| prcp          |    145838 |          0.94 |    2.98 |   7.82 |    0.0 |    0.0 |    0.0 |    2.3 |  2286 | ▇▁▁▁▁ |
| snow          |    381221 |          0.85 |    4.99 |  27.22 |  -13.0 |    0.0 |    0.0 |    0.0 | 10160 | ▇▁▁▁▁ |
| snwd          |    591786 |          0.77 |   37.31 | 113.54 |    0.0 |    0.0 |    0.0 |    0.0 |  9195 | ▇▁▁▁▁ |
| tmax          |   1134358 |          0.56 |   13.98 |  11.14 |  -38.9 |    5.0 |   15.0 |   23.3 |    60 | ▁▂▇▆▁ |
| tmin          |   1134420 |          0.56 |    3.03 |  10.40 |  -59.4 |   -3.9 |    3.3 |   11.1 |    60 | ▁▁▇▂▁ |

-   The most commonly observed values for snowfall are **0**. It states
    that **there was no snowfall on most number of days.**

making 2-panel plots for avg max temperature:

``` r
ny_noaa_clean_plot1 =  ny_noaa_clean %>% 
  group_by(id, year, month) %>%
  summarise(tmax_avg = mean(tmax), na.rm = TRUE)%>%
  filter (month %in% c ("Jan", "Jul")) %>%
  ggplot(aes(x = year, y = tmax_avg, color = id)) + 
  geom_point(alpha = 0.5) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1))+
  facet_grid(.~ month)+
  labs(
    title = "Avg Max Temp plot for Jan and July",
    x = "Year",
    y = "Average Max temperature (C)")

ny_noaa_clean_plot1
```

<img src="hw3drafting_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

-   According to this, it can be seen that **the average maximum
    temperature was lower overall in January as compared to July for the
    years 1981-2010**. There is an **outlier for the month of July
    \~1988 when the maximum temperature was \~ 14 degree Celsius**.
    Also, **the range of average maximum temperatures is wider for
    January than the range of average maximum temperatures for July**.

making 2-panel plots for tmax tmin and snowfall

``` r
tmax_tmin = ny_noaa_clean %>%
  ggplot(aes(x = tmax, y = tmin)) + 
  geom_hex () + 
  theme (legend.position = "right")+
  labs (
    title = "tmax vs tmin",
    x = "tmax (C)",
    y = "tmin (C)")

tmax_tmin
```

<img src="hw3drafting_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

``` r
snowfall_year = ny_noaa_clean %>%
  group_by (year) %>%
  filter (snow %in% 1.0:99.0) %>%
  ggplot(aes(x = snow, y = as.factor(year))) + 
  geom_density_ridges() + 
  theme(legend.position = "none")+
  labs(
    title = "Snowfall Distribution by Years",
    x = "Snowfall (mm)",
    y = "Year")

snowfall_year
```

<img src="hw3drafting_files/figure-gfm/unnamed-chunk-9-2.png" width="90%" />

``` r
tmax_tmin + snowfall_year
```

<img src="hw3drafting_files/figure-gfm/unnamed-chunk-9-3.png" width="90%" />

-   According
