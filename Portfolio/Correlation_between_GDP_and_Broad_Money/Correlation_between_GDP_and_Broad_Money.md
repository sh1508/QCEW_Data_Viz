Correlation between GDP and Broad Money
================
Seiji Hirano
2022-12-18

## Setting Up My Environment

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggpubr)

oecd_avgwage <- read_csv('/Users/hiranoseiji/Downloads/Data_Analysis/Macro_Economic_Wage_Analysis/oecd_avgwage.csv')
```

    ## Rows: 1079 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): country, indicator
    ## dbl (2): year, avg_wage
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
picked_country <- c('AUS', 'CAN', 'DEU', 'FRA', 'GBR', 'ITA', 'JPN', 'USA')
```

# About This Project

**Average Wages** is one of the most important indicators in economics.
Many organizations target employee pay at the average rate to ensure
employees receive competitive compensation while the business is still
managing overall costs. <br> <br> The graph below shows changes in
Average Wages over 30 years in 8 OECD countries. While Average Wages
have increased from time to time in most countries, Average Wages in
countries like **Japan** have remained almost the same for about 30
years. <br> <br>

``` r
ggplot(subset(oecd_avgwage, country %in% picked_country), aes(year, avg_wage)) +
  geom_line(aes(color=country))
```

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/OECD%20Average%20Wages%20Data-1.png)<!-- -->

To explain the discrepancies between these countries, I decided to find
out **what indicator is the most correlated to wage increases.**

## Alternative Indicator of Average Wages

Unfortunately, I was not able to find data on Average Wages on a global
scale, so I will use **Nominal GDP** data as an alternative indicator
(It is difficult for companies to increase wages when the country’s
economy is not growing. In other words, Average Wages tend to not grow
when the GDP does not grow).

## Analysis

So, **what should a country do to increase its GDP and, in turn, its
average wages?** <br> <br> Through analyzing many countries and
indicators and their correlation to GDP growth, I concluded that **broad
money growth has the strongest correlation to GDP growth.** <br> <br>
Let me show you my analysis and visualizations below.

### Read a CSV file

``` r
wb_gdp_n_broadmoney <- read_csv('/Users/hiranoseiji/Downloads/Data_Analysis/Macro_Economic_Wage_Analysis/worldbank_gdp_n_broadmoney.csv')
```

    ## Rows: 7499 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): code, name, income_group
    ## dbl (4): year, gdp, bm_percent, broad_money
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<br> I manipulated the data using PostgreSQL and created a CSV file that
contains data on GDP and broad money of over 150 countries. <br> <br>

``` r
head(wb_gdp_n_broadmoney)
```

    ## # A tibble: 6 × 7
    ##   code  name   year income_group        gdp bm_percent broad_money
    ##   <chr> <chr> <dbl> <chr>             <dbl>      <dbl>       <dbl>
    ## 1 ABW   Aruba  1986 High income  405586592.       43.5  176368715.
    ## 2 ABW   Aruba  1987 High income  487709497.       51.3  250223464.
    ## 3 ABW   Aruba  1988 High income  596648045.       49.3  293910615.
    ## 4 ABW   Aruba  1989 High income  695530726.       52.1  362625698.
    ## 5 ABW   Aruba  1990 High income  764804469.       55.6  425251397.
    ## 6 ABW   Aruba  1991 High income  872067039.       57.0  496867039.

<br> Below are **scatter plots showing correlation between GDP growth
and broad money growth in Australia, United Kingdom, United States, and
Japan** (data for German, France, and Italy was not available and I
excluded data for Canada as it only had data until 2008).

### Australia

``` r
ggscatter(subset(wb_gdp_n_broadmoney, code == "AUS"), x = "broad_money", y = "gdp", add = "reg.line") +
  stat_regline_equation(label.x = 100000000000, label.y = 1100000000000,  aes(label = ..rr.label..)) +
  stat_regline_equation(label.x = 100000000000, label.y = 1200000000000,  aes(label = ..eq.label..))
```

    ## Warning: The dot-dot notation (`..rr.label..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(rr.label)` instead.
    ## ℹ The deprecated feature was likely used in the ggpubr package.
    ##   Please report the issue at <]8;;https://github.com/kassambara/ggpubr/issueshttps://github.com/kassambara/ggpubr/issues]8;;>.

    ## `geom_smooth()` using formula = 'y ~ x'

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/visualizing%20AUS%20data-1.png)<!-- -->

### United Kingdom

``` r
ggscatter(subset(wb_gdp_n_broadmoney, code == "GBR"), x = "broad_money", y = "gdp", add = "reg.line") +
  stat_regline_equation(label.x = 250000000000, label.y = 2300000000000,  aes(label = ..rr.label..)) +
  stat_regline_equation(label.x = 250000000000, label.y = 2500000000000,  aes(label = ..eq.label..))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/visualizing%20GBR%20data-1.png)<!-- -->

### United States

``` r
ggscatter(subset(wb_gdp_n_broadmoney, code == "USA"), x = "broad_money", y = "gdp", add = "reg.line") +
  stat_regline_equation(label.x = 1300000000000, label.y = 18000000000000,  aes(label = ..rr.label..)) +
  stat_regline_equation(label.x = 1300000000000, label.y = 20000000000000,  aes(label = ..eq.label..))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/visualizing%20USA%20data-1.png)<!-- -->

### Japan

``` r
ggscatter(subset(wb_gdp_n_broadmoney, code == "JPN"), x = "broad_money", y = "gdp", add = "reg.line") +
  stat_regline_equation(label.x = 700000000000, label.y = 4400000000000,  aes(label = ..rr.label..)) +
  stat_regline_equation(label.x = 700000000000, label.y = 4800000000000,  aes(label = ..eq.label..))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/visualizing%20JPN%20data-1.png)<!-- -->
<br> <br> It is very obvious that GDP growth and Broad Money growth is
strongly correlated. **Circulating more money in the economy will help
GDP growth which will then increase average wages.** <br> <br> Here is
**Year-over-Year line graphs on broad money** for the same countries.

### Australia

``` r
ggplot(filter(wb_gdp_n_broadmoney, code == "AUS" & year %in% 1990:2020), aes(year, broad_money)) + geom_line()
```

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/AUS%20line%20chart-1.png)<!-- -->

``` r
round((filter(wb_gdp_n_broadmoney, code == "AUS" & year %in% 1990:2020)$broad_money[31] - filter(wb_gdp_n_broadmoney, code == "AUS" & year %in% 1990:2020)$broad_money[1])/filter(wb_gdp_n_broadmoney, code == "AUS" & year %in% 1990:2020)$broad_money[31]*100, digits = 2)
```

    ## [1] 90.77

Broad Money has increased by **90.77%** since 1990 in Australia.

### United Kingdom

``` r
ggplot(filter(wb_gdp_n_broadmoney, code == "GBR" & year %in% 1990:2020), aes(year, broad_money)) + geom_line()
```

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/GBR%20line%20chart-1.png)<!-- -->

``` r
round((filter(wb_gdp_n_broadmoney, code == "GBR" & year %in% 1990:2020)$broad_money[31] - filter(wb_gdp_n_broadmoney, code == "GBR" & year %in% 1990:2020)$broad_money[1])/filter(wb_gdp_n_broadmoney, code == "GBR" & year %in% 1990:2020)$broad_money[31]*100, digits = 2)
```

    ## [1] 78.99

Broad Money has increased by **78.99%** since 1990 in the United
Kingdom.

### United States

``` r
ggplot(filter(wb_gdp_n_broadmoney, code == "USA" & year %in% 1990:2020), aes(year, broad_money)) + geom_line()
```

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/USA%20line%20chart-1.png)<!-- -->

``` r
round((filter(wb_gdp_n_broadmoney, code == "USA" & year %in% 1990:2020)$broad_money[31] - filter(wb_gdp_n_broadmoney, code == "USA" & year %in% 1990:2020)$broad_money[1])/filter(wb_gdp_n_broadmoney, code == "USA" & year %in% 1990:2020)$broad_money[31]*100, digits = 2)
```

    ## [1] 81.77

Broad Money has increased by **81.77%** since 1990 in the United States.

### Japan

``` r
ggplot(filter(wb_gdp_n_broadmoney, code == "JPN" & year %in% 1990:2020), aes(year, broad_money)) + geom_line()
```

![](Correlation_between_GDP_and_Broad_Money_files/figure-gfm/JPN%20line%20chart-1.png)<!-- -->

``` r
round((filter(wb_gdp_n_broadmoney, code == "JPN" & year %in% 1990:2020)$broad_money[31] - filter(wb_gdp_n_broadmoney, code == "JPN" & year %in% 1990:2020)$broad_money[1])/filter(wb_gdp_n_broadmoney, code == "JPN" & year %in% 1990:2020)$broad_money[31]*100, digits = 2)
```

    ## [1] 58.96

Broad Money has increased by **58.96%** since 1990 in Japan. <br> <br>
<br> This explains **why average wages stagnate in countries like Japan
where broad money has not increased much in recent decades.** <br> I
also created a dashboard that contains data of other countries and
published it on Tableau. If you are interested, click
[here](https://public.tableau.com/app/profile/seiji.hirano/viz/MostCorrelatedIndicatortoCountrysAverageWagesGrowth/Dashboard1).
<br>

## Resources

[Average Wages Data from
OECD](https://data.oecd.org/earnwage/average-wages.htm)<br> [GDP Data
from World
Bank](https://data.worldbank.org/indicator/NY.GDP.MKTP.CD)<br> [Broad
Money Data from World
Bank](https://data.worldbank.org/indicator/FM.LBL.BMNY.GD.ZS)
