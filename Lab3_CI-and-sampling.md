Geog 4/6300: Lab 3-Confidence intervals
================

## Confidence intervals

**Your name: Mary Elizabeth Rudd**

**Overview:**

In this lab, we will create confidence intervals for January
precipitation in Washington during two time periods: 1950-1969 and
2005-2024. Using county-level precipitation data from the ERA5 dataset,
you will estimate statewide precipitation patterns, then examine
variation and change at the county level. We’ve summed the precipitation
in January for each county over two twenty year time periods: 1950-1969
and 2005-2024. You’ll be using those data to create confidence intervals
at a state and county level.

To load these data, load the csv file, which is included in the data
folder:

``` r
era5_data<-read_csv("~/Downloads/Local GEOG Notes:Projects/geog4300_lab3/data/precip_uswest_era5.csv")
```

This is a long format dataset including county identifiers, county
population, median household income, and poverty rate. Census figures
are based on the 2018-22 ACS. The last two columns (year, prcp_m,
time_range) give the year and total precipitation in meters from the
ERA5 NetCDF file, and two time ranges (1950-1969 and 2005-2024).

The script used to create this dataframe is also provided in this
repository (‘lab3_dataprep’), along with rasters of the yearly mean
total precipitation for Januaries in the two time periods of interest.

For this lab to be marked complete, it must demonstrate the following
learning standards:

- 11. Create effective data visualizations across multiple chart types
      (line, scatter, histogram, Q-Q plot), applying appropriate
      aesthetic choices such as color, grouping, and labeling. (Tasks 2
      and 5)
- 12. Produce and interpret confidence interval plots that enable visual
      comparison of means and uncertainty across groups or time periods.
      (Tasks 2, 3, 5, and 6)
- 13. Visualize spatial data using appropriate mapping tools, selecting
      suitable classification schemes and cartographic elements to
      support clear communication. (Task 7)
- 14. Arrange and present tabular results in a readable format
      appropriate for a technical document or report. (Tasks 1 and 4)
- 15. Use measures of central tendency and dispersion to describe the
      variables in tabular datasets (Tasks 1 and 4)
- 20. Explaining how evidence from a sample supports conclusions about a
      population while acknowledging sampling variability and
      uncertainty. (Tasks 3 and 6)
- 22. Construct and interpret confidence intervals, explaining how
      factors such as sample size and spatial scale affect the precision
      of an estimate. (Tasks 1, 3, 4 and 6)

# Part 1: Calculating statewide confidence intervals for Washington’s precipitation

In the first part of this lab, you’ll determine a confidence interval
for Washington’s precipitation for these two time periods. This can be
helpful in identifying whether precipitation has changed across the
state (e.g., whether the two confidence intervals overlap) using the
county data as your sample.

**Task 1** *Load the data and filter it to just include data for the
state of Washington. Then create a table showing mean precipitation,
standard deviation, number of observations (sample size), and
upper/lower confidence interval bounds for each of the two time periods
in the `time_range` field. Use `kable` to display the table when you are
done.*

``` r
era5_WA<-era5_data %>%
  filter(st_name=="Washington") %>% #Filters for state of WA
  group_by(time_range) %>%
  summarise(
    mean_prcp=mean(prcp_m, na.rm = TRUE),
    sd_prcp=sd(prcp_m, na.rm = TRUE),
    observation_count=sum(!is.na(prcp_m)),
    se=sd_prcp / sqrt(observation_count),
    lower_ci=mean_prcp - qt(0.975, df = observation_count - 1) * se,
    upper_ci=mean_prcp + qt(0.975, df = observation_count - 1) * se)

era5_WA%>%
  kable()
```

| time_range | mean_prcp |   sd_prcp | observation_count |        se |  lower_ci |  upper_ci |
|:-----------|----------:|----------:|------------------:|----------:|----------:|----------:|
| 1950-69    | 0.2032239 | 0.1562948 |               780 | 0.0055963 | 0.1922383 | 0.2142094 |
| 2005-24    | 0.1664626 | 0.1230138 |               780 | 0.0044046 | 0.1578163 | 0.1751089 |

Using the code provided in class, create a ggplot graphic that
visualizes these two confidence intervals. You’ll want to use
`geom_point` and `geom_linerange` to do so. Use states as the y-axis and
mean precipitation as the x axis. Vary the color and position of the
dots and lines for the mean/CI by the `time_range` variable. Feel free
to also experiment with axis labels/themes to make the graph more
appealing and accessible.

**Task 2** *Create a ggplot graphic as described above to show the
confidence intervals for precipitation in Washington for each time
range.*

``` r
ggplot(era5_WA) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci, y = time_range, color = time_range)) +
  geom_point(aes(x = mean_prcp, y = time_range, color = time_range)) +
  scale_color_manual(values = c("#4e6d58", "#836685")) +  
  labs(x = "Mean January Precipitation (m)", y = "Time period", color = "Time period") +
  theme_minimal()
```

![](Lab3_CI-and-sampling_files/figure-gfm/Task3-1.png)<!-- -->

**Task 3** *Interpret your table and graph. What do they tell us about
the change in Washington’s precipitation during this time?*

In Washington, the mean level of precipitation in 1950-1969 was 0.203
meters, and 0.166 meters in 2005-2024. This shows a change of about
0.037 meters of rainfall. The 95% confidence interval for 1950-1969 runs
from 0.1922383 to 0.2142094, and for 2005-2024 the CI ran from 0.1578163
to 0.1751089. The intervals do not overlap, there seems to be some
evidence that the statewide mean changed between the time periods of
1950-1969 and 2005-2024.

# Part 2: Analyzing county data in Washington.

In part 1, you ran a state level analysis. What if you wanted to
identify where populations might be negatively affected by precipitation
changes within a state? In this section, you’ll look at the changes in
precipitation **at county level** in Washington to

To start with, adapt your code from step 1 so that you create confidence
intervals for **counties** instead of states. After creating that
dataset, create another ggplot graph of those confidence intervals that
you can compare to the state level data in Task 2 to examine how
precipitation has changed across counties in Washington.

**Task 4** *Create a summary table for January precipitation in
Washington counties for both time periods. Use the same approach as in
Task 1, but calculate statistics for counties rather than the state as a
whole. Call the head of your table with `kable` when you are done.*

``` r
era5_WA_CTY<-era5_data %>%
   filter(st_name =="Washington",month(year) == 1) %>%
  group_by(cty_name,time_range) %>%
  summarise(
    mean_prcp=mean(prcp_m, na.rm = TRUE),
    sd_prcp=sd(prcp_m, na.rm = TRUE),
    observation_count=sum(!is.na(prcp_m)),
    se=sd_prcp / sqrt(observation_count),
    lower_ci=mean_prcp - qt(0.975, df = observation_count - 1) * se,
    upper_ci=mean_prcp + qt(0.975, df = observation_count - 1) * se)
```

    ## `summarise()` has regrouped the output.
    ## ℹ Summaries were computed grouped by cty_name and time_range.
    ## ℹ Output is grouped by cty_name.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(cty_name, time_range))` for per-operation grouping
    ##   (`?dplyr::dplyr_by`) instead.

``` r
era5_WA_CTY %>%
  kable()
```

| cty_name | time_range | mean_prcp | sd_prcp | observation_count | se | lower_ci | upper_ci |
|:---|:---|---:|---:|---:|---:|---:|---:|
| Adams County, Washington | 1950-69 | 0.0561916 | 0.0275746 | 20 | 0.0061659 | 0.0432862 | 0.0690969 |
| Adams County, Washington | 2005-24 | 0.0455177 | 0.0183023 | 20 | 0.0040925 | 0.0369519 | 0.0540834 |
| Asotin County, Washington | 1950-69 | 0.0917109 | 0.0412774 | 20 | 0.0092299 | 0.0723925 | 0.1110293 |
| Asotin County, Washington | 2005-24 | 0.0661066 | 0.0238369 | 20 | 0.0053301 | 0.0549506 | 0.0772627 |
| Benton County, Washington | 1950-69 | 0.0439175 | 0.0199657 | 20 | 0.0044645 | 0.0345733 | 0.0532617 |
| Benton County, Washington | 2005-24 | 0.0377814 | 0.0152653 | 20 | 0.0034134 | 0.0306370 | 0.0449258 |
| Chelan County, Washington | 1950-69 | 0.2184237 | 0.0983827 | 20 | 0.0219991 | 0.1723791 | 0.2644682 |
| Chelan County, Washington | 2005-24 | 0.1817817 | 0.0613328 | 20 | 0.0137144 | 0.1530770 | 0.2104863 |
| Clallam County, Washington | 1950-69 | 0.3252100 | 0.1331571 | 20 | 0.0297748 | 0.2628905 | 0.3875295 |
| Clallam County, Washington | 2005-24 | 0.2724353 | 0.0979751 | 20 | 0.0219079 | 0.2265815 | 0.3182891 |
| Clark County, Washington | 1950-69 | 0.3545254 | 0.1621728 | 20 | 0.0362629 | 0.2786262 | 0.4304247 |
| Clark County, Washington | 2005-24 | 0.2886253 | 0.1206821 | 20 | 0.0269853 | 0.2321443 | 0.3451063 |
| Columbia County, Washington | 1950-69 | 0.1004024 | 0.0475038 | 20 | 0.0106222 | 0.0781699 | 0.1226349 |
| Columbia County, Washington | 2005-24 | 0.0771332 | 0.0302954 | 20 | 0.0067743 | 0.0629545 | 0.0913119 |
| Cowlitz County, Washington | 1950-69 | 0.3290205 | 0.1501020 | 20 | 0.0335638 | 0.2587706 | 0.3992704 |
| Cowlitz County, Washington | 2005-24 | 0.2670517 | 0.1120230 | 20 | 0.0250491 | 0.2146233 | 0.3194801 |
| Douglas County, Washington | 1950-69 | 0.0755475 | 0.0305795 | 20 | 0.0068378 | 0.0612358 | 0.0898591 |
| Douglas County, Washington | 2005-24 | 0.0677107 | 0.0237489 | 20 | 0.0053104 | 0.0565959 | 0.0788255 |
| Ferry County, Washington | 1950-69 | 0.0872902 | 0.0340067 | 20 | 0.0076041 | 0.0713745 | 0.1032058 |
| Ferry County, Washington | 2005-24 | 0.0690214 | 0.0318703 | 20 | 0.0071264 | 0.0541057 | 0.0839372 |
| Franklin County, Washington | 1950-69 | 0.0471717 | 0.0234902 | 20 | 0.0052526 | 0.0361780 | 0.0581654 |
| Franklin County, Washington | 2005-24 | 0.0396481 | 0.0162463 | 20 | 0.0036328 | 0.0320446 | 0.0472517 |
| Garfield County, Washington | 1950-69 | 0.0911108 | 0.0432525 | 20 | 0.0096716 | 0.0708681 | 0.1113536 |
| Garfield County, Washington | 2005-24 | 0.0695885 | 0.0257205 | 20 | 0.0057513 | 0.0575510 | 0.0816261 |
| Grant County, Washington | 1950-69 | 0.0515536 | 0.0214117 | 20 | 0.0047878 | 0.0415327 | 0.0615746 |
| Grant County, Washington | 2005-24 | 0.0452047 | 0.0166150 | 20 | 0.0037152 | 0.0374287 | 0.0529808 |
| Grays Harbor County, Washington | 1950-69 | 0.3744625 | 0.1624178 | 20 | 0.0363177 | 0.2984486 | 0.4504764 |
| Grays Harbor County, Washington | 2005-24 | 0.3134319 | 0.1198127 | 20 | 0.0267909 | 0.2573578 | 0.3695060 |
| Island County, Washington | 1950-69 | 0.2155585 | 0.0854027 | 20 | 0.0190966 | 0.1755888 | 0.2555282 |
| Island County, Washington | 2005-24 | 0.1810308 | 0.0653799 | 20 | 0.0146194 | 0.1504321 | 0.2116296 |
| Jefferson County, Washington | 1950-69 | 0.3949528 | 0.1741901 | 20 | 0.0389501 | 0.3134293 | 0.4764763 |
| Jefferson County, Washington | 2005-24 | 0.3272575 | 0.1196150 | 20 | 0.0267467 | 0.2712760 | 0.3832391 |
| King County, Washington | 1950-69 | 0.3027204 | 0.1330477 | 20 | 0.0297504 | 0.2404521 | 0.3649886 |
| King County, Washington | 2005-24 | 0.2557177 | 0.0919941 | 20 | 0.0205705 | 0.2126631 | 0.2987722 |
| Kitsap County, Washington | 1950-69 | 0.2298190 | 0.0985732 | 20 | 0.0220416 | 0.1836853 | 0.2759526 |
| Kitsap County, Washington | 2005-24 | 0.1797918 | 0.0671085 | 20 | 0.0150059 | 0.1483840 | 0.2111995 |
| Kittitas County, Washington | 1950-69 | 0.1483310 | 0.0667248 | 20 | 0.0149201 | 0.1171029 | 0.1795592 |
| Kittitas County, Washington | 2005-24 | 0.1245704 | 0.0403266 | 20 | 0.0090173 | 0.1056970 | 0.1434439 |
| Klickitat County, Washington | 1950-69 | 0.1327846 | 0.0603296 | 20 | 0.0134901 | 0.1045494 | 0.1610197 |
| Klickitat County, Washington | 2005-24 | 0.1060991 | 0.0408844 | 20 | 0.0091420 | 0.0869646 | 0.1252336 |
| Lewis County, Washington | 1950-69 | 0.2980728 | 0.1346273 | 20 | 0.0301036 | 0.2350653 | 0.3610803 |
| Lewis County, Washington | 2005-24 | 0.2437228 | 0.0952494 | 20 | 0.0212984 | 0.1991447 | 0.2883009 |
| Lincoln County, Washington | 1950-69 | 0.0690049 | 0.0296052 | 20 | 0.0066199 | 0.0551492 | 0.0828606 |
| Lincoln County, Washington | 2005-24 | 0.0547771 | 0.0232163 | 20 | 0.0051913 | 0.0439115 | 0.0656427 |
| Mason County, Washington | 1950-69 | 0.3667742 | 0.1645045 | 20 | 0.0367843 | 0.2897837 | 0.4437646 |
| Mason County, Washington | 2005-24 | 0.2940343 | 0.1157409 | 20 | 0.0258805 | 0.2398659 | 0.3482027 |
| Okanogan County, Washington | 1950-69 | 0.1151731 | 0.0492290 | 20 | 0.0110079 | 0.0921333 | 0.1382130 |
| Okanogan County, Washington | 2005-24 | 0.0949922 | 0.0305825 | 20 | 0.0068385 | 0.0806791 | 0.1093052 |
| Pacific County, Washington | 1950-69 | 0.2992748 | 0.1259111 | 20 | 0.0281546 | 0.2403466 | 0.3582030 |
| Pacific County, Washington | 2005-24 | 0.2460436 | 0.1056389 | 20 | 0.0236216 | 0.1966030 | 0.2954841 |
| Pend Oreille County, Washington | 1950-69 | 0.1303871 | 0.0546806 | 20 | 0.0122270 | 0.1047958 | 0.1559784 |
| Pend Oreille County, Washington | 2005-24 | 0.0978301 | 0.0429671 | 20 | 0.0096077 | 0.0777209 | 0.1179394 |
| Pierce County, Washington | 1950-69 | 0.2933901 | 0.1318172 | 20 | 0.0294752 | 0.2316977 | 0.3550824 |
| Pierce County, Washington | 2005-24 | 0.2421974 | 0.0893287 | 20 | 0.0199745 | 0.2003903 | 0.2840045 |
| San Juan County, Washington | 1950-69 | 0.2023890 | 0.0737337 | 20 | 0.0164874 | 0.1678805 | 0.2368974 |
| San Juan County, Washington | 2005-24 | 0.1744785 | 0.0564386 | 20 | 0.0126200 | 0.1480645 | 0.2008926 |
| Skagit County, Washington | 1950-69 | 0.3644765 | 0.1521314 | 20 | 0.0340176 | 0.2932768 | 0.4356762 |
| Skagit County, Washington | 2005-24 | 0.3067599 | 0.1106866 | 20 | 0.0247503 | 0.2549569 | 0.3585628 |
| Skamania County, Washington | 1950-69 | 0.3942506 | 0.1831491 | 20 | 0.0409534 | 0.3085342 | 0.4799670 |
| Skamania County, Washington | 2005-24 | 0.3103656 | 0.1283386 | 20 | 0.0286974 | 0.2503013 | 0.3704299 |
| Snohomish County, Washington | 1950-69 | 0.3658901 | 0.1571137 | 20 | 0.0351317 | 0.2923586 | 0.4394216 |
| Snohomish County, Washington | 2005-24 | 0.3123520 | 0.1152938 | 20 | 0.0257805 | 0.2583929 | 0.3663112 |
| Spokane County, Washington | 1950-69 | 0.0983015 | 0.0448093 | 20 | 0.0100197 | 0.0773300 | 0.1192729 |
| Spokane County, Washington | 2005-24 | 0.0761750 | 0.0312343 | 20 | 0.0069842 | 0.0615569 | 0.0907931 |
| Stevens County, Washington | 1950-69 | 0.0974383 | 0.0371358 | 20 | 0.0083038 | 0.0800582 | 0.1148184 |
| Stevens County, Washington | 2005-24 | 0.0744463 | 0.0326261 | 20 | 0.0072954 | 0.0591768 | 0.0897158 |
| Thurston County, Washington | 1950-69 | 0.2486569 | 0.1132318 | 20 | 0.0253194 | 0.1956628 | 0.3016510 |
| Thurston County, Washington | 2005-24 | 0.2034090 | 0.0827510 | 20 | 0.0185037 | 0.1646804 | 0.2421377 |
| Wahkiakum County, Washington | 1950-69 | 0.2836752 | 0.1204220 | 20 | 0.0269272 | 0.2273160 | 0.3400344 |
| Wahkiakum County, Washington | 2005-24 | 0.2261165 | 0.0975296 | 20 | 0.0218083 | 0.1804713 | 0.2717618 |
| Walla Walla County, Washington | 1950-69 | 0.0673853 | 0.0326224 | 20 | 0.0072946 | 0.0521176 | 0.0826531 |
| Walla Walla County, Washington | 2005-24 | 0.0542915 | 0.0230428 | 20 | 0.0051525 | 0.0435071 | 0.0650758 |
| Whatcom County, Washington | 1950-69 | 0.3371718 | 0.1432597 | 20 | 0.0320338 | 0.2701242 | 0.4042194 |
| Whatcom County, Washington | 2005-24 | 0.2825477 | 0.0981786 | 20 | 0.0219534 | 0.2365987 | 0.3284967 |
| Whitman County, Washington | 1950-69 | 0.0855892 | 0.0426485 | 20 | 0.0095365 | 0.0656291 | 0.1055493 |
| Whitman County, Washington | 2005-24 | 0.0678525 | 0.0250818 | 20 | 0.0056085 | 0.0561138 | 0.0795912 |
| Yakima County, Washington | 1950-69 | 0.1377246 | 0.0637040 | 20 | 0.0142446 | 0.1079102 | 0.1675390 |
| Yakima County, Washington | 2005-24 | 0.1141443 | 0.0395149 | 20 | 0.0088358 | 0.0956508 | 0.1326379 |

**Task 5** *Visualize the confidence intervals for the county-level data
you generated in task 4, using the same style as you did for Task 2
above. For an optional extra challenge, visually highlight the counties
where the confidence intervals between time periods do NOT overlap.*

``` r
cty_overlap <- era5_WA_CTY %>%
  group_by(cty_name) %>%
  summarise(no_overlap = max(lower_ci) > min(upper_ci))  

era5_WA_CTY_plot <- era5_WA_CTY %>%
  left_join(cty_overlap, by = "cty_name")

ggplot(era5_WA_CTY_plot,
       aes(y = reorder(cty_name, mean_prcp), color = time_range)) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci, linewidth = no_overlap), position = position_dodge(width = 0.6)) +
  geom_point(aes(x = mean_prcp, size = no_overlap), position = position_dodge(width = 0.6)) +
  scale_color_manual(values = c("#4e6d58", "#836685")) +        
  scale_linewidth_manual(values = c(`TRUE` = 2, `FALSE` = 0.5), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 1.5), guide = "none") +
  labs(x = "Mean January Precipitation (m)", y = "County", color = "Time period")+
  theme_minimal()
```

![](Lab3_CI-and-sampling_files/figure-gfm/Task6-1.png)<!-- -->

**Task 6**: *Compare the size of the error terms for your county level
data to the error terms at state level that you calculated in Task 1.
Are they bigger or smaller? Explain why the formula you used for
calculating the error terms produced error terms of different sizes. How
is this difference visible in your ggplot graphs?*

THe standard error of the data on a state level is around 0.004 - 0.005
meters, while the county data varies much more from county to county.
Some of the standard errors at the county level are as large as 0.04m,
and as small as 0.003m. When we look at the standard error on the state
level vs county level, the n term we use in calculating standard error
(SE = s / √n) is very large when we are grouping all the data by just
time range. The n is much smaller when we isolate just the january
occurrences and further group things by county. BEcause the n is
smaller, the standard error for the county level is larger at the county
level. This is consistent with our visualization, as we see the county
standard errors overlapping more than the state.

**Task 7**: *Transform the county data from task 4 into wide format so
that the means for each time period are in the same row for each county.
Then calculate the change in precipitation between the time periods.
Join this table to the geopackage file that contains county boundaries
and census data, making sure that you only have Washington counties
included - use the geopackage `data/acsdata_cty_westreg.gpkg`. Then use
`tmap` to map out the difference in precipitation means across the two
time periods. Make sure the legend of this map is interpretable.*

``` r
era5_CTY_Wide <- era5_WA_CTY %>%
  ungroup() %>%
  select(cty_name, time_range, mean_prcp) %>%
  pivot_wider(names_from = time_range, values_from = mean_prcp) %>%
  rename(`1950-1969` = 2, `2005-2024` = 3) %>%
  mutate(prcp_change = `2005-2024` - `1950-1969`)

era5_CTY_Wide %>%
  head() %>%
  kable(digits = 4)
```

| cty_name                   | 1950-1969 | 2005-2024 | prcp_change |
|:---------------------------|----------:|----------:|------------:|
| Adams County, Washington   |    0.0562 |    0.0455 |     -0.0107 |
| Asotin County, Washington  |    0.0917 |    0.0661 |     -0.0256 |
| Benton County, Washington  |    0.0439 |    0.0378 |     -0.0061 |
| Chelan County, Washington  |    0.2184 |    0.1818 |     -0.0366 |
| Clallam County, Washington |    0.3252 |    0.2724 |     -0.0528 |
| Clark County, Washington   |    0.3545 |    0.2886 |     -0.0659 |

``` r
cty_data <- st_read("data/acsdata_cty_westreg.gpkg")
```

    ## Reading layer `acsdata_cty_westreg' from data source 
    ##   `/Users/maryerudd/Downloads/Local GEOG Notes:Projects/geog4300_lab3/data/acsdata_cty_westreg.gpkg' 
    ##   using driver `GPKG'
    ## Simple feature collection with 414 features and 8 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -124.7322 ymin: 31.33223 xmax: -102.0416 ymax: 49.00237
    ## Geodetic CRS:  WGS 84

``` r
wa_cty_data <- cty_data %>%
  filter(st_name == "Washington") %>%
  left_join(era5_CTY_Wide, by = "cty_name")

tm_shape(wa_cty_data) +
  tm_polygons(fill = "prcp_change", fill.scale = tm_scale_intervals(values = met.brewer("Monet", n = 7, type = "continuous")),fill.legend = tm_legend(title = "Change in mean January\nprecipitation (m)\n1950-1969 to 2005-2024"))
```

![](Lab3_CI-and-sampling_files/figure-gfm/Task7-1.png)<!-- -->

# Optional challenge section: Interpreting the implications of precipitation changes

Now that you’ve calculated these means and confidence intervals, let’s
identify who might be most affected by changes in precipitation. To see
who is affected, create a dummy variable that flags any counties with a
decline of 5 centimeters or more (0.05 meters) with a 1, labeling all
other counties with a 0. Use group_by and summarise to do the following:

- Count the total population in each of these two county groups.
- Calculate the mean value for median income and poverty rate in these
  two groups.

**Task 8** *Create a dummy variable for counties with a change of 0.05
meters or more between the time periods. Calculate how many people
currently live in these counties along with the mean value of median
income (medinc) and poverty rate (pov_pop_pct) for those areas.*

``` r
wa_challenge <- wa_cty_data %>%                                 
  mutate(decline = if_else(prcp_change <= -0.05, 1, 0))   # 1 = dropped 0.05 m or more

wa_challenge %>%
  group_by(decline) %>%
  summarise(
    n_counties  = n(),
    total_pop   = sum(totpop_race, na.rm = TRUE),
    mean_medinc = mean(medinc, na.rm = TRUE),
    mean_pov    = mean(pov_pop_pct, na.rm = TRUE)
  ) %>%
  kable()
```

| decline | n_counties | total_pop | mean_medinc | mean_pov | geom |
|---:|---:|---:|---:|---:|:---|
| 0 | 24 | 4320376 | 70041.46 | 12.98885 | MULTIPOLYGON (((-123.0109 4… |
| 1 | 15 | 3368173 | 76272.07 | 11.38263 | MULTIPOLYGON (((-122.6126 4… |

**Task 9** *Create a one paragraph summary of your results from tasks 7
and 8. What do you learn about how precipitation has changed in the last
70 years across the state of Washington? How would you describe the
population affected by large drops in precipitation based on your
results?*

Between 1950-1969 and 2005-2024, mean January precipitation in
Washington decreased by about 0.037 m statewide, and the 95% confidence
intervals for the two periods did not overlap. At the county level,
county confidence intervals overlap more often and varies alot from
county to county. 15 of 39 counties in Washington saw precipitation
declines by 0.05 meters or more, and about 3.37 million people live in
those counties total. Compared with the other counties that did not see
those declines, the counties with the 0.05+ declines have a higher mean
median household income and a lower mean poverty rate. The population
affected by large drops in precipitation seems to be slightly better off
economically than the rest of the state.

Small multiples or multi-panel maps are often a helpful way to show
change over time. See this page for more information about how these are
created in `tmap`:

- <https://r-tmap.github.io/tmap/articles/basics_facets>

Using these tools, create a small multiples map of your precipitation
data for counties in a state of your choice for all 40 years in these
two time periods. This means you will have one figure with 40 individual
maps, all of the same state in different years. These maps should allow
easy comparison across years and should be clearly labeled so that they
could be interpreted by someone with no knowledge of this assignment.

In addition, describe two interesting trends you see in the figure you
created.

``` r
wa_yearly <- wa_cty_data %>%
  select(cty_name) %>%
  inner_join(
    era5_data %>%
      filter(st_name == "Washington") %>%
      mutate(yr = factor(year(year))) %>%
      select(cty_name, yr, prcp_m),
    by = "cty_name")

tm_shape(wa_yearly) +
  tm_polygons("prcp_m",
              lwd = 0.2,
              fill.legend = tm_legend(title = "January\nprecipitation (m)", title.size = 0.7, text.size = 0.6)) +
  tm_facets_wrap("yr", nrow = 5, ncol = 8) +
  tm_layout(scale = 0.3, inner.margins = 0, outer.margins = 0, between_margin = 0, panel.label.size = 0.7)
```

![](Lab3_CI-and-sampling_files/figure-gfm/challenge-1.png)<!-- -->

Two patterns: First, the level of january precipitation is pretty
consistent from west to east, with eastern Washington seeing
significantly less precipitation. This is consistent from what I know
about how mountains impact rainfall. The part of the state east of the
Cascades gets significantly less rainfall, as the air passes over the
mountain peaks and warms, causing rain clouds to dissipate. The second
thing I noticed is that that there are some years (1953 and 2020) that
seem to be pretty extreme with rainfall. There also seems to be more
rainfall in the 1950-1969 maps compared to the more recent maps.

# Final submission stuff

**Disclosure of assistance:** Besides class materials, what other
sources of assistance did you use while completing this lab? These can
include input from classmates, relevant material identified through web
searches (e.g., Stack Overflow), or assistance from ChatGPT or other AI
tools. How did these sources support your own learning in completing
this lab?

I could not figure out how to visually highlight the counties where the
confidence intervals between time periods didnt overlap, so I did
utilize Claude to teach me how to do that and would lovee to know if the
way Claude did it is the way you would have done it. I also used Claude
for general debugging. Other than that I used our previous scripts and
free online R textbooks. I utilized Google to answer any other questions
including ones related to the challenge question.

**Lab reflection:** How do you feel about the work you did on this lab?
Was it easy, moderate, or hard? What are the biggest things you learned
by completing it?

This lab was moderate, I tried to really stretch myself with the data
visualization and make some good looking visuals for my data in this
lab, so I felt like I grew in that way! Also brushing up on the joins
was good as we haven’t focused on those in a second. The data
interpretation could likely be better, so I’m excited to get working on
that in the latter half of the semester.
