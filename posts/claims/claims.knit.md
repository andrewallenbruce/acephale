---
title: "Claims Analysis"
format:
  html:
    reference-location: block
editor_options: 
  chunk_output_type: console
---

::: {.cell}

:::





# Summary




::: {.cell layout-align="center"}

```{.r .cell-code}
procedure_sum <- procedures |>
  summarise(
    n_hcpcs = n_distinct(hcpcs, na.rm = TRUE),
    n_units = na_sum(units),
    n_mod1 = na_sum(!is.na(mod1)),
    n_mod2 = na_sum(!is.na(mod2)),
    charges = na_sum(charges),
    allowed = na_sum(allowed),
    payments = na_sum(payments),
    adjustments = na_sum(adjustments),
    .by = c(id, enc, dos)) |>
  filter(charges > 0) |>
  arrange(dos) |> 
  left_join(
    encounters, by = join_by(id, enc, dos)) |> 
  select(
    id, 
    enc, 
    dos, 
    year:wday_l,
    n_hcpcs, 
    n_units, 
    n_mod1, 
    n_mod2, 
    charges, 
    allowed, 
    payments, 
    adjustments,
    dob,
    age,
    rendering,
    cred,
    icd,
    pos,
    pos_name,
    pos_type,
    loc,
    ins_class,
    ins_prim)

procedure_sum
```

::: {.cell-output .cell-output-stdout}

```
[90m# A tibble: 38,852 × 31[39m
   id       enc dos         year quarter month month_l  week
   [3m[90m<chr>[39m[23m  [3m[90m<int>[39m[23m [3m[90m<date>[39m[23m     [3m[90m<int>[39m[23m   [3m[90m<int>[39m[23m [3m[90m<int>[39m[23m [3m[90m<ord>[39m[23m   [3m[90m<int>[39m[23m
[90m 1[39m PT0491     1 2022-06-14  [4m2[24m022       2     6 Jun        24
[90m 2[39m PT2360     1 2022-07-31  [4m2[24m022       3     7 Jul        31
[90m 3[39m PT0671     1 2022-08-06  [4m2[24m022       3     8 Aug        32
[90m 4[39m PT1499     1 2022-08-06  [4m2[24m022       3     8 Aug        32
[90m 5[39m PT1485     3 2022-08-09  [4m2[24m022       3     8 Aug        32
[90m 6[39m PT1626     1 2022-08-10  [4m2[24m022       3     8 Aug        32
[90m 7[39m PT2430     1 2022-08-10  [4m2[24m022       3     8 Aug        32
[90m 8[39m PT1677     1 2022-08-11  [4m2[24m022       3     8 Aug        32
[90m 9[39m PT2181     1 2022-08-11  [4m2[24m022       3     8 Aug        32
[90m10[39m PT1043     1 2022-08-12  [4m2[24m022       3     8 Aug        32
[90m# ℹ 38,842 more rows[39m
[90m# ℹ 23 more variables: day <int>, yday <int>, wday <int>,[39m
[90m#   wday_l <ord>, n_hcpcs <int>, n_units <int>,[39m
[90m#   n_mod1 <int>, n_mod2 <int>, charges <dbl>,[39m
[90m#   allowed <dbl>, payments <dbl>, adjustments <dbl>,[39m
[90m#   dob <date>, age <int>, rendering <chr>, cred <chr>,[39m
[90m#   icd <chr>, pos <chr>, pos_name <chr>, pos_type <fct>, …[39m
```


:::
:::

::: {.cell layout-align="center"}

:::

::: {.cell layout-align="center"}

```{.r .cell-code}
procedure_sum |> 
  summarise(
    encounters = n(),
    n_hcpcs = na_sum(n_hcpcs),
    n_units = na_sum(n_units),
    n_mod1 = na_sum(n_mod1),
    n_mod2 = na_sum(n_mod2),
    charges = na_sum(charges),
    allowed = na_sum(allowed),
    payments = na_sum(payments),
    adjustments = na_sum(adjustments),
    .by = c(year, quarter, month_l, rendering, ins_prim)) |> 
  arrange(year, quarter, month_l) |> 
  ggplot(aes(y = charges)) +
  geom_boxplot(aes(group = year, fill = year)) +
  scale_x_continuous(labels = scales::label_number(scale = 0.001, suffix = "k")) +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "none") +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
```

::: {.cell-output-display}
![](claims_files/figure-html/unnamed-chunk-4-1.png){fig-align='center' width=100%}
:::
:::

::: {.cell layout-align="center"}

```{.r .cell-code}
procedure_sum |> 
  summarise(
    encounters = n(),
    n_hcpcs = na_sum(n_hcpcs),
    n_units = na_sum(n_units),
    n_mod1 = na_sum(n_mod1),
    n_mod2 = na_sum(n_mod2),
    charges = na_sum(charges),
    allowed = na_sum(allowed),
    payments = na_sum(payments),
    adjustments = na_sum(adjustments),
    .by = c(year, quarter, month_l, rendering, ins_prim)) |> 
  arrange(year, quarter, month_l) |> 
  ggplot(aes(x = payments, y = charges)) +
  geom_point(aes(size = encounters, group = ins_prim, fill = ins_prim), colour = "black", shape = 21) +
  geom_quantile(colour = "royalblue", linewidth = 1) +
  # geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  # geom_vline(xintercept = 0, color = "grey50", linewidth = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.001, suffix = "k")) +
  scale_x_continuous(labels = scales::label_number(scale = 0.001, suffix = "k")) +
  ggthemes::theme_fivethirtyeight() +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
```

::: {.cell-output .cell-output-stderr}

```
[1m[22mSmoothing formula not specified. Using: y ~ x
```


:::

::: {.cell-output-display}
![](claims_files/figure-html/unnamed-chunk-5-1.png){fig-align='center' width=100%}
:::
:::





## Ratio Analysis

Revenue Per Visit
: $RPV =$ Total Payments $\div$ Number of Patient Visits





::: {.cell layout-align="center"}

```{.r .cell-code}
claims_summary <- claims |>
    select(
    id,
    enc,
    age,
    ins_prim,
    charges,
    allowed,
    payments,
    adjustments) |> 
  summarise(
    procedures = n(),
    charges = sum(charges, na.rm = TRUE),
    allowed = sum(allowed, na.rm = TRUE),
    payments = sum(payments, na.rm = TRUE),
    adjustments = sum(adjustments, na.rm = TRUE),
    .by = c(id, enc, age)) |>
  summarise(
    visits = n(),
    procedures = sum(procedures, na.rm = TRUE),
    charges = sum(charges, na.rm = TRUE),
    allowed = sum(allowed, na.rm = TRUE),
    payments = sum(payments, na.rm = TRUE),
    adjustments = sum(adjustments, na.rm = TRUE),
    .by = c(age)
  ) |> 
  summarise(
    # patients = n(),
    avg_age = mean(age, na.rm = TRUE),
    procedures = sum(procedures, na.rm = TRUE),
    visits = sum(visits, na.rm = TRUE),
    charges = sum(charges, na.rm = TRUE),
    allowed = sum(allowed, na.rm = TRUE),
    revenue = sum(payments, na.rm = TRUE),
    adjustments = sum(adjustments, na.rm = TRUE))

claims_summary |> 
  set_names(
    c("Average Patient Age", 
      "Procedures", 
      "Visits", 
      "Charges", 
      "Allowed", 
      "Net Payment", 
      "Adjustments")) |> 
  pivot_longer(
    cols = everything(), 
    names_to = "Metric", 
    values_to = "Value") |>
  gt() |> 
  opt_table_font(font = google_font(name = "Fira Code")) |> 
  tab_options(table.width = pct(100),
              quarto.disable_processing = TRUE)
```

::: {.cell-output-display}

```{=html}
<div id="opdzahlubc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Fira+Code:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#opdzahlubc table {
  font-family: 'Fira Code', system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#opdzahlubc thead, #opdzahlubc tbody, #opdzahlubc tfoot, #opdzahlubc tr, #opdzahlubc td, #opdzahlubc th {
  border-style: none;
}

#opdzahlubc p {
  margin: 0;
  padding: 0;
}

#opdzahlubc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 100%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#opdzahlubc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#opdzahlubc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#opdzahlubc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#opdzahlubc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#opdzahlubc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opdzahlubc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#opdzahlubc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#opdzahlubc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#opdzahlubc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#opdzahlubc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#opdzahlubc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#opdzahlubc .gt_spanner_row {
  border-bottom-style: hidden;
}

#opdzahlubc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#opdzahlubc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#opdzahlubc .gt_from_md > :first-child {
  margin-top: 0;
}

#opdzahlubc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#opdzahlubc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#opdzahlubc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#opdzahlubc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#opdzahlubc .gt_row_group_first td {
  border-top-width: 2px;
}

#opdzahlubc .gt_row_group_first th {
  border-top-width: 2px;
}

#opdzahlubc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opdzahlubc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#opdzahlubc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#opdzahlubc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opdzahlubc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opdzahlubc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#opdzahlubc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#opdzahlubc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#opdzahlubc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opdzahlubc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#opdzahlubc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#opdzahlubc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#opdzahlubc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#opdzahlubc .gt_left {
  text-align: left;
}

#opdzahlubc .gt_center {
  text-align: center;
}

#opdzahlubc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#opdzahlubc .gt_font_normal {
  font-weight: normal;
}

#opdzahlubc .gt_font_bold {
  font-weight: bold;
}

#opdzahlubc .gt_font_italic {
  font-style: italic;
}

#opdzahlubc .gt_super {
  font-size: 65%;
}

#opdzahlubc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#opdzahlubc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#opdzahlubc .gt_indent_1 {
  text-indent: 5px;
}

#opdzahlubc .gt_indent_2 {
  text-indent: 10px;
}

#opdzahlubc .gt_indent_3 {
  text-indent: 15px;
}

#opdzahlubc .gt_indent_4 {
  text-indent: 20px;
}

#opdzahlubc .gt_indent_5 {
  text-indent: 25px;
}

#opdzahlubc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#opdzahlubc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="true" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Metric">Metric</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Value">Value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Metric" class="gt_row gt_left">Average Patient Age</td>
<td headers="Value" class="gt_row gt_right">46.5</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Procedures</td>
<td headers="Value" class="gt_row gt_right">49721.0</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Visits</td>
<td headers="Value" class="gt_row gt_right">39839.0</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Charges</td>
<td headers="Value" class="gt_row gt_right">11659949.0</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Allowed</td>
<td headers="Value" class="gt_row gt_right">4830582.7</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Net Payment</td>
<td headers="Value" class="gt_row gt_right">4648361.4</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Adjustments</td>
<td headers="Value" class="gt_row gt_right">5948070.3</td></tr>
  </tbody>
  
  
</table>
</div>
```

:::

```{.r .cell-code}
claims_summary |>
  mutate(
    revenue_adjustments = revenue + adjustments, 
    .after = charges) |> 
  reframe(
    cpp = charges / procedures,
    alpp = allowed / procedures,
    rpp = revenue / procedures,
    adpp = adjustments / procedures,
    ppv = procedures / visits,
    cpv = charges / visits,
    alpv = allowed / visits,
    rpv = revenue / visits,
    adpv = adjustments / visits,
    cpr = charges / revenue_adjustments,
    alpr = allowed / revenue,
    adpr = adjustments / revenue,
    net_collect = revenue / (charges - adjustments),
    net_allow = revenue / allowed,
    net_adjust = revenue / adjustments,
    ) |> 
  set_names(
    c("Charge Per Procedure", 
      "Allowed Per Procedure", 
      "Payment Per Procedure", 
      "Adjustment Per Procedure", 
      "Procedures Per Visit", 
      "Charge Per Visit", 
      "Allowed Per Visit", 
      "Payment Per Visit", 
      "Adjustment Per Visit",
      "Charge To Payment + Adjustment",
      "Allowed Per Dollar", 
      "Adjustment Per Dollar",
      "Net Collection Ratio",
      "Net Allowable Ratio",
      "Net Adjustment Ratio"
      )) |> 
  pivot_longer(
    cols = everything(), 
    names_to = "Metric", 
    values_to = "Value") |> 
  gt() |> 
  opt_table_font(font = google_font(name = "Fira Code")) |> 
  tab_options(table.width = pct(100),
              quarto.disable_processing = TRUE)
```

::: {.cell-output-display}

```{=html}
<div id="psdilgnkww" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Fira+Code:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#psdilgnkww table {
  font-family: 'Fira Code', system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#psdilgnkww thead, #psdilgnkww tbody, #psdilgnkww tfoot, #psdilgnkww tr, #psdilgnkww td, #psdilgnkww th {
  border-style: none;
}

#psdilgnkww p {
  margin: 0;
  padding: 0;
}

#psdilgnkww .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 100%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#psdilgnkww .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#psdilgnkww .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#psdilgnkww .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#psdilgnkww .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#psdilgnkww .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#psdilgnkww .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#psdilgnkww .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#psdilgnkww .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#psdilgnkww .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#psdilgnkww .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#psdilgnkww .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#psdilgnkww .gt_spanner_row {
  border-bottom-style: hidden;
}

#psdilgnkww .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#psdilgnkww .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#psdilgnkww .gt_from_md > :first-child {
  margin-top: 0;
}

#psdilgnkww .gt_from_md > :last-child {
  margin-bottom: 0;
}

#psdilgnkww .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#psdilgnkww .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#psdilgnkww .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#psdilgnkww .gt_row_group_first td {
  border-top-width: 2px;
}

#psdilgnkww .gt_row_group_first th {
  border-top-width: 2px;
}

#psdilgnkww .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#psdilgnkww .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#psdilgnkww .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#psdilgnkww .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#psdilgnkww .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#psdilgnkww .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#psdilgnkww .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#psdilgnkww .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#psdilgnkww .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#psdilgnkww .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#psdilgnkww .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#psdilgnkww .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#psdilgnkww .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#psdilgnkww .gt_left {
  text-align: left;
}

#psdilgnkww .gt_center {
  text-align: center;
}

#psdilgnkww .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#psdilgnkww .gt_font_normal {
  font-weight: normal;
}

#psdilgnkww .gt_font_bold {
  font-weight: bold;
}

#psdilgnkww .gt_font_italic {
  font-style: italic;
}

#psdilgnkww .gt_super {
  font-size: 65%;
}

#psdilgnkww .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#psdilgnkww .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#psdilgnkww .gt_indent_1 {
  text-indent: 5px;
}

#psdilgnkww .gt_indent_2 {
  text-indent: 10px;
}

#psdilgnkww .gt_indent_3 {
  text-indent: 15px;
}

#psdilgnkww .gt_indent_4 {
  text-indent: 20px;
}

#psdilgnkww .gt_indent_5 {
  text-indent: 25px;
}

#psdilgnkww .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#psdilgnkww div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="true" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Metric">Metric</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Value">Value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Metric" class="gt_row gt_left">Charge Per Procedure</td>
<td headers="Value" class="gt_row gt_right">234.5075320</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Allowed Per Procedure</td>
<td headers="Value" class="gt_row gt_right">97.1537722</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Payment Per Procedure</td>
<td headers="Value" class="gt_row gt_right">93.4888964</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Adjustment Per Procedure</td>
<td headers="Value" class="gt_row gt_right">119.6289359</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Procedures Per Visit</td>
<td headers="Value" class="gt_row gt_right">1.2480484</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Charge Per Visit</td>
<td headers="Value" class="gt_row gt_right">292.6767489</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Allowed Per Visit</td>
<td headers="Value" class="gt_row gt_right">121.2526095</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Payment Per Visit</td>
<td headers="Value" class="gt_row gt_right">116.6786671</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Adjustment Per Visit</td>
<td headers="Value" class="gt_row gt_right">149.3027014</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Charge To Payment + Adjustment</td>
<td headers="Value" class="gt_row gt_right">1.1003656</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Allowed Per Dollar</td>
<td headers="Value" class="gt_row gt_right">1.0392012</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Adjustment Per Dollar</td>
<td headers="Value" class="gt_row gt_right">1.2796058</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Net Collection Ratio</td>
<td headers="Value" class="gt_row gt_right">0.8138060</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Net Allowable Ratio</td>
<td headers="Value" class="gt_row gt_right">0.9622776</td></tr>
    <tr><td headers="Metric" class="gt_row gt_left">Net Adjustment Ratio</td>
<td headers="Value" class="gt_row gt_right">0.7814907</td></tr>
  </tbody>
  
  
</table>
</div>
```

:::
:::

::: {.cell layout-align="center"}

```{.r .cell-code}
insurance_summary <- claims |>
  summarise(
    procedures = n(),
    charges = sum(charges, na.rm = TRUE),
    allowed = sum(allowed, na.rm = TRUE),
    payments = sum(payments, na.rm = TRUE),
    adjustments = sum(adjustments, na.rm = TRUE),
    .by = c(id, enc, age, ins_prim)) |>
  summarise(
    visits = n(),
    procedures = sum(procedures, na.rm = TRUE),
    charges = sum(charges, na.rm = TRUE),
    allowed = sum(allowed, na.rm = TRUE),
    payments = sum(payments, na.rm = TRUE),
    adjustments = sum(adjustments, na.rm = TRUE),
    .by = c(id, age, ins_prim)
  ) |> 
  summarise(
    patients = n(),
    avg_age = mean(age, na.rm = TRUE),
    procedures = sum(procedures, na.rm = TRUE),
    visits = sum(visits, na.rm = TRUE),
    charges = sum(charges, na.rm = TRUE),
    allowed = sum(allowed, na.rm = TRUE),
    revenue = sum(payments, na.rm = TRUE),
    adjustments = sum(adjustments, na.rm = TRUE),
    .by = c(ins_prim)
  ) |> 
  filter(!is.na(ins_prim)) |> 
  arrange(desc(procedures))

# Patients are counted once per insurance class
insurance_summary |>
  gt(rowname_col = "ins_prim") |> 
  opt_all_caps() |> 
  opt_table_font(font = google_font(name = "Fira Code")) |> 
  tab_options(table.width = pct(100),
              quarto.disable_processing = TRUE)
```

::: {.cell-output-display}

```{=html}
<div id="ubwyyhzqpn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Fira+Code:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#ubwyyhzqpn table {
  font-family: 'Fira Code', system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ubwyyhzqpn thead, #ubwyyhzqpn tbody, #ubwyyhzqpn tfoot, #ubwyyhzqpn tr, #ubwyyhzqpn td, #ubwyyhzqpn th {
  border-style: none;
}

#ubwyyhzqpn p {
  margin: 0;
  padding: 0;
}

#ubwyyhzqpn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 100%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ubwyyhzqpn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ubwyyhzqpn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ubwyyhzqpn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ubwyyhzqpn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ubwyyhzqpn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ubwyyhzqpn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ubwyyhzqpn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ubwyyhzqpn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ubwyyhzqpn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ubwyyhzqpn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ubwyyhzqpn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ubwyyhzqpn .gt_spanner_row {
  border-bottom-style: hidden;
}

#ubwyyhzqpn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ubwyyhzqpn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ubwyyhzqpn .gt_from_md > :first-child {
  margin-top: 0;
}

#ubwyyhzqpn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ubwyyhzqpn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ubwyyhzqpn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ubwyyhzqpn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ubwyyhzqpn .gt_row_group_first td {
  border-top-width: 2px;
}

#ubwyyhzqpn .gt_row_group_first th {
  border-top-width: 2px;
}

#ubwyyhzqpn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ubwyyhzqpn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ubwyyhzqpn .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ubwyyhzqpn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ubwyyhzqpn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ubwyyhzqpn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ubwyyhzqpn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ubwyyhzqpn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ubwyyhzqpn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ubwyyhzqpn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ubwyyhzqpn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ubwyyhzqpn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ubwyyhzqpn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ubwyyhzqpn .gt_left {
  text-align: left;
}

#ubwyyhzqpn .gt_center {
  text-align: center;
}

#ubwyyhzqpn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ubwyyhzqpn .gt_font_normal {
  font-weight: normal;
}

#ubwyyhzqpn .gt_font_bold {
  font-weight: bold;
}

#ubwyyhzqpn .gt_font_italic {
  font-style: italic;
}

#ubwyyhzqpn .gt_super {
  font-size: 65%;
}

#ubwyyhzqpn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ubwyyhzqpn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ubwyyhzqpn .gt_indent_1 {
  text-indent: 5px;
}

#ubwyyhzqpn .gt_indent_2 {
  text-indent: 10px;
}

#ubwyyhzqpn .gt_indent_3 {
  text-indent: 15px;
}

#ubwyyhzqpn .gt_indent_4 {
  text-indent: 20px;
}

#ubwyyhzqpn .gt_indent_5 {
  text-indent: 25px;
}

#ubwyyhzqpn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ubwyyhzqpn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="true" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="patients">patients</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="avg_age">avg_age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="procedures">procedures</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="visits">visits</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="charges">charges</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="allowed">allowed</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="revenue">revenue</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="adjustments">adjustments</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">BCBS</th>
<td headers="stub_1_1 patients" class="gt_row gt_right">2155</td>
<td headers="stub_1_1 avg_age" class="gt_row gt_right">30.51601</td>
<td headers="stub_1_1 procedures" class="gt_row gt_right">11073</td>
<td headers="stub_1_1 visits" class="gt_row gt_right">9348</td>
<td headers="stub_1_1 charges" class="gt_row gt_right">2648855</td>
<td headers="stub_1_1 allowed" class="gt_row gt_right">1113623.43</td>
<td headers="stub_1_1 revenue" class="gt_row gt_right">1052272.49</td>
<td headers="stub_1_1 adjustments" class="gt_row gt_right">1426863.60</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">UHC</th>
<td headers="stub_1_2 patients" class="gt_row gt_right">1877</td>
<td headers="stub_1_2 avg_age" class="gt_row gt_right">34.26905</td>
<td headers="stub_1_2 procedures" class="gt_row gt_right">10091</td>
<td headers="stub_1_2 visits" class="gt_row gt_right">7492</td>
<td headers="stub_1_2 charges" class="gt_row gt_right">2691070</td>
<td headers="stub_1_2 allowed" class="gt_row gt_right">938135.72</td>
<td headers="stub_1_2 revenue" class="gt_row gt_right">873231.19</td>
<td headers="stub_1_2 adjustments" class="gt_row gt_right">1538893.10</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">SELF</th>
<td headers="stub_1_3 patients" class="gt_row gt_right">2801</td>
<td headers="stub_1_3 avg_age" class="gt_row gt_right">33.87429</td>
<td headers="stub_1_3 procedures" class="gt_row gt_right">8770</td>
<td headers="stub_1_3 visits" class="gt_row gt_right">8153</td>
<td headers="stub_1_3 charges" class="gt_row gt_right">1020064</td>
<td headers="stub_1_3 allowed" class="gt_row gt_right">775974.68</td>
<td headers="stub_1_3 revenue" class="gt_row gt_right">773146.09</td>
<td headers="stub_1_3 adjustments" class="gt_row gt_right">101145.74</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">AETNA</th>
<td headers="stub_1_4 patients" class="gt_row gt_right">1010</td>
<td headers="stub_1_4 avg_age" class="gt_row gt_right">28.61881</td>
<td headers="stub_1_4 procedures" class="gt_row gt_right">4901</td>
<td headers="stub_1_4 visits" class="gt_row gt_right">3991</td>
<td headers="stub_1_4 charges" class="gt_row gt_right">1279925</td>
<td headers="stub_1_4 allowed" class="gt_row gt_right">445925.74</td>
<td headers="stub_1_4 revenue" class="gt_row gt_right">430341.11</td>
<td headers="stub_1_4 adjustments" class="gt_row gt_right">728428.95</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">CIGNA</th>
<td headers="stub_1_5 patients" class="gt_row gt_right">758</td>
<td headers="stub_1_5 avg_age" class="gt_row gt_right">28.15963</td>
<td headers="stub_1_5 procedures" class="gt_row gt_right">4265</td>
<td headers="stub_1_5 visits" class="gt_row gt_right">3651</td>
<td headers="stub_1_5 charges" class="gt_row gt_right">986190</td>
<td headers="stub_1_5 allowed" class="gt_row gt_right">345288.15</td>
<td headers="stub_1_5 revenue" class="gt_row gt_right">323414.76</td>
<td headers="stub_1_5 adjustments" class="gt_row gt_right">585340.34</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub">MEDICARE</th>
<td headers="stub_1_6 patients" class="gt_row gt_right">547</td>
<td headers="stub_1_6 avg_age" class="gt_row gt_right">70.09872</td>
<td headers="stub_1_6 procedures" class="gt_row gt_right">2966</td>
<td headers="stub_1_6 visits" class="gt_row gt_right">1340</td>
<td headers="stub_1_6 charges" class="gt_row gt_right">787140</td>
<td headers="stub_1_6 allowed" class="gt_row gt_right">292738.64</td>
<td headers="stub_1_6 revenue" class="gt_row gt_right">287262.53</td>
<td headers="stub_1_6 adjustments" class="gt_row gt_right">487146.83</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub">OTHER</th>
<td headers="stub_1_7 patients" class="gt_row gt_right">744</td>
<td headers="stub_1_7 avg_age" class="gt_row gt_right">29.41667</td>
<td headers="stub_1_7 procedures" class="gt_row gt_right">2804</td>
<td headers="stub_1_7 visits" class="gt_row gt_right">2416</td>
<td headers="stub_1_7 charges" class="gt_row gt_right">848695</td>
<td headers="stub_1_7 allowed" class="gt_row gt_right">491911.51</td>
<td headers="stub_1_7 revenue" class="gt_row gt_right">489813.89</td>
<td headers="stub_1_7 adjustments" class="gt_row gt_right">196970.03</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub">SUNFLOWER</th>
<td headers="stub_1_8 patients" class="gt_row gt_right">283</td>
<td headers="stub_1_8 avg_age" class="gt_row gt_right">15.55830</td>
<td headers="stub_1_8 procedures" class="gt_row gt_right">1279</td>
<td headers="stub_1_8 visits" class="gt_row gt_right">1018</td>
<td headers="stub_1_8 charges" class="gt_row gt_right">476005</td>
<td headers="stub_1_8 allowed" class="gt_row gt_right">141674.71</td>
<td headers="stub_1_8 revenue" class="gt_row gt_right">140850.71</td>
<td headers="stub_1_8 adjustments" class="gt_row gt_right">301252.45</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_left gt_stub">AMBETTER</th>
<td headers="stub_1_9 patients" class="gt_row gt_right">174</td>
<td headers="stub_1_9 avg_age" class="gt_row gt_right">33.93103</td>
<td headers="stub_1_9 procedures" class="gt_row gt_right">967</td>
<td headers="stub_1_9 visits" class="gt_row gt_right">904</td>
<td headers="stub_1_9 charges" class="gt_row gt_right">215805</td>
<td headers="stub_1_9 allowed" class="gt_row gt_right">80836.37</td>
<td headers="stub_1_9 revenue" class="gt_row gt_right">76822.76</td>
<td headers="stub_1_9 adjustments" class="gt_row gt_right">123926.40</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_left gt_stub">TRICARE WEST</th>
<td headers="stub_1_10 patients" class="gt_row gt_right">159</td>
<td headers="stub_1_10 avg_age" class="gt_row gt_right">23.72956</td>
<td headers="stub_1_10 procedures" class="gt_row gt_right">811</td>
<td headers="stub_1_10 visits" class="gt_row gt_right">645</td>
<td headers="stub_1_10 charges" class="gt_row gt_right">241225</td>
<td headers="stub_1_10 allowed" class="gt_row gt_right">69920.50</td>
<td headers="stub_1_10 revenue" class="gt_row gt_right">68832.31</td>
<td headers="stub_1_10 adjustments" class="gt_row gt_right">167099.63</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_left gt_stub">HUMANA</th>
<td headers="stub_1_11 patients" class="gt_row gt_right">149</td>
<td headers="stub_1_11 avg_age" class="gt_row gt_right">58.39597</td>
<td headers="stub_1_11 procedures" class="gt_row gt_right">732</td>
<td headers="stub_1_11 visits" class="gt_row gt_right">330</td>
<td headers="stub_1_11 charges" class="gt_row gt_right">199090</td>
<td headers="stub_1_11 allowed" class="gt_row gt_right">50943.39</td>
<td headers="stub_1_11 revenue" class="gt_row gt_right">49748.17</td>
<td headers="stub_1_11 adjustments" class="gt_row gt_right">142202.78</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_left gt_stub">MEDICAID</th>
<td headers="stub_1_12 patients" class="gt_row gt_right">60</td>
<td headers="stub_1_12 avg_age" class="gt_row gt_right">38.56667</td>
<td headers="stub_1_12 procedures" class="gt_row gt_right">255</td>
<td headers="stub_1_12 visits" class="gt_row gt_right">87</td>
<td headers="stub_1_12 charges" class="gt_row gt_right">84010</td>
<td headers="stub_1_12 allowed" class="gt_row gt_right">26401.45</td>
<td headers="stub_1_12 revenue" class="gt_row gt_right">26224.00</td>
<td headers="stub_1_12 adjustments" class="gt_row gt_right">50286.00</td></tr>
    <tr><th id="stub_1_13" scope="row" class="gt_row gt_left gt_stub">MERITAIN</th>
<td headers="stub_1_13 patients" class="gt_row gt_right">45</td>
<td headers="stub_1_13 avg_age" class="gt_row gt_right">24.48889</td>
<td headers="stub_1_13 procedures" class="gt_row gt_right">206</td>
<td headers="stub_1_13 visits" class="gt_row gt_right">163</td>
<td headers="stub_1_13 charges" class="gt_row gt_right">52500</td>
<td headers="stub_1_13 allowed" class="gt_row gt_right">19143.52</td>
<td headers="stub_1_13 revenue" class="gt_row gt_right">18537.76</td>
<td headers="stub_1_13 adjustments" class="gt_row gt_right">32399.90</td></tr>
    <tr><th id="stub_1_14" scope="row" class="gt_row gt_left gt_stub">VA</th>
<td headers="stub_1_14 patients" class="gt_row gt_right">32</td>
<td headers="stub_1_14 avg_age" class="gt_row gt_right">54.12500</td>
<td headers="stub_1_14 procedures" class="gt_row gt_right">140</td>
<td headers="stub_1_14 visits" class="gt_row gt_right">60</td>
<td headers="stub_1_14 charges" class="gt_row gt_right">41865</td>
<td headers="stub_1_14 allowed" class="gt_row gt_right">11575.76</td>
<td headers="stub_1_14 revenue" class="gt_row gt_right">11575.76</td>
<td headers="stub_1_14 adjustments" class="gt_row gt_right">27735.43</td></tr>
    <tr><th id="stub_1_15" scope="row" class="gt_row gt_left gt_stub">TRUSTMARK</th>
<td headers="stub_1_15 patients" class="gt_row gt_right">20</td>
<td headers="stub_1_15 avg_age" class="gt_row gt_right">27.90000</td>
<td headers="stub_1_15 procedures" class="gt_row gt_right">86</td>
<td headers="stub_1_15 visits" class="gt_row gt_right">75</td>
<td headers="stub_1_15 charges" class="gt_row gt_right">20680</td>
<td headers="stub_1_15 allowed" class="gt_row gt_right">6352.17</td>
<td headers="stub_1_15 revenue" class="gt_row gt_right">6403.32</td>
<td headers="stub_1_15 adjustments" class="gt_row gt_right">11933.25</td></tr>
    <tr><th id="stub_1_16" scope="row" class="gt_row gt_left gt_stub">RAILROAD</th>
<td headers="stub_1_16 patients" class="gt_row gt_right">13</td>
<td headers="stub_1_16 avg_age" class="gt_row gt_right">73.61538</td>
<td headers="stub_1_16 procedures" class="gt_row gt_right">71</td>
<td headers="stub_1_16 visits" class="gt_row gt_right">35</td>
<td headers="stub_1_16 charges" class="gt_row gt_right">19435</td>
<td headers="stub_1_16 allowed" class="gt_row gt_right">7233.90</td>
<td headers="stub_1_16 revenue" class="gt_row gt_right">7164.62</td>
<td headers="stub_1_16 adjustments" class="gt_row gt_right">12310.38</td></tr>
    <tr><th id="stub_1_17" scope="row" class="gt_row gt_left gt_stub">TRICARE EAST</th>
<td headers="stub_1_17 patients" class="gt_row gt_right">11</td>
<td headers="stub_1_17 avg_age" class="gt_row gt_right">21.36364</td>
<td headers="stub_1_17 procedures" class="gt_row gt_right">66</td>
<td headers="stub_1_17 visits" class="gt_row gt_right">61</td>
<td headers="stub_1_17 charges" class="gt_row gt_right">13570</td>
<td headers="stub_1_17 allowed" class="gt_row gt_right">6160.89</td>
<td headers="stub_1_17 revenue" class="gt_row gt_right">5924.85</td>
<td headers="stub_1_17 adjustments" class="gt_row gt_right">7409.11</td></tr>
    <tr><th id="stub_1_18" scope="row" class="gt_row gt_left gt_stub">MAGELLAN</th>
<td headers="stub_1_18 patients" class="gt_row gt_right">21</td>
<td headers="stub_1_18 avg_age" class="gt_row gt_right">35.71429</td>
<td headers="stub_1_18 procedures" class="gt_row gt_right">62</td>
<td headers="stub_1_18 visits" class="gt_row gt_right">58</td>
<td headers="stub_1_18 charges" class="gt_row gt_right">14050</td>
<td headers="stub_1_18 allowed" class="gt_row gt_right">1530.98</td>
<td headers="stub_1_18 revenue" class="gt_row gt_right">1389.98</td>
<td headers="stub_1_18 adjustments" class="gt_row gt_right">3244.02</td></tr>
    <tr><th id="stub_1_19" scope="row" class="gt_row gt_left gt_stub">SEDGWICK</th>
<td headers="stub_1_19 patients" class="gt_row gt_right">2</td>
<td headers="stub_1_19 avg_age" class="gt_row gt_right">47.00000</td>
<td headers="stub_1_19 procedures" class="gt_row gt_right">20</td>
<td headers="stub_1_19 visits" class="gt_row gt_right">2</td>
<td headers="stub_1_19 charges" class="gt_row gt_right">6825</td>
<td headers="stub_1_19 allowed" class="gt_row gt_right">1216.83</td>
<td headers="stub_1_19 revenue" class="gt_row gt_right">1216.83</td>
<td headers="stub_1_19 adjustments" class="gt_row gt_right">283.17</td></tr>
    <tr><th id="stub_1_20" scope="row" class="gt_row gt_left gt_stub">LIBERTY</th>
<td headers="stub_1_20 patients" class="gt_row gt_right">1</td>
<td headers="stub_1_20 avg_age" class="gt_row gt_right">73.00000</td>
<td headers="stub_1_20 procedures" class="gt_row gt_right">15</td>
<td headers="stub_1_20 visits" class="gt_row gt_right">2</td>
<td headers="stub_1_20 charges" class="gt_row gt_right">5050</td>
<td headers="stub_1_20 allowed" class="gt_row gt_right">1381.57</td>
<td headers="stub_1_20 revenue" class="gt_row gt_right">1381.57</td>
<td headers="stub_1_20 adjustments" class="gt_row gt_right">118.43</td></tr>
    <tr><th id="stub_1_21" scope="row" class="gt_row gt_left gt_stub">WELLCARE</th>
<td headers="stub_1_21 patients" class="gt_row gt_right">4</td>
<td headers="stub_1_21 avg_age" class="gt_row gt_right">56.75000</td>
<td headers="stub_1_21 procedures" class="gt_row gt_right">14</td>
<td headers="stub_1_21 visits" class="gt_row gt_right">6</td>
<td headers="stub_1_21 charges" class="gt_row gt_right">4350</td>
<td headers="stub_1_21 allowed" class="gt_row gt_right">1029.80</td>
<td headers="stub_1_21 revenue" class="gt_row gt_right">1011.22</td>
<td headers="stub_1_21 adjustments" class="gt_row gt_right">2538.78</td></tr>
    <tr><th id="stub_1_22" scope="row" class="gt_row gt_left gt_stub">HARTFORD</th>
<td headers="stub_1_22 patients" class="gt_row gt_right">2</td>
<td headers="stub_1_22 avg_age" class="gt_row gt_right">52.00000</td>
<td headers="stub_1_22 procedures" class="gt_row gt_right">10</td>
<td headers="stub_1_22 visits" class="gt_row gt_right">2</td>
<td headers="stub_1_22 charges" class="gt_row gt_right">3550</td>
<td headers="stub_1_22 allowed" class="gt_row gt_right">1583.00</td>
<td headers="stub_1_22 revenue" class="gt_row gt_right">1795.50</td>
<td headers="stub_1_22 adjustments" class="gt_row gt_right">542.00</td></tr>
  </tbody>
  
  
</table>
</div>
```

:::
:::

::: {.cell layout-align="center"}

```{.r .cell-code}
insurance_summary |> 
  reframe(
    ins_prim,
    pts = patients,
    vpp = visits / patients,
    ppp = procedures / patients,
    rpp = revenue / patients,
    ppv = procedures / visits,
    rpv = revenue / visits,
    rpp = revenue / procedures
  ) |> 
  arrange(desc(pts)) |>
  gt(rowname_col = "ins_prim") |> 
  opt_all_caps() |> 
  opt_table_font(font = google_font(name = "Fira Code")) |> 
  tab_options(table.width = pct(100),
              quarto.disable_processing = TRUE)
```

::: {.cell-output-display}

```{=html}
<div id="kdcbeijzbc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Fira+Code:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#kdcbeijzbc table {
  font-family: 'Fira Code', system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#kdcbeijzbc thead, #kdcbeijzbc tbody, #kdcbeijzbc tfoot, #kdcbeijzbc tr, #kdcbeijzbc td, #kdcbeijzbc th {
  border-style: none;
}

#kdcbeijzbc p {
  margin: 0;
  padding: 0;
}

#kdcbeijzbc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 100%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#kdcbeijzbc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#kdcbeijzbc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kdcbeijzbc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kdcbeijzbc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kdcbeijzbc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kdcbeijzbc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kdcbeijzbc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#kdcbeijzbc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#kdcbeijzbc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kdcbeijzbc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kdcbeijzbc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#kdcbeijzbc .gt_spanner_row {
  border-bottom-style: hidden;
}

#kdcbeijzbc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#kdcbeijzbc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#kdcbeijzbc .gt_from_md > :first-child {
  margin-top: 0;
}

#kdcbeijzbc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kdcbeijzbc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#kdcbeijzbc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#kdcbeijzbc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#kdcbeijzbc .gt_row_group_first td {
  border-top-width: 2px;
}

#kdcbeijzbc .gt_row_group_first th {
  border-top-width: 2px;
}

#kdcbeijzbc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kdcbeijzbc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#kdcbeijzbc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#kdcbeijzbc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kdcbeijzbc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kdcbeijzbc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kdcbeijzbc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#kdcbeijzbc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kdcbeijzbc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kdcbeijzbc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kdcbeijzbc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kdcbeijzbc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kdcbeijzbc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kdcbeijzbc .gt_left {
  text-align: left;
}

#kdcbeijzbc .gt_center {
  text-align: center;
}

#kdcbeijzbc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kdcbeijzbc .gt_font_normal {
  font-weight: normal;
}

#kdcbeijzbc .gt_font_bold {
  font-weight: bold;
}

#kdcbeijzbc .gt_font_italic {
  font-style: italic;
}

#kdcbeijzbc .gt_super {
  font-size: 65%;
}

#kdcbeijzbc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#kdcbeijzbc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#kdcbeijzbc .gt_indent_1 {
  text-indent: 5px;
}

#kdcbeijzbc .gt_indent_2 {
  text-indent: 10px;
}

#kdcbeijzbc .gt_indent_3 {
  text-indent: 15px;
}

#kdcbeijzbc .gt_indent_4 {
  text-indent: 20px;
}

#kdcbeijzbc .gt_indent_5 {
  text-indent: 25px;
}

#kdcbeijzbc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#kdcbeijzbc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="true" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="pts">pts</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="vpp">vpp</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ppp">ppp</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="rpp">rpp</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ppv">ppv</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="rpv">rpv</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">SELF</th>
<td headers="stub_1_1 pts" class="gt_row gt_right">2801</td>
<td headers="stub_1_1 vpp" class="gt_row gt_right">2.910746</td>
<td headers="stub_1_1 ppp" class="gt_row gt_right">3.131025</td>
<td headers="stub_1_1 rpp" class="gt_row gt_right">88.15805</td>
<td headers="stub_1_1 ppv" class="gt_row gt_right">1.075678</td>
<td headers="stub_1_1 rpv" class="gt_row gt_right">94.82964</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">BCBS</th>
<td headers="stub_1_2 pts" class="gt_row gt_right">2155</td>
<td headers="stub_1_2 vpp" class="gt_row gt_right">4.337819</td>
<td headers="stub_1_2 ppp" class="gt_row gt_right">5.138283</td>
<td headers="stub_1_2 rpp" class="gt_row gt_right">95.03048</td>
<td headers="stub_1_2 ppv" class="gt_row gt_right">1.184531</td>
<td headers="stub_1_2 rpv" class="gt_row gt_right">112.56659</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">UHC</th>
<td headers="stub_1_3 pts" class="gt_row gt_right">1877</td>
<td headers="stub_1_3 vpp" class="gt_row gt_right">3.991476</td>
<td headers="stub_1_3 ppp" class="gt_row gt_right">5.376132</td>
<td headers="stub_1_3 rpp" class="gt_row gt_right">86.53564</td>
<td headers="stub_1_3 ppv" class="gt_row gt_right">1.346903</td>
<td headers="stub_1_3 rpv" class="gt_row gt_right">116.55515</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">AETNA</th>
<td headers="stub_1_4 pts" class="gt_row gt_right">1010</td>
<td headers="stub_1_4 vpp" class="gt_row gt_right">3.951485</td>
<td headers="stub_1_4 ppp" class="gt_row gt_right">4.852475</td>
<td headers="stub_1_4 rpp" class="gt_row gt_right">87.80680</td>
<td headers="stub_1_4 ppv" class="gt_row gt_right">1.228013</td>
<td headers="stub_1_4 rpv" class="gt_row gt_right">107.82789</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">CIGNA</th>
<td headers="stub_1_5 pts" class="gt_row gt_right">758</td>
<td headers="stub_1_5 vpp" class="gt_row gt_right">4.816623</td>
<td headers="stub_1_5 ppp" class="gt_row gt_right">5.626649</td>
<td headers="stub_1_5 rpp" class="gt_row gt_right">75.82996</td>
<td headers="stub_1_5 ppv" class="gt_row gt_right">1.168173</td>
<td headers="stub_1_5 rpv" class="gt_row gt_right">88.58251</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub">OTHER</th>
<td headers="stub_1_6 pts" class="gt_row gt_right">744</td>
<td headers="stub_1_6 vpp" class="gt_row gt_right">3.247312</td>
<td headers="stub_1_6 ppp" class="gt_row gt_right">3.768817</td>
<td headers="stub_1_6 rpp" class="gt_row gt_right">174.68398</td>
<td headers="stub_1_6 ppv" class="gt_row gt_right">1.160596</td>
<td headers="stub_1_6 rpv" class="gt_row gt_right">202.73754</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub">MEDICARE</th>
<td headers="stub_1_7 pts" class="gt_row gt_right">547</td>
<td headers="stub_1_7 vpp" class="gt_row gt_right">2.449726</td>
<td headers="stub_1_7 ppp" class="gt_row gt_right">5.422303</td>
<td headers="stub_1_7 rpp" class="gt_row gt_right">96.85183</td>
<td headers="stub_1_7 ppv" class="gt_row gt_right">2.213433</td>
<td headers="stub_1_7 rpv" class="gt_row gt_right">214.37502</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub">SUNFLOWER</th>
<td headers="stub_1_8 pts" class="gt_row gt_right">283</td>
<td headers="stub_1_8 vpp" class="gt_row gt_right">3.597173</td>
<td headers="stub_1_8 ppp" class="gt_row gt_right">4.519435</td>
<td headers="stub_1_8 rpp" class="gt_row gt_right">110.12565</td>
<td headers="stub_1_8 ppv" class="gt_row gt_right">1.256385</td>
<td headers="stub_1_8 rpv" class="gt_row gt_right">138.36023</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_left gt_stub">AMBETTER</th>
<td headers="stub_1_9 pts" class="gt_row gt_right">174</td>
<td headers="stub_1_9 vpp" class="gt_row gt_right">5.195402</td>
<td headers="stub_1_9 ppp" class="gt_row gt_right">5.557471</td>
<td headers="stub_1_9 rpp" class="gt_row gt_right">79.44443</td>
<td headers="stub_1_9 ppv" class="gt_row gt_right">1.069690</td>
<td headers="stub_1_9 rpv" class="gt_row gt_right">84.98093</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_left gt_stub">TRICARE WEST</th>
<td headers="stub_1_10 pts" class="gt_row gt_right">159</td>
<td headers="stub_1_10 vpp" class="gt_row gt_right">4.056604</td>
<td headers="stub_1_10 ppp" class="gt_row gt_right">5.100629</td>
<td headers="stub_1_10 rpp" class="gt_row gt_right">84.87338</td>
<td headers="stub_1_10 ppv" class="gt_row gt_right">1.257364</td>
<td headers="stub_1_10 rpv" class="gt_row gt_right">106.71676</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_left gt_stub">HUMANA</th>
<td headers="stub_1_11 pts" class="gt_row gt_right">149</td>
<td headers="stub_1_11 vpp" class="gt_row gt_right">2.214765</td>
<td headers="stub_1_11 ppp" class="gt_row gt_right">4.912752</td>
<td headers="stub_1_11 rpp" class="gt_row gt_right">67.96198</td>
<td headers="stub_1_11 ppv" class="gt_row gt_right">2.218182</td>
<td headers="stub_1_11 rpv" class="gt_row gt_right">150.75203</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_left gt_stub">MEDICAID</th>
<td headers="stub_1_12 pts" class="gt_row gt_right">60</td>
<td headers="stub_1_12 vpp" class="gt_row gt_right">1.450000</td>
<td headers="stub_1_12 ppp" class="gt_row gt_right">4.250000</td>
<td headers="stub_1_12 rpp" class="gt_row gt_right">102.83922</td>
<td headers="stub_1_12 ppv" class="gt_row gt_right">2.931034</td>
<td headers="stub_1_12 rpv" class="gt_row gt_right">301.42529</td></tr>
    <tr><th id="stub_1_13" scope="row" class="gt_row gt_left gt_stub">MERITAIN</th>
<td headers="stub_1_13 pts" class="gt_row gt_right">45</td>
<td headers="stub_1_13 vpp" class="gt_row gt_right">3.622222</td>
<td headers="stub_1_13 ppp" class="gt_row gt_right">4.577778</td>
<td headers="stub_1_13 rpp" class="gt_row gt_right">89.98913</td>
<td headers="stub_1_13 ppv" class="gt_row gt_right">1.263804</td>
<td headers="stub_1_13 rpv" class="gt_row gt_right">113.72859</td></tr>
    <tr><th id="stub_1_14" scope="row" class="gt_row gt_left gt_stub">VA</th>
<td headers="stub_1_14 pts" class="gt_row gt_right">32</td>
<td headers="stub_1_14 vpp" class="gt_row gt_right">1.875000</td>
<td headers="stub_1_14 ppp" class="gt_row gt_right">4.375000</td>
<td headers="stub_1_14 rpp" class="gt_row gt_right">82.68400</td>
<td headers="stub_1_14 ppv" class="gt_row gt_right">2.333333</td>
<td headers="stub_1_14 rpv" class="gt_row gt_right">192.92933</td></tr>
    <tr><th id="stub_1_15" scope="row" class="gt_row gt_left gt_stub">MAGELLAN</th>
<td headers="stub_1_15 pts" class="gt_row gt_right">21</td>
<td headers="stub_1_15 vpp" class="gt_row gt_right">2.761905</td>
<td headers="stub_1_15 ppp" class="gt_row gt_right">2.952381</td>
<td headers="stub_1_15 rpp" class="gt_row gt_right">22.41903</td>
<td headers="stub_1_15 ppv" class="gt_row gt_right">1.068966</td>
<td headers="stub_1_15 rpv" class="gt_row gt_right">23.96517</td></tr>
    <tr><th id="stub_1_16" scope="row" class="gt_row gt_left gt_stub">TRUSTMARK</th>
<td headers="stub_1_16 pts" class="gt_row gt_right">20</td>
<td headers="stub_1_16 vpp" class="gt_row gt_right">3.750000</td>
<td headers="stub_1_16 ppp" class="gt_row gt_right">4.300000</td>
<td headers="stub_1_16 rpp" class="gt_row gt_right">74.45721</td>
<td headers="stub_1_16 ppv" class="gt_row gt_right">1.146667</td>
<td headers="stub_1_16 rpv" class="gt_row gt_right">85.37760</td></tr>
    <tr><th id="stub_1_17" scope="row" class="gt_row gt_left gt_stub">RAILROAD</th>
<td headers="stub_1_17 pts" class="gt_row gt_right">13</td>
<td headers="stub_1_17 vpp" class="gt_row gt_right">2.692308</td>
<td headers="stub_1_17 ppp" class="gt_row gt_right">5.461538</td>
<td headers="stub_1_17 rpp" class="gt_row gt_right">100.91014</td>
<td headers="stub_1_17 ppv" class="gt_row gt_right">2.028571</td>
<td headers="stub_1_17 rpv" class="gt_row gt_right">204.70343</td></tr>
    <tr><th id="stub_1_18" scope="row" class="gt_row gt_left gt_stub">TRICARE EAST</th>
<td headers="stub_1_18 pts" class="gt_row gt_right">11</td>
<td headers="stub_1_18 vpp" class="gt_row gt_right">5.545455</td>
<td headers="stub_1_18 ppp" class="gt_row gt_right">6.000000</td>
<td headers="stub_1_18 rpp" class="gt_row gt_right">89.77045</td>
<td headers="stub_1_18 ppv" class="gt_row gt_right">1.081967</td>
<td headers="stub_1_18 rpv" class="gt_row gt_right">97.12869</td></tr>
    <tr><th id="stub_1_19" scope="row" class="gt_row gt_left gt_stub">WELLCARE</th>
<td headers="stub_1_19 pts" class="gt_row gt_right">4</td>
<td headers="stub_1_19 vpp" class="gt_row gt_right">1.500000</td>
<td headers="stub_1_19 ppp" class="gt_row gt_right">3.500000</td>
<td headers="stub_1_19 rpp" class="gt_row gt_right">72.23000</td>
<td headers="stub_1_19 ppv" class="gt_row gt_right">2.333333</td>
<td headers="stub_1_19 rpv" class="gt_row gt_right">168.53667</td></tr>
    <tr><th id="stub_1_20" scope="row" class="gt_row gt_left gt_stub">SEDGWICK</th>
<td headers="stub_1_20 pts" class="gt_row gt_right">2</td>
<td headers="stub_1_20 vpp" class="gt_row gt_right">1.000000</td>
<td headers="stub_1_20 ppp" class="gt_row gt_right">10.000000</td>
<td headers="stub_1_20 rpp" class="gt_row gt_right">60.84150</td>
<td headers="stub_1_20 ppv" class="gt_row gt_right">10.000000</td>
<td headers="stub_1_20 rpv" class="gt_row gt_right">608.41500</td></tr>
    <tr><th id="stub_1_21" scope="row" class="gt_row gt_left gt_stub">HARTFORD</th>
<td headers="stub_1_21 pts" class="gt_row gt_right">2</td>
<td headers="stub_1_21 vpp" class="gt_row gt_right">1.000000</td>
<td headers="stub_1_21 ppp" class="gt_row gt_right">5.000000</td>
<td headers="stub_1_21 rpp" class="gt_row gt_right">179.55000</td>
<td headers="stub_1_21 ppv" class="gt_row gt_right">5.000000</td>
<td headers="stub_1_21 rpv" class="gt_row gt_right">897.75000</td></tr>
    <tr><th id="stub_1_22" scope="row" class="gt_row gt_left gt_stub">LIBERTY</th>
<td headers="stub_1_22 pts" class="gt_row gt_right">1</td>
<td headers="stub_1_22 vpp" class="gt_row gt_right">2.000000</td>
<td headers="stub_1_22 ppp" class="gt_row gt_right">15.000000</td>
<td headers="stub_1_22 rpp" class="gt_row gt_right">92.10467</td>
<td headers="stub_1_22 ppv" class="gt_row gt_right">7.500000</td>
<td headers="stub_1_22 rpv" class="gt_row gt_right">690.78500</td></tr>
  </tbody>
  
  
</table>
</div>
```

:::
:::





# Reimbursement




::: {.cell layout-align="center"}

```{.r .cell-code}
# mult = 0: "No adjustment. If procedure is reported on 
# the same day as another procedure, base the payment on 
# the lower of (a) the actual charge, or (b) the fee 
# schedule amount for the procedure."

# glob = ZZZ: "Code related to another service and is 
# always included in Global period of other service."

pprrvu |> 
  reframe(
    # dos, 
    yr = data.table::year(dos),
    qtr = data.table::quarter(dos),
    # mon = data.table::month(dos),
    pos,
    hcpcs, 
    # description, 
    # glob_days,
    # mult_proc,
    work_rvu, 
    pe_rvu, 
    mp_rvu, 
    tot_rvu = rvu_total,
    cf = conv_factor,
    allow_unadj = tot_rvu * cf,
    nonpar_unadj = allow_unadj * 1.15,
    lim_unadj = nonpar_unadj * 0.95) |> 
  distinct() |> 
  arrange(hcpcs, yr, qtr) |>
  # head(n = 50) |>
  gt(rowname_col = "year", 
     groupname_col = "hcpcs",
     row_group_as_column = TRUE) |> 
  opt_table_font(
    font = google_font(name = "Roboto Condensed")) |> 
  fmt_currency(
    columns = c("nonpar_unadj", 
                "allow_unadj", 
                "lim_unadj", 
                "cf")) |>
  cols_label(
    yr = md("**YR**"),
    qtr = md("**QTR**"),
    work_rvu = md("<b><small>RVU</small></b><i><sub>wk</sub></i>"),
    pe_rvu = md("<b><small>RVU</small></b><i><sub>pe</sub></i>"),
    mp_rvu = md("<b><small>RVU</small></b><i><sub>mp</sub></i>"),
    cf = md("<b><small>Conversion<br>Factor</small></b>"),
    tot_rvu = md("<b><small>RVU</small></b><i><sub>total</sub></i>"),
    allow_unadj = md("<b><small>Allowed<br>Amount</small></b>"),
    nonpar_unadj = md("<b><small>Non-Participating<br>Amount</small></b>"),
    lim_unadj = md("<b><small>Limiting<br>Charge</small></b>")) |> 
  cols_align(align = "right") |>
  tab_style(
      style = cell_text(
        align = 'center',
        size = px(16),
        font = google_font(name = "Fira Code"),
        weight = "bold"),
      locations = cells_row_groups()) |> 
  tab_style(
      style = cell_text(
        align = 'right',
        font = google_font(name = "Fira Code")),
      locations = cells_body()) |> 
  tab_options(
    quarto.disable_processing = TRUE,
    table.width = "100%"
  )
```

::: {.cell-output-display}

```{=html}
<div id="umujsqffrs" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Fira+Code:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Fira+Code:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#umujsqffrs table {
  font-family: 'Roboto Condensed', system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#umujsqffrs thead, #umujsqffrs tbody, #umujsqffrs tfoot, #umujsqffrs tr, #umujsqffrs td, #umujsqffrs th {
  border-style: none;
}

#umujsqffrs p {
  margin: 0;
  padding: 0;
}

#umujsqffrs .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 100%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#umujsqffrs .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#umujsqffrs .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#umujsqffrs .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#umujsqffrs .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#umujsqffrs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umujsqffrs .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#umujsqffrs .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#umujsqffrs .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#umujsqffrs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#umujsqffrs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#umujsqffrs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#umujsqffrs .gt_spanner_row {
  border-bottom-style: hidden;
}

#umujsqffrs .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#umujsqffrs .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#umujsqffrs .gt_from_md > :first-child {
  margin-top: 0;
}

#umujsqffrs .gt_from_md > :last-child {
  margin-bottom: 0;
}

#umujsqffrs .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#umujsqffrs .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#umujsqffrs .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#umujsqffrs .gt_row_group_first td {
  border-top-width: 2px;
}

#umujsqffrs .gt_row_group_first th {
  border-top-width: 2px;
}

#umujsqffrs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umujsqffrs .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#umujsqffrs .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#umujsqffrs .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umujsqffrs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umujsqffrs .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#umujsqffrs .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#umujsqffrs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#umujsqffrs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umujsqffrs .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#umujsqffrs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#umujsqffrs .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#umujsqffrs .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#umujsqffrs .gt_left {
  text-align: left;
}

#umujsqffrs .gt_center {
  text-align: center;
}

#umujsqffrs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#umujsqffrs .gt_font_normal {
  font-weight: normal;
}

#umujsqffrs .gt_font_bold {
  font-weight: bold;
}

#umujsqffrs .gt_font_italic {
  font-style: italic;
}

#umujsqffrs .gt_super {
  font-size: 65%;
}

#umujsqffrs .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#umujsqffrs .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#umujsqffrs .gt_indent_1 {
  text-indent: 5px;
}

#umujsqffrs .gt_indent_2 {
  text-indent: 10px;
}

#umujsqffrs .gt_indent_3 {
  text-indent: 15px;
}

#umujsqffrs .gt_indent_4 {
  text-indent: 20px;
}

#umujsqffrs .gt_indent_5 {
  text-indent: 25px;
}

#umujsqffrs .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#umujsqffrs div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="true" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;KipZUioq&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;strong&gt;YR&lt;/strong&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="KipZUioq"><div class='gt_from_md'><p><strong>YR</strong></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;KipRVFIqKg==&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;strong&gt;QTR&lt;/strong&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="KipRVFIqKg=="><div class='gt_from_md'><p><strong>QTR</strong></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="pos">pos</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;PGI+PHNtYWxsPlJWVTwvc21hbGw+PC9iPjxpPjxzdWI+d2s8L3N1Yj48L2k+&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;b&gt;&lt;small&gt;RVU&lt;/small&gt;&lt;/b&gt;&lt;i&gt;&lt;sub&gt;wk&lt;/sub&gt;&lt;/i&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="PGI+PHNtYWxsPlJWVTwvc21hbGw+PC9iPjxpPjxzdWI+d2s8L3N1Yj48L2k+"><div class='gt_from_md'><p><b><small>RVU</small></b><i><sub>wk</sub></i></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;PGI+PHNtYWxsPlJWVTwvc21hbGw+PC9iPjxpPjxzdWI+cGU8L3N1Yj48L2k+&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;b&gt;&lt;small&gt;RVU&lt;/small&gt;&lt;/b&gt;&lt;i&gt;&lt;sub&gt;pe&lt;/sub&gt;&lt;/i&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="PGI+PHNtYWxsPlJWVTwvc21hbGw+PC9iPjxpPjxzdWI+cGU8L3N1Yj48L2k+"><div class='gt_from_md'><p><b><small>RVU</small></b><i><sub>pe</sub></i></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;PGI+PHNtYWxsPlJWVTwvc21hbGw+PC9iPjxpPjxzdWI+bXA8L3N1Yj48L2k+&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;b&gt;&lt;small&gt;RVU&lt;/small&gt;&lt;/b&gt;&lt;i&gt;&lt;sub&gt;mp&lt;/sub&gt;&lt;/i&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="PGI+PHNtYWxsPlJWVTwvc21hbGw+PC9iPjxpPjxzdWI+bXA8L3N1Yj48L2k+"><div class='gt_from_md'><p><b><small>RVU</small></b><i><sub>mp</sub></i></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;PGI+PHNtYWxsPlJWVTwvc21hbGw+PC9iPjxpPjxzdWI+dG90YWw8L3N1Yj48L2k+&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;b&gt;&lt;small&gt;RVU&lt;/small&gt;&lt;/b&gt;&lt;i&gt;&lt;sub&gt;total&lt;/sub&gt;&lt;/i&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="PGI+PHNtYWxsPlJWVTwvc21hbGw+PC9iPjxpPjxzdWI+dG90YWw8L3N1Yj48L2k+"><div class='gt_from_md'><p><b><small>RVU</small></b><i><sub>total</sub></i></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;PGI+PHNtYWxsPkNvbnZlcnNpb248YnI+RmFjdG9yPC9zbWFsbD48L2I+&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;b&gt;&lt;small&gt;Conversion&lt;br&gt;Factor&lt;/small&gt;&lt;/b&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="PGI+PHNtYWxsPkNvbnZlcnNpb248YnI+RmFjdG9yPC9zbWFsbD48L2I+"><div class='gt_from_md'><p><b><small>Conversion<br>Factor</small></b></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;PGI+PHNtYWxsPkFsbG93ZWQ8YnI+QW1vdW50PC9zbWFsbD48L2I+&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;b&gt;&lt;small&gt;Allowed&lt;br&gt;Amount&lt;/small&gt;&lt;/b&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="PGI+PHNtYWxsPkFsbG93ZWQ8YnI+QW1vdW50PC9zbWFsbD48L2I+"><div class='gt_from_md'><p><b><small>Allowed<br>Amount</small></b></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;PGI+PHNtYWxsPk5vbi1QYXJ0aWNpcGF0aW5nPGJyPkFtb3VudDwvc21hbGw+PC9iPg==&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;b&gt;&lt;small&gt;Non-Participating&lt;br&gt;Amount&lt;/small&gt;&lt;/b&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="PGI+PHNtYWxsPk5vbi1QYXJ0aWNpcGF0aW5nPGJyPkFtb3VudDwvc21hbGw+PC9iPg=="><div class='gt_from_md'><p><b><small>Non-Participating<br>Amount</small></b></p>
</div></div></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&lt;div data-qmd-base64=&quot;PGI+PHNtYWxsPkxpbWl0aW5nPGJyPkNoYXJnZTwvc21hbGw+PC9iPg==&quot;&gt;&lt;div class='gt_from_md'&gt;&lt;p&gt;&lt;b&gt;&lt;small&gt;Limiting&lt;br&gt;Charge&lt;/small&gt;&lt;/b&gt;&lt;/p&gt;&#10;&lt;/div&gt;&lt;/div&gt;"><div data-qmd-base64="PGI+PHNtYWxsPkxpbWl0aW5nPGJyPkNoYXJnZTwvc21hbGw+PC9iPg=="><div class='gt_from_md'><p><b><small>Limiting<br>Charge</small></b></p>
</div></div></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_row_group_first"><td headers="80305 stub_1_1 stub_1" rowspan="2" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">80305</td>
<td headers="80305 stub_1_1 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="80305 stub_1_1 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="80305 stub_1_1 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="80305 stub_1_1 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="80305 stub_1_1 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="80305 stub_1_1 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="80305 stub_1_1 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="80305 stub_1_1 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="80305 stub_1_1 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="80305 stub_1_1 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="80305 stub_1_1 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td></tr>
    <tr><td headers="80305 yr_2 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="80305 yr_2 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="80305 yr_2 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="80305 yr_2 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="80305 yr_2 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="80305 yr_2 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="80305 yr_2 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="80305 yr_2 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="80305 yr_2 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="80305 yr_2 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="80305 yr_2 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td></tr>
    <tr class="gt_row_group_first"><td headers="90791 stub_1_3 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90791</td>
<td headers="90791 stub_1_3 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90791 stub_1_3 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90791 stub_1_3 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90791 stub_1_3 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 stub_1_3 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90791 stub_1_3 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="90791 stub_1_3 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.45</td>
<td headers="90791 stub_1_3 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90791 stub_1_3 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$154.00</td>
<td headers="90791 stub_1_3 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$177.10</td>
<td headers="90791 stub_1_3 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$168.24</td></tr>
    <tr><td headers="90791 yr_4 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90791 yr_4 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90791 yr_4 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90791 yr_4 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_4 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.21</td>
<td headers="90791 yr_4 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="90791 yr_4 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.17</td>
<td headers="90791 yr_4 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90791 yr_4 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$178.91</td>
<td headers="90791 yr_4 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$205.75</td>
<td headers="90791 yr_4 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$195.46</td></tr>
    <tr><td headers="90791 yr_5 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90791 yr_5 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90791 yr_5 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90791 yr_5 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_5 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.21</td>
<td headers="90791 yr_5 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="90791 yr_5 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.17</td>
<td headers="90791 yr_5 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90791 yr_5 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$178.91</td>
<td headers="90791 yr_5 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$205.75</td>
<td headers="90791 yr_5 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$195.46</td></tr>
    <tr><td headers="90791 yr_6 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90791 yr_6 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90791 yr_6 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90791 yr_6 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_6 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90791 yr_6 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="90791 yr_6 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.45</td>
<td headers="90791 yr_6 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90791 yr_6 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$154.00</td>
<td headers="90791 yr_6 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$177.10</td>
<td headers="90791 yr_6 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$168.24</td></tr>
    <tr><td headers="90791 yr_7 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90791 yr_7 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90791 yr_7 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90791 yr_7 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_7 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.22</td>
<td headers="90791 yr_7 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_7 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.16</td>
<td headers="90791 yr_7 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90791 yr_7 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$174.86</td>
<td headers="90791 yr_7 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$201.09</td>
<td headers="90791 yr_7 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$191.03</td></tr>
    <tr><td headers="90791 yr_8 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90791 yr_8 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90791 yr_8 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90791 yr_8 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_8 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.51</td>
<td headers="90791 yr_8 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_8 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.45</td>
<td headers="90791 yr_8 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90791 yr_8 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$150.80</td>
<td headers="90791 yr_8 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$173.42</td>
<td headers="90791 yr_8 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$164.75</td></tr>
    <tr><td headers="90791 yr_9 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90791 yr_9 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90791 yr_9 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90791 yr_9 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_9 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.22</td>
<td headers="90791 yr_9 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_9 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.16</td>
<td headers="90791 yr_9 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90791 yr_9 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$174.86</td>
<td headers="90791 yr_9 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$201.09</td>
<td headers="90791 yr_9 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$191.03</td></tr>
    <tr><td headers="90791 yr_10 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90791 yr_10 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90791 yr_10 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90791 yr_10 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_10 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.51</td>
<td headers="90791 yr_10 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_10 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.45</td>
<td headers="90791 yr_10 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90791 yr_10 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$150.80</td>
<td headers="90791 yr_10 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$173.42</td>
<td headers="90791 yr_10 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$164.75</td></tr>
    <tr><td headers="90791 yr_11 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90791 yr_11 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90791 yr_11 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90791 yr_11 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_11 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.22</td>
<td headers="90791 yr_11 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_11 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.16</td>
<td headers="90791 yr_11 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90791 yr_11 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$174.86</td>
<td headers="90791 yr_11 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$201.09</td>
<td headers="90791 yr_11 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$191.03</td></tr>
    <tr><td headers="90791 yr_12 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90791 yr_12 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90791 yr_12 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90791 yr_12 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_12 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.51</td>
<td headers="90791 yr_12 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_12 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.45</td>
<td headers="90791 yr_12 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90791 yr_12 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$150.80</td>
<td headers="90791 yr_12 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$173.42</td>
<td headers="90791 yr_12 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$164.75</td></tr>
    <tr><td headers="90791 yr_13 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90791 yr_13 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90791 yr_13 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90791 yr_13 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_13 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.51</td>
<td headers="90791 yr_13 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_13 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.45</td>
<td headers="90791 yr_13 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90791 yr_13 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$150.80</td>
<td headers="90791 yr_13 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$173.42</td>
<td headers="90791 yr_13 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$164.75</td></tr>
    <tr><td headers="90791 yr_14 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90791 yr_14 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90791 yr_14 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90791 yr_14 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_14 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.22</td>
<td headers="90791 yr_14 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_14 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.16</td>
<td headers="90791 yr_14 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90791 yr_14 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$174.86</td>
<td headers="90791 yr_14 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$201.09</td>
<td headers="90791 yr_14 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$191.03</td></tr>
    <tr><td headers="90791 yr_15 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90791 yr_15 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90791 yr_15 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90791 yr_15 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_15 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.23</td>
<td headers="90791 yr_15 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_15 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.17</td>
<td headers="90791 yr_15 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90791 yr_15 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$172.10</td>
<td headers="90791 yr_15 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$197.91</td>
<td headers="90791 yr_15 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$188.02</td></tr>
    <tr><td headers="90791 yr_16 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90791 yr_16 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90791 yr_16 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90791 yr_16 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90791 yr_16 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90791 yr_16 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90791 yr_16 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.43</td>
<td headers="90791 yr_16 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90791 yr_16 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.46</td>
<td headers="90791 yr_16 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$169.58</td>
<td headers="90791 yr_16 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$161.10</td></tr>
    <tr class="gt_row_group_first"><td headers="90792 stub_1_17 stub_1" rowspan="5" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90792</td>
<td headers="90792 stub_1_17 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90792 stub_1_17 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90792 stub_1_17 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90792 stub_1_17 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.16</td>
<td headers="90792 stub_1_17 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.46</td>
<td headers="90792 stub_1_17 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="90792 stub_1_17 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.79</td>
<td headers="90792 stub_1_17 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90792 stub_1_17 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$200.37</td>
<td headers="90792 stub_1_17 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$230.43</td>
<td headers="90792 stub_1_17 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$218.90</td></tr>
    <tr><td headers="90792 yr_18 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90792 yr_18 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90792 yr_18 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90792 yr_18 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.16</td>
<td headers="90792 yr_18 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.46</td>
<td headers="90792 yr_18 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.18</td>
<td headers="90792 yr_18 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.80</td>
<td headers="90792 yr_18 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90792 yr_18 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$196.55</td>
<td headers="90792 yr_18 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$226.03</td>
<td headers="90792 yr_18 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$214.73</td></tr>
    <tr><td headers="90792 yr_19 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90792 yr_19 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90792 yr_19 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90792 yr_19 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.16</td>
<td headers="90792 yr_19 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.46</td>
<td headers="90792 yr_19 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.18</td>
<td headers="90792 yr_19 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.80</td>
<td headers="90792 yr_19 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90792 yr_19 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$196.55</td>
<td headers="90792 yr_19 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$226.03</td>
<td headers="90792 yr_19 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$214.73</td></tr>
    <tr><td headers="90792 yr_20 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90792 yr_20 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90792 yr_20 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90792 yr_20 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.16</td>
<td headers="90792 yr_20 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.49</td>
<td headers="90792 yr_20 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="90792 yr_20 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.82</td>
<td headers="90792 yr_20 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90792 yr_20 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$193.73</td>
<td headers="90792 yr_20 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$222.79</td>
<td headers="90792 yr_20 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$211.65</td></tr>
    <tr><td headers="90792 yr_21 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90792 yr_21 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90792 yr_21 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90792 yr_21 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.16</td>
<td headers="90792 yr_21 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.75</td>
<td headers="90792 yr_21 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="90792 yr_21 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.08</td>
<td headers="90792 yr_21 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90792 yr_21 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$169.10</td>
<td headers="90792 yr_21 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$194.47</td>
<td headers="90792 yr_21 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$184.74</td></tr>
    <tr class="gt_row_group_first"><td headers="90827 stub_1_22 stub_1" rowspan="1" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90827</td>
<td headers="90827 stub_1_22 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90827 stub_1_22 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90827 stub_1_22 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90827 stub_1_22 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90827 stub_1_22 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90827 stub_1_22 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90827 stub_1_22 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90827 stub_1_22 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90827 stub_1_22 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90827 stub_1_22 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90827 stub_1_22 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr class="gt_row_group_first"><td headers="90832 stub_1_23 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90832</td>
<td headers="90832 stub_1_23 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90832 stub_1_23 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90832 stub_1_23 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90832 stub_1_23 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 stub_1_23 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.48</td>
<td headers="90832 stub_1_23 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90832 stub_1_23 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.25</td>
<td headers="90832 stub_1_23 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90832 stub_1_23 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$77.86</td>
<td headers="90832 stub_1_23 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$89.54</td>
<td headers="90832 stub_1_23 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$85.07</td></tr>
    <tr><td headers="90832 yr_24 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90832 yr_24 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90832 yr_24 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90832 yr_24 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_24 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.22</td>
<td headers="90832 yr_24 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90832 yr_24 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.99</td>
<td headers="90832 yr_24 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90832 yr_24 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$68.87</td>
<td headers="90832 yr_24 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$79.20</td>
<td headers="90832 yr_24 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.24</td></tr>
    <tr><td headers="90832 yr_25 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90832 yr_25 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90832 yr_25 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90832 yr_25 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_25 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.48</td>
<td headers="90832 yr_25 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90832 yr_25 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.25</td>
<td headers="90832 yr_25 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90832 yr_25 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$77.86</td>
<td headers="90832 yr_25 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$89.54</td>
<td headers="90832 yr_25 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$85.07</td></tr>
    <tr><td headers="90832 yr_26 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90832 yr_26 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90832 yr_26 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90832 yr_26 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_26 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.22</td>
<td headers="90832 yr_26 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90832 yr_26 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.99</td>
<td headers="90832 yr_26 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90832 yr_26 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$68.87</td>
<td headers="90832 yr_26 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$79.20</td>
<td headers="90832 yr_26 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.24</td></tr>
    <tr><td headers="90832 yr_27 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90832 yr_27 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90832 yr_27 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90832 yr_27 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_27 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90832 yr_27 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="90832 yr_27 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.23</td>
<td headers="90832 yr_27 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90832 yr_27 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.57</td>
<td headers="90832 yr_27 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$86.90</td>
<td headers="90832 yr_27 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$82.56</td></tr>
    <tr><td headers="90832 yr_28 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90832 yr_28 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90832 yr_28 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90832 yr_28 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_28 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.22</td>
<td headers="90832 yr_28 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="90832 yr_28 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="90832 yr_28 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90832 yr_28 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$66.42</td>
<td headers="90832 yr_28 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.38</td>
<td headers="90832 yr_28 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$72.56</td></tr>
    <tr><td headers="90832 yr_29 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90832 yr_29 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90832 yr_29 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90832 yr_29 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_29 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.22</td>
<td headers="90832 yr_29 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="90832 yr_29 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="90832 yr_29 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90832 yr_29 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$66.42</td>
<td headers="90832 yr_29 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.38</td>
<td headers="90832 yr_29 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$72.56</td></tr>
    <tr><td headers="90832 yr_30 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90832 yr_30 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90832 yr_30 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90832 yr_30 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_30 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90832 yr_30 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="90832 yr_30 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.23</td>
<td headers="90832 yr_30 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90832 yr_30 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.57</td>
<td headers="90832 yr_30 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$86.90</td>
<td headers="90832 yr_30 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$82.56</td></tr>
    <tr><td headers="90832 yr_31 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90832 yr_31 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90832 yr_31 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90832 yr_31 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_31 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90832 yr_31 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="90832 yr_31 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.23</td>
<td headers="90832 yr_31 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90832 yr_31 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.57</td>
<td headers="90832 yr_31 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$86.90</td>
<td headers="90832 yr_31 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$82.56</td></tr>
    <tr><td headers="90832 yr_32 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90832 yr_32 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90832 yr_32 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90832 yr_32 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_32 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.22</td>
<td headers="90832 yr_32 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="90832 yr_32 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="90832 yr_32 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90832 yr_32 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$66.42</td>
<td headers="90832 yr_32 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.38</td>
<td headers="90832 yr_32 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$72.56</td></tr>
    <tr><td headers="90832 yr_33 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90832 yr_33 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90832 yr_33 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90832 yr_33 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_33 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90832 yr_33 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="90832 yr_33 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.23</td>
<td headers="90832 yr_33 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90832 yr_33 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.57</td>
<td headers="90832 yr_33 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$86.90</td>
<td headers="90832 yr_33 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$82.56</td></tr>
    <tr><td headers="90832 yr_34 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90832 yr_34 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90832 yr_34 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90832 yr_34 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.70</td>
<td headers="90832 yr_34 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.22</td>
<td headers="90832 yr_34 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="90832 yr_34 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="90832 yr_34 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90832 yr_34 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$66.42</td>
<td headers="90832 yr_34 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.38</td>
<td headers="90832 yr_34 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$72.56</td></tr>
    <tr><td headers="90832 yr_35 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90832 yr_35 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90832 yr_35 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90832 yr_35 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.78</td>
<td headers="90832 yr_35 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.52</td>
<td headers="90832 yr_35 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.05</td>
<td headers="90832 yr_35 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.35</td>
<td headers="90832 yr_35 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90832 yr_35 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$78.23</td>
<td headers="90832 yr_35 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$89.96</td>
<td headers="90832 yr_35 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$85.46</td></tr>
    <tr><td headers="90832 yr_36 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90832 yr_36 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90832 yr_36 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90832 yr_36 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.78</td>
<td headers="90832 yr_36 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.22</td>
<td headers="90832 yr_36 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.05</td>
<td headers="90832 yr_36 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.05</td>
<td headers="90832 yr_36 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90832 yr_36 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$68.24</td>
<td headers="90832 yr_36 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$78.48</td>
<td headers="90832 yr_36 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$74.55</td></tr>
    <tr class="gt_row_group_first"><td headers="90833 stub_1_37 stub_1" rowspan="1" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90833</td>
<td headers="90833 stub_1_37 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90833 stub_1_37 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90833 stub_1_37 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90833 stub_1_37 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.50</td>
<td headers="90833 stub_1_37 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90833 stub_1_37 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90833 stub_1_37 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.05</td>
<td headers="90833 stub_1_37 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90833 stub_1_37 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$69.47</td>
<td headers="90833 stub_1_37 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$79.89</td>
<td headers="90833 stub_1_37 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.89</td></tr>
    <tr class="gt_row_group_first"><td headers="90834 stub_1_38 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90834</td>
<td headers="90834 stub_1_38 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90834 stub_1_38 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90834 stub_1_38 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90834 stub_1_38 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 stub_1_38 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.64</td>
<td headers="90834 stub_1_38 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90834 stub_1_38 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.97</td>
<td headers="90834 stub_1_38 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90834 stub_1_38 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$102.78</td>
<td headers="90834 stub_1_38 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.20</td>
<td headers="90834 stub_1_38 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.29</td></tr>
    <tr><td headers="90834 yr_39 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90834 yr_39 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90834 yr_39 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90834 yr_39 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_39 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.29</td>
<td headers="90834 yr_39 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90834 yr_39 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.62</td>
<td headers="90834 yr_39 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90834 yr_39 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.67</td>
<td headers="90834 yr_39 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.27</td>
<td headers="90834 yr_39 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.06</td></tr>
    <tr><td headers="90834 yr_40 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90834 yr_40 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90834 yr_40 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90834 yr_40 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_40 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.64</td>
<td headers="90834 yr_40 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90834 yr_40 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.97</td>
<td headers="90834 yr_40 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90834 yr_40 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$102.78</td>
<td headers="90834 yr_40 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.20</td>
<td headers="90834 yr_40 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.29</td></tr>
    <tr><td headers="90834 yr_41 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90834 yr_41 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90834 yr_41 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90834 yr_41 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_41 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.29</td>
<td headers="90834 yr_41 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90834 yr_41 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.62</td>
<td headers="90834 yr_41 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90834 yr_41 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.67</td>
<td headers="90834 yr_41 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.27</td>
<td headers="90834 yr_41 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.06</td></tr>
    <tr><td headers="90834 yr_42 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90834 yr_42 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90834 yr_42 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90834 yr_42 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_42 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.30</td>
<td headers="90834 yr_42 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_42 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="90834 yr_42 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90834 yr_42 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.11</td>
<td headers="90834 yr_42 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.32</td>
<td headers="90834 yr_42 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$96.26</td></tr>
    <tr><td headers="90834 yr_43 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90834 yr_43 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90834 yr_43 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90834 yr_43 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_43 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.65</td>
<td headers="90834 yr_43 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_43 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.95</td>
<td headers="90834 yr_43 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90834 yr_43 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.97</td>
<td headers="90834 yr_43 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.96</td>
<td headers="90834 yr_43 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.21</td></tr>
    <tr><td headers="90834 yr_44 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90834 yr_44 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90834 yr_44 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90834 yr_44 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_44 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.65</td>
<td headers="90834 yr_44 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_44 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.95</td>
<td headers="90834 yr_44 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90834 yr_44 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.97</td>
<td headers="90834 yr_44 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.96</td>
<td headers="90834 yr_44 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.21</td></tr>
    <tr><td headers="90834 yr_45 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90834 yr_45 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90834 yr_45 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90834 yr_45 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_45 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.30</td>
<td headers="90834 yr_45 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_45 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="90834 yr_45 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90834 yr_45 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.11</td>
<td headers="90834 yr_45 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.32</td>
<td headers="90834 yr_45 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$96.26</td></tr>
    <tr><td headers="90834 yr_46 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90834 yr_46 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90834 yr_46 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90834 yr_46 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_46 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.65</td>
<td headers="90834 yr_46 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_46 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.95</td>
<td headers="90834 yr_46 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90834 yr_46 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.97</td>
<td headers="90834 yr_46 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.96</td>
<td headers="90834 yr_46 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.21</td></tr>
    <tr><td headers="90834 yr_47 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90834 yr_47 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90834 yr_47 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90834 yr_47 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_47 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.30</td>
<td headers="90834 yr_47 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_47 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="90834 yr_47 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90834 yr_47 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.11</td>
<td headers="90834 yr_47 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.32</td>
<td headers="90834 yr_47 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$96.26</td></tr>
    <tr><td headers="90834 yr_48 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90834 yr_48 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90834 yr_48 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90834 yr_48 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_48 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.65</td>
<td headers="90834 yr_48 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_48 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.95</td>
<td headers="90834 yr_48 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90834 yr_48 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.97</td>
<td headers="90834 yr_48 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.96</td>
<td headers="90834 yr_48 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.21</td></tr>
    <tr><td headers="90834 yr_49 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90834 yr_49 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90834 yr_49 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90834 yr_49 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="90834 yr_49 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.30</td>
<td headers="90834 yr_49 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_49 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="90834 yr_49 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90834 yr_49 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.11</td>
<td headers="90834 yr_49 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.32</td>
<td headers="90834 yr_49 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$96.26</td></tr>
    <tr><td headers="90834 yr_50 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90834 yr_50 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90834 yr_50 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90834 yr_50 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.35</td>
<td headers="90834 yr_50 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="90834 yr_50 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_50 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.10</td>
<td headers="90834 yr_50 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90834 yr_50 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$103.19</td>
<td headers="90834 yr_50 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.67</td>
<td headers="90834 yr_50 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.74</td></tr>
    <tr><td headers="90834 yr_51 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90834 yr_51 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90834 yr_51 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90834 yr_51 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.35</td>
<td headers="90834 yr_51 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.30</td>
<td headers="90834 yr_51 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="90834 yr_51 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.71</td>
<td headers="90834 yr_51 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90834 yr_51 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.21</td>
<td headers="90834 yr_51 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$103.74</td>
<td headers="90834 yr_51 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.55</td></tr>
    <tr class="gt_row_group_first"><td headers="90836 stub_1_52 stub_1" rowspan="5" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90836</td>
<td headers="90836 stub_1_52 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90836 stub_1_52 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90836 stub_1_52 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90836 stub_1_52 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.90</td>
<td headers="90836 stub_1_52 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.34</td>
<td headers="90836 stub_1_52 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90836 stub_1_52 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.32</td>
<td headers="90836 stub_1_52 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90836 stub_1_52 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$80.29</td>
<td headers="90836 stub_1_52 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$92.33</td>
<td headers="90836 stub_1_52 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$87.71</td></tr>
    <tr><td headers="90836 yr_53 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90836 yr_53 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90836 yr_53 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90836 yr_53 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.90</td>
<td headers="90836 yr_53 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.62</td>
<td headers="90836 yr_53 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90836 yr_53 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="90836 yr_53 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90836 yr_53 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$89.98</td>
<td headers="90836 yr_53 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$103.47</td>
<td headers="90836 yr_53 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.30</td></tr>
    <tr><td headers="90836 yr_54 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90836 yr_54 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90836 yr_54 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90836 yr_54 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.90</td>
<td headers="90836 yr_54 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.34</td>
<td headers="90836 yr_54 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90836 yr_54 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.32</td>
<td headers="90836 yr_54 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90836 yr_54 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$78.62</td>
<td headers="90836 yr_54 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.41</td>
<td headers="90836 yr_54 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$85.89</td></tr>
    <tr><td headers="90836 yr_55 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90836 yr_55 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90836 yr_55 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90836 yr_55 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.90</td>
<td headers="90836 yr_55 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.62</td>
<td headers="90836 yr_55 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90836 yr_55 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="90836 yr_55 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90836 yr_55 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.11</td>
<td headers="90836 yr_55 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.32</td>
<td headers="90836 yr_55 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$96.26</td></tr>
    <tr><td headers="90836 yr_56 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90836 yr_56 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90836 yr_56 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90836 yr_56 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.99</td>
<td headers="90836 yr_56 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.66</td>
<td headers="90836 yr_56 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90836 yr_56 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.73</td>
<td headers="90836 yr_56 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90836 yr_56 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.87</td>
<td headers="90836 yr_56 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.51</td>
<td headers="90836 yr_56 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.28</td></tr>
    <tr class="gt_row_group_first"><td headers="90837 stub_1_57 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90837</td>
<td headers="90837 stub_1_57 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90837 stub_1_57 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90837 stub_1_57 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90837 stub_1_57 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 stub_1_57 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.94</td>
<td headers="90837 stub_1_57 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="90837 stub_1_57 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.36</td>
<td headers="90837 stub_1_57 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90837 stub_1_57 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$150.88</td>
<td headers="90837 stub_1_57 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$173.52</td>
<td headers="90837 stub_1_57 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$164.84</td></tr>
    <tr><td headers="90837 yr_58 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90837 yr_58 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90837 yr_58 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90837 yr_58 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_58 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.42</td>
<td headers="90837 yr_58 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="90837 yr_58 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90837 yr_58 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90837 yr_58 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$132.89</td>
<td headers="90837 yr_58 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$152.82</td>
<td headers="90837 yr_58 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$145.18</td></tr>
    <tr><td headers="90837 yr_59 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90837 yr_59 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90837 yr_59 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90837 yr_59 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_59 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.42</td>
<td headers="90837 yr_59 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="90837 yr_59 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="90837 yr_59 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90837 yr_59 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$132.89</td>
<td headers="90837 yr_59 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$152.82</td>
<td headers="90837 yr_59 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$145.18</td></tr>
    <tr><td headers="90837 yr_60 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90837 yr_60 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90837 yr_60 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90837 yr_60 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_60 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.94</td>
<td headers="90837 yr_60 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="90837 yr_60 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.36</td>
<td headers="90837 yr_60 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90837 yr_60 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$150.88</td>
<td headers="90837 yr_60 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$173.52</td>
<td headers="90837 yr_60 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$164.84</td></tr>
    <tr><td headers="90837 yr_61 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90837 yr_61 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90837 yr_61 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90837 yr_61 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_61 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.95</td>
<td headers="90837 yr_61 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90837 yr_61 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.34</td>
<td headers="90837 yr_61 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90837 yr_61 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.07</td>
<td headers="90837 yr_61 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$169.13</td>
<td headers="90837 yr_61 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$160.67</td></tr>
    <tr><td headers="90837 yr_62 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90837 yr_62 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90837 yr_62 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90837 yr_62 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_62 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.43</td>
<td headers="90837 yr_62 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90837 yr_62 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.82</td>
<td headers="90837 yr_62 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90837 yr_62 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.45</td>
<td headers="90837 yr_62 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$148.87</td>
<td headers="90837 yr_62 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.42</td></tr>
    <tr><td headers="90837 yr_63 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90837 yr_63 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90837 yr_63 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90837 yr_63 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_63 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.95</td>
<td headers="90837 yr_63 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90837 yr_63 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.34</td>
<td headers="90837 yr_63 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90837 yr_63 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.07</td>
<td headers="90837 yr_63 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$169.13</td>
<td headers="90837 yr_63 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$160.67</td></tr>
    <tr><td headers="90837 yr_64 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90837 yr_64 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90837 yr_64 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90837 yr_64 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_64 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.43</td>
<td headers="90837 yr_64 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90837 yr_64 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.82</td>
<td headers="90837 yr_64 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90837 yr_64 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.45</td>
<td headers="90837 yr_64 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$148.87</td>
<td headers="90837 yr_64 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.42</td></tr>
    <tr><td headers="90837 yr_65 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90837 yr_65 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90837 yr_65 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90837 yr_65 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_65 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.95</td>
<td headers="90837 yr_65 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90837 yr_65 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.34</td>
<td headers="90837 yr_65 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90837 yr_65 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.07</td>
<td headers="90837 yr_65 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$169.13</td>
<td headers="90837 yr_65 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$160.67</td></tr>
    <tr><td headers="90837 yr_66 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90837 yr_66 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90837 yr_66 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90837 yr_66 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_66 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.43</td>
<td headers="90837 yr_66 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90837 yr_66 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.82</td>
<td headers="90837 yr_66 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90837 yr_66 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.45</td>
<td headers="90837 yr_66 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$148.87</td>
<td headers="90837 yr_66 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.42</td></tr>
    <tr><td headers="90837 yr_67 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90837 yr_67 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90837 yr_67 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90837 yr_67 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_67 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.95</td>
<td headers="90837 yr_67 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90837 yr_67 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.34</td>
<td headers="90837 yr_67 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90837 yr_67 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.07</td>
<td headers="90837 yr_67 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$169.13</td>
<td headers="90837 yr_67 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$160.67</td></tr>
    <tr><td headers="90837 yr_68 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90837 yr_68 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90837 yr_68 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90837 yr_68 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="90837 yr_68 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.43</td>
<td headers="90837 yr_68 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90837 yr_68 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.82</td>
<td headers="90837 yr_68 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90837 yr_68 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.45</td>
<td headers="90837 yr_68 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$148.87</td>
<td headers="90837 yr_68 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.42</td></tr>
    <tr><td headers="90837 yr_69 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90837 yr_69 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90837 yr_69 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90837 yr_69 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.47</td>
<td headers="90837 yr_69 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.44</td>
<td headers="90837 yr_69 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90837 yr_69 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.00</td>
<td headers="90837 yr_69 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90837 yr_69 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$133.15</td>
<td headers="90837 yr_69 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$153.12</td>
<td headers="90837 yr_69 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$145.47</td></tr>
    <tr><td headers="90837 yr_70 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90837 yr_70 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90837 yr_70 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90837 yr_70 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.47</td>
<td headers="90837 yr_70 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.01</td>
<td headers="90837 yr_70 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90837 yr_70 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.57</td>
<td headers="90837 yr_70 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90837 yr_70 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$152.12</td>
<td headers="90837 yr_70 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$174.94</td>
<td headers="90837 yr_70 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$166.20</td></tr>
    <tr class="gt_row_group_first"><td headers="90838 stub_1_71 stub_1" rowspan="1" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90838</td>
<td headers="90838 stub_1_71 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90838 stub_1_71 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90838 stub_1_71 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90838 stub_1_71 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90838 stub_1_71 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.82</td>
<td headers="90838 stub_1_71 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90838 stub_1_71 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.42</td>
<td headers="90838 stub_1_71 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90838 stub_1_71 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.35</td>
<td headers="90838 stub_1_71 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$136.11</td>
<td headers="90838 stub_1_71 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.30</td></tr>
    <tr class="gt_row_group_first"><td headers="90839 stub_1_72 stub_1" rowspan="4" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90839</td>
<td headers="90839 stub_1_72 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90839 stub_1_72 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90839 stub_1_72 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90839 stub_1_72 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.13</td>
<td headers="90839 stub_1_72 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.94</td>
<td headers="90839 stub_1_72 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90839 stub_1_72 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.17</td>
<td headers="90839 stub_1_72 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90839 stub_1_72 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.31</td>
<td headers="90839 stub_1_72 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$162.51</td>
<td headers="90839 stub_1_72 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$154.38</td></tr>
    <tr><td headers="90839 yr_73 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90839 yr_73 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90839 yr_73 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90839 yr_73 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.13</td>
<td headers="90839 yr_73 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.94</td>
<td headers="90839 yr_73 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90839 yr_73 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.17</td>
<td headers="90839 yr_73 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90839 yr_73 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.31</td>
<td headers="90839 yr_73 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$162.51</td>
<td headers="90839 yr_73 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$154.38</td></tr>
    <tr><td headers="90839 yr_74 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90839 yr_74 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90839 yr_74 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90839 yr_74 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.13</td>
<td headers="90839 yr_74 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.94</td>
<td headers="90839 yr_74 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90839 yr_74 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.17</td>
<td headers="90839 yr_74 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90839 yr_74 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.31</td>
<td headers="90839 yr_74 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$162.51</td>
<td headers="90839 yr_74 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$154.38</td></tr>
    <tr><td headers="90839 yr_75 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90839 yr_75 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90839 yr_75 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90839 yr_75 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.13</td>
<td headers="90839 yr_75 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.94</td>
<td headers="90839 yr_75 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="90839 yr_75 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.17</td>
<td headers="90839 yr_75 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90839 yr_75 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.31</td>
<td headers="90839 yr_75 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$162.51</td>
<td headers="90839 yr_75 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$154.38</td></tr>
    <tr class="gt_row_group_first"><td headers="90840 stub_1_76 stub_1" rowspan="1" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90840</td>
<td headers="90840 stub_1_76 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90840 stub_1_76 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90840 stub_1_76 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90840 stub_1_76 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.50</td>
<td headers="90840 stub_1_76 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="90840 stub_1_76 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="90840 stub_1_76 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.07</td>
<td headers="90840 stub_1_76 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90840 stub_1_76 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$70.15</td>
<td headers="90840 stub_1_76 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$80.67</td>
<td headers="90840 stub_1_76 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.64</td></tr>
    <tr class="gt_row_group_first"><td headers="90843 stub_1_77 stub_1" rowspan="2" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90843</td>
<td headers="90843 stub_1_77 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90843 stub_1_77 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90843 stub_1_77 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90843 stub_1_77 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 stub_1_77 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 stub_1_77 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 stub_1_77 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 stub_1_77 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 stub_1_77 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 stub_1_77 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 stub_1_77 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="90843 yr_78 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90843 yr_78 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90843 yr_78 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90843 yr_78 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 yr_78 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 yr_78 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 yr_78 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 yr_78 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 yr_78 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 yr_78 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="90843 yr_78 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr class="gt_row_group_first"><td headers="90846 stub_1_79 stub_1" rowspan="10" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90846</td>
<td headers="90846 stub_1_79 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90846 stub_1_79 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90846 stub_1_79 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90846 stub_1_79 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 stub_1_79 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.33</td>
<td headers="90846 stub_1_79 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90846 stub_1_79 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.82</td>
<td headers="90846 stub_1_79 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90846 stub_1_79 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$97.59</td>
<td headers="90846 stub_1_79 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.23</td>
<td headers="90846 stub_1_79 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$106.62</td></tr>
    <tr><td headers="90846 yr_80 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90846 yr_80 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90846 yr_80 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90846 yr_80 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 yr_80 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.35</td>
<td headers="90846 yr_80 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90846 yr_80 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.84</td>
<td headers="90846 yr_80 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90846 yr_80 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.28</td>
<td headers="90846 yr_80 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$113.02</td>
<td headers="90846 yr_80 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$107.37</td></tr>
    <tr><td headers="90846 yr_81 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90846 yr_81 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90846 yr_81 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90846 yr_81 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 yr_81 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.33</td>
<td headers="90846 yr_81 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90846 yr_81 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.82</td>
<td headers="90846 yr_81 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90846 yr_81 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$97.59</td>
<td headers="90846 yr_81 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.23</td>
<td headers="90846 yr_81 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$106.62</td></tr>
    <tr><td headers="90846 yr_82 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90846 yr_82 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90846 yr_82 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90846 yr_82 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 yr_82 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.35</td>
<td headers="90846 yr_82 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90846 yr_82 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.82</td>
<td headers="90846 yr_82 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90846 yr_82 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.56</td>
<td headers="90846 yr_82 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.90</td>
<td headers="90846 yr_82 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.40</td></tr>
    <tr><td headers="90846 yr_83 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90846 yr_83 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90846 yr_83 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90846 yr_83 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 yr_83 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.34</td>
<td headers="90846 yr_83 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90846 yr_83 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.81</td>
<td headers="90846 yr_83 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90846 yr_83 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.22</td>
<td headers="90846 yr_83 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.51</td>
<td headers="90846 yr_83 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.03</td></tr>
    <tr><td headers="90846 yr_84 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90846 yr_84 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90846 yr_84 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90846 yr_84 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 yr_84 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.35</td>
<td headers="90846 yr_84 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90846 yr_84 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.82</td>
<td headers="90846 yr_84 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90846 yr_84 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.56</td>
<td headers="90846 yr_84 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.90</td>
<td headers="90846 yr_84 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.40</td></tr>
    <tr><td headers="90846 yr_85 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90846 yr_85 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90846 yr_85 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90846 yr_85 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 yr_85 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.35</td>
<td headers="90846 yr_85 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90846 yr_85 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.82</td>
<td headers="90846 yr_85 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90846 yr_85 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.56</td>
<td headers="90846 yr_85 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.90</td>
<td headers="90846 yr_85 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.40</td></tr>
    <tr><td headers="90846 yr_86 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90846 yr_86 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90846 yr_86 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90846 yr_86 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 yr_86 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.34</td>
<td headers="90846 yr_86 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90846 yr_86 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.81</td>
<td headers="90846 yr_86 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90846 yr_86 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.22</td>
<td headers="90846 yr_86 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.51</td>
<td headers="90846 yr_86 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.03</td></tr>
    <tr><td headers="90846 yr_87 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90846 yr_87 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90846 yr_87 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90846 yr_87 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.40</td>
<td headers="90846 yr_87 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.35</td>
<td headers="90846 yr_87 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90846 yr_87 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.82</td>
<td headers="90846 yr_87 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90846 yr_87 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.56</td>
<td headers="90846 yr_87 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.90</td>
<td headers="90846 yr_87 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.40</td></tr>
    <tr><td headers="90846 yr_88 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90846 yr_88 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90846 yr_88 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90846 yr_88 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.51</td>
<td headers="90846 yr_88 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.35</td>
<td headers="90846 yr_88 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90846 yr_88 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.93</td>
<td headers="90846 yr_88 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90846 yr_88 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$97.53</td>
<td headers="90846 yr_88 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.16</td>
<td headers="90846 yr_88 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$106.55</td></tr>
    <tr class="gt_row_group_first"><td headers="90847 stub_1_89 stub_1" rowspan="13" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90847</td>
<td headers="90847 stub_1_89 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90847 stub_1_89 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90847 stub_1_89 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90847 stub_1_89 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 stub_1_89 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.35</td>
<td headers="90847 stub_1_89 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90847 stub_1_89 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.94</td>
<td headers="90847 stub_1_89 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90847 stub_1_89 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.74</td>
<td headers="90847 stub_1_89 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$117.00</td>
<td headers="90847 stub_1_89 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$111.15</td></tr>
    <tr><td headers="90847 yr_90 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90847 yr_90 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90847 yr_90 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90847 yr_90 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_90 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.34</td>
<td headers="90847 yr_90 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90847 yr_90 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.93</td>
<td headers="90847 yr_90 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90847 yr_90 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.40</td>
<td headers="90847 yr_90 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$116.61</td>
<td headers="90847 yr_90 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.78</td></tr>
    <tr><td headers="90847 yr_91 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90847 yr_91 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90847 yr_91 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90847 yr_91 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_91 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.35</td>
<td headers="90847 yr_91 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90847 yr_91 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.94</td>
<td headers="90847 yr_91 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90847 yr_91 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.74</td>
<td headers="90847 yr_91 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$117.00</td>
<td headers="90847 yr_91 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$111.15</td></tr>
    <tr><td headers="90847 yr_92 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="90847 yr_92 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90847 yr_92 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90847 yr_92 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_92 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.34</td>
<td headers="90847 yr_92 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="90847 yr_92 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.93</td>
<td headers="90847 yr_92 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="90847 yr_92 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.40</td>
<td headers="90847 yr_92 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$116.61</td>
<td headers="90847 yr_92 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.78</td></tr>
    <tr><td headers="90847 yr_93 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90847 yr_93 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90847 yr_93 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90847 yr_93 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_93 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.36</td>
<td headers="90847 yr_93 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_93 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.93</td>
<td headers="90847 yr_93 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90847 yr_93 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.29</td>
<td headers="90847 yr_93 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.18</td>
<td headers="90847 yr_93 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.47</td></tr>
    <tr><td headers="90847 yr_94 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90847 yr_94 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90847 yr_94 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90847 yr_94 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_94 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.37</td>
<td headers="90847 yr_94 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_94 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.94</td>
<td headers="90847 yr_94 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90847 yr_94 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.63</td>
<td headers="90847 yr_94 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.57</td>
<td headers="90847 yr_94 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.84</td></tr>
    <tr><td headers="90847 yr_95 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90847 yr_95 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90847 yr_95 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90847 yr_95 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_95 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.37</td>
<td headers="90847 yr_95 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_95 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.94</td>
<td headers="90847 yr_95 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90847 yr_95 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.63</td>
<td headers="90847 yr_95 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.57</td>
<td headers="90847 yr_95 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.84</td></tr>
    <tr><td headers="90847 yr_96 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90847 yr_96 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="90847 yr_96 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90847 yr_96 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_96 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.36</td>
<td headers="90847 yr_96 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_96 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.93</td>
<td headers="90847 yr_96 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90847 yr_96 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.29</td>
<td headers="90847 yr_96 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.18</td>
<td headers="90847 yr_96 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.47</td></tr>
    <tr><td headers="90847 yr_97 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90847 yr_97 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="90847 yr_97 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90847 yr_97 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_97 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.37</td>
<td headers="90847 yr_97 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_97 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.94</td>
<td headers="90847 yr_97 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90847 yr_97 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.63</td>
<td headers="90847 yr_97 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.57</td>
<td headers="90847 yr_97 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.84</td></tr>
    <tr><td headers="90847 yr_98 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90847 yr_98 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90847 yr_98 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90847 yr_98 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_98 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.37</td>
<td headers="90847 yr_98 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_98 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.94</td>
<td headers="90847 yr_98 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90847 yr_98 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.63</td>
<td headers="90847 yr_98 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.57</td>
<td headers="90847 yr_98 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.84</td></tr>
    <tr><td headers="90847 yr_99 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90847 yr_99 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90847 yr_99 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90847 yr_99 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.50</td>
<td headers="90847 yr_99 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.36</td>
<td headers="90847 yr_99 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_99 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.93</td>
<td headers="90847 yr_99 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90847 yr_99 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.29</td>
<td headers="90847 yr_99 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.18</td>
<td headers="90847 yr_99 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.47</td></tr>
    <tr><td headers="90847 yr_100 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90847 yr_100 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90847 yr_100 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90847 yr_100 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.62</td>
<td headers="90847 yr_100 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.38</td>
<td headers="90847 yr_100 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_100 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.07</td>
<td headers="90847 yr_100 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90847 yr_100 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$102.19</td>
<td headers="90847 yr_100 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$117.52</td>
<td headers="90847 yr_100 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$111.65</td></tr>
    <tr><td headers="90847 yr_101 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="90847 yr_101 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="90847 yr_101 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="90847 yr_101 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.62</td>
<td headers="90847 yr_101 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.36</td>
<td headers="90847 yr_101 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="90847 yr_101 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.05</td>
<td headers="90847 yr_101 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="90847 yr_101 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.53</td>
<td headers="90847 yr_101 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$116.76</td>
<td headers="90847 yr_101 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.92</td></tr>
    <tr class="gt_row_group_first"><td headers="90853 stub_1_102 stub_1" rowspan="1" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">90853</td>
<td headers="90853 stub_1_102 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="90853 stub_1_102 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="90853 stub_1_102 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="90853 stub_1_102 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.59</td>
<td headers="90853 stub_1_102 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.18</td>
<td headers="90853 stub_1_102 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="90853 stub_1_102 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.79</td>
<td headers="90853 stub_1_102 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="90853 stub_1_102 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$26.77</td>
<td headers="90853 stub_1_102 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$30.79</td>
<td headers="90853 stub_1_102 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$29.25</td></tr>
    <tr class="gt_row_group_first"><td headers="96101 stub_1_103 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96101</td>
<td headers="96101 stub_1_103 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96101 stub_1_103 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96101 stub_1_103 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96101 stub_1_103 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 stub_1_103 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 stub_1_103 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 stub_1_103 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 stub_1_103 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 stub_1_103 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 stub_1_103 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 stub_1_103 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_104 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96101 yr_104 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96101 yr_104 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96101 yr_104 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_104 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_104 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_104 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_104 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_104 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_104 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_104 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_105 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96101 yr_105 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96101 yr_105 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96101 yr_105 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_105 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_105 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_105 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_105 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_105 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_105 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_105 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_106 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96101 yr_106 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96101 yr_106 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96101 yr_106 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_106 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_106 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_106 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_106 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_106 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_106 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_106 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_107 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96101 yr_107 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96101 yr_107 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96101 yr_107 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_107 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_107 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_107 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_107 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_107 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_107 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_107 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_108 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96101 yr_108 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96101 yr_108 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96101 yr_108 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_108 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_108 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_108 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_108 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_108 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_108 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_108 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_109 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96101 yr_109 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96101 yr_109 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96101 yr_109 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_109 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_109 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_109 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_109 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_109 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_109 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_109 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_110 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96101 yr_110 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96101 yr_110 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96101 yr_110 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_110 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_110 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_110 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_110 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_110 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_110 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_110 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_111 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96101 yr_111 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96101 yr_111 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96101 yr_111 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_111 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_111 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_111 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_111 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_111 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_111 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_111 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_112 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96101 yr_112 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96101 yr_112 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96101 yr_112 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_112 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_112 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_112 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_112 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_112 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_112 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_112 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_113 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96101 yr_113 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96101 yr_113 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96101 yr_113 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_113 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_113 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_113 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_113 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_113 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_113 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_113 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_114 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96101 yr_114 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96101 yr_114 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96101 yr_114 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_114 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_114 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_114 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_114 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_114 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_114 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_114 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_115 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96101 yr_115 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96101 yr_115 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96101 yr_115 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_115 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_115 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_115 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_115 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_115 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_115 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_115 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr><td headers="96101 yr_116 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96101 yr_116 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96101 yr_116 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96101 yr_116 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_116 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_116 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_116 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_116 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_116 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_116 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96101 yr_116 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr class="gt_row_group_first"><td headers="96103 stub_1_117 stub_1" rowspan="1" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96103</td>
<td headers="96103 stub_1_117 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96103 stub_1_117 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96103 stub_1_117 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96103 stub_1_117 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96103 stub_1_117 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96103 stub_1_117 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96103 stub_1_117 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96103 stub_1_117 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96103 stub_1_117 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96103 stub_1_117 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td>
<td headers="96103 stub_1_117 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">NA</td></tr>
    <tr class="gt_row_group_first"><td headers="96116 stub_1_118 stub_1" rowspan="3" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96116</td>
<td headers="96116 stub_1_118 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96116 stub_1_118 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96116 stub_1_118 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96116 stub_1_118 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.86</td>
<td headers="96116 stub_1_118 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.81</td>
<td headers="96116 stub_1_118 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="96116 stub_1_118 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.75</td>
<td headers="96116 stub_1_118 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96116 stub_1_118 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$93.19</td>
<td headers="96116 stub_1_118 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$107.17</td>
<td headers="96116 stub_1_118 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.81</td></tr>
    <tr><td headers="96116 yr_119 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96116 yr_119 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96116 yr_119 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96116 yr_119 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.86</td>
<td headers="96116 yr_119 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.43</td>
<td headers="96116 yr_119 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="96116 yr_119 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.37</td>
<td headers="96116 yr_119 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96116 yr_119 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$80.31</td>
<td headers="96116 yr_119 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$92.36</td>
<td headers="96116 yr_119 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$87.74</td></tr>
    <tr><td headers="96116 yr_120 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96116 yr_120 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96116 yr_120 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96116 yr_120 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.86</td>
<td headers="96116 yr_120 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.81</td>
<td headers="96116 yr_120 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.08</td>
<td headers="96116 yr_120 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.75</td>
<td headers="96116 yr_120 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96116 yr_120 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$93.19</td>
<td headers="96116 yr_120 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$107.17</td>
<td headers="96116 yr_120 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$101.81</td></tr>
    <tr class="gt_row_group_first"><td headers="96121 stub_1_121 stub_1" rowspan="1" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96121</td>
<td headers="96121 stub_1_121 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96121 stub_1_121 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96121 stub_1_121 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96121 stub_1_121 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.71</td>
<td headers="96121 stub_1_121 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="96121 stub_1_121 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96121 stub_1_121 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.24</td>
<td headers="96121 stub_1_121 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96121 stub_1_121 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.91</td>
<td headers="96121 stub_1_121 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$87.29</td>
<td headers="96121 stub_1_121 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$82.93</td></tr>
    <tr class="gt_row_group_first"><td headers="96130 stub_1_122 stub_1" rowspan="15" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96130</td>
<td headers="96130 stub_1_122 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96130 stub_1_122 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96130 stub_1_122 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96130 stub_1_122 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 stub_1_122 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.84</td>
<td headers="96130 stub_1_122 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="96130 stub_1_122 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.51</td>
<td headers="96130 stub_1_122 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96130 stub_1_122 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$121.47</td>
<td headers="96130 stub_1_122 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$139.69</td>
<td headers="96130 stub_1_122 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$132.70</td></tr>
    <tr><td headers="96130 yr_123 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96130 yr_123 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96130 yr_123 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96130 yr_123 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_123 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.84</td>
<td headers="96130 yr_123 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="96130 yr_123 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.51</td>
<td headers="96130 yr_123 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96130 yr_123 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$121.47</td>
<td headers="96130 yr_123 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$139.69</td>
<td headers="96130 yr_123 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$132.70</td></tr>
    <tr><td headers="96130 yr_124 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96130 yr_124 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96130 yr_124 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96130 yr_124 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_124 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="96130 yr_124 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="96130 yr_124 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.16</td>
<td headers="96130 yr_124 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96130 yr_124 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.36</td>
<td headers="96130 yr_124 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.76</td>
<td headers="96130 yr_124 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$119.47</td></tr>
    <tr><td headers="96130 yr_125 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96130 yr_125 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96130 yr_125 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96130 yr_125 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_125 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.49</td>
<td headers="96130 yr_125 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="96130 yr_125 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.16</td>
<td headers="96130 yr_125 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96130 yr_125 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.36</td>
<td headers="96130 yr_125 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.76</td>
<td headers="96130 yr_125 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$119.47</td></tr>
    <tr><td headers="96130 yr_126 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96130 yr_126 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96130 yr_126 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96130 yr_126 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_126 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.84</td>
<td headers="96130 yr_126 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="96130 yr_126 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.51</td>
<td headers="96130 yr_126 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96130 yr_126 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$121.47</td>
<td headers="96130 yr_126 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$139.69</td>
<td headers="96130 yr_126 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$132.70</td></tr>
    <tr><td headers="96130 yr_127 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96130 yr_127 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96130 yr_127 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96130 yr_127 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_127 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96130 yr_127 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96130 yr_127 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.21</td>
<td headers="96130 yr_127 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96130 yr_127 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.78</td>
<td headers="96130 yr_127 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.09</td>
<td headers="96130 yr_127 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.84</td></tr>
    <tr><td headers="96130 yr_128 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96130 yr_128 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96130 yr_128 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96130 yr_128 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_128 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.89</td>
<td headers="96130 yr_128 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96130 yr_128 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.55</td>
<td headers="96130 yr_128 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96130 yr_128 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$120.30</td>
<td headers="96130 yr_128 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$138.34</td>
<td headers="96130 yr_128 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$131.43</td></tr>
    <tr><td headers="96130 yr_129 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96130 yr_129 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96130 yr_129 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96130 yr_129 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_129 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.89</td>
<td headers="96130 yr_129 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96130 yr_129 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.55</td>
<td headers="96130 yr_129 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96130 yr_129 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$120.30</td>
<td headers="96130 yr_129 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$138.34</td>
<td headers="96130 yr_129 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$131.43</td></tr>
    <tr><td headers="96130 yr_130 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96130 yr_130 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96130 yr_130 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96130 yr_130 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_130 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96130 yr_130 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96130 yr_130 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.21</td>
<td headers="96130 yr_130 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96130 yr_130 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.78</td>
<td headers="96130 yr_130 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.09</td>
<td headers="96130 yr_130 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.84</td></tr>
    <tr><td headers="96130 yr_131 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96130 yr_131 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96130 yr_131 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96130 yr_131 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_131 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.89</td>
<td headers="96130 yr_131 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96130 yr_131 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.55</td>
<td headers="96130 yr_131 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96130 yr_131 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$120.30</td>
<td headers="96130 yr_131 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$138.34</td>
<td headers="96130 yr_131 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$131.43</td></tr>
    <tr><td headers="96130 yr_132 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96130 yr_132 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96130 yr_132 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96130 yr_132 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_132 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96130 yr_132 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96130 yr_132 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.21</td>
<td headers="96130 yr_132 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96130 yr_132 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.78</td>
<td headers="96130 yr_132 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.09</td>
<td headers="96130 yr_132 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.84</td></tr>
    <tr><td headers="96130 yr_133 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96130 yr_133 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96130 yr_133 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96130 yr_133 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_133 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.89</td>
<td headers="96130 yr_133 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96130 yr_133 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.55</td>
<td headers="96130 yr_133 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96130 yr_133 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$120.30</td>
<td headers="96130 yr_133 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$138.34</td>
<td headers="96130 yr_133 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$131.43</td></tr>
    <tr><td headers="96130 yr_134 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96130 yr_134 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96130 yr_134 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96130 yr_134 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_134 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96130 yr_134 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96130 yr_134 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.21</td>
<td headers="96130 yr_134 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96130 yr_134 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.78</td>
<td headers="96130 yr_134 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.09</td>
<td headers="96130 yr_134 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.84</td></tr>
    <tr><td headers="96130 yr_135 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96130 yr_135 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96130 yr_135 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96130 yr_135 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_135 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.92</td>
<td headers="96130 yr_135 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="96130 yr_135 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.60</td>
<td headers="96130 yr_135 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="96130 yr_135 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$119.84</td>
<td headers="96130 yr_135 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$137.81</td>
<td headers="96130 yr_135 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$130.92</td></tr>
    <tr><td headers="96130 yr_136 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96130 yr_136 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96130 yr_136 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96130 yr_136 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96130 yr_136 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.57</td>
<td headers="96130 yr_136 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="96130 yr_136 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.25</td>
<td headers="96130 yr_136 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="96130 yr_136 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.18</td>
<td headers="96130 yr_136 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$124.41</td>
<td headers="96130 yr_136 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$118.19</td></tr>
    <tr class="gt_row_group_first"><td headers="96131 stub_1_137 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96131</td>
<td headers="96131 stub_1_137 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96131 stub_1_137 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96131 stub_1_137 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96131 stub_1_137 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 stub_1_137 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="96131 stub_1_137 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="96131 stub_1_137 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.61</td>
<td headers="96131 stub_1_137 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96131 stub_1_137 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.32</td>
<td headers="96131 stub_1_137 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$103.87</td>
<td headers="96131 stub_1_137 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.68</td></tr>
    <tr><td headers="96131 yr_138 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96131 yr_138 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96131 yr_138 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96131 yr_138 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_138 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="96131 yr_138 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="96131 yr_138 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.61</td>
<td headers="96131 yr_138 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96131 yr_138 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.32</td>
<td headers="96131 yr_138 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$103.87</td>
<td headers="96131 yr_138 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.68</td></tr>
    <tr><td headers="96131 yr_139 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96131 yr_139 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96131 yr_139 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96131 yr_139 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_139 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.27</td>
<td headers="96131 yr_139 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="96131 yr_139 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.32</td>
<td headers="96131 yr_139 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96131 yr_139 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$80.29</td>
<td headers="96131 yr_139 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$92.33</td>
<td headers="96131 yr_139 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$87.71</td></tr>
    <tr><td headers="96131 yr_140 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96131 yr_140 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96131 yr_140 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96131 yr_140 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_140 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.27</td>
<td headers="96131 yr_140 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="96131 yr_140 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.32</td>
<td headers="96131 yr_140 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96131 yr_140 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$80.29</td>
<td headers="96131 yr_140 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$92.33</td>
<td headers="96131 yr_140 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$87.71</td></tr>
    <tr><td headers="96131 yr_141 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96131 yr_141 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96131 yr_141 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96131 yr_141 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_141 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="96131 yr_141 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="96131 yr_141 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.61</td>
<td headers="96131 yr_141 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96131 yr_141 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.32</td>
<td headers="96131 yr_141 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$103.87</td>
<td headers="96131 yr_141 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.68</td></tr>
    <tr><td headers="96131 yr_142 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96131 yr_142 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96131 yr_142 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96131 yr_142 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_142 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.26</td>
<td headers="96131 yr_142 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96131 yr_142 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.26</td>
<td headers="96131 yr_142 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96131 yr_142 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.59</td>
<td headers="96131 yr_142 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.07</td>
<td headers="96131 yr_142 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$83.67</td></tr>
    <tr><td headers="96131 yr_143 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96131 yr_143 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96131 yr_143 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96131 yr_143 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_143 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="96131 yr_143 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96131 yr_143 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96131 yr_143 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96131 yr_143 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$86.75</td>
<td headers="96131 yr_143 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.76</td>
<td headers="96131 yr_143 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$94.78</td></tr>
    <tr><td headers="96131 yr_144 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96131 yr_144 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96131 yr_144 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96131 yr_144 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_144 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="96131 yr_144 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96131 yr_144 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96131 yr_144 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96131 yr_144 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$86.75</td>
<td headers="96131 yr_144 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.76</td>
<td headers="96131 yr_144 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$94.78</td></tr>
    <tr><td headers="96131 yr_145 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96131 yr_145 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96131 yr_145 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96131 yr_145 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_145 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.26</td>
<td headers="96131 yr_145 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96131 yr_145 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.26</td>
<td headers="96131 yr_145 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96131 yr_145 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.59</td>
<td headers="96131 yr_145 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.07</td>
<td headers="96131 yr_145 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$83.67</td></tr>
    <tr><td headers="96131 yr_146 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96131 yr_146 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96131 yr_146 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96131 yr_146 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_146 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="96131 yr_146 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96131 yr_146 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96131 yr_146 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96131 yr_146 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$86.75</td>
<td headers="96131 yr_146 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.76</td>
<td headers="96131 yr_146 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$94.78</td></tr>
    <tr><td headers="96131 yr_147 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96131 yr_147 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96131 yr_147 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96131 yr_147 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_147 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.26</td>
<td headers="96131 yr_147 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96131 yr_147 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.26</td>
<td headers="96131 yr_147 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96131 yr_147 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.59</td>
<td headers="96131 yr_147 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.07</td>
<td headers="96131 yr_147 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$83.67</td></tr>
    <tr><td headers="96131 yr_148 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96131 yr_148 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96131 yr_148 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96131 yr_148 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_148 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="96131 yr_148 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96131 yr_148 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96131 yr_148 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96131 yr_148 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$86.75</td>
<td headers="96131 yr_148 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.76</td>
<td headers="96131 yr_148 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$94.78</td></tr>
    <tr><td headers="96131 yr_149 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96131 yr_149 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96131 yr_149 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96131 yr_149 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_149 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.26</td>
<td headers="96131 yr_149 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96131 yr_149 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.26</td>
<td headers="96131 yr_149 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96131 yr_149 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$76.59</td>
<td headers="96131 yr_149 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$88.07</td>
<td headers="96131 yr_149 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$83.67</td></tr>
    <tr><td headers="96131 yr_150 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96131 yr_150 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96131 yr_150 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96131 yr_150 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96131 yr_150 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="96131 yr_150 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.05</td>
<td headers="96131 yr_150 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.57</td>
<td headers="96131 yr_150 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="96131 yr_150 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$85.55</td>
<td headers="96131 yr_150 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.38</td>
<td headers="96131 yr_150 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$93.46</td></tr>
    <tr class="gt_row_group_first"><td headers="96132 stub_1_151 stub_1" rowspan="5" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96132</td>
<td headers="96132 stub_1_151 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96132 stub_1_151 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96132 stub_1_151 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96132 stub_1_151 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96132 stub_1_151 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.18</td>
<td headers="96132 stub_1_151 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96132 stub_1_151 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="96132 stub_1_151 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96132 stub_1_151 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$130.13</td>
<td headers="96132 stub_1_151 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$149.65</td>
<td headers="96132 stub_1_151 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$142.16</td></tr>
    <tr><td headers="96132 yr_152 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96132 yr_152 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96132 yr_152 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96132 yr_152 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96132 yr_152 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.18</td>
<td headers="96132 yr_152 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96132 yr_152 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="96132 yr_152 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96132 yr_152 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$130.13</td>
<td headers="96132 yr_152 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$149.65</td>
<td headers="96132 yr_152 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$142.16</td></tr>
    <tr><td headers="96132 yr_153 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96132 yr_153 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96132 yr_153 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96132 yr_153 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96132 yr_153 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.18</td>
<td headers="96132 yr_153 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96132 yr_153 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="96132 yr_153 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96132 yr_153 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$130.13</td>
<td headers="96132 yr_153 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$149.65</td>
<td headers="96132 yr_153 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$142.16</td></tr>
    <tr><td headers="96132 yr_154 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96132 yr_154 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96132 yr_154 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96132 yr_154 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96132 yr_154 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.18</td>
<td headers="96132 yr_154 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="96132 yr_154 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.84</td>
<td headers="96132 yr_154 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96132 yr_154 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$130.13</td>
<td headers="96132 yr_154 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$149.65</td>
<td headers="96132 yr_154 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$142.16</td></tr>
    <tr><td headers="96132 yr_155 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96132 yr_155 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96132 yr_155 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96132 yr_155 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.56</td>
<td headers="96132 yr_155 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.20</td>
<td headers="96132 yr_155 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.09</td>
<td headers="96132 yr_155 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.85</td>
<td headers="96132 yr_155 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="96132 yr_155 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$128.16</td>
<td headers="96132 yr_155 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.38</td>
<td headers="96132 yr_155 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$140.01</td></tr>
    <tr class="gt_row_group_first"><td headers="96133 stub_1_156 stub_1" rowspan="5" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96133</td>
<td headers="96133 stub_1_156 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96133 stub_1_156 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96133 stub_1_156 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96133 stub_1_156 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96133 stub_1_156 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.92</td>
<td headers="96133 stub_1_156 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96133 stub_1_156 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.92</td>
<td headers="96133 stub_1_156 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96133 stub_1_156 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.95</td>
<td headers="96133 stub_1_156 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$113.79</td>
<td headers="96133 stub_1_156 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.10</td></tr>
    <tr><td headers="96133 yr_157 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96133 yr_157 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96133 yr_157 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96133 yr_157 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96133 yr_157 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.92</td>
<td headers="96133 yr_157 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96133 yr_157 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.92</td>
<td headers="96133 yr_157 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96133 yr_157 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.95</td>
<td headers="96133 yr_157 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$113.79</td>
<td headers="96133 yr_157 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.10</td></tr>
    <tr><td headers="96133 yr_158 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96133 yr_158 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96133 yr_158 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96133 yr_158 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96133 yr_158 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.92</td>
<td headers="96133 yr_158 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96133 yr_158 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.92</td>
<td headers="96133 yr_158 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96133 yr_158 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.95</td>
<td headers="96133 yr_158 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$113.79</td>
<td headers="96133 yr_158 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.10</td></tr>
    <tr><td headers="96133 yr_159 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96133 yr_159 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96133 yr_159 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96133 yr_159 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96133 yr_159 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.92</td>
<td headers="96133 yr_159 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96133 yr_159 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.92</td>
<td headers="96133 yr_159 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96133 yr_159 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.95</td>
<td headers="96133 yr_159 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$113.79</td>
<td headers="96133 yr_159 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.10</td></tr>
    <tr><td headers="96133 yr_160 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96133 yr_160 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96133 yr_160 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96133 yr_160 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="96133 yr_160 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.91</td>
<td headers="96133 yr_160 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.05</td>
<td headers="96133 yr_160 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.92</td>
<td headers="96133 yr_160 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="96133 yr_160 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$97.20</td>
<td headers="96133 yr_160 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$111.78</td>
<td headers="96133 yr_160 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$106.19</td></tr>
    <tr class="gt_row_group_first"><td headers="96136 stub_1_161 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96136</td>
<td headers="96136 stub_1_161 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96136 stub_1_161 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96136 stub_1_161 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96136 stub_1_161 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 stub_1_161 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.71</td>
<td headers="96136 stub_1_161 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96136 stub_1_161 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="96136 stub_1_161 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96136 stub_1_161 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$44.99</td>
<td headers="96136 stub_1_161 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$51.74</td>
<td headers="96136 stub_1_161 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$49.15</td></tr>
    <tr><td headers="96136 yr_162 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96136 yr_162 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96136 yr_162 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96136 yr_162 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_162 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.71</td>
<td headers="96136 yr_162 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96136 yr_162 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="96136 yr_162 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96136 yr_162 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$44.99</td>
<td headers="96136 yr_162 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$51.74</td>
<td headers="96136 yr_162 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$49.15</td></tr>
    <tr><td headers="96136 yr_163 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96136 yr_163 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96136 yr_163 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96136 yr_163 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_163 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="96136 yr_163 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96136 yr_163 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.70</td>
<td headers="96136 yr_163 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96136 yr_163 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$24.22</td>
<td headers="96136 yr_163 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$27.86</td>
<td headers="96136 yr_163 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$26.47</td></tr>
    <tr><td headers="96136 yr_164 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96136 yr_164 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96136 yr_164 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96136 yr_164 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_164 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.11</td>
<td headers="96136 yr_164 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96136 yr_164 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.70</td>
<td headers="96136 yr_164 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96136 yr_164 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$24.22</td>
<td headers="96136 yr_164 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$27.86</td>
<td headers="96136 yr_164 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$26.47</td></tr>
    <tr><td headers="96136 yr_165 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96136 yr_165 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96136 yr_165 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96136 yr_165 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_165 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.71</td>
<td headers="96136 yr_165 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.04</td>
<td headers="96136 yr_165 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="96136 yr_165 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96136 yr_165 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$44.99</td>
<td headers="96136 yr_165 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$51.74</td>
<td headers="96136 yr_165 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$49.15</td></tr>
    <tr><td headers="96136 yr_166 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96136 yr_166 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96136 yr_166 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96136 yr_166 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_166 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96136 yr_166 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_166 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.26</td>
<td headers="96136 yr_166 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96136 yr_166 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.70</td>
<td headers="96136 yr_166 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$49.10</td>
<td headers="96136 yr_166 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$46.65</td></tr>
    <tr><td headers="96136 yr_167 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96136 yr_167 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96136 yr_167 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96136 yr_167 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_167 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="96136 yr_167 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_167 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96136 yr_167 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96136 yr_167 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$23.38</td>
<td headers="96136 yr_167 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$26.89</td>
<td headers="96136 yr_167 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$25.55</td></tr>
    <tr><td headers="96136 yr_168 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96136 yr_168 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96136 yr_168 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96136 yr_168 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_168 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96136 yr_168 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_168 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.26</td>
<td headers="96136 yr_168 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96136 yr_168 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.70</td>
<td headers="96136 yr_168 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$49.10</td>
<td headers="96136 yr_168 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$46.65</td></tr>
    <tr><td headers="96136 yr_169 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96136 yr_169 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96136 yr_169 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96136 yr_169 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_169 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="96136 yr_169 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_169 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96136 yr_169 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96136 yr_169 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$23.38</td>
<td headers="96136 yr_169 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$26.89</td>
<td headers="96136 yr_169 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$25.55</td></tr>
    <tr><td headers="96136 yr_170 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96136 yr_170 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96136 yr_170 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96136 yr_170 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_170 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96136 yr_170 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_170 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.26</td>
<td headers="96136 yr_170 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96136 yr_170 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.70</td>
<td headers="96136 yr_170 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$49.10</td>
<td headers="96136 yr_170 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$46.65</td></tr>
    <tr><td headers="96136 yr_171 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96136 yr_171 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96136 yr_171 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96136 yr_171 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_171 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="96136 yr_171 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_171 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96136 yr_171 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96136 yr_171 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$23.38</td>
<td headers="96136 yr_171 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$26.89</td>
<td headers="96136 yr_171 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$25.55</td></tr>
    <tr><td headers="96136 yr_172 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96136 yr_172 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96136 yr_172 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96136 yr_172 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_172 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96136 yr_172 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_172 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.26</td>
<td headers="96136 yr_172 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96136 yr_172 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.70</td>
<td headers="96136 yr_172 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$49.10</td>
<td headers="96136 yr_172 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$46.65</td></tr>
    <tr><td headers="96136 yr_173 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96136 yr_173 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96136 yr_173 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96136 yr_173 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_173 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="96136 yr_173 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_173 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96136 yr_173 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96136 yr_173 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$23.38</td>
<td headers="96136 yr_173 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$26.89</td>
<td headers="96136 yr_173 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$25.55</td></tr>
    <tr><td headers="96136 yr_174 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96136 yr_174 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96136 yr_174 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96136 yr_174 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="96136 yr_174 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.68</td>
<td headers="96136 yr_174 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96136 yr_174 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.25</td>
<td headers="96136 yr_174 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="96136 yr_174 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$41.61</td>
<td headers="96136 yr_174 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$47.85</td>
<td headers="96136 yr_174 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$45.46</td></tr>
    <tr class="gt_row_group_first"><td headers="96137 stub_1_175 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96137</td>
<td headers="96137 stub_1_175 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96137 stub_1_175 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96137 stub_1_175 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96137 stub_1_175 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 stub_1_175 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96137 stub_1_175 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96137 stub_1_175 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.17</td>
<td headers="96137 stub_1_175 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96137 stub_1_175 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$40.49</td>
<td headers="96137 stub_1_175 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$46.56</td>
<td headers="96137 stub_1_175 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$44.23</td></tr>
    <tr><td headers="96137 yr_176 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96137 yr_176 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96137 yr_176 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96137 yr_176 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_176 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96137 yr_176 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96137 yr_176 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.17</td>
<td headers="96137 yr_176 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96137 yr_176 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$40.49</td>
<td headers="96137 yr_176 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$46.56</td>
<td headers="96137 yr_176 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$44.23</td></tr>
    <tr><td headers="96137 yr_177 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96137 yr_177 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96137 yr_177 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96137 yr_177 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_177 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="96137 yr_177 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96137 yr_177 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.54</td>
<td headers="96137 yr_177 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96137 yr_177 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$18.69</td>
<td headers="96137 yr_177 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$21.49</td>
<td headers="96137 yr_177 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$20.42</td></tr>
    <tr><td headers="96137 yr_178 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96137 yr_178 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96137 yr_178 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96137 yr_178 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_178 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="96137 yr_178 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96137 yr_178 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.54</td>
<td headers="96137 yr_178 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96137 yr_178 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$18.69</td>
<td headers="96137 yr_178 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$21.49</td>
<td headers="96137 yr_178 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$20.42</td></tr>
    <tr><td headers="96137 yr_179 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="96137 yr_179 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96137 yr_179 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96137 yr_179 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_179 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96137 yr_179 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.02</td>
<td headers="96137 yr_179 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.17</td>
<td headers="96137 yr_179 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="96137 yr_179 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$40.49</td>
<td headers="96137 yr_179 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$46.56</td>
<td headers="96137 yr_179 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$44.23</td></tr>
    <tr><td headers="96137 yr_180 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96137 yr_180 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96137 yr_180 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96137 yr_180 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_180 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96137 yr_180 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_180 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.16</td>
<td headers="96137 yr_180 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96137 yr_180 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.31</td>
<td headers="96137 yr_180 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$45.21</td>
<td headers="96137 yr_180 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.95</td></tr>
    <tr><td headers="96137 yr_181 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96137 yr_181 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96137 yr_181 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96137 yr_181 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_181 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="96137 yr_181 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_181 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.53</td>
<td headers="96137 yr_181 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96137 yr_181 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$17.96</td>
<td headers="96137 yr_181 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$20.65</td>
<td headers="96137 yr_181 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$19.62</td></tr>
    <tr><td headers="96137 yr_182 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96137 yr_182 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96137 yr_182 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96137 yr_182 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_182 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96137 yr_182 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_182 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.16</td>
<td headers="96137 yr_182 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96137 yr_182 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.31</td>
<td headers="96137 yr_182 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$45.21</td>
<td headers="96137 yr_182 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.95</td></tr>
    <tr><td headers="96137 yr_183 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96137 yr_183 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96137 yr_183 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96137 yr_183 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_183 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="96137 yr_183 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_183 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.53</td>
<td headers="96137 yr_183 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96137 yr_183 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$17.96</td>
<td headers="96137 yr_183 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$20.65</td>
<td headers="96137 yr_183 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$19.62</td></tr>
    <tr><td headers="96137 yr_184 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96137 yr_184 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96137 yr_184 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96137 yr_184 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_184 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96137 yr_184 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_184 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.16</td>
<td headers="96137 yr_184 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96137 yr_184 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.31</td>
<td headers="96137 yr_184 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$45.21</td>
<td headers="96137 yr_184 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.95</td></tr>
    <tr><td headers="96137 yr_185 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96137 yr_185 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96137 yr_185 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96137 yr_185 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_185 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="96137 yr_185 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_185 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.53</td>
<td headers="96137 yr_185 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96137 yr_185 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$17.96</td>
<td headers="96137 yr_185 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$20.65</td>
<td headers="96137 yr_185 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$19.62</td></tr>
    <tr><td headers="96137 yr_186 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96137 yr_186 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96137 yr_186 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96137 yr_186 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_186 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.69</td>
<td headers="96137 yr_186 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_186 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.16</td>
<td headers="96137 yr_186 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96137 yr_186 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.31</td>
<td headers="96137 yr_186 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$45.21</td>
<td headers="96137 yr_186 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.95</td></tr>
    <tr><td headers="96137 yr_187 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96137 yr_187 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96137 yr_187 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="96137 yr_187 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_187 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="96137 yr_187 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_187 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.53</td>
<td headers="96137 yr_187 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96137 yr_187 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$17.96</td>
<td headers="96137 yr_187 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$20.65</td>
<td headers="96137 yr_187 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$19.62</td></tr>
    <tr><td headers="96137 yr_188 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="96137 yr_188 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96137 yr_188 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96137 yr_188 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.46</td>
<td headers="96137 yr_188 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.66</td>
<td headers="96137 yr_188 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96137 yr_188 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.13</td>
<td headers="96137 yr_188 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="96137 yr_188 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$37.61</td>
<td headers="96137 yr_188 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$43.26</td>
<td headers="96137 yr_188 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$41.09</td></tr>
    <tr class="gt_row_group_first"><td headers="96138 stub_1_189 stub_1" rowspan="4" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96138</td>
<td headers="96138 stub_1_189 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96138 stub_1_189 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96138 stub_1_189 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96138 stub_1_189 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="96138 stub_1_189 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.00</td>
<td headers="96138 stub_1_189 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96138 stub_1_189 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.01</td>
<td headers="96138 stub_1_189 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96138 stub_1_189 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.23</td>
<td headers="96138 stub_1_189 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.36</td>
<td headers="96138 stub_1_189 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$37.39</td></tr>
    <tr><td headers="96138 yr_190 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96138 yr_190 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96138 yr_190 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96138 yr_190 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="96138 yr_190 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.00</td>
<td headers="96138 yr_190 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96138 yr_190 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.01</td>
<td headers="96138 yr_190 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96138 yr_190 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.23</td>
<td headers="96138 yr_190 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.36</td>
<td headers="96138 yr_190 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$37.39</td></tr>
    <tr><td headers="96138 yr_191 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96138 yr_191 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96138 yr_191 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96138 yr_191 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="96138 yr_191 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.00</td>
<td headers="96138 yr_191 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96138 yr_191 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.01</td>
<td headers="96138 yr_191 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96138 yr_191 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.23</td>
<td headers="96138 yr_191 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.36</td>
<td headers="96138 yr_191 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$37.39</td></tr>
    <tr><td headers="96138 yr_192 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96138 yr_192 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96138 yr_192 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96138 yr_192 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="96138 yr_192 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.00</td>
<td headers="96138 yr_192 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96138 yr_192 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.01</td>
<td headers="96138 yr_192 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96138 yr_192 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.23</td>
<td headers="96138 yr_192 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.36</td>
<td headers="96138 yr_192 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$37.39</td></tr>
    <tr class="gt_row_group_first"><td headers="96139 stub_1_193 stub_1" rowspan="4" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">96139</td>
<td headers="96139 stub_1_193 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96139 stub_1_193 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="96139 stub_1_193 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96139 stub_1_193 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="96139 stub_1_193 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.03</td>
<td headers="96139 stub_1_193 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96139 stub_1_193 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.04</td>
<td headers="96139 stub_1_193 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96139 stub_1_193 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$35.24</td>
<td headers="96139 stub_1_193 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$40.53</td>
<td headers="96139 stub_1_193 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$38.50</td></tr>
    <tr><td headers="96139 yr_194 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96139 yr_194 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="96139 yr_194 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96139 yr_194 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="96139 yr_194 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.03</td>
<td headers="96139 yr_194 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96139 yr_194 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.04</td>
<td headers="96139 yr_194 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96139 yr_194 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$35.24</td>
<td headers="96139 yr_194 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$40.53</td>
<td headers="96139 yr_194 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$38.50</td></tr>
    <tr><td headers="96139 yr_195 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96139 yr_195 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="96139 yr_195 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96139 yr_195 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="96139 yr_195 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.03</td>
<td headers="96139 yr_195 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96139 yr_195 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.04</td>
<td headers="96139 yr_195 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96139 yr_195 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$35.24</td>
<td headers="96139 yr_195 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$40.53</td>
<td headers="96139 yr_195 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$38.50</td></tr>
    <tr><td headers="96139 yr_196 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="96139 yr_196 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="96139 yr_196 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="96139 yr_196 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="96139 yr_196 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.03</td>
<td headers="96139 yr_196 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.01</td>
<td headers="96139 yr_196 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.04</td>
<td headers="96139 yr_196 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="96139 yr_196 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$35.24</td>
<td headers="96139 yr_196 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$40.53</td>
<td headers="96139 yr_196 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$38.50</td></tr>
    <tr class="gt_row_group_first"><td headers="99024 stub_1_197 stub_1" rowspan="1" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99024</td>
<td headers="99024 stub_1_197 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99024 stub_1_197 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99024 stub_1_197 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99024 stub_1_197 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99024 stub_1_197 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99024 stub_1_197 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99024 stub_1_197 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99024 stub_1_197 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99024 stub_1_197 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="99024 stub_1_197 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="99024 stub_1_197 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td></tr>
    <tr class="gt_row_group_first"><td headers="99080 stub_1_198 stub_1" rowspan="3" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99080</td>
<td headers="99080 stub_1_198 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99080 stub_1_198 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99080 stub_1_198 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99080 stub_1_198 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 stub_1_198 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 stub_1_198 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 stub_1_198 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 stub_1_198 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99080 stub_1_198 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="99080 stub_1_198 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="99080 stub_1_198 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td></tr>
    <tr><td headers="99080 yr_199 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99080 yr_199 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99080 yr_199 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99080 yr_199 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 yr_199 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 yr_199 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 yr_199 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 yr_199 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99080 yr_199 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="99080 yr_199 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="99080 yr_199 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td></tr>
    <tr><td headers="99080 yr_200 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99080 yr_200 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99080 yr_200 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99080 yr_200 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 yr_200 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 yr_200 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 yr_200 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.00</td>
<td headers="99080 yr_200 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99080 yr_200 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="99080 yr_200 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td>
<td headers="99080 yr_200 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$0.00</td></tr>
    <tr class="gt_row_group_first"><td headers="99203 stub_1_201 stub_1" rowspan="4" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99203</td>
<td headers="99203 stub_1_201 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99203 stub_1_201 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99203 stub_1_201 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99203 stub_1_201 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.60</td>
<td headers="99203 stub_1_201 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.52</td>
<td headers="99203 stub_1_201 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="99203 stub_1_201 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.29</td>
<td headers="99203 stub_1_201 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99203 stub_1_201 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$113.85</td>
<td headers="99203 stub_1_201 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$130.93</td>
<td headers="99203 stub_1_201 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$124.39</td></tr>
    <tr><td headers="99203 yr_202 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99203 yr_202 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99203 yr_202 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99203 yr_202 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.60</td>
<td headers="99203 yr_202 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.56</td>
<td headers="99203 yr_202 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="99203 yr_202 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.33</td>
<td headers="99203 yr_202 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99203 yr_202 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.84</td>
<td headers="99203 yr_202 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.77</td>
<td headers="99203 yr_202 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$123.28</td></tr>
    <tr><td headers="99203 yr_203 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99203 yr_203 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99203 yr_203 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99203 yr_203 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.60</td>
<td headers="99203 yr_203 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.56</td>
<td headers="99203 yr_203 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="99203 yr_203 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.33</td>
<td headers="99203 yr_203 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99203 yr_203 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.84</td>
<td headers="99203 yr_203 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.77</td>
<td headers="99203 yr_203 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$123.28</td></tr>
    <tr><td headers="99203 yr_204 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99203 yr_204 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99203 yr_204 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99203 yr_204 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.60</td>
<td headers="99203 yr_204 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.68</td>
<td headers="99203 yr_204 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="99203 yr_204 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.45</td>
<td headers="99203 yr_204 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99203 yr_204 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$83.02</td>
<td headers="99203 yr_204 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.48</td>
<td headers="99203 yr_204 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.70</td></tr>
    <tr class="gt_row_group_first"><td headers="99204 stub_1_205 stub_1" rowspan="13" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99204</td>
<td headers="99204 stub_1_205 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99204 stub_1_205 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99204 stub_1_205 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99204 stub_1_205 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 stub_1_205 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.06</td>
<td headers="99204 stub_1_205 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.24</td>
<td headers="99204 stub_1_205 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.90</td>
<td headers="99204 stub_1_205 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99204 stub_1_205 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$169.57</td>
<td headers="99204 stub_1_205 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$195.01</td>
<td headers="99204 stub_1_205 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$185.26</td></tr>
    <tr><td headers="99204 yr_206 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99204 yr_206 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99204 yr_206 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99204 yr_206 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_206 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.06</td>
<td headers="99204 yr_206 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.24</td>
<td headers="99204 yr_206 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.90</td>
<td headers="99204 yr_206 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99204 yr_206 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$169.57</td>
<td headers="99204 yr_206 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$195.01</td>
<td headers="99204 yr_206 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$185.26</td></tr>
    <tr><td headers="99204 yr_207 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99204 yr_207 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99204 yr_207 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99204 yr_207 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_207 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.11</td>
<td headers="99204 yr_207 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.24</td>
<td headers="99204 yr_207 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.95</td>
<td headers="99204 yr_207 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99204 yr_207 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$136.69</td>
<td headers="99204 yr_207 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$157.20</td>
<td headers="99204 yr_207 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$149.34</td></tr>
    <tr><td headers="99204 yr_208 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99204 yr_208 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99204 yr_208 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99204 yr_208 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_208 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.11</td>
<td headers="99204 yr_208 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.23</td>
<td headers="99204 yr_208 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.94</td>
<td headers="99204 yr_208 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99204 yr_208 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$167.40</td>
<td headers="99204 yr_208 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$192.51</td>
<td headers="99204 yr_208 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$182.89</td></tr>
    <tr><td headers="99204 yr_209 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99204 yr_209 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99204 yr_209 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99204 yr_209 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_209 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.11</td>
<td headers="99204 yr_209 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.23</td>
<td headers="99204 yr_209 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.94</td>
<td headers="99204 yr_209 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99204 yr_209 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$133.52</td>
<td headers="99204 yr_209 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$153.54</td>
<td headers="99204 yr_209 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$145.87</td></tr>
    <tr><td headers="99204 yr_210 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99204 yr_210 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99204 yr_210 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99204 yr_210 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_210 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.11</td>
<td headers="99204 yr_210 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.23</td>
<td headers="99204 yr_210 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.94</td>
<td headers="99204 yr_210 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99204 yr_210 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$167.40</td>
<td headers="99204 yr_210 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$192.51</td>
<td headers="99204 yr_210 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$182.89</td></tr>
    <tr><td headers="99204 yr_211 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99204 yr_211 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99204 yr_211 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99204 yr_211 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_211 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.11</td>
<td headers="99204 yr_211 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.23</td>
<td headers="99204 yr_211 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.94</td>
<td headers="99204 yr_211 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99204 yr_211 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$133.52</td>
<td headers="99204 yr_211 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$153.54</td>
<td headers="99204 yr_211 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$145.87</td></tr>
    <tr><td headers="99204 yr_212 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99204 yr_212 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99204 yr_212 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99204 yr_212 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_212 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.11</td>
<td headers="99204 yr_212 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.23</td>
<td headers="99204 yr_212 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.94</td>
<td headers="99204 yr_212 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99204 yr_212 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$167.40</td>
<td headers="99204 yr_212 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$192.51</td>
<td headers="99204 yr_212 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$182.89</td></tr>
    <tr><td headers="99204 yr_213 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99204 yr_213 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99204 yr_213 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99204 yr_213 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_213 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.11</td>
<td headers="99204 yr_213 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.23</td>
<td headers="99204 yr_213 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.94</td>
<td headers="99204 yr_213 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99204 yr_213 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$133.52</td>
<td headers="99204 yr_213 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$153.54</td>
<td headers="99204 yr_213 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$145.87</td></tr>
    <tr><td headers="99204 yr_214 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99204 yr_214 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99204 yr_214 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99204 yr_214 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_214 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.11</td>
<td headers="99204 yr_214 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.23</td>
<td headers="99204 yr_214 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4.94</td>
<td headers="99204 yr_214 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99204 yr_214 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$167.40</td>
<td headers="99204 yr_214 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$192.51</td>
<td headers="99204 yr_214 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$182.89</td></tr>
    <tr><td headers="99204 yr_215 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99204 yr_215 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99204 yr_215 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99204 yr_215 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_215 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.11</td>
<td headers="99204 yr_215 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.23</td>
<td headers="99204 yr_215 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.94</td>
<td headers="99204 yr_215 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99204 yr_215 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$133.52</td>
<td headers="99204 yr_215 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$153.54</td>
<td headers="99204 yr_215 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$145.87</td></tr>
    <tr><td headers="99204 yr_216 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99204 yr_216 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99204 yr_216 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99204 yr_216 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_216 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.18</td>
<td headers="99204 yr_216 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.24</td>
<td headers="99204 yr_216 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.02</td>
<td headers="99204 yr_216 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99204 yr_216 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$167.10</td>
<td headers="99204 yr_216 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$192.17</td>
<td headers="99204 yr_216 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$182.56</td></tr>
    <tr><td headers="99204 yr_217 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99204 yr_217 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99204 yr_217 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99204 yr_217 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.60</td>
<td headers="99204 yr_217 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.13</td>
<td headers="99204 yr_217 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.24</td>
<td headers="99204 yr_217 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.97</td>
<td headers="99204 yr_217 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99204 yr_217 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$132.15</td>
<td headers="99204 yr_217 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$151.97</td>
<td headers="99204 yr_217 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$144.38</td></tr>
    <tr class="gt_row_group_first"><td headers="99205 stub_1_218 stub_1" rowspan="4" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99205</td>
<td headers="99205 stub_1_218 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99205 stub_1_218 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99205 stub_1_218 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99205 stub_1_218 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.50</td>
<td headers="99205 stub_1_218 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.66</td>
<td headers="99205 stub_1_218 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.32</td>
<td headers="99205 stub_1_218 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">6.48</td>
<td headers="99205 stub_1_218 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99205 stub_1_218 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$224.25</td>
<td headers="99205 stub_1_218 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$257.89</td>
<td headers="99205 stub_1_218 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$244.99</td></tr>
    <tr><td headers="99205 yr_219 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99205 yr_219 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99205 yr_219 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99205 yr_219 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.50</td>
<td headers="99205 yr_219 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.66</td>
<td headers="99205 yr_219 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.32</td>
<td headers="99205 yr_219 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">6.48</td>
<td headers="99205 yr_219 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99205 yr_219 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$224.25</td>
<td headers="99205 yr_219 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$257.89</td>
<td headers="99205 yr_219 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$244.99</td></tr>
    <tr><td headers="99205 yr_220 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99205 yr_220 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99205 yr_220 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99205 yr_220 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.50</td>
<td headers="99205 yr_220 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.71</td>
<td headers="99205 yr_220 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.31</td>
<td headers="99205 yr_220 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">6.52</td>
<td headers="99205 yr_220 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99205 yr_220 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$220.94</td>
<td headers="99205 yr_220 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$254.09</td>
<td headers="99205 yr_220 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$241.38</td></tr>
    <tr><td headers="99205 yr_221 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99205 yr_221 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99205 yr_221 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99205 yr_221 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.50</td>
<td headers="99205 yr_221 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.71</td>
<td headers="99205 yr_221 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.31</td>
<td headers="99205 yr_221 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">6.52</td>
<td headers="99205 yr_221 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99205 yr_221 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$220.94</td>
<td headers="99205 yr_221 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$254.09</td>
<td headers="99205 yr_221 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$241.38</td></tr>
    <tr class="gt_row_group_first"><td headers="99212 stub_1_222 stub_1" rowspan="4" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99212</td>
<td headers="99212 stub_1_222 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99212 stub_1_222 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99212 stub_1_222 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99212 stub_1_222 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.70</td>
<td headers="99212 stub_1_222 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.89</td>
<td headers="99212 stub_1_222 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="99212 stub_1_222 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.66</td>
<td headers="99212 stub_1_222 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99212 stub_1_222 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$57.45</td>
<td headers="99212 stub_1_222 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$66.06</td>
<td headers="99212 stub_1_222 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$62.76</td></tr>
    <tr><td headers="99212 yr_223 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99212 yr_223 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99212 yr_223 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99212 yr_223 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.70</td>
<td headers="99212 yr_223 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.29</td>
<td headers="99212 yr_223 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="99212 yr_223 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.06</td>
<td headers="99212 yr_223 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99212 yr_223 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$36.68</td>
<td headers="99212 yr_223 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$42.18</td>
<td headers="99212 yr_223 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$40.08</td></tr>
    <tr><td headers="99212 yr_224 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99212 yr_224 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99212 yr_224 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99212 yr_224 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.70</td>
<td headers="99212 yr_224 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.89</td>
<td headers="99212 yr_224 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.07</td>
<td headers="99212 yr_224 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.66</td>
<td headers="99212 yr_224 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99212 yr_224 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$57.45</td>
<td headers="99212 yr_224 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$66.06</td>
<td headers="99212 yr_224 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$62.76</td></tr>
    <tr><td headers="99212 yr_225 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99212 yr_225 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99212 yr_225 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99212 yr_225 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.70</td>
<td headers="99212 yr_225 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.92</td>
<td headers="99212 yr_225 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.06</td>
<td headers="99212 yr_225 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.68</td>
<td headers="99212 yr_225 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99212 yr_225 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$56.93</td>
<td headers="99212 yr_225 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$65.47</td>
<td headers="99212 yr_225 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$62.20</td></tr>
    <tr class="gt_row_group_first"><td headers="99213 stub_1_226 stub_1" rowspan="12" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99213</td>
<td headers="99213 stub_1_226 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99213 stub_1_226 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99213 stub_1_226 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99213 stub_1_226 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 stub_1_226 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.26</td>
<td headers="99213 stub_1_226 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 stub_1_226 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.66</td>
<td headers="99213 stub_1_226 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99213 stub_1_226 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$92.05</td>
<td headers="99213 stub_1_226 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$105.86</td>
<td headers="99213 stub_1_226 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$100.57</td></tr>
    <tr><td headers="99213 yr_227 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99213 yr_227 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99213 yr_227 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99213 yr_227 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_227 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="99213 yr_227 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_227 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99213 yr_227 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99213 yr_227 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$67.48</td>
<td headers="99213 yr_227 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$77.60</td>
<td headers="99213 yr_227 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$73.72</td></tr>
    <tr><td headers="99213 yr_228 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99213 yr_228 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99213 yr_228 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99213 yr_228 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_228 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.26</td>
<td headers="99213 yr_228 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_228 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.66</td>
<td headers="99213 yr_228 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99213 yr_228 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$92.05</td>
<td headers="99213 yr_228 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$105.86</td>
<td headers="99213 yr_228 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$100.57</td></tr>
    <tr><td headers="99213 yr_229 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99213 yr_229 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99213 yr_229 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99213 yr_229 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_229 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="99213 yr_229 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_229 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99213 yr_229 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99213 yr_229 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$67.48</td>
<td headers="99213 yr_229 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$77.60</td>
<td headers="99213 yr_229 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$73.72</td></tr>
    <tr><td headers="99213 yr_230 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99213 yr_230 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99213 yr_230 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99213 yr_230 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_230 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.28</td>
<td headers="99213 yr_230 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_230 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.68</td>
<td headers="99213 yr_230 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99213 yr_230 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.82</td>
<td headers="99213 yr_230 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.44</td>
<td headers="99213 yr_230 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.22</td></tr>
    <tr><td headers="99213 yr_231 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99213 yr_231 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99213 yr_231 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99213 yr_231 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_231 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="99213 yr_231 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_231 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99213 yr_231 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99213 yr_231 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$66.08</td>
<td headers="99213 yr_231 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.99</td>
<td headers="99213 yr_231 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$72.19</td></tr>
    <tr><td headers="99213 yr_232 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99213 yr_232 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99213 yr_232 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99213 yr_232 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_232 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.28</td>
<td headers="99213 yr_232 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_232 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.68</td>
<td headers="99213 yr_232 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99213 yr_232 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.82</td>
<td headers="99213 yr_232 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.44</td>
<td headers="99213 yr_232 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.22</td></tr>
    <tr><td headers="99213 yr_233 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99213 yr_233 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99213 yr_233 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99213 yr_233 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_233 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.28</td>
<td headers="99213 yr_233 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_233 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.68</td>
<td headers="99213 yr_233 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99213 yr_233 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.82</td>
<td headers="99213 yr_233 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.44</td>
<td headers="99213 yr_233 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.22</td></tr>
    <tr><td headers="99213 yr_234 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99213 yr_234 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99213 yr_234 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99213 yr_234 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_234 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.28</td>
<td headers="99213 yr_234 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_234 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.68</td>
<td headers="99213 yr_234 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99213 yr_234 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.82</td>
<td headers="99213 yr_234 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.44</td>
<td headers="99213 yr_234 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.22</td></tr>
    <tr><td headers="99213 yr_235 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99213 yr_235 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99213 yr_235 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99213 yr_235 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_235 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.55</td>
<td headers="99213 yr_235 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_235 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99213 yr_235 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99213 yr_235 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$66.08</td>
<td headers="99213 yr_235 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.99</td>
<td headers="99213 yr_235 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$72.19</td></tr>
    <tr><td headers="99213 yr_236 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99213 yr_236 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99213 yr_236 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99213 yr_236 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_236 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.56</td>
<td headers="99213 yr_236 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_236 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.96</td>
<td headers="99213 yr_236 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99213 yr_236 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$65.24</td>
<td headers="99213 yr_236 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$75.03</td>
<td headers="99213 yr_236 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$71.28</td></tr>
    <tr><td headers="99213 yr_237 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99213 yr_237 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99213 yr_237 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99213 yr_237 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.30</td>
<td headers="99213 yr_237 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.33</td>
<td headers="99213 yr_237 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.10</td>
<td headers="99213 yr_237 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.73</td>
<td headers="99213 yr_237 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99213 yr_237 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$90.87</td>
<td headers="99213 yr_237 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.51</td>
<td headers="99213 yr_237 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.28</td></tr>
    <tr class="gt_row_group_first"><td headers="99214 stub_1_238 stub_1" rowspan="13" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99214</td>
<td headers="99214 stub_1_238 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99214 stub_1_238 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99214 stub_1_238 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99214 stub_1_238 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 stub_1_238 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.71</td>
<td headers="99214 stub_1_238 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99214 stub_1_238 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.75</td>
<td headers="99214 stub_1_238 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99214 stub_1_238 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.77</td>
<td headers="99214 stub_1_238 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$149.24</td>
<td headers="99214 stub_1_238 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.78</td></tr>
    <tr><td headers="99214 yr_239 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99214 yr_239 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99214 yr_239 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99214 yr_239 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_239 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.71</td>
<td headers="99214 yr_239 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99214 yr_239 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.75</td>
<td headers="99214 yr_239 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99214 yr_239 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$129.77</td>
<td headers="99214 yr_239 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$149.24</td>
<td headers="99214 yr_239 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$141.78</td></tr>
    <tr><td headers="99214 yr_240 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99214 yr_240 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99214 yr_240 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99214 yr_240 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_240 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.82</td>
<td headers="99214 yr_240 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99214 yr_240 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.86</td>
<td headers="99214 yr_240 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99214 yr_240 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$98.97</td>
<td headers="99214 yr_240 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$113.82</td>
<td headers="99214 yr_240 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.13</td></tr>
    <tr><td headers="99214 yr_241 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99214 yr_241 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99214 yr_241 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99214 yr_241 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_241 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.73</td>
<td headers="99214 yr_241 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_241 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.79</td>
<td headers="99214 yr_241 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99214 yr_241 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$128.43</td>
<td headers="99214 yr_241 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.70</td>
<td headers="99214 yr_241 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$140.31</td></tr>
    <tr><td headers="99214 yr_242 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99214 yr_242 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99214 yr_242 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99214 yr_242 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_242 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.82</td>
<td headers="99214 yr_242 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_242 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.88</td>
<td headers="99214 yr_242 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99214 yr_242 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$97.60</td>
<td headers="99214 yr_242 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.23</td>
<td headers="99214 yr_242 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$106.62</td></tr>
    <tr><td headers="99214 yr_243 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99214 yr_243 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99214 yr_243 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99214 yr_243 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_243 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.73</td>
<td headers="99214 yr_243 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_243 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.79</td>
<td headers="99214 yr_243 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99214 yr_243 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$128.43</td>
<td headers="99214 yr_243 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.70</td>
<td headers="99214 yr_243 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$140.31</td></tr>
    <tr><td headers="99214 yr_244 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99214 yr_244 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99214 yr_244 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99214 yr_244 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_244 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.82</td>
<td headers="99214 yr_244 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_244 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.88</td>
<td headers="99214 yr_244 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99214 yr_244 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$97.60</td>
<td headers="99214 yr_244 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.23</td>
<td headers="99214 yr_244 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$106.62</td></tr>
    <tr><td headers="99214 yr_245 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99214 yr_245 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99214 yr_245 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99214 yr_245 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_245 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.73</td>
<td headers="99214 yr_245 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_245 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.79</td>
<td headers="99214 yr_245 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99214 yr_245 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$128.43</td>
<td headers="99214 yr_245 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.70</td>
<td headers="99214 yr_245 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$140.31</td></tr>
    <tr><td headers="99214 yr_246 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99214 yr_246 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99214 yr_246 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99214 yr_246 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_246 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.82</td>
<td headers="99214 yr_246 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_246 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.88</td>
<td headers="99214 yr_246 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99214 yr_246 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$97.60</td>
<td headers="99214 yr_246 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.23</td>
<td headers="99214 yr_246 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$106.62</td></tr>
    <tr><td headers="99214 yr_247 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99214 yr_247 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99214 yr_247 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99214 yr_247 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_247 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.73</td>
<td headers="99214 yr_247 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_247 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.79</td>
<td headers="99214 yr_247 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99214 yr_247 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$128.43</td>
<td headers="99214 yr_247 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.70</td>
<td headers="99214 yr_247 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$140.31</td></tr>
    <tr><td headers="99214 yr_248 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99214 yr_248 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99214 yr_248 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99214 yr_248 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_248 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.82</td>
<td headers="99214 yr_248 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_248 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.88</td>
<td headers="99214 yr_248 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99214 yr_248 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$97.60</td>
<td headers="99214 yr_248 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$112.23</td>
<td headers="99214 yr_248 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$106.62</td></tr>
    <tr><td headers="99214 yr_249 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99214 yr_249 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99214 yr_249 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99214 yr_249 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_249 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.79</td>
<td headers="99214 yr_249 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_249 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.85</td>
<td headers="99214 yr_249 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99214 yr_249 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$128.16</td>
<td headers="99214 yr_249 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$147.38</td>
<td headers="99214 yr_249 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$140.01</td></tr>
    <tr><td headers="99214 yr_250 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99214 yr_250 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99214 yr_250 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99214 yr_250 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.92</td>
<td headers="99214 yr_250 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.83</td>
<td headers="99214 yr_250 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.14</td>
<td headers="99214 yr_250 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.89</td>
<td headers="99214 yr_250 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99214 yr_250 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$96.20</td>
<td headers="99214 yr_250 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.63</td>
<td headers="99214 yr_250 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$105.10</td></tr>
    <tr class="gt_row_group_first"><td headers="99215 stub_1_251 stub_1" rowspan="4" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99215</td>
<td headers="99215 stub_1_251 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99215 stub_1_251 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99215 stub_1_251 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99215 stub_1_251 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.80</td>
<td headers="99215 stub_1_251 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.28</td>
<td headers="99215 stub_1_251 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.21</td>
<td headers="99215 stub_1_251 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.29</td>
<td headers="99215 stub_1_251 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99215 stub_1_251 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$183.07</td>
<td headers="99215 stub_1_251 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$210.53</td>
<td headers="99215 stub_1_251 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$200.00</td></tr>
    <tr><td headers="99215 yr_252 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99215 yr_252 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99215 yr_252 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99215 yr_252 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.80</td>
<td headers="99215 yr_252 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.31</td>
<td headers="99215 yr_252 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.20</td>
<td headers="99215 yr_252 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.31</td>
<td headers="99215 yr_252 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99215 yr_252 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$179.94</td>
<td headers="99215 yr_252 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$206.93</td>
<td headers="99215 yr_252 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$196.59</td></tr>
    <tr><td headers="99215 yr_253 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99215 yr_253 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99215 yr_253 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99215 yr_253 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.80</td>
<td headers="99215 yr_253 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.31</td>
<td headers="99215 yr_253 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.20</td>
<td headers="99215 yr_253 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.31</td>
<td headers="99215 yr_253 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99215 yr_253 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$179.94</td>
<td headers="99215 yr_253 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$206.93</td>
<td headers="99215 yr_253 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$196.59</td></tr>
    <tr><td headers="99215 yr_254 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99215 yr_254 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99215 yr_254 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99215 yr_254 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.80</td>
<td headers="99215 yr_254 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.31</td>
<td headers="99215 yr_254 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.20</td>
<td headers="99215 yr_254 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">5.31</td>
<td headers="99215 yr_254 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99215 yr_254 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$179.94</td>
<td headers="99215 yr_254 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$206.93</td>
<td headers="99215 yr_254 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$196.59</td></tr>
    <tr class="gt_row_group_first"><td headers="99404 stub_1_255 stub_1" rowspan="14" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99404</td>
<td headers="99404 stub_1_255 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99404 stub_1_255 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99404 stub_1_255 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99404 stub_1_255 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 stub_1_255 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.75</td>
<td headers="99404 stub_1_255 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="99404 stub_1_255 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.87</td>
<td headers="99404 stub_1_255 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99404 stub_1_255 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.32</td>
<td headers="99404 stub_1_255 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.22</td>
<td headers="99404 stub_1_255 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.51</td></tr>
    <tr><td headers="99404 yr_256 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99404 yr_256 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99404 yr_256 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99404 yr_256 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_256 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.19</td>
<td headers="99404 yr_256 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="99404 yr_256 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="99404 yr_256 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99404 yr_256 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.55</td>
<td headers="99404 yr_256 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$131.73</td>
<td headers="99404 yr_256 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.14</td></tr>
    <tr><td headers="99404 yr_257 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99404 yr_257 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99404 yr_257 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99404 yr_257 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_257 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.75</td>
<td headers="99404 yr_257 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="99404 yr_257 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.87</td>
<td headers="99404 yr_257 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99404 yr_257 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$99.32</td>
<td headers="99404 yr_257 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.22</td>
<td headers="99404 yr_257 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.51</td></tr>
    <tr><td headers="99404 yr_258 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99404 yr_258 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99404 yr_258 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99404 yr_258 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_258 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.19</td>
<td headers="99404 yr_258 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.17</td>
<td headers="99404 yr_258 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.31</td>
<td headers="99404 yr_258 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99404 yr_258 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$114.55</td>
<td headers="99404 yr_258 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$131.73</td>
<td headers="99404 yr_258 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.14</td></tr>
    <tr><td headers="99404 yr_259 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99404 yr_259 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99404 yr_259 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99404 yr_259 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_259 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.19</td>
<td headers="99404 yr_259 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_259 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.26</td>
<td headers="99404 yr_259 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99404 yr_259 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.47</td>
<td headers="99404 yr_259 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$127.04</td>
<td headers="99404 yr_259 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$120.69</td></tr>
    <tr><td headers="99404 yr_260 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99404 yr_260 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99404 yr_260 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99404 yr_260 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_260 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.76</td>
<td headers="99404 yr_260 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_260 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.83</td>
<td headers="99404 yr_260 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99404 yr_260 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.90</td>
<td headers="99404 yr_260 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.29</td>
<td headers="99404 yr_260 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.77</td></tr>
    <tr><td headers="99404 yr_261 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99404 yr_261 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99404 yr_261 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99404 yr_261 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_261 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.76</td>
<td headers="99404 yr_261 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_261 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.83</td>
<td headers="99404 yr_261 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99404 yr_261 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.90</td>
<td headers="99404 yr_261 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.29</td>
<td headers="99404 yr_261 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.77</td></tr>
    <tr><td headers="99404 yr_262 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99404 yr_262 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2</td>
<td headers="99404 yr_262 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99404 yr_262 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_262 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.19</td>
<td headers="99404 yr_262 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_262 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.26</td>
<td headers="99404 yr_262 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99404 yr_262 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.47</td>
<td headers="99404 yr_262 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$127.04</td>
<td headers="99404 yr_262 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$120.69</td></tr>
    <tr><td headers="99404 yr_263 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99404 yr_263 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99404 yr_263 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99404 yr_263 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_263 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.19</td>
<td headers="99404 yr_263 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_263 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.26</td>
<td headers="99404 yr_263 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99404 yr_263 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.47</td>
<td headers="99404 yr_263 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$127.04</td>
<td headers="99404 yr_263 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$120.69</td></tr>
    <tr><td headers="99404 yr_264 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99404 yr_264 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99404 yr_264 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99404 yr_264 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_264 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.76</td>
<td headers="99404 yr_264 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_264 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.83</td>
<td headers="99404 yr_264 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99404 yr_264 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.90</td>
<td headers="99404 yr_264 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.29</td>
<td headers="99404 yr_264 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.77</td></tr>
    <tr><td headers="99404 yr_265 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99404 yr_265 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99404 yr_265 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99404 yr_265 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_265 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.19</td>
<td headers="99404 yr_265 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_265 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.26</td>
<td headers="99404 yr_265 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99404 yr_265 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.47</td>
<td headers="99404 yr_265 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$127.04</td>
<td headers="99404 yr_265 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$120.69</td></tr>
    <tr><td headers="99404 yr_266 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2023</td>
<td headers="99404 yr_266 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99404 yr_266 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99404 yr_266 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_266 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.76</td>
<td headers="99404 yr_266 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_266 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.83</td>
<td headers="99404 yr_266 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.89</td>
<td headers="99404 yr_266 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$95.90</td>
<td headers="99404 yr_266 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$110.29</td>
<td headers="99404 yr_266 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$104.77</td></tr>
    <tr><td headers="99404 yr_267 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99404 yr_267 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99404 yr_267 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99404 yr_267 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_267 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.21</td>
<td headers="99404 yr_267 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_267 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3.28</td>
<td headers="99404 yr_267 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99404 yr_267 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$109.18</td>
<td headers="99404 yr_267 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$125.56</td>
<td headers="99404 yr_267 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$119.28</td></tr>
    <tr><td headers="99404 yr_268 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2024</td>
<td headers="99404 yr_268 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1</td>
<td headers="99404 yr_268 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99404 yr_268 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.95</td>
<td headers="99404 yr_268 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.76</td>
<td headers="99404 yr_268 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.12</td>
<td headers="99404 yr_268 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2.83</td>
<td headers="99404 yr_268 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$33.29</td>
<td headers="99404 yr_268 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$94.20</td>
<td headers="99404 yr_268 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$108.33</td>
<td headers="99404 yr_268 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$102.92</td></tr>
    <tr class="gt_row_group_first"><td headers="99441 stub_1_269 stub_1" rowspan="2" class="gt_row gt_left gt_stub_row_group" style="font-family: 'Fira Code'; font-size: 16px; text-align: center; font-weight: bold;">99441</td>
<td headers="99441 stub_1_269 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99441 stub_1_269 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">3</td>
<td headers="99441 stub_1_269 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Facility</td>
<td headers="99441 stub_1_269 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.70</td>
<td headers="99441 stub_1_269 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.29</td>
<td headers="99441 stub_1_269 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.05</td>
<td headers="99441 stub_1_269 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.04</td>
<td headers="99441 stub_1_269 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99441 stub_1_269 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$35.99</td>
<td headers="99441 stub_1_269 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$41.39</td>
<td headers="99441 stub_1_269 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$39.32</td></tr>
    <tr><td headers="99441 yr_270 yr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">2022</td>
<td headers="99441 yr_270 qtr" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">4</td>
<td headers="99441 yr_270 pos" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">Non-Facility</td>
<td headers="99441 yr_270 work_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.70</td>
<td headers="99441 yr_270 pe_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.89</td>
<td headers="99441 yr_270 mp_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">0.05</td>
<td headers="99441 yr_270 tot_rvu" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">1.64</td>
<td headers="99441 yr_270 cf" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$34.61</td>
<td headers="99441 yr_270 allow_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$56.75</td>
<td headers="99441 yr_270 nonpar_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$65.27</td>
<td headers="99441 yr_270 lim_unadj" class="gt_row gt_right" style="font-family: 'Fira Code'; text-align: right;">$62.00</td></tr>
  </tbody>
  
  
</table>
</div>
```

:::
:::

::: {.cell layout-align="center"}

```{.r .cell-code}
rvu_join <- pprrvu |> 
  reframe(
    dos,
    pos,
    hcpcs, 
    wrvu = work_rvu, 
    prvu = pe_rvu, 
    mrvu = mp_rvu, 
    trvu = rvu_total,
    cf = conv_factor,
    all_un = trvu * cf,
    npar_un = all_un * 1.15,
    lim_un = npar_un * 0.95
    )

rvu_claims <- claims |> 
  select(
    dos, 
    id, 
    enc, 
    ren,
    ins_class,
    ins_prim, 
    ord,
    hcpcs,
    units,
    pos = pos_type,
    charges, 
    allowed, 
    payments, 
    adjustments
    ) |> 
  left_join(
    rvu_join, 
    by = join_by(dos, hcpcs, pos)) |> 
  filter(
    ins_class != "SELF", 
    !is.na(hcpcs),
    !is.na(wrvu),
    # !(wrvu == 0.0 & prvu == 0.0 & mrvu == 0.0),
    charges > 0) |> 
  arrange(dos, id, enc, ord) |> 
  mutate(across(c(all_un, npar_un, lim_un), ~ . * units))

rvu_claims |>
  head(n = 10) |>
  gt() |> 
  opt_all_caps() |> 
  opt_table_font(font = google_font(name = "Fira Code")) |> 
  tab_options(table.width = pct(100),
              quarto.disable_processing = TRUE)
```

::: {.cell-output-display}

```{=html}
<div id="lbzbrqxoyr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Fira+Code:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#lbzbrqxoyr table {
  font-family: 'Fira Code', system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lbzbrqxoyr thead, #lbzbrqxoyr tbody, #lbzbrqxoyr tfoot, #lbzbrqxoyr tr, #lbzbrqxoyr td, #lbzbrqxoyr th {
  border-style: none;
}

#lbzbrqxoyr p {
  margin: 0;
  padding: 0;
}

#lbzbrqxoyr .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 100%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lbzbrqxoyr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lbzbrqxoyr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lbzbrqxoyr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lbzbrqxoyr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lbzbrqxoyr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbzbrqxoyr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lbzbrqxoyr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lbzbrqxoyr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lbzbrqxoyr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lbzbrqxoyr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lbzbrqxoyr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lbzbrqxoyr .gt_spanner_row {
  border-bottom-style: hidden;
}

#lbzbrqxoyr .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#lbzbrqxoyr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lbzbrqxoyr .gt_from_md > :first-child {
  margin-top: 0;
}

#lbzbrqxoyr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lbzbrqxoyr .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lbzbrqxoyr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#lbzbrqxoyr .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lbzbrqxoyr .gt_row_group_first td {
  border-top-width: 2px;
}

#lbzbrqxoyr .gt_row_group_first th {
  border-top-width: 2px;
}

#lbzbrqxoyr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbzbrqxoyr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lbzbrqxoyr .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lbzbrqxoyr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbzbrqxoyr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbzbrqxoyr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lbzbrqxoyr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lbzbrqxoyr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lbzbrqxoyr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbzbrqxoyr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lbzbrqxoyr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbzbrqxoyr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lbzbrqxoyr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbzbrqxoyr .gt_left {
  text-align: left;
}

#lbzbrqxoyr .gt_center {
  text-align: center;
}

#lbzbrqxoyr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lbzbrqxoyr .gt_font_normal {
  font-weight: normal;
}

#lbzbrqxoyr .gt_font_bold {
  font-weight: bold;
}

#lbzbrqxoyr .gt_font_italic {
  font-style: italic;
}

#lbzbrqxoyr .gt_super {
  font-size: 65%;
}

#lbzbrqxoyr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lbzbrqxoyr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lbzbrqxoyr .gt_indent_1 {
  text-indent: 5px;
}

#lbzbrqxoyr .gt_indent_2 {
  text-indent: 10px;
}

#lbzbrqxoyr .gt_indent_3 {
  text-indent: 15px;
}

#lbzbrqxoyr .gt_indent_4 {
  text-indent: 20px;
}

#lbzbrqxoyr .gt_indent_5 {
  text-indent: 25px;
}

#lbzbrqxoyr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#lbzbrqxoyr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="true" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="dos">dos</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="id">id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="enc">enc</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="ren">ren</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="ins_class">ins_class</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="ins_prim">ins_prim</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ord">ord</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="hcpcs">hcpcs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="units">units</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="pos">pos</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="charges">charges</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="allowed">allowed</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="payments">payments</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="adjustments">adjustments</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="wrvu">wrvu</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="prvu">prvu</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mrvu">mrvu</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="trvu">trvu</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="cf">cf</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="all_un">all_un</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="npar_un">npar_un</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="lim_un">lim_un</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0017</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV02</td>
<td headers="ins_class" class="gt_row gt_left">COMMERCIAL</td>
<td headers="ins_prim" class="gt_row gt_left">BCBS</td>
<td headers="ord" class="gt_row gt_right">1</td>
<td headers="hcpcs" class="gt_row gt_right">90837</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Facility</td>
<td headers="charges" class="gt_row gt_right">200</td>
<td headers="allowed" class="gt_row gt_right">149.00</td>
<td headers="payments" class="gt_row gt_right">131.82</td>
<td headers="adjustments" class="gt_row gt_right">68.18</td>
<td headers="wrvu" class="gt_row gt_right">3.31</td>
<td headers="prvu" class="gt_row gt_right">0.42</td>
<td headers="mrvu" class="gt_row gt_right">0.11</td>
<td headers="trvu" class="gt_row gt_right">3.84</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">132.88781</td>
<td headers="npar_un" class="gt_row gt_right">152.82098</td>
<td headers="lim_un" class="gt_row gt_right">145.17993</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0028</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV13</td>
<td headers="ins_class" class="gt_row gt_left">COMMERCIAL</td>
<td headers="ins_prim" class="gt_row gt_left">UHC</td>
<td headers="ord" class="gt_row gt_right">1</td>
<td headers="hcpcs" class="gt_row gt_right">90832</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Non-Facility</td>
<td headers="charges" class="gt_row gt_right">100</td>
<td headers="allowed" class="gt_row gt_right">51.92</td>
<td headers="payments" class="gt_row gt_right">51.92</td>
<td headers="adjustments" class="gt_row gt_right">48.08</td>
<td headers="wrvu" class="gt_row gt_right">1.70</td>
<td headers="prvu" class="gt_row gt_right">0.48</td>
<td headers="mrvu" class="gt_row gt_right">0.07</td>
<td headers="trvu" class="gt_row gt_right">2.25</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">77.86395</td>
<td headers="npar_un" class="gt_row gt_right">89.54354</td>
<td headers="lim_un" class="gt_row gt_right">85.06637</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0113</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV14</td>
<td headers="ins_class" class="gt_row gt_left">COMMERCIAL</td>
<td headers="ins_prim" class="gt_row gt_left">BCBS</td>
<td headers="ord" class="gt_row gt_right">1</td>
<td headers="hcpcs" class="gt_row gt_right">90834</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Facility</td>
<td headers="charges" class="gt_row gt_right">175</td>
<td headers="allowed" class="gt_row gt_right">71.00</td>
<td headers="payments" class="gt_row gt_right">71.00</td>
<td headers="adjustments" class="gt_row gt_right">104.00</td>
<td headers="wrvu" class="gt_row gt_right">2.24</td>
<td headers="prvu" class="gt_row gt_right">0.29</td>
<td headers="mrvu" class="gt_row gt_right">0.09</td>
<td headers="trvu" class="gt_row gt_right">2.62</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">90.66824</td>
<td headers="npar_un" class="gt_row gt_right">104.26848</td>
<td headers="lim_un" class="gt_row gt_right">99.05506</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0132</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV03</td>
<td headers="ins_class" class="gt_row gt_left">COMMERCIAL</td>
<td headers="ins_prim" class="gt_row gt_left">CIGNA</td>
<td headers="ord" class="gt_row gt_right">1</td>
<td headers="hcpcs" class="gt_row gt_right">90837</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Non-Facility</td>
<td headers="charges" class="gt_row gt_right">200</td>
<td headers="allowed" class="gt_row gt_right">65.00</td>
<td headers="payments" class="gt_row gt_right">65.00</td>
<td headers="adjustments" class="gt_row gt_right">135.00</td>
<td headers="wrvu" class="gt_row gt_right">3.31</td>
<td headers="prvu" class="gt_row gt_right">0.94</td>
<td headers="mrvu" class="gt_row gt_right">0.11</td>
<td headers="trvu" class="gt_row gt_right">4.36</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">150.88303</td>
<td headers="npar_un" class="gt_row gt_right">173.51549</td>
<td headers="lim_un" class="gt_row gt_right">164.83971</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0229</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV10</td>
<td headers="ins_class" class="gt_row gt_left">COMMERCIAL</td>
<td headers="ins_prim" class="gt_row gt_left">CIGNA</td>
<td headers="ord" class="gt_row gt_right">1</td>
<td headers="hcpcs" class="gt_row gt_right">90791</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Non-Facility</td>
<td headers="charges" class="gt_row gt_right">225</td>
<td headers="allowed" class="gt_row gt_right">75.00</td>
<td headers="payments" class="gt_row gt_right">75.00</td>
<td headers="adjustments" class="gt_row gt_right">150.00</td>
<td headers="wrvu" class="gt_row gt_right">3.84</td>
<td headers="prvu" class="gt_row gt_right">1.21</td>
<td headers="mrvu" class="gt_row gt_right">0.12</td>
<td headers="trvu" class="gt_row gt_right">5.17</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">178.91405</td>
<td headers="npar_un" class="gt_row gt_right">205.75116</td>
<td headers="lim_un" class="gt_row gt_right">195.46360</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0234</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV10</td>
<td headers="ins_class" class="gt_row gt_left">MEDICAID</td>
<td headers="ins_prim" class="gt_row gt_left">UHC</td>
<td headers="ord" class="gt_row gt_right">1</td>
<td headers="hcpcs" class="gt_row gt_right">90791</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Facility</td>
<td headers="charges" class="gt_row gt_right">225</td>
<td headers="allowed" class="gt_row gt_right">177.45</td>
<td headers="payments" class="gt_row gt_right">177.45</td>
<td headers="adjustments" class="gt_row gt_right">47.55</td>
<td headers="wrvu" class="gt_row gt_right">3.84</td>
<td headers="prvu" class="gt_row gt_right">0.49</td>
<td headers="mrvu" class="gt_row gt_right">0.12</td>
<td headers="trvu" class="gt_row gt_right">4.45</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">153.99759</td>
<td headers="npar_un" class="gt_row gt_right">177.09723</td>
<td headers="lim_un" class="gt_row gt_right">168.24237</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0234</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV10</td>
<td headers="ins_class" class="gt_row gt_left">MEDICAID</td>
<td headers="ins_prim" class="gt_row gt_left">UHC</td>
<td headers="ord" class="gt_row gt_right">2</td>
<td headers="hcpcs" class="gt_row gt_right">96130</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Facility</td>
<td headers="charges" class="gt_row gt_right">200</td>
<td headers="allowed" class="gt_row gt_right">120.35</td>
<td headers="payments" class="gt_row gt_right">120.35</td>
<td headers="adjustments" class="gt_row gt_right">79.65</td>
<td headers="wrvu" class="gt_row gt_right">2.56</td>
<td headers="prvu" class="gt_row gt_right">0.49</td>
<td headers="mrvu" class="gt_row gt_right">0.11</td>
<td headers="trvu" class="gt_row gt_right">3.16</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">109.35559</td>
<td headers="npar_un" class="gt_row gt_right">125.75893</td>
<td headers="lim_un" class="gt_row gt_right">119.47098</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0234</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV10</td>
<td headers="ins_class" class="gt_row gt_left">MEDICAID</td>
<td headers="ins_prim" class="gt_row gt_left">UHC</td>
<td headers="ord" class="gt_row gt_right">3</td>
<td headers="hcpcs" class="gt_row gt_right">96131</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Facility</td>
<td headers="charges" class="gt_row gt_right">215</td>
<td headers="allowed" class="gt_row gt_right">89.51</td>
<td headers="payments" class="gt_row gt_right">89.51</td>
<td headers="adjustments" class="gt_row gt_right">125.49</td>
<td headers="wrvu" class="gt_row gt_right">1.96</td>
<td headers="prvu" class="gt_row gt_right">0.27</td>
<td headers="mrvu" class="gt_row gt_right">0.09</td>
<td headers="trvu" class="gt_row gt_right">2.32</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">80.28638</td>
<td headers="npar_un" class="gt_row gt_right">92.32934</td>
<td headers="lim_un" class="gt_row gt_right">87.71287</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0234</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV10</td>
<td headers="ins_class" class="gt_row gt_left">MEDICAID</td>
<td headers="ins_prim" class="gt_row gt_left">UHC</td>
<td headers="ord" class="gt_row gt_right">4</td>
<td headers="hcpcs" class="gt_row gt_right">96136</td>
<td headers="units" class="gt_row gt_right">1</td>
<td headers="pos" class="gt_row gt_left">Facility</td>
<td headers="charges" class="gt_row gt_right">100</td>
<td headers="allowed" class="gt_row gt_right">44.24</td>
<td headers="payments" class="gt_row gt_right">44.24</td>
<td headers="adjustments" class="gt_row gt_right">55.76</td>
<td headers="wrvu" class="gt_row gt_right">0.55</td>
<td headers="prvu" class="gt_row gt_right">0.11</td>
<td headers="mrvu" class="gt_row gt_right">0.04</td>
<td headers="trvu" class="gt_row gt_right">0.70</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">24.22434</td>
<td headers="npar_un" class="gt_row gt_right">27.85799</td>
<td headers="lim_un" class="gt_row gt_right">26.46509</td></tr>
    <tr><td headers="dos" class="gt_row gt_right">2022-08-16</td>
<td headers="id" class="gt_row gt_left">PT0234</td>
<td headers="enc" class="gt_row gt_right">1</td>
<td headers="ren" class="gt_row gt_left">PROV10</td>
<td headers="ins_class" class="gt_row gt_left">MEDICAID</td>
<td headers="ins_prim" class="gt_row gt_left">UHC</td>
<td headers="ord" class="gt_row gt_right">5</td>
<td headers="hcpcs" class="gt_row gt_right">96137</td>
<td headers="units" class="gt_row gt_right">3</td>
<td headers="pos" class="gt_row gt_left">Facility</td>
<td headers="charges" class="gt_row gt_right">345</td>
<td headers="allowed" class="gt_row gt_right">119.52</td>
<td headers="payments" class="gt_row gt_right">119.52</td>
<td headers="adjustments" class="gt_row gt_right">225.48</td>
<td headers="wrvu" class="gt_row gt_right">0.46</td>
<td headers="prvu" class="gt_row gt_right">0.06</td>
<td headers="mrvu" class="gt_row gt_right">0.02</td>
<td headers="trvu" class="gt_row gt_right">0.54</td>
<td headers="cf" class="gt_row gt_right">34.6062</td>
<td headers="all_un" class="gt_row gt_right">56.06204</td>
<td headers="npar_un" class="gt_row gt_right">64.47135</td>
<td headers="lim_un" class="gt_row gt_right">61.24778</td></tr>
  </tbody>
  
  
</table>
</div>
```

:::
:::

::: {.cell layout-align="center"}

:::

::: {.cell layout-align="center"}

:::

{{< pagebreak >}}






## Session Info




::: {.cell layout-align="center"}

```{.r .cell-code}
sessioninfo::session_info()
```

::: {.cell-output .cell-output-stdout}

```
[1m[36m─ Session info ───────────────────────────────────────────[39m[22m
 [3m[90msetting [39m[23m [3m[90mvalue[39m[23m
 version  R version 4.3.2 (2023-10-31 ucrt)
 os       Windows 11 x64 (build 22631)
 system   x86_64, mingw32
 ui       RTerm
 language (EN)
 collate  English_United States.utf8
 ctype    English_United States.utf8
 tz       America/Los_Angeles
 date     2024-09-11
 pandoc   3.1.11 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

[1m[36m─ Packages ───────────────────────────────────────────────[39m[22m
 [3m[90m![39m[23m [3m[90mpackage      [39m[23m [3m[90m*[39m[23m [3m[90mversion    [39m[23m [3m[90mdate (UTC)[39m[23m [3m[90mlib[39m[23m [3m[90msource[39m[23m
   base64enc       0.1-3       [90m2015-07-28[39m [90m[1][39m [90mCRAN (R 4.3.1)[39m
   bit             4.0.5       [90m2022-11-15[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   bit64           4.0.5       [90m2020-08-30[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   cheapr          [1m[35m0.9.3.9000 [39m[22m [90m2024-09-01[39m [90m[1][39m [1m[35mGithub (NicChr/cheapr@ae1d1d9)[39m[22m
   cli             [1m[35m3.6.3.9000 [39m[22m [90m2024-06-25[39m [90m[1][39m [1m[35mGithub (r-lib/cli@d9febb5)[39m[22m
   collapse        2.0.16      [90m2024-08-21[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   colorspace      2.1-1       [90m2024-07-26[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   commonmark      1.9.1       [90m2024-01-30[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   crayon          1.5.3       [90m2024-06-20[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   data.table      1.16.0      [90m2024-08-27[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   digest          0.6.37      [90m2024-08-19[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   dplyr         * 1.1.4       [90m2023-11-17[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   ellipsis        0.3.2       [90m2021-04-29[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   evaluate        0.24.0      [90m2024-06-10[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   fansi           1.0.6       [90m2023-12-08[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   farver          2.1.2       [90m2024-05-13[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   fastmap         1.2.0       [90m2024-05-15[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   fontawesome     0.5.2       [90m2023-08-19[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   forcats       * 1.0.0       [90m2023-01-29[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   fs              1.6.4       [90m2024-04-25[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   fuimus        * 0.0.2       [90m2024-08-23[39m [90m[1][39m [1m[35mGithub (andrewallenbruce/fuimus@c029c8b)[39m[22m
   generics        0.1.3       [90m2022-07-05[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   ggiraph       * 0.8.10      [90m2024-05-17[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   ggplot2       * 3.5.1       [90m2024-04-23[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   ggthemes        5.1.0       [90m2024-02-10[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   glue            1.7.0       [90m2024-01-09[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   gt            * 0.11.0      [90m2024-07-09[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   gtable          0.3.5       [90m2024-04-22[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   gtExtras      * 0.5.0       [90m2023-09-15[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   here          * 1.0.1       [90m2020-12-13[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   hms             1.1.3       [90m2023-03-21[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   htmltools       0.5.8.1     [90m2024-04-04[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   htmlwidgets     1.6.4       [90m2023-12-06[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   janitor         2.2.0       [90m2023-02-02[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   jsonlite        1.8.8       [90m2023-12-04[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   knitr           1.48        [90m2024-07-07[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   labeling        0.4.3       [90m2023-08-29[39m [90m[1][39m [90mCRAN (R 4.3.1)[39m
   lattice         0.22-6      [90m2024-03-20[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   lifecycle       1.0.4       [90m2023-11-07[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   lubridate     * 1.9.3       [90m2023-09-27[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   magrittr        2.0.3       [90m2022-03-30[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   markdown        1.13        [90m2024-06-04[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   MASS            7.3-60.0.1  [90m2024-01-13[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   Matrix          1.6-5       [90m2024-01-11[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   MatrixModels    0.5-3       [90m2023-11-06[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   munsell         0.5.1       [90m2024-04-01[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   northstar     * 0.0.7       [90m2024-07-30[39m [90m[1][39m [1m[35mGithub (andrewallenbruce/northstar@5c01119)[39m[22m
   paletteer       1.6.0       [90m2024-01-21[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   pillar          1.9.0       [90m2023-03-22[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   pins            1.3.0       [90m2023-11-09[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   pkgconfig       2.0.3       [90m2019-09-22[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   purrr         * 1.0.2       [90m2023-08-10[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   qs              0.26.3      [90m2024-05-16[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   quantreg        5.98        [90m2024-05-26[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   R6              2.5.1       [90m2021-08-19[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   ragg            1.3.2       [90m2024-05-15[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   RApiSerialize   0.1.3       [90m2024-05-14[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   rappdirs        0.3.3       [90m2021-01-31[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   rbrvs         * 0.0.1       [90m2024-08-20[39m [90m[1][39m [1m[35mGithub (andrewallenbruce/rvu@8e09ebe)[39m[22m
   Rcpp            1.0.13      [90m2024-07-17[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
 [37m[41mD[49m[39m RcppParallel    5.1.9       [90m2024-08-19[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   readr         * 2.1.5       [90m2024-01-10[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   rematch2        2.1.2       [90m2020-05-01[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   rlang           1.1.4       [90m2024-06-04[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   rmarkdown       2.28        [90m2024-08-17[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   rprojroot       2.0.4       [90m2023-11-05[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   rstudioapi      [1m[35m0.16.0.9000[39m[22m [90m2024-06-04[39m [90m[1][39m [1m[35mGithub (rstudio/rstudioapi@a985492)[39m[22m
   sass            0.4.9       [90m2024-03-15[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   scales          1.3.0       [90m2023-11-28[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   sessioninfo     1.2.2       [90m2021-12-06[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   snakecase       0.11.1      [90m2023-08-27[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   SparseM         1.84-2      [90m2024-07-17[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   stringfish      0.16.0      [90m2023-11-28[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   stringi         1.8.4       [90m2024-05-06[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   stringr       * 1.5.1       [90m2023-11-14[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   survival        3.7-0       [90m2024-06-05[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   systemfonts     1.1.0       [90m2024-05-15[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   textshaping     0.4.0       [90m2024-05-24[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   tibble        * 3.2.1       [90m2023-03-20[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   tidyr         * 1.3.1       [90m2024-01-24[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   tidyselect      1.2.1       [90m2024-03-11[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   tidyverse     * 2.0.0       [90m2023-02-22[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   timechange      0.3.0       [90m2024-01-18[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   timeplyr        [1m[35m0.8.2.9000 [39m[22m [90m2024-09-01[39m [90m[1][39m [1m[35mGithub (NicChr/timeplyr@bdfaaf7)[39m[22m
   tzdb            0.4.0       [90m2023-05-12[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   utf8            1.2.4       [90m2023-10-22[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   uuid            1.2-1       [90m2024-07-29[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   vctrs           0.6.5       [90m2023-12-01[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   vroom           1.6.5       [90m2023-12-05[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   withr           3.0.1       [90m2024-07-31[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   xfun            0.47        [90m2024-08-17[39m [90m[1][39m [90mCRAN (R 4.3.3)[39m
   xml2            1.3.6       [90m2023-12-04[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m
   yaml            2.3.10      [90m2024-07-26[39m [90m[1][39m [90mCRAN (R 4.3.2)[39m

[90m [1] C:/Program Files/R/R-4.3.2/library[39m

 [41m[37mD[39m[49m ── DLL MD5 mismatch, broken installation.

[1m[36m──────────────────────────────────────────────────────────[39m[22m
```


:::
:::

