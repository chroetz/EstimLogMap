library(tidyverse)
filePaths <- list.files(path = "./data/", pattern = "^error.*\\.csv$", full.names=TRUE)
data <-
  lapply(filePaths, \(filePath) filePath |> read_csv() |> mutate(fileName = basename(filePath))) |>
  bind_rows()
results <-
  data |>
  separate_wider_regex(fileName, patterns = c("error", method = "[^_]*", "_", noise = "[^\\.]*", "\\.csv"))
wideAbsErrs <-
  results |>
  select(n, noiseParam, method, noise, AE_mean) |>
  pivot_wider(names_from=method, values_from=AE_mean) |>
  arrange(noise, n, noiseParam)
ranks <-
  results |>
  mutate(rank = rank(AE_mean, ties.method="random"), .by = c(n, noiseParam, noise)) |>
  select(n, noiseParam, method, noise, rank)
wideRanks <-
  ranks |>
  pivot_wider(names_from=method, values_from=rank) |>
  arrange(noise, n, noiseParam)

library(gt)
tbl <-
  wideAbsErrs |>
  gt() |>
  opt_row_striping(row_striping = TRUE) |>
  opt_vertical_padding(scale = 0) |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    noise = "Type",
    noiseParam = "Param",
    n = "N"
  ) |>
  cols_move_to_start(c(noise, noiseParam)) |>
  tab_spanner(
    label = "Noise",
    columns = c(noise, noiseParam),
  ) |>
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) |>
  fmt_engineering(
    columns = -c(n, noiseParam, noise),
    decimals = 2
  ) |>
  data_color(
    columns = -c(n, noiseParam, noise),
    fn = \(x) (scales::col_numeric(
        "viridis",
        domain = c(-3, 0),
        na.color = "#808080",
        alpha = FALSE,
        reverse = TRUE
    ))(pmin(0, pmax(-3, log10(x))))
  )
tbl
write_lines(as_raw_html(tbl), "error_results.html")


reducedWideAbsErrs <-
  wideAbsErrs |>
  filter(n %in% c(8, 16)) |>
  filter(noiseParam < 1000 & noiseParam >= 0.01)

tblReducedGauss <-
  reducedWideAbsErrs |>
  filter(noise == "Gauss") |>
  select(-noise) |>
  gt() |>
  opt_row_striping(row_striping = TRUE) |>
  opt_vertical_padding(scale = 0) |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    noiseParam = "sd",
    n = "n"
  ) |>
  cols_move_to_start(c(noiseParam)) |>
  cols_move_to_end(Gs1) |>
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) |>
  fmt_number(
    columns = -c(n, noiseParam),
    n_sigfig = 2
  ) |>
  data_color(
    columns = -c(n, noiseParam),
    method = "numeric",
    palette = "viridis",
    direction = "row"
  ) |>
  tab_row_group("n=8", n==8) |>
  tab_row_group("n=16", n==16) |>
  cols_hide(n)
tblReducedGauss
write_lines(as_raw_html(tblReducedGauss), "error_results_reduced_Gauss.html")


tblReducedBeta <-
  reducedWideAbsErrs |>
  filter(noise == "Beta", noiseParam >= 1) |>
  select(-noise) |>
  gt() |>
  opt_row_striping(row_striping = TRUE) |>
  opt_vertical_padding(scale = 0) |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    noiseParam = "a",
    n = "n"
  ) |>
  cols_move_to_start(c(noiseParam)) |>
  cols_move_to_end(Gs1) |>
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) |>
  fmt_number(
    columns = -c(n, noiseParam),
    n_sigfig = 2
  ) |>
  data_color(
    columns = -c(n, noiseParam),
    method = "numeric",
    palette = "viridis",
    direction = "row"
  ) |>
  tab_row_group("n=8", n==8) |>
  tab_row_group("n=16", n==16) |>
  cols_hide(n)
tblReducedBeta
write_lines(as_raw_html(tblReducedBeta), "error_results_reduced_Beta.html")


pltGauss <-
  reducedWideAbsErrs |>
  filter(noise == "Gauss", noiseParam <= 0.32) |>
  pivot_longer(c(Gs1, Ols, Tls), names_to="Estimator", values_to="MAE") |>
  mutate(Estimator = factor(Estimator, levels = c("Ols", "Tls", "Gs1"), labels = c("OLS", "TLS", "Grid Search"))) |>
  mutate(n = as.factor(n)) |>
  ggplot(aes(x = noiseParam, y = MAE, color = Estimator), ) +
  geom_point(aes(shape = n)) +
  geom_line(aes(linetype = n)) +
  scale_y_log10(limits = c(NA, 1)) +
  scale_x_log10() +
  xlab("Noise Standard Deviation") +
  ggtitle("Gaussian Noise")
pltGauss

library("scales")
reverselog_trans <- function (base = exp(1)) {
  force(base)
  new_transform(paste0("log-", format(base)), function(x) -log(x,
      base), function(x) base^(-x), d_transform = function(x) -1/x/log(base),
      d_inverse = function(x) -base^x * log(base), breaks = log_breaks(base = base),
      domain = c(1e-100, Inf))
}
pltBeta <-
  reducedWideAbsErrs |>
  filter(noise == "Beta", noiseParam >= 1) |>
  pivot_longer(c(Gs1, Ols, Tls), names_to="Estimator", values_to="MAE") |>
  mutate(Estimator = factor(Estimator, levels = c("Ols", "Tls", "Gs1"), labels = c("OLS", "TLS", "Grid Search"))) |>
  mutate(n = as.factor(n)) |>
  ggplot(aes(x = noiseParam, y = MAE, color = Estimator), ) +
  geom_point(aes(shape = n)) +
  geom_line(aes(linetype = n)) +
  scale_y_log10(limits = c(NA, 1)) +
  scale_x_continuous(transform  = reverselog_trans(10)) +
  xlab("Noise Precision") +
  ggtitle("Beta Noise")
pltBeta

ggsave("EstimLogMap-Restricted-Gauss.pdf", plot = pltGauss, width = 5, height = 3)
ggsave("EstimLogMap-Restricted-Beta.pdf", plot = pltBeta, width = 5, height = 3)
