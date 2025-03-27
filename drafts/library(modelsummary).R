library(modelsummary)
library(tinytable)
N <- 1e4
x <- rnorm(N)
x2 <- rnorm(N, 1.3, .8)
y <- rnorm(N, 2 + .3*x - .6*x2)
df_to_lm <- data.frame(x, x2, y)
lm_totable_1 <- lm(y ~ x, df_to_lm)
lm_totable_2 <- lm(y ~ x + x2, df_to_lm)
lm_totable_3 <- lm(y ~ x*x2, df_to_lm)


tt_model_cbu <- function(x) {
    modelsummary(
    models = x, 
    fmt = 2,
    estimate = c("B" = "{estimate}"),
    statistic = c("Błąd Standardowy" = "{std.error}"), # Correctly renaming estimate
    coef_omit = "Intercept",
    shape = term ~ model + statistic,
    gof_map = "r.squared"
  )
}

tt_model_cbu(list("model 1" = lm_totable_1, "model 2" = lm_totable_2, "model 3" = lm_totable_3))
