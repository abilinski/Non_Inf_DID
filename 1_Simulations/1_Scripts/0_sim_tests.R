#### TEST MAKE_DATA_UP_FUNCTION ####
here::i_am("1_Simulations/1_Scripts/0_sim_tests.R")
source("global_options.R")

# -------------------------------
# Begin Test Cases using testthat
# -------------------------------

# Some AI-generated tests as an extra check!

test_that("n1 and n2 parameters produce correct dimensions and groups", {
  n1 <- 5; n2 <- 5; periods <- 4; post.time <- 3
  df <- make_data_up(n1, n2, periods, post.time)
  
  # Check dimensions
  expect_equal(nrow(df), (n1 + n2) * periods)
  
  # Verify group assignments
  expect_equal(sum(df$group == "Treatment"), n1 * periods)
  expect_equal(sum(df$group == "Control"), n2 * periods)
})

test_that("periods parameter sets correct year values and factor levels", {
  n1 <- 3; n2 <- 2; periods <- 10; post.time <- 6
  df <- make_data_up(n1, n2, periods, post.time)
  
  # Verify unique years equal the number of periods
  expect_equal(length(unique(df$year)), periods)
  
  # Check that factoryear is a factor with levels as characters "1" to "periods"
  expect_equal(levels(df$factoryear), as.character(1:periods))
})

test_that("post.time is correctly marked", {
  n1 <- 4; n2 <- 4; periods <- 8; post.time <- 5
  df <- make_data_up(n1, n2, periods, post.time)
  
  # Years before post.time should be FALSE; after (and including) post.time, TRUE.
  expect_true(all(df$post[df$year < post.time] == FALSE))
  expect_true(all(df$post[df$year >= post.time] == TRUE))
})

test_that("Invalid post.time returns an appropriate error message", {
  n1 <- 4; n2 <- 4; periods <- 8; post.time <- 9
  res <- make_data_up(n1, n2, periods, post.time)
  expect_equal(res, "The intervention must start within the specified period (post.time <= periods) and no earlier than time 3.")
})

test_that("Treatment effect (trt) is applied correctly", {
  n1 <- 3; n2 <- 3; periods <- 5; post.time <- 3; trt <- 3
  df <- make_data_up(n1, n2, periods, post.time, trt = trt)
  
  # For treatment units in post period, the treatment effect should be added.
  df_treat_post <- df %>% filter(group == "Treatment", post == TRUE)
  computed_trt <- df_treat_post$outcome - (df_treat_post$state_err + df_treat_post$vio + df_treat_post$indiv.error)
  expect_true(all(abs(computed_trt - trt) < 1e-6))
})

test_that("Linear violation is applied correctly", {
  n1 <- 4; n2 <- 4; periods <- 6; post.time <- 4; trt <- 2
  slope <- 1.0
  df <- make_data_up(n1, n2, periods, post.time, trt = trt, vio.type = "Linear", vio.linear.slope = slope)
  
  # For treatment units, violation should equal year * slope; for control units, zero.
  df_treatment <- df %>% filter(group == "Treatment")
  expect_equal(df_treatment$vio, df_treatment$year * slope)
  
  df_control <- df %>% filter(group == "Control")
  expect_true(all(df_control$vio == 0))
})

test_that("Sudden jump violation is applied correctly with a specified period", {
  n1 <- 4; n2 <- 4; periods <- 8; post.time <- 5; trt <- 2 
  jump_period <- 4; jump_size <- 3
  df <- make_data_up(n1, n2, periods, post.time, trt = trt, vio.type = "Sudden jump", 
                     vio.sudden.jump.per = jump_period, vio.sudden.jump = jump_size)
  
  # For treatment units, violation should equal jump_size for years >= jump_period, else zero.
  df_treatment <- df %>% filter(group == "Treatment")
  expect_equal(df_treatment$vio, ifelse(df_treatment$year >= jump_period, jump_size, 0))
})

test_that("Under heteroskedasticity, sigma2 is constant within each unit", {
  n1 <- 5; n2 <- 5; periods <- 10; post.time <- 5
  df <- make_data_up(n1, n2, periods, post.time, sigma2 = 1, hetero = TRUE)
  
  # Group by unit and ensure sigma2 is constant within each unit.
  sigma2_by_unit <- df %>% group_by(unit.ID) %>% summarise(n_unique = n_distinct(sigma2))
  expect_true(all(sigma2_by_unit$n_unique == 1))
})

test_that("Autocorrelation parameter is approximately 0.2 on average across simulations", {
  sims <- 50
  acf_estimates <- numeric(sims)
  #set.seed(42)
  for(i in 1:sims) {
    df <- make_data_up(n1 = 10, n2 = 10, periods = 30, post.time = 5, sigma2 = 1, autocorr = TRUE)
    # Compute lag-1 acf for each unit
    unit_acfs <- df %>% 
      group_by(unit.ID) %>% 
      summarise(acf_val = tryCatch(acf(indiv.error, plot = FALSE, lag.max = 1)$acf[2],
                                   error = function(e) NA)) %>% 
      pull(acf_val)
    acf_estimates[i] <- mean(unit_acfs, na.rm = TRUE)
  }
  overall_mean_acf <- mean(acf_estimates, na.rm = TRUE)
  expect_equal(overall_mean_acf, 0.2, tolerance = 0.1)
})

test_that("Autocorrelation parameter is approximately 0.2 on average across simulations when hetero is F", {
  sims <- 50
  acf_estimates <- numeric(sims)
  #set.seed(42)
  for(i in 1:sims) {
    df <- make_data_up(n1 = 10, n2 = 10, periods = 30, post.time = 5, sigma2 = 1, autocorr = TRUE, hetero = F)
    # Compute lag-1 acf for each unit
    unit_acfs <- df %>% 
      group_by(unit.ID) %>% 
      summarise(acf_val = tryCatch(acf(indiv.error, plot = FALSE, lag.max = 1)$acf[2],
                                   error = function(e) NA)) %>% 
      pull(acf_val)
    acf_estimates[i] <- mean(unit_acfs, na.rm = TRUE)
  }
  overall_mean_acf <- mean(acf_estimates, na.rm = TRUE)
  expect_equal(overall_mean_acf, 0.2, tolerance = 0.1)
})

test_that("Outcome is correctly computed as the sum of treatment effect, violation, state error, and individual error", {
  n1 <- 3; n2 <- 3; periods <- 5; post.time <- 3; trt <- 2
  df <- make_data_up(n1, n2, periods, post.time, trt = trt, vio.type = "Linear", vio.linear.slope = 0.5)
  
  # Recompute the outcome manually for all rows.
  computed_outcome <- ifelse(df$trtpost, trt, 0) + df$vio + df$state_err + df$indiv.error
  expect_equal(df$outcome, computed_outcome, tolerance = 1e-6)
})

test_that("Under heteroskedasticity with autocorrelation, empirical variance tracks assigned sigma2", {
  n1 <- 10; n2 <- 10; periods <- 100; post.time <- 50
  #set.seed(123)
  df <- make_data_up(n1, n2, periods, post.time, sigma2 = 1, hetero = TRUE, autocorr = TRUE)
  
  var_by_unit <- df %>% 
    group_by(unit.ID) %>% 
    summarise(assigned = first(sigma2),
              empirical = var(indiv.error))
  
  cor_test <- cor.test(var_by_unit$assigned, var_by_unit$empirical)
  expect_gt(cor_test$estimate, 0.5)
})

test_that("With hetero=TRUE and autocorr=FALSE, high-assigned-variance units have much larger empirical error variance", {
  #set.seed(202)
  
  # Choose sigma2 small so sigma2_cluster has big relative dispersion (and truncation at 0.1 creates spread)
  n1 <- 50; n2 <- 50; periods <- 400; post.time <- 200
  df <- make_data_up(n1, n2, periods, post.time, sigma2 = 0.2, hetero = TRUE, autocorr = FALSE)
  
  var_by_unit <- df %>%
    group_by(unit.ID) %>%
    summarise(
      assigned = first(sigma2),
      empirical = var(indiv.error),
      .groups = "drop"
    )
  
  # Compare bottom vs top quartile of assigned variance
  q <- quantile(var_by_unit$assigned, probs = c(0.25, 0.75), na.rm = TRUE)
  low  <- var_by_unit %>% filter(assigned <= q[[1]])
  high <- var_by_unit %>% filter(assigned >= q[[2]])
  
  ratio <- mean(high$empirical, na.rm = TRUE) / mean(low$empirical, na.rm = TRUE)
  
  # If heteroskedasticity is actually used in iid errors, this should be comfortably > 1.
  # If errors are homoskedastic (buggy version), ratio should be ~1 (up to noise).
  expect_gt(ratio, 1.6)
})

test_that("With hetero=TRUE and autocorr=TRUE, high-assigned-variance units have larger empirical variance", {
  #set.seed(123)
  n1 <- 50; n2 <- 50; periods <- 400; post.time <- 200
  
  df <- make_data_up(n1, n2, periods, post.time, sigma2 = 0.2, hetero = TRUE, autocorr = TRUE)
  
  var_by_unit <- df %>%
    group_by(unit.ID) %>%
    summarise(
      assigned = first(sigma2),
      empirical = var(indiv.error),
      .groups = "drop"
    )
  
  q <- quantile(var_by_unit$assigned, probs = c(0.25, 0.75), na.rm = TRUE)
  low  <- var_by_unit %>% filter(assigned <= q[[1]])
  high <- var_by_unit %>% filter(assigned >= q[[2]])
  
  ratio <- mean(high$empirical, na.rm = TRUE) / mean(low$empirical, na.rm = TRUE)
  
  expect_gt(ratio, 1.4)
})


test_that("With hetero=TRUE and autocorr=TRUE, empirical variance increases with assigned variance (Spearman)", {
  #set.seed(123)
  n1 <- 30; n2 <- 30; periods <- 300; post.time <- 150
  
  df <- make_data_up(n1, n2, periods, post.time, sigma2 = 0.5, hetero = TRUE, autocorr = TRUE)
  
  var_by_unit <- df %>%
    group_by(unit.ID) %>%
    summarise(
      assigned = first(sigma2),
      empirical = var(indiv.error),
      .groups = "drop"
    )
  
  rho <- cor(var_by_unit$assigned, var_by_unit$empirical, method = "spearman")
  expect_gt(rho, 0.5)
})

test_that("With hetero=FALSE and autocorr=TRUE, errors are homoskedastic", {
  n1 <- 50; n2 <- 50; periods <- 400; post.time <- 200
  
  df <- make_data_up(n1, n2, periods, post.time, sigma2 = 1, hetero = FALSE, autocorr = TRUE)
  
  var_by_unit <- df %>%
    group_by(unit.ID) %>%
    summarise(
      empirical = var(indiv.error),
      .groups = "drop"
    )
  
  # Under homoskedasticity, empirical variances should be similar across units
  # The coefficient of variation should be small
  cv <- sd(var_by_unit$empirical) / mean(var_by_unit$empirical)
  
  # With 400 periods per unit, sampling variance is low, so CV should be small if truly homoskedastic
  expect_lt(cv, 0.15)
})

