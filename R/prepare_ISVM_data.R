#' Prepare ISVM data
#'
#' Preprocessing ISVM data and getting coefficient estimates.
#'
#' @param DATA A file generated from the "make_ISVM_data" function
#'
#' @return n: Sample size, i.e., the number of days
#' @return beta01: Vector of regression coefficient beta^{(0,1)} (see equation (36))
#' @return beta02: Vector of regression coefficient beta^{(0,2)} (see equation (36))
#' @return beta10: Vector of regression coefficient beta^{(1,0)} (see equation (36))
#' @return v: Vector of spot volatilities, i.e., beta^{(0,0)} in equation (36)
#' @return x_all: IV after data filteration
#' @return t_all: Time-to-maturities after data filteration
#' @return k_all: Log-moneyness after data filteration
#' @return r: Risk-free interest rates after data filteration
#' @return d: Dividend yields after data filteration
#' @return numofIV: Vector for recording the number of IV observations on each day

#' @export
data_crypto <- function(DATA){
  # Load empirical dataset
  dat_emp <- DATA
  names(dat_emp) = c("IV", "money", "mtr", "Sobs", "cp", "d", "date", "price", "r")

  Trange1 <- 5
  Trange2 <- 60 # The first choice of maturities is between Trange1 and Trange 2

  # # Extract items in data
  # Extract items in data
  date_use  <- dat_emp[['date']]
  k_emp <- dat_emp[['money']]
  x_emp <- dat_emp[['IV']]
  tau_emp <- dat_emp[['mtr']]
  r_emp <- dat_emp[['r']]/100
  d <- dat_emp[['d']]/100
  n <- length(date_use)

  # Initialization
  r <- rep(NaN, n)
  v <- rep(NaN, n)
  beta01 <- rep(NaN, n)
  beta02 <- rep(NaN, n)
  beta10 <- rep(NaN, n)
  numofIV <- rep(NaN, n) # This is used for recording the number of IV observations on each day
  model_fit <- vector(mode = "list", length = n)
  x_all <- NULL
  t_all <- NULL
  k_all <- NULL
  target_t_all <- NULL # This is used for recording unique time-to-maturities
  
  # Apply the data filters described in Section 5 and run bivariate regression (36)
  for (i in 1:n) {

    #### Apply the data filters ####
    # Extract the data on the ith day
    r_trial = as.vector(r_emp[i,])
    k_trial = as.vector(k_emp[i,])
    t_trial = as.vector(tau_emp[i,])/360
    x_trial = as.vector(x_emp[i,])

    # Estimate spot volatilities by the IV at the smallest |k| for the smallest maturity greater than Trange1
    k_temp = k_trial[which(t_trial == min(t_trial[!is.nan(t_trial) & t_trial >= Trange1/360]))]
    x_temp = x_trial[which(t_trial == min(t_trial[!is.nan(t_trial) & t_trial >= Trange1/360]))]
    vol = mean(x_temp[abs(k_temp) == min(abs(k_temp)) & !is.nan(abs(k_temp))])

    # Choose options with log-moneyness within -v*sqrt(tau) and v*sqrt(tau)
    pos = which(k_trial > 2*vol*sqrt(t_trial) | k_trial < 2*-vol*sqrt(t_trial) | is.nan(k_trial))

    if (length(pos) > 0) {
      x_trial = x_trial[-pos]
      r_trial = r_trial[-pos]
      t_trial = t_trial[-pos]
      k_trial = k_trial[-pos]
    }

    # Decision Tree for time-to-maturities, i.e., data filters (see Section 5)
    unique_t = unique(t_trial[!is.nan(t_trial)])
    if (length(unique_t) < 4) { # We skip this day
      v[i] = NaN
      x_all[i] = list(NaN)
      t_all[i] = list(NaN)
      k_all[i] = list(NaN)
      next
    }
    nwithin = sum(unique_t <= Trange2/360 & unique_t >= Trange1/360)
    if (nwithin >= 4) { # We keep all the data with time-to-maturities between 5 and 60 days
      target_t = unique_t[unique_t <= Trange2/360 & unique_t >= Trange1/360]
    }

    if (nwithin == 3) { # We need to find one more candidate
      l_candi = unique_t[unique_t < Trange1/360 & unique_t >= 8/360]
      r_candi = unique_t[unique_t > Trange2/360 & unique_t <= 160/360]
      if (length(l_candi) + length(r_candi) == 0) {
        v[i] = NaN
        x_all[i] = list(NaN)
        t_all[i] = list(NaN)
        k_all[i] = list(NaN)
        next
      } else {
        combine_candi = c(l_candi, r_candi)
        dist_candi = c(Trange1/360 - l_candi, r_candi - Trange2/360)
        final_candi = combine_candi[which.min(dist_candi)]
        target_t = c(final_candi, unique_t[unique_t <= Trange2/360 & unique_t >= Trange1/360])
      }
    }

    if (nwithin == 2) { # We need to find two more candidates
      l_candi = unique_t[unique_t < Trange1/360 & unique_t >= 8/360]
      r_candi = unique_t[unique_t > Trange2/360 & unique_t <= 160/360]
      if (length(l_candi) + length(r_candi) <= 1){
        v[i] = NaN
        x_all[i] = list(NaN)
        t_all[i] = list(NaN)
        k_all[i] = list(NaN)
        next
      } else {
        combine_candi = c(l_candi, r_candi)
        dist_candi = c(Trange1/360 - l_candi, r_candi - Trange2/360)
        sort_dist = sort(dist_candi)
        final_candi = combine_candi[dist_candi == sort_dist[1] | dist_candi == sort_dist[2]]
        target_t = c(final_candi, unique_t[unique_t <= Trange2/360 & unique_t >= Trange1/360])
      }
    }

    if (nwithin == 1) {  # We need to find three more candidates
      l_candi = unique_t[unique_t < Trange1/360 & unique_t >= 8/360]
      r_candi = unique_t[unique_t > Trange2/360 & unique_t <= 160/360]
      if (length(l_candi) + length(r_candi) <= 2) {
        v[i] = NaN
        x_all[i] = list(NaN)
        t_all[i] = list(NaN)
        k_all[i] = list(NaN)
        next
      } else {
        combine_candi = c(l_candi, r_candi)
        dist_candi = c(Trange1/360 - l_candi, r_candi - Trange2/360)
        sort_dist = sort(dist_candi)
        final_candi = combine_candi[dist_candi == sort_dist[1] | dist_candi == sort_dist[2] | dist_candi == sort_dist[3]]
        target_t = c(final_candi, unique_t[unique_t <= Trange2/360 & unique_t >= Trange1/360])
      }
    }

    if (nwithin == 0) { # We skip this day
      v[i] = NaN
      x_all[i] = list(NaN)
      t_all[i] = list(NaN)
      k_all[i] = list(NaN)
      next
    }

    target_t_all = c(target_t_all, target_t*360)
    pos = (t_trial %in% target_t)

    x_trial = x_trial[pos]
    r_trial = r_trial[pos]
    t_trial = t_trial[pos]
    k_trial = k_trial[pos]

    # Delete NaNs in x_trial
    r_trial = r_trial[!is.nan(x_trial)]
    k_trial = k_trial[!is.nan(x_trial)]
    t_trial = t_trial[!is.nan(x_trial)]
    x_trial = x_trial[!is.nan(x_trial)]
    x_all[i] = list(x_trial); t_all[i] = list(t_trial); k_all[i] = list(k_trial)
    numofIV[i] = length(x_trial)

    # Set the risk-free interest rate r as the average of all data on this day
    r[i] = mean(r_trial)
    #### The end of data filters ####

    #### Bivariate regressions ####
    # Construct the regressors (see equation (36))
    k2 = k_trial^2 # k^2
    tk = t_trial*k_trial # tau*k
    t2 = t_trial^2 # tau^2
    t2k = t_trial^2*k_trial # tau^2*k
    # t_sqrt = sqrt(t_trial)
    # t_sqrt_k = t_sqrt*k_trial
    # t_minus_sqrt = t^(-0.5)
    # t_minus_sqrt_k2 = t_minus_sqrt * k2
    # t_min1 <- t^(-1)
    # k3 = k_trial^3
    # t_min1_k3 = t_min1*k3

    # Bivariate regression
    obj = lm(x_trial ~ k_trial + k2 + t_trial + tk + t2 + t2k)
    # obj_eq32 = lm(x_trial ~ t_trial + t2 + k_trial + tk + t2k + k2  ) # equation (32)
    # obj_eq36 = lm(x_trial ~ t_sqrt + t_trial + k_trial + t_sqrt_k + t_minus_sqrt_k2 + k2 + t_min1_k3)
    model_fit[[i]] <- obj
    v[i] = obj$coefficients[1]
    beta01[i] = obj$coefficients[2]
    beta02[i] = obj$coefficients[3]
    beta10[i] = obj$coefficients[4]

    # phi

    #### The end of regressions ####
  }

  # Delete the NaNs in v
  ind = which(is.nan(v) | v >= 1.2 | v <= 0) # Indices of v that are either nan or extremely large

  if (length(ind) > 0) {
    beta01 = beta01[-ind]
    beta02 = beta02[-ind]
    beta10 = beta10[-ind]
    r = r[-ind]
    d = d[-ind]
    v = v[-ind]
    numofIV = numofIV[-ind]
    x_all = x_all[-ind]
    k_all = k_all[-ind]
    t_all = t_all[-ind]
    model_fit <- model_fit[-ind]
  }
  n = length(v) 

  tryCatch(expr = {
    v[[1]]
  },
  error = function(e){
    message('Error: All values of v are either nan or extremely large/small')
    print(e)
    return(list())
  })
  # Combine and return the results
  return(list(n = n,
              beta01 = beta01,
              beta02 = beta02,
              beta10 = beta10,
              v = v,
              x_all = x_all,
              t_all = t_all,
              k_all = k_all,
              r = r,
              d = d,
              numofIV = numofIV,
              model_fit = model_fit))

}
