

# expenses adjusted with inflation
inflation_adjusted_expenses <- function(base_expenses, n_months, annual_inflation, start_age) {
  years <- floor((0:(n_months - 1)) / 12)
  ages <- start_age + (0:(n_months - 1)) / 12
  
  get_multiplier <- function(age) {
    ifelse(age < 30, 1.0,
           ifelse(age < 40, 1.2,
                  ifelse(age < 50, 0.9,
                         ifelse(age < 70, 0.7, 0.5))))
  }
  
  multipliers <- get_multiplier(ages)
  base_expenses * (1 + annual_inflation) ^ years * multipliers
}


# Polish PIT tax function 
polish_pit_tax <- function(annual_income) {
  tax_free = 30000
  lower_threshold = 120000
  lower_rate = 0.12
  upper_rate = 0.32
  fixed_tax = 10800  # (120000-30000)*0.12
  
  tax <- numeric(length(annual_income))
  # Below tax-free threshold
  tax[annual_income <= tax_free] <- 0
  # Between tax-free and upper threshold
  idx1 <- annual_income > tax_free & annual_income <= lower_threshold
  tax[idx1] <- (annual_income[idx1] - tax_free) * lower_rate
  # Above upper threshold
  idx2 <- annual_income > lower_threshold
  tax[idx2] <- fixed_tax + (annual_income[idx2] - lower_threshold) * upper_rate
  return(tax)
}


retirement_duration <- function(retirement_savings, monthly_cost) {
  total_months <- floor(retirement_savings / monthly_cost)
  years <- floor(total_months / 12)
  months <- total_months %% 12
  
  data.frame(
    years = years,
    months = months,
    total_months = total_months,
    monthly_cost = monthly_cost,
    retirement_savings = retirement_savings
  )
}

