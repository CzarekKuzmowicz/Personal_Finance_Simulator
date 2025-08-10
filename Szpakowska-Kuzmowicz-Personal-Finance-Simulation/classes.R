# Classes

setClass(
  "UserProfile",
  slots = c(
    age = "numeric",
    retire_age = "numeric",
    life_expectancy = "numeric",
    income = "numeric",             
    income_growth = "numeric",      
    expenses = "numeric",           
    inflation = "numeric",          
    saving_rate = "numeric",        
    has_loan = "logical",
    loan = "Loan"
  )
)

setClass(
  "Loan",
  slots = c(
    amount = "numeric",
    rate = "numeric", 
    term = "numeric"   
  )
)

setClass(
  "SimulationResult",
  slots = c(
    user = "UserProfile",
    data = "data.frame"
  )
)

# Simulation Logic 

setGeneric("simulate_finances", function(user, alloc_bank, alloc_bonds, alloc_stocks) standardGeneric("simulate_finances"))

setMethod("simulate_finances", "UserProfile", function(user, alloc_bank, alloc_bonds, alloc_stocks) {
  months <- seq(0, (user@life_expectancy - user@age) * 12)
  n_months <- length(months)
  years <- user@age + months / 12
  
  if (n_months <= 0) {
    stop("Life expectancy must be greater than current age!")
  }
  
  # Monthly Income Calculation
  current_ages <- user@age + (0:(n_months - 1)) / 12
  years_passed <- floor((0:(n_months - 1)) / 12)
  
  final_salary <- user@income * (1 + user@income_growth) ^ (user@retire_age - user@age)
  is_retired <- current_ages >= user@retire_age
  
  monthly_income <- numeric(n_months)
  working_salary <- user@income * (1 + user@income_growth) ^ years_passed
  
  # Pension: 30% of last salary before retirement, grows 2% per year after retirement
  pension_growth_rate <- 0.02
  retirement_month <- which(is_retired)[1]
  pension <- numeric(n_months)
  
  if (!is.na(retirement_month)) {
    months_retired <- 0:(n_months - retirement_month)
    pension[is_retired] <- (final_salary * 0.3) * (1 + pension_growth_rate) ^ (months_retired / 12)
  }
  
  monthly_income[!is_retired] <- working_salary[!is_retired]
  monthly_income[is_retired]  <- pension[is_retired]
  
  # Tax calculation 
  annual_income <- monthly_income * 12
  monthly_tax <- polish_pit_tax(annual_income) / 12
  net_monthly_income <- monthly_income - monthly_tax

  
  # Monthly expenses adjusted for inflation
  monthly_expenses <- inflation_adjusted_expenses(
    base_expenses = user@expenses,
    n_months = n_months,
    annual_inflation = user@inflation,
    start_age = user@age
  )  
  
  available_cash <- net_monthly_income - monthly_expenses
  monthly_savings <- available_cash * (user@saving_rate / 100)  
  monthly_savings <- pmax(monthly_savings, 0) 
  
  # Setting annual returns 
  bank_return_annual <- 0.5
  bonds_return_annual <- 2
  annual_returns <- rnorm(ceiling(n_months/12), mean = 5, sd = 15)
  stocks_return_annual <- rep(annual_returns, each = 12)[1:n_months]
  
  w_bank <- alloc_bank
  w_bonds <- alloc_bonds
  w_stocks <- alloc_stocks
  
  # Monthly rates
  bank_rate_monthly <- (bank_return_annual - user@inflation) / 100 / 12
  bonds_rate_monthly <- bonds_return_annual / 100 / 12
  stocks_rate_monthly <- stocks_return_annual / 100 / 12
  
  # Cumulative function for each investment bucket
  cumulative_investment <- function(savings, rates) {
    Reduce(
      function(prev, i) prev * (1 + rates[i]) + savings[i], 
      seq_along(savings), 
      init = 0, 
      accumulate = TRUE
    )[-1]  # removing the initial zero
  }
  
  inv_bank <- cumulative_investment(monthly_savings * w_bank, rep(bank_rate_monthly, n_months))
  inv_bonds <- cumulative_investment(monthly_savings * w_bonds, rep(bonds_rate_monthly, n_months))
  inv_stocks <- cumulative_investment(monthly_savings * w_stocks, stocks_rate_monthly)
  
  
  
  
  # Monthly loan amortization if loan is used
debt <- numeric(n_months)
if (user@has_loan) {
  loan <- user@loan
  r_monthly <- loan@rate / 100 / 12
  n_payments <- loan@term * 12
  payment <- (loan@amount * r_monthly) / (1 - (1 + r_monthly)^(-n_payments))
  
  i <- seq_len(min(n_payments, n_months))  
  
  debt[i] <- loan@amount * (1 + r_monthly)^i - 
    payment * (( (1 + r_monthly)^i - 1) / r_monthly)
  
  debt <- pmax(debt, 0)
}
# If no loan, debt is already zero vector :P


# Combining into a result dataframe
df <- data.frame(
  Month = months,
  Year = years,
  Income = monthly_income,
  Tax = monthly_tax,
  NetIncome = net_monthly_income,
  Expenses = monthly_expenses,
  Savings = monthly_savings,
  Bank = inv_bank,
  Bonds = inv_bonds,
  Stocks = inv_stocks,
  TotalInvestment = inv_bank + inv_bonds + inv_stocks,
  Debt = debt,
  NetWorth = inv_bank + inv_bonds + inv_stocks - debt
)

new("SimulationResult", user = user, data = df)
})