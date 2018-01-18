paramNames <- c("start_capital", "annual_mean_return", "annual_ret_std_dev",
                "annual_inflation", "annual_inf_std_dev", "monthly_withdrawals", "n_obs",
                "n_sim")

simulate_nav <- function(start_capital = 2000000, annual_mean_return = 5.0,
                         annual_ret_std_dev = 7.0, annual_inflation = 2.5,
                         annual_inf_std_dev = 1.5, monthly_withdrawals = 1000,
                         n_obs = 20, n_sim = 200) {
  #-------------------------------------
  # Inputs
  #-------------------------------------
  
  # Initial capital
  start.capital = start_capital
  
  # Investment
  annual.mean.return = annual_mean_return / 100
  annual.ret.std.dev = annual_ret_std_dev / 100
  
  # Inflation
  annual.inflation = annual_inflation / 100
  annual.inf.std.dev = annual_inf_std_dev / 100
  
  # Withdrawals
  monthly.withdrawals = monthly_withdrawals
  
  # Number of observations (in Years)
  n.obs = n_obs
  
  # Number of simulations
  n.sim = n_sim
  
  
  #-------------------------------------
  # Simulation
  #-------------------------------------
  
  # number of months to simulate
  n.obs = 12 * n.obs
  
  # monthly Investment and Inflation assumptions
  monthly.mean.return = annual.mean.return / 12
  monthly.ret.std.dev = annual.ret.std.dev / sqrt(12)
  
  monthly.inflation = annual.inflation / 12
  monthly.inf.std.dev = annual.inf.std.dev / sqrt(12)
  
  # simulate Returns
  monthly.invest.returns = matrix(0, n.obs, n.sim)
  monthly.inflation.returns = matrix(0, n.obs, n.sim)
  
  monthly.invest.returns[] = rnorm(n.obs * n.sim, mean = monthly.mean.return, sd = monthly.ret.std.dev)
  monthly.inflation.returns[] = rnorm(n.obs * n.sim, mean = monthly.inflation, sd = monthly.inf.std.dev)
  
  # simulate Withdrawals
  nav = matrix(start.capital, n.obs + 1, n.sim)
  for (j in 1:n.obs) {
    nav[j + 1, ] = nav[j, ] * (1 + monthly.invest.returns[j, ] - monthly.inflation.returns[j, ]) - monthly.withdrawals
  }
  
  # once nav is below 0 => run out of money
  nav[ nav < 0 ] = NA
  
  # convert to millions
  nav = nav / 1000000
  
  return(nav)
}

plot_nav <- function(nav) {
  
  layout(matrix(c(1,2,1,3),2,2))
  
  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))
  
  # plot all scenarios
  matplot(nav,
          type = 'l', lwd = 0.5, lty = 1, col = 1:5,
          xlab = 'Months', ylab = 'Millions',
          main = 'Projected Value of Initial Capital')
  
  # plot % of scenarios that are still paying
  p.alive = 1 - rowSums(is.na(nav)) / ncol(nav)
  
  plot(100 * p.alive, las = 1, xlab = 'Months', ylab = 'Percentage Paying',
       main = 'Percentage of Paying Scenarios', ylim=c(0,100))
  grid()
  
  
  last.period = nrow(nav)
  
  # plot distribution of final wealth
  final.nav = nav[last.period, ]
  final.nav = final.nav[!is.na(final.nav)]
  
  if(length(final.nav) ==  0) return()
  
  plot(density(final.nav, from=0, to=max(final.nav)), las = 1, xlab = 'Final Capital',
       main = paste0('Distribution of Final Capital\n', 100 * p.alive[last.period], '% are still paying'))
  grid()
}

# Define server logic required to generate and plot a random distribution
#
# Idea and original code by Pierre Chretien
# Small updates by Michael Kapler
#

library(shiny)
#########################
renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             sliderInput(paste0(prefix, "_", "n_obs"), "Number of observations (in Years):", min = 0, max = 40, value = 20),
             sliderInput(paste0(prefix, "_", "start_capital"), "Initial capital invested :", min = 100000, max = 10000000, value = 2000000, step = 100000, pre = "$", sep = ","),
             sliderInput(paste0(prefix, "_", "annual_mean_return"), "Annual investment return (in %):", min = 0.0, max = 30.0, value = 5.0, step = 0.5),
             sliderInput(paste0(prefix, "_", "annual_ret_std_dev"), "Annual investment volatility (in %):", min = 0.0, max = 25.0, value = 7.0, step = 0.1)
      ),
      column(6,
             sliderInput(paste0(prefix, "_", "annual_inflation"), "Annual inflation (in %):", min = 0, max = 20, value = 2.5, step = 0.1),
             sliderInput(paste0(prefix, "_", "annual_inf_std_dev"), "Annual inflation volatility. (in %):", min = 0.0, max = 5.0, value = 1.5, step = 0.05),
             sliderInput(paste0(prefix, "_", "monthly_withdrawals"), "Monthly capital withdrawals:", min = 1000, max = 100000, value = 10000, step = 1000, pre = "$", sep = ","),
             sliderInput(paste0(prefix, "_", "n_sim"), "Number of simulations:", min = 0, max = 2000, value = 200)
      )
    ),
    p(actionButton(paste0(prefix, "_", "recalc"),
                   "Re-run simulation", icon("random")
    ))
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

  fluidRow(
    column(6, tags$h3("Scenario A")),
    column(6, tags$h3("Scenario B"))
  ),
  fluidRow(
    column(6, renderInputs("a")),
    column(6, renderInputs("b"))
  ),
  fluidRow(
    column(6,
           plotOutput("a_distPlot", height = "600px")
    ),
    column(6,
           plotOutput("b_distPlot", height = "600px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params
  }
  
  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  navA <- reactive(do.call(simulate_nav, getParams("a")))
  navB <- reactive(do.call(simulate_nav, getParams("b")))
  
  # Expression that plot NAV paths. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  output$a_distPlot <- renderPlot({
    plot_nav(navA())
  })
  output$b_distPlot <- renderPlot({
    plot_nav(navB())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

