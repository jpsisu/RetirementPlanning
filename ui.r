library(shiny)
library(plotly)

# Define UI for application that plots random distributions 

shinyUI(
    fluidPage( #have two open parens

        titlePanel("Planning Income and Expenditure for Retirement"),
    
        sidebarLayout(position="right",
    	    sidebarPanel(
        	    h4('Adjust sliders to simulate a retirement scenario\n'),
        
        	    sliderInput("age.now", "Current Age:", min = 55, max = 75, value = 60),
        	    sliderInput("n.obs", "Projection in years:", min = 0, max = 40, value = 30),
        	    sliderInput("years.wait", "Years Until Pension:", min = 0, max = 25, value = 2, step = 1),
        		sliderInput("social.security", "Income from Pension (annual):", min = 10000, max = 30000, value = 11000, step = 1000, sep = ",", pre = "Eur"),
        		sliderInput("total.pension", "Total Savings:", min = 20000, max = 700000, value = 90000, step = 2000, sep = ",", pre = "Eur"),
        		sliderInput("capital.contribs", "Capital Contributions (annual):", min = 0, max = 5000, value = 1000, step = 100, sep = ",", pre = "Eur"),
        		sliderInput("annual.mean.return", "Investment Return (annual %):", min = 0.0, max = 30.0, value = 5.0, step = 0.5, sep = ",", post = "%"),
        		sliderInput("annual.ret.std.dev", "Investment Volatility (annual %):", min = 0.0, max = 25.0, value = 5.0, step = 0.1, sep = ",", post = "%"), 
        		sliderInput("annual.inflation", "Inflation % (annual):", min = 0, max = 10, value = 2.5, step = 0.1, sep = ",", post = "%"),
        		sliderInput("annual.inf.std.dev", "Inflation Volatility % (annual):", min = 0.0, max = 5.0, value = 1.5, step = 0.05, sep = ",", post = "%"),
        		sliderInput("monthly.withdrawals", "Capital Withdrawals (monthly):", min = 0, max = 1500, value = 300, step = 100, sep = ",", pre = "Eur"),
        		sliderInput("n.sim", "Number of Simulations:", min = 1, max = 70, value = 20, step = 1),
        		
        		helpText("Idea by Pierre Chretien, adapted and modified by Jonathan Mallia") 
        	), #sidebarPanel
        
        	mainPanel( 
        	    tabsetPanel(id='mytab',
                    tabPanel('Chart', value='doc', plotlyOutput("distPlot")),
                    tabPanel('Documentation', 
                         value='doc', 
                         tags$div(class="header", 
                                  tags$br(),
                                  tags$h4("Current Age"),
                                  tags$p("Specify your current age"),
                                  tags$h4("Projection in years"),
                                  tags$p("Specify the length of retirement span"),
                                  tags$h4("Years Until Pension"),
                                  tags$p("Specify the number of years until pension income will initiate"),
                                  tags$h4("Total Savings"),
                                  tags$p("The total funds currently accumulated"),
                                  tags$h4("Capital Contributions (annual)"),
                                  tags$p("The total yearly contributions to be paid until retirement"),
                                  tags$h4("Investment Return (annual %)"),
                                  tags$p("The expected % return from investments"),
                                  tags$h4("Investment Volatility (annual %)"),
                                  tags$p("Specify the level of volality of the investments"),
                                  tags$h4("Inflation % (annual)"),
                                  tags$p("Specify the projected inflation"),
                                  tags$h4("Inflation Volatility % (annual)"),
                                  tags$p("Specify the level of volality of inflation"),
                                  tags$h4("Capital Withdrawals (monthly)"),
                                  tags$p("Specify the monthly withdrawals"),
                                  tags$h4("Number of Simulations"),
                                  tags$p("The number of simulations to be generated by the model"),
                                  
                                  tags$br(),
                                  tags$br(),
                                  
                                  tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
                         )
                    )
        	    )
        	) #mainPanel
        ) #sidebarLayout
    ) #fluidPage
) #shinyUI
