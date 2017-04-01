library(shiny)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output) {

  projectRetirement <- reactive({ 
	yearsObserving = input$n.obs # lenght of time we want to look at
	monthsObserving = 12 * yearsObserving # Convert length of time to months

	ageNow = input$age.now # current age
	delayYears = input$years.wait # years until retirement
	retireAgeYears = ageNow + delayYears # current age + years until retirement
	ageMonths= ageNow * 12 # current age in months
	retireAgeMonths = retireAgeYears * 12 # retirement age in months
	monthsToRetirement = retireAgeMonths - ageMonths # 24
	
	numSims = input$n.sim # number of simulations we want to perform

	totalPension = input$total.pension  # Total Savings in period

	# PENSION MATRIX
	pensionAllocation = totalPension / monthsObserving # pension lump sum per month
	pensionMatrix = matrix(pensionAllocation,monthsObserving,numSims)
	
	monthlyWithdrawals = input$monthly.withdrawals
	
	ageSeq = seq(from=ageMonths, by=1, length.out=monthsObserving)
	ageVec = matrix(ageSeq)
	ageVecYears = ageVec/12

	ssAmount = input$social.security
	ssStartYear = retireAgeYears
	ssStartMonth = ssStartYear * 12
	ssStartDelta =  ssStartMonth - ageMonths
	
	if (ssStartDelta < 0 ) { ssStartDelta = 0 } # not dealing with negative time
	ssMatrixA = matrix(0, ssStartDelta, numSims)  # two matrices - one before SS starts
	ssMatrixB = matrix(ssAmount, (monthsObserving-ssStartDelta), numSims)  # one matrix for social security time
	ssMatrix = rbind(ssMatrixA, ssMatrixB)

	yearlyCapitalContribs = input$capital.contribs
	yearsContributing2capital = delayYears

	if ( (yearlyCapitalContribs > 0) & (yearsContributing2capital > 0) ) { #assuming that capital contribution time finite
		monthlyCapitalContribs = yearlyCapitalContribs / 12
		monthsContributing2capital = yearsContributing2capital * 12
		capitalContribMatrixA = matrix(monthlyCapitalContribs, monthsContributing2capital, numSims) 
		capitalContribMatrixB = matrix(0, (monthsObserving-monthsContributing2capital), numSims) 
		capitalContribMatrix = rbind(capitalContribMatrixA, capitalContribMatrixB)
	} else {
		capitalContribMatrix = matrix(0, monthsObserving, numSims)
	}
	startCapital = pensionMatrix + ssMatrix - capitalContribMatrix

	# monthly Investment and Inflation assumptions
	annualMeanReturn = input$annual.mean.return/100
	monthlyReturnMean = annualMeanReturn / 12
	annualReturnStdDev = input$annual.ret.std.dev/100
	monthlyReturnStdDev = annualReturnStdDev / sqrt(12)

	# simulate Returns
	investReturnsMatrix = matrix(0, monthsObserving, numSims)
	investReturnsMatrix[] = rnorm(monthsObserving * numSims, mean = monthlyReturnMean, sd = monthlyReturnStdDev)
	
	annualInflation = input$annual.inflation/100
	monthlyInflation = annualInflation / 12
	annualInflationStdDev = input$annual.inf.std.dev/100
	monthlyInflationStdDev = annualInflationStdDev / sqrt(12)
	
	# simulate effect of inflation
	inflationMatrix = matrix(0, monthsObserving, numSims)
	inflationMatrix[] = rnorm(monthsObserving * numSims, mean = monthlyInflation, sd = monthlyInflationStdDev)

	for (j in 1:(monthsObserving-1)) {
	    startCapital[j + 1, ] =  (startCapital[j, ] * (1 + investReturnsMatrix[j, ] - inflationMatrix[j, ])) 
	}

		for (j in 1:(monthsObserving)) {
	    startCapital[j, ] =  startCapital[j, ] - monthlyWithdrawals 
	}
	
	#startCapital[ startCapital < 0 ] = NA # once nav is below 0 => run out of money
	Retirement = startCapital 
	Retirement = cbind(ageVecYears,Retirement)
	
	output$documentationText = renderText({'... projecting retirement assets over time ...\n'})
	output$sourceText = renderText({"Idea by Pierre Chretien, adapted to local scenario by Jonathan Mallia"})

	return(Retirement)
  })
  
#   output$distPlot <- renderPlot({
# 	Retirement = projectRetirement()
# 	layout(matrix(c(1,2,1,3),2,2))
# 
# 	matplot(x = Retirement[ , 1], y = Retirement[ , -1 ], type = 'l', las = 1, ylab='', xlab='Age')
#   })
	
    output$distPlot <- renderPlotly({
      	Retirement = projectRetirement()
    
      	ds_ret <-as.data.frame(Retirement)
      	
      	gg <- melt(ds_ret,id=1, variable.name="Sim", value.name="Val")
      	
      	ggp <- ggplot(data = gg, aes(x = V1, y=Val,color=Sim)  ) +
      	    geom_line(show.legend = F) +
      	    labs(title="Simulations", x="Age", y="Income", colour = "", fill="") +
      	    theme(axis.text.x = element_text(size=8), 
      	          axis.text.y = element_text(size=8), 
      	          legend.text=element_text(size=0), 
      	          #legend.key.size = unit(1, "cm"), 
      	          plot.title=element_text(size=16, vjust=3), 
      	          plot.margin = unit(c(1,0.9,1,1), "cm"), 
      	          axis.text=element_text(size=10), 
      	          axis.title = element_text(size=12), 
      	          axis.title.y=element_text(margin=margin(r = 13)), 
      	          axis.title.x=element_text(margin = margin(t = 10)))
      	
      	ggplotly(ggp) %>% 
      	    layout(showlegend = FALSE) %>% 
      	    layout(autosize = F, width = 1000, height = 1000)
    })
  
})
