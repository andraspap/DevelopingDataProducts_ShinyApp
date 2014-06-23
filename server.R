library(UsingR)
data(mtcars)

predictorRange <- function(predictorIndex) {
	range <- c(min(mtcars$disp),max(mtcars$disp))
	if(2 == input$id1) {
		range <- c(min(mtcars$hp),max(mtcars$hp))
	}
	if(3 == input$id1) {
		range <- c(min(mtcars$wt),max(mtcars$wt))
	}
	if(4 == input$id1) {
		range <- c(min(mtcars$qsec),max(mtcars$qsec))
	}	
	return(range)
}

predictorSelectorF <- function(predictorIndex) {
	predictor <- mtcars$disp
	if(2 == predictorIndex) {
		predictor <- mtcars$hp
	}
	if(3 == predictorIndex) {
		predictor <- mtcars$wt
	}
	if(4 == predictorIndex) {
		predictor <- mtcars$qsec
	}	
	return(predictor)
}

colNameF <- function(predictorIndex) {
	colName <- "disp"
	if(2 == predictorIndex) {
		colName <- "hp"
	}
	if(3 == predictorIndex) {
		colName <- "wt"
	}
	if(4 == predictorIndex) {
		colName <- "qsec"
	}	
	return(colName)
}

lmBothF <- function(predictor) {
	return(lm(mpg ~ predictor + am + predictor * am, mtcars))
}

predictF <- function(modelFit, predictor) {
	c(modelFit$coef[1] + predictor * modelFit$coef[2],
	  modelFit$coef[1] + modelFit$coef[3] + predictor * (modelFit$coef[2] + modelFit$coef[4]))	
}

shinyServer(
	function(input, output) {
		predictor <- reactive(predictorSelectorF(input$id1))
		colName <- reactive(colNameF(input$id1))
		lmBoth <- reactive(lmBothF(predictor()))
		predict <- reactive(predictF(lmBoth(),input$id2))
		
		output$plot <- renderPlot({
			
			
			title <- "MPG v.s. Displacement"
			xlab <- "Displacement (cu in.)"

			if(2 == input$id1) {
				title <- "MPG v.s. Horse Power"
				xlab <- "Horse power (hp)"
			}
			if(3 == input$id1) {
				title <- "MPG v.s. Weight"
				xlab <- "Weight (lb/1000)"
			}
			if(4 == input$id1) {
				title <- "MPG v.s. 1/4 mile time"
				xlab <- "1/4 mile time (sec)"
			}
			
			plot(mpg ~ predictor(),mtcars,col = (mtcars$am == 0)*2 + 2,main=title,xlab=xlab) # col 2(red) manual 4(blue) automatic			
			abline(c(lmBoth()$coef[1],lmBoth()$coef[2]),col='blue')		
			abline(c(lmBoth()$coef[1] + lmBoth()$coef[3],lmBoth()$coef[2] + lmBoth()$coef[4]),col='red')
			
			input$goButton
			isolate(points(c(predict()[1]) ~ c(input$id2), col='blue', pch = 16))
			input$goButton
			isolate(points(c(predict()[2]) ~ c(input$id2), col='red', pch = 16))
		})
		
		output$oid1 <- renderPrint(input$id1)
		output$oid2 <- renderPrint(input$id2)
		output$predictedValue <- renderText({input$goButton 
					             isolate(sprintf("For %s %g: automatic is: %g mpg standard is %g mpg",
							      colName(),
							      input$id2, 
							      predict()[1],
							      predict()[2]))
							      })
	}
)
