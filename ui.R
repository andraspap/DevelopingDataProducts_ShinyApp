library(UsingR)
data(mtcars)
predictorRange <<- list(c(min(mtcars$disp),max(mtcars$disp)),
			c(min(mtcars$hp),max(mtcars$hp)),
			c(min(mtcars$wt),max(mtcars$wt)),
			c(min(mtcars$qsec),max(mtcars$qsec)))

shinyUI(pageWithSidebar(
	headerPanel("Milage Calculator"),
	sidebarPanel(
		radioButtons("id1", "Choose predictor", c("Displacement" = "1",
					                  "Horse Power" = "2",
					                  "Weitght" = "3",
					                  "1/4 mile time" = "4")),
		numericInput("id2","Predictor value",(predictorRange[[1]][1] + predictorRange[[1]][2])/2.0,
			     			     min=predictorRange[[1]][1],
			                             max=predictorRange[[1]][2],
			     		             step=0.0000001),
		actionButton('goButton','Go!')
	),
	mainPanel(
		plotOutput('plot'),
		h4('Predicted Milage values:'),
		verbatimTextOutput("predictedValue")
	)
))

