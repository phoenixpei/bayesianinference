library(shiny)

shinyUI(

    # Use a fluid Bootstrap layout
    fluidPage(

        titlePanel('Conjugate Bayesian Inference for Normal Likelihood'),

        sidebarLayout(

            sidebarPanel(

                sliderInput('n.sample', h4('Number of Samples'),
                            min = 0, max = 100, value = 0, step = 1,
                            animate = animationOptions(interval = 500, loop = TRUE)),

                textInput('mean', label = h4('Prior Mean'), value = 0),

                textInput('std', label = h4('Prior Standard Deviation'), value = 1),

                textInput('seed', label = h4('Random Seed'), value = pi),

                checkboxGroupInput('selectPlots', label = h4('Select Plots'),
                                   choices = list('Likelihood' = 'Likelihood',
                                                  'Posterior' = 'Posterior',
                                                  'Sampling Distribution' = 'Sampling Distribution',
                                                  'Empirical Sampling Distribution' = 'Empirical Sampling Distribution'),
                                   selected = c('Posterior')),

                sliderInput("plotRange", "Plot Range",
                            min = -5, max = 5, step = .1, value = c(-3, 3))

            ),

            # Create a spot for the barplot
            mainPanel(
                plotOutput('plot')
            )

        )
    )
)
