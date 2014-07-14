library(shiny)

shinyUI(

    # Use a fluid Bootstrap layout
    navbarPage('Calibrating HPLC Output',

        tabPanel('Simulation', # =============================================

            sidebarLayout(

                sidebarPanel(

                    sliderInput('std', h5('Standard Deviation of Relative Error [%]:'),
                                min = 1, max = 50, value = 5, step = 1),

                    actionButton('resample', h5('Generate New Random Data')),

                    sliderInput('plot.range', h5('Select Plot Range:'),
                                min = 0, max = 10, value = c(0, 10), step = 0.01)
                ),

                # Create a spot for the barplot
                mainPanel(

                    h3('Calibration Models:'),

                    plotOutput('plot'),

                    h3('Explanation:'),

                    withMathJax(
                        'The plots show two different models fitted to data that was
                    generated from a linear model
                        $$A_t = (ac_t + b)\\epsilon_t$$
                    where $$\\epsilon_t\\sim\\mathcal{N}(1, \\sigma^2)$$
                    The red model was fit with standard least squares, while the
                    blue one used weighted least squares with weights proportional
                    to c. Dotted lines indicate standard errors. Note that both
                    models are wrong in the sense, that they allow negative
                    values, however the blue model is far more realistic about the
                    error margins in the lower part. This issue will be addressed by
                    a third model.'),

                    h3('Contact'),

                    'kevinkunzmann(at)gmx.net'
                )

            )
        ), # /end tabPanel Simulation

        tabPanel('Your Data', # ==============================================

             sidebarLayout(

                 sidebarPanel(

                     fileInput('yourDataFileValidation',
                               label = h5('Validation Data')),

                     fileInput('yourDataFileCalibration',
                               label = h5('Calibration Data')),

                     hr(),

                     uiOutput('selectX'),

                     uiOutput('selectY')
                ),

                mainPanel(tabsetPanel(type = 'tabs',

                    tabPanel('Validation',

                        plotOutput('plotValidation'),

                        wellPanel(
                            uiOutput('selectPlotRangeValidation')
                        ),

                        h4('Power Law Exponent:'),

                        textOutput('alpha'),

                        'This value minimizes the correlation of absolute,
                        standardized residuals with the indepentend variable
                        (concentration).',

                        h4('Scatterplot of Standardized Absolute Residuals:'),

                        plotOutput('plotResidualsValidation')
                    ),

                    tabPanel('Calibration',

                        plotOutput('plotCalibration'),

                        wellPanel(
                            uiOutput('selectPlotRangeCalibration')
                        )
                    ),

                    tabPanel('Measure', # ====================================

                        fileInput('yourDataFileMeasurements', label = 'Measurements [Y]'),

                        tableOutput('view')
                    )

                )

             ))

        ) # /end Your Data
    )
)
