library(shiny)

shinyUI(

    # Use a fluid Bootstrap layout
    navbarPage(h1('Calibrating HPLC Output'),

        tabPanel(h3('Simulation'), # =============================================

            sidebarLayout(

                sidebarPanel(

                    sliderInput('std', h5('Std. of Relative Error [%]:'),
                                min = 1, max = 50, value = 5, step = 1),

                    actionButton('resample', h5('Generate New Random Data')),

                    sliderInput('plot.range', h5('Select Plot Range:'),
                                min = 0, max = 10, value = c(0, 10), step = 0.01,
                                width = '100%')
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

        tabPanel(h3('Your Data'), # ==============================================

             sidebarLayout(

                 sidebarPanel(

                     fileInput('yourDataFileValidation',
                               label = h5('Validation Data:')),

                     fileInput('yourDataFileCalibration',
                               label = h5('Calibration Data:')),

                     fileInput('yourDataFileMeasurements',
                               label = h5('Measurement Data:')),

                     hr(),

                     uiOutput('selectX'),

                     uiOutput('selectY'),

                     hr(),

                     downloadButton('downloadData', 'Download Processed Measurments')
                ),

                mainPanel(tabsetPanel(type = 'tabs',

                    tabPanel('Validation',

                        plotOutput('plotValidation'),

                        wellPanel(
                            uiOutput('selectPlotRangeValidation')
                        ),

                        h4('Power Law Exponent:'),

                        p('The errors will not be exactly proportional
                        to the measured value, as larger peaks become relatively
                        more precise. This is accounted for by searching a value
                        $alpha$ between 0 and 2, that best decorrelates [TODO switch
                        to distance correlation] the residuals from their location.
                        A value of: '),

                        textOutput('alpha', container = p),

                        p('This is a reasonable way of finding the governing
                        power law. [TODO: Stabelize via CV]'),

                        h4('Scatterplot of Standardized Absolute Residuals:'),

                        p('The scatterplot should not exihibt obvious trends in
                        the distribution of the residuals. Actually these
                        normalized residuals should be $iid$ and uncorrelated
                        with $X$.'),

                        plotOutput('plotResidualsValidation'),

                        h4('Normal Q-Q Plot for the Standardized Absolute Residuals'),

                        p('The quantile-quantile plot should be as close to a
                        straight line as possible if the normality assumption
                        holds true. If there are larger deviations in the center
                        the model can hardly be justified. Deviations in the
                        outer tails are not as severe and rather effect the
                        meaningfulness of the standard error. A sigmoidal
                        shape indicates smaller sample than theoretical quantiles
                        and thus the stanard errors will be rather conservative.'),

                        plotOutput('plotNormalQQValidation')
                    ),

                    tabPanel('Calibration',

                        plotOutput('plotCalibration'),

                        wellPanel(
                            uiOutput('selectPlotRangeCalibration')
                        )
                    ),

                    tabPanel('Measure', # ====================================

                        h4('Errorbar Plot of the Inferred Concentrations:'),

                        plotOutput('plotMeasurments'),

                        p('The errors depicted are standard errors assuming
                        normality, check the quantile-quantile plots for a first
                        idea whether or not this is appropriate.'),

                        h4('Raw and Processed Measurements:'),

                        p('The following table gives both the independent
                        variable, that is now observed, as well as the dependent
                        variable, that is now inferred. This is the data that
                        you can download on the left (.csv).'),

                        tableOutput('view')
                    )

                )

             ))

        ), # /end Your Data

        collapsable = TRUE

    )
)
