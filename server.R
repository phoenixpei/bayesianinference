library(shiny)

# Define a server for the Shiny app
shinyServer(function(input, output) {

    lmWeighted <- function(x, y, alpha) {
        w <- 1/x**(2*alpha)
        return(lm(y ~ x, data.frame(x = x, y = y), weights = w))
    }

    lmWeightedBounds <- function(x, model, alpha) {
        sds   <- sd(model$residuals/model$fitted.values**alpha)*model$fitted.values
        upper <- predict(model, x) + sds
        lower <- predict(model, x) - sds
        return(list(u = upper, l = lower))
    }

    m <- 1000
    c <- 100
    x <- c(.05, .1, .5, 1, 2, 5, 10)

    err <- reactive({
        input$resample
        return(rnorm(length(x)))
    })

    values <- reactive({
        err <- err()*input$std/100
        y <- (m*x + c)*(1 + err)
        return(list(err = err,
                    y   = y))
    })

    # Fill in the spot we created for a plot
    output$plot <- renderPlot({

        y <- values()$y

        model1 <- lm(y ~ x, data.frame(x = x, y = y))
        w <- 1/x**2
        model2 <- lm(y ~ x, data.frame(x = x, y = y), weights = w)

        xlim <- input$plot.range

        # y lims
        ylim <- c(min(predict(model1, data.frame(x=xlim[1])) - summary(model1)$sigma,
                      predict(model2, data.frame(x=xlim[1])) - sd(model2$residuals/model2$fitted.values)*predict(model2, data.frame(x=xlim[1]))),
                  max(predict(model1, data.frame(x=xlim[2])) + summary(model1)$sigma,
                      predict(model2, data.frame(x=xlim[2])) + sd(model2$residuals/model2$fitted.values)*predict(model2, data.frame(x=xlim[1]))))

        plot(x, y, xlim = xlim, ylim = ylim, pch = 4,  ylab = 'Area', xlab = 'Concentration')

        lines(x, m*x+c, col = 'green', lwd = 2)

        lines(x, predict(model1), col = rgb(1, 0, 0, 1))
        lines(x, predict(model1) + summary(model1)$sigma, col = rgb(1, 0, 0, .5), lty = 2)
        lines(x, predict(model1) - summary(model1)$sigma, col = rgb(1, 0, 0, .5), lty = 2)
        abline(h = 0)
        abline(v = 0)

        lines(x, predict(model2), col = rgb(0, 0, 1, 1))
        lines(x, predict(model2) + sd(model2$residuals/model2$fitted.values)*model2$fitted.values, col = rgb(0, 0, 1, .5), lty = 3)
        lines(x, predict(model2) - sd(model2$residuals/model2$fitted.values)*model2$fitted.values, col = rgb(0, 0, 1, .5), lty = 3)
    })

    ### YOUR DATA ########################################################

    yourDataValidation <- reactive({
        return(read.csv(input$yourDataFileValidation[1 , 'datapath']))
    })

    yourDataCalibration <- reactive({
        return(read.csv(input$yourDataFileCalibration[1 , 'datapath']))
    })

    yourDataMeasurements <- reactive({
        return(read.csv(input$yourDataFileMeasurements[1 , 'datapath']))
    })

    varNames <- reactive({
        return(colnames(yourDataValidation()))
    })

    output$selectX <- renderUI({
        if (is.null(input$yourDataFileValidation)) {
            return(NULL)
        }
        selectInput('selectValidationX',
                    label    = h5('Independent Variable [X]:'),
                    choices  = varNames(),
                    selected = varNames()[1])
    })

    output$selectY <- renderUI({
        if (is.null(input$yourDataFileValidation)) {
            return(NULL)
        }
        selectInput('selectValidationY',
                    label   = h5('Dependent Variable [Y]:'),
                    choices = varNames(),
                    selected = varNames()[2])
    })

    yourDataValidationX <- reactive({
        return(as.numeric(yourDataValidation()[ , input$selectValidationX]))
    })

    yourDataCalibrationX <- reactive({
        return(as.numeric(yourDataCalibration()[ , input$selectValidationX]))
    })

    output$selectPlotRangeValidation <- renderUI({
        if (is.null(input$yourDataFileValidation)) {
            return(NULL)
        } else {
            xlims <- c(.9*min(yourDataValidationX()), 1.01*max(yourDataValidationX()))
            sliderInput('plotRangeValidation', h5('Select Plot Range:'),
                        min = xlims[1], max = xlims[2], value = xlims,
                        step = (xlims[2] - xlims[1])/1000)
        }
    })

    output$selectPlotRangeCalibration <- renderUI({
        if (is.null(input$yourDataFileCalibration)) {
            return(NULL)
        } else {
            xlims <- c(.9*min(yourDataCalibrationX()),
                       1.01*max(yourDataCalibrationX()))
            sliderInput('plotRangeCalibration', h5('Select Plot Range:'),
                        min = xlims[1], max = xlims[2], value = xlims,
                        step = (xlims[2] - xlims[1])/1000)
        }
    })

    yourDataValidationY <- reactive({
        return(as.numeric(yourDataValidation()[ , input$selectValidationY]))
    })

    yourDataCalibrationY <- reactive({
        return(as.numeric(yourDataCalibration()[ , input$selectValidationY]))
    })

    fittedPower <- reactive({
        if (is.null(input$yourDataFileValidation)) {
            return(1.0)
        } else {
            .F <- function(alpha, x, y) {

                model <- lmWeighted(x, y, alpha)
                absres <- abs(model$residuals/model$fitted.values**alpha)
                return(cor(x, absres)**2)
            }
            alphaOpt <- optimize(.F, interval = c(.0, 2.0),
                                 x = yourDataValidationX(),
                                 y = yourDataValidationY())$minimum
            return(alphaOpt)
        }
    })

    output$alpha <- renderText({
        fittedPower()
    })

    modelStandard <- reactive({
        if (is.null(input$yourDataFileValidation)) {
            return(NULL)
        } else {
            x <- yourDataValidationX()
            y <- yourDataValidationY()
            model <- lm(y ~ x, data.frame(x = x, y = y))
            return(model)
        }
    })

    modelPower <- reactive({
        if (is.null(input$yourDataFileValidation)) {
            return(NULL)
        } else {
            x <- yourDataValidationX()
            y <- yourDataValidationY()
            alpha  <- fittedPower()
            model <- lmWeighted(x, y, alpha)
            return(model)
        }
    })

    output$plotValidation <- renderPlot({
        if (is.null(input$yourDataFileValidation)) {
            return(NULL)
        } else {
            x <- yourDataValidationX()
            y <- yourDataValidationY()

            m0 <- modelStandard()

            alpha  <- fittedPower()
            m1 <- modelPower()

            xlim <- input$plotRangeValidation

            ylim <- c(min(predict(m0, data.frame(x=xlim[1])) - summary(m0)$sigma,
                          predict(m1, data.frame(x=xlim[1])) - sd(m1$residuals/m1$fitted.values**alpha)*predict(m1, data.frame(x=xlim[1]))**alpha),
                      max(predict(m0, data.frame(x=xlim[2])) + summary(m0)$sigma,
                          predict(m1, data.frame(x=xlim[2])) + sd(m1$residuals/m1$fitted.values**alpha)*predict(m1, data.frame(x=xlim[1]))**alpha))

            plot(x, y, xlim = xlim, ylim = ylim, pch = 4,  ylab = 'Y', xlab = 'X', las = 1)

            lines(x, predict(m0), col = rgb(1, 0, 0, 1))
            lines(x, predict(m0) + summary(m0)$sigma, col = rgb(1, 0, 0, .5), lty = 2)
            lines(x, predict(m0) - summary(m0)$sigma, col = rgb(1, 0, 0, .5), lty = 2)
            abline(h = 0)
            abline(v = 0)

            lines(x, predict(m1), col = rgb(0, 0, 1, 1))
            lines(x, predict(m1) + sd(m1$residuals/m1$fitted.values**alpha)*m1$fitted.values**alpha, col = rgb(0, 0, 1, .5), lty = 3)
            lines(x, predict(m1) - sd(m1$residuals/m1$fitted.values**alpha)*m1$fitted.values**alpha, col = rgb(0, 0, 1, .5), lty = 3)
        }
    })

    output$plotCalibration <- renderPlot({
        if (is.null(input$yourDataFileValidation)) {
            return(NULL)
        } else {
            x     <- yourDataCalibrationX()
            y     <- yourDataCalibrationY()

            m0    <- modelStandard() # only needed for error!
            mm0   <- lm(y ~ x, data.frame(x = x, y = y))

            alpha <- fittedPower()
            m1    <- modelPower() # only needed for errors!
            mm1   <- lmWeighted(x, y, alpha)

            xlim  <- input$plotRangeCalibration

            ylim  <- c(min(predict(mm0, data.frame(x=xlim[1])) - summary(m0)$sigma,
                           predict(mm1, data.frame(x=xlim[1])) - sd(m1$residuals/m1$fitted.values**alpha)*predict(mm1, data.frame(x=xlim[1]))**alpha),
                       max(predict(mm0, data.frame(x=xlim[2])) + summary(m0)$sigma,
                           predict(mm1, data.frame(x=xlim[2])) + sd(m1$residuals/m1$fitted.values**alpha)*predict(mm1, data.frame(x=xlim[1]))**alpha))

            plot(x, y, xlim = xlim, ylim = ylim, pch = 4,  ylab = 'Y', xlab = 'X', las = 1)

            lines(x, predict(mm0), col = rgb(1, 0, 0, 1))
            lines(x, predict(mm0) + summary(m0)$sigma, col = rgb(1, 0, 0, .5), lty = 2)
            lines(x, predict(mm0) - summary(m0)$sigma, col = rgb(1, 0, 0, .5), lty = 2)
            abline(h = 0)
            abline(v = 0)

            lines(x, predict(mm1), col = rgb(0, 0, 1, 1))
            lines(x, predict(mm1) + sd(m1$residuals/m1$fitted.values**alpha)*mm1$fitted.values**alpha, col = rgb(0, 0, 1, .5), lty = 3)
            lines(x, predict(mm1) - sd(m1$residuals/m1$fitted.values**alpha)*mm1$fitted.values**alpha, col = rgb(0, 0, 1, .5), lty = 3)
        }
    })

    output$plotResidualsValidation <- renderPlot({
        if (is.null(input$yourDataFileValidation)) {
            return(NULL)
        } else {
            plot(yourDataValidationX(),
                 abs(modelPower()$residuals/modelPower()$fitted.values**fittedPower()),
                 ylab = 'Standardized Absolute Residuals',
                 xlab = 'X',
                 las  = 1)
        }
    })

    measurements <- reactive({
        x     <- yourDataCalibrationX()
        y     <- yourDataCalibrationY()
        alpha <- fittedPower()
        m1    <- modelPower()
        mm1   <- lmWeighted(x, y, alpha)
        yM <- yourDataMeasurements()[ , input$selectValidationY]
        xM <- (yM - mm1$coefficients[1])/mm1$coefficients[2]
        stdErr <- sd(m1$residuals/m1$fitted.values**alpha)*yM**alpha/mm1$coefficients[2]

        return(list(yM = yM, xM = xM, stdErr = stdErr))
    })

    output$view <- renderTable({
        tmp <- measurements()
        data.frame(Y=tmp$yM, X = tmp$xM, Std=tmp$stdErr)
    })

})
