library(shiny)

# Define a server for the Shiny app
shinyServer(function(input, output) {

    # Fill in the spot we created for a plot
    output$plot <- renderPlot({

        n <- as.integer(input$n.sample)
        mu <- as.numeric(input$mean)
        s <- as.numeric(input$std)

        set.seed(as.numeric(input$seed))

        observations <- rnorm(n)

        x <- seq(input$plotRange[1], input$plotRange[2], length.out = 1000)

        # likelihood
        y.like <- dnorm(x)

        # prior
        y.prior <- dnorm(x, mean = mu, sd = s)

        # sampling distribution of the mean + posterior
        if (n > 1) {
            y.smpl <- dnorm(x, mean = 0, sd = 1/sqrt(n))

            sn   <- sqrt(1/(n/1^2 + 1/s^2))
            mun <- (mu/s^2 + sum(observations)/1^2)*sn^2
            y.post <- dnorm(x, mean = mun, sd = sn)

            y.emp.smpl <- dnorm(x, mean = mean(observations), sd = sd(observations)/sqrt(n))
        }

        if (n > 1) {
            y.lims <- c(.0, max(y.like, y.prior, y.smpl, y.post, y.emp.smpl))
        } else {
            y.lims <- c(.0, max(y.like, y.prior))
        }
        plot(x, y.prior, ylim = y.lims, col = 'skyblue', type = 'l',
             las = 1, ylab = 'PDF', lty = 2)
        if ('Likelihood' %in% input$selectPlots) {
            lines(x, y.like, col = 'blue', lty = 2)
        }
        if (n > 1) {
            if ('Posterior' %in% input$selectPlots) {
                lines(x, y.post, col     = 'darkblue')
            }
            if ('Sampling Distribution' %in% input$selectPlots) {
                lines(x, y.smpl, col     = 'green')
            }
            if ('Empirical Sampling Distribution' %in% input$selectPlots) {
                lines(x, y.emp.smpl, col = 'turquoise')
            }
        }
        sel <- input$selectPlots
        sel['Prior'] <- 'Prior'
        sel <- as.character(sel)
        cols <- list('Prior'                             = 'skyblue',
                     'Likelihood'                        = 'blue',
                     'Posterior'                         = 'darkblue',
                     'Sampling Distribution'             = 'green',
                     'Empirical Sampling Distribution'   = 'turquoise')
        legend('topright', legend = sel, col = as.character(cols[sel]), lty = 1)
    })
})
