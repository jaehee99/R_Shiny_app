# Jaehee Lee

library(shiny)
library(ggplot2)
library(ggthemes)
plot_choices = c("Density Plot", "Histogram", "Fruquency Polygon")
ui <- fluidPage(
    varSelectInput("var1", "X Variable", data = mtcars),
    plotOutput("plot"), 
    radioButtons("Choices", "Choose a plot type", choices = plot_choices),
    
)

server <- function(input, output) {
    output$plot <- renderPlot({
        ggplot(mtcars, aes(x = !!input$var1)) + 
            switch( input$Choices,
                "Histogram"= geom_histogram(),
                "Density Plot"= geom_density(), 
                "Fruquency Polygon" = geom_freqpoly()
                
            ) +
            theme_bw()
        

    })
}

shinyApp(ui = ui, server = server)