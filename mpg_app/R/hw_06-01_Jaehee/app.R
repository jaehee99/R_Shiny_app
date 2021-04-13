# Jaehee Lee
library(shiny)
library(ggplot2)

ui <- fluidPage(
    varSelectInput("var1", "X Variable", data = mpg, selected = "cty"),
    varSelectInput("var2", "Y Variable", data = mpg, selected = "hwy"),
    varSelectInput("var3", "Color Variable", data = mpg, selected = "class"),
    plotOutput("plot")
)

server <- function(input, output) {
    output$plot <- renderPlot({
        ggplot(mpg, aes(x = !!input$var1, y = !!input$var2, color = !!input$var3)) +
            geom_point()
    })
}

shinyApp(ui, server)