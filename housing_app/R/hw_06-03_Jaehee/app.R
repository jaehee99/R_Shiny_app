library(shiny)
library(tidyverse)
library(broom)
library(ggstance)

estate <- read_csv("estate.csv", col_types = cols("AC" = col_factor(), "Pool" = col_factor(), "Highway" = col_factor()))
estate %>% 
    mutate(Price = Price/1000) %>%  
    rename("Price($K)" = Price ) -> estate

ui <- fluidPage(
    titlePanel("EDA of Estate Data"), 
    tabsetPanel(
        tabPanel("Univariate", 
                 sidebarLayout( position = "left", 
                     sidebarPanel(varSelectInput("var1", "Variable", data = estate), 
                                  checkboxInput("log", "Log_Transform?"),
                                  sliderInput("bins", 
                                              "Number of Bins?:", 
                                              min = 1, 
                                              max = 100, 
                                              value = 40 ), 
                                  numericInput("num", "Null Value", value = 0),
                                  tableOutput("t_test")
                     ), 
                     mainPanel(
                         plotOutput("distPlot")
                     )
                 )), 
        tabPanel("Bivariate",
                 sidebarLayout(position = "left", 
                               sidebarPanel(varSelectInput("var1_0", "X Variable", data = estate, selected = "Area"),
                                            checkboxInput("log_1", "Log_Transform?"),
                                            varSelectInput("var2_0", "Y Variable", data = estate, selected = "Price($K)"), 
                                            checkboxInput("log_2", "Log_Transform?"),
                                            checkboxInput("ols", "Fit OLS?")
                 ),
                 mainPanel(
                     plotOutput("plot")
                 )
                 )),
        tabPanel("Spreadsheet", 
        dataTableOutput("table")
        )
)
)
server <- function(input, output) {

    output$distPlot <- renderPlot({

     ggplot(estate, aes(x = !!input$var1))

                if (is.numeric(estate[[input$var1]])){
            if(input$log){
                ggplot(estate, aes(x = !!input$var1)) + 
                    geom_histogram(bins = input$bins)+
                    scale_x_log10()+
                    labs(x = paste("Log(", input$var1,")"))
            }else{
                ggplot(estate, aes(x = !!input$var1)) + 
                    geom_histogram(bins = input$bins)
            }
        }
        else{
                if(input$log){
                    validate(
                        need(is.double(estate[[input$var1]]),
                        "not numeric")
                    )
                }
            ggplot(estate, aes(x = !!input$var1))+ geom_bar()
            }
})
    # x1 <- rnorm(input$n1, input$mean1, input$sd1)
    # x2 <- rnorm(input$n2, input$mean2, input$sd2)
    # t_test(x1, x2)
    # output$t_test <- renderPrint({
    #     t.test() %>%
    #         tidy() %>%
    #         select(estimate, `P-value` = p.value, Lower = conf.low, Higher = conf.high)
    # })
    output$plot <- renderPlot({
        ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) 
        if (is.numeric(estate[[input$var1_0]]) && is.numeric(estate[[input$var2_0]])){
            if(input$log_1 && input$log_2 && input$ols){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_x_log10() +
                    scale_y_log10() +
                    geom_smooth(method = "lm")
            }
            else if(input$log_1 && input$log_2){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_x_log10() +
                    scale_y_log10()
            }
            else if(input$log_1){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_x_log10()
            }
            else if (input$log_2){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_y_log10()
            }
            else{
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) +
                    geom_point()
            }
        }
       else if (is.factor(estate[[input$var1_0]]) && is.numeric(estate[[input$var2_0]])){
            if(input$log_2){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) + 
                    geom_boxplot() +
                    scale_y_log10()
            }
              else{
                  ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) + 
                      geom_boxplot()
                  }
                      
        }
        else if (is.numeric(estate[[input$var1_0]]) && is.factor(estate[[input$var2_0]])){
            if(input$log_1){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) + 
                    geom_boxploth() +
                scale_x_log10()
            }
            else{
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) + 
                    geom_boxploth()
            }
        }
        else {
            if(input$log){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) + 
                    geom_jitter()+
                    scale_x_log10()
            }
            else{
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) + 
                    geom_jitter()
            }
        }
    })
            output$table <- renderDataTable({
            keep(estate, ~ typeof(.) == "double")
        })
}
shinyApp(ui = ui, server = server)

