# Jaehee Lee 
library(shiny)
library(tidyverse)
library(broom)
library(ggstance)
estate <- read_csv("../../data/estate.csv", col_types = cols("AC" = col_factor(), "Pool" = col_factor(), "Highway" = col_factor()))
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
                 verticalLayout(sidebarLayout(position = "left", 
                               sidebarPanel(varSelectInput("var1_0", "X Variable", data = estate, selected = "Area"),
                                            checkboxInput("log_1", "Log_Transform?"),
                                            varSelectInput("var2_0", "Y Variable", data = estate, selected = "Price($K)"), 
                                            checkboxInput("log_2", "Log_Transform?"),
                                            checkboxInput("ols", "Fit OLS?")
                 ),
                 mainPanel(
                     plotOutput("plot")
                 ))
                 ),
                 conditionalPanel(condition = "input.ols",
                                  sidebarLayout(position = "left",
                                                sidebarPanel(verbatimTextOutput("ols_results"), 
                                                             verbatimTextOutput("lm_1")), 
                 mainPanel(
                               column(width = 6, plotOutput("plot_res")),
                                column(width = 6, plotOutput("plot_qq"))
                 )))
                 ),
        
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
    output$t_test <- renderTable({
        if (is.numeric(estate[[input$var1]])){
            if(input$log){
                estate %>% 
                    select(input$var1) %>%  
                    log() %>%  
                    t.test(alternative = "two.sided", mu = input$num, conf.level = 0.95) %>%  
                    tidy() %>% 
                    select(p.value,estimate, conf.low, conf.high)  %>%  
                    rename(c('P-Value' = p.value, 'Estimate' = estimate, '95% Lower' = conf.low, '95% Higher' = conf.high))
           }
            else{
                estate %>% 
                    select(input$var1) %>%  
                    t.test(alternative = "two.sided", mu = input$num, conf.level = 0.95) %>%  
                    tidy() %>% 
                    select(p.value,estimate, conf.low, conf.high) %>%  
                    rename(c('P-Value' = p.value, 'Estimate' = estimate, '95% Lower' = conf.low, '95% Higher' = conf.high))
                
            }
        }
        else{
            if(input$log){
                validate(
                    need(is.double(estate[[input$var1]]),
                         "not numeric")
                )
            }
                print("Variable is not numeric")            
        }
        
    })
    
    output$lm_1 <- renderPrint({
        if(is.numeric(estate[[input$var2_0]]) && is.numeric(estate[[input$var1_0]])){
            if(input$log_1 && input$log_2 && input$ols){
        lmout_0 <- lm(log(estate[[input$var2_0]]) ~ log(estate[[input$var1_0]]), data = estate)
        print(summary(lmout_0))
            }
            else if(input$log_1 && input$ols){
        lmout_1 <- lm(estate[[input$var2_0]] ~ log(estate[[input$var1_0]]), data = estate)
        print(summary(lmout_1))
            }
            else if(input$log_2 && input$ols){
                lmout_2 <- lm(log(estate[[input$var2_0]]) ~ estate[[input$var1_0]], data = estate)
                print(summary(lmout_2))
            }
            else if(input$ols){
        lmout <- lm(estate[[input$var2_0]] ~ estate[[input$var1_0]], data = estate)
        print(summary(lmout))
            }
        }
        else{
            print("not numeric")
        }
    })
    output$plot_res <- renderPlot({
        if(is.numeric(estate[[input$var1_0]]) && is.numeric(estate[[input$var2_0]])){
            if(input$log_1 && input$log_2 && input$ols){
                lmout_0 <- lm(log(estate[[input$var1_0]]) ~ log(estate[[input$var2_0]]), data = estate)
                ggplot(lmout_0, aes(x=.fitted, y=.resid))+
                    geom_point()+
                    labs(x="x", y = "y", title = "Residuals vs Fitted") 
            }
            else if(input$log_1 && input$ols){
                lmout_1 <- lm(estate[[input$var2_0]] ~ log(estate[[input$var1_0]]), data = estate)
                ggplot(lmout_1, aes(x=.fitted, y=.resid))+
                    geom_point()+
                    labs(x="x", y = "y", title = "Residuals vs Fitted") 
            }
            else if(input$log_2 && input$ols){
                lmout_2 <- lm(log(estate[[input$var2_0]]) ~ estate[[input$var1_0]], data = estate)
                ggplot(lmout_2, aes(x=.fitted, y=.resid))+
                    geom_point()+
                    labs(x="x", y = "y", title = "Residuals vs Fitted") 
            }
            if(input$ols)
                lmout <- lm(estate[[input$var1_0]] ~ estate[[input$var2_0]], data = estate)
            ggplot(lmout, aes(x=.fitted, y=.resid))+
                geom_point()  +
                labs(x="x", y = "y", title = "Residuals vs Fitted") 
        }
        
        else{
            print("not numeric")
        }
    })
    output$plot_qq <- renderPlot({

        if(is.numeric(estate[[input$var1_0]]) && is.numeric(estate[[input$var2_0]])){
            if(input$log_1 && input$log_2 && input$ols){
                lmout_0 <- lm(log(estate[[input$var1_0]]) ~ log(estate[[input$var2_0]]), data = estate)
                ggplot(lmout_0, aes(sample=.fitted)) +
                    stat_qq() + 
                    stat_qq_line() +
                    labs(x="theoretical", y = "sample", title = "QQPlot") 
            }
            
            else if(input$log_1 && input$ols){
                lmout_1 <- lm(estate[[input$var2_0]] ~ log(estate[[input$var1_0]]), data = estate)
                ggplot(lmout_1, aes(sample=.fitted)) +
                    stat_qq() + 
                    stat_qq_line() +
                    labs(x="theoretical", y = "sample", title = "QQPlot") 
            }
            else if(input$log_2 && input$ols){
                lmout_2 <- lm(log(estate[[input$var2_0]]) ~ estate[[input$var1_0]], data = estate)
                ggplot(lmout_2, aes(sample=.fitted)) +
                    stat_qq() + 
                    stat_qq_line() +
                    labs(x="theoretical", y = "sample", title = "QQPlot") 
            }
            else if (input$ols){
                lmout <- lm(estate[[input$var1_0]] ~ estate[[input$var2_0]], data = estate)
                ggplot(lmout, aes(sample=.fitted)) +
                    stat_qq() + 
                    stat_qq_line() +
                   labs(x="theoretical", y = "sample", title = "QQPlot")
                }
            else{
                print("not numeric")
            }
        }
    })
    output$plot <- renderPlot({
        ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) 
        if (is.numeric(estate[[input$var1_0]]) && is.numeric(estate[[input$var2_0]])){
            if(input$log_1 && input$log_2 && input$ols){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_x_log10() +
                    scale_y_log10() +
                    geom_smooth(method = "lm") +
                    labs(x = paste("Log(", input$var1_0,")"))+
                    labs(y = paste("Log(", input$var2_0,")"))
            }
            else if(input$log_1 && input$ols){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_x_log10() +
                    geom_smooth(method = "lm") +
                    labs(x = paste("Log(", input$var1_0,")"))
            }
            else if(input$log_2 && input$ols){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_y_log10() +
                    geom_smooth(method = "lm") +
                    labs(y = paste("Log(", input$var2_0,")"))
            }
            else if(input$ols){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    geom_smooth(method = "lm")
                
            }
            else if(input$log_1 && input$log_2){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_x_log10() +
                    scale_y_log10() +
                    labs(x = paste("Log(", input$var1_0,")"))+
                    labs(y = paste("Log(", input$var2_0,")"))
                
                
            }
            else if(input$log_1){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_x_log10() +
                    labs(x = paste("Log(", input$var1_0,")"))
                    
            }
            else if (input$log_2){
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0))+
                    geom_point()+
                    scale_y_log10() +
                    labs(y = paste("Log(", input$var2_0,")"))
                
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
            else if(input$log_1){
                validate(
                    need(is.double(estate[[input$var1_0]]),
                         "not numeric")
                )
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
            else if(input$log_2){
                validate(
                    need(is.double(estate[[input$var2_0]]),
                         "not numeric")
                )
            }
            else{
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) + 
                    geom_boxploth()
            }
        }
     
            else{
                ggplot(estate, aes(x = !!input$var1_0, y = !!input$var2_0)) + 
                    geom_jitter()
            }
 
    })
    output$table <- renderDataTable({
        keep(estate, ~ typeof(.) == "double")
    })
}
shinyApp(ui = ui, server = server)


# I completed Extra credit Tab 2  

# Extra credit Webinar

#1. How has the R Studio shiny development changed their focus over the past several years?
#In the past, there were heavy loads on shiny applications, they monitored the traffic, used hands, and complicated, etc. These Days, they are focusing on making improvements in ui to be more flexible. Also, they are making shiny ready for production and doing automatic testing. They are also improving the speed and they are working on reinventing the space, providing lots of examples and documentation for users. When doing Shiny apps people have to have different perspectives than doing typical R. It is good if you learn how to do software engineering to be more efficient. 
#Furthermore, RStudio's shiny development is embracing people from a lot of backgrounds. For example, there was a person who came from a designer background and was involved in RStudio Shiny development. This designer perspective helped to visualize better and update the web library, seeing from different perspectives compared to other computer science background people. To sum up, in the past RStudio shiny was complicated and had some traffic, but these days it is making it more interactive to the users and becoming more efficient.

#2. How does your approach to writing shiny apps change when you think they will need to be maintained
#over the long run by your “future self” or others?
# If I wrote a shiny app and if I assume that this will need to be maintained over the long run by my “future self” or others, then I will use Github for version control. This is good for managing the whole process and also good for collaboration. Or I can use other version management systems for this Shiny app. This is good because people can go back and do that, several times. This helps to track and manage changes to the code on R shiny app. This will help it to be maintained for over the long run.



