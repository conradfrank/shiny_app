library(shiny)
library(WDI)
library(plotly)
library(forecast)

#Creating function to use ggplot to plot a time series data
plot_forecast <- function(forec.obj, data.color = 'blue', fit.color = 'red', forec.color = 'black',
                          lower.fill = 'dodgerblue', upper.fill = 'lightskyblue', format.date = F)
{
  serie.orig = forec.obj$x
  serie.fit = forec.obj$fitted
  pi.strings = paste(forec.obj$level, '%', sep = '')
  
  if(format.date)
    dates = as.Date(time(serie.orig))
  else
    dates = time(serie.orig)
  
  serie.df = data.frame(date = dates, serie.orig = serie.orig, serie.fit = serie.fit)
  
  forec.M = cbind(forec.obj$mean, forec.obj$lower[, 1:2], forec.obj$upper[, 1:2])
  forec.df = as.data.frame(forec.M)
  colnames(forec.df) = c('forec.val', 'l0', 'l1', 'u0', 'u1')
  
  if(format.date)
    forec.df$date = as.Date(time(forec.obj$mean))
  else
    forec.df$date = time(forec.obj$mean)
  
  p = ggplot() + theme_minimal() +
    theme(legend.title = element_text(size = 8,face="bold",colour = "orangered" ),legend.text = element_text(size = 7),
          legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6))+
    geom_line(aes(date, serie.orig, colour = 'data'), data = serie.df) + 
    geom_line(aes(date, serie.fit, colour = 'fit'), data = serie.df) + 
    scale_y_continuous() +
    geom_ribbon(aes(x = date, ymin = l0, ymax = u0, fill = 'lower'), data = forec.df, alpha = I(0.4)) + 
    geom_ribbon(aes(x = date, ymin = l1, ymax = u1, fill = 'upper'), data = forec.df, alpha = I(0.3)) + 
    geom_line(aes(date, forec.val, colour = 'forecast'), data = forec.df) + 
    scale_color_manual('Series', values=c('data' = data.color, 'fit' = fit.color, 'forecast' = forec.color)) +
    scale_fill_manual('C.I.', values=c('lower' = lower.fill, 'upper' = upper.fill), label=c("80","95"))
  
  if (format.date)
    p = p + scale_x_date()
  
  p
}


shinyServer(function(input, output) {
   
  #function to accept country name
  fun1 <- reactive({
    cnty_name <-input$select
    cnty_name
  })
  
  #function to derive GDP data for eh country selected
  fun2 <- reactive({
    dummy <-fun1()
    cnty_name <- WDI_data$country[WDI_data$country[,3] == dummy,][1]
    data <- WDI(indicator = "NY.GDP.PCAP.CD", country = cnty_name, start = 1990, end = 2018 )  
    data
  })
  
  #function to plot GDP data
  fun3 <- reactive({
    cnty_name <- fun1()
    data <- fun2()
    p <- plot_ly() %>%
      add_lines(x=data$year, y=data$NY.GDP.PCAP.CD, type = "scatter", mode = "lines") %>% 
      layout(title = paste0("GDP of ",cnty_name),yaxis = list(title="GDP in bn US$"))
    p
  })
  
  #function to accept slider input from forecast tab
  fun4 <- reactive({
    no_years <- input$slider1
    no_years
  })
  
  #function to derive model and predict the GDP
  fun5 <- reactive({
    cnty_name <- fun1()
    data <- fun2()
    yrs <- fun4()
    count1 <- yrs - 2017
    data1 <- data[order(data$year),]
    df <- ts(data1$NY.GDP.PCAP.CD,start = 1990, frequency = 1)
    ARIMAfit = auto.arima(df, approximation=FALSE,trace=FALSE)
    forcst <- forecast(ARIMAfit,h=count1)
    forcst
  })
  
  #function to  plot forecast data using plot_forecast()
  fun6 <- reactive({
    cnty_name <- fun1()
    forcst <- fun5()
    g1 <- plot_forecast(forcst)
    g2 <- ggplotly(g1) %>% 
      layout(title = paste0("Forecast GDP of ",cnty_name),yaxis = list(title="GDP in bn US$",
                                                                       xaxis=list(title="year")))
      g2
  })
  
  output$text1 <- renderText({
      fun1()
  })
  
  text_reactive1 <- eventReactive( input$button1, {
    fun3()
  })
  
    output$plot1 <- renderPlotly({
      text_reactive1()  }) 
    
   output$text2 <- renderText({
     fun1()
   }) 
   
   text_reactive2 <- eventReactive( input$button2, {
     fun6()
   })
   
   output$plot2 <- renderPlotly({
     text_reactive2()  }) 
   
    
  })
  
