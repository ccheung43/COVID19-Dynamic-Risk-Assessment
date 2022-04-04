library(shiny)
library(plotly)
library(stringr)
library(rsconnect)
source("supcodes/LoadUS.R")
source("supcodes/LibrariesNeeded.R")

P <- function(l, p, t) {
  prev = (p)*(1-exp(-t/l))
  return (prev)
}

R <- function(l, p, n, t) {
  risk = 100*(1-(1-P(l, p, t))^n)
  return (risk)
}

US_DATA = LoadUS()
RegionName = US_DATA$RegionName
StateNames <- word(RegionName, 2, sep = fixed(', '))
CountyNames <- word(RegionName, 1, sep = fixed(', '))
pInf <- US_DATA$pInf
df <- data.frame(StateNames, CountyNames, pInf)
allStates = unique(df$StateNames)

overview <- "This model shows the prevalence and risk of COVID-19 over a span of 2 months after testing has occured for  all individuals of the population. The model assumes all individuals who test positive are removed into quarantine."
explainer <- "First, use the dropdowns below to select the state and county. Then, use the sliders to select the values of lambda, ascertainment bias, and event size. Input 'Lambda' as the current average length of infection, 'Ascertainment Bias' as the proportion of unreported to reported cases, and 'Event Size' as the number of individuals at the gathering:" 

ui <- fluidPage(
  titlePanel(h1("COVID-19 Group Prevalence and Risk after Testing and Removal", align = "left")),
  
  sidebarLayout(position = "left",
                sidebarPanel(h3("Selections for Group Characteristics"), overview, br(), br(), explainer, br(), br(), 
                             selectInput(inputId = "state", 
                                         label = "State",
                                         choices = allStates),
                             selectInput(inputId = "county",
                                         label = "County", 
                                         choices = NULL),
                             sliderInput(inputId = "lambda", 
                                         label = "Lambda", 
                                         value = 10, min = 5, max = 15), 
                             sliderInput(inputId = "ab", 
                                         label = "Ascertainment Bias", 
                                         value = 4, min = 3, max = 5, step = 1),
                             sliderInput(inputId = "n", 
                                         label = "Event Size", 
                                         value = 100, min = 1, max = 1000, step = 10)),
                mainPanel(plotlyOutput("prevalence"),
                          plotlyOutput("risk")))
)


server <- function(input, output, session) {
  t = seq(0,60, by = 1)
  observe({
    updateSelectInput(session, "county", "County", 
                      choices = df[df$StateNames == input$state,]$CountyNames)
  })
  output$prevalence <- renderPlotly({ 
    p = input$ab*df[df$StateNames == input$state & df$CountyNames == input$county,]$pInf
    df1 <- data.frame(t, y=P(input$lambda, p, t)) 
    title1 <- paste("Prevalence Assessment:", 
						'<br>',
                                    '<sup>',
                                    input$county, "County," , input$state, "-", date(),
                                    '</sup>')
    legend1 <- paste("Lambda =", input$lambda, "\n Ascertainment Bias = ", input$ab) 
    prev <- plot_ly(df1, x = ~t, y= ~y, name = 'prevalence', color = "blue", type = 'scatter', mode = 'lines', line=list(width=~5))%>% 
      layout(title=title1, subtitle = "hi",  
             xaxis = list(title = list(text ='Time(days)')),
             yaxis = list(title = list(text ='Prevalence')),
             shapes = list(list(type = "line", x0 = 0, x1 = 60, y0 = p, y1 = p, 
                                line=list(color="LightGray",width=3, dash = "dot"))))%>% 
      add_annotations(x = 10, y = p, 
                      text = "baseline prevalence", 
                      textfont = list(color = '#000000'), showarrow = F)%>%
      add_annotations(x = 50, y = 0.0005, 
                      text = legend1, 
                      textfont = list(color = "LightGray"), showarrow = F)
  })
  output$risk <- renderPlotly({
    p = input$ab*df[df$StateNames == input$state & df$CountyNames == input$county,]$pInf
    df2 = data.frame(t, y=R(input$lambda, p, input$n, t))
    bRisk = 100*(1-(1-p)^input$n)
    title2 <- paste("Risk Assessment:", 
						'<br>',
                                    '<sup>',
                                    input$county, "County," , input$state, "-", date(),
                                    '</sup>')

    legend2 <- paste("Lambda =", input$lambda, "\nAscertainment Bias = ", input$ab, "\nEvent Size = ", input$n) 
    risk <- plot_ly(df2, x = ~t, y= ~y, name = 'risk', type = 'scatter', mode = 'lines', line=list(width=~5))%>% 
      layout(title= title2, 
             xaxis = list(title = list(text ='Time(days)')),
             yaxis = list(title = list(text ='Risk that one or more in group is infectious(%)'), range = list(0,100)),
             shapes = list(list(type = "line", x0 = 0, x1 = 60, y0 = bRisk, y1 = bRisk, 
                                line=list(color="LightGray",width=3, dash = "dot"))))%>% 
      add_annotations(x = 10, y = bRisk, 
                      text = "baseline risk", 
                      textfont = list(color = '#000000'), showarrow = F)%>%
      add_annotations(x = 50, y = 10, 
                      text = legend2, 
                      textfont = list(color = "LightGray"), showarrow = F)
    
    
  })
}
shinyApp(ui = ui, server = server)