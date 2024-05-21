carbon_fp <- read.csv("D:\\Data Science MTU\\Data Visualisation\\Project 2\\ddf--datapoints--carbonfp_pcap--by--country--time.csv", header = T)
gdp <- read.csv("D:\\Data Science MTU\\Data Visualisation\\Project 2\\ddf--datapoints--gdp_pcap--by--country--time.csv",  header = T)
gini <- read.csv("D:\\Data Science MTU\\Data Visualisation\\Project 2\\ddf--datapoints--gini--by--country--time.csv", header = T)

##############################################################################
# Cutting data to 1980 - 2020
##############################################################################
time_start = as.integer(1980)
time_end = as.integer(2020)

carbon_fp <- subset(carbon_fp, carbon_fp$time >= time_start)
carbon_fp <- subset(carbon_fp, carbon_fp$time <= time_end)

gdp <- subset(gdp, gdp$time >= time_start)
gdp <- subset(gdp, gdp$time <= time_end)

#str(gini)
gini <- subset(gini, time >= time_start)
gini <- subset(gini, time <= time_end)

#View(carbon_fp)
#View(gdp)
data <- carbon_fp
data <- merge(data, gdp, by = c("country","time"))
data <- merge(data, gini, by = c("country","time"))
#View(data)

#write.csv(data, "C:\\Users\\jverl\\Desktop\\Data Science MTU\\Data Visualisation\\Project 2\\data.csv", row.names=FALSE)

#install.packages("countries")
library(countries)
#install.packages("shinythemes")
library(patchwork)
library(tidyverse)
library(shiny)
library(shinythemes)
library(ggthemes)
library(forecast)
library(vars)

info <- country_info(data$country)
data$country <- info$name.common
#View(data)

#View(info)

####################################################
# Creating table of correlation values
####################################################

countries <- unique(data$country)
num_countries <- length(countries)

correlations <- replicate(num_countries,NA)

for (i in 1:num_countries){
  place <- countries[i]
  correlations[i] <- cor(data$carbonfp_pcap[data$country==place],
                         data$gdp_pcap[data$country==place])
}
correlation_df <-data.frame(countries,correlations)

correlation_df$countries <- as.factor(correlation_df$countries)


###############################################################
# Dashboard Code
###############################################################



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("superhero"),
    navbarPage("Carbon Footprint, Economic Performance and Inequality Analysis",
               
                  tabPanel("Trends",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "place", 
                                      label="Select Country", 
                                      choices=unique(data$country),
                                      selected="Afghanistan"
                                      )
                          ),
                  mainPanel(
                      plotOutput(outputId =  "trends")
                  )
            )
          ),
           tabPanel("Correlations",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(inputId = "zoom",
                                    label="Zoom by Region",
                                    choices=c("World" = "World",
                                              "Africa" = "Africa",
                                              "Asia" = "Asia",
                                              "Europe" = "Europe",
                                              "South East Asia" = "SEAsia",
                                              "North America" = "NAmerica",
                                              "Central America" = "SAmerica",
                                              "South America" = "SAmerica",
                                              "Oceania" = "Oceania"
                                              ),
                                    selected="World")
                      ),
                    mainPanel(
                       plotOutput("map")
                      )
                    )
                    ),       tabPanel("Forecast",
                                          sidebarLayout(
                                            sidebarPanel(
                                              selectInput(inputId = "region", 
                                                          label="Select Country", 
                                                          choices=unique(data$country),
                                                          selected="Afghanistan"
                                              ),
                                              sliderInput(inputId = "future", 
                                                          label="Select Length of Forecast", 
                                                          min = 1,
                                                          max = 10,
                                                          step = T,
                                                          post = " years ahead",
                                                          value = 5
                                              )
                                              
                                            ),
                                            mainPanel(
                                              plotOutput("forecast"),
                                              tableOutput("diagnostic")
                                            )
                                          )
                        )
                  )
          )


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$trends <- renderPlot({
    data_country <- data %>%
      filter(country==input$place)
    p1 <-ggplot(data=data_country, aes_string(x="time", y="carbonfp_pcap")) +
    geom_smooth() +
      labs(subtitle="Carbon Footprint Per Capita 1980-2020", 
           y="Carbon Footprint per Capita", 
           x="Year", 
           title="Carbon Production Over Time", 
           caption = "Source: Gapminder")+
      theme_economist()

    p2 <-ggplot(data=data_country, aes_string(x="time", y="gdp_pcap" )) +
      geom_smooth()+
      labs(subtitle="Gross Domestic Product Per Capita 1980-2020", 
           y="GDP per Capita", 
           x="Year", 
           title="GDP Over Time", 
           caption = "Source: Gapminder")+
      theme_economist()
    
    
    p3 <-ggplot(data=data_country, aes_string(x="time", y="gini" )) +
      geom_smooth() +
      labs(subtitle="Gini Coefficient 1980-2020", 
           y="Gini coefficient Capita", 
           x="Year", 
           title="Inequality Over Time", 
           caption = "Source: Gapminder") +
      theme_economist()
    

    p <- p1+p2+p3
    p
  })
  
  output$map <- renderPlot({
    zoom <- input$zoom 
    world <- map_data("world")
    quick_map(correlation_df, plot_col = "correlations", reverse_palette = F, zoom = zoom)
     })
  
  output$forecast <- renderPlot({
    
    
    ts_plotter <- function(region, h){
      region <- input$region
      model_data <- subset(data, data$country==region)
      carbon <- ts(model_data$carbonfp_pcap, start = c(1980,1), frequency = 1)
      gdp <- ts(model_data$gdp_pcap, start = c(1980,1), frequency = 1)
      gini <- ts(model_data$gini, start = c(1980,1), frequency = 1)
      
      ts_data <- cbind(carbon, gdp)
      ts_data <- cbind(ts_data, gini)
      
      aic <- VARselect(ts_data, lag.max=8, type="trend")$selection["AIC(n)"]
      model <- VAR(ts_data, p=aic, type="both")
      
      future = forecast(model, h = h)
      p <-autoplot(future) + ggtitle("Multivariate Forecast")
      p <- p + theme_economist()
      p
      
      
    }
    ts_plotter(input$region, input$future)
    
  })
  
  output$diagnostic <- renderTable({
    
    region <- input$region
    model_data <- subset(data, data$country==region)
    carbon <- ts(model_data$carbonfp_pcap, start = c(1980,1), frequency = 1)
    gdp <- ts(model_data$gdp_pcap, start = c(1980,1), frequency = 1)
    gini <- ts(model_data$gini, start = c(1980,1), frequency = 1)
    
    ts_data <- cbind(carbon, gdp)
    ts_data <- cbind(ts_data, gini)
    
    aic <- VARselect(ts_data, lag.max=8, type="trend")$selection["AIC(n)"]
    model <- VAR(ts_data, p=aic, type="both")
    ######################################################################
    # Diagnostics
    ######################################################################
    # True Return always means fail
    
    autocor_check <- function(model){
      # Checking for autocorrelation
      serial1 <- serial.test(model)
      p_autocor <- serial1$serial[[3]]
      if(p_autocor < 0.05){
        return(TRUE) # autocorrelation present - failed test
      }else{
        return(FALSE) # autocorrelation not present - passed
      }
    }
    
    het_ked_check <- function(model){
      # Checking for Heteroskedasticity
      arch1 <-arch.test(model)
      p_hetero <- arch1$arch.mul[[3]]
      if(p_hetero < 0.05){
        return(TRUE) # Heteroskedasticity is present - fail
      }else{
        return(FALSE) # No heteroskedasticity is present - pass
      }
    }
    
    normality_check <- function(model){
      norm1 <- normality.test(model)
      p_norm <- norm1$jb.mul$JB[[3]] 
      p_skew <- norm1$jb.mul$Skewness[[3]] 
      p_kurt <- norm1$jb.mul$Kurtosis[[3]] 
      if(p_norm < 0.05 || p_skew < 0.05 || p_kurt < 0.05){
        return(TRUE) # not normal - fail
      }else{
        return(FALSE) # normal - pass
      }
    }
    
    diagnostic <- c("Autocorrelation", "heteroscedasticity", "Normality")
    result <- c(autocor_check(model),het_ked_check(model),normality_check(model))
    statement <- replicate(n=3, NA)
    
    for (i in 1:3){
      if(result[i]==T){
        statement[i] = "Fail"
      } else {
        statement[i] = "Pass"
      }
    }
    
    diagnostic_df <- cbind(diagnostic, result)
    View(diagnostic_df)
    diagnostic_df <- cbind(diagnostic_df, statement)
    diagnostic_df
  })
}

shinyApp(ui = ui, server = server)

#model_data <- subset(data, data$country=="Germany")
#View(model_data)

#model <- lm(model_data$gini~model_data$carbonfp_pcap+
#          model_data$gdp_pcap + 
#          model_data$carbonfp_pcap+
#          model_data$gdp_pcap*model_data$carbonfp_pcap)

#summary(model)
#p_carb_gdp <- ggplot(data = model_data, aes(y=carbonfp_pcap, x = gdp_pcap)) + 
#  geom_point() +
#  ggtitle("Carbon vs GDP")
#p_carb_gdp
#p_carb_gini <- ggplot(data = model_data, aes(y=carbonfp_pcap, x = gini)) +
#  geom_point() +
#  ggtitle("Carbon vs Gini")#p_carb_gini
#p_gdp_gini <- ggplot(data = model_data, aes(y=gdp_pcap, x = gini)) +
#  geom_point() +
#  ggtitle("GDP vs Gini")
#p_gdp_gini

#p <- (p_carb_gdp + p_carb_gini) / p_gdp_gini
#p
#plot(model_data$gdp_pcap~model_data$gini)

#model_data <- subset(data, data$country=="Germany")
#View(model_data)

#plot(model_data$carbonfp_pcap~model_data$gdp_pcap)
#plot(model_data$carbonfp_pcap~model_data$gini)
#plot(model_data$gdp_pcap~model_data$gini)
