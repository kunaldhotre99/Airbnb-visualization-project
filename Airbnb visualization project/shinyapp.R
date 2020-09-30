# load required libraries
library(shinythemes)
library(shiny)
library(leaflet)
library(ggplot2)
library(wordcloud)
options(warn = -1)
#Read Data into R
airbnbDf <- read.csv("newlistings.csv")
q3df <- read.csv("q3.csv")
expenseDf <- read.csv("Expensive.csv")
cheapDf <- read.csv("Cheap.csv")
states <- geojsonio::geojson_read("neighbourhoodsNew.geojson", what = "sp")





# create the user interface and include a drop down menu to let the user choose their desired theme
ui <- fluidPage(theme = shinytheme("superhero"),navbarPage("Analysis of Melbourne's Airbnb data",
                                                           # define a panel for the introduction page
                                                           tabPanel("Introduction",h3("Purpose of this app"),h5("This app is developed in order to answer three analytical questions using visualizations"),h5("1. What are the important keywords used in the airbnb listings"),h5("2. Which areas have more traffic than others?"),h5("3. How does price and number of reviews affect the number for days a given listing is booked."),hr(),h3("How to navigate through the app?"),h5("Click on the tabs on the top and click on the graphs you want to visualize. Description about each graph is given in each tabs."),h5("1. Visualizing imprtant keywords for cheaper and expensive listings."),h5("2. Visualizing which neighbourhood regions have highest number of listings and also plotting a map which is clusterd according to the neighbourhood groups."),h5("3. Visualizing how price and number of reviews affect occupancy rate.")),
                                                           # define a panel for the first tab
                                                           tabPanel("Question-1", fluidRow(column(10,
                                                                                                  h3("What are the important keywords used in listing?"),
                                                                                                  h4("Description"),
                                                                                                  h5("The plot is called as word cloud. As we can see, different words are used for describing cheaper and expensive listings in airbnb. Expensive listings use words like luxurious, private,parking(a parking for a car),townhouse,cbd,gym,pool,etc. Cheaper airbnb listings have words like apartments,free, wifi, cozy,comfy,etc. The lisitngs are considered expensive if the price of a listing is more than A$500.Apparently the listings near airport, Richmond and yarra are considered expensive."))),
                                                                    hr(),
                                                                    # define a sidebar panel and include a drop down menu and radio buttons
                                                                    fluidRow(sidebarPanel(selectInput("bw", "Select the type of plot:",
                                                                                                      c("Bar"="bar","Wordcloud"="wordcloud"), selected = "bar"),
                                                                      width = 3,
                                                                      h4("Select the type of listings:"),
                                                                      radioButtons("varRadio", NULL,
                                                                                   c("Cheaper airbnb listings" = "A",
                                                                                     "Expensive airbnb Listings" = "B"), selected = "A")),
                                                                      # define a main panel that contains a conditional panel that works based on the user's input
                                                                      mainPanel(conditionalPanel(condition = "input.varRadio == 'A' && input.bw == 'bar'",
                                                                                                 plotOutput("plot1")),
                                                                                conditionalPanel(condition = "input.varRadio == 'A' && input.bw == 'wordcloud'",
                                                                                                 plotOutput("plot2")),
                                                                                conditionalPanel(condition = "input.varRadio == 'B' && input.bw == 'bar'",
                                                                                                 plotOutput("plot3")),
                                                                                conditionalPanel(condition = "input.varRadio == 'B' && input.bw == 'wordcloud'",
                                                                                                 plotOutput("plot4")))),
                                                                    hr()),
                                                           # define a panel for the second tab
                                                           tabPanel("Question-2",
                                                                    fluidRow(column(10,
                                                                                    h3("Which areas have more traffic than others?"),
                                                                                    h4("Description"),
                                                                                    p("Use the Radio buttons to switch between bargraph and the map. The map is clustered woth respect to neighbourhoods. Clicking on the clusters, we can see in which neighbourhood whch pops up in the chloropleth map. As we can see Melbourne has highest number of listings and as we can see more than 7000 listings are based in melbourne as they are close to the city "))),
                                                                    hr(),
                                                                    fluidRow(sidebarPanel(
                                                                      width = 3,
                                                                      h4("Type of plot:"),
                                                                      radioButtons("varRadio2", NULL,
                                                                                   c("Bargraph" = "bar",
                                                                                     "map" = "map"), selected = "bar")),
                                                                      # define a main panel that contains a conditional panel that works based on the user's input
                                                                      mainPanel(conditionalPanel(condition = "input.varRadio2 == 'bar'",
                                                                                                 plotOutput("plot21")),
                                                                                conditionalPanel(condition = "input.varRadio2 == 'map'",
                                                                                                 leafletOutput("plot22"))))),
                                                           
                                                           # define a panel for the third tab
                                                           tabPanel("Question-3",fluidRow(column(10,
                                                                                                 h3("Does price and number of reviews have any affect on the number of days a given listing is booked. i.e. with respect to occupancy rate?"),
                                                                                                 h4("Description"),
                                                                                                 h5("Select the radio button of your liking. As we can see, by selecting the first radio button we can see how occupancy rate affects the price of an airbnb listing. We can see that the occupancy rate is lower for listings with higher prices in comparison to moderate or lower priced airbnb listings. NUmber of reviews by the customers also increases with increase in occupancy rate for that listing. "))),
                                                                    hr(),
                                                                    fluidRow(sidebarPanel(
                                                                      width = 3,
                                                                      h4("Select the plot you want to view:"),
                                                                      radioButtons("varRadio3", NULL,
                                                                                   c("Listing price vs Occupancyrate(no.of days a listing is booked)" = "pVo",
                                                                                     "Number of reviews vs Occupancy rate(no.of days a listing is booked)" = "rVo"), selected = "pVo")),
                                                                      # define a main panel that contains a conditional panel that works based on the user's input
                                                                      mainPanel(conditionalPanel(condition = "input.varRadio3 == 'pVo'",
                                                                                                 plotOutput("plot31")),
                                                                                conditionalPanel(condition = "input.varRadio3 == 'rVo'",
                                                                                                 plotOutput("plot32")))))
))

# create the server side functionality
server <- function(input, output)
{
  # create a plot of barplot for cheaper listings
  output$plot1 <- renderPlot({
    barplot(cheapDf$count, las = 2, names.arg = cheapDf$words,
            col ="lightblue", main ="Most frequent words",
            ylab = "Word frequencies")
    
  })
  
  # create a wordcloud for of important keywords forcheaper listings 
  output$plot2 <- renderPlot({

    wordcloud(words = cheapDf$words, freq = cheapDf$count, min.freq = 220, max.words=4557, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    
  })
  # create a barplot of top keywords for expensive listings
  output$plot3 <- renderPlot({
    barplot(expenseDf$count, las = 2, names.arg = expenseDf$words,
            col ="lightblue", main ="Most frequent words",
            ylab = "Word frequencies")

  })
  # create a wordcloud for top keywords for expensive listings
  output$plot4 <- renderPlot({
    wordcloud(words = expenseDf$words, freq = expenseDf$count, min.freq = 220, max.words=4557, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    
    
  })
  
  
  
  
  # create a bar graph for how number lof listings are distributed with respect to neighbourhood
  output$plot21 <- renderPlot({
    ggplot(airbnbDf,aes(x=reorder(neighbourhood,neighbourhood,function(x)-length(x))))+ geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    
  })
  # leaflet for visualizing the distribution of listings w.r.t. neighbourhood
  output$plot22 <- renderLeaflet({
    pal <- colorNumeric("viridis",NULL)
    
    leaflet(states) %>%
      addTiles() %>%
      addPolygons(stroke = TRUE,weight = 2,color = "white",smoothFactor = 0.9, fillOpacity = 0.9,
                  fillColor = ~pal((count)),highlightOptions = highlightOptions( weight = 5,color = "#666",

                                                                                     fillOpacity = 0.7,
                                                                                     bringToFront = TRUE),
                  label = ~paste0(neighbourhood, ": ", formatC(count, big.mark = ","))) %>%
      addLegend(pal = pal, values = ~(count), opacity = 1.0,
                labFormat = labelFormat())
    
    
  })
  #create a bar plot for occupancy rate vs price
  output$plot31 <- renderPlot({
    ggplot(q3df,aes(x=occupany_rate,y=price))+geom_point()
    
    
    
  })
  # create a bar plot for occupancy rate vs number of reviews
  output$plot32 <- renderPlot({
    ggplot(q3df,aes(x=occupany_rate,y=number_of_reviews))+geom_point()
    
  })
}


shinyApp(ui, server)

