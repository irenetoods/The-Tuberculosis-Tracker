library(viridis)
library(ggthemes)
library(reshape2)
library(tidytext)
library(textclean)
library(wordcloud)
library(readr)
library(RColorBrewer)
library(stringr)
library(tibble)
library(wordcloud2)
library(shiny)
library(shinythemes)
library(streamgraph)
library(data.table)
library(ggthemes)
library(ggplot2)
library(gtrendsR)
library(DT)
library(dplyr)
library(maps)


ui <- shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  navbarPage("Tuberculosis Tracker App",
             tabPanel("Home",
                      navlistPanel( widths = c(3, 9), 
                                    tabPanel("Overview",
                                             column(12,
                                                    h2("Project objective"),
                                                    br(),
                                                    p("Tuberculosis is the biggest infectious disease killer in the world. [1]"),
                                                    p("This year, the attempts to raise awareness about the devastating disease have been overshadowed by the COVID-19 pandemic. 
                                                      This project has been set out to call for global awareness on Tuberculosis (TB) and reminds the public that TB remains the No.1 killer among the infectious causes of death."),
                                             ),
                                             column(12,
                                                    style = "padding-top: 10px;",
                                                    h2("What is Tuberculosis?")
                                             ),
                                             column(12,
                                                    style = "padding: 10px;",
                                                    p("TB is caused by bacteria (Mycobacterium tuberculosis) and it most often affects the lungs. 
                                          TB is spread through the air when people with lung TB cough, sneeze or spit. A person needs to inhale only a few germs to become infected.")
                                             ),
                                             column(12,
                                                    style = "padding-top: 10px;",
                                                    h2("Why is it so important?")
                                             ),
                                             column(8,
                                                    style = "background-color:#1D8DC9; color: #fff; padding: 20px; margin: 15px;",
                                                    tags$ul(
                                                      tags$li("10 million people fall ill with tuberculosis (TB) every year."), 
                                                      tags$li("1.5 million people die from Tuberculosis (TB) each year."), 
                                                      tags$li("Tuberculosis (TB) is still the number one killer among the infectious causes of death.")
                                                    )
                                             ),
                                             column(12,
                                                    p("According to WHO, although TB is present all over the world, most of the active cases occured in low and middle income countries. About half of all people with TB can be found in 8 countries: Bangladesh, China, India, Indonesia, Nigeria, Pakistan, Philippines and South Africa."),
                                                    br(),
                                                    p("About one-quarter of the world’s population is estimated to be infected by TB bacteria. Only 5-15% of these people will fall ill with active TB disease. The rest have TB infection but are not ill and cannot transmit the disease. 
                                          Both TB infection and disease are curable using antibiotics."),
                                                    br(),
                                                    img(src = "pic2.jpg", height = 550, width = 800),
                                             ),
                                             column(12,
                                                    style = "padding-top: 10px;",
                                                    h2("For more information"),
                                                    a("World Health Organization (WHO)", href="https://www.who.int/news-room/fact-sheets/detail/tuberculosis"),
                                                    br(),
                                                    a("Center for Disease Control and Prevention (CDC)", href="https://www.cdc.gov/tb/topic/basics/default.htm")
                                             ),
                                             column(12,
                                                    style = "padding-top: 10px; padding-bottom: 50px;",
                                                    h3("Reference:"),
                                                    p("1. Global Tuberculosis Report 2019. Geneva, World Health Organization, 2019"),
                                                    a("", href="https://apps.who.int/iris/bitstream/handle/10665/329368/9789241565714-eng.pdf?ua=1")
                                             )
                                    ),
                                    tabPanel("Symptoms",
                                             column(12,
                                                    h2("Symptoms"),
                                                    br(),
                                                    p("Signs and symptoms of active TB include:")
                                             ),
                                             tags$ul(
                                               tags$li("Coughing that lasts three or more weeks"), 
                                               tags$li("Coughing up blood"), 
                                               tags$li("Chest pain, or pain with breathing or coughing"),
                                               tags$li("Unintentional weight loss"), 
                                               tags$li("Fever"), 
                                               tags$li("Night sweats"),
                                               tags$li("Loss of appetite"),
                                               br()
                                             ),
                                             column(12,
                                                    p("Often, these symptoms will be mild for many months, thus leading to delays in seeking care and increasing the risk of spreading the infection to others.
                                          Tuberculosis can also affect other parts of your body, including your kidneys, spine or brain. When TB occurs outside your lungs, signs and symptoms vary according to the organs involved."),
                                                    br(),
                                                    img(src = "pic1.png", height = 600, width = 630)
                                             )
                                    ),
                                    tabPanel("How to use the App?",
                                             column(12,
                                                    h2("How to use the Apps?"),
                                                    br(),
                                                    p("This is a Tuberculosis Tracker App. There are 4 tabs in the app which are:")
                                             ),
                                             tags$ol(
                                               tags$li("Home"),
                                               p("If you wish to understand what Tuberculosis (TB) is, you can get all the information required in here. It explains in details what is TB, symptoms and contains the statistics of TB to show how important the awareness of TB to public. Besides that, it includes how to use the apps and insights that you can receive after using the app."),
                                               br(),
                                               tags$li("Maps"), 
                                               p("You can see the estimated incident of TB cases from 2004 to 2018 in world map. Based on the year that you have selected, the world map will be displayed in gradient color to indicate the number of incidence accordingly."),
                                               br(),
                                               tags$li("Google Trend"),
                                               p("You can understand the google trend of TB in here. First, you can search for the Google Trend by Country based on the keyword, country and date range that you have selected. The result will display in either graph or tabular format. When you click on the Google Trend by Keyword (Correlations), you can view the google trend by keyword in line chart. Word cloud is to display the related topic according to Google Search."),
                                               br(),
                                               tags$li("Stream Graph"), 
                                               p("If you are interested to know the number by country over a period of time (2004 to 2018) on World Wide historical TB case and World Wide Google Search Trend on TB, you can get the stream graph here."),
                                               br()
                                             )
                                    )
                      )
             ),
                        
             tabPanel("Maps",
                      fluidPage(
                        fluidRow(
                          column(3, selectInput("select2", h3("Select Year"),
                                                choices = list("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"), selected = "2018")),
                          column(9, plotOutput(outputId = "per100kmap"))
                        )
                      )
                      ),
             tabPanel("Google Trend",
                      navlistPanel(widths = c(3, 9), 
                                   tabPanel("Google Trend by Country",
                                            fluidPage(
                                              sidebarLayout(
                                                mainPanel(
                                                  tabsetPanel(id = "YonY",
                                                              tabPanel("Trend", plotOutput(outputId = "trendPlot")),
                                                              tabPanel("Table", h5("Google Search hits breakdown by day"), br(), DT::dataTableOutput("tablePlot"), style="height:450px;overflow-y:scroll;"),
                                                              tabPanel("Year-on-Year", h5("Google Search popularity by state"), br(), plotOutput(outputId = "yoyTrendPlot", height = "100%"), style="height:450px;overflow-y:auto;")
                                                  )
                                                ),
                                                sidebarPanel(
                                                  selectInput("keywords", h3("Keyword"),
                                                              choices = list("Tuberculosis" = "Tuberculosis",
                                                                             "Mycobacterium" = "Mycobacterium",
                                                                             "BCG" = "BCG",
                                                                             "Vaccine" = "Vaccine",
                                                                             "Disease" = "Disease",
                                                                             "Cough" = "Cough")),
                                                  selectInput("select", h3("Country"),
                                                              choices = list("Afghanistan" = "AF", "Albania" = "AL", "Algeria" = "DZ", "Andorra" = "AD", "Angola" = "AO", "Antigua and Barbuda" = "AG", "Argentina" = "AR", "Armenia" = "AM", "Aruba" = "AW", "Australia" = "AU", "Austria" = "AT", "Azerbaijan" = "AZ", "Bahamas" = "BS", "Bahrain" = "BH", "Bangladesh" = "BD", "Barbados" = "BB", "Belarus" = "BY", "Belgium" = "BE", "Belize" = "BZ", "Benin" = "BJ", "Bermuda" = "BM", "Bhutan" = "BT", "Bolivia" = "BO", "Bosnia and Herzegovina" = "BA", "Botswana" = "BW", "Brazil" = "BR", "Brunei" = "BN", "Bulgaria" = "BG", "Burkina Faso" = "BF", "Burundi" = "BI", "Cambodia" = "KH", "Cameroon" = "CM", "Canada" = "CA", "Cape Verde" = "CV", "Cayman Islands" = "KY", "Chad" = "TD", "Chile" = "CL", "China" = "CN", "Colombia" = "CO", "Congo" = "CG", "Costa Rica" = "CR", "Croatia" = "HR", "Cuba" = "CU", "Cyprus" = "CY", "Czechia" = "CZ", "Denmark" = "DK", "Djibouti" = "DJ", "Dominica" = "DM", "Dominican Republic" = "DO", "Ecuador" = "EC", "Egypt" = "EG", "El Salvador" = "SV", "Equatorial Guinea" = "GQ", "Eritrea" = "ER", "Estonia" = "EE", "Eswatini" = "SZ", "Ethiopia" = "ET", "Fiji" = "FJ", "Finland" = "FI", "France" = "FR", "French Polynesia" = "PF", "Gabon" = "GA", "Gambia" = "GM", "Georgia" = "GE", "Germany" = "DE", "Ghana" = "GH", "Greece" = "GR", "Greenland" = "GL", "Grenada" = "GD", "Guam" = "GU", "Guatemala" = "GT", "Guinea" = "GN", "Guinea-Bissau" = "GW", "Guyana" = "GY", "Haiti" = "HT", "Honduras" = "HN", "Hong Kong" = "HK", "Hungary" = "HU", "Iceland" = "IS", "India" = "IN", "Indonesia" = "ID", "Iran" = "IR", "Iraq" = "IQ", "Ireland" = "IE", "Israel" = "IL", "Italy" = "IT", "Jamaica" = "JM", "Japan" = "JP", "Jordan" = "JO", "Kazakhstan" = "KZ", "Kenya" = "KE", "Kuwait" = "KW", "Kyrgyzstan" = "KG", "Laos" = "LA", "Latvia" = "LV", "Lebanon" = "LB", "Lesotho" = "LS", "Liberia" = "LR", "Libya" = "LY", "Lithuania" = "LT", "Luxembourg" = "LU", "Madagascar" = "MG", "Malawi" = "MW", "Malaysia" = "MY", "Maldives" = "MV", "Mali" = "ML", "Malta" = "MT", "Marshall Islands" = "MH", "Mauritania" = "MR", "Mauritius" = "MU", "Mexico" = "MX", "Mongolia" = "MN", "Montenegro" = "ME", "Morocco" = "MA", "Mozambique" = "MZ", "Myanmar" = "MM", "Namibia" = "NA", "Nepal" = "NP", "Netherlands" = "NL", "New Caledonia" = "NC", "New Zealand" = "NZ", "Nicaragua" = "NI", "Niger" = "NE", "Nigeria" = "NG", "North Macedonia" = "MK", "Northern Mariana Islands" = "MP", "Norway" = "NO", "Oman" = "OM", "Pakistan" = "PK", "Palestine" = "PS", "Panama" = "PA", "Papua New Guinea" = "PG", "Paraguay" = "PY", "Peru" = "PE", "Philippines" = "PH", "Poland" = "PL", "Portugal" = "PT", "Puerto Rico" = "PR", "Qatar" = "QA", "Romania" = "RO", "Russia" = "RU", "Rwanda" = "RW", "Samoa" = "WS", "San Marino" = "SM", "Saudi Arabia" = "SA", "Senegal" = "SN", "Serbia" = "RS", "Seychelles" = "SC", "Sierra Leone" = "SL", "Singapore" = "SG", "Sint Maarten" = "SX", "Slovakia" = "SK", "Slovenia" = "SI", "Solomon Islands" = "SB", "Somalia" = "SO", "South Africa" = "ZA", "South Korea" = "KR", "South Sudan" = "SS", "Spain" = "ES", "Sri Lanka" = "LK", "Sudan" = "SD", "Suriname" = "SR", "Sweden" = "SE", "Switzerland" = "CH", "Syria" = "SY", "Taiwan" = "TW", "Tajikistan" = "TJ", "Thailand" = "TH", "Timor-Leste" = "TL", "Togo" = "TG", "Trinidad and Tobago" = "TT", "Tunisia" = "TN", "Turkey" = "TR", "Turkmenistan" = "TM", "Uganda" = "UG", "Ukraine" = "UA", "United Arab Emirates" = "AE", "United Kingdom" = "GB", "United States" = "US", "Uruguay" = "UY", "Uzbekistan" = "UZ", "Venezuela" = "VE", "Vietnam" = "VN", "Yemen" = "YE", "Zambia" = "ZM", "Zimbabwe" = "ZW")),
                                                  dateRangeInput("daterange", h3("Date Range"),
                                                                 min = "2004-01-01", max = format(Sys.Date()-1, "%Y-%m-%d"), start="2004-01-01", end=format(Sys.Date()-1, "%Y-%m-%d"), format="dd-M-yyyy"),
                                                  br(),
                                                  h5("Use Keyword selection to see how they trend on Google."),
                                                  h5("Check out Country selection to choose your country of interest."),
                                                  h5("Choose the dates flexibly with Date Range.")
                                                )
                                              )
                                            )
                                   ),
                                   tabPanel("Google Trend by Keyword (Correlations)",
                                            h5("These are how the related keywords fair on Google Search over the time. It appears that 'TB' is going against the trend of other keywords, most likely due to the growing trend in the adoption of shortform/internet slang."), br(),
                                            plotOutput(outputId = "relatedTopics")
                                   ),
                                   tabPanel("Word Cloud",
                                            h5("These are the related topics according to Google Search. The size of the keywords varies by their relevancy. See what others who searched for Tuberculosis also searched for:"), br(),
                                            wordcloud2Output(outputId = "relatedTopicsCloud")
                                   )
                      )
             ),
             tabPanel("Stream Graph", 
                      fluidPage(
                        fluidRow(
                          column(12, h5("The stream graph shows the number by country over a period of time. Each color in the stream graph represents a country and the size of the stream graph represents the total number at a particular time point."), br()),
                          column(12, h3("Worldwide Historical TB Cases (2004-2018)", style="text-align:center"), streamgraphOutput(outputId = "streamPlot"), br()),
                          column(12, h5("As we can see from both the stream graphs above and in below, the pattern does not match when comparing the worldwide trend in historical TB cases to Google Search Trend on Tuberculosis term between year 2004 and 2018."), br()),
                          column(12, h3("Worldwide Google Search Trend on TB (2004-2018)", style="text-align:center"), streamgraphOutput(outputId = "streamPlot2"))
                        )))
             )
))

server <- shinyServer(function(input, output) {
  
  ####################  TB Explorer   ####################  
  
  observe({
    if (input$select == "US") {
      showTab(inputId = "YonY", target = "Year-on-Year")
    } else {
      hideTab(inputId = "YonY", target = "Year-on-Year")
    }
  })
  
  output$trendPlot <- renderPlot({
    x <- faithful$waiting
    withProgress(message = "Loading ", value = 0.2, {
      if (input$daterange[1] == input$daterange[2]) {
        yr <- paste(as.Date(input$daterange[1])-2, " ", input$daterange[2], sep="")
      } else {
        yr <- paste(input$daterange[1], " ", input$daterange[2], sep="")
      }
      res <- gtrends(input$keywords,
                     geo = input$select,
                     time = yr)
    })
    
    plot.gtrends.silent <- function(x, ...) {
      df <- res$interest_over_time
      df$date <- as.Date(df$date)
      df$hits <- if(typeof(df$hits) == 'character') {
        as.numeric(gsub('<', '', df$hits))
      } else {
        df$hits
      }
      df$legend <- paste(df$keyword, " (", df$geo, ")", sep = "")
      p <- ggplot(df, aes_string(x = "date", y = "hits", color = "legend")) +
        geom_line() +
        xlab("Date") +
        ylab("Search hits") +
        ggtitle(paste("Interest on", input$keywords, " over time", sep = " ")) +
        theme_bw() +
        theme(legend.title = element_blank())
    }
    
    my_plot <- plot.gtrends.silent(res)
    my_plot + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme(legend.position = "none")
  })
  output$tablePlot <- renderDataTable({
    x <- faithful$waiting
    withProgress(message = "Loading ", value = 0.2, {
      if (input$daterange[1] == input$daterange[2]) {
        yr <- paste(as.Date(input$daterange[1])-2, " ", input$daterange[2], sep="")
      } else {
        yr <- paste(input$daterange[1], " ", input$daterange[2], sep="")
      }
      res <- gtrends(input$keywords,
                     geo = input$select,
                     time = yr)
    })
    
    plot.table.silent <- function(x, ...) {
      df <- res$interest_over_time
      df$date <- as.Date(df$date)
      df$hits <- if(typeof(df$hits) == 'character') {
        as.numeric(gsub('<', '', df$hits))
      } else {
        df$hits
      }
      p <- datatable(df[c("date","hits","keyword")])
    }
    
    my_plot <- plot.table.silent(res)
  })
  output$yoyTrendPlot <- renderPlot({
    x <- faithful$waiting
    withProgress(message = "Loading ", value = 0, {
      res <- c()
      startDt <- input$daterange[1]
      endDt <- input$daterange[2]
      startYr <- as.numeric(substr(startDt, 1, 4))
      endYr <- as.numeric(substr(endDt, 1, 4))
      if (startYr != endYr) {
        for (year in startYr:(endYr-1)) {
          if (year == startYr) {
            if (startDt == endDt) {
              yr <- paste(as.Date(startDt)-2, " ", endDt, sep="")
            } else if (substr(startDt, 6, 10) == "12-31") {
              yr <- paste(startYr, "-12-30 ", startYr, "-12-31", sep="")
            } else {
              yr <- paste(startDt, " ", year, "-12-31", sep="")
            }
          } else {
            yr <- paste(year, "-01-01 ", year, "-12-31", sep="")
          }
          res1 <- gtrends(input$keywords, geo="US", time=yr)
          res1$interest_by_region$year <- year
          if (is.null(res)) {
            res = res1
          } else {
            res$interest_by_region = rbind(res$interest_by_region, res1$interest_by_region)
          }
          if (year == startYr) {
            incProgress(0.04, detail = "retrieving data..")
          } else {
            incProgress(((year-startYr) / (endYr-startYr)), detail = "retrieving data..")
          }
        }
        yr <- paste(endYr, "-01-01 ", endDt, sep="")
        res1 <- gtrends(input$keywords, geo="US", time=yr)
        res1$interest_by_region$year <- endYr
        res$interest_by_region = rbind(res$interest_by_region, res1$interest_by_region)
      } else {
        incProgress(0.2, detail = "retrieving data..")
        if (startDt == endDt) {
          yr <- endDt
        } else {
          yr <- paste(startDt, " ", endDt, sep="")
        }
        res <- gtrends(input$keywords,
                       geo = input$select,
                       time = yr)
        res$interest_by_region$year <- endYr
        incProgress(0.5, detail = "retrieving data..")
      }
    })
    plot.gtrendsyoy.silent <- function(x, ...) {
      state <- map_data("state")
      res$interest_by_region %>%
        mutate(region = tolower(location)) %>%
        select(region, hits, year) -> my_df
      
      p <- ggplot() +
        geom_map(data = state,
                 map = state,
                 aes(x = long, y = lat, map_id = region),
                 fill="#ffffff", color="#ffffff", size=0.15) +
        geom_map(data = my_df,
                 map = state,
                 aes(fill = hits, map_id = region),
                 color="#ffffff", size=0.15) +
        scale_fill_continuous(low = "grey", high = "red") +
        facet_wrap(~ year, ncol=3) +
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())
    }
    
    plot(plot.gtrendsyoy.silent(res))
  })
  
  ####################  stream graph  ####################  
  
  output$streamPlot <- renderStreamgraph({
    df <- read.csv('data/TB_burden_countries_2020-05-07.csv')
    
    eval(call("streamgraph", subset(df, year>=2004), "country", "e_inc_100k", "year"))
  })
  
  output$streamPlot2 <- renderStreamgraph({
    df <- read.csv('data/googletrend_interestbycountry.csv')
    eval(call("streamgraph", df, "location", "hits", "year"))
  })
  
  ####################  word cloud  ####################  
  
  output$relatedTopicsCloud <- renderWordcloud2({
    a1 <- read.csv(
      "https://raw.githubusercontent.com/Irnn/dataset_wqd7001/master/wqd7001%20group%20assignment/tuberculosis_12monthsWorldwide_relatedEntities.csv",
      header = TRUE,
      sep = "")
    a2 <- read.csv(
      "https://raw.githubusercontent.com/Irnn/dataset_wqd7001/master/wqd7001%20group%20assignment/USrelatedEntities.csv",
      header = TRUE,
      sep = "")
    a <- rbind(a1, a2)
    
    c <- a$Category. %>%
      str_to_lower() %>%
      replace_contraction() %>%
      strip()
    
    e <- enframe(c,
                 value = "word",
                 name = NULL) %>%
      unnest_tokens(word, word) %>%
      count(word, sort = T) %>%
      anti_join(stop_words)
    
    wordcloud2(
      data = e,
      minSize = 1,
      color = 'random-dark',
      shape = 'circle'
    )
  })
  
  ####################  world map - active cases  ####################  
  
  output$per100kmap <- renderPlot({
    df <- read.csv("https://raw.githubusercontent.com/Irnn/dataset_wqd7001/master/wqd7001%20group%20assignment/TB_burden_countries_2020-05-07.csv")
    df <- df[c("country",'iso3','year','e_pop_num','e_inc_100k','e_inc_num','e_mort_num')]
    df$year <- factor(df$year)
    
    world_map <- map_data("world")                                       ### Create data for world map
    world_map <- world_map[world_map$region != "Antarctica",]            ### remove Antartica 
    
    map_region <- unique(world_map$region)                               ### get names of countries
    country <- unique(df$country)  
    
    df_sub <- df %>%                            
      select(country,                                   
             year,                                        
             e_pop_num,                                          
             e_inc_100k
      )
    
    #ggplot
    inset_plot <- df %>% 
      select(year, e_inc_100k) %>% 
      filter(year == input$select2) %>% 
      ggplot(aes(x = e_inc_100k)) + 
      geom_histogram(Color = "yellow", fill = "plum") + 
      labs(title = "Histogram of\nincidence of TB", subtitle = "per 100,000 population", x = "", y = "") + 
      theme(title = element_text(size = 8), axis.text = element_text(color = "white"))
    g <- ggplotGrob(inset_plot)
    
    world_map_joined <- left_join(world_map,
                                  df %>% filter(year == input$select2),
                                  by = c('region' = 'country')) 
    
    ggplot(data = world_map_joined) + geom_polygon(aes (x = long, y = lat, group = group, fill = e_inc_100k)) +
      labs(title = paste0("The Global Map of TB ", input$select2),
      #labs(title = "The Global Map of TB ", input$select, 
           subtitle = "Estimated Incidence (per 100,000)",
           caption = "WHO | Tuberculosis Data",
           x ="", y = "") +
      scale_fill_gradient(name = "incidence  ",
                          guide = "colourbar",
                          low = "plum",
                          high = "mediumvioletred") +
      scale_colour_hc("darkunica") +
      theme(legend.key.width = unit(2, "cm"),
            panel.grid.major.y = element_blank(),
            legend.just = c("right","bottom"),
            axis.text = element_blank()) +
      annotation_custom(grob = g,
                        xmin = -Inf,
                        xmax = -80,
                        ymin = -Inf,
                        ymax = 20)
  })
  
  ####################  google trend correlations  ####################  
  
  output$relatedTopics <- renderPlot({
    df <- read.csv("https://raw.githubusercontent.com/Irnn/dataset_wqd7001/master/wqd7001%20group%20assignment/TB_burden_countries_2020-05-07.csv")
    df <- df[c("country",'iso3','year','e_pop_num','e_inc_100k','e_inc_num','e_mort_num')]
    df$year <- factor(df$year)
    
    res_ <- gtrends(c("tuberculosis", "TB", "mycobacterium", "silicosis", "PPD"), gprop = "web", time = "all", onlyInterest=TRUE)[[1]]
    res_$year <- substr(res_$date, 1,4)
    res <- dcast(res_, year ~ keyword + geo, value.var = "hits", sum)
    rownames(res) <- res$year
    res <- res[0:(15),]
    
    res$total_cases <- aggregate(df$e_inc_100k, by=list(Category=df$year), FUN=sum)[5:19,"x"]
    
    # z-normalization
    q <- res %>% 
      mutate_if(is.numeric, scale) %>%
      melt()
    ggplot(data=q, aes(x=year, y=value, group=variable, color=variable)) +
      geom_line()
  })
    
  
} )

shinyApp(ui = ui, server = server)
