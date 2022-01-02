#------------------------------------------------------------#
#                         LIBRARY                            #
#------------------------------------------------------------#
library(shiny)
library(tidyverse) 
library(tidyquant)
library(ggplot2)
library(datasets)
require(usmap)
require(openintro)
require(maps)
library(shinydashboard)
library(rtweet)
library(plotly)
library(tidytext)
library(textdata)
library(ggwordcloud)
library(shinyalert)
library(shinyWidgets)



#------------------------------------------------------------#
#                         COVID DATA                         #
#------------------------------------------------------------#

covidus=read.csv("./us_states_covid19_daily_2.csv", sep = ";")

abb=covidus$state
region=abbr2state(abb)

covidus2=cbind(covidus,region)
covidus2['fips'] <- fips(covidus2$state)
states=map_data("state")


#### Agrégation des données ####

states2=states %>%
    group_by(region) %>%
    summarise(lat = max(lat),long=max(long))


covid_agg=covidus2 %>%
    group_by(region,state) %>%
    summarise(total_pos = sum(positive,na.rm = TRUE),total_neg=sum(negative,na.rm = TRUE),
              total_nt=sum(negative,na.rm = TRUE),
              hospitalized_nt = sum(hospitalized,na.rm = TRUE),
              death_nt = sum(death,na.rm = TRUE))
covid_agg$region=tolower(covid_agg$region)
map.df <- merge(covid_agg,states2, by="region", all.x = T)

#------------------------------------------------------------#
#                         TWITTER DATA                       #
#------------------------------------------------------------#

account = read.csv("./twitter_dev.csv", sep = ";")

api_key = account$api_key
api_secret_key = account$api_secret_key
access_token = account$access_token
access_secret = account$access_secret


token <- create_token(
    app = "covid-sentiment",
    consumer_key = api_key,
    consumer_secret = api_secret_key,
    access_token = access_token,
    access_secret = access_secret)



query = "COVID19,lang:en"
tweets_covid = search_tweets(query,
                             n = 200,
                             include_rts = FALSE)

# Tidy the data 
tweets_tokenized_tbl = tweets_covid %>%
    select(text) %>%
    rowid_to_column() %>%
    unnest_tokens(word, text)


tweets_tokenized_tbl %>% count(word, sort = TRUE)


# Join sentiment with Tokenized Text

sentiment_bing_tbl = tweets_tokenized_tbl %>%
    inner_join(get_sentiments ("bing"))



counts = sentiment_bing_tbl %>% count(sentiment)


counts[counts$sentiment=="negative", ]$n


# Sentiment par utulisateur

sentiment_by_row_id_tbl = sentiment_bing_tbl %>%
    select(-word) %>%
    count(rowid, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
    mutate(sentiment = positive - negative) %>%
    left_join(
        tweets_covid %>% select(text) %>% rowid_to_column ()
    )




#------------------------------------------------------------#
#                         UI                                 #
#------------------------------------------------------------#



ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "Covid-19 aux États-Unis",
                    titleWidth = 300),
    
    
    ## Sidebar content
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Sentiment Twitter ", tabName = "Sentiment", icon = icon("random", lib = "glyphicon")),
            menuItem("Positifs", tabName = "Positifs", icon = icon("ok-sign", lib = "glyphicon")),
            menuItem("Négatifs", tabName = "Négatifs", icon = icon("remove-sign", lib = "glyphicon")),
            menuItem("Hospitalisations", tabName = "Hospitalisations", icon = icon("tint", lib = "glyphicon")),
            menuItem("Morts", tabName = "Morts", icon = icon("warning-sign", lib = "glyphicon"))
        )
        
    ),
    ## Body content
    
    dashboardBody(
        # For all sections
        tabItems(
            
            tabItem(tabName = "Sentiment",
                    h3("Analyse en temps réel des Tweets sur le Covid 19 "),
                    
                    useShinyalert(),
                    
                    fluidRow(
                    
                        valueBoxOutput("positive_t"),
                        valueBoxOutput("negative_t"),
                        actionBttn(
                            inputId = "reload",
                            label = "Actualiser les données",
                            color = "warning",
                            style = "material-flat",
                            icon = icon("refresh"),
                            block = TRUE
                        ),
                        
                    ),
                    
                    fluidRow(
                        h3("Polarité"),
                        box(width = 200 , plotlyOutput("plotly", height = 250))
                    ),
                    
                    fluidRow(
                        h3("Nuage de mots"),
                        box(width = 200 , plotOutput("wordcloud", height = 250))
                    )
            ),
            
            
            # section positifs 
            tabItem(tabName = "Positifs",
                    h2("Nombre de cas positifs"),
                    fluidRow(
                        # A static valueBox
                        valueBox(format(sum(covidus$positive,na.rm = TRUE),big.mark=" "), "Total", color = "red", icon = icon("certificate" , lib = "glyphicon")),
                        valueBox(format(mean(covidus$positive,na.rm = TRUE),big.mark=" "), "Moyenne", color = "black", icon = icon("stats" , lib = "glyphicon"))
                        
                        
                        ),
                        
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("region_P",
                                         "Région :",
                                         choiceNames = c("USA","Ouest des USA", "Nord-Est des USA", "Sud des USA"),
                                         choiceValues = c(1,2,3,4))
                        ),
                        
                        mainPanel(
                            plotOutput("usmapPlot_pos")
                        )
                    )
            ),
            
            # section Négatifs 
            tabItem(tabName = "Négatifs",
                    h2("Nombre de cas négatifs"),
                    fluidRow(
                        # A static valueBox
                        valueBox(format(sum(covidus$negative,na.rm = TRUE),big.mark=" "), "Total",color = "green", icon = icon("heart", lib = "glyphicon")),
                        valueBox(format(mean(covidus$negative,na.rm = TRUE),big.mark=" "), "Moyenne",color = "black", icon = icon("stats", lib = "glyphicon"))
                    
                        
                        ),
                    
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("region_N",
                                         "Région :",
                                         choiceNames = c("USA","Ouest des USA", "Nord-Est des USA", "Sud des USA"),
                                         choiceValues = c(5,6,7,8))
                        ),
                        
                        mainPanel(
                            plotOutput("usmapPlot_neg")
                        )
                    )
            ),
            
            # section Hospitalisations 
            tabItem(tabName = "Hospitalisations",
                    h2("Nombre d'hospitalisations"),
                    fluidRow(
                        #  static valueBox
                        valueBox(format(sum(covidus$hospitalized,na.rm = TRUE),big.mark=" "), "Total",color = "blue", icon = icon("tint", lib = "glyphicon")),
                        valueBox(format(mean(covidus$hospitalized,na.rm = TRUE),big.mark=" "), "Moyenne",color = "black", icon = icon("stats", lib = "glyphicon"))
                        ),
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("Hosp",
                                         "Hospitalisations :",
                                         choiceNames = c("USA","Ouest des USA", "Nord-Est des USA", "Sud des USA"),
                                         choiceValues = c(9,10,11,12))
                        ),
                        
                        mainPanel(
                            plotOutput("usmapPlot_hosp")
                        )
                    )
            ),
            
            # section Morts 
            tabItem(tabName = "Morts",
                    h2("Nombre de morts"),
                    
                    fluidRow(
                        # A static valueBox
                        valueBox(format(sum(covidus$death,na.rm = TRUE),big.mark=" "), "Total",color = "yellow", icon = icon("ban-circle", lib = "glyphicon")),
                        valueBox(format(mean(covidus$death,na.rm = TRUE),big.mark=" "), "Moyenne",color = "black", icon = icon("stats", lib = "glyphicon"))
                        ),
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("morts",
                                         "Morts :",
                                         choiceNames = c("USA","Ouest des USA", "Nord-Est des USA", "Sud des USA"),
                                         choiceValues = c(13,14,15,16))
                        ),
                        
                        mainPanel(
                            plotOutput("usmapPlot_morts")
                        )
                    )
            )
            
     
        )
        
    )
)

    
    
    
    

    
#------------------------------------------------------------#
#                         SERVER                             #
#------------------------------------------------------------#


server <- function(input, output, session) {
    
    output$usmapPlot_pos <- renderPlot({
        if(input$region_P==1){
            plot_usmap(data = map.df, values = "total_pos",   labels=TRUE) + 
                scale_fill_continuous( low = "white", high = "red", 
                                       name = "Cas positifs", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Cas positifs de la Covid-19 aux États-Unis", caption = "Source: .....")
        }
        else if(input$region_P==2){
            plot_usmap(data = map.df, values = "total_pos",   labels=TRUE,include = c("CA", "NV", "ID", "OR", "WA")) + 
                scale_fill_continuous( low = "white", high = "red", 
                                       name = "Cas positifs", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Cas positifs de la Covid-19 dans l'ouest des États-Unis", caption = "Source : ....")
        }
        else if(input$region_P==3) {
            plot_usmap(data = map.df, values = "total_pos",   labels=TRUE,include = c("PA", "NJ", "CT", "NY", "MA" , "RI" , "VT" , "NH" , "ME")) + 
                scale_fill_continuous( low = "white", high = "red", 
                                       name = "Cas positifs", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Cas positifs de la Covid-19 dans le Nord-Est des États-Unis", caption = "Source: .... ")
        }
        else if(input$region_P==4) {
            plot_usmap(data = map.df, values = "total_pos",   labels=TRUE,include = c("TX", "OK", "AR", "LA", "MS" , "AL" , "GA" , "FL")) + 
                scale_fill_continuous( low = "white", high = "red", 
                                       name = "Cas positifs", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Cas positifs de la Covid-19 dans le Sud des États-Unis", caption = "Source: .... ")
        }
    })
    
    output$usmapPlot_neg <- renderPlot({
        if(input$region_N==5){
            plot_usmap(data = map.df, values = "total_neg",   labels=TRUE) + 
                scale_fill_continuous( low = "white", high = "green", 
                                       name = "Cas négatifs", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Cas négatifs de la Covid-19 aux États-Unis", caption = "Source: .....")
        }
        else if(input$region_N==6){
            plot_usmap(data = map.df, values = "total_neg",   labels=TRUE,include = c("CA", "NV", "ID", "OR", "WA")) + 
                scale_fill_continuous( low = "white", high = "green", 
                                       name = "Cas négatifs", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Cas négatifs de la Covid-19 dans l'ouest des États-Unis", caption = "Source : ....")
        }
        else if(input$region_N==7) {
            plot_usmap(data = map.df, values = "total_neg",   labels=TRUE,include = c("PA", "NJ", "CT", "NY", "MA" , "RI" , "VT" , "NH" , "ME")) + 
                scale_fill_continuous( low = "white", high = "green", 
                                       name = "Cas négatifs", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Cas négatifs de la Covid-19 dans le Nord-Est des États-Unis", caption = "Source: .... ")
        }
        else if(input$region_N==8) {
            plot_usmap(data = map.df, values = "total_neg",   labels=TRUE,include = c("TX", "OK", "AR", "LA", "MS" , "AL" , "GA" , "FL")) + 
                scale_fill_continuous( low = "white", high = "green", 
                                       name = "Cas négatifs", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Cas négatifs de la Covid-19 dans le Sud des États-Unis", caption = "Source: .... ")
        }
    })
    
    output$usmapPlot_hosp <- renderPlot({
        if(input$Hosp==9){
            plot_usmap(data = map.df, values = "hospitalized_nt" ,   labels=TRUE) + 
                scale_fill_continuous( low = "white", high = "blue", 
                                       name = "Hospitalisations", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Hospitalisations de la Covid-19 aux États-Unis", caption = "Source: @SRK")
        }
        else if(input$Hosp==10) {
            plot_usmap(data = map.df, values = "hospitalized_nt",   labels=TRUE,include = c("CA", "NV", "ID", "OR", "WA")) + 
                scale_fill_continuous( low = "white", high = "blue", 
                                       name = "Hospitalisations", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Hospitalisations de la Covid-19 dans l'ouest des États-Unis", caption = "Source: .... ")
        }
        
        else if(input$Hosp==11) {
            plot_usmap(data = map.df, values = "hospitalized_nt",   labels=TRUE,include = c("PA", "NJ", "CT", "NY", "MA" , "RI" , "VT" , "NH" , "ME")) + 
                scale_fill_continuous( low = "white", high = "blue", 
                                       name = "Hospitalisations", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Hospitalisations de la Covid-19 dans le Nord-Est des États-Unis", caption = "Source: .... ")
        }
        else if(input$Hosp==12) {
            plot_usmap(data = map.df, values = "hospitalized_nt",   labels=TRUE,include = c("TX", "OK", "AR", "LA", "MS" , "AL" , "GA" , "FL")) + 
                scale_fill_continuous( low = "white", high = "blue", 
                                       name = "Hospitalisations", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Hospitalisations de la Covid-19 dans le Sud des États-Unis", caption = "Source: .... ")
        }
        
        
    })
    
    output$usmapPlot_morts <- renderPlot({
        if(input$morts==13){
            plot_usmap(data = map.df, values = "death_nt" ,   labels=TRUE) + 
                scale_fill_continuous( low = "white", high = "yellow", 
                                       name = "morts", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Nombre de morts de la Covid-19 aux États-Unis", caption = "Source: @SRK")
        }
        else if (input$morts==14){
            plot_usmap(data = map.df, values = "death_nt" ,   labels=TRUE, include = c("CA", "NV", "ID", "OR", "WA")) + 
                scale_fill_continuous( low = "white", high = "yellow", 
                                       name = "morts", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Nombre de morts de la Covid-19 dans l'ouest des États-Unis", caption = "Source: @SRK")
        }
        else if (input$morts==15){
            plot_usmap(data = map.df, values = "death_nt" ,   labels=TRUE, include = c("PA", "NJ", "CT", "NY", "MA" , "RI" , "VT" , "NH" , "ME")) + 
                scale_fill_continuous( low = "white", high = "yellow", 
                                       name = "morts", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Nombre de morts de la Covid-19 dans le Nord-Est des États-Unis", caption = "Source: @SRK")
        }
        else if (input$morts==16){
            plot_usmap(data = map.df, values = "death_nt" ,   labels=TRUE, include = c("TX", "OK", "AR", "LA", "MS" , "AL" , "GA" , "FL")) + 
                scale_fill_continuous( low = "white", high = "yellow", 
                                       name = "morts", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Nombre de morts de la Covid-19 dans le Sud des États-Unis", caption = "Source: @SRK")
        }
    })
    
   
    
    
    output$plotly <- renderPlotly({
        
        
        label_wrap = label_wrap_gen (width = 60)
        
        data_formatted = sentiment_by_row_id_tbl %>%
            mutate(text_formatted = str_glue("{label_wrap(text)}"))
        
        
        g = data_formatted %>%
            ggplot(aes(rowid, sentiment)) +
            geom_line(color = "#2c3e50", alpha = 0.5) +
            geom_point(aes(text = text_formatted), color = "#2c3e50" ) +
            geom_smooth(methode = "loess", span = 0.25, se = FALSE , color = "blue") +
            geom_smooth (method = "loess", span = 0.25, se = FALSE, color = "blue") +
            
            geom_hline(aes (yintercept = mean (sentiment)), color = "blue") +
            geom_hline(aes (yintercept = median(sentiment) + 1.96*IQR (sentiment)), color = "red") +
            geom_hline(aes (yintercept = median(sentiment) - 1.96*IQR (sentiment)), color = "red") +
            theme_tq() + 
            labs(Title = "Polarity", x = "Tweets", y = "Sentiment")
        
        g   
        
        # slider 
        ggplotly(g, tooltip = "text") %>%
            layout(
                xaxis = list(
                    rangeslider = list(type = "date")
                )
            )
    })
    
    
    
    
    output$wordcloud <- renderPlot({
        
        sentiment_by_word_tbl = sentiment_bing_tbl %>%
            count(word, sentiment , sort = TRUE)
        
        
        
        sentiment_by_word_tbl %>%
            slice(1:100) %>%
            mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
            ggplot(aes(label = word, color = sentiment, size = n)) +
            geom_text_wordcloud_area() +
            facet_wrap(~ sentiment, ncol = 2) +
            theme_tq()+
            scale_color_tq() +
            scale_size_area(max_size = 16) +
            labs(title = "Fréquence des mots")
    })
    
    output$positive_t <-  renderValueBox({
        
        valueBox(
            width = 4, counts[counts$sentiment=="positive", ]$n , "positive",color = "green", icon = icon("thumbs-up" , lib = "glyphicon")
        )
        
    })
    
    output$negative_t <-  renderValueBox({
        
        valueBox(
            width = 4, counts[counts$sentiment=="negative", ]$n , "positive",color = "red", icon = icon("thumbs-down" , lib = "glyphicon")
        )
        
    })
    
    
    
    
    #------------------------------------------------------------#
    #                         Actualiser les données             #
    #------------------------------------------------------------#
    
    
    
    observeEvent(input$reload, {
        
        
        
        output$positive_t <-  renderValueBox({
            
            valueBox(
                width = 4, counts[counts$sentiment=="positive", ]$n , "positive",color = "green", icon = icon("thumbs-up" , lib = "glyphicon")
                )
            
        })
        
        
        
        output$negative_t <-  renderValueBox({
            
            valueBox(
                width = 4, counts[counts$sentiment=="negative", ]$n , "positive",color = "red", icon = icon("thumbs-down" , lib = "glyphicon")
            )
            
        })
     
        
        
        tweets_covid = search_tweets(query,
                                     n = 200,
                                     include_rts = FALSE)
        
        # Tidy the data 
        tweets_tokenized_tbl = tweets_covid %>%
            select(text) %>%
            rowid_to_column() %>%
            unnest_tokens(word, text)
        
        
        tweets_tokenized_tbl %>% count(word, sort = TRUE)
        
        
        # Join sentiment with Tokenized Text
        
        sentiment_bing_tbl = tweets_tokenized_tbl %>%
            inner_join(get_sentiments ("bing"))
        
        
        
        counts = sentiment_bing_tbl %>% count(sentiment)
        
        
        # Sentiment par utulisateur
        
        sentiment_by_row_id_tbl = sentiment_bing_tbl %>%
            select(-word) %>%
            count(rowid, sentiment) %>%
            pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
            mutate(sentiment = positive - negative) %>%
            left_join(
                tweets_covid %>% select(text) %>% rowid_to_column ()
            )
        
        
        output$plotly <- renderPlotly({
            
            
            label_wrap = label_wrap_gen (width = 60)
            
            data_formatted = sentiment_by_row_id_tbl %>%
                mutate(text_formatted = str_glue("{label_wrap(text)}"))
            
            
            
            g = data_formatted %>%
                ggplot(aes(rowid, sentiment)) +
                geom_line(color = "#2c3e50", alpha = 0.5) +
                geom_point(aes(text = text_formatted), color = "#2c3e50" ) +
                geom_smooth(methode = "loess", span = 0.25, se = FALSE , color = "blue") +
                geom_smooth (method = "loess", span = 0.25, se = FALSE, color = "blue") +
                
                geom_hline(aes (yintercept = mean (sentiment)), color = "blue") +
                geom_hline(aes (yintercept = median(sentiment) + 1.96*IQR (sentiment)), color = "red") +
                geom_hline(aes (yintercept = median(sentiment) - 1.96*IQR (sentiment)), color = "red") +
                theme_tq() + 
                labs(Title = "Polarity", x = "Tweets", y = "Sentiment")
            
            g   
            
            # slider 
            ggplotly(g, tooltip = "text") %>%
                layout(
                    xaxis = list(
                        rangeslider = list(type = "date")
                    )
                )
        })
        
        
        output$wordcloud <- renderPlot({
            
            sentiment_by_word_tbl = sentiment_bing_tbl %>%
                count(word, sentiment , sort = TRUE)
            
            
            
            sentiment_by_word_tbl %>%
                slice(1:100) %>%
                mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
                ggplot(aes(label = word, color = sentiment, size = n)) +
                geom_text_wordcloud_area() +
                facet_wrap(~ sentiment, ncol = 2) +
                theme_tq()+
                scale_color_tq() +
                scale_size_area(max_size = 16) +
                labs(title = "Fréquence des mots")
        })
        
        shinyalert(
            title = "Mise à jour des données...",
            text = "Actualisation des données terminé",
            size = "s", 
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "success",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE
        )
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
