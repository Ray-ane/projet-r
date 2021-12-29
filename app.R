library(shiny)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(datasets)
require(usmap)
require(openintro)
require(maps)
library(shinydashboard)


covidus=read.csv("us_states_covid19_daily_2.csv", sep = ";")

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



ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "Covid-19 aux États-Unis"),
    
    
    ## Sidebar content
    
    dashboardSidebar(
        sidebarMenu(
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
            
            # section positifs 
            tabItem(tabName = "Positifs",
                    h2("Nombre de cas positifs"),
                    fluidRow(
                        # A static valueBox
                        valueBox(sum(covidus$positive,na.rm = TRUE), "Total",color = "red", icon = icon("eye-close", , lib = "glyphicon"))),
                        
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
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("Hosp",
                                         "Hospitalisations :",
                                         choiceNames = c("USA","Nord-Est des USA"),
                                         choiceValues = c(9,10))
                        ),
                        
                        mainPanel(
                            plotOutput("usmapPlot_hosp")
                        )
                    )
            ),
            
            # section Morts 
            tabItem(tabName = "Morts",
                    h2("Nombre de morts"),
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("morts",
                                         "Morts :",
                                         choiceNames = c("USA","Nord-Est des USA", "Sud des USA"),
                                         choiceValues = c(11,12,13))
                        ),
                        
                        mainPanel(
                            plotOutput("usmapPlot_morts")
                        )
                    )
            )
            
            
            
            
            
        )
        
    )
)

    
    
    
    
    


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
                labs(title = "Cas négatifs de la Covid-19 dans le nord-est des États-Unis", caption = "Source: .... ")
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
            plot_usmap(data = map.df, values = "hospitalized_nt",   labels=TRUE,include = c("PA", "NJ", "CT", "NY", "MA" , "RI" , "VT" , "NH" , "ME")) + 
                scale_fill_continuous( low = "white", high = "blue", 
                                       name = "Hospitalisations", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Hospitalisations de la Covid-19 dans le nord-est des États-Unis", caption = "Source: .... ")
        }
    })
    
    output$usmapPlot_morts <- renderPlot({
        if(input$morts==11){
            plot_usmap(data = map.df, values = "death_nt" ,   labels=TRUE) + 
                scale_fill_continuous( low = "white", high = "yellow", 
                                       name = "morts", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Nombre de morts de la Covid-19 aux États-Unis", caption = "Source: @SRK")
        }
        else if (input$morts==12){
            plot_usmap(data = map.df, values = "death_nt" ,   labels=TRUE, include = c("PA", "NJ", "CT", "NY", "MA" , "RI" , "VT" , "NH" , "ME")) + 
                scale_fill_continuous( low = "white", high = "yellow", 
                                       name = "morts", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Nombre de morts de la Covid-19 dans le Nord-Est des États-Unis", caption = "Source: @SRK")
        }
        else if (input$morts==13){
            plot_usmap(data = map.df, values = "death_nt" ,   labels=TRUE, include = c("TX", "OK", "AR", "LA", "MS" , "AL" , "GA" , "FL")) + 
                scale_fill_continuous( low = "white", high = "yellow", 
                                       name = "morts", label = scales::comma
                ) + 
                theme(legend.position = "right") + 
                theme(panel.background = element_rect(colour = "black")) + 
                labs(title = "Nombre de morts de la Covid-19 dans le Sud des États-Unis", caption = "Source: @SRK")
        }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
