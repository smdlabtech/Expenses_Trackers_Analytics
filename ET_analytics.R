#******** loading Packages ********
library(tm)
library(webshot)
library(stringi)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(gtsummary)
library(RColorBrewer)
library(reactable)
library(shinyjs)
library(kableExtra)
library(ggrepel)
library(plotly)
library(IRdisplay)
library(stats)
library(data.table)
library(openxlsx)                #library de ConvertToDate 
library(sp)
library(shiny)
library(leaflet)
library(DT)
library(tibble) 
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(fpp)
library(sp)

#********************************************************************************************
#******************************** User Interface ********************************************
# Definir l'interface Utilisateur pour l'app de telechargement de donnees----

ui <- shinyUI(
  
  #------ Page d'Acceuil du tableau de bord -------
  dashboardPage(skin="green",   #skin: pour changer la changer la couleur du tableau de bord
                
                ##A) Titre du tableau de bord
                # dashboardHeader(disable = TRUE),  # Enlever l'entete de la page
                dashboardHeader(title = "Expenses Tracker "),

                # ###B) Barre du Cote du Dashboard
                dashboardSidebar(disable = TRUE), # Enlever le side barre
                
                # dashboardSidebar(tags$style(type = 'text/css',".badge{min-width: 200px;}"),
                # 
                #                  sidebarMenu(
                #                    # #--- Insertion du Menu sur la barre de cote droite ---
                #                    #menuItem("Appli en cours de developpement !", tabName = 'dashboard'),
                # 
                # 
                #                  )),
                
                
                ###C) Corp de la navigation dans l'application
                dashboardBody(
                  navbarPage(
                    
                  titlePanel(""),
                  
                  
                  # # Page1-------------------------------------------------------------------------------------------------------------------------------
                  tabPanel("Import Data",icon = icon("database"),
                           
                           fluidPage(
                             #--- Affichage de la table de donnees en joli format HTML ---
                             fluidRow(
                               sidebarLayout(#-- Panneau lateral pour les entrees
                                 sidebarPanel(width = 3,
                                              
                                              #--Charger le fichier data.csv --
                                              fileInput("file1", "Upload CSV File",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
                                              
                                              tags$hr(),
                                              tags$hr(),
                                              actionButton("go", "Reset date range",icon = icon("calendar")),
                                              tags$hr(), #barre horizontale
                                              uiOutput('daterange'),
                                              
                                 ),
                                 
                                 #--- Paneau principal pour l'affichage des resultats ---
                                 mainPanel(tableOutput("contents"),
                                           column(1),column(10, reactableOutput("table1")),column(1) #Affichage de la table de Choose CSV Data File
                                 )
                               )
                             )  #End fluidRow
                             
                           )
                           
                  ), #End tabPanel
                  
                  # Page2-------------------------------------------------------------------------------------------------------------------------------
                  tabPanel("Interactive Overview",icon = icon("bar-chart-o"),
                           
                           fluidPage(
                             
                             #------------------
                             fluidRow(column(1),
                                      valueBoxOutput(width=4,'valuebox1'),
                                      valueBoxOutput(width=2,'valuebox3'),
                                      valueBoxOutput(width=2,'valuebox2'),
                                      valueBoxOutput(width=2,'valuebox4')
                             ),
                             
                             #------------------
                             fluidRow(column(1),
                                      #box(width=5,status = "danger",plotOutput('plot4',height = 300),downloadButton('Save_plot4','')),
                                      box(width=5,plotOutput('plot4',height = 300)),
                                      box(width=5,plotOutput('plot3',height = 300))
                             ), 
                             
                             #------------------
                             fluidRow(column(1),
                                      box(width=5,plotlyOutput('plot8',height = 300)),
                                      box(width=5,plotOutput('plot6',height = 300))
                                      
                             ),
                             
                             #------------------
                             fluidRow(column(1),
                                      box(width = 3,title = "Selected Months",solidHeader = FALSE,collapsible = TRUE,reactableOutput("table9")),
                                      box(width = 5,title = "Summary statistics for selected date range",solidHeader = FALSE,collapsible = TRUE,reactableOutput("table5")),
                                      box(width = 2,title = "Selected Years",solidHeader = FALSE,collapsible = TRUE,reactableOutput("table7"))
                                      
                             ),
                             
                             #------------------
                             fluidRow(column(1),
                                      box(width=5,title = "Most frequent expenses",plotOutput('plot5',height = 320)),
                                      box(width=5,title="Spenders",plotOutput('plot7',height = 320))
                             ), #End fluidRow
                             
                             #------------------
                             fluidRow(column(1),
                                      box(width=3,
                                          title="Words cloud",tatus = "warning",
                                          sliderInput("freq","Minimum frequency:", min = 1,  max = 100, value = 15),
                                          sliderInput("word","Maximum number of Words:",min = 1,  max = 400,  value = 100)
                                      )#,
                                      
                                      ,box(width = 4,title = "Spenders list",solidHeader = FALSE,collapsible = TRUE,reactableOutput("table10"))
                             ) 
                             
                           )#End FluidPage
                           
                  )#,  #End tabPanel
                  
                  # Page4------------------------------------------------------------------------------------------------------------------------------------------
                  # #tabbpanel for 3 next month expenses futures
                  # tabPanel("Next forcasts",icon=icon("line-chart"),
                  #          fluidPage(
                  #            fluidRow(
                  #              
                  #            )#End fluidRow
                  #            
                  #          ) #End FluidPage
                  #          
                  # ) #End tabPanel
                  
                )#End navbarPage 
                
                )#End dashboardBody
                
                
  ) #End dashboardPage
  
) #End shinyUI

#**************************************************************
#*************************** Server ***************************
server<-function(input, output, session) {
  
  #====================================================== df1 ======================================================
  output$contents <-reactive({
    
    req(input$file1) #requete de chargement du jeu de donnees
    
    tryCatch(
      {
        #--Upload CSV Data File--
        df1 <- read.csv(input$file1$datapat,header = T,sep = ',', stringsAsFactors=FALSE, fileEncoding="latin1")
        colnames(df1)<-c("Designation","Costs","Date","Purchase")  #Renommer les Champs de la table de donnees ("Designation","Prix","Date","Achat")
        df1$Date<-convertToDate(df1$Date)                          #convertir le format de date excel en format r date
        
        #Mise en format des variables de la table de donnees de base
        df1$Designation=as.factor(df1$Designation)
        df1$Date=as.Date(df1$Date,format = "%d/%m/%Y")   #format de la date (jj/mm/aaaa)
        df1$Costs=as.numeric(df1$Costs)                    
        df1$Purchase=as.factor(df1$Purchase)
        
        #Creation de la table Costs_daysum
        Costs_daysum<-aggregate(Costs~Date, data=df1, FUN = sum)
        Costs_daysum$Month=month(Costs_daysum$Date)     #Ajout du mois de depenses (Ajout la colonne: month)
        Costs_daysum$Year=year(Costs_daysum$Date)       #Ajout de l'annee de depenses (Ajout la colonne: year)
        
        #Mise en format entier des mois
        Costs_daysum$Month=as.integer(Costs_daysum$Month)
      },
      
      error = function(e) { #Gestion des erreurs
        stop(safeError(e)) 
      }
    )   
    
    
    #-------------------------------------- Creation de la daterange --------------------------------------------
    
    #Ajout de la daterange pour permettant l'affichage de la df1 apres mise a jour
    output$daterange <- renderUI({
      dateRangeInput(inputId = "daterange",label = "Select a date range",
                     start = min(df1$Date),      # Start date
                     end = max(df1$Date),        # End date
                     min = min(df1$Date),        # The min allowed date
                     max = max(df1$Date),        # The max allowed date
                     format = "yyyy/mm/dd",      # The format of the date to display in the browser. try "mm/dd/yy"  
                     separator = "to"            # String to display between the start and end input boxes
      )
      
    })
    
    #-------------------- Mise a jour de la table df1 en fonction de la daterange selectionnee -------------------
    
    #Mettre a jour la table df1 apres les selection des dates de debut et de fin 
    Update_df1 <- reactive({
      comp.reg.final <- df1[which(df1$Date >= input$daterange[1] & df1$Date <= input$daterange[2]),]
      return (comp.reg.final)
      
    })
    
    #Remmettre a jour la plage de date de depart(Reset data range)
    observeEvent(input$go , {
      updateDateRangeInput(session, "daterange", start = min(df1$Date), end = max(df1$Date)
      )
    })
    
    #--------------------------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------------------------
    #                                   Interactive plot
    #
    #------------------------- Interactive plot : plot3 and Save (.img, .png) -------------------------
    # #graphique d'evolution 
    output$plot3<-renderPlot({
      
      Purchase_grp<-Update_df1() %>%
        group_by(Purchase) %>%
        summarise(Costs = sum(Costs)) %>%
        mutate(rank = dense_rank(desc(Costs))) %>%
        arrange(desc(Costs))
      
      Purchase_grp<-data.frame(Purchase_grp)
      
      ggplot(data = Purchase_grp,aes(x=reorder(Purchase,Costs),y=Costs),label=Costs)+
        geom_bar(stat = "identity",fill = "#8f1a09",color = "white")+coord_flip() + 
        ggtitle("Distribution of types purchases")
      
    })
    
    # #------------------------- Interactive plot : plot4 and Save (.img, .png) -----------------------------
    output$plot4<-renderPlot({
      ggplot(Update_df1(), aes(Purchase, Costs, fill = factor(Purchase))) +
        geom_boxplot()+
        ggtitle("Boxplot of the purchase types")
    })
    
    #----
    #Configuration du telechargement du barplot
    output$Save_plot4<-downloadHandler(
      filename = function(){ paste("plot4","png",sep = ".")},
      
      content=function(file){
        png(file)
        plot(
          ggplot(Update_df1(), aes(Purchase, Costs, fill = factor(Purchase))) +
            geom_boxplot()+
            ggtitle("Boxplot of the purchase types")
        )
        
        dev.off()
      }
      
    ) #End Save_plot4
    
    
    # #------------------------- Interactive plot : plot5 and Save (.img, .png) -----------------------------
    #Words cloud consumption
    output$plot5<-renderPlot({
      text <- Update_df1() %>% 
        pull(Designation)
      
      corpus <- stri_trans_general(text, "latin-ascii")
      corpus <- Corpus(VectorSource(corpus))
      
      dtm <- TermDocumentMatrix(corpus)
      matrix <- as.matrix(dtm)
      words <- sort(rowSums(matrix),decreasing=TRUE) 
      df <- data.frame(word = names(words),freq=words)
      
      set.seed(1234) # for reproducibility
      wordcloud(words = df$word,freq = df$freq,min.freq = input$freq,
                max.words = input$word,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))
      
    }) #End output
    
    # #------------------------- Interactive plot : plot6 and Save (.img, .png) -----------------------------
    output$plot6<-renderPlot({
      
      Costs_day_sum<-aggregate(Costs~Date, data=Update_df1(), FUN = sum)
      Costs_day_sum$Month=month(Costs_day_sum$Date)
      
      #Cost_Month
      Costs_month_sum<-Costs_day_sum %>%
        group_by(Month) %>%
        summarise(Costs = sum(Costs)) 
      
      Costs_month_sum %>%
        ggplot(aes(x=Month, y=Costs)) + geom_line(color = "red") +
        scale_x_continuous(breaks = 1:12) + expand_limits(y = 0)+
        ggtitle("Evolution of Costs following each month")
      
    })
    
    # #------------------------- Interactive plot : plot7 and Save (.img, .png) -----------------------------
    # #graphique du Top 10 spenders
    output$plot7<-renderPlot({
      Desig_grp<-Update_df1() %>%
        group_by(Designation) %>%
        summarise(Costs = sum(Costs)) %>%
        mutate(rank = dense_rank(desc(Costs))) %>%
        arrange(desc(Costs))
      
      Desig_grp<-data.frame(Desig_grp)
      Desig_grp_top10<-head(Desig_grp,10) #Afficher le Top5 des plus depensiers
      
      #graphique barplot
      ggplot(data = Desig_grp_top10,mapping = aes(x=reorder(Designation,Costs),y=Costs),label=Costs)+
        geom_bar(stat = "identity",fill = "#f89e0c",color = "white")+
        geom_label(aes(y = Costs, label = round(Costs, 2)))+ #etiquettes sur les designation
        coord_flip() + ggtitle("Top 10 spenders")
      
    })
    
    # #------------------------- Interactive plot : plot8 and Save (.img, .png) -----------------------------
    # #donut chart of Purchase
    output$plot8<-renderPlotly({
      
      Purchase_donut<-Update_df1() %>%
        group_by(Purchase) %>%
        summarise(Costs = sum(Costs)) %>%
        mutate(pct= round(prop.table(Costs),4)) %>%
        arrange(desc(Costs))
      
      Purchase_donut<-data.frame(Purchase_donut)
      
      #creation de la palette de couleur en fonction des Types d'Achats (Palette de Couleur : "Spectral", "RdYlGn", "BrBG", "RdYlBu")
      colourCount <- length(unique(Purchase_donut$pct))         # number of levels
      getPalette <- colorRampPalette(brewer.pal(9, "Set1"))   # definition de la palette de couleur
      
      plot_ly(data = Purchase_donut, labels = ~Purchase, values = ~pct, sort= FALSE,
              marker= list(colors=colorRampPalette(brewer.pal(11,"RdYlGn"))(colourCount), line = list(color="black", width=1))) %>%
        add_pie(hole = 0.6) %>%
        layout(title="Donut Chart of purchases")
      
      
    })
    
    #----------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------
    #                            Interactive valueBox
    #
    #------------------------- Interactive valuebox : valuebox1 and Save (.img, .png) -------------------------
    output$valuebox1<- renderValueBox({
      Total_costs<-Update_df1() %>%
        group_by(Purchase) %>%
        summarise(Costs = sum(Costs))
      
      Total_costs=sum(Total_costs$Costs)
      
      #--------
      valueBox(
        paste0(Total_costs," EUR"), "Total expenses", icon = icon("credit-card"),
        color = "purple"
      )
    })
    
    #------------------------- Interactive valuebox : valuebox2 and Save (.img, .png) -------------------------
    output$valuebox2<- renderValueBox({
      
      Total_big_grp<-Update_df1() %>%
        group_by(Designation) %>%
        summarise(Costs = sum(Costs)) %>%
        mutate(rank = dense_rank(desc(Costs))) %>%
        mutate(pct= 100*round(prop.table(Costs),4)) %>%   #Pourcentage que prend le top10 des depenses
        arrange(desc(Costs))
      
      Total_big_grp<-data.frame(Total_big_grp)
      Total_big_grp_top10<-head(Total_big_grp,10) #Afficher le Top10 des plus depensiers
      #Total_big_grp_costs<-sum(Total_big_grp_top10$Costs)      #Somme des couts des depenses des top10
      Total_big_grp_costs<-sum(Total_big_grp_top10$pct)         #Somme des %centage des couts des depenses des top10
      
      
      valueBox(
        paste0(Total_big_grp_costs," %"), "Top 10 spenders", icon = icon("list"), #icon = icon("credit-card"),
        color = "blue"
      )
    })
    
    #------------------------- Interactive valuebox : valuebox3 and Save (.img, .png) -------------------------
    output$valuebox3<- renderValueBox({
      
      Top_Purchase<-Update_df1() %>%
        group_by(Purchase) %>%
        summarise(Costs = sum(Costs)) %>%
        mutate(pct= 100*round(prop.table(Costs),4)) %>%
        arrange(desc(Costs))
      
      Top_Purchase<-data.frame(Top_Purchase)
      Top_Purchase_top10<-head(Top_Purchase,1)      #Afficher le Top5 des plus depensiers
      
      Top1_Purchase<-Top_Purchase_top10$pct
      Top1_Purchase_name<-Top_Purchase_top10$Purchase
      
      valueBox(
        paste0(Top1_Purchase," %"), Top1_Purchase_name, icon = icon("list"),
        color = "aqua"
      )
    })
    
    #------------------------- Interactive valuebox : valuebox4 and Save (.img, .png) -------------------------
    output$valuebox4<- renderValueBox({
      
      Desig_grp<-Update_df1() %>%
        group_by(Designation) %>%
        summarise(Costs = sum(Costs)) %>%
        arrange(desc(Costs))
      
      valueBox(
        paste0(nrow(Desig_grp)), "spenders",
        color = "blue"
      )
    })
    
    #--------------------------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------------------------
    #                            Interactive table
    #
    #------------------------- Interactive table : table1 and Save (.imgn .png) -------------------------
    output$table1 <- renderReactable({ #sortie table df1 en mode rectable
      reactable(
        Update_df1(),
        compact = TRUE,
        resizable = TRUE,            #Rendre la table redimensionnable avec resizeable
        searchable = TRUE,defaultPageSize = 10, 
        
        #Mise en forme de table de donnees
        columns = list(
          Date = colDef(format = colFormat(date = TRUE, locales = "en-GB")),
          Designation = colDef(footer = "Total"),
          
          #customization du total des couts
          Costs = colDef(footer = JS("function(colInfo) {
          var total = 0
          colInfo.data.forEach(function(row) {
            total += row[colInfo.column.id]})
          return total.toFixed(2)+' EUR'}")
                         
          ,format = colFormat(currency = "EUR"))
          
        ),
        
        #theme et sortie du tableau et de la fenetre de recherche
        theme = reactableTheme( 
          searchInputStyle = list(width = "100%"),
          
          headerStyle = list(
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),borderColor = "#555")
        ),
        
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold")) #Mise en forme du footer 
      )
      
    })
    
    
    #------------------------- Interactive table : table5 and Save (.img, .png) -------------------------
    output$table5 <- renderReactable({
      reactable(Update_df1() %>%
                  group_by(Purchase) %>%
                  summarise(Costs = sum(Costs)) %>%
                  mutate(pct= round(prop.table(Costs),4)) %>%
                  arrange(desc(Costs))
                
                ,searchable = TRUE,
                
                columns = list(
                  Costs=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
                  pct = colDef(format = colFormat(percent = TRUE, digits = 1))
                )
                
                ,defaultPageSize = 8
                
      )
    })
    
    #------------------------------- Interactive table: table7 (Year)-----------------
    output$table7 <- renderReactable({
      # Add Year column
      Costs_day_sum<-aggregate(Costs~Date, data=Update_df1(), FUN = sum)
      Costs_day_sum$Year=year(Costs_day_sum$Date)
      
      #Total costs per year selected
      Costs_year_sum<-Costs_day_sum %>%
        group_by(Year) %>%
        summarise(Costs = sum(Costs))
      
      reactable(Costs_year_sum,
                columns = list(
                  Costs=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE"))
                )
      )
    })
    
    ##------------------------------- Interactive table: table9 (Month)-----------------
    output$table9 <- renderReactable({
      # Add Month column
      Costs_day_sum<-aggregate(Costs~Date, data=Update_df1(), FUN = sum)
      Costs_day_sum$Month=month(Costs_day_sum$Date)
      
      #Cost_Month
      Costs_month_sum<-Costs_day_sum %>%
        group_by(Month) %>%
        summarise(Costs = sum(Costs)) %>%
        mutate(rank = dense_rank(desc(Costs))) %>%
        mutate(Month=month.abb[Month])
      
      reactable(Costs_month_sum,
                columns = list(
                  Costs=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE"))
                )
      )
    })
    
    #------------------------------------------------------------------------------------
    ##------------------------------- Interactive table: table10 (Month)-----------------
    ## Afficher les designations les plus depensiers
    output$table10 <- renderReactable({
      reactable(
        Update_df1() %>%
          group_by(Designation) %>%
          summarise(Costs = sum(Costs)) %>%
          mutate(rank = dense_rank(desc(Costs))) %>%
          arrange(desc(Costs))
        
        ,searchable = TRUE,
        
        columns = list(
          Costs=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE"))
        )
        
        ,defaultPageSize = 4
      )
      
    })
    
    #------------------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------------------  
    
  }) #output$contents
  
} #-------- Accolade de fermeture --------
#End function(input, output, session)

#************ Creation of Shiny App ************
# Create Shiny app ----
shinyApp(ui, server)
