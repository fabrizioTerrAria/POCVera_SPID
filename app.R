#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# https://stackoverflow.com/questions/42379402/how-to-terminate-reactivetimer-in-shiny
# https://stackoverflow.com/questions/50650616/stream-system-output-to-shiny-front-end-continuously

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bs4Dash)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(sf)
library(dplyr)
library(reactable)
library(reactable.extras)
library(reactablefmtr)
library(highcharter)
library(password)
library(sendmailR)
library(tools)
library(tippy)
library(fs)
library(ipc)
library(future)
library(promises)
plan(multisession)


source("global.R")
source("functions.R")
source("functionsDB.R")


shinyApp(
  ui = dashboardPage(
    title = "VERA",
    fullscreen = TRUE,
    help = NULL,
    dark = NULL,
    scrollToTop = FALSE,
      header = dashboardHeader(
      title = dashboardBrand(
        title = "POC VERA",
        color = "info",
        href = "https://www.lepida.net/",
        image = "logo_POC.png",
        opacity = 1
      ),
      .list = "Gemella Digitale dell'Emilia Romagna",
      skin = "light",
      status = "white",
      border = TRUE,
      fixed = FALSE,
      rightUi = tagList(
        # tags$li(class = "dropdown", downloadBttn(outputId = "downloadUserGuide",label = "User Guide",style = "bordered",color = "primary")),
        userOutput("user")
      )
    ),
    sidebar = bs4DashSidebar(
      id = "sidebar",
      skin = "light",
      elevation = 3,
      status = "info",
      minified = TRUE,
      sidebarMenu(
        id = "main_menu",
        sidebarHeader("Menu"),
        bs4Dash::menuItem(
          text = "Home",
          tabName = "home",
          icon = icon("house", lib = "font-awesome")
        ),
        bs4Dash::menuItem(
          text = "Crea Scenario",
          tabName = "scenario",
          icon = icon("chart-line", lib = "font-awesome")
        ),
        bs4Dash::menuItem(
          text = "Dashboard",
          tabName = "dashboard",
          icon = icon("dashboard", lib = "font-awesome")
        ),
        bs4Dash::menuItem(
          text = "Emissioni",
          tabName = "emissions",
          icon = icon("industry", lib = "font-awesome")
        ),
        bs4Dash::menuItem(
          text = "Concentrazioni",
          tabName = "concentrations",
          icon = icon("wind", lib = "font-awesome")
        ),
        sidebarHeader("Menu utente"),
        bs4Dash::menuItem(
          text = "Gestione Scenari",
          tabName = "scenarioConfig",
          icon = icon("list-check", lib = "font-awesome")
        ),
        uiOutput('sidebar_admin'),
        uiOutput('menu_admin')
        # uiOutput('menu_config')

      )
    ),
    footer = bs4DashFooter(

      left = HTML(paste0(
		a(img(src='lepida_logo_viola.svg',height='35'),href="https://www.lepida.net/",target = "_blank",rel="noreferrer",id = "logo1"), "&nbsp;", "&nbsp;",
        a(img(src='ARPAE.svg',height='35'),href="https://www.arpae.it/it",target = "_blank",rel="noreferrer",id = "logo2"), "&nbsp;", "&nbsp;",
        a(img(src='logo_RER.jpg',height='35'),href="https://www.regione.emilia-romagna.it/",target = "_blank",rel="noreferrer",id = "logo3"), "&nbsp;", "&nbsp;",
        img(src='Loghi_CoesioneITA_UE_REP_ER_21-27_compatta_colore.jpg',height='35')
      )),
      right = HTML(paste0("Sviluppato da:&nbsp;",a(img(src='logo_Terraria_Ridimensionato.png',height='35'),href="https://www.terraria.com",target = "_blank",rel="noreferrer",id = "logo4"))),
      fixed = TRUE
      
    ),
    body = dashboardBody(
      
      useShinyjs(),
      reactable.extras::reactable_extras_dependency(),
      # useShinyFeedback(),
      
      # tags$head(tags$script("Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = './noAuth.html';});")),
      tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
      tags$head(tags$link(rel="icon", href="https://www.ifabfoundation.org/wp-content/uploads/2020/10/Favicon.svg")),
      tags$head(includeCSS("styles.css")),
      tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
      
      ## funzione Javascript per aggiornare le mappe una volta massimizzato il box che le contiene
      tags$script(
        "$(function() {
            $('[data-card-widget=\"maximize\"]').on('click', function() {
              $('#emissionsMap').trigger('resize');
							$('#mapEmiAbsPanel').insertAfter($('#emissionsMap'));
							$('#concentrationsMap').trigger('resize');
							$('#mapConcAbsPanel').insertAfter($('#concentrationsMap'));

            });
          });
          "
      ),
      
      ## funzione javascript per creare link ai tab
      tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      ')),
      
      ## funzione javascript per il testo nei pickerInput
      tags$script(
        HTML("$.fn.selectpicker.Constructor.DEFAULTS.countSelectedText = function(x,y) {
            if (x < y) {
                return 'Selezionati: ' + x + ' / ' + y;
            } else {
                return '< Selezionati tutti >';
            }
        };")
      ),
  
      
      tabItems(
        
        ####################
        # HOME
        ####################
        
        tabItem(tabName = "home",

                  fluidRow(
                    # column(width = 4, img(src='logo_POC.png', height = "120px",style="display: block; margin-left: auto; margin-right: auto;")),
                    # column(width = 8, h1(html("Benvenuti nel POC di VERA</br>Gemella digitale dell'Emilia Romagna"), align = "center"))
                    
                    uiOutput('main_ui'),
                    
                    
                    userBox(
                      title = userDescription(
                        image = "logo_POC.png",
                        title = "Benvenuti nel POC di VERA",
                        subtitle = "Gemella digitale dell'Emilia Romagna"
                      ),
                      collapsible = FALSE,
                      width = 12,
                      status = "info",
                      elevation = 4,
                      
                      br(),
                      fluidRow(
                        
                        box(title = tags$strong("Sommario"), width = 6, collapsible = F, collapsed = F, status = "info", solidHeader = FALSE,
                            align = 'left', height = 450, id = "sommarioBox",
                            
                            p("Attraverso quest'applicazione è possibile valutare scenari di traffico a livello regionale e comunale."),
                            fluidRow(
                              column(width = 3, icon("chart-line", lib = "font-awesome"), a(tags$span("Crea Scenario"), href="#", onclick = "fakeClick('scenario')")),
                              column(width = 9, 
                                     tags$div(style="text-align: justify;",HTML(paste0("Attraverso il modulo ", tags$strong("Crea scenario", style="color: #17a2b8;")," è possibile definire un nuovo scenario di traffico:"))),
                                     tags$li("modificando i flussi di traffico sulla rete stradale regionale"),
                                     tags$li("modificando le politiche comunali di blocco del traffico su una o più aree urbane"),
                                     tags$li("applicando riduzioni emissive a livello comunale"))
                            ),
                            br(),
                            fluidRow(
                              column(width = 3, icon("dashboard", lib = "font-awesome"), a(tags$span("Dashboard"), href="#", onclick = "fakeClick('dashboard')")),
                              column(width = 9, tags$div(style="text-align: justify;",HTML(paste0("Nel modulo ", tags$strong("Dashboard", style="color: #17a2b8;")," è possibile confrontare più scenari contemporaneamente sia a livello regionale che per singolo comune"))))
                            ),
                            br(),
                            fluidRow(
                              column(width = 3, icon("industry", lib = "font-awesome"), a(tags$span("Emissioni"), href="#", onclick = "fakeClick('emissions')")),
                              column(width = 9, tags$div(style="text-align: justify;",HTML(paste0("Nel modulo ", tags$strong("Emissioni", style="color: #17a2b8;")," è possibile valutare la distribuzione spaziale delle emissioni a livello comunale e su griglia dei diversi inquianti"))))
                            ),
                            br(),
                            fluidRow(
                              column(width = 3, icon("wind", lib = "font-awesome"), a(tags$span("Concentrazioni"), href="#", onclick = "fakeClick('concentrations')")),
                              column(width = 9, tags$div(style="text-align: justify;",HTML(paste0("Nel modulo ", tags$strong("Concentrazioni", style="color: #17a2b8;")," è possibile valutare la distribuzione spaziale delle concentrazioni a livello comunale e su griglia dei diversi inquianti"))))
                            ),
                            br(),
                            fluidRow(
                              column(width = 3, icon("list-check", lib = "font-awesome"), a(tags$span("Gestione scenari"), href="#", onclick = "fakeClick('scenarioConfig')")),
                              column(width = 9, tags$div(style="text-align: justify;",HTML(paste0("Nel modulo ", tags$strong("Gestione scenari", style="color: #17a2b8;")," è possibile visualizzare, modificare ed eliminare gli scenari creati"))))
                            )
                            
                            
                        ),
                        
                        box(title = tags$strong("Riferimenti"), width = 6, collapsible = F, collapsed = F, status = "info", solidHeader = FALSE,
                            align = 'left', height = 450, id = "riferimentiBox",
                            
                            tags$li(tags$span(style="text-align: justify;",HTML(paste0(tags$strong("Grafo stradale e flussi di traffico", style="color: #17a2b8;"))),tags$br())),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("Il grafo stradale deriva dalla simulazioni dei flussi di traffico del ", tags$a("modello regionale dei trasporti",href="https://mobilita.regione.emilia-romagna.it/pianificazione/modellazione-dei-trasporti",target = "_blank", rel="noreferrer"), " per l'anno 2023."))),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("La distribuzione temporale dei flussi è stata effettuata utilizzando i dati del ", tags$a("sistema di monitoraggio regionale",href="https://mobilita.regione.emilia-romagna.it/pianificazione/monitoraggio-del-sistema-di-mobilita",target = "_blank", rel="noreferrer"), " per l'anno 2023."))),
                            
                            tags$li(tags$span(style="text-align: justify;",HTML(paste0(tags$strong("Parco veicoli regionale", style="color: #17a2b8;"))),tags$br())),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("La distribuzione del parco veicolare circolante è stata estratta dai ", tags$a("dati ACI",href="https://www.aci.it/laci/studi-e-ricerche/dati-e-statistiche/autoritratto/autoritratto-2023.html",target = "_blank", rel="noreferrer"), " per l'anno 2023."))),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("I valori del numero di autobus deriva dal parco TPL di regione Emilia Romagna per l'anno 2021."))),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("I valori del numero di ciclomotori deriva dalle stima ISPRA per l'anno 2021."))),
                            
                            tags$li(tags$span(style="text-align: justify;",HTML(paste0(tags$strong("Residenti", style="color: #17a2b8;"))),tags$br())),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("Il numero dei cittadini residenti a livello comunale deriva dai ", tags$a("dati demografici ISTAT",href="https://demo.istat.it/",target = "_blank", rel="noreferrer"), " per l'anno 2023."))),
                            
                            tags$li(tags$span(style="text-align: justify;",HTML(paste0(tags$strong("Emissioni", style="color: #17a2b8;"))),tags$br())),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("Il calcolo delle emissioni è stato effettuato secondo la ", tags$a("metodologia INEMAR.",href="https://www.inemar.eu/xwiki/bin/view/InemarWiki/",target = "_blank", rel="noreferrer")))),
                            
                            tags$li(tags$span(style="text-align: justify;",HTML(paste0(tags$strong("Concentrazioni", style="color: #17a2b8;"))),tags$br())),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("La stima delle concentrazioni utilizza la metodologia di calcolo derivante dal ", tags$a("progetto SHERPA del JRC.",href="https://academy.europa.eu/courses/sherpa-a-tool-to-support-the-design-of-urban-regional-air-quality-plans",target = "_blank", rel="noreferrer")))),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("Le funzioni sorgente/recettore sono state addestrate da ARPAE per la definizione degli scenari del ", tags$a("PAIR 2030.",href="https://ambiente.regione.emilia-romagna.it/it/aria/pair-2030",target = "_blank", rel="noreferrer")))),
                            tags$div(style="text-align: justify; margin-left: 20px;",HTML(paste0("Le concentrazioni delle scenario di riferimento sono calcolate usando le emissioni relative all'anno 2017 usate nel PAIR2030.")))
                        
                        )
                      
                      
                    ),
                    
                    fluidRow(
                      column(width = 12, align = "center", tags$div("Partner di progetto:")),
                      column(width = 12, align = "center", HTML(paste0(a(img(src='lepida_logo_viola.svg',height='35'),href="https://www.lepida.net/",target = "_blank",rel="noreferrer",id = "logo1"), "&nbsp;", "&nbsp;",
                                                                       a(img(src='ARPAE.svg',height='35'),href="https://www.arpae.it/it",target = "_blank",rel="noreferrer",id = "logo2"), "&nbsp;", "&nbsp;",
                                                                       a(img(src='logo_RER.jpg',height='35'),href="https://www.regione.emilia-romagna.it/",target = "_blank",rel="noreferrer",id = "logo3")))),
                      column(width = 12, align = "center", tags$div("Progetto realizzato grazie ai Fondi europei della Regione Emilia-Romagna:")),
                      column(width = 12, align = "center", HTML(paste0(img(src='Loghi_CoesioneITA_UE_REP_ER_21-27_compatta_colore.jpg',height='45')))),
                      column(width = 12, align = "center", tags$div("Sviluppato da:")),
                      column(width = 12, align = "center", HTML(paste0(a(img(src='logo_Terraria_Ridimensionato.png',height='35'),href="https://www.terraria.com",target = "_blank",rel="noreferrer",id = "logo4"))))
                      
                    )
                    
                  )
                
                  
                    
                    
                    
                  )
                ),
        
        
        ####################
        # CREA SCENARIO
        ####################


        tabItem(tabName = "scenario",

                fluidRow(
                  column(width = 12,
                         fluidRow(
                           column(width = 4, textInput(inputId = "scenarioName",label = "Nome scenario",placeholder = "Inserisci il nome dello scenario...", width = '100%')),
                           column(width = 8, textInput(inputId = "scenarioDescription",label = "Descrizione scenario",placeholder = "[Opzionale] Inserisci una descrizione...", width = '100%'))
                         )
                  )
                ),
                hr(),
                
                fluidRow(
                  column(width = 8,align = 'center',
                         
                         prettyRadioButtons(
                           inputId = "defineTipoScenarioOptions",
                           label = "Scegli un'opzione:",
                           choices = SCENARIO_DEFINITION_CHOICES,
                           inline = TRUE
                         )
                         
                  ),
                  
                  column(width = 4,
                         div(style="padding-top:27px;",
                          materialSwitch(inputId = "runWithML",label = "Usa ML per il calcolo delle concentrazioni", value = TRUE, status = "info", inline = TRUE)
                         )
                         
                  )
                ),
                
                column(width = 12,
                  
                  div(id = "modificoGrafo",
                      
                      socialBox(
                        title = userBlock(
                          image = "road-solid.svg",
                          title = "Modifica i dati di traffico sul grafo stradale regionale",
                          subtitle = "Tramite quest'opzione è possibile agire direttamente sul traffico lineare, modificando i flussi di veicoli sulla rete stradale regionale."
                        ),
                        status = "info",
                        maximizable = FALSE,
                        collapsible = FALSE,
                        boxToolSize = "sm",
                        width = 12,
                      
                        fluidRow(h6("Carica un file csv ...")),
                        fluidRow(
                          column(width = 2,
                                 downloadButton(outputId = "downloadTemplateTrafficButton",label = "Scarica CSV template", class = "btn-info2", icon = icon("download"))
                          ),
                          column(width = 6,
                                 fileInput("trafficFile", "Carica un file CSV",
                                           multiple = FALSE,
                                           accept = c(".csv"),
                                           width = "100%")
                          ),
                          column(width = 4, align = 'right',
                                 actionButton(inputId = "updateTrafficButton",label = "Aggiorna", class = "btn-success",icon = icon("forward"))
                          )
                        ),
                        hr(),
                        fluidRow(h6("... oppure modifica i dati nella tabella")),
                        fluidRow(
                          
                          column(6, 
                                 fluidRow(
                                   column(width = 3, pickerInput(inputId = "comuniChoiceScenariLineari",label = "Comune", width = "100%", choices = NULL)),
                                   column(width = 3, pickerInput(inputId = "tipoStradaChoiceScenariLineari",label = "Tipo Strada", width = "100%", choices = NULL)),
                                   column(width = 3, pickerInput(inputId = "tipoVeicoloChoiceScenariLineari",label = "Tipo Veicolo", width = "100%", choices = NULL)),
                                   column(width = 2, textInput(inputId = "riduzioneScenariLineari",label = "Var. [%]", placeholder = "0%", width = '100%')),
                                   column(width = 1, actionButton(inputId = "applicaRiduzioneScenariLineari",label = "", class = "btn-success2",icon = icon("sync-alt"),width = "100%"))
                                 ),
                                 fluidRow(
                                   reactableOutput("trafficScenarioTable", height = 500, width = "100%")
                                 )
                          ),
                          column(6, leafletOutput(outputId = "trafficScenarioMap", height = 500))
                          
                        )
                        
                      )
                    
                    ),
                  
                  hidden(
                    div(id = "politicaUrbana",
                        
                        socialBox(
                          title = userBlock(
                            image = "car-solid.svg",
                            title = "Definisci una politica a livello comunale",
                            subtitle = "Tramite quest'opzione è possibile modificare il parco auto in circolazione per uno o più comuni."
                          ),
                          status = "info",
                          maximizable = FALSE,
                          collapsible = FALSE,
                          boxToolSize = "sm",
                          width = 12,
                      
                          box(title = "Seleziona i comuni su cui applicare la politica", width = 12, collapsible = T, collapsed = FALSE, status = "info", solidHeader = FALSE,
                              
                              fluidRow(h6("Carica un file csv ...")),
                              fluidRow(
                                column(width = 2,
                                       downloadButton(outputId = "downloadTemplateComuniButton",label = "Scarica CSV template",class = "btn-info2",icon = icon("download"))
                                ),
                                column(width = 6,
                                       fileInput("comuniFile", "Carica un file CSV",
                                                 multiple = FALSE,
                                                 accept = c(".csv"),
                                                 width = "100%")
                                ),
                                column(width = 4, align = 'right',
                                       actionButton(inputId = "updateComuniSelezionatiButton",label = "Aggiorna", class = "btn-success",icon = icon("forward"))
                                )
                              ),
                              hr(),
                              fluidRow(h6("... oppure modifica i dati nella tabella")),
                              fluidRow(
                                
                                column(6, 
                                       fluidRow(
                                         column(width = 3, pickerInput(inputId = "comuniChoiceScenarioDiffuse",label = "Comune", width = "100%", choices = NULL)),
                                         column(width = 3, pickerInput(inputId = "residentiChoiceScenarioDiffuse",label = "Residenti", width = "100%", choices = NULL)),
                                         column(width = 3, pickerInput(inputId = "classeClimaChoiceScenarioDiffuse",label = "Classe climatica", width = "100%", choices = NULL)),
                                         column(width = 3, pickerInput(inputId = "zonaChoiceScenarioDiffuse",label = "Zona", width = "100%", choices = NULL))
                                         # column(width = 2, textInput(inputId = "riduzioneScenarioDiffuse",label = "Rid. [%]", placeholder = "0%", width = '100%')),
                                         # column(width = 1, actionButton(inputId = "applicaRiduzioneScenariLineari",label = "", class = "btn-success2",icon = icon("sync-alt"),width = "100%"))
                                       ),
                                       fluidRow(
                                         reactableOutput("diffuseScenarioTable", height = 500, width = "100%")
                                       )
                                ),
                                column(6, leafletOutput(outputId = "diffuseScenarioMap", height = 500))
                                
                              )
                              
                          ),
                          
                          box(title = "Definisci la politica", width = 12, collapsible = T, collapsed = T, status = "info", solidHeader = FALSE,
                              # align = 'center',
                              
                              fluidRow(h6("Carica un file csv ...")),
                              fluidRow(
                                column(width = 2,
                                       downloadButton(outputId = "downloadTemplateParcoButton",label = "Scarica CSV template", class = "btn-info2",icon = icon("download"))
                                ),
                                column(width = 6,
                                       fileInput("parcoFile", "Carica un file CSV",
                                                 multiple = FALSE,
                                                 accept = c(".csv"),
                                                 width = "100%")
                                ),
                                column(width = 4, align = 'right',
                                       actionButton(inputId = "updateParcoButton",label = "Aggiorna", class = "btn-success",icon = icon("forward"))
                                )
                              ),
                              hr(),
                              fluidRow(h6("... oppure modifica i dati nella tabella")),
                              hidden(
                                fluidRow(
                                  column(width = 12,
                                         prettyRadioButtons(
                                           inputId = "defineTipoPoliticaOptions",
                                           label = "Scegli un'opzione:",
                                           choices = SCENARIO_TIPO_POLITICA_CHOICES,
                                           selected = SCENARIO_TIPO_POLITICA_CHOICES[2],
                                           inline = TRUE
                                         )
                                         
                                  )
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       div(id = "convertiParcoBox",
                                           fluidRow(h6("Selezionando quest'opzione il numero totale di veicoli non cambia. Si modifica la distribuzione nelle diverse categorie.")),
                                           fluidRow(column(12, reactableOutput("convertiParcoScenarioTable"))),
                                           fluidRow(
                                             column(6, fluidRow(highchartOutput("convertiParcoScenarioGraphEuro", height = 250))),
                                             column(6, fluidRow(highchartOutput("convertiParcoScenarioGraphFuel", height = 250)))
                                           )
                                       ),
                                       hidden(
                                         div(id = "modificaParcoBox",
                                             fluidRow(h6("Selezionando quest'opzione si modifica il numero totale di veicoli in circolazione.")),
                                             fluidRow(
                                               column(width = 2.5, class = "col-sm-2-5", pickerInput(inputId = "settoreModificaParco",label = "Settore", width = "100%", choices = NULL, multiple = TRUE)),
                                               column(width = 2.5, class = "col-sm-2-5", pickerInput(inputId = "combustibileModificaParco",label = "Combustibile", width = "100%", choices = NULL, multiple = TRUE)),
                                               column(width = 2.5, class = "col-sm-2-5", pickerInput(inputId = "classificazioneModificaParco",label = "Classificazione", width = "100%", choices = NULL, multiple = TRUE)),
                                               column(width = 2.5, class = "col-sm-2-5", pickerInput(inputId = "tipoEuroModificaParco",label = "Tipo Euro", width = "100%", choices = NULL, multiple = TRUE)),
                                               column(width = 1, textInput(inputId = "riduzioneModificaParco",label = "Var. [%]", placeholder = "0%", width = '100%')),
                                               column(width = 1, actionButton(inputId = "applicaRiduzioneModificaParco",label = "", class = "btn-success2",icon = icon("sync-alt"),width = "100%"))
                                               
                                             ),
                                             fluidRow(column(12, reactableOutput("modificaParcoScenarioTable"))),
                                             fluidRow(
                                               column(6, fluidRow(highchartOutput("modificaParcoScenarioGraphEuro", height = 250))),
                                               column(6, fluidRow(highchartOutput("modificaParcoScenarioGraphFuel", height = 250)))
                                             )
                                         )
                                       )
                                )
                              )
                          ),
                          
                          box(title = "Opzioni di calcolo", width = 12, collapsible = T, collapsed = T, status = "info", solidHeader = FALSE,
                              
                              fluidRow(
                                
                                # column(width = 4, pickerInput(inputId = "fasciaOraria",label = "Seleziona fascia Oraria", width = "100%", choices = NULL, multiple = TRUE,
                                #                               options = list(
                                #                                 `actions-box` = TRUE,
                                #                                 `selected-text-format` = "count > 2",
                                #                                 `count-selected-text` = "< Tutte le fasce orarie >"))),
                                # column(width = 4, pickerInput(inputId = "giornoTipo",label = "Selezione giorno Tipo", width = "100%", choices = NULL, multiple = TRUE,
                                #                               options = list(
                                #                                 `actions-box` = TRUE,
                                #                                 `selected-text-format` = "count > 2",
                                #                                 `count-selected-text` = "< Tutti i giorni >"))),
                                # column(width = 4, pickerInput(inputId = "stagione",label = "Seleziona la stagione", width = "100%", choices = NULL, multiple = TRUE,
                                #                               options = list(
                                #                                 `actions-box` = TRUE,
                                #                                 `selected-text-format` = "count > 3",
                                #                                 `count-selected-text` = "< Tutte le stagioni >"))),
                                column(width = 4, 
                                         div(style="padding-top:33px;",
                                           materialSwitch(inputId = "applicaAlleLineari",label = "Applica la politica anche al traffico lineare", value = FALSE, status = "info")
                                         )
                                      ),
                                column(width = 8, hidden( pickerInput(inputId = "tipoStradaDiffuse",label = "Applica alle strade:", choices = NULL, multiple = TRUE,
                                                                      options = list(
                                                                        `actions-box` = TRUE,
                                                                        `selected-text-format` = "count > 6",
                                                                        `count-selected-text` = "< Tutte le strade >")))
                                      )
                                
                              )
                              
                          )
                          
                        )
                        
                    )  
                  ),
                  
                  hidden(
                    div(id = "riduzioniEmissive",
                        
                        socialBox(
                          title = userBlock(
                            image = "cloud-solid.svg",
                            title = "Applica riduzioni emissive",
                            subtitle = "Tramite quest'opzione è possibile ridurre le emissioni da traffico per uno o più comuni."
                          ),
                          status = "info",
                          maximizable = FALSE,
                          collapsible = FALSE,
                          boxToolSize = "sm",
                          width = 12,
                          
                          
                          fluidRow(h6("Carica un file csv ...")),
                          fluidRow(
                            column(width = 2,
                                   downloadButton(outputId = "downloadTemplateRidEmissionButton",label = "Scarica CSV template", class = "btn-info2", icon = icon("download"))
                            ),
                            column(width = 6,
                                   fileInput("ridEmissionFile", "Carica un file CSV",
                                             multiple = FALSE,
                                             accept = c(".csv"),
                                             width = "100%")
                            ),
                            column(width = 4, align = 'right',
                                   actionButton(inputId = "updateRidEmissionButton",label = "Aggiorna", class = "btn-success",icon = icon("forward"))
                            )
                          ),
                          hr(),
                          fluidRow(h6("... oppure modifica i dati nella tabella")),
                          fluidRow(
                            
                            column(12, 
                                   fluidRow(
                                     column(width = 2, pickerInput(inputId = "zonaChoiceRidEmission",label = "Zona", width = "100%", choices = NULL)),
                                     column(width = 2, pickerInput(inputId = "comuniChoiceRidEmission",label = "Comune", width = "100%", choices = NULL)),
                                     column(width = 2, pickerInput(inputId = "settoreChoiceRidEmission",label = "Settore", width = "100%", choices = NULL)),
                                     column(width = 2, pickerInput(inputId = "combustibileChoiceRidEmission",label = "Combustibile", width = "100%", choices = NULL)),
                                     column(width = 2, pickerInput(inputId = "inquinanteChoiceRidEmission",label = "Inquinante", width = "100%", choices = NULL)),
                                     column(width = 1, textInput(inputId = "riduzioneRidEmission",label = "Var. [%]", placeholder = "0%", width = '100%')),
                                     column(width = 1, actionButton(inputId = "applicaRiduzioneRidEmission",label = "", class = "btn-success2",icon = icon("sync-alt"),width = "100%"))
                                   ),
                                   fluidRow(
                                     reactableOutput("ridEmissionTable", width = "100%")
                                   )
                            )
                            
                          )
                          
                          
                          
                          
                        )
                        
                        
                        
                        
                        
                    )  
                  )
                  
                ),
                  
                fluidRow(
                  column(width = 6, align = 'left',
                         actionButton(inputId = "resetScenarioButton",label = "Reset", class = "btn-success",icon = icon("sync-alt"))
                  ),
                  column(width = 6, align = 'right',
                    actionButton(inputId = "runScenarioButton",label = "Crea Scenario", class = "btn-success",icon = icon("sync-alt"))
                  )
                  

                )

        ),

        ####################
        # DASHBOARD
        ####################

        tabItem(tabName = "dashboard",

                fluidRow(
                  column(width = 6, pickerInput(inputId = "scenarioChoiceDashboard",label = "Seleziona uno o più scenari",width = "100%",choices = NULL, multiple = TRUE)),
                  column(width = 6, pickerInput(inputId = "comuneChoiceDashboard",label = "Seleziona un comune", width = "100%",choices = NULL))
                ),

                hr(),

                h4("Emissioni"),
                fluidRow(
                  column(width = 6, highchartOutput('emiGraph')),
                  column(width = 6, highchartOutput('deltaEmiGraph'))
                ),

                br(),
                hr(),

                h4("Concentrazioni"),
                fluidRow(
                  column(width = 6, highchartOutput('concGraph')),
                  column(width = 6, highchartOutput('deltaConcGraph'))
                )


        ),

        ####################
        # EMISSIONI
        ####################

        tabItem(tabName = "emissions",

                fluidRow(
                  column(width = 3, pickerInput(inputId = "scenarioChoiceEmissions",label = "Seleziona uno scenario",width = "100%",choices = NULL)),
                  column(width = 9, textInput(inputId = "scenarioDescriptionEmissions",label = "Descrizione scenario", width = '100%', value = NULL))
                ),

                hr(),
                fluidRow(
                  column(width = 2,
                         pickerInput(
                           inputId = "pollutantChoiceEmissions",
                           label = "Inquinante",
                           width = "100%",
                           choices = PRECURSORS_DESC)
                  ),
                  column(width = 4,
                         pickerInput(
                           inputId = "sectorChoiceEmissions",
                           label = "Settore",
                           choices = SECTORS,
                           width = "100%"
                         )
                  ),
                  column(width = 2,
                         pickerInput(
                           inputId = "deltaChoiceEmissions",
                           label = "Valori assoluti/Variazioni",
                           choices = DELTA_ABS_CHOICHES,
                           width = "100%"
                         )
                  ),
                  column(width = 2,
                         pickerInput(
                           inputId = "densityChoiceEmissions",
                           label = "Totali/Densità",
                           choices = EMI_DENS_CHOICHES,
                           width = "100%"
                         )
                  ),
                  column(width = 2,
                         pickerInput(
                           inputId = "aggregationChoiceEmissions",
                           label = "Spatial aggregation",
                           choices = SPATIAL_AGGREGATION_CHOICES,
                           width = "100%"
                         )
                  )
                ),
                fluidRow(
                  column(width = 12, id = "colBox",
                         box(title = "Mappa delle emissioni", width = 12, collapsible = FALSE, height = 700, maximizable = T, collapsed = FALSE,
                             div(style = "height:100%",

                                 absolutePanel(id = "mapEmiAbsPanel",
                                               class = "absoluteEmiMapPanel",
                                               fixed = TRUE,
                                               draggable = FALSE,
                                               shinyWidgets::dropdown(
                                                 label = "",
                                                 size = "lg", width = "185",
                                                 right = TRUE, up = FALSE,
                                                 circle = FALSE,
                                                 arrow = FALSE,
                                                 status = "bttMap",
                                                 icon = icon("gear"),
                                                 tooltip = tooltipOptions(title = "Opzioni!"),

                                                 tags$div(class="mapmenu",
                                                          tags$div("Modifica min-max", style = "padding-bottom:5px;text-align:center"),
                                                          fluidRow(
                                                            tags$div(class = "inline",
                                                                     uiOutput("minValEmiMapUI"),
                                                                     uiOutput("maxValEmiMapUI")
                                                            ),
                                                            # actionBttn(inputId = "updateEmiMap",label = "Update Map",icon = icon("sync-alt"), block = TRUE, size = "sm")
                                                            actionButton(inputId = "updateEmiMap",label = "Aggiorna mappa", class = "btn-success",icon = icon("sync-alt"), style = "width:100%")
                                                          ),
                                                          br(),
                                                          tags$div("Stampa mappa", style = "padding-bottom:5px;text-align:center"),
                                                          fluidRow(
                                                            # actionBttn(inputId = "printEmiMap",label = "Export Map",icon = icon("print"), block = TRUE, size = "sm")
                                                            actionButton(inputId = "printEmiMap",label = "Esporta", class = "btn-success",icon = icon("print"), style = "width:100%")
                                                          )
                                                 )
                                               )

                                 ),

                                 leafletOutput('emissionsMap', height = "100%")
                             )
                         ),
                  )

                )

        ),

        ####################
        # CONCENTRAZIONI
        ####################

        tabItem(tabName = "concentrations",

                fluidRow(
                  # column(width = 1),
                  column(width = 3, pickerInput(inputId = "scenarioChoiceConcentrations",label = "Seleziona uno scenario", width = "100%", choices = NULL)),
                  column(width = 9, textInput(inputId = "scenarioDescriptionConcentrations",label = "Descrizione scenario", width = '100%', value = NULL )),
                  # column(width = 1)
                ),
                hr(),
                fluidRow(
                  column(width = 2, pickerInput(inputId = "pollutantChoiceConcentrations",label = "Inquinante", width = "100%", choices = NULL)),
                  column(width = 3,
                         pickerInput(
                           inputId = "deltaChoiceConcentrations",
                           label = "Delta/Absolute values",
                           choices = DELTA_ABS_CHOICHES,
                           width = "100%"
                         )
                  ),
                  column(width = 7,
                         pickerInput(
                           inputId = "aggregationChoiceConcentrations",
                           label = "Spatial aggregation",
                           choices = SPATIAL_AGGREGATION_CHOICES,
                           width = "100%"
                         )
                  )
                ),
                fluidRow(
                  column(width = 12, id = "colBox",
                         box(title = "Mappa di qualità dell'aria", width = 12, height = 700, maximizable = TRUE, collapsible = FALSE, collapsed = FALSE,
                             div(style = "height:100%",

                                 absolutePanel(id = "mapConcAbsPanel",
                                               class = "absoluteConcMapPanel",
                                               fixed = TRUE,
                                               draggable = FALSE,
                                               shinyWidgets::dropdown(
                                                 label = "",
                                                 size = "lg", width = "185",
                                                 right = TRUE, up = FALSE,
                                                 circle = FALSE,
                                                 arrow = FALSE,
                                                 status = "bttMap",
                                                 icon = icon("gear"),
                                                 tooltip = tooltipOptions(title = "Opzioni!"),

                                                 tags$div(class="mapmenu",
                                                          tags$div("Modifica min-max", style = "padding-bottom:5px;text-align:center"),
                                                          fluidRow(
                                                            tags$div(class = "inline",
                                                                     uiOutput("minValConcMapUI"),
                                                                     uiOutput("maxValConcMapUI")
                                                            ),
                                                            # actionBttn(inputId = "updateConcMap",label = "Update Map",icon = icon("sync-alt"), block = TRUE, size = "sm")
                                                            actionButton(inputId = "updateConcMap",label = "Aggiorna mappa", class = "btn-success",icon = icon("sync-alt"), style = "width:100%")
                                                          ),
                                                          br(),
                                                          tags$div("Stampa mappa", style = "padding-bottom:5px;text-align:center"),
                                                          fluidRow(
                                                            # actionBttn(inputId = "printConcMap",label = "Export Map",icon = icon("print"), block = TRUE, size = "sm")
                                                            actionButton(inputId = "printConcMap",label = "Esporta", class = "btn-success",icon = icon("print"), style = "width:100%")
                                                          )
                                                 )
                                               )

                                 ),

                                 leafletOutput('concentrationsMap', height = "100%")

                             )
                         )
                  )
                )

        ),

        ####################
        # GESTIONE SCENARI
        ####################

        tabItem(tabName = "scenarioConfig",

                fluidRow(

                  column(width = 12,
                         div(class = "tab-header",
                              h4(icon("list-check", style = "color:#17a2b8; padding-right: 10px;"), "Lista scenari",style = "margin: 0px")
                             ),
                         br(),
                         div(class = "tab-table",

                              reactableOutput('scenarioConfigTable'),

                         ),
                         div(
                           style = "visibility: hidden;",
                           downloadButton("downloadData", label = "")
                         ) # bottone nascosto di download degli scenari
                  )

                )

        )    
      
      )
      
    )
  ),
  
  
  server = function(input, output, session) {
    
    #######################################################################
    # INITIALIZE APPLICATION
    #######################################################################
   
    if (!interactive()) sink(stderr(), type = "output")
    options(shiny.maxRequestSize=30*1024^2)
    sf_use_s2(FALSE)
    
    initializeDB()
    
    if (Sys.info()['sysname']=="Linux") {
      username <- Sys.getenv("SHINYPROXY_USERNAME")
    } else {
      username <- "XXXXXX70S04B300D"
    }
    
    usergroup <- "USER"


    print(paste0(Sys.time(), " - SHINYPROXY_USERNAME: ", Sys.getenv("SHINYPROXY_USERNAME")))
    print(paste0(Sys.time(), " - SHINYPROXY_USERGROUPS: ", Sys.getenv("SHINYPROXY_USERGROUPS")))
    
    print(paste0(Sys.time(), " - Login by user: ", username))
    print(paste0(Sys.time(), " - User group: ", usergroup))
    
    # output$sidebar_admin <- renderUI({
    #   
    #   if (usergroup == "ADMIN") {
    #     
    #     sidebarHeader("Menu amministratore")
    #     
    #   }
    #   
    # })
    
    urlApp <- reactive(session$clientData$url_hostname)
    
    # Check authorization on app start
    observe({

      req(urlApp)

      if (callAttributeAuthority(username) != "POC_USER") {

        print(paste0(Sys.time(), " - Utente NON autorizzato ad accedere al POC VERA"))
        shinyjs::runjs(paste0('window.location.href = "noAuth.html";'))

      }

    })
    

    output$menu_admin <- renderUI({
      
      if (usergroup == "ADMIN") {
        
        bs4Dash::menuItem(
          text = "Gestione utenti",
          tabName = "userConfig",
          icon = icon("users", lib = "font-awesome")
        )
        
      }
      
    })
    

    
    output$user <- renderUser({

      # firstName <- user_list$dataframe[user_list$dataframe$username == username,"firstName"]
      # lastName <- user_list$dataframe[user_list$dataframe$username == username,"lastName"]
      # name <- paste(firstName,lastName)

      dashboardUser(
        name = username,
        image = "user-solid.svg",
        title = "Codice Fiscale Utente",
        # subtitle = HTML(paste0(istitute,"<br>",username)),
        subtitle = username,
        column(
          width = 12,
          align = "center",
          a(actionButton("logout", "Logout"), href ="/logout"),
        )
      )

    })
    
    
    #######################################################################
    # SCENARI
    #######################################################################
    
    
    scenario_list <- reactiveValues(
      
      list = as.list(scenarioListfromDB(username,usergroup)),
      dataframe = readScenarioFromDB(username,usergroup),
      value = NULL,
      timer = reactiveTimer(1000), 
      started = TRUE
      
    )
    
    
    observeEvent(input$defineTipoScenarioOptions, {
      
      # print(paste0(Sys.time()," - Ritorno a carica scenario"))
      
      choice <- match(input$defineTipoScenarioOptions,SCENARIO_DEFINITION_CHOICES)
      
      if (choice == 1) {

        shinyjs::show(id = "modificoGrafo", anim = FALSE)
        shinyjs::hide(id = "politicaUrbana", anim = FALSE)
        shinyjs::hide(id = "riduzioniEmissive", anim = FALSE)
        
      } else if (choice == 2) {
        
        shinyjs::hide(id = "modificoGrafo", anim = FALSE)
        shinyjs::show(id = "politicaUrbana", anim = FALSE)
        shinyjs::hide(id = "riduzioniEmissive", anim = FALSE)
        
      } else {
        
        shinyjs::hide(id = "modificoGrafo", anim = FALSE)
        shinyjs::hide(id = "politicaUrbana", anim = FALSE)
        shinyjs::show(id = "riduzioniEmissive", anim = FALSE)
        
      }
      
    })
    
    
    
    #######################################################################
    # SCENARI - TRAFFICO LINEARE
    #######################################################################
    
    shinyjs::removeClass(id ="downloadTemplateTrafficButton", class = "disabled")
    # shinyjs::removeClass(id ="downloadTemplateTrafficButton", class = "btn-default")
    
    trafficData <- reactiveValues(
      
      map = traffic,
      table = trafficDT
      
    )
    
    observe({
      
      updatePickerInput(
        inputId = "comuniChoiceScenariLineari",
        choices = c("< Tutti i comuni >",
                    unique(trafficComuni$COMUNE)[order(unique(trafficComuni$COMUNE))]),
        selected = NULL,
      )
      
    })
    
    observe({
      
      updatePickerInput(
        inputId = "tipoStradaChoiceScenariLineari",
        choices = c("< Tutte le tipologie >",
          unique(traffic$NOME_TIPO_STRADA)[order(unique(traffic$NOME_TIPO_STRADA))]),
        selected = NULL,
      )
      
    })
    
    observe({
      
      updatePickerInput(
        inputId = "tipoVeicoloChoiceScenariLineari",
        choices = c("< Tutte le tipologie >", "Automobili", "Comm. Leggeri", "Comm. Pesanti"),
        selected = NULL,
      )
      
    })

    
    output$downloadTemplateTrafficButton = downloadHandler(

      filename = function() {

        return("grafoStradale.csv")

      },
      content = function(file) {

        fileTodownload = "grafoStradale.csv"
        reportTodownload <- read.csv2(paste0(TEMPLATEDIR,"/",fileTodownload), sep = ";", dec = ".")
        write.table(reportTodownload, file, row.names = FALSE, quote = FALSE, sep = ";", dec = ".", na = "")

      }
    )
    
    trafficDTFiltered <- reactive({
      
      req(input$comuniChoiceScenariLineari)
      req(input$tipoStradaChoiceScenariLineari)
      
      data <- trafficData$table
      
      if (input$tipoStradaChoiceScenariLineari != "< Tutte le tipologie >") {
        
        data <- data %>%
          filter(NOME_TIPO_STRADA == input$tipoStradaChoiceScenariLineari)
        
      }
      
      if (input$comuniChoiceScenariLineari != "< Tutti i comuni >") {
        
        listaArchiID <- trafficComuni$ID_GRAFO[trafficComuni$COMUNE == input$comuniChoiceScenariLineari]
          
        data <- data %>%
          filter(ID_GRAFO %in% listaArchiID)
        
      }
      
      return(data)
      
    })
    
    output$trafficScenarioTable <- renderReactable({

      reactable(trafficDTFiltered(),
                wrap = FALSE,
                highlight = TRUE,
                selection = "multiple",
                defaultColDef = colDef(align = "left", sortable = FALSE),
                columns = list(
                  ID_GRAFO = colDef(name = "ID Grafo",maxWidth = 80),
                  NOME_TIPO_STRADA = colDef(name = "Tipologia strada", maxWidth = 120),
                  Automobili = colDef(cell = text_extra("textAuto")),
                  Leggeri = colDef(name = "Comm. Leggeri", cell = text_extra("textComLegg")),
                  Pesanti = colDef(name = "Comm. Pesanti", cell = text_extra("textComPes"))
                ),
                onClick = "select",
                class = "reactablecss")

    })
    

    current_page_Traff <- reactive({
      getReactableState("trafficScenarioTable")$page
    })

    current_selection_Traff <- reactive({
      getReactableState("trafficScenarioTable")$selected
    })


    observeEvent(input$textAuto, {

      req(input$textAuto)

      file <- paste0(TEMPDIR,"/",userUUID, "_1.csv")
      values <- input$textAuto
      string <- paste0(trafficDTFiltered()$ID_GRAFO[values$row],",",toString(values), "\n")
      cat(string, file = file,append = TRUE)

    })

    observeEvent(input$textComLegg, {

      req(input$textComLegg)

      file <- paste0(TEMPDIR,"/",userUUID, "_2.csv")
      values <- input$textComLegg
      string <- paste0(trafficDTFiltered()$ID_GRAFO[values$row],",",toString(values), "\n")
      cat(string, file = file,append = TRUE)

    })

    observeEvent(input$textComPes, {

      req(input$textComPes)

      file <- paste0(TEMPDIR,"/",userUUID, "_3.csv")
      values <- input$textComPes
      string <- paste0(trafficDTFiltered()$ID_GRAFO[values$row],",",toString(values), "\n")
      cat(string, file = file,append = TRUE)

    })
    
    output$trafficScenarioMap <- renderLeaflet({

      leaflet() %>%
        addTiles() %>%
        setView(lng = 11.0, lat = 44.5, zoom = 8.0)
      
      # labels <- sprintf("<strong>ID Grafo: %s</strong><br/>Tipologia: %s<br/>Veicoli Eq.: %s<br/>", traffic$ID_GRAFO, traffic$NOME_TIPO_STRADA, traffic$VeicEquiv) %>% lapply(htmltools::HTML)
      # 
      # values <- traffic$VeicEquiv
      # minVal <- min(traffic$VeicEquiv)
      # maxVal <- max(traffic$VeicEquiv) * 0.6
      # 
      # values[values >= maxVal] <- maxVal
      # 
      # palTraffic <- colorNumeric(palette = "RdYlBu", c(minVal,maxVal), reverse = TRUE)
      # title <-  "Veicoli Eq."
      # 
      # leaflet(traffic) %>%
      #   addTiles() %>%
      #   setView(lng = 11.0, lat = 44.5, zoom = 8.0) %>%
      #   leaflet.extras2::addSpinner()  %>%
      #   leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 15)) %>%
      #   addPolylines(
      #     # color = "#03F",
      #     color = ~palTraffic(values),
      #     weight = 3,
      #     smoothFactor = 0.5,
      #     opacity = 0.5,
      #     fillOpacity = 0.5,
      #     layerId = traffic$ID,
      #     highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
      #     label = labels,
      #     labelOptions = labelOptions(
      #       style = list("font-weight" = "normal", padding = "3px 8px"),
      #       textsize = "15px",
      #       direction = "auto"),
      #     group = "trafficLines"
      #   ) %>%
      #   # Add the legend
      #   addLegend(
      #     "bottomright",
      #     pal = palTraffic,
      #     values = c(minVal,maxVal),
      #     title = title,
      #     opacity = 1
      #   ) %>%
      #   leaflet.extras2::stopSpinner()

      
    })
    
    observe({

      labels <- sprintf("<strong>ID Grafo: %s</strong><br/>Tipologia: %s<br/>Veicoli Eq.: %s<br/>", traffic$ID_GRAFO, traffic$NOME_TIPO_STRADA, traffic$VeicEquiv) %>% lapply(htmltools::HTML)

      values <- traffic$VeicEquiv
      minVal <- min(traffic$VeicEquiv)
      maxVal <- max(traffic$VeicEquiv) * 0.6

      values[values >= maxVal] <- maxVal

      palTraffic <- colorNumeric(palette = "RdYlBu", c(minVal,maxVal), reverse = TRUE)
      title <-  "Veicoli Eq."

      updateTrafficMap("trafficScenarioMap",palTraffic,labels,traffic,values,c(minVal,maxVal),title)

    })
    
    outputOptions(output, "trafficScenarioMap", suspendWhenHidden = FALSE)
    outputOptions(output, "trafficScenarioTable", suspendWhenHidden = FALSE)
    

    fileGrafo <- reactiveValues(data = NULL)
    
    observeEvent(input$updateTrafficButton, {
    
      file <- input$trafficFile
      
      if (is.null(file)) {
        
        print(paste0(Sys.time()," - file non caricato!"))
        sendAlert("Caricare un file con le stime di traffico!") 
        return(NULL) 
        
      }
      
      if (file_ext(file$datapath) %!in% c("csv")) {
        
        print(paste0(Sys.time()," - estensione file non corretta: ", file_ext(file$datapath)))
        sendAlert("Estensione file non corretta. Sono accettati solo file csv.") 
        
        # fileEmiRed$data <- NULL
        shinyjs::reset('trafficFile')
        
        return(NULL) 
        
      } else {
        
        print(paste0(Sys.time()," - Leggo il file caricato: ", file$datapath))
        
        tryCatch({
          
          temporaryFiles <- read.csv2(file$datapath, sep = ";", dec = ".")
          
          trafficOrigDT <- trafficData$table
          trafficOrigMap <- trafficData$map
          
          trafficScen <- temporaryFiles %>%
            dplyr::select(ID_GRAFO, Automobili, Leggeri, Pesanti) %>%
            pivot_longer(cols = c("Automobili", "Leggeri", "Pesanti"), names_to = "Sector", values_to = "newValue")
          
          newTrafficDT <- trafficOrigDT %>%
            pivot_longer(cols = c("Automobili", "Leggeri", "Pesanti"), names_to = "Sector", values_to = "Value") %>%
            left_join(trafficScen, join_by(ID_GRAFO, Sector))  %>%
            mutate(Value = if_else(is.na(newValue), Value, newValue)) %>%
            dplyr::select(-newValue) %>%
            pivot_wider(names_from = "Sector", values_from = "Value")
          
          newTrafficMap <- trafficOrigMap %>%
            left_join(subset(newTrafficDT, select = -c(NOME_TIPO_STRADA)), join_by(ID_GRAFO))  %>%
            mutate(VeicEquiv = Automobili + Leggeri * 2 + Pesanti * 3) %>%
            dplyr::select(-Automobili, -Leggeri, -Pesanti)
          
          print(paste0(Sys.time()," - File letto senza errori"))
          
        }, error=function(e) {
          
          sendAlert("Si è verificato un errore all'apertura del file csv!") 
          print(paste0(Sys.time()," - Errore nella lettura del file"))
          
          return(NULL)
          
        })
        
        shinyjs::reset('trafficFile')
        
        trafficData$table <- newTrafficDT
        trafficData$map <- newTrafficMap        
        
        # aggiorno la tabella
        # trafficData$table <- newTrafficDT
        updateReactable("trafficScenarioTable", newTrafficDT, page = current_page_Traff())
        
        # aggiorno la mappa
        # trafficData$map <- newTrafficMap
        labels <- sprintf("<strong>ID Grafo: %s</strong><br/>Tipologia: %s<br/>Veicoli Eq.: %s<br/>", newTrafficMap$ID_GRAFO, newTrafficMap$NOME_TIPO_STRADA, newTrafficMap$VeicEquiv) %>% lapply(htmltools::HTML)
        
        values <- newTrafficMap$VeicEquiv
        minVal <- min(newTrafficMap$VeicEquiv)
        maxVal <- max(newTrafficMap$VeicEquiv) * 0.6
        
        values[values >= maxVal] <- maxVal
        
        palTraffic <- colorNumeric(palette = "RdYlBu", c(minVal,maxVal), reverse = TRUE)
        title <-  "Veicoli Eq."
        
        updateTrafficMap("trafficScenarioMap",palTraffic,labels,newTrafficMap,values,c(minVal,maxVal),title)
        

      }
      
    })
    
    observeEvent(input$applicaRiduzioneScenariLineari, {
      
      value <- as.numeric(input$riduzioneScenariLineari)
      temporaryFiles <- list.files(path = TEMPDIR, pattern = userUUID, full.names = TRUE)
      
      if (input$riduzioneScenariLineari == "" &  length(temporaryFiles) == 0) {
        
        return(NULL)
        
      } 
      
      if (input$riduzioneScenariLineari != "" & (is.na(value) | value < 0 | value > 100)) {
        
        # print(paste0(Sys.time()," - file non caricato!"))
        sendAlert("Inserire un valore compreso tra 0 e 100") 
        return(NULL) 
        
      }

      if (length(temporaryFiles) != 0) {
        
        trafficOrigDT <- trafficData$table
        trafficOrigMap <- trafficData$map

        trafficScen <- temporaryFiles %>%
          lapply(read.table, sep = ",", strip.white = TRUE) %>%
          bind_rows %>%
          rename(ID_GRAFO = V1, Row = V2, newValue = V3, Sector = V4) %>%
          dplyr::select(-Row) %>%
          group_by(ID_GRAFO, Sector) %>%
          summarise(ID_GRAFO = last(ID_GRAFO), newValue = last(newValue), Sector = last(Sector))
        
        newTrafficDT <- trafficOrigDT %>%
          pivot_longer(cols = c("Automobili", "Leggeri", "Pesanti"), names_to = "Sector", values_to = "Value") %>%
          left_join(trafficScen, join_by(ID_GRAFO, Sector))  %>%
          mutate(Value = if_else(is.na(newValue), Value, newValue)) %>%
          dplyr::select(-newValue) %>%
          pivot_wider(names_from = "Sector", values_from = "Value")
        
        newTrafficMap <- trafficOrigMap %>%
          left_join(subset(newTrafficDT, select = -c(NOME_TIPO_STRADA)), join_by(ID_GRAFO))  %>%
          mutate(VeicEquiv = Automobili + Leggeri * 2 + Pesanti * 3) %>%
          dplyr::select(-Automobili, -Leggeri, -Pesanti)

        trafficData$table <- newTrafficDT
        trafficData$map <- newTrafficMap        
        
        # elimino i file temporanei
        unlink(temporaryFiles)
        
      } 
      
      if(!is.null(value)) {
        
        trafficOrigDT <- trafficData$table
        trafficOrigMap <- trafficData$map
        
        trafficDTFiltered <- trafficDTFiltered()
        
        if (input$tipoVeicoloChoiceScenariLineari == "< Tutte le tipologie >") {
          cols <- c("Automobili", "Leggeri", "Pesanti")
        } else {
          cols <- gsub("Comm. ", "", input$tipoVeicoloChoiceScenariLineari)
        }
        
        variation <- as.numeric(value)/100
        
        trafficScen <- trafficDTFiltered %>%
          mutate_at(vars(cols), ~. * variation) %>%
          pivot_longer(cols = c("Automobili", "Leggeri", "Pesanti"), names_to = "Sector", values_to = "newValue")
        
        newTrafficDT <- trafficOrigDT %>%
          pivot_longer(cols = c("Automobili", "Leggeri", "Pesanti"), names_to = "Sector", values_to = "Value") %>%
          left_join(subset(trafficScen, select = -c(NOME_TIPO_STRADA)), join_by(ID_GRAFO, Sector))  %>%
          mutate(Value = if_else(is.na(newValue), Value, round(newValue, digits = 0))) %>%
          dplyr::select(-newValue) %>%
          pivot_wider(names_from = "Sector", values_from = "Value")
        
        newTrafficMap <- trafficOrigMap %>%
          left_join(subset(newTrafficDT, select = -c(NOME_TIPO_STRADA)), join_by(ID_GRAFO))  %>%
          mutate(VeicEquiv = Automobili + Leggeri * 2 + Pesanti * 3) %>%
          dplyr::select(-Automobili, -Leggeri, -Pesanti)
        
        trafficData$table <- newTrafficDT
        trafficData$map <- newTrafficMap        
        
        
      }


      # aggiorno la tabella
      # trafficData$table <- newTrafficDT
        updateReactable("trafficScenarioTable", newTrafficDT, page = current_page_Traff())

      # aggiorno la mappa
      # trafficData$map <- newTrafficMap
      labels <- sprintf("<strong>ID Grafo: %s</strong><br/>Tipologia: %s<br/>Veicoli Eq.: %s<br/>", newTrafficMap$ID_GRAFO, newTrafficMap$NOME_TIPO_STRADA, newTrafficMap$VeicEquiv) %>% lapply(htmltools::HTML)
      
      values <- newTrafficMap$VeicEquiv
      minVal <- min(newTrafficMap$VeicEquiv)
      maxVal <- max(newTrafficMap$VeicEquiv) * 0.6
      
      values[values >= maxVal] <- maxVal
      
      palTraffic <- colorNumeric(palette = "RdYlBu", c(minVal,maxVal), reverse = TRUE)
      title <-  "Veicoli Eq."
      
      updateTrafficMap("trafficScenarioMap",palTraffic,labels,newTrafficMap,values,c(minVal,maxVal),title)
        

    })
    
    
    
    
    observe({

      rowSelected <- getReactableState("trafficScenarioTable", "selected")
      if(is.null(rowSelected)) {

        leaflet::leafletProxy( mapId = "trafficScenarioMap" ) %>%
          clearGroup("selectedLines") %>%
          setView(11,44.5,8)

      } else {
        
        grafoFiltrato <- trafficDTFiltered()
        idGrafo_selected <- grafoFiltrato$ID_GRAFO[rowSelected] 
        
        row_selected <- traffic[traffic$ID_GRAFO %in% idGrafo_selected,]
        # print(row_selected)

        bbox <- row_selected %>% st_bbox()

        maxLong = as.numeric(bbox$xmax)
        maxLat = as.numeric(bbox$ymax)
        minLong = as.numeric(bbox$xmin)
        minLat = as.numeric(bbox$ymin)

        bbox <- row_selected %>% st_bbox()
        labels <- sprintf("<strong>ID Grafo: %s</strong><br/>Tipologia: %s<br/>Veicoli Eq.: %s<br/>", row_selected$ID_GRAFO, row_selected$NOME_TIPO_STRADA, row_selected$VeicEquiv) %>% lapply(htmltools::HTML)
        
        leaflet::leafletProxy( mapId = "trafficScenarioMap" ) %>%
          clearGroup("selectedLines") %>%
          fitBounds(minLong,minLat,maxLong,maxLat) %>%
          addPolylines( data = row_selected,
                        color = "yellow",
                        weight = 5,
                        opacity = 1,
                        group = "selectedLines",
                        label = labels,
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "15px",
                          direction = "auto")
          )

      }


    })
    
    
 
    #######################################################################
    # SCENARI - TRAFFICO DIFFUSO
    #######################################################################
    
    shinyjs::removeClass(id ="downloadTemplateComuniButton", class = "disabled")
    shinyjs::removeClass(id ="downloadTemplateParcoButton", class = "disabled")
    
    diffuseData <- reactiveValues(
      
      map = comuni[[1]],
      table = comuniDT,
      parco = parco
      
    )
    
    observe({
      
      updatePickerInput(
        inputId = "comuniChoiceScenarioDiffuse",
        choices = c("< Tutti i comuni >",comuniDT$COMUNE[order(comuniDT$COMUNE)]),
        selected = NULL,
      )
      
    })
    
    observe({
      
      updatePickerInput(
        inputId = "residentiChoiceScenarioDiffuse",
        choices = CLASSE_RESIDENTI_CHOICES,
        selected = NULL,
      )
      
    })
    
    observe({

      updatePickerInput(
        inputId = "classeClimaChoiceScenarioDiffuse",
        choices = CLASSE_CLIMA_CHOICES,
        selected = NULL,
      )

    })
    
    observe({
      
      updatePickerInput(
        inputId = "zonaChoiceScenarioDiffuse",
        choices = ZONE_CHOICES,
        selected = NULL,
      )
      
    })
    
    
    
    output$downloadTemplateComuniButton = downloadHandler(
      
      filename = function() {
        
        return("comuni.csv")
        
      },
      content = function(file) {
        
        fileTodownload = "comuni.csv"
        reportTodownload <- read.csv2(paste0(TEMPLATEDIR,"/",fileTodownload), sep = ";", dec = ".")
        write.table(reportTodownload, file, row.names = FALSE, quote = FALSE, sep = ";", dec = ".", na = "")
        
      }
    )
    
    comuniDTFiltered <- reactiveValues(
      
      comuni = comuniDT,
      selezioneDaFiltro = TRUE,
      selezione = NULL
      
    )
      
    
    observe({

      req(input$comuniChoiceScenarioDiffuse)
      req(input$residentiChoiceScenarioDiffuse)
      # req(getReactableState("diffuseScenarioTable"))

      data <- comuniDT

      classeResidenti <- CLASSE_RESIDENTI_ID[match(input$residentiChoiceScenarioDiffuse, CLASSE_RESIDENTI_CHOICES)]
      claseClima <- CLASSE_CLIMA_ID[match(input$classeClimaChoiceScenarioDiffuse, CLASSE_CLIMA_CHOICES)]
      zona <- input$zonaChoiceScenarioDiffuse

      if (classeResidenti != "All") {

        data <- data %>%
          filter(CLASSE_RESIDENTI == paste0("CLASSE",classeResidenti))

      }

      if (claseClima != "All") {

        data <- data %>%
          filter(FK_ID_CLASSE_CLIMATICA == as.numeric(claseClima))

      }

      if (input$comuniChoiceScenarioDiffuse != "< Tutti i comuni >") {

        data <- data %>%
          filter(COMUNE %in% input$comuniChoiceScenarioDiffuse)

      }
      
      if (input$zonaChoiceScenarioDiffuse != "< Tutte le zone >") {
        
        data <- data %>%
          filter(ZONA %in% input$zonaChoiceScenarioDiffuse)
        
      }
      
      isolate(comuniDTFiltered$comuni <- data)
      # if (isolate(comuniDTFiltered$selezioneDaFiltro) == TRUE) updateReactable(outputId = "diffuseScenarioTable", data = data, selected = 1:nrow(data))
      
      # updateReactable(outputId = "diffuseScenarioTable", data = data)
      return(data)

    })
    

    output$diffuseScenarioTable <- renderReactable({
      
      if (isolate(comuniDTFiltered$selezioneDaFiltro) == TRUE) {
        
        if (nrow(comuniDTFiltered$comuni)>0) {
          selected <- 1:nrow(comuniDTFiltered$comuni)
        } else {
          selected <- NULL
        }
        
      } else {
        
        selected <- comuniDTFiltered$selezione
        
      }
      
      reactable(comuniDTFiltered$comuni,
                wrap = FALSE,
                highlight = TRUE,
                selection = "multiple",
                # defaultSelected = 1:nrow(comuniDT),
                defaultSelected = selected,
                defaultColDef = colDef(align = "left", sortable = FALSE),
                columns = list(
                  COMUNE = colDef(name = "Comune"),
                  PRO_COM = colDef(show = FALSE),
                  RESIDENTI = colDef(name = "Residenti"),
                  CLASSE_RESIDENTI = colDef(show = FALSE),
                  FK_ID_CLASSE_CLIMATICA = colDef(show = FALSE),
                  ZONA = colDef(name = "Zona")
                ),
                onClick = "select",
                class = "reactablecss")
      
      
    })
    
    
    observeEvent(input$updateComuniSelezionatiButton, {
      
      file <- input$comuniFile
      
      if (is.null(file)) {
        
        print(paste0(Sys.time()," - file non caricato!"))
        sendAlert("Caricare un file con i comuni in cui applicare la politica!") 
        return(NULL) 
        
      }
      
      if (file_ext(file$datapath) %!in% c("csv")) {
        
        print(paste0(Sys.time()," - estensione file non corretta: ", file_ext(file$datapath)))
        sendAlert("Estensione file non corretta. Sono accettati solo file csv.") 
        
        # fileEmiRed$data <- NULL
        shinyjs::reset('trafficFile')
        
        return(NULL) 
        
      } else {
        
        print(paste0(Sys.time()," - Leggo il file caricato: ", file$datapath))
        
        tryCatch({
          
          temporaryFile <- read.csv2(file$datapath, sep = ";", dec = ".")
          
          comuniScen <- comuniDT %>% 
            left_join(temporaryFile, join_by(PRO_COM == ISTAT_COMUNE))
          
          listaComuniSelezionati <- which(comuniScen$SELEZIONATO == "SI")
          
          if (length(listaComuniSelezionati) > 0) {
            
            comuniDTFiltered$selezioneDaFiltro <- FALSE
            comuniDTFiltered$selezione <- listaComuniSelezionati
            comuniDTFiltered$comuni <- comuniDT
            
            updatePickerInput(inputId = "comuniChoiceScenarioDiffuse", selected = "< Tutti i comuni >")
            updatePickerInput(inputId = "residentiChoiceScenarioDiffuse", selected = CLASSE_RESIDENTI_CHOICES[[1]])
            updatePickerInput(inputId = "classeClimaChoiceScenarioDiffuse", selected = CLASSE_CLIMA_CHOICES[[1]])
            # aggiorno la tabella
            updateReactable(outputId = "diffuseScenarioTable", data = comuniDT)
            
          } else {
            
            sendAlert("Nessun comune selezionato nel file csv.") 
            
          }
          
          print(paste0(Sys.time()," - File letto senza errori"))
          
        }, error=function(e) {
          
          sendAlert("Si è verificato un errore all'apertura del file csv!") 
          print(paste0(Sys.time()," - Errore nella lettura del file: "))
          print(paste0(Sys.time()," - Errore: ", e))
          
          
          return(NULL)
          
        })
        
        shinyjs::reset('comuniFile')
        
      }
      
    })
    
    output$diffuseScenarioMap <- renderLeaflet({
      
      shp <- comuni[[1]]
      
      palDiffuse <- colorNumeric(palette = "RdYlBu", domain = shp$RESIDENTI, reverse = TRUE)
      labels <- sprintf("<strong>%s</strong><br/>Istat: %s", shp$COMUNE, shp$PRO_COM) %>% lapply(htmltools::HTML)
      
      leaflet(shp) %>%
        addTiles()  %>%
        setView(11,44.5,8) %>%
        addPolygons(
          color = "blue",
          # color = ~palDiffuse(RESIDENTI),           
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0.5,
          fillOpacity = 0,
          layerId = traffic$ID,
          group = "click.list",
          highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        )
      # Add the legend
      # addLegend(
      #   "bottomright",
      #   pal = palDiffuse, 
      #   values = ~RESIDENTI,
      #   title = "Emissioni??",
      #   opacity = 1
      # )
      
      
    })
    
    

    
    
    observe({
      
      req(getReactableState("diffuseScenarioTable"))
      
      shp <- comuni[[1]] 
      
      comuniFiltrati <- isolate(comuniDTFiltered$comuni)
      # rowSelected <- getReactableState("diffuseScenarioTable", name = "selected")
      
      if (isolate(comuniDTFiltered$selezioneDaFiltro) == TRUE) {
        
        rowSelected <- getReactableState("diffuseScenarioTable", name = "selected")
        
      } else {
        
        rowSelected <- comuniDTFiltered$selezione
        
      }
      
      if(nrow(comuniFiltrati)==0 | is.null(rowSelected)) {
      
        leaflet::leafletProxy( mapId = "diffuseScenarioMap" ) %>%
          clearGroup("selectedPoly") 
          # %>% setView(11,44.5,8) 
        
      } else {
        
        # if (length(rowSelected) > nrow(comuniFiltrati)) return(NULL) 
        
        idComuni_selected <- comuniFiltrati$PRO_COM[rowSelected] 
        
        row_selected <- shp[shp$PRO_COM %in% idComuni_selected,]
        
        # row_selected <- shp[rowSelected,]
        print(row_selected)
        
        # bbox <- row_selected %>% st_bbox()
        # 
        # maxLong = as.numeric(bbox$xmax)
        # maxLat = as.numeric(bbox$ymax)
        # minLong = as.numeric(bbox$xmin)
        # minLat = as.numeric(bbox$ymin)

        labels <- sprintf("<strong>%s</strong><br/>Istat: %s", row_selected$COMUNE, row_selected$PRO_COM) %>% lapply(htmltools::HTML)
        
        leaflet::leafletProxy( mapId = "diffuseScenarioMap" ) %>%
          clearGroup("selectedPoly") %>%
          # fitBounds(minLong,minLat,maxLong,maxLat) %>% 
          addPolygons( data = row_selected, 
                       # color = "yellow",
                       weight = 1,
                       fillOpacity = 1,
                       # fillColor = "yellow",
                       fillColor = "#17a2b8",
                       opacity = 1,
                       group = "selectedPoly",
                       label = labels,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto")
          ) 
        
        diffuseData$map <- row_selected
        diffuseData$table <- row_selected %>% st_drop_geometry() 
        isolate(comuniDTFiltered$selezioneDaFiltro <- TRUE)
        # comuniDTFiltered$selezione <- NULL
        
      }
      
      
      
    })
    
    
    
    observeEvent(input$defineTipoPoliticaOptions, {
      
      # print(paste0(Sys.time()," - Ritorno a carica scenario"))
      
      choice <- match(input$defineTipoPoliticaOptions,SCENARIO_TIPO_POLITICA_CHOICES)
      
      if (choice == 1) {
        
        shinyjs::hide(id = "modificaParcoBox", anim = FALSE)
        shinyjs::show(id = "convertiParcoBox", anim = FALSE)
        
      } else {
        
        shinyjs::hide(id = "convertiParcoBox", anim = FALSE)
        shinyjs::show(id = "modificaParcoBox", anim = FALSE)
        
      }
      
    })
    
    output$downloadTemplateParcoButton = downloadHandler(
      
      filename = function() {
        
        return("parco.csv")
        
      },
      content = function(file) {
        
        fileTodownload = "parco.csv"
        reportTodownload <- read.csv2(paste0(TEMPLATEDIR,"/",fileTodownload), sep = ";", dec = ".")
        write.table(reportTodownload, file, row.names = FALSE, quote = FALSE, sep = ";", dec = ".", na = "")
        
      }
    )
    
    observeEvent(input$updateParcoButton, {
      
      file <- input$parcoFile
      
      if (is.null(file)) {
        
        print(paste0(Sys.time()," - file non caricato!"))
        sendAlert("Caricare un file con il parco su cui applicare la politica!") 
        return(NULL) 
        
      }
      
      if (file_ext(file$datapath) %!in% c("csv")) {
        
        print(paste0(Sys.time()," - estensione file non corretta: ", file_ext(file$datapath)))
        sendAlert("Estensione file non corretta. Sono accettati solo file csv.") 
        
        # fileEmiRed$data <- NULL
        shinyjs::reset('parcoFile')
        
        return(NULL) 
        
      } else {
        
        print(paste0(Sys.time()," - Leggo il file caricato: ", file$datapath))
        
        tryCatch({
          
          temporaryFile <- read.csv2(file$datapath, sep = ";", dec = ".")
          
          temp <- temporaryFile
          newParco <- modificaParcoTable$parco

          newParco <- newParco %>%
            left_join(temp[,c("CODICE_COPART", "VARIAZIONE")], join_by(CODICE_COPART)) %>%
            mutate(VAR = if_else(is.na(VARIAZIONE),VAR,VAR * VARIAZIONE/100)) %>%
            group_by(SET_NOME) %>%
            mutate(PERC_SETTORE = NUMERO_VEICOLI * VAR / sum(NUMERO_VEICOLI) ) %>%
            ungroup() %>%
            arrange(SET_NOME,CODICE_COPART) %>%
            dplyr::select(CODICE_COPART,SET_NOME,ABBREVIAZIONE_COMBUST,CLASSIFICAZIONE,TIPO_LEGISLATIVO_VEICOLO,EURO,NUMERO_VEICOLI,PERC_SETTORE,VAR)
          
          modificaParcoTable$parco <- newParco
          
          updateReactable("modificaParcoScenarioTable", newParco)
          
          print(paste0(Sys.time()," - File letto senza errori"))
          
        }, error=function(e) {
          
          sendAlert("Si è verificato un errore all'apertura del file csv!") 
          print(paste0(Sys.time()," - Errore nella lettura del file: "))
          print(paste0(Sys.time()," - Errore: ", e))
          
          
          return(NULL)
          
        })
        
        shinyjs::reset('parcoFile')
        
      }
      
    })
    
    rowSelectedParco <- reactive(getReactableState("convertiParcoScenarioTable", "selected"))

    convertiParcoTable <- reactive({

      rowSelected <- rowSelectedParco()
      if (is.null(rowSelected)) {
        rowSelected <- 1:nrow(parco)
      }

      temp <- parco
      temp$VAR <- 0
      temp$VAR[rowSelected] <- 1
      temp$NEW_VEIC <- temp$VAR * temp$NUMERO_VEICOLI

      sumTot <- temp %>%
        mutate(NEW_VEIC = VAR * NUMERO_VEICOLI) %>%
        group_by(SET_NOME) %>%
        summarise(NEW_TOT = sum(NEW_VEIC))

      parco %>%
        bind_cols(NEW_VEIC = temp$NEW_VEIC) %>%
        left_join(sumTot, join_by(SET_NOME)) %>%
        group_by(SET_NOME) %>%
        mutate(PERC_SETTORE = if_else(NEW_TOT != 0, NEW_VEIC / NEW_TOT, NUMERO_VEICOLI / sum(NUMERO_VEICOLI)) ) %>%
        mutate(NUMERO_VEICOLI = sum(NUMERO_VEICOLI) * PERC_SETTORE) %>%
        dplyr::select(CODICE_COPART,SET_NOME,ABBREVIAZIONE_COMBUST,CLASSIFICAZIONE,TIPO_LEGISLATIVO_VEICOLO,EURO,NUMERO_VEICOLI,PERC_SETTORE,NEW_TOT,NEW_VEIC,VAR)

       # updateReactable("parcoScenarioTable", parcoTable, selected = rowSelectedParco,)

    })

    # observeEvent(input$updateParcoButton, {
    # 
    #   updateReactable("convertiParcoScenarioTable", convertiParcoTable(), selected = rowSelectedParco())
    # 
    # })

    output$convertiParcoScenarioTable <- renderReactable({

      reactable(parco,
                wrap = FALSE,
                highlight = TRUE,
                selection = "multiple",
                defaultSelected = 1:nrow(parco),
                defaultColDef = colDef(align = "left", aggregate = "sum", sortable = FALSE),
                groupBy = c("SET_NOME","ABBREVIAZIONE_COMBUST","CLASSIFICAZIONE"),
                columns = list(
                  CODICE_COPART = colDef(show = FALSE),
                  SET_NOME = colDef(name = "Settore"),
                  ABBREVIAZIONE_COMBUST = colDef(name = "Combustibile"),
                  CLASSIFICAZIONE = colDef(name = "Classificazione"),
                  TIPO_LEGISLATIVO_VEICOLO = colDef(name = "Tipo Legislativo"),
                  EURO = colDef(show = FALSE),
                  NUMERO_VEICOLI = colDef(show = FALSE),
                  PERC_SETTORE= colDef(show = FALSE, name = "Perc. Veicoli",format = colFormat(percent = TRUE, digits = 2)),
                  VAR = colDef(show = FALSE)
                ),
                class = "reactablecss",
                onClick = "select",
                theme = reactableTheme(
                  # rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #117A8B")
                )
              )
    })

    output$convertiParcoScenarioGraphEuro <- renderHighchart({

      finalData <- convertiParcoTable() %>%
        group_by(SET_NOME, EURO) %>%
        summarise(PERC_SETTORE = sum(PERC_SETTORE, na.rm = T))

      hchart(finalData,"bar", hcaes(x = SET_NOME, y = PERC_SETTORE, group = EURO)) %>%
        hc_yAxis(title = list(text = NA)) %>%
        hc_xAxis(title = list(text = NA))  %>%
        hc_legend(align = "right", verticalAlign = "middle", layout = "vertical") %>%
        hc_plotOptions(series = list(stacking = "percent"))

    })

    output$convertiParcoScenarioGraphFuel <- renderHighchart({

      finalData <- convertiParcoTable() %>%
        group_by(SET_NOME, ABBREVIAZIONE_COMBUST) %>%
        summarise(PERC_SETTORE = sum(PERC_SETTORE, na.rm = T))

      hchart(finalData,"bar", hcaes(x = SET_NOME, y = PERC_SETTORE, group = ABBREVIAZIONE_COMBUST)) %>%
        hc_yAxis(title = list(text = NA)) %>%
        hc_xAxis(title = list(text = NA))  %>%
        hc_legend(align = "right", verticalAlign = "middle", layout = "vertical") %>%
        hc_plotOptions(series = list(stacking = "percent"))


    })
    
    
    observe({
      
      updatePickerInput(
        inputId = "settoreModificaParco",
        choices = unique(parco$SET_NOME),
        selected = unique(parco$SET_NOME),
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count"
        )
      )
      
    })
    
    observe({
      
      req(input$settoreModificaParco)
      
      choices <- parco %>%
        filter(SET_NOME %in% input$settoreModificaParco) %>%
        distinct(ABBREVIAZIONE_COMBUST) %>%
        pull()
      
      
      updatePickerInput(
        inputId = "combustibileModificaParco",
        choices = choices,
        selected = choices,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count"
        )
      )
      
    })
    
    observe({
      
      req(input$settoreModificaParco)
      req(input$combustibileModificaParco)
      
      choices <- parco %>%
        filter(SET_NOME %in% input$settoreModificaParco) %>%
        filter(ABBREVIAZIONE_COMBUST %in% input$combustibileModificaParco) %>%
        distinct(CLASSIFICAZIONE) %>%
        pull()
      
      updatePickerInput(
        inputId = "classificazioneModificaParco",
        choices = choices,
        selected = choices,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count"
        )
      )
      
    })
    
    observe({
      
      req(input$settoreModificaParco)
      req(input$combustibileModificaParco)
      req(input$classificazioneModificaParco)
      
      choices <- parco %>%
        filter(SET_NOME %in% input$settoreModificaParco) %>%
        filter(ABBREVIAZIONE_COMBUST %in% input$combustibileModificaParco) %>%
        filter(CLASSIFICAZIONE %in% input$classificazioneModificaParco) %>%
        distinct(EURO) %>%
        pull()
      
      updatePickerInput(
        inputId = "tipoEuroModificaParco",
        choices = choices,
        selected = choices,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count"
        )
      )
      
    })
    
    modificaParcoTable <- reactiveValues(
      
      parco = parco
      
    )
      
    
    observeEvent(input$applicaRiduzioneModificaParco, {
      
      value <- as.numeric(input$riduzioneModificaParco)
      
      if (is.na(value) | value < 0 | value > 100) {
        
        # print(paste0(Sys.time()," - file non caricato!"))
        sendAlert("Inserire un valore compreso tra 0 e 100") 
        return(NULL) 
        
      }
      
      temp <- modificaParcoTable$parco
      newParco <- modificaParcoTable$parco
      # temp$VAR <- 1
      
      # if (input$settoreModificaParco != "< Tutti i settori >") {
      #   
      #   temp <- temp %>%
      #     filter(SET_NOME == input$settoreModificaParco)
      #   
      # }
      # 
      # if (input$combustibileModificaParco != "< Tutti i combustibili >") {
      #   
      #   temp <- temp %>%
      #     filter(ABBREVIAZIONE_COMBUST== input$combustibileModificaParco)
      #   
      # }
      # 
      # if (input$tipoEuroModificaParco != "< Tutte le categorie >") {
      #   
      #   temp <- temp %>%
      #     filter(EURO == input$tipoEuroModificaParco)
      #   
      # }
      
      temp <- temp %>%
        filter(SET_NOME %in% input$settoreModificaParco) %>%
        filter(ABBREVIAZIONE_COMBUST %in% input$combustibileModificaParco) %>%
        filter(CLASSIFICAZIONE %in% input$classificazioneModificaParco) %>%
        filter(EURO %in% input$tipoEuroModificaParco) 
        
      temp$NEW_VAR <- as.numeric(input$riduzioneModificaParco)/100
      # temp$NEW_VEIC <- temp$NEW_VAR * temp$NUMERO_VEICOLI
      
      newParco <- newParco %>%
        left_join(temp[,c("CODICE_COPART", "NEW_VAR")], join_by(CODICE_COPART)) %>%
        # mutate(NUMERO_VEICOLI = if_else(is.na(NEW_VEIC),NUMERO_VEICOLI,NEW_VEIC)) %>%
        mutate(VAR = if_else(is.na(NEW_VAR),VAR,VAR * NEW_VAR)) %>%
        group_by(SET_NOME) %>%
        mutate(PERC_SETTORE = NUMERO_VEICOLI * VAR / sum(NUMERO_VEICOLI) ) %>%
        ungroup() %>%
        arrange(SET_NOME,CODICE_COPART) %>%
        dplyr::select(CODICE_COPART,SET_NOME,ABBREVIAZIONE_COMBUST,CLASSIFICAZIONE,TIPO_LEGISLATIVO_VEICOLO,EURO,NUMERO_VEICOLI,PERC_SETTORE,VAR)
      
      modificaParcoTable$parco <- newParco
      
      updateReactable("modificaParcoScenarioTable", newParco)
      
    })
  
    output$modificaParcoScenarioTable <- renderReactable({
      
      reactable(parco,
                wrap = FALSE,
                highlight = TRUE,
                # selection = "multiple",
                # defaultSelected = 1:nrow(parco),
                defaultColDef = colDef(align = "left", aggregate = "sum", sortable = FALSE),
                groupBy = c("SET_NOME","ABBREVIAZIONE_COMBUST","CLASSIFICAZIONE"),
                columns = list(
                  CODICE_COPART = colDef(show = FALSE),
                  SET_NOME = colDef(name = "Settore"),
                  ABBREVIAZIONE_COMBUST = colDef(name = "Combustibile"),
                  CLASSIFICAZIONE = colDef(name = "Classificazione"),
                  TIPO_LEGISLATIVO_VEICOLO = colDef(name = "Tipo Legislativo"),
                  EURO = colDef(show = FALSE),
                  NUMERO_VEICOLI = colDef(show = FALSE),
                  PERC_SETTORE= colDef(show = FALSE, name = "Perc. Veicoli",format = colFormat(percent = TRUE, digits = 2)),
                  VAR = colDef(show = TRUE, name = "Var. [%]",format = colFormat(percent = TRUE, digits = 2), 
                               aggregate = 
                                 htmlwidgets::JS(
                                  "function(values, rows) {
                                    var numerator = 0
                                    var denominator = 0
                                    
                                    rows.forEach(function(row, index) {
                                      numerator += row['NUMERO_VEICOLI'] * values[index]
                                      denominator += row['NUMERO_VEICOLI']
                                    })
                                    
                                    return numerator / denominator
                                    
                                  }")
                              )
                ),
                class = "reactablecss",
                onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #117A8B")
                )
      )
    })
    
    output$modificaParcoScenarioGraphEuro <- renderHighchart({
      
      finalData <- modificaParcoTable$parco %>%
        group_by(SET_NOME, EURO) %>%
        summarise(PERC_SETTORE = round(sum(PERC_SETTORE, na.rm = T) * 100, digits = 2)) %>%
        arrange(SET_NOME)
      
      hchart(finalData,"bar", hcaes(x = SET_NOME, y = PERC_SETTORE, group = EURO)) %>%
        hc_yAxis(title = list(text = NA), min = 0, max = 100) %>%
        hc_xAxis(title = list(text = NA))  %>%
        hc_legend(align = "right", verticalAlign = "middle", layout = "vertical") %>%
        # hc_plotOptions(series = list(stacking = "percent")) %>%
        hc_plotOptions(series = list(stacking = "normal"))
      
    })
    
    output$modificaParcoScenarioGraphFuel <- renderHighchart({
      
      finalData <- modificaParcoTable$parco %>%
        group_by(SET_NOME, ABBREVIAZIONE_COMBUST) %>%
        summarise(PERC_SETTORE = round(sum(PERC_SETTORE, na.rm = T) * 100, digits = 2)) 
      
      hchart(finalData,"bar", hcaes(x = SET_NOME, y = PERC_SETTORE, group = ABBREVIAZIONE_COMBUST)) %>%
        hc_yAxis(title = list(text = NA), min = 0, max = 100) %>%
        hc_xAxis(title = list(text = NA), categories = list("Autobus", "Automobili", "Ciclomotori", "Comm. Leggeri", "Comm. Pesanti", "Motocicli")) %>%
        hc_legend(align = "right", verticalAlign = "middle", layout = "vertical") %>%
        # hc_plotOptions(series = list(stacking = "percent")) %>%
        hc_plotOptions(series = list(stacking = "normal"))
      
      
    })
    
    
    
    observe({
      
      updatePickerInput(
        inputId = "fasciaOraria",
        choices = c("8:00 ÷ 13:00",
                    "13:00 ÷ 18:00",
                    "18:00 ÷ 8:00"),
        selected = c("8:00 ÷ 13:00",
                     "13:00 ÷ 18:00",
                     "18:00 ÷ 8:00")
      )
      
    })
    
    observe({
      
      updatePickerInput(
        inputId = "giornoTipo",
        choices = c("Feriale",
                    "Prefestivo",
                    "Festivo"),
        selected = c("Feriale",
                     "Prefestivo",
                     "Festivo")
      )
      
    })
    
    observe({
      
      updatePickerInput(
        inputId = "stagione",
        choices = c("Primavera",
                    "Estate",
                    "Autunno",
                    "Inverno"),
        selected = c("Primavera",
                    "Estate",
                    "Autunno",
                    "Inverno"),
      )
      
    })
    
    
    observe({
      
      updatePickerInput(
        inputId = "tipoStradaDiffuse",
        choices = unique(trafficDT$NOME_TIPO_STRADA),
        selected = c("Comunali","Urbane"),
      )
      
    })
    
    
    observeEvent(input$applicaAlleLineari, {
      
      if (input$applicaAlleLineari) {
        
        shinyjs::show(id = "tipoStradaDiffuse", anim = FALSE)
        
      } else {
        
        shinyjs::hide(id = "tipoStradaDiffuse", anim = FALSE)
        
      }
      
    })
    
    
    #######################################################################
    #  SCENARI - RIDUZIONI EMISSIVE
    #######################################################################
    
    shinyjs::removeClass(id ="downloadTemplateRidEmissionButton", class = "disabled")
    
    ridEmissionData <- reactiveValues(
      
      table = emissioniDT,
      
    )
    
    observe({
      
      updatePickerInput(
        inputId = "zonaChoiceRidEmission",
        choices = ZONE_CHOICES,
        selected = NULL,
      )
      
    })
    
    observe({
      
      req(input$zonaChoiceRidEmission)
      
      if (input$zonaChoiceRidEmission != "< Tutte le zone >") {
        
        comuniList <- emissioniDT %>%
          filter(ZONA %in% input$zonaChoiceRidEmission) %>%
          arrange(COMUNE) %>%
          distinct(COMUNE) %>%
          pull()
          
      } else {
        
        comuniList <- emissioniDT %>%
          arrange(COMUNE) %>%
          distinct(COMUNE) %>%
          pull()
        
      }
      
      updatePickerInput(
        inputId = "comuniChoiceRidEmission",
        choices = c("< Tutti i comuni >", comuniList),
        selected = NULL,
      )
      
    })
    
    observe({
      
      updatePickerInput(
        inputId = "settoreChoiceRidEmission",
        choices = c("< Tutti i settori >", SECTORS),
        selected = NULL,
      )
      
    })
    
   
    observe({
      
      updatePickerInput(
        inputId = "combustibileChoiceRidEmission",
          choices = c("< Tutti i combustibili >", COMB),
        selected = NULL,
      )
      
    })
    
    observe({
      
      updatePickerInput(
        inputId = "inquinanteChoiceRidEmission",
        choices = c("< Tutti gli inquinanti >", PRECURSORS_DESC),
        selected = NULL,
      )
      
    })
    
    
    
    output$downloadTemplateRidEmissionButton = downloadHandler(
      
      filename = function() {
        
        return("emissioni.csv")
        
      },
      content = function(file) {
        
        fileTodownload = "emissioni.csv"
        reportTodownload <- read.csv2(paste0(TEMPLATEDIR,"/",fileTodownload), sep = ";", dec = ".")
        write.table(reportTodownload, file, row.names = FALSE, quote = FALSE, sep = ";", dec = ".", na = "")
        
      }
    )
    
    observeEvent(input$updateRidEmissionButton, {
      
      file <- input$ridEmissionFile
      
      if (is.null(file)) {
        
        print(paste0(Sys.time()," - file non caricato!"))
        sendAlert("Caricare un file con le riduzioni emissive comunali!") 
        return(NULL) 
        
      }
      
      if (file_ext(file$datapath) %!in% c("csv")) {
        
        print(paste0(Sys.time()," - estensione file non corretta: ", file_ext(file$datapath)))
        sendAlert("Estensione file non corretta. Sono accettati solo file csv.") 
        
        # fileEmiRed$data <- NULL
        shinyjs::reset('ridEmissionFile')
        
        return(NULL) 
        
      } else {
        
        print(paste0(Sys.time()," - Leggo il file caricato: ", file$datapath))
        
        tryCatch({
          
          temporaryFile <- read.csv2(file$datapath, sep = ";", dec = ".")
          
          emissScen <- temporaryFile %>%
            pivot_longer(cols = starts_with(PRECURSORS), names_sep = "_", names_to = c("POLL",".value")) %>%
            rename(NEW_VAR = VAR)
          
          emiss <- emissioniDT %>%
            rename_at(vars(PRECURSORS), ~ paste0(.x, "_EMI")) %>%
            pivot_longer(cols = starts_with(PRECURSORS), names_sep = "_", names_to = c("POLL",".value"))
          
          newRidEmiss <- emiss %>%
            left_join(emissScen[,c("FK_ISTAT_COMUNE","FK_ID_SETTORE","FK_ID_COMBUSTIBILE","POLL","NEW_VAR")], join_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,POLL)) %>%
            # mutate(NUMERO_VEICOLI = if_else(is.na(NEW_VEIC),NUMERO_VEICOLI,NEW_VEIC)) %>%
            mutate(VAR = if_else(is.na(NEW_VAR),VAR,VAR * NEW_VAR/100)) %>%
            dplyr::select(-NEW_VAR) %>%
            pivot_wider(names_from = POLL, values_from = c(EMI,VAR), names_glue = "{POLL}_{.value}") %>%
            rename_at(vars(ends_with("_EMI")), ~ gsub("_EMI", "", .x))
          
          ridEmissionData$table <- newRidEmiss
          
          updateReactable("ridEmissionTable", newRidEmiss)
          
          print(paste0(Sys.time()," - File letto senza errori"))
          
        }, error=function(e) {
          
          sendAlert("Si è verificato un errore all'apertura del file csv!") 
          print(paste0(Sys.time()," - Errore nella lettura del file: "))
          print(paste0(Sys.time()," - Errore: ", e))
          
          
          return(NULL)
          
        })
        
        shinyjs::reset('ridEmissionFile')
        
      }
      
    })
    
    
    
    observeEvent(input$applicaRiduzioneRidEmission, {
      
      value <- as.numeric(input$riduzioneRidEmission)
      
      if (is.na(value) | value < 0 | value > 100) {
        
        # print(paste0(Sys.time()," - file non caricato!"))
        sendAlert("Inserire un valore compreso tra 0 e 100") 
        return(NULL) 
        
      }
      
      newRidEmiss <- temp <- ridEmissionData$table %>%
        rename_at(vars(PRECURSORS), ~ paste0(.x, "_EMI")) %>%
        pivot_longer(cols = starts_with(PRECURSORS), names_sep = "_", names_to = c("POLL",".value"))
      
      temp <- temp %>% dplyr::select(-c(EMI,VAR))
      
      if (input$zonaChoiceRidEmission != "< Tutte le zone >") {
        
        temp <- temp %>%
          filter(ZONA %in% input$zonaChoiceRidEmission)
        
      }
      
      if (input$comuniChoiceRidEmission != "< Tutti i comuni >") {
        
        temp <- temp %>%
          filter(COMUNE %in% input$comuniChoiceRidEmission)
        
      }
      
      if (input$settoreChoiceRidEmission != "< Tutti i settori >") {
        
        temp <- temp %>%
          filter(SET_NOME %in% input$settoreChoiceRidEmission)
        
      }
      
      if (input$combustibileChoiceRidEmission != "< Tutti i combustibili >") {
        
        temp <- temp %>%
          filter(ABBREVIAZIONE_COMBUST %in% input$combustibileModificaParco)
        
      }
      
      if (input$inquinanteChoiceRidEmission != "< Tutti gli inquinanti >") {
        
        temp <- temp %>%
          filter(POLL %in% PRECURSORS[match(input$inquinanteChoiceRidEmission,PRECURSORS_DESC)])
        
      }
      
      temp$NEW_VAR <- as.numeric(input$riduzioneRidEmission)/100
      temp <- temp %>% dplyr::select(-c(ZONA,COMUNE,SET_NOME,ABBREVIAZIONE_COMBUST))
      
      newRidEmiss <- newRidEmiss %>%
        left_join(temp, join_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,POLL)) %>%
        # mutate(NUMERO_VEICOLI = if_else(is.na(NEW_VEIC),NUMERO_VEICOLI,NEW_VEIC)) %>%
        mutate(VAR = if_else(is.na(NEW_VAR),VAR,VAR * NEW_VAR)) %>%
        dplyr::select(-NEW_VAR) %>%
        # rename(VAR = NEW_VAR) %>%
        pivot_wider(names_from = POLL, values_from = c(EMI,VAR), names_glue = "{POLL}_{.value}") %>%
        rename_at(vars(ends_with("_EMI")), ~ gsub("_EMI", "", .x))
      
      ridEmissionData$table <- newRidEmiss
      
      updateReactable("ridEmissionTable", newRidEmiss)
      
    })
    
    
    output$ridEmissionTable <- renderReactable({
      
      prec_list <- list()
      var_list <- list()
      
      for (i in 1:(length(PRECURSORS))){
        prec_list[[PRECURSORS[i]]] <- reactable::colDef(show = FALSE)
      }
      
      for (i in 1:(length(PRECURSORS))){
        
        var <- paste0(PRECURSORS[i], "_VAR")
        varName <- PRECURSORS_DESC[match(PRECURSORS[i], PRECURSORS)]
          
        var_list[[var]] <- colDef(show = TRUE, name = varName , maxWidth = 80, format = colFormat(percent = TRUE, digits = 2), 
                                            aggregate = 
                                              htmlwidgets::JS(paste0(
                                                "function(values, rows) {
                                                var numerator = 0
                                                var denominator = 0
                                                rows.forEach(function(row, index) {
                                                  numerator += row['",PRECURSORS[i],"'] * values[index]
                                                  denominator += row['",PRECURSORS[i],"']
                                                })
                                                return numerator / denominator
                                              }"))
                                            )
      }
      
      reactable(emissioniDT,
                wrap = FALSE,
                highlight = TRUE,
                # selection = "multiple",
                # defaultSelected = 1:nrow(parco),
                defaultColDef = colDef(align = "left", aggregate = "sum", sortable = FALSE),
                groupBy = c("ZONA","COMUNE","SET_NOME","ABBREVIAZIONE_COMBUST"),
                columns = c(
                  list(
                    ZONA = colDef(name = "Zona", show = TRUE),
                    FK_ISTAT_COMUNE = colDef(show = FALSE),
                    COMUNE = colDef(name = "Comune", show = TRUE),
                    FK_ID_SETTORE = colDef(show = FALSE),
                    SET_NOME = colDef(name = "Settore"),
                    FK_ID_COMBUSTIBILE = colDef(show = FALSE),
                    ABBREVIAZIONE_COMBUST = colDef(name = "Combustibile")
                  ),
                  prec_list,
                  var_list
                ),
                class = "reactablecss",
                onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #117A8B")
                ),
                columnGroups = list(
                  colGroup(name = "Variazioni emissive [%]", columns = paste0(PRECURSORS, "_VAR"))
                )
      )
      
    })
    
    
    
    
    #######################################################################
    # LANCIO PROCEDURE
    #######################################################################
    
    
    observeEvent(input$resetScenarioButton, {
      
      # resetto il grafo
      shinyjs::reset('trafficFile')
      trafficData$table <- trafficDT
      trafficData$map <- traffic
      
      updatePickerInput(inputId = "comuniChoiceScenariLineari", selected = "< Tutti i comuni >")
      updatePickerInput(inputId = "tipoVeicoloChoiceScenariLineari", selected = "< Tutte le tipologie >")
      updatePickerInput(inputId = "tipoStradaChoiceScenariLineari", selected = "< Tutte le tipologie >")
      updateTextInput(inputId = "riduzioneScenariLineari", value = "" )
      
      updateReactable("trafficScenarioTable", traffic, page = current_page_Traff())
      
      labels <- sprintf("<strong>ID Arco: %s</strong><br/>Tipologia: %s<br/>Veicoli Eq.: %s<br/>", traffic$ID_GRAFO, traffic$NOME_TIPO_STRADA, traffic$VeicEquiv) %>% lapply(htmltools::HTML)
      
      values <- traffic$VeicEquiv
      minVal <- min(traffic$VeicEquiv)
      maxVal <- max(traffic$VeicEquiv) * 0.6
      
      values[values >= maxVal] <- maxVal
      
      palTraffic <- colorNumeric(palette = "RdYlBu", c(minVal,maxVal), reverse = TRUE)
      title <-  "Veicoli Eq."
      
      updateTrafficMap("trafficScenarioMap",palTraffic,labels,traffic,values,c(minVal,maxVal),title)

      # resetto i comuni
      shinyjs::reset('comuniFile')
      comuniDTFiltered$selezioneDaFiltro <- FALSE
      comuniDTFiltered$selezione <- 1:nrow(comuniDT)
      comuniDTFiltered$comuni <- comuniDT
      
      updatePickerInput(inputId = "comuniChoiceScenarioDiffuse", selected = "< Tutti i comuni >")
      updatePickerInput(inputId = "residentiChoiceScenarioDiffuse", selected = CLASSE_RESIDENTI_CHOICES[[1]])
      updatePickerInput(inputId = "classeClimaChoiceScenarioDiffuse", selected = CLASSE_CLIMA_CHOICES[[1]])
      # aggiorno la tabella
      updateReactable(outputId = "diffuseScenarioTable", data = comuniDT)
      
      # resetto il parco
      shinyjs::reset('parcoFile')
      updateReactable(outputId = "convertiParcoScenarioTable", data = parco, selected = 1:nrow(parco))
      
      modificaParcoTable$parco <- parco
      updateReactable("modificaParcoScenarioTable", parco)
      
      
      # resetto le opzioni delle diffuse
      updateMaterialSwitch(inputId = "applicaAlleLineari",value = FALSE, session = session)
      updatePickerInput(inputId = "tipoStradaDiffuse", selected = c("Comunali","Urbane"))
      
      
      # resetto le riduzioni emissive
      ridEmissionData$table <- emissioniDT
      
      updatePickerInput(inputId = "zonaChoiceRidEmission", selected = "< Tutte le zone >")
      updatePickerInput(inputId = "comuniChoiceRidEmission", selected = "< Tutti i comuni >")
      updatePickerInput(inputId = "settoreChoiceRidEmission", selected = "< Tutti i settori >")
      updatePickerInput(inputId = "combustibileChoiceRidEmission", selected = "< Tutti i combustibili >")
      updatePickerInput(inputId = "inquinanteChoiceRidEmission", selected = "< Tutti gli inquinanti >")
      updateTextInput(inputId = "riduzioneRidEmission", value = "" )
      
      updateReactable("ridEmissionTable", emissioniDT, page = current_page_Traff())
      
      
      
      showModal(
        modalDialogFunction(FALSE, paste0("Sistema impostato con i valori di default!"))
      )
      
      
    })
    
    

    observeEvent(input$runScenarioButton, {

      print(paste0(Sys.time()," - Lancio lo scenario:", input$scenarioName))

      if (input$scenarioName == "") {

        sendAlert("Inserire il nome dello scenario!")
        return(NULL)

      }

      if (input$scenarioName %in% scenarioListAllfromDB()) {

        print(paste0(Sys.time()," - Nome dello scenario già presente"))

        sendAlert("Nome dello scenario già presente!")
        return(NULL)

      }

      # if (path_sanitize(input$scenarioName) != input$scenarioName) {
      # 
      #   print(paste0(Sys.time()," - Caratteri non utilizzabili nel nome dello scenario!"))
      #   sendAlert("Caratteri non utilizzabili nel nome dello scenario!")
      #   return(NULL)
      # 
      # }

      
      tryCatch({

        newScenario <- input$scenarioName
        newDescription <- input$scenarioDescription
        #directory <- clearDirectoryName(input$scenarioName)
        directory <- UUIDgenerate()
        newDir <- paste0(SCENDIR,"/", directory)
        
        progressFile <- paste0(newDir,"/","progress.txt")
        statusFile <- paste0(newDir,"/","status.txt")
        
        print(paste0(Sys.time()," - Creo la directory: ", newDir))
        dir.create(newDir)
        write.table(0,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
        write.table("Inizio calcolo",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
        
        
        tipoScenario <- match(input$defineTipoScenarioOptions,SCENARIO_DEFINITION_CHOICES)
        calcoloML <- input$runWithML
        
        if (tipoScenario == 1) { 
          
          ## scenario di modifica del traffico su grafo stradale
          newGrafo <- trafficData$table
          
          grafoFile <- paste0(newDir,"/",directory,"_grafo.csv")
          write.table(newGrafo,grafoFile,row.names = FALSE, sep = ";", dec = ".")
          
        } else if (tipoScenario == 2) { 
          
          ## scenario di politica urbana
          newComuni <- diffuseData$table
          
          comuniFile <- paste0(newDir,"/",directory,"_comuni.csv")
          write.table(newComuni,comuniFile,row.names = FALSE, sep = ";", dec = ".")
          
          
          choice <- match(input$defineTipoPoliticaOptions,SCENARIO_TIPO_POLITICA_CHOICES)
          
          if (choice == 1) {
            newParco <- convertiParcoTable() %>%
              dplyr::select(CODICE_COPART,SET_NOME,ABBREVIAZIONE_COMBUST,CLASSIFICAZIONE,TIPO_LEGISLATIVO_VEICOLO,EURO,NUMERO_VEICOLI)
          } else {
            newParco <- modificaParcoTable$parco %>%
              mutate(NUMERO_VEICOLI = NUMERO_VEICOLI * VAR) %>%
              dplyr::select(CODICE_COPART,SET_NOME,ABBREVIAZIONE_COMBUST,CLASSIFICAZIONE,TIPO_LEGISLATIVO_VEICOLO,EURO,NUMERO_VEICOLI)
          }
          
          parcoFile <- paste0(newDir,"/",directory,"_parco.csv")
          write.table(newParco,parcoFile,row.names = FALSE, sep = ";", dec = ".")
          
          if (input$applicaAlleLineari) {
            opzioni <- input$tipoStradaDiffuse
          } else {
            opzioni <- NULL
          }
          
          
        } else {
          
          ## scenario di modifica delle emissioni comunali
          newEmissioni <- ridEmissionData$table
          
          emissioniFile <- paste0(newDir,"/",directory,"_emissioni.csv")
          write.table(newEmissioni,emissioniFile,row.names = FALSE, sep = ";", dec = ".")
          
          
        }
        
        addScenarioInDB(newScenario, directory, newDescription, username)
        scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
        updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())
        
        scenario_list$started <- TRUE
        scenario_list$timer <- reactiveTimer(1000)
  
        showModal(
          modalDialogFunction(FALSE, paste0("Scenario ", newScenario , " in esecuzione!"))
        )
  
        future_promise({
          
          if (calcoloML) {
            
            calcolaEmissioniTotali(newScenario,tipoScenario,opzioni,TRUE,0,80)
            emissionsList <- calcolaEmissioniTotaliPerComune(newScenario)
            save(emissionsList, file = paste0(newDir,"/","emiPolyTable.rda"))
            emissionsStack <- calcolaEmissioniTotaliPerGriglia(newScenario,TRUE,80,90)
            save(emissionsStack, file = paste0(newDir,"/","emiRaster.rda"))
            emissionsList <- calcolaEmissioniPerGrafici(newScenario)
            dat <- creaDatiGraficiEmissioni(emissionsList, newScenario)
            save(dat, file = paste0(newDir,"/","emiPlot.rda"))
            
            concentrationsStack <- calcolaConcentrazioniML(newScenario,emissionsStack,TRUE,90,99)
            save(concentrationsStack, file = paste0(newDir,"/","concRaster.rda"))
            shpConcExtraction <- interpolaRaster(concentrationsStack,"mean",FALSE,0,0)
            save(shpConcExtraction, file = paste0(newDir,"/","concPolyTable.rda"))
            dat <- creaDatiGraficiConcentrazioni(concentrationsStack, newScenario)
            save(dat, file = paste0(newDir,"/","concBoxPlot.rda"))
            
            
          } else {
            
            calcolaEmissioniTotali(newScenario,tipoScenario,opzioni,TRUE,0,40)
            emissionsList <- calcolaEmissioniTotaliPerComune(newScenario)
            save(emissionsList, file = paste0(newDir,"/","emiPolyTable.rda"))
            emissionsStack <- calcolaEmissioniTotaliPerGriglia(newScenario,TRUE,40,50)
            save(emissionsStack, file = paste0(newDir,"/","emiRaster.rda"))
            emissionsList <- calcolaEmissioniPerGrafici(newScenario)
            dat <- creaDatiGraficiEmissioni(emissionsList, newScenario)
            save(dat, file = paste0(newDir,"/","emiPlot.rda"))
            
            concentrationsStack <- calcolaConcentrazioni(newScenario,emissionsStack,TRUE,50,99)
            save(concentrationsStack, file = paste0(newDir,"/","concRaster.rda"))
            shpConcExtraction <- interpolaRaster(concentrationsStack,"mean",FALSE,0,0)
            save(shpConcExtraction, file = paste0(newDir,"/","concPolyTable.rda"))
            dat <- creaDatiGraficiConcentrazioni(concentrationsStack, newScenario)
            save(dat, file = paste0(newDir,"/","concBoxPlot.rda"))
            
          }
          
  
  
        }) %>%
          then(
             onFulfilled = function(value) {
  
               scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
               scenario_list$value <- NULL
               
               # outDir <- paste0(SCENDIR,"/",newScenario)
  
               progressFile <- paste0(newDir,"/","progress.txt")
               statusFile <- paste0(newDir,"/","status.txt")
  
               write.table("Ok",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
               write.table(100,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
               print(paste0(Sys.time()," - Calcolo finito per lo scenario: ", newScenario))
  
             },
             onRejected = function(value) {
  
               sendAlert(paste0("Si è verificato un errore durante il calcolo dello scenario: ", newScenario))
  
               # outDir <- paste0(SCENDIR,"/",newScenario)
  
               progressFile <- paste0(newDir,"/","progress.txt")
               statusFile <- paste0(newDir,"/","status.txt")
  
               write.table("Errore",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
               write.table(100,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
               print(paste0(Sys.time(), " - Errore per lo scenario:", newScenario))
               print(paste0(Sys.time(), " - Errore: ", value))
  
             })
  
  
      }, error=function(e) {
  
        sendAlert("Si è verificato un errore durante il calcolo dello scenario!")
        write.table(100,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  
        print(e)
        # unlink(newDir, recursive = TRUE)
  
      })

      # Return something other than the future so we don't block the UI
      NULL
  
    })
     
     
    #######################################################################
    # DASHBOARD
    #######################################################################

    observe({

      updatePickerInput(
        inputId = "scenarioChoiceDashboard",
        choices = list(
          `Seleziona gli scenari da visualizzare` = scenario_list$list),
        selected = scenario_list$list,
        options = pickerOptions(
          actionsBox = TRUE,
          # countSelectedText = "{0} di {1} scenari selezionati", 
          selectedTextFormat = "count"
        )
      )

    })
    
    observe({
      
      updatePickerInput(
        inputId = "comuneChoiceDashboard",
        choices = list(
          `Seleziona il comune da visualizzare` = c("< Tutti i comuni >", comuniDT$COMUNE)),
        selected = "< Tutti i comuni >"
      )
      
    })
    
    

    DATASCEN_DASH <- eventReactive(input$scenarioChoiceDashboard, {

      req(input$scenarioChoiceDashboard)

      SCENARIO_NAME <- input$scenarioChoiceDashboard
      SCENARIO_DIR <- paste0(SCENDIR,"/",getScenarioDirectoryInDB(input$scenarioChoiceDashboard))

      SCENARIO_EMIX = do.call("rbind",lapply(paste0(SCENARIO_DIR,"/","emiPlot.rda"), function(x) base::get(load((x)))))
      SCENARIO_CONC = do.call("rbind",lapply(paste0(SCENARIO_DIR,"/","concBoxPlot.rda"), function(x) base::get(load((x)))))

      return(list(SCENARIO_NAME = SCENARIO_NAME,
                  SCENARIO_DIR = SCENARIO_DIR,
                  SCENARIO_EMIX = SCENARIO_EMIX,
                  SCENARIO_CONC = SCENARIO_CONC
      ))

    }, ignoreNULL = FALSE)

    output$emiGraph <- renderHighchart({

      scen <- input$scenarioChoiceDashboard
      comune <- input$comuneChoiceDashboard
      dat <- DATASCEN_DASH()$SCENARIO_EMIX
      
      if (comune == "< Tutti i comuni >") {
        comune = "All"
      }
      
      dat <- dat[dat$name %in% scen & dat$absDel == "absolute" & dat$COMUNE == comune,]

      # dat <- dat[match(dat$name,input$scenarioChoiceDashboard),]

      highchart() %>%
        hc_xAxis(type = "category") %>%
        hc_yAxis(min = 0, title = list(text = "Emissioni [ton]")) %>%
        hc_add_series_list(dat) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
        hc_title(
          text = "Emissioni totali",
          align = "center"
          # style = list(color = "#22A884", useHTML = TRUE)
        )
      # hc_add_theme(hc_theme_smpl())

    })

    output$deltaEmiGraph <- renderHighchart({

      scen <- input$scenarioChoiceDashboard
      comune <- input$comuneChoiceDashboard
      dat <- DATASCEN_DASH()$SCENARIO_EMIX
      
      if (comune == "< Tutti i comuni >") {
        comune = "All"
      }
      
      dat <- dat[dat$name %in% scen & dat$absDel == "delta" & dat$COMUNE == comune,]
      # dat <- dat[match(dat$name,input$scenarioChoiceDashboard),]

      highchart() %>%
        hc_xAxis(type = "category") %>%
        hc_yAxis(title = list(text = "Emissioni [ton]")) %>%
        hc_add_series_list(dat) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
        hc_title(
          text = "Variazioni emissive",
          align = "center"
          # style = list(color = "#22A884", useHTML = TRUE)
        )
      # hc_add_theme(hc_theme_smpl())

    })


    output$concGraph <- renderHighchart({

      scen <- input$scenarioChoiceDashboard
      comune <- input$comuneChoiceDashboard
      dat <- DATASCEN_DASH()$SCENARIO_CONC
      
      if (comune == "< Tutti i comuni >") {
        comune = "All"
      }
      
      dat <- dat[dat$name %in% scen & dat$absDel == "absolute" & dat$id == comune,]
      

      highchart() %>%
        hc_xAxis(type = "category") %>%
        hc_yAxis(min = 0, title = list(text = "Concentrazioni [\u00b5g/m\u00b3]")) %>%
        hc_add_series_list(dat) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
        hc_title(
          text = "Concentrazioni totali",
          align = "center"
          # style = list(color = "#22A884", useHTML = TRUE)
        )
      # hc_add_theme(hc_theme_smpl())

    })

    output$deltaConcGraph <- renderHighchart({

      scen <- input$scenarioChoiceDashboard
      comune <- input$comuneChoiceDashboard
      dat <- DATASCEN_DASH()$SCENARIO_CONC
      
      if (comune == "< Tutti i comuni >") {
        comune = "All"
      }
      
      dat <- dat[dat$name %in% scen & dat$absDel == "delta" & dat$id == comune,]

      highchart() %>%
        hc_xAxis(type = "category") %>%
        hc_yAxis(title = list(text = "Concentrazioni [\u00b5g/m\u00b3]")) %>%
        hc_add_series_list(dat) %>%
        hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
        hc_title(
          text = "Variazione delle concentrazioni",
          align = "center"
          # style = list(color = "#22A884", useHTML = TRUE)
        )
      # hc_add_theme(hc_theme_smpl())

    })

    #######################################################################
    # EMISSIONS
    #######################################################################

    observe({

      updatePickerInput(
        inputId = "scenarioChoiceEmissions",
        choices = scenario_list$list,
        selected = scenario_list$value
      )

    })


    DATA_EMIX <- eventReactive(input$scenarioChoiceEmissions, {

      req(input$scenarioChoiceEmissions)

      shinyjs::addClass(id ="scenarioDescriptionEmissions", class = "disabledText")
      # shinyjs::addClass(id ="scenarioYearEmissions", class = "disabledText")
      # shinyjs::addClass(id ="scenarioConfigurationEmissions", class = "disabledText")

      SCENARIO_NAME <- input$scenarioChoiceEmissions
      SCENARIO_DIR <- paste0(SCENDIR,"/",getScenarioDirectoryInDB(input$scenarioChoiceEmissions))
      SCENARIO_DSCR <- getScenarioDescriptionInDB(input$scenarioChoiceEmissions)
      # SCENARIO_CFG <- getScenarioConfigurationInDB(input$scenarioChoiceEmissions)
      # SCENARIO_YEAR <- getConfigurationYearInDB(SCENARIO_CFG)

      return(list(SCENARIO_NAME = SCENARIO_NAME,
                  SCENARIO_DIR = SCENARIO_DIR,
                  SCENARIO_DSCR = SCENARIO_DSCR
                  # SCENARIO_CFG = SCENARIO_CFG,
                  # SCENARIO_YEAR = SCENARIO_YEAR
      ))

    }, ignoreNULL = FALSE)



    DATASCEN_EMIX <- eventReactive(input$scenarioChoiceEmissions, {

      req(input$scenarioChoiceEmissions)

      SCENARIO_DIR <- DATA_EMIX()$SCENARIO_DIR
      # SCENARIO_CFG <- DATA_EMIX()$SCENARIO_CFG

      SCENARIO_EMIXRaster = base::get(load(paste0(SCENARIO_DIR,"/","emiRaster.rda")))
      SCENARIO_EMIXTable = base::get(load(paste0(SCENARIO_DIR,"/","emiPolyTable.rda")))
      SCENARIO_EMIXShp = comuni

      return(list(SCENARIO_EMIXRaster = SCENARIO_EMIXRaster,
                  SCENARIO_EMIXTable = SCENARIO_EMIXTable,
                  SCENARIO_EMIXShp = SCENARIO_EMIXShp
      ))

    }, ignoreNULL = FALSE)


    observe({

      value <- DATA_EMIX()$SCENARIO_DSCR
      updateTextInput(inputId = "scenarioDescriptionEmissions", value = value )

    })

    aggrEMIX <- reactive({
      match(input$aggregationChoiceEmissions,c(SPATIAL_AGGREGATION_SHP,"Griglia"))
    })


    delta_absEMIX <- reactive({
      match(input$deltaChoiceEmissions,DELTA_ABS_CHOICHES)
    })

    densityEMIX <- reactive({
      match(input$densityChoiceEmissions,EMI_DENS_CHOICHES)
    })


    observe({

      req(input$scenarioChoiceEmissions)

      updatePickerInput(
        inputId = "sectorChoiceEmissions",
        choices = c("< Tutti i settori >", SECTORS),
        selected = "< Tutti i settori >"
      )

    })


    pollEMIX <- reactive({

      pollEMIX <- PRECURSORS[match(input$pollutantChoiceEmissions,PRECURSORS_DESC)]
      
      if (input$sectorChoiceEmissions == "< Tutti i settori >") {
        msEMIX <- "All"
      } else {
        msEMIX <- SECTORS_ID[match(input$sectorChoiceEmissions,SECTORS)]
      }
      
      delta <- ""
      if (delta_absEMIX() == 2) delta <- "delta_"
      if (delta_absEMIX() == 3) delta <- "deltaP_"

      pollEMIX <- paste0(delta,pollEMIX,"_Sett",msEMIX)
      pollEMIX

    })

    dataEMIX <- reactive({

      poll <- PRECURSORS[match(input$pollutantChoiceEmissions,PRECURSORS_DESC)]
      field <- SPATIAL_AGGREGATION_FIELD[aggrEMIX()]
      
      idx <- (aggrEMIX() - 1) + delta_absEMIX()
      
      # shp <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]
      data <- DATASCEN_EMIX()$SCENARIO_EMIXTable[[idx]]
      
      if (input$sectorChoiceEmissions == "< Tutti i settori >") {
        msEMIX <- "All"
      } else {
        msEMIX <- SECTORS_ID[match(input$sectorChoiceEmissions,SECTORS)]
      }
      
      dataEMIX <- data[data$FK_ID_SETTORE == as.character(msEMIX),] %>%
        dplyr::select(all_of(c(field,poll)))
      
      dataEMIX

    })


    output$emissionsMap <- renderLeaflet({

       leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Positron (minimal)", providerTileOptions(zIndex=-100)) %>%
        addProviderTiles("CartoDB.Voyager", group = "Carto Voyager", providerTileOptions(zIndex=-100)) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)", providerTileOptions(zIndex=-100)) %>%
        # addProviderTiles(provider = names(providers)[109]) %>%
        setView(lng = 11, lat = 44.5, zoom = 9) %>% 
        # fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
        # addFullscreenControl() %>%
        addEasyprint(options = easyprintOptions(
          hidden = TRUE,
          hideControlContainer = FALSE,
          hideClasses = "absoluteEmiMapPanel",
          tileWait = 2000,
          exportOnly = TRUE,
          sizeModes = list("Custom Size"=list(
            width= 800,
            height= 800,
            name = "A custom landscape size tooltip",
            className= 'customEmiCssClass')
          ))) %>%
        # Layers control
        addLayersControl(
          baseGroups = c(
            "OpenStreetMap",
            "Positron (minimal)",
            "Carto Voyager",
            "World Imagery (satellite)"
          ),
          position = c("topleft"),
          options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)
        )

    })

    outputOptions(output, "emissionsMap", suspendWhenHidden = FALSE)

    output$minValEmiMapUI <- renderUI({

      pollx <- PRECURSORS[match(input$pollutantChoiceEmissions,PRECURSORS_DESC)]

      if (aggrEMIX() == 1) {

        shp <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]

        data <- shp %>% left_join(dataEMIX(), by = c("PRO_COM" = "FK_ISTAT_COMUNE"))
        values <- data[,pollx] %>% st_drop_geometry() %>% pull()

        if (densityEMIX() == 2 & delta_absEMIX() != 3) {
          values <- round(as.numeric(values / st_area(shp) * 10^6), digits = 3)
        }

      } else {

        data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
        rasterEmix <- data[[pollEMIX()]]

        ifelse(densityEMIX() == 1, cellArea <- 1, cellArea <- raster::values(area(rasterEmix)))

        values <- raster::values(rasterEmix) / cellArea

      }

      value <- round(min(values, na.rm = T), digits = 2)
      textInput(inputId = "minValEmiMap", label = "Min:", value = value )

    })

    output$maxValEmiMapUI <- renderUI({

      pollx <- PRECURSORS[match(input$pollutantChoiceEmissions,PRECURSORS_DESC)]

      if (aggrEMIX() == 1) {

        shp <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]

        data <- shp %>% left_join(dataEMIX(), by = c("PRO_COM" = "FK_ISTAT_COMUNE"))
        values <- data[,pollx] %>% st_drop_geometry() %>% pull()

        if (densityEMIX() == 2 & delta_absEMIX() != 3) {
          values <- round(as.numeric(values / st_area(shp) * 10^6), digits = 3)
        }

      } else {

        data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
        rasterEmix <- data[[pollEMIX()]]

        ifelse(densityEMIX() == 1, cellArea <- 1, cellArea <- raster::values(area(rasterEmix)))

        values <- raster::values(rasterEmix) / cellArea

      }

      value <- round(max(values, na.rm = T), digits = 2)
      textInput(inputId = "maxValEmiMap", label = "Max:", value = value )

    })


    observeEvent(input$updateEmiMap, {

      poll <- input$pollutantChoiceEmissions
      pollx <- PRECURSORS[match(input$pollutantChoiceEmissions,PRECURSORS_DESC)]


      ifelse(densityEMIX() == 1, units <- "[ton]", units <- "[ton/km\u00b2]")
      if (delta_absEMIX() == 3) units <- "[\u0025]"
      title <- paste(poll, units)

      if (aggrEMIX() == 1) {

        shp <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]

        data <- shp %>% left_join(dataEMIX(), by = c("PRO_COM" = "FK_ISTAT_COMUNE"))
        values <- data[,pollx] %>% st_drop_geometry() %>% pull()

        if (densityEMIX() == 2 & delta_absEMIX() != 3) {
          values <- round(as.numeric(values / st_area(shp) * 10^6), digits = 3)
        }

        # verifico assegnazione corretta dei valori
        if (is.null(input$minValEmiMap) | is.na(as.numeric(input$minValEmiMap)) ) {
          updateTextInput(session, "minValEmiMap", value = min(values))
          minVal <- min(values, na.rm = T)

        } else {

          minVal <- as.numeric(input$minValEmiMap)

        }

        # if (is.null(input$maxValEmiMap) | as.numeric(input$maxValEmiMap) > max(values) | as.numeric(input$maxValEmiMap) < min(values) | is.na(as.numeric(input$maxValEmiMap)) ) {
        if (is.null(input$maxValEmiMap) | is.na(as.numeric(input$maxValEmiMap)) ) {

          updateTextInput(session, "maxValEmiMap", value = max(values))
          maxVal <- max(values, na.rm = T)

        } else {

          maxVal <- as.numeric(input$maxValEmiMap)

        }

        if (delta_absEMIX() == 1) {
          pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent")
        } else {
          pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent", reverse = T)
        }

        labels <- sprintf("<strong>%s</strong><br/>%s: %g %s", data$COMUNE , poll, values, units) %>% lapply(htmltools::HTML)

        values[values <= minVal] <- minVal
        values[values >= maxVal] <- maxVal

        updatePolyMap("emissionsMap",pal,labels,shp,values,c(minVal,maxVal),title,"customEmiCssClass","absoluteEmiMapPanel")

      } else {

        data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
        rasterEmix <- data[[pollEMIX()]]

        ifelse(densityEMIX() == 1, cellArea <- 1, cellArea <- raster::values(area(rasterEmix)))

        values <- raster::values(rasterEmix) / cellArea

        # verifico assegnazione corretta dei valori
        if (is.null(input$minValEmiMap) | is.na(as.numeric(input$minValEmiMap)) ) {

          updateTextInput(session, "minValEmiMap", value = min(values))
          minVal <- min(values, na.rm = T)

        } else {

          minVal <- as.numeric(input$minValEmiMap)

        }

        if (is.null(input$maxValEmiMap) | is.na(as.numeric(input$maxValEmiMap)) ) {

          updateTextInput(session, "maxValEmiMap", value = max(values))
          maxVal <- max(values, na.rm = T)

        } else {

          maxVal <- as.numeric(input$maxValEmiMap)

        }

        values[values <= minVal] <- minVal + 0.00001
        values[values >= maxVal] <- maxVal - 0.00001

        rasterMapEmix <- rasterEmix/cellArea
        rasterMapEmix[rasterMapEmix <= minVal] <- minVal + 0.00001
        rasterMapEmix[rasterMapEmix >= maxVal] <- maxVal - 0.00001

        # rasterMapEmix <- raster::projectRaster(rasterMapEmix,crs = "+init=epsg:3857")

        if (delta_absEMIX() == 1) {
          pal <- colorNumeric("RdYlBu", values, na.color="transparent", reverse = T)
        } else {
          pal <- colorNumeric("RdYlBu", values, na.color="transparent")
        }

        updateRasterMap("emissionsMap",pal,rasterMapEmix,values,c(minVal,maxVal),title,"customEmiCssClass","absoluteEmiMapPanel")

      }

    })

    observeEvent(input$printEmiMap, {

      leafletProxy("emissionsMap") %>%
        easyprintMap(sizeModes="customEmiCssClass"
        )

    })




    observe({

      poll <- input$pollutantChoiceEmissions
      pollx <- PRECURSORS[match(input$pollutantChoiceEmissions,PRECURSORS_DESC)]

      ifelse(densityEMIX() == 1, units <- "[ton]", units <- "[ton/km\u00b2]")
      if (delta_absEMIX() == 3) units <- "[\u0025]"
      title <- paste(poll, units)

      if (aggrEMIX() == 1) {

        shp <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]

        data <- shp %>% left_join(dataEMIX(), by = c("PRO_COM" = "FK_ISTAT_COMUNE"))
        values <- data[,pollx] %>% st_drop_geometry() %>% pull()

        if (densityEMIX() == 2 & delta_absEMIX() != 3) {
          values <- round(as.numeric(values / st_area(shp) * 10^6), digits = 3)
        }

        if (delta_absEMIX() == 1) {
          pal <- colorNumeric("YlOrRd", values, na.color="transparent")
        } else {
          pal <- colorNumeric("YlOrRd", values, na.color="transparent", reverse = T)
        }

        labels <- sprintf("<strong>%s</strong><br/>%s: %g %s", data$COMUNE, poll, values, units) %>% lapply(htmltools::HTML)

        updatePolyMap("emissionsMap",pal,labels,shp,values,c(min(values, na.rm = T),max(values, na.rm = T)),title,"customEmiCssClass","absoluteEmiMapPanel")

      } else {

        data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
        rasterEmix <- data[[pollEMIX()]]

        ifelse(densityEMIX() == 1, cellArea <- 1, cellArea <- raster::values(area(rasterEmix)))

        values <- raster::values(rasterEmix) / cellArea

        if (delta_absEMIX() == 1) {
          pal <- colorNumeric("RdYlBu", values, na.color="transparent", reverse = T)
        } else {
          pal <- colorNumeric("RdYlBu", values, na.color="transparent")
        }

        updateRasterMap("emissionsMap",pal,rasterEmix/cellArea,values,c(min(values, na.rm = T),max(values, na.rm = T)),title,"customEmiCssClass","absoluteEmiMapPanel")

      }

    })

    observe({

      click <- input$emissionsMap_click
      poll <- input$pollutantChoiceEmissions

      if (aggrEMIX() < 1) return(NULL)

      if (!is.null(click)) {

        data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
        rasterEmix <- data[[pollEMIX()]]

        ifelse(densityEMIX() == 1, cellArea <- 1, cellArea <- raster::values(area(rasterEmix)))
        ifelse(densityEMIX() == 1, units <- "[ton]", units <- "[ton/km\u00b2]")
        if (delta_absEMIX() == 3) units <- "[\u0025]"
        # title <- paste(poll, units)

        xy <- data.frame(lon = click$lng, lat = click$lat)
        coordinates(xy) <- ~ lon + lat
        crs(xy) <- paste("+proj=longlat +datum=WGS84", sep="")

        emiValue <- raster::extract(rasterEmix/cellArea, spTransform(xy, crs(rasterEmix)))

        if (is.na(emiValue)) return(NULL)

        popup <- htmltools::HTML(sprintf("Long: %g Lat: %g<br/><strong>%s</strong>: %g %s", click$lng, click$lat, poll, round(emiValue, digits = 1), units))

        leafletProxy("emissionsMap") %>%
          clearPopups() %>%
          addPopups(click$lng, click$lat, popup = popup,
                    options = popupOptions(closeOnClick = TRUE,
                                           keepInView = FALSE)
          )

      }

    })

    #######################################################################
    # CONCENTRAZIONI
    #######################################################################

    observe({

      updatePickerInput(
        inputId = "scenarioChoiceConcentrations",
        choices = scenario_list$list,
        selected = scenario_list$value,
      )

    })

    observe({

      updatePickerInput(
        inputId = "pollutantChoiceConcentrations",
        choices = SR_POLLUTANT_CHOICES,
        selected = SR_POLLUTANT_CHOICES[1]
      )

    })

    DATA_CONC <- eventReactive(input$scenarioChoiceConcentrations, {

      req(input$scenarioChoiceConcentrations)

      shinyjs::addClass(id ="scenarioDescriptionConcentrations", class = "disabledText")
      # shinyjs::addClass(id ="scenarioConfigurationConcentrations", class = "disabledText")
      # shinyjs::addClass(id ="scenarioYearConcentrations", class = "disabledText")

      SCENARIO_NAME <- input$scenarioChoiceConcentrations
      SCENARIO_DIR <- paste0(SCENDIR,"/",getScenarioDirectoryInDB(input$scenarioChoiceConcentrations))
      # SCENARIO_DSCR <- getScenarioDescriptionInDB(input$scenarioChoiceConcentrations)
      # SCENARIO_CFG <- getScenarioConfigurationInDB(input$scenarioChoiceConcentrations)
      # SCENARIO_YEAR <- getConfigurationYearInDB(SCENARIO_CFG)

      return(list(SCENARIO_NAME = SCENARIO_NAME,
                  SCENARIO_DIR = SCENARIO_DIR
                  # SCENARIO_DSCR = SCENARIO_DSCR,
                  # SCENARIO_CFG = SCENARIO_CFG,
                  # SCENARIO_YEAR = SCENARIO_YEAR
      ))

    }, ignoreNULL = FALSE)

    DATASCEN_CONC <- eventReactive(input$scenarioChoiceConcentrations, {

      req(input$scenarioChoiceConcentrations)

      SCENARIO_DIR <- DATA_CONC()$SCENARIO_DIR
      SCENARIO_CFG <- DATA_CONC()$SCENARIO_CFG

      SCENARIO_CONCRaster = base::get(load(paste0(SCENARIO_DIR,"/","concRaster.rda")))
      SCENARIO_CONCTable = base::get(load(paste0(SCENARIO_DIR,"/","concPolyTable.rda")))
      SCENARIO_CONCShp = comuni


      return(list(SCENARIO_CONCRaster = SCENARIO_CONCRaster,
                  SCENARIO_CONCTable = SCENARIO_CONCTable,
                  SCENARIO_CONCShp = SCENARIO_CONCShp
      ))

    }, ignoreNULL = FALSE)


    observe({

      value <- DATA_CONC()$SCENARIO_DSCR
      updateTextInput(inputId = "scenarioDescriptionConcentrations", value = value )

    })


    aggrCONC <- reactive({
      match(input$aggregationChoiceConcentrations,c(SPATIAL_AGGREGATION_SHP,"Griglia"))
    })


    delta_absCONC <- reactive({
      match(input$deltaChoiceConcentrations,DELTA_ABS_CHOICHES)
    })




    pollCONC <- reactive({

      poll <- SR_POLLUTANT[match(input$pollutantChoiceConcentrations,SR_POLLUTANT_CHOICES)]

      delta <- ""
      if (delta_absCONC() == 2) delta <- "delta_"
      if (delta_absCONC() == 3) delta <- "deltaP_"

      pollCONC <- paste0(delta,poll)
      pollCONC

    })


    output$concentrationsMap <- renderLeaflet({

      leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Positron (minimal)", providerTileOptions(zIndex=-100)) %>%
        addProviderTiles("CartoDB.Voyager", group = "Carto Voyager", providerTileOptions(zIndex=-100)) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)", providerTileOptions(zIndex=-100)) %>%
        # addProviderTiles(provider = names(providers)[109]) %>%
        setView(lng = 11, lat = 44.5, zoom = 9) %>%
        # fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
        # addFullscreenControl() %>%
        addEasyprint(options = easyprintOptions(
          hidden = TRUE,
          hideControlContainer = FALSE,
          hideClasses = "absoluteEmiMapPanel",
          tileWait = 2000,
          exportOnly = TRUE,
          sizeModes = list("Custom Size"=list(
            width= 800,
            height= 800,
            name = "A custom landscape size tooltip",
            className= 'customEmiCssClass')
          ))) %>%
        # Layers control
        addLayersControl(
          baseGroups = c(
            "OpenStreetMap",
            "Positron (minimal)",
            "Carto Voyager",
            "World Imagery (satellite)"
          ),
          position = c("topleft"),
          options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)
        )

    })

    outputOptions(output, "concentrationsMap", suspendWhenHidden = FALSE)

    output$minValConcMapUI <- renderUI({

      if (aggrCONC() == 1) {

        data <- DATASCEN_CONC()$SCENARIO_CONCTable[[aggrCONC()]]
        shpConc <- data[,c('NAME',pollCONC())]

        values <- shpConc[,2]

      } else {

        data <- DATASCEN_CONC()$SCENARIO_CONCRaster
        rasterConc <- data[[pollCONC()]]
        values <- raster::values(rasterConc)

      }

      value <- min(values)
      textInput(inputId = "minValConcMap", label = "Min:", value = value )

    })

    output$maxValConcMapUI <- renderUI({

      if (aggrCONC() == 1) {

        data <- DATASCEN_CONC()$SCENARIO_CONCTable[[aggrCONC()]]
        shpConc <- data[,c('NAME',pollCONC())]

        values <- shpConc[,2]

      } else {

        data <- DATASCEN_CONC()$SCENARIO_CONCRaster
        rasterConc <- data[[pollCONC()]]
        values <- raster::values(rasterConc)

      }

      value <- max(values)
      textInput(inputId = "maxValConcMap", label = "Max:", value = value )

    })

    observeEvent(input$updateConcMap, {

      units <- SR_UNIT[match(input$pollutantChoiceConcentrations,SR_POLLUTANT_CHOICES)]
      poll <- input$pollutantChoiceConcentrations

      if (delta_absCONC() == 3) units <- "[\u0025]"
      title <- paste(poll, units)

      if (aggrCONC() == 1) {

        shp <- DATASCEN_CONC()$SCENARIO_CONCShp[[aggrCONC()]]

        data <- DATASCEN_CONC()$SCENARIO_CONCTable[[aggrCONC()]]
        shpConc <- data[,c('NAME',pollCONC())]
        values <- shpConc[,2]

        # verifico assegnazione corretta dei valori
        if (is.null(input$minValConcMap) | is.na(as.numeric(input$minValConcMap)) ) {

          updateTextInput(session, "minValConcMap", value = min(values))
          minVal <- min(values)

        } else {

          minVal <- as.numeric(input$minValConcMap)

        }

        if (is.null(input$maxValConcMap) | is.na(as.numeric(input$maxValConcMap)) ) {

          updateTextInput(session, "maxValConcMap", value = max(values))
          maxVal <- max(values)

        } else {

          maxVal <- as.numeric(input$maxValConcMap)

        }

        if (delta_absCONC() == 1) {
          pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent")
        } else {
          pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent", reverse = T)
        }

        labels <- sprintf("<strong>%s</strong><br/>%s: %g %s", shpConc$NAME, poll, values, units) %>% lapply(htmltools::HTML)

        values[values <= minVal] <- minVal
        values[values >= maxVal] <- maxVal

        updatePolyMap("concentrationsMap",pal,labels,shp,values,c(minVal,maxVal),title,"customConcCssClass","absoluteConcMapPanel")

      } else {

        data <- DATASCEN_CONC()$SCENARIO_CONCRaster
        rasterConc <- data[[pollCONC()]]
        values <- raster::values(rasterConc)

        # verifico assegnazione corretta dei valori
        if (is.null(input$minValConcMap) | is.na(as.numeric(input$minValConcMap)) ) {

          updateTextInput(session, "minValConcMap", value = min(values))
          minVal <- min(values)

        } else {

          minVal <- as.numeric(input$minValConcMap)

        }

        if (is.null(input$maxValConcMap) | is.na(as.numeric(input$maxValConcMap)) ) {

          updateTextInput(session, "maxValConcMap", value = max(values))
          maxVal <- max(values)

        } else {

          maxVal <- as.numeric(input$maxValConcMap)

        }

        values[values <= minVal] <- minVal + 0.00001
        values[values >= maxVal] <- maxVal - 0.00001

        rasterMapConc <- rasterConc
        rasterMapConc[rasterMapConc <= minVal] <- minVal + 0.00001
        rasterMapConc[rasterMapConc >= maxVal] <- maxVal - 0.00001

        if (delta_absCONC() == 1) {
          pal <- colorNumeric("RdYlBu", c(minVal,maxVal), na.color="transparent", reverse = T)
        } else {
          pal <- colorNumeric("RdYlBu", c(minVal,maxVal), na.color="transparent")
        }

        updateRasterMap("concentrationsMap",pal,rasterMapConc,values,c(minVal,maxVal),title,"customConcCssClass","absoluteConcMapPanel")

      }

    })

    observeEvent(input$printConcMap, {

      leafletProxy("concentrationsMap") %>%
        easyprintMap(sizeModes="customConcCssClass")

    })


    observe({

      req(input$pollutantChoiceConcentrations)

      units <- SR_UNIT[match(input$pollutantChoiceConcentrations,SR_POLLUTANT_CHOICES)]
      poll <- input$pollutantChoiceConcentrations

      if (delta_absCONC() == 3) units <- "[\u0025]"
      title <- paste(poll, units)

      if (aggrCONC() == 1) {

        shp <- DATASCEN_CONC()$SCENARIO_CONCShp[[aggrCONC()]]

        data <- DATASCEN_CONC()$SCENARIO_CONCTable[[aggrCONC()]]
        shpConc <- data[,c('NAME',pollCONC())]
        values <- shpConc[,2]

        if (delta_absCONC() == 1) {
          pal <- colorNumeric("YlOrRd", values, na.color="transparent")
        } else {
          pal <- colorNumeric("YlOrRd", values, na.color="transparent", reverse = T)
        }

        labels <- sprintf("<strong>%s</strong><br/>%s: %g %s", shpConc$NAME, poll, values, units) %>% lapply(htmltools::HTML)

        updatePolyMap("concentrationsMap",pal,labels,shp,values,c(min(values),max(values)),title,"customConcCssClass","absoluteConcMapPanel")

      } else {

        data <- DATASCEN_CONC()$SCENARIO_CONCRaster
        rasterConc <- data[[pollCONC()]]
        values <- raster::values(rasterConc)

        if (delta_absCONC() == 1) {
          pal <- colorNumeric("RdYlBu", values, na.color="transparent", reverse = T)
        } else {
          pal <- colorNumeric("RdYlBu", values, na.color="transparent")
        }

        updateRasterMap("concentrationsMap",pal,rasterConc,values,c(min(values, na.rm = T),max(values, na.rm = T)),title,"customConcCssClass","absoluteConcMapPanel")


      }

    })

    observe({

      click <- input$concentrationsMap_click
      units <- SR_UNIT[match(input$pollutantChoiceConcentrations,SR_POLLUTANT_CHOICES)]
      poll <- input$pollutantChoiceConcentrations

      if (delta_absCONC() == 3) units <- "[\u0025]"

      if (aggrCONC() < 1) return(NULL)

      if (!is.null(click)) {

        data <- DATASCEN_CONC()$SCENARIO_CONCRaster
        rasterConc <- data[[pollCONC()]]

        xy <- data.frame(lon = click$lng, lat = click$lat)
        coordinates(xy) <- ~ lon + lat
        crs(xy) <- paste("+proj=longlat +datum=WGS84", sep="")

        concValue <- raster::extract(rasterConc, spTransform(xy, crs(rasterConc)))

        if (is.na(concValue)) return(NULL)

        popup <- htmltools::HTML(sprintf("Long: %g Lat: %g<br/><strong>%s</strong>: %g %s", click$lng, click$lat, poll, concValue, units))

        leafletProxy("concentrationsMap") %>%
          clearPopups() %>%
          addPopups(click$lng, click$lat, popup = popup,
                    options = popupOptions(closeOnClick = TRUE,
                                           keepInView = FALSE)
          )

      }

    })

    #######################################################################
    # SCENARIO CONFIGURATION
    #######################################################################

    observe({

      scenario_list$timer()

      isolate({

        if (scenario_list$started) {

          progressList <- getScenarioProgressInDB()
          
          if (any(is.na(progressList))) {
            return(NULL)
          }
          
          if (all(progressList == 100)) {

            # stoppo il refresh
            scenario_list$timer <- reactiveTimer(Inf)
            scenario_list$started <- FALSE

          } else {

            updateProgressScenario()
            updateStatusScenario()

          }

          scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
          scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
          # updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())

        }

      })

    })


    output$scenarioConfigTable <- renderReactable({

      scenarioDT <- scenario_list$dataframe
      
      reactable(scenarioDT,
                compact = TRUE,
                pagination = TRUE,
                wrap = FALSE,
                searchable = FALSE,
                striped = FALSE,
                highlight = TRUE,
                bordered = TRUE,
                defaultPageSize = 15,
                defaultColDef = colDef(headerClass = "header", align = "left", vAlign = "center", headerVAlign = "center", headerStyle = "min-height: 40px", sortable = FALSE),
                columns = list(
                  scenarioName = reactable::colDef(maxWidth = 200, name = "Scenario"),
                  directory = reactable::colDef(show = FALSE),
                  description = reactable::colDef(name = "Descrizione",
                                                  cell = function(value) {
                                                    div(style = "cursor: help",
                                                        tippy(value, value))
                                                  }),
                  dateCreation = reactable::colDef(name = "Data creazione", maxWidth = 150),
                  userName = reactable::colDef(name = "Utente", maxWidth = 150),
                  progress = colDef(
                    name = "Avanzamento",
                    # defaultSortOrder = "desc",
                    cell = JS('function(cellInfo) {
                        // Format as percentage
                        const pct = (cellInfo.value).toFixed(1) + "%"
                        // Pad single-digit numbers
                        let value = pct.padStart(5)
                        let bck = cellInfo.row.status === "Errore" ? "#ff0000" : "#28a745"
                        // Render bar chart
                        return `
                          <div class="bar-cell">
                            <span class="number">${value}</span>
                            <div class="bar-chart" style="background-color: #e1e1e1">
                              <div class="bar" style="width: ${pct}; background-color: ${bck}"></div>
                            </div>
                          </div>
                        `
                      }'),
                      html = TRUE
                  ),
                  status = colDef(
                    name = "Stato",
                    width = 200,
                    cell = function(value) {
                      if (value  == "Ok" )
                        div(icon("check", class = "fas", style = "color:green;padding-right: 5px"), "Elaborazione terminata")
                      else if (value  == "Errore" )
                        div(icon("xmark", class = "fas", style = "color:red;padding-right: 5px"), "Si è verificato un errore")
                      else
                        div(icon("rotate", class = "fas", style = "color:orange;padding-right: 5px"), value)
                    }),
                  UpdateBtn = reactable::colDef(sortable = FALSE, html = TRUE, name = "Modifica", maxWidth = 70, align = "center", cell = function(value) {
                    if (is.na(value)) {
                      htmltools::tags$div(class = "disabledTableCell")
                    } else {
                      htmltools::tags$div(shiny::actionButton("scenList_Update",label = NULL, icon = icon("pen-to-square")))
                    }
                  }) ,
                  DownloadBtn = reactable::colDef(sortable = FALSE, html = TRUE, name = "Scarica", maxWidth = 65, align = "center", cell = function(value) {
                    if (is.na(value)) {
                      htmltools::tags$div(class = "disabledTableCell")
                    } else {
                      htmltools::tags$div(shiny::actionButton("scenList_Download",label = NULL, icon = icon("download")))
                    }
                  }) ,
                  DeleteBtn = reactable::colDef(sortable = FALSE, html = TRUE, name = "Elimina", maxWidth = 70, align = "center", cell = function(value) {
                    if (is.na(value)) {
                      htmltools::tags$div(class = "disabledTableCell")
                    } else {
                      htmltools::tags$div(shiny::actionButton("scenList_Delete",label = NULL, icon = icon("trash-alt")))
                    }
                  })
                ),
                onClick = JS("function(rowInfo, column) {
															    if (column.id !== 'UpdateBtn' && column.id !== 'DeleteBtn' && column.id !== 'DownloadBtn') {
															      return
															    }

															    if (window.Shiny) {
															      Shiny.setInputValue('scenarioConfigAction', { index: rowInfo.index + 1, button: column.id }, { priority: 'event' })
															    }
															  }"),
                theme = reactableTheme(
                  backgroundColor = "white",
                  borderColor = "#dfe2e5",
                  stripedColor = "#f6f8fa",
                  highlightColor = "#f0f5f9",
                  cellPadding = "3px 4px",
                  # style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                  searchInputStyle = list(width = "100%")
                ),
              class = "reactablecss"

        )


    })
    
    outputOptions(output, "scenarioConfigTable", suspendWhenHidden = FALSE)

    selected_row_Scen <- reactive({
      input$scenarioConfigAction$index
    })

    selected_button <- reactive({
      input$scenarioConfigAction$button
    })

    current_page_Scen <- reactive({
      getReactableState("scenarioConfigTable")$page
    })

    observeEvent(input$scenarioConfigAction, {

      action <- input$scenarioConfigAction$button
      button <- scenario_list$dataframe[selected_row_Scen(), selected_button()]

      scenName <- scenario_list$dataframe[selected_row_Scen(), "scenarioName"]
      scenDesc <- scenario_list$dataframe[selected_row_Scen(), "description"]

      if (action == "UpdateBtn" & !is.na(button)) {

        showModal(
          modalDialog(
            title = "Modifica Scenario",
            # p(values$dataframe[selected_row_Scen(), ]),
            textInput(inputId = "newScenarioName",
                      label = "Nome scenario",
                      value = scenName),
            textInput(inputId = "newDescription",
                      label = "Descrizione",
                      value = scenDesc),
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton("saveUpdateScenario", "Save")
            )
          )
        )

      } else if (action == "DeleteBtn" & !is.na(button)) {

        showModal(
          modalDialog(
            title = "Conferma",
            span("Eliminare lo scenario ", tags$b(scenName), "?"),
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton("deleteScenario", "OK")
            )
          )
        )

      } else if (action == "DownloadBtn" & !is.na(button)) {

        showModal(
          modalDialog(
            title = "Conferma",
            span("Scaricare i file dello scenario ", tags$b(scenName), "?"),
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton("downloadScenario", "OK")
            )
          )
        )

      }


    })

    observeEvent(input$downloadScenario, {
      # runjs("$('#downloadData')[0].click();") # DOWNLOAD BUTTON
      shinyjs::click("downloadData")
      removeModal()

    })

    output$downloadData = downloadHandler(

      filename = function() {

        scenName <- scenario_list$dataframe[selected_row_Scen(), "scenarioName"]
        filezip <- paste0(scenName,".zip")
        print(paste0(Sys.time()," - Scarico il file : ", filezip))
        return(filezip)

      },
      content = function(filezip) {

        scenName <- scenario_list$dataframe[selected_row_Scen(), "scenarioName"]
        directory <- getScenarioDirectoryInDB(scenName)
        filezip <- paste0(SCENDIR,"/",directory,"/",directory,".zip")
        zip(zipfile = filezip, files = list.files(path = paste0(SCENDIR,"/",directory,"/"), pattern = "csv", full.names = TRUE))

      },
      contentType = "application/zip"

    )


    observeEvent(input$saveUpdateScenario, {

      oldDir <- paste0(SCENDIR,"/",scenario_list$dataframe[selected_row_Scen(), "directory"])
      newDir <- paste0(SCENDIR,"/",clearDirectoryName(input$newScenarioName))
      file.rename(oldDir,newDir)
      
      #rinomino i file di input
      oldfileName <- paste0(newDir,"/",oldDir,"_parco.csv")
      newfileName <- paste0(newDir,"/",newDir,"_parco.csv")
      if (file.exists(oldfileName)) {
        file.rename(oldfileName,newfileName)
      }
      
      oldfileName <- paste0(newDir,"/",oldDir,"_comuni.csv")
      newfileName <- paste0(newDir,"/",newDir,"_comuni.csv")
      if (file.exists(oldfileName)) {
        file.rename(oldfileName,newfileName)
      }
      
      oldfileName <- paste0(newDir,"/",oldDir,"_grafo.csv")
      newfileName <- paste0(newDir,"/",newDir,"_grafo.csv")
      if (file.exists(oldfileName)) {
        file.rename(oldfileName,newfileName)
      }
      
      oldfileName <- paste0(newDir,"/",oldDir,"_emissioni.csv")
      newfileName <- paste0(newDir,"/",newDir,"_emissioni.csv")
      if (file.exists(oldfileName)) {
        file.rename(oldfileName,newfileName)
      }
      

      ## aggiorno i grafici della dashboard
      data <- base::get(load(paste0(newDir,"/emiPlot.rda")))
      data$name <- input$newScenarioName
      save(data , file = paste0(newDir,"/emiPlot.rda"))
      data <- base::get(load(paste0(newDir,"/concBoxPlot.rda")))
      data$name <- input$newScenarioName
      save(data , file = paste0(newDir,"/concBoxPlot.rda"))

      updateScenarioInfoInDB(scenario_list$dataframe[selected_row_Scen(), "scenarioName"],input$newScenarioName,clearDirectoryName(input$newScenarioName),input$newDescription)

      scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
      scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))

      updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())

      removeModal()

    })

    observeEvent(input$deleteScenario, {

      deleteScenarioInDB(scenario_list$dataframe[selected_row_Scen(), "scenarioName"])

      dirToRemove <- paste0(SCENDIR,"/",scenario_list$dataframe[selected_row_Scen(), "directory"])
      unlink(dirToRemove, recursive = TRUE)

      # scenario_list$dataframe <- scenario_list$dataframe[-selected_row_Scen(),]
      scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
      scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
      # scenario_list$fromOtherUser <- getScenarioPublicFromDB(username,usergroup)

      updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())

      removeModal()


    })


    
    
  }
)




