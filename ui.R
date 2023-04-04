ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = "stijl.css",
  
  tabsetPanel(
    id = "swing_wizard",
    type = "hidden",
    
    header = HTML("<h1> <strong> Monitordata uit SPSS omzetten naar Kubusbestanden voor Swing </strong></h1>"),
    
    tabPanel("home",
             column(width = 6,
                    
                    fluidRow(
                      actionButton("naar_kies_data1",icon = icon("wrench"),
                                   "Maak een nieuwe configuratie",
                                   style = "height:400px; width: 100%; font-size: 200%")),
                    
                    fluidRow(
                      actionButton("naar_lees", icon = icon("book"),
                                   "Voer een configuratie uit", 
                                   style = "height:400px; width: 100%; font-size: 200%"))
                    )
             ),
    
    #Instellen van de configuratie over meerdere verspreid
    #Dit verhoogt de kans dat er goed gelezen wordt voordat een configuratie wordt opgeslagen
    tabPanel("kies_data",
             fluidRow(
               column(6,
                      box(width = 12,
                          HTML(glue::glue("<h1><strong> 1. Kies een SPSS .sav bestand </strong></h1>
                          <p><ul> 
                           <li> Klik op <strong>Lees een SPSS-bestand</strong> en selecteer een .sav bestand uit de map 
                           <strong>{basismap_spss_bestanden}</strong>
                           </li>
                           
                           <li> Zorg dat alle gebruikte variabelen <strong> volledig gelabeld </strong> zijn in SPSS.</li>  
                           
                           <li> Zorg dat de dataset alleen data uit de eigen regio bevat </li>
                           
                           <li> Voor iedere variabele die bij <strong>Variabelen</strong> wordt geselecteerd zal een kubusdatabestand
                           opgeslagen worden in de map <strong>{basismap_output}</strong></li>
                           
                           
                           </ul></p>")
                             )
                        )
                      ),
               
               column(6,
                      box(width = 12,
                          shinyFiles::shinyFilesButton("spss_bestand","Lees een SPSS-bestand",
                                                       "Lees een SPSS-bestand",F,
                                                       icon = icon("folder"),
                                                       style = "height:200px; width: 100%; font-size: 200%;"),
                          uiOutput("spss_data_preview")%>% shinycssloaders::withSpinner())
                      )
               ),
             
             fluidRow(
               
               navigatieknoppen("naar_home1","naar_home2","naar_naam_config1"))
             ),
    
    tabPanel("naam_config",
            
             column(6,                
                    box(width = 12,
                      HTML(
                      glue::glue(
                      "<h1><strong> 2. Kies de bestandsnaam van de configuratie en de naam van de bron </strong></h1>
                      
                      <p><ul><li> Het <strong>configuratiebestand</strong> wordt als een.xlsx bestand opgeslagen in <strong>{basismap_configuraties}</strong>.
                      </li>
                           
                      <li> De <strong> Naam van de databron </strong> bepaald bij welke 'source' indicatoren worden ingedeeld in Swing 
                      </li>
                           
                      <li> Klik op <strong> Voeg een bron toe </strong> om een nieuwe bronnaam toe te voegen
                      </li>
                           
                      <li> Om verder te kunnen moeten er een bestandsnaam EN bronnaam gekozen zijn. 
                      </li></ul></p>"))
                      )
                    ),
             
             column(6,
                    
                    box(width = 12,
                        uiOutput("input_bestandsnaam_configuratie"),
                        shinyjs::hidden(uiOutput("input_configuratie_overschrijven")),
                        uiOutput("input_bron"),
                        uiOutput("bron_toevoegen")
                        )
                    ),
             
             fluidRow(
               navigatieknoppen("naar_kies_data2","naar_home3","naar_variabelen_en_crossings1"))
             ),
    
  tabPanel("variabelen_en_crossings",
           
           fluidRow(
             column(6,
                    box(width = 12,
                        HTML(glue::glue("<h1><strong> 3. Selecteer crossings en inhoudelijke variabelen </strong></h1>
                        <p><ul><li> Selecteer crossings en inhoudelijke variabelen door er op te klikken. 
                           Variabelen aan de rechterkant van de invoer zijn geselecteerd. </strong>. 
                           </li>
                           
                           <li> Als alternatieve invoermethode kunnen inhoudelijke variabelen geplakt worden uit <strong> Excel of SPSS </strong>. 
                           Bij een grote hoeveelheid variabelen is dit sneller dan handmatig aanklikken.
                           </li>
                           
                           <li> Als er al een configuratie bestaat met dezelfde bestandsnaam kan gekozen worden deze te overschrijven. </li>
                           
                           <li> Om verder te kunnen moeten er <strong> minstens 1 crossing en 1 variabele </strong> geselecteerd zijn. 
                           </li>
                           
                           </ul></p>")
                      )
                    )
             ),
             
             column(6,
                    box(width = 12,
                        fluidRow(
                          uiOutput("input_crossings")),
                        fluidRow(
                          uiOutput("input_variabelen"),
                          uiOutput("input_geplakte_variabelen")
                          )
                        )
                    )
             ),
           fluidRow(
             navigatieknoppen("naar_naam_config2","naar_home4","naar_periode1")
             )
           ),
  
  
  
  tabPanel("periode",
           
           fluidRow(
             column(6,
                    box(width = 12,
                      HTML(glue::glue("<h1><strong> 4. Stel periode in </strong></h1>
                        
                        
                           <p> Geef op deze pagina aan in over welke periode de kubusdata gaat.
                           <br>
                           Als je dataset in het geheel samengevat kan worden in 1 periode: 
                           
                           <ul>
                           <li> Laat <strong>Per jaar analyseren? </strong> uitstaan.</li> 
                           <li> Vul bij <strong>Type Periode</strong> de indeling van de periode in (bijvoorbeeld: 'Jaar')</li>
                           <li> Vul bij <strong>Naam Periode</strong> de naam van de periode in (bijvoorbeeld: '2022')</li>
                           </ul>
                           </p>
                           
                           <p>Als je dataset gegevens bevat die per jaar uitgesplitst moeten worden:
                           <br>
                           <ul>
                           <li> Vink <strong> Per jaar analyseren? </strong> aan </li> 
                           <li> Kies de variabele die de periode vastlegt in het drop-down menu; <strong>Kies de jaarvariabele</strong></li>
                           <li> Kies welke jaren uit de dataset meegenomen moeten worden in het selectiemenu 
                           <strong>Kies welke jaren meegnomen moeten worden</strong> </li> 
                           </ul>
                           </p>")))
                    ),
             
             column(6,       
                    box(width = 12,
                        fluidRow(
                          uiOutput("input_is_meer_jaar")),
                        fluidRow(
                          uiOutput("input_jaarvariabele"),
                          uiOutput("input_jaren_analyse"),
                          shinyjs::hidden(uiOutput("input_type_periode")),
                          shinyjs::hidden(uiOutput("input_naam_periode")))
                        ))
             ),
           
           fluidRow(
             navigatieknoppen("naar_variabelen_en_crossings2","naar_home5","naar_gebiedsindeling1"))
           ),
  
  tabPanel("gebiedsindeling",
           
           fluidRow(
             column(6,
                    box(width = 12,
                        HTML(glue::glue(
                          "<h1><strong> 5. Kies de gebiedsindeling </strong></h1>
                        
                        
                           <p> Stel op deze pagina in of de data op regioniveau of per gemeente geanalyseerd moet worden.
                           </p>

                           <ul><li> <strong>Let op:</strong> De Wizard neemt aan dat de SPSS data <strong> alleen data uit de eigen regio </strong> bevat. 
                           Als de data landelijke / bovenregionale data bevat wordt dit niet goed verwerkt. </li> 
                           
                           <li> De ingestelde regiocode voor Swing is <strong> {nr_regio} </strong>. Dit kan aangepast worden in global.R </li>
                           
                           <li> Als bij <strong>Niveau gebied</strong> 'gemeente' is gekozen moet in de dropdown
                           <strong> Gebiedsindeling </strong> een variabele geselecteerd worden die de gemeenten vastlegt.</li>
                           </ul>
        
                           </p>"
                          )))
                    ),
             
             column(6,
                    box(width = 12,
                        fluidRow(box(uiOutput("input_gebiedsniveau"))),
                        fluidRow(uiOutput("input_gebiedsindeling"))))
             ),
        
           fluidRow(
             navigatieknoppen("naar_periode2","naar_home6","naar_gewogen")
           )),
  
  tabPanel("gewogen",
           
           fluidRow(
             column(6,
                    box(width = 12,
                        HTML("<h1><strong> 5. Gewogen design & minimum observaties per groep instellen </strong></h1>
                             <p><ul><li> Vink <strong> Gewogen design?</strong> aan als de kubusdata gewogen  uitgerekend moeten worden
                             </li>
                             <li> Selecteer vervolgens bij <strong>Weegfactor</strong> de variabele die de weegfactor vastlegt
                             </li>
                             <li> Stel bij <strong> mimimum aantal observaties per groep </strong> de 'Treshold' in.
                             Swing zal de data van groepen die een lager aantal <strong>ongewogen</strong> observaties hebben
                             afschermen.
                             </li>
                             <li> Klik als je klaar bent rechtsonder op <strong>Configuratie opslaan</strong>
                             </li></ul></p>"))
                    ),
             column(6,
                    box(width = 12,
                        fluidRow(
                          uiOutput("input_is_gewogen")
                          ),
                        fluidRow(
                          uiOutput("input_weegfactor"),
                          uiOutput("input_minimum_observaties")
                          )
                        )
                    )
             ),
           
           fluidRow(
             div(style = "position: absolute; bottom: 0; left: 0; right: 0",  
                 
                 column(offset = 1,
                        3,
                        actionButton(inputId = "naar_gebiedsindeling2",
                                     label ="Vorige",
                                     icon = icon("fa-sharp fa-solid fa-arrow-left",
                                                 verify_fa = F),
                                     style = "height:100px; width: 100%; font-size: 200%;")
                 ),
                 column(3,
                        actionButton(inputId = "naar_home7",
                                     label = "Home",
                                     icon = icon("home"),
                                     style = "height:100px; width: 100%; font-size: 200%;")
                 ),
                 
                 column(3,
                        uiOutput("input_maak_configuratie")
                        
                        
                 )
                 )
             
           )
           ),
  
  tabPanel("lees_configuratie",
             
             fluidRow(
               
               column(width = 6,
                      box(width = 12,
                          HTML("<h1> <strong> Lees een configuratie en voer de configuratie uit</strong></h1>
                               <p><ul><li> Klik op <strong> Lees een configuratie </strong> om een eerder opgeslagen configuratie te gebruiken.
                               </li>
                               <li> Controleer in de tabellen of de juiste variabelen zijn geselecteerd en ze correct gelabeld zijn.
                               Als de labels NIET volledig zijn <strong> Kan een configuratie NIET uitgevoerd worden </strong>
                               </li>
                               <li> Klik op <strong> Bewerk configuratie </strong> als een configuratie aangepast moet worden. 
                               </li>
                               <li> Klik op <strong> Selecteer een map voor kubusdata </strong> om te bepalen waar de .xlsx bestanden met 
                               kubusdata worden opgeslagen.
                               </li>
                               <li> Selecteer <strong> Alleen 'Data' sheets maken </strong> als de gemaakte bestanden alleen data moeten bevatten
                               Dit is nuttig om data aan te vullen van indicatoren die al in Swing staan. De gemaakte .xlsx bestanden bevatten
                               dan geen metadata. Hierdoor zullen de namen van indicatoren/dimensies niet overschreven worden bij een upload naar Swing.
                               Dit voorkomt dat handmatige aanpassingen van namen in Swing teniet worden gedaan.
                               </li>
                               </ul>
                               "))
                      ),
               
               column(width = 6,
                      box(width = 12,
                          shinyFiles::shinyFilesButton("configuratie_bestand",
                                                       "Lees een Configuratiebestand",
                                                       "Lees een Configuratiebestand",F),
                          
                          uiOutput("geen_map_gekozen"),
                          shinyDirButton("folder","Selecteer een map voor kubusdata","Kies een bestemming voor kubusdata", F),
                          checkboxInput("alleen_data","Alleen 'Data' sheets maken")
                          )
                      )
             ),
          

             #Validatie van configuratie
             fluidRow(
               br(),
               column(4,uiOutput("overzicht_crossings") %>% shinycssloaders::withSpinner()),
               column(4,uiOutput("overzicht_variabelen")%>% shinycssloaders::withSpinner()),
               column(4,uiOutput("overzicht_gebiedsindeling")%>% shinycssloaders::withSpinner())

               ),
             
             fluidRow(
               div(style = "position: absolute; bottom: 0; left: 0; right: 0",  
               column(width = 5, offset = 1, 
                             #Knoppen Maak kubusdata & bewerk kubusdata
                             uiOutput("knop_bewerk_configuratie")
                             ),
               column(width = 5, 
                      uiOutput("knop_maak_kubusdata")
                      )
               )
               
               )
             )
    
 
  
)
)