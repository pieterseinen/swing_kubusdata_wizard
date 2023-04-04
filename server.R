server <- function(input,output, session){
  
#### Data & Configuratiebestanden ####
  
#Reactive df met SPSS data. 
  #Wordt opgehaald uit fileinput OF configuratie
  #Reactiveval die bijhoudt waar de data opgehaald moet worden 
  databron_uit_configuratie <- reactiveVal(F)

  #Dataframe
  spss_data <- reactive({
    
    if(databron_uit_configuratie()){
      #spss data inlezen uit lees_configuratie
      
      df <-  haven::read_spss(configuratie_algemeen()$bestandsnaam,user_na =T) %>%
        labelled::user_na_to_na()
    }else{
      
      
      #spss data inlezen inputveld spss_data op pagina maak_configuratie
      df <-  haven::read_spss(glue("{basismap_spss_bestanden}/{input$spss_bestand$files$`0`[[2]]}"), user_na =T) %>%
        labelled::user_na_to_na()
    }
    
    #Arbitraire regiovariabele aanmaken: Makkelijker dan een group_by statement 
    #aan/uitzetten o.b.v. de ingstelde regio 
    df$Regiovar_met_uniekenaam <- nr_regio
    var_label(df$Regiovar_met_uniekenaam) <- "Regio"
    val_label(df$Regiovar_met_uniekenaam, nr_regio) <- naam_regio 
    
    df
  })
  
  #Apparte reactive voor bestandsnaam (t.b.v het wegschrijven van configuratie)
  spss_bestandsnaam <- reactive({
    if(databron_uit_configuratie()){
      configuratie_algemeen()$bestandsnaam
    }else{
      glue("{basismap_spss_bestanden}/{input$spss_bestand$files$`0`[[2]]}")
    }
  })
  
  
  #Vector met alle kolommen uit SPSS data tbv inputs
  alle_kolommen <- reactive({
    kolommen <- names(spss_data())
  })
  
#Configuratie
  #str die vastlegt waar de configuratie uit gelzen moet worden
  locatie_configuratiebestand <- reactiveVal("leeg")
  

  #trigger(): arbitraire reactiveVal die veranderd als een configuratie veranderd
  #Dit is nodig om ervoor te zorgen dat de configuratie_... reactive dataframes altijd updaten. 
  
  #Reactives updaten alleen wanneer objecten binnen de reactive veranderen.
  #Als iem. een configuratie leest, bewerkt en daarna opslaat onder DEZELFDE naam hebben de reactive configuratie_ dataframes geen verandering waargenomen. 
  #de Reactiveval: locatie_configuratiebestand() heeft namelijk dezelfde inhoud (de naam vh configuratiebestand).
  
  #Om een update van de dataframes te forceren wordt de arbitraire waarde trigger veranderd als een configuratie veranderd
  trigger <- reactiveVal(0)
  
  
  observeEvent(input$configuratie_bestand,{
    
    if(is.list(input$configuratie_bestand)){
      
      databron_uit_configuratie(T)
    
      #Naam configuratiebestand uit fileinput halen
      naam_configuratiebestand(str_remove(input$configuratie_bestand$files$`0`[[2]],".xlsx"))

      #configuratie_gelezen(T)
      locatie_configuratiebestand(glue("{basismap_configuraties}/{input$configuratie_bestand$files$`0`[[2]]}"))
      
      nieuwe_trigger <- trigger()+1
      trigger(nieuwe_trigger)
    }
    
  })
  
  
  #data uit gelezen configuratie ophalen
    #df algemene instellingen
    configuratie_algemeen <- reactive({
      req(locatie_configuratiebestand() != "leeg")
      x <- trigger()
      
      openxlsx::read.xlsx(locatie_configuratiebestand(), sheet = "algemeen")
      
      
    })
    #Vector met gekozen variabelen
    configuratie_variabelen <- reactive({
      req(locatie_configuratiebestand() != "leeg")
      x <- trigger()
      openxlsx::read.xlsx(req(locatie_configuratiebestand()), sheet = "variabelen")$variabelen
    })
    
    #Vector met gekozen crossings
    configuratie_crossings <- reactive({
      req(locatie_configuratiebestand() != "leeg")
      x <- trigger()
      
      openxlsx::read.xlsx(req(locatie_configuratiebestand()), sheet = "crossings")$crossings
      
    })
    
    
    #Functie om configuratie op te slaan. In een functie gezet om het kopieren van code te voorkomen
    #Ik zou een functie normaal gesproken in global.R zetten, maar op deze manier hoeven we geen argumenten
    #aan de functie te geven omdat alle objecten in de functie al binnen de scope van sessie bestaan
    
    sla_configuratie_op <- function(){
      workbook <- createWorkbook()
      
      #Algemene instellingen toevoegen
      addWorksheet(workbook, sheetName = "algemeen")
      writeData(workbook, sheet = "algemeen",
                cbind("bestandsnaam" = spss_bestandsnaam(),
                      "bron" = input$bron,
                      "meer_jaren_in_bestand" = input$is_meer_jaar,
                      "jaarvariabele" = ifelse(input$is_meer_jaar, input$jaarvariabele,
                                               input$type_periode),
                      "jaren_voor_analyse" = ifelse(input$is_meer_jaar, str_c(input$jaren_analyse,collapse = ","),
                                                    input$naam_periode),
                      "is_gewogen" = input$is_gewogen,
                      "strata" = input$strata,
                      "weegfactor" = if(input$is_gewogen){input$weegfactor}else{""} ,
                      "gebiedsniveau" = input$gebiedsniveau,
                      "gebiedsindeling" = 
                        if(input$gebiedsniveau == "ggd"){"Regiovar_met_uniekenaam"}else{
                          input$gebiedsindeling},
                      "minimum_observaties" = input$minimum_observaties)
      )
      
      #Variabelen toevoegen
      addWorksheet(workbook, sheetName = "variabelen")
      writeData(workbook, sheet = "variabelen", c("variabelen",input$variabelen))
      
      #Crossings toevoegen
      addWorksheet(workbook, sheetName = "crossings")
      writeData(workbook, sheet = "crossings", c("crossings",input$crossings))
      
      saveWorkbook(workbook, glue("{basismap_configuraties}/{input$bestandsnaam_configuratie}.xlsx"), overwrite = T)
      
      
    }
    
    
  
  
  
####Stap 0: Home ####  
  #Homeknoppen navigeren naar home
  observeEvent(c(input$naar_home1,input$naar_home2,input$naar_home3,input$naar_home4,input$naar_home5,input$naar_home6, input$naar_home7),
               switch_page("home")
               )
  

  
  #Alle 'volgende' knoppen uitschakelen bij opstarten
  #Deze worden 'enabled' wanneer er aan voorwaarden is voldaan
  observeEvent(session,
               
               #'Volgende' uitzetten
               
               lapply(c("naar_naam_config1",
                        "naar_variabelen_en_crossings1",
                        "naar_periode1",
                        "naar_gebiedsindeling1",
                        "naar_gewogen",
                        "configuratie_opslaan"
                        #,"maak_kubusdata",
                        #"naar_opslaan"
                        ), function(x){
                 
                 shinyjs::disable(x)
                 
               })
  )
  


####Stap 1: kies_data####
  #### Navigatie naar Kies Data ####
  #Navigatie: Van home naar naar kies_data  
    kies_data_navigatie <- reactive(list(input$naar_kies_data1,input$naar_kies_data2))
  
    observeEvent(kies_data_navigatie(),
                 if(any(kies_data_navigatie() > 0)){  
                 switch_page("kies_data")}
                 )

  #### UI/Inputs voor Kies Data ####
  #FileInput voor SPSS bestand
  shinyFileChoose(input, 'spss_bestand', roots=c(wd = basismap_spss_bestanden), filetypes = c('','sav'))
  
  #DF met basale info over spss bestand 
    output$spss_data_preview <- function(){
      
      validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = "Lees een SPSS bestand"))
      data.frame(
        "Bestandsnaam" = spss_bestandsnaam(),
        "Aantal observaties" = nrow(spss_data()),
        "Aantal variabelen" = ncol(spss_data())) %>%
        knitr::kable("html")%>%
        kable_styling("striped", full_width = F) %>%
        add_header_above(c("Dataset" = 3))
      
      
      
    }
    
  #### Voorwaarden Kies Data ####
  #Voorwaarden voor volgende pagina:
  #bij upload spss_bestand: Databron uit Input & Enable knop 'volgende'
    observeEvent(c(input$spss_bestand),{
  
      if(is.list(input$spss_bestand)){
        
        #databron komt uit upload ipv configuratie
        databron_uit_configuratie(F)
        
        #Knop 'Volgende' aanzetten
        shinyjs::enable("naar_naam_config1")
      }else if(databron_uit_configuratie()){
        shinyjs::enable("naar_naam_config1")
        }
        
      })

  
  
#### Stap 2: naam_config ####
  #### Navigatie naar Naam Configuratie #####
  naam_config_navigatie <- reactive(list(input$naar_naam_config1,input$naar_naam_config2))
  #naar naam_config
  observeEvent(naam_config_navigatie(),
               if(any(naam_config_navigatie() > 0))
               switch_page("naam_config")
               )
  

  #### UI/Inputs Naam Configuratie####
  #TextInput voor de bestandsnaam van de configuratie
  output$input_bestandsnaam_configuratie <- renderUI({
    
    
    #Als gegevens uit config komen; invullen in input
    alvast_geselecteerd <- if(databron_uit_configuratie()){
      
      naam_configuratiebestand()

      
    }else{
      NULL
    }
    
    textInput(inputId = "bestandsnaam_configuratie",
              label = "Geef de configuratie een bestandsnaam",
              value = alvast_geselecteerd,
              width = "50%")
    
    
  })
  
  #Checkbox om bestandsnaam te overschrijven
  output$input_configuratie_overschrijven <- renderUI({
    
    box(title = "Er is al een configuratie met deze naam!",
        
        checkboxInput(inputId = "configuratie_overschrijven",
                      label = "Overschrijven?",
                      value = F,
                      width = "100%"),
        class = "cbcontainer"
    )
    
  })
  
  #Input voor lijst met bronnen.
  #Omdat er door typefouten/verschillen tussen manier van opschrijven er meerdere bronnamen voor dezelfde bron ontstonden is er gekozen een dropdownmenu te gebruiken
  #Een gebruiker kiest uit een lijst met bestaande bronnen & kan zonodig een bron toevoegen aan de lijst
  #lijst opgeslagen in www/bronnen.csv
  
  #Lijst met opgeslagen bronnen ophalen als reactive
  bronnen <- reactive({
    
    read.csv("www/bronnen.csv")
    
  })
  
  #Pickerinput voor Bron
  output$input_bron <- renderUI({
    
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    
    #Input vullen met info uit config / of nieuw toegevoegde ingevoerde naam
    alvast_geselecteerd <- if(!is.null(input$naam_nieuwe_bron)){
      
      #Als er een nieuwe naam is ingevoerd: selecteer die naam
      input$naam_nieuwe_bron
    }else if (databron_uit_configuratie()){
      #Als er een configuratie wordt bewerkt: selecteer bron uit config
      configuratie_algemeen()$bron
    }else{
      #Als er nog niks gekozen is: "Kies een bron"
      "Kies een bron"
    }
    
    
    
    pickerInput(inputId = "bron",
                label = "Naam van databron in Swing (bv. Kindermonitor)",
                choices =  c(bronnen()$bron,alvast_geselecteerd, input$naam_nieuwe_bron),
                selected = alvast_geselecteerd
    )
    
  })
  
  #Actionbutton voor het toevoegen van een bron
  output$bron_toevoegen <- renderUI({
    actionButton("bron_toevoegen","Voeg een bron toe")
    
  })
  
  #Modal (popup) met optie om bron toe te voegen
  nieuwe_bron <- function() {
    modalDialog(
      
      HTML("<h1><strong> Voer de naam van een bron in</h1></strong>"),
      
      
      #Naaminput
      textInput("naam_nieuwe_bron","Naam bron"),
      
      actionButton("naam_bron_opslaan","Opslaan")
    )
  }
  
  #Als knop bron_toevoegen geklikt wordt: Modal laten zien om naam in te voeren
  observeEvent(input$bron_toevoegen,{
    
    showModal(nieuwe_bron())
    
  }) 
  
  #Als knop naam_bron_opslaan geklikt wordt; lijst met bronnen bijwerken
  observeEvent(input$naam_bron_opslaan,{
    
    bijgewerkte_bronnen <- bronnen() %>% rbind(input$naam_nieuwe_bron)
    
    write.csv(bijgewerkte_bronnen,"www/bronnen.csv", row.names = F)
    
    removeModal()
  })
  
  #### Voorwaarden Naam configuratie ####
  #Checken of aan de voorwaarden op de pagina naam_config is voldaan
  #Wanneer een input wordt gebruikt op de naam_config pagina: 
  #valideer inputs & zet volgende AAN als ze oke zijn.
  inputs_naam_config <- reactive(list(input$bron,
                                      input$bestandsnaam_configuratie,
                                      input$configuratie_overschrijven)) 
  
  

  observeEvent(unlist(inputs_naam_config()) ,{
    
    #Bestaat de bestandsnaam al?
    bestandsnaam_bestaat <- file.exists(glue("{basismap_configuraties}/{input$bestandsnaam_configuratie}.xlsx"))

    #Als er al een config bestaat met zelfde bestandsnaam: overschrijven?
    if(bestandsnaam_bestaat){
      #Laat checkbox voor overschrijven zien
      shinyjs::show("input_configuratie_overschrijven")

    } else{
      shinyjs::hide("input_configuratie_overschrijven")
    }

    
    #De bestandsnaam is prima waneer er iets is ingevuld EN deze naam nog niet bestaat, of overschreven mag worden
    bestandsnaam_ok <- nchar(input$bestandsnaam_configuratie) > 0 & (!bestandsnaam_bestaat |  isTruthy(input$configuratie_overschrijven))
    
    #Bron is prima wanneer er iets is ingevuld
    bron_ok <- input$bron != "Kies een bron"
    
    
    if(bestandsnaam_ok & bron_ok){
      shinyjs::enable("naar_variabelen_en_crossings1")
    }else{
      shinyjs::disable("naar_variabelen_en_crossings1")
    } 
           

  })
  
#### Stap 3 Variabelen en Crossings ####
  #### Navigatie naar variabelen en crossings####
  variabelen_en_crossings_navigatie <- reactive(list(input$naar_variabelen_en_crossings1,input$naar_variabelen_en_crossings2))
  
  observeEvent(variabelen_en_crossings_navigatie(),
               if(any(variabelen_en_crossings_navigatie() > 0)){
                   switch_page("variabelen_en_crossings")
                   
                 }
               )
  
  #### UI/Inputs Variabelen en Crossings####
  #Input voor crossings#
  output$input_crossings <- renderUI({
    
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    
    #Als er een configuratie bewerkt moet worden; selecteer de vast ingestelde variabelen
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_crossings()}else{NULL}
    
    multiInput(inputId = "crossings",
               label = "Crossings",
               choices = alle_kolommen(),
               selected = alvast_geselecteerd,
               width = "100%")
    
    
  })
  
  
  #Input voor variabelen #
  
  #Picker Inputveld voor variabelen
  output$input_variabelen <- renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    

    #Als een config bewerkt wordt: Input vullen met gegevens uit config
    alvast_geselecteerd_uit_config <- if(databron_uit_configuratie()){

        configuratie_variabelen()
    
    }else{
      NULL
      }
    

    #Variabelen uit alternatieve invoer ophalen
    geselecteerd_door_plakken <- geplakte_variabelen()

    #Unique over gecombineerde vector v. config & geplakte variabelen zodat overlap in variabelnamen
    #tussen beide geen issue is.    
    alvast_geselecteerd <- unique(c(geselecteerd_door_plakken,alvast_geselecteerd_uit_config))
    
    #Input variabelen
    multiInput(inputId = "variabelen",
               label = "Variabelen",
               choices = alle_kolommen(),   
               selected = alvast_geselecteerd,
               width = "100%")
    
    
  })
  
  #Alternatieve input voor variabele waar je een lijst variabelen in kunt plakken door een kolom uit excel te kopieren
  output$input_geplakte_variabelen <-  renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    textInput("plak_variabelen","Heb je geen zin om alle variabelen aan te klikken in bovenstaand menu? \n
                                Plak hier dan een lijst variabelen uit excel / spss!")
    
  })
  
  #ReactiveVal die geplakte variabelen opslaat
  geplakte_variabelen <- reactiveVal(NULL)
  
  #Als de tekstinput waar variabalen in geplakt kunnen worden bewerkt wordt:
  #Toevoegen aan reactiveVal met geplakte variabele
  observeEvent(input$plak_variabelen,{
    
    net_geplakt <- str_split(input$plak_variabelen," ") %>% unlist()
    
    
    geplakte_variabelen(c(geplakte_variabelen(),net_geplakt))
    
    
  })
  
  #### Voorwaarden Variabelen en Crossings####
  #Checken of aan de voorwaarden op de pagina variabelen_en_crossings is voldaan: tenminste 1 van elk geselecteerd
  inputs_variabelen_en_crossings <- reactive(list(input$variabelen,input$crossings)) 
  
  observeEvent(unlist(inputs_variabelen_en_crossings()),{

    if(length(input$variabelen) > 0 & length(input$crossings) > 0){
      shinyjs::enable("naar_periode1")
    }else{
      shinyjs::disable("naar_periode1")
    }
  })

#### Stap 4 Periode ####
  #### Navigatie naar Periode####
  periode_knoppen <- reactive(list(input$naar_periode1,input$naar_periode2))
  
  observeEvent(periode_knoppen(),
               if(any(periode_knoppen() > 0)){
                 switch_page("periode")
               }
    
  )
  
  #### UI/Inputs Perioden####
  
  #Heeft de dataset meerdere jaren waar onderscheid tussen gemaakt moet worden
  output$input_is_meer_jaar <- renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    
    #Als een config bewerkt wordt: neem instelling over in input 
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_algemeen()$meer_jaren_in_bestand}else{FALSE}
    
    
    box(checkboxInput(inputId = "is_meer_jaar",
                      label = "Per jaar analyseren?",
                      value = alvast_geselecteerd,
                      width = "100%"),
        class = "cbcontainer")
    
    
  })
  
  #Inputveld voor een jaarvariabele
  output$input_jaarvariabele <- renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_algemeen()$jaarvariabele}else{"Kies een variabele"}
    
    
    pickerInput("jaarvariabele",
                "Kies de jaarvariabele",
                choices = c(alle_kolommen(),"Kies een variabele"),
                options = list(`live-search` = T),
                selected = alvast_geselecteerd,
                width = "100%")
    
  })
  
  #jaren in data reactiveVal
  #jaren_in_data <- reactiveVal()
  
  #Inputveld voor de jaren die in de analyse meegenomen moeten worden
  output$input_jaren_analyse <- renderUI({
    validate(need((is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg") & input$jaarvariabele != "Kies een variabele", message = ""))
    
    jaren_in_data <- unique(spss_data()[[input$jaarvariabele]])
 
    alvast_geselecteerd <- if(databron_uit_configuratie()){
        
        configuratie_algemeen()$jaren_voor_analyse %>%
          str_split(",")%>%
          unlist()%>%
          as.numeric()
        
    }else{
        NULL
    }
    
    multiInput(inputId = "jaren_analyse",
               label = "Kies welke jaren meegenomen moeten worden",
               choices = jaren_in_data,
               selected = alvast_geselecteerd,
               width = "100%")
    
    
  })
  
  #Inputveld voor naam periode; Alleen relevant wanneer er geen jaarvariabele in bestand is opgegeven
  output$input_naam_periode <- renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    
    
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_algemeen()$jaren_voor_analyse}else{NULL}
    
    textInput(inputId = "naam_periode",
              label = "Naam periode",
              value = alvast_geselecteerd,
              width = "50%")
    
  })
  #Input voor type periode; Alleen relevant wanneer er geen jaarvariabele in het bestand is opgegeven
  output$input_type_periode <- renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    
    
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_algemeen()$jaarvariabele}else{NULL}
    
    textInput(inputId = "type_periode",
              label = "Type periode",
              value = alvast_geselecteerd,
              width = "50%")
    
    
  })
  
  #### Voorwaarden Periode####
  #Checken of aan alle voorwaarden voor perioden is voldaan
  periode_inputs <- reactive(list(input$is_meer_jaar,input$jaarvariabele,input$jaren_analyse, input$type_periode,input$naam_periode))
  
  observeEvent(periode_inputs(),{

    if(isTruthy(input$is_meer_jaar)){
      #Relevante inputs tonen en verbergen
      shinyjs::hide("input_type_periode")
      shinyjs::hide("input_naam_periode")
      shinyjs::show("input_jaarvariabele")
      shinyjs::show("input_jaren_analyse")
      
      #Is er een var geselecteerd & zijn er jaren uitgekozen?
      if(isTruthy(input$jaarvariabele != "Kies een variabele") & isTruthy(length(input$jaren_analyse) > 0 )){
        
          shinyjs::enable("naar_gebiedsindeling1")
          
        }else{
          
          shinyjs::disable("naar_gebiedsindeling1")
          
        }
      #Als input$is_meer_jaar == F
      }else{
        #Relevante inputs tonen en verbergen
      shinyjs::show("input_type_periode")
      shinyjs::show("input_naam_periode")
      shinyjs::hide("input_jaarvariabele")
      shinyjs::hide("input_jaren_analyse")
      
      if(isTruthy(nchar(input$type_periode) > 0) & isTruthy(nchar(input$naam_periode) > 0)){
        shinyjs::enable("naar_gebiedsindeling1")
      }else{
        shinyjs::disable("naar_gebiedsindeling1")
      }
        
      }

  })
  
#### Stap 5: Gebiedsindeling ####  
  #### Navigatie naar Gebiedsindeling ####
  knoppen_gebiedsindeling <- reactive(list(input$naar_gebiedsindeling1,input$naar_gebiedsindeling2))
  
  observeEvent(knoppen_gebiedsindeling(),
               if(any(knoppen_gebiedsindeling() > 0))
               switch_page("gebiedsindeling")
               )
  #### UI/Inputs Gebiedsindeling ####
  #Gebiedsniveau, huidige opties: ggd / gemeente
  output$input_gebiedsniveau <- renderUI({
    
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    
    alvast_geselecteerd <- if(databron_uit_configuratie()){
      configuratie_algemeen()$gebiedsniveau}else{"gemeente"}
    
    
    radioButtons("gebiedsniveau",
                 label = "Niveau gebied",
                 choices = c("ggd","gemeente"),
                 selected = alvast_geselecteerd
    )  
  })
  
  
  #Picker voor variabele gebiedsindeling
  output$input_gebiedsindeling <- renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_algemeen()$gebiedsindeling}else{"Kies een variabele"}
    
    
    pickerInput("gebiedsindeling","Gebiedsindeling",
                choices = c("Kies een variabele",
                            alle_kolommen()),
                selected = alvast_geselecteerd,
                options = list(`live-search` = T),
                width = "100%")
    
  })
  
  #### Voorwaarden Gebiedsindeling ####
  inputs_gebiedsindeling <- reactive(list(input$gebiedsniveau,input$gebiedsindeling))
  
  
  observeEvent(inputs_gebiedsindeling(),{
    
               req(input$gebiedsniveau)
    
               if(input$gebiedsniveau == "gemeente"){
                 
                 shinyjs::show("gebiedsindeling")
                 
                 if(input$gebiedsindeling == "Kies een variabele"){
                   disable("naar_gewogen")
                 }else{
                   enable("naar_gewogen")
                 }
                #Als selectie ggd is: 
               }else{
                 
                shinyjs::hide("gebiedsindeling")
                shinyjs::enable("naar_gewogen")
               }
  })

#### Stap 6: Gewogen / Min observaties ####
  #### Navigatie naar Gewogen ####
  observeEvent(input$naar_gewogen,{
    
    switch_page("gewogen")
    
  })
  
  #### UI/Inputs Gewogen / Min observaties ####  
  #Checkbox; is het een gewogen design?
  output$input_is_gewogen <- renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_algemeen()$is_gewogen}else{FALSE}
    
    box(
      checkboxInput(inputId = "is_gewogen",
                    label = "Gewogen design?",
                    value = alvast_geselecteerd,
                    width = "100%"),
      class = "cbcontainer"
    )
    
  })
  
  #Inputveld voor weegfactor
  output$input_weegfactor <- renderUI({
    validate(need((is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg") & input$is_gewogen, message = ""))
    
    
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_algemeen()$weegfactor}else{"Kies een variabele"}
    
    pickerInput("weegfactor","Weegfactor",
                choices = c("Kies een variabele",
                            alle_kolommen()),
                selected = alvast_geselecteerd,
                options = list(`live-search` = T),
                width = "100%")
    
  })
  
  
  
  #Input voor minimum observaties
  output$input_minimum_observaties <- renderUI({
    validate(need(is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg", message = ""))
    alvast_geselecteerd <- if(databron_uit_configuratie()){configuratie_algemeen()$minimum_observaties}else{30}
    
    numericInput("minimum_observaties","Minimum aantal observaties per groep", 
                 value = alvast_geselecteerd,
                 width = "100%")
    
  })
  
  #ReactivVal om bestandsnaam configuratie in op te slaan
  #Wordt bijgewerkt bij lezen van configuratie / opslaan van nieuwe configuratie
  #Vult 'alvast_geselecteerd' voor input$bestandsnaam_configuratie
 
  
  #Knop: Configuratie opslaan
  output$input_maak_configuratie <- renderUI({
    validate(need(
      #Knop alleen renderen wanneer er een spss bestand gelezen uit fileInput of configuratiebestand 
      (is.list(input$spss_bestand) | locatie_configuratiebestand() != "leeg") &&
        #Knop alleen renderen wanneer de volgende parameters ingevuld zijn:
        #crossings, variabelen, gebiedsindeling
        (nchar(input$crossings) > 0 && nchar(input$variabelen) > 0 &&
        (input$gebiedsniveau == "ggd" || (nchar(input$gebiedsindeling) > 0 &&  input$gebiedsindeling != "Kies een variabele")) 
         
        ), message = ""))
    
    
    actionButton(inputId = "configuratie_opslaan",
                 label = div("Configuratie opslaan",icon("fa-sharp fa-solid fa-arrow-right",
                                                         verify_fa = FALSE)),
                 style = "height:100px; width: 100%; font-size: 200%;")
    
    
  })
  
  #Als iemand op de knop maak_configuratie klikt
  observeEvent(input$configuratie_opslaan,{
    
    sla_configuratie_op()
    
    naam_configuratiebestand(input$bestandsnaam_configuratie)
    
    locatie_configuratiebestand(glue("{basismap_configuraties}/{input$bestandsnaam_configuratie}.xlsx"))
    
    #Willekeurge reactiveVal trigger() van waarde laten veranderen
    #Dit is gedaan omdat reactives alleen verversen wanneer er iets in veranderd.
    #In het geval dat een eerder opgeslagen configuratie bewerkt wordt en wordt opgeslagen met dezelfde naam
    #Heeft de reactive die de configuratie leest niet door dat er iets veranderd is. Het is immers hetzelfde bestandsnaam
    #We dwingen een refresh af door een trigger() te veranderen en aan te roepen in de reactives() die de configuratie lezen
    
    nieuwe_trigger <- trigger()+1
    trigger(nieuwe_trigger)
    
    switch_page("lees_configuratie")
    
  })
  
  
  #### Voorwaarden Gewogen ####
  inputs_gewogen <- reactive(list(input$minimum_aantal,input$weegfactor,input$is_gewogen))
  
  observeEvent(inputs_gewogen(),{
    
    if(isTruthy(input$is_gewogen) &  isTruthy(input$weegfactor == "Kies een variabele")){
      shinyjs::disable("configuratie_opslaan")
    }else{
      shinyjs::enable("configuratie_opslaan")
    }
               
  })
  
  
#### Stap 7: Configuratie lezen ####
  #### Navigatie: Configuratie Lezen ####
  observeEvent(input$naar_lees, switch_page("lees_configuratie"))
  
  #Naar bewerk: Overschrijven bestandsnaam config aan & switch naar kies_data
  observeEvent(input$bewerk_configuratie,{
    
    databron_uit_configuratie(T)
    
    geplakte_variabelen(NULL)
    updateCheckboxInput(session,"configuratie_overschrijven",value =  T)
    switch_page("kies_data")
    
    shinyjs::enable("naar_naam_config1")
    
  })

  #### UI /Inputs voor Configuratie Lezen ####
  
  #Input voor configuratiebestand
  shinyFileChoose(input, 'configuratie_bestand', roots=c(wd = basismap_configuraties), filetypes = c('','xlsx'))
  
  #Reactiveval waar naam_configuratiebestand in wordt opgeslagen 
  naam_configuratiebestand <- reactiveVal()
  
  
  
  #Mapselectie voor bestemming kubusdata
  shinyDirChoose(input, 'folder', roots=c(wd= basismap_output), filetypes=c('', 'xlsx'))
  
  #gekozen_map als lege reactive initialiseren zodat er gevalideerd kan worden of een bestand is gekozen
  gekozen_map <- reactiveVal("")
  
  #### path opslaan in 'gekozen_map'
  observeEvent(input$folder,{
    
    geselecteerde_map <- unlist(input$folder[[1]])
    
    geselecteerde_map <- str_c(geselecteerde_map[2:length(geselecteerde_map)], collapse = '/' ) 
    
    gekozen_map(glue("{basismap_output}/{geselecteerde_map}")) 
    
    
    }
    )
  #UI die laat zien welke/of er een map is gekozen
  output$geen_map_gekozen <- renderUI({
    
    if(gekozen_map() == "" | length(unlist(input$folder[[1]])) < 2 ){
      HTML("<h3 style='color:red;'> Kies een bestemming voor kubusdata")
    }else{
      HTML(glue("<h3><strong> Bestemming kubusdata:</strong><br> {gekozen_map()}</h3>"))
    }

  })
  
  
  #Knop voor bewerken configuratie
  output$knop_bewerk_configuratie <- renderUI({
    validate(need(nchar(locatie_configuratiebestand()) > 0 ,""))
    
    actionButton("bewerk_configuratie",
                 "Bewerk configuratie",
                 icon = icon("wrench"),
                 style = "height:100px; width: 100%; font-size: 200%;")
    
    
  })
  
  #Knop voor maken kubusdata.
  output$knop_maak_kubusdata <- renderUI({

    disabled(
      actionButton("maak_kubusdata",
                   "Maak een swing kubusbestand",
                   icon = icon("rocket"),
                   style = "height:100px; width: 100%; font-size: 200%;")
    )

  })
  
  #Waarschuwing bij problemen met labels
  observeEvent(is.list(input$configuratie_bestand) | input$swing_wizard == "lees_configuratie" | is.list(input$folder),{
    
    if(input$swing_wizard == "lees_configuratie"){
    #Als er ergens een label ontbreekt:
    if(!(crossing_labels_compleet() & variabele_labels_compleet() & gebiedsindeling_labels_compleet())){

      sendSweetAlert(session = session,
                     title = "Fout",
                     text = "Er zijn variabelen/crossings/gebiedsindelingen waarbij de labels niet compleet zijn.
                      Pas dit aan in SPSS, of verwijder deze variabelen.",
                     type = "error")
      
      disable("knop_maak_kubusdata")

    }else{
      if(is.list(input$folder) & length(unlist(input$folder[[1]])) < 2){
        
        
      enable("knop_maak_kubusdata")
      }
    }
    }
  })
  
  #Modal (popup) met voortgangsinfo maak_kubusdata
  voortgang <- function() {
    modalDialog(
      
      HTML("<h1><strong> R is een kubusbestand aan het maken</h1></strong>"),
      
      #Voortgang 
      progressBar(id = "voortgang_rij",
                  value = 0,
                  #total = 100,
                  title = "",
                  display_pct = T)
      )
  }
  
  #Optie om alleen data sheets te uploaden
  
  #Als iemand op de maak_kubusdata knop klikt: Maak kubusdata
  observeEvent(input$maak_kubusdata,{
    
    #Popup laten zien die voortgang bijhoudt
    showModal(voortgang())
    
    
    maak_kubusdata(data_totaal = spss_data(),
                   
                   jaren_voor_analyse = configuratie_algemeen()$jaren_voor_analyse,
                   heeft_meer_jaar = configuratie_algemeen()$meer_jaren_in_bestand,
                   jaarvariabele = configuratie_algemeen()$jaarvariabele,
                   is_gewogen = configuratie_algemeen()$is_gewogen,
                   weegfactor = configuratie_algemeen()$weegfactor,
                   gebiedsindeling = configuratie_algemeen()$gebiedsindeling,
                   geolevel = configuratie_algemeen()$gebiedsniveau,
                   variabelen = configuratie_variabelen(),
                   crossings = configuratie_crossings(),
                   min_observaties_antwoord = configuratie_algemeen()$minimum_observaties,
                   bron = configuratie_algemeen()$bron,
                   session = session,
                   gekozen_map = gekozen_map(),
                   alleen_data = input$alleen_data
    )
  })
  
  #### Voorwaarden Configuratie uitvoeren ####
  #Validatie: DF's met basale info over gekozen paramets
  
  #Validatie van instellingen
  #Variabelen valideren
  df_variabele_levels <- reactive({
    
    levels_per_variabele <- lapply(configuratie_variabelen(), function(x){
      
      
      
      #Labels voor tabel
      variabele_label <- var_label(spss_data()[[x]])
      if(is.null(variabele_label)){variabele_label <- NA}
      
      variabele_values <- unname(val_labels(spss_data()[[x]]))
      if(is.null(variabele_values)){variabele_values <- NA}
      
      variabele_value_labels <-names(val_labels(spss_data()[[x]]))
      if(is.null(variabele_value_labels)){variabele_value_labels <- NA}
      

      
      
      cbind("variabele" = x,
            "variabele_label" = variabele_label,
            "values" = variabele_values,
            "value_labels" = variabele_value_labels)
    })
    
    do.call(rbind, levels_per_variabele) %>% as.data.frame()
    
    
  })
  
  #Zijn alle labels er voor variabelen / levels?
  variabele_labels_compleet <- reactive({
    #Er mogen geen variabele / value labels ontbreken. 
    !any(is.na(df_variabele_levels()$value_labels)) & !any(is.na(df_variabele_levels()$variabel_label))
    
    
  })
  
  
  #Tabel renderen met overzicht variabele
  output$overzicht_variabelen <- function(){
    
    validate(need(nchar(locatie_configuratiebestand()) > 0 & 
                    locatie_configuratiebestand() != "leeg"
                  ,"Lees een configuratiebestand"))
    
    df_variabele_levels() %>%
      knitr::kable("html")%>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(c("Variabelen" = 4))%>%
      scroll_box(height = "500px")
  }
  
  
  #Crossings valideren
  df_crossing_levels <- reactive({
    
    
    levels_per_crossing <- lapply(configuratie_crossings(), function(x){
      
      #Afwezige labels
      crossing_levels <- val_labels(spss_data()[[x]])
      crossing_naam <- var_label(spss_data()[[x]])
      crossing_value <- unname(crossing_levels)
      crossing_value_labels <- names(crossing_levels)
      
      
      if(is.null(crossing_levels)){crossing_levels <- NA}
      if(is.null(crossing_naam)){crossing_naam <- NA}
      if(is.null(crossing_value)){crossing_value <- NA}
      if(is.null(crossing_value_labels)){crossing_value_labels <- NA}
      
      
      cbind("crossing" = x,
            "crossing_label" = crossing_naam,
            "values" = crossing_value,
            "value_labels" = crossing_value_labels) 
    })
    
    
    do.call(rbind, levels_per_crossing) %>% as.data.frame()
    
    
  })
  
  #Zijn alle labels er voor crossings/ levels?
  crossing_labels_compleet <- reactive({
    
    !any(is.na(df_crossing_levels()$value_labels)) & !any(is.na(df_crossing_levels()$crossing_label))
    
  })
  
  #Tabel renderen met overzicht crossings
  output$overzicht_crossings <- output$overzicht_crossings2 <- function(){
    validate(need(nchar(locatie_configuratiebestand()) > 0 & 
                    locatie_configuratiebestand() != "leeg"
                  ,""))
    
    #Tabel output
    df_crossing_levels() %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(c("Crossings" = 4))%>%
      scroll_box(height = "500px")
  }
  
  #Gebiedsindeling valideren
  df_gebiedsindeling_levels <- reactive({
    
    variabele_gebied <- configuratie_algemeen()$gebiedsindeling
    
    variabele_label <- var_label(spss_data()[[variabele_gebied]])
    variabele_values <- unname(val_labels(spss_data()[[variabele_gebied]]))
    variabele_value_labels <- names(val_labels(spss_data()[[variabele_gebied]]))
    
    
    if(is.null(variabele_label)){variabele_label <- NA}
    if(is.null(variabele_values)){variabele_values <- NA}
    if(is.null(variabele_value_labels)){variabele_value_labels <- NA}
    
                                    
    data.frame(
      "variabele" = variabele_gebied,
      "variabele_label" = variabele_label,
      "values" = variabele_values,
      "value_labels" = variabele_value_labels
      )
    
    
    
  })
  
  #Zijn alle labels er voor gebiedsindeling?
  gebiedsindeling_labels_compleet <- reactive({
    #Er mogen geen variabele / value labels ontbreken. 
    !any(is.na(df_gebiedsindeling_levels()$value_labels)) & !any(is.na(df_gebiedsindeling_levels()$variabele_label))
    
  })
  
  
  #Tabel renderen met overzicht gebiedsindeling
  output$overzicht_gebiedsindeling <- function(){
    validate(need(nchar(locatie_configuratiebestand()) > 0 & 
                    locatie_configuratiebestand() != "leeg"
                  ,""))
    #Tabel output
    df_gebiedsindeling_levels() %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(c("Gebiedsindeling" = 4))%>%
      scroll_box(height = "500px")
  }
 
  
  
  

}



