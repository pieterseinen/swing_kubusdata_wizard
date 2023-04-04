#Algemene parameters
basismap_output <- "P:/Data voor Swing"
basismap_spss_bestanden <- "P:/SPSS data"
basismap_configuraties <- "P:/Configuraties"

nr_regio <- 2014
naam_regio <- "Gelderland-Zuid"

missing_voor_privacy <- -99996
minimum_obs_per_rij <- 2

max_char_labels <- 100

#Shiny dingen
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyFiles)
library(shinyjs)

#SPSS verwerken
library(haven)
library(labelled)
#Excelbestanden maken
library(openxlsx)

#overig
library(dplyr)
library(glue)
library(kableExtra)
library(stringr)
library(tidyr)

#Opties:
options(stringsAsFactors = F,
        shiny.maxRequestSize=200*1024^2 #Max filesize
        )

####Helperfuncties####

#Wissel pagina
switch_page <- function(page) {
  updateTabsetPanel(inputId = "swing_wizard", selected = page)
}


#Navigatieknoppen: Functie om knoppen vorige, home, volgende te maken
navigatieknoppen <- function(vorige,home,volgende, hide_volgende = T){
 div(style = "position: absolute; bottom: 0; left: 0; right: 0",  

  column(offset = 1,
         3,
         actionButton(inputId = vorige,
                      label ="Vorige",
                      icon = icon("fa-sharp fa-solid fa-arrow-left",
                                  verify_fa = F),
                      style = "height:100px; width: 100%; font-size: 200%;")
         ),
  column(3,
         actionButton(inputId = home,
                      label = "Home",
                      icon = icon("home"),
                      style = "height:100px; width: 100%; font-size: 200%;")
         ),
  column(3,

         actionButton(inputId = volgende,
                      label = div("Volgende",icon("fa-sharp fa-solid fa-arrow-right",
                                                  verify_fa = FALSE)),
                      style = "height:100px; width: 100%; font-size: 200%;")

  
  )
  )
}


#Kubusdata functie.
#van loops binnen de functie te meten.
maak_kubusdata <- function(data_totaal = NULL, jaren_voor_analyse = NULL, heeft_meer_jaar = FALSE, jaarvariabele = NULL, is_gewogen = FALSE,
                           weegfactor = NULL, gebiedsindeling = NULL,variabelen = NULL, crossings = NULL, geolevel = "gemeente",
                           min_observaties_antwoord = 0, bron = NULL, session = NULL, gekozen_map = NULL, alleen_data = F){
  
  #Alle variabelen die NIET gebruikt worden wegfilteren. Zou de snelheid van subsetten ten goede moeten komen
  assign("data_totaal",data_totaal, envir = .GlobalEnv)
  
  jaarvariabele_in_data <- NULL
  jaarvariabele_in_data <- if(heeft_meer_jaar){jaarvariabele}
  

  weegfactor_in_data <- NULL
  weegfactor_in_data <- if(is_gewogen){weegfactor}

  gebruikte_variabelen <- c(jaarvariabele_in_data,weegfactor_in_data, gebiedsindeling,variabelen,crossings)
  
  #jaren voor analyse komt als 1 str binnen, omzetten naar numeric vector 
  jaren_voor_analyse <- str_split(jaren_voor_analyse, ",") %>% unlist() %>% as.numeric()
  
  data_totaal <- data_totaal %>% dplyr::select(all_of(gebruikte_variabelen))
  
  #Variabele die increment om voortgang bij te houden
  #zie rij_per_combinatie_crossings
  huidige_variabele <- 0
  totaal_variabelen <- length(variabelen)
  
  #Variabele die te lange indicatornamen bijhoudt
  namen_te_lang <<- cbind("variabele" = NULL,"naam" = NULL)
  #Variabele die bijhoudt of er jaren voor analyse zijn opgegeven waarvoor er geen jaren in de data zitten (voor specifieke variabele) 
  ontbrekende_jaren <- NULL 
  #lege variabele: Is er een combinatie waarbij alleen maar missings waren
  missing_variabele  <- c()
  
  #Variabele die bijhoudt of kubusdata niet opgeslagen kon worden (omdat iemand het te overschrijven bestand in gebruik heeft)
  var_kubusdata_niet_opgeslagen <- c()
  
  #Opgegeven jaren voor analyse ophalen
  if(heeft_meer_jaar){
  jaren_voor_analyse <- str_split(jaren_voor_analyse,",") %>% unlist() %>% as.numeric()
  
  #Dan filteren op jaren die voor analyse zijn opgegeven
  data_totaal <- data_totaal[which(data_totaal[[jaarvariabele]]%in% jaren_voor_analyse),]
  }else{
    
    data_totaal[[jaarvariabele]] <- jaren_voor_analyse
    
  }
  
  #Met gewichten?
  if(is_gewogen){
    data_totaal$weegfactor <- data_totaal[[weegfactor]]
  }else{
    #Anders 'ongewogen' (door een betekenisloze weging te doen)
    data_totaal$weegfactor <- 1
  }

  #Voor alle variabelen
  lapply(variabelen, function(variabele){

    
    #Maak een kubus dataframe
    kubus_df <- data_totaal %>%
      filter(.[[jaarvariabele]] %in% jaren_voor_analyse,
             !is.na(.[[variabele]]),
             !is.na(.[[gebiedsindeling]]),
             across(.cols = all_of(crossings) ,
                    .fns = ~ !is.na(.x))) %>%
      mutate(n = 1,
             var = factor(.[[variabele]],
                          levels = val_labels(.[[variabele]]),
                          labels = names(val_labels(.[[variabele]]))))%>%
      group_by(.[[jaarvariabele]],.[[gebiedsindeling]], across(all_of(crossings)), var) %>%
      summarise(n_gewogen = sum(weegfactor),
                n_ongewogen = sum(n)) %>%
      ungroup()%>%
      pivot_wider(names_from = var, values_from = n_gewogen) %>%
      replace(is.na(.),0)
    
    #Als je een variabele opgeeft die (icm crossings / jaren) alleen maar missing kent crasht de boel.
    #Je kan immers geen kruistabel maken met niks.
    #Als dit het geval is willen we de kubusmaakfunctie vroegtijdig afbreken en een melding geven
    #Als kubus_df geen rijen telt; zijn er alleen maar missings op de combinatie van
    #jaar: jaren_voor_analyse,gebiedsindeling, crossings, var
    
    if(nrow(kubus_df) == 0){
      
      missing_variabele <<- c(missing_variabele,variabele)

      #Skip deze variabele
      return()

    }

    #Volgorde van kolomtoewijzing is ongeregelmatig tussen configuraties. (configuratie met meerdere jaren
    #doen iets anders dan configuratie met 1 jaar). Mogelijke veroorzaakt door pivot met 1 vs meer jaren.
    #oplossing:  kolomvolgorde forceren
    volgorde_labels <- names(val_labels(data_totaal[[variabele]]))
    
    #De volgorde waarop de labels nu zijn ingedeeld in df
    volgorde_labels_in_df <- lapply(volgorde_labels, function(x){
    
      which(names(kubus_df) == x)
      
    }) %>% unlist()
    
    #Bedoelde indeling
    bedoelde_kolomindexen <- c(1:(min(volgorde_labels_in_df)-1), volgorde_labels_in_df)
    #toepassen op df
    kubus_df <- kubus_df[,bedoelde_kolomindexen]
    
    #Kolomnamen toewijzen
    namen_variabel_kolommen <- glue("{variabele}_{val_labels(data_totaal[[variabele]])}")
    names(kubus_df) <- c(jaarvariabele,gebiedsindeling,crossings,'n_ongewogen',namen_variabel_kolommen)
    
    #Functie om te kleine aantallen te verwijderen
    verwijder_kleine_aantallen <- function(x, ongewogen){if(ongewogen < minimum_obs_per_rij){missing_voor_privacy}else{x}}
    
    #Na pivot opnieuw groeperen en summarizen, daarna te lage aantallen weghalen
    kubus_df <- kubus_df %>%
      mutate(Niveaucode = geolevel)%>%
      group_by(.[[jaarvariabele]],
               Niveaucode,
               .[[gebiedsindeling]],
               across(all_of(crossings)))%>%
      summarise_at(.vars = c(namen_variabel_kolommen, "n_ongewogen"),
                   .funs = sum) %>%
      #Verwijder te lage aantallen
      mutate(across(.cols = c(all_of(namen_variabel_kolommen),"n_ongewogen") ,
                    .fns = Vectorize(verwijder_kleine_aantallen), ongewogen = n_ongewogen))%>%
      ungroup()


    if(geolevel == "ggd"){
      naam_geolevel <- "Regiocode"
    }else{
      naam_geolevel <- "Gemeentecode"
      }
    
    names(kubus_df)[1] <- jaarvariabele
    names(kubus_df)[3] <- naam_geolevel
    names(kubus_df)[length(names(kubus_df))] <- glue("{variabele}_ONG")

    totaal_rijen <<- nrow(kubus_df)
    lege_rijen <<- nrow(kubus_df[kubus_df[[length(kubus_df)]] == missing_voor_privacy,])
    
    #We willen een warning geven wanneer er jaren voor analyse zijn opgegeven die niet in de data terugkomen.
    #Welke jaren zijn opgeslagen
    jaren_in_kubusdata <- unique(kubus_df[[1]])
    
    #Welke jaren zijn opgegeven, maar komen niet voor in df?
    jaren_niet_in_kubusdata <- jaren_voor_analyse[!jaren_voor_analyse %in% jaren_in_kubusdata] %>% str_c(collapse = ",")
    
    #Als input = is_meer_jaar en jaren_voor_analyse bevat jaren die niet in kubus df zitten: vul melding aan
    if(heeft_meer_jaar & nchar(jaren_niet_in_kubusdata) > 0 ){
      
      ontbrekende_jaren <<- c(ontbrekende_jaren, glue("<strong>{variabele}:</strong> {str_c(jaren_niet_in_kubusdata,collapse = ',')}")) 
    }
    
    #Variabel labels vastleggen
    variabel_labels <- val_labels(data_totaal[[variabele]])
    
    #Bij dichotome variabelen willen we dat het label van de variabele (vraag) weergeven wordt,
    #Als variabelen niet dichotoom zijn willen we een label voor elk antwoord
    
    is_dichotoom <- all(unname(val_labels(data_totaal[[variabele]])) %in% c(0,1))
    
    if(is_dichotoom){
      variabel_namen <- var_label(data_totaal[[variabele]])
    }
    

    n_labels <- length(variabel_labels)
    
    #Excelbestand maken
    workbook <- createWorkbook()
    
    #Data toevoegen aan WB
    addWorksheet(workbook, sheetName = "Data")
    writeData(workbook,"Data",kubus_df)
    

    #Definities data toevoegen aan WB
    addWorksheet(workbook, sheetName = "Data_def")
    
    writeData(workbook, "Data_def",
              
              cbind("col" = c(jaarvariabele,
                              "Niveaucode",
                              if_else(geolevel == "ggd","Regiocode","Gemeentecode"),
                              #Alle crossings
                              crossings,
                              #Alle variabel-levels als kolommen
                              unlist(lapply(unname(variabel_labels), function(x){glue("{variabele}_{x}")})),
                              #Ongewogen kolom
                              glue("{variabele}_ONG")),
              "type" = c("period","geolevel","geoitem",
                         #Crossings zijn dimensies
                         rep("dim", length(crossings)),
                         #Variabel_levels (en de _ONG kolom) zijn variabelen
                         rep("var", n_labels + 1)))
              )
    
    if(!alleen_data){
    #Variabel_labels toevoegen aan WB
    addWorksheet(workbook, sheetName = "Label_var")
    writeData(workbook,"Label_var",
              
              cbind(
                "Onderwerpcode" = c(#Alle variabel-levels als kolommen
                  unlist(lapply(unname(variabel_labels), function(x){glue("{variabele}_{x}")})),
                  #Ongewogen kolom
                  glue("{variabele}_ONG")),
                
                "Naam" = c(unlist(lapply(names(variabel_labels), function(x){glue({"Aantal {x}"})})), 
                           "Totaal aantal ongewogen"),
                "Eenheid" = rep("Personen",n_labels,+1))
              )
    
    
    #Voor dimensies hebben we een lijst met namen crossings nodig. 
    #Als variabel_label NULL is wordt de naam "".
    namen_crossings <-  unlist(lapply(crossings,function(x){
      #Naam van een crossing is het var_label
      var_label(data_totaal[[x]])
      
    }))
    
    #Info over Dimensies toevoegen
    addWorksheet(workbook, sheetName = "Dimensies")
    writeData(workbook,"Dimensies",
              
              cbind("Dimensiecode" = c(crossings),
                    "Naam" = namen_crossings))
    

    #Sheets voor de de dimensies;
    #met namen en volgnummers voor de levels van alle crossingvariabelen
    lapply(crossings, function(crossing){
      
      crossing_labels <- val_labels(data_totaal[[crossing]])
      
      #Te lange crossing labels afknippen & opslaan in lijst met te lang
      for(label_index in 1:length(crossing_labels)){
        
        naam_label <- names(crossing_labels[label_index])
        
        if(nchar(naam_label) > max_char_labels){
          #Lijst met te lange namen aanvullen 
          if(!crossing %in% unique(namen_te_lang[,1])){
            namen_te_lang <<- rbind(namen_te_lang,
                                   cbind("variabele" = crossing,
                                         "naam" = naam_label))
          }
          
          names(crossing_labels)[label_index] <- substr(names(crossing_labels[label_index]),1,max_char_labels)
          }
  
      }  
      #Per crossing / dimensie een sheet toevoegen        
      addWorksheet(workbook, sheetName = crossing)
      writeData(workbook, crossing, 
                
                cbind("Itemcode" =  c(unname(crossing_labels)),
                      "Naam" = names(crossing_labels),
                      "Volgnr" = seq(1:length(crossing_labels))
                )
                )
      
    })
    
    #Tabblad Indicators; 
    addWorksheet(workbook, sheetName = "Indicators")
    
    writeData(workbook, "Indicators",
  
              cbind("Indicator code" = 
                      #Alle variabel-levels
                      c(
                        glue("{variabele}_{variabel_labels}"),
                        #ongewogen totaal
                        glue("{variabele}_ONG"),
                        #gewogen totaal
                        glue("{variabele}_GEW"),
                        #Percentage_per level tenzij het om een dichotome variabele gaat
                        if(is_dichotoom){
                          glue("{variabele}_perc")
                        }else{
                          glue("{variabele}_{variabel_labels}_perc")}
                      ),
                    
                    
                    "Name" = 
                      #Alle variabel-levels
                      c(unlist(lapply(names(variabel_labels), function(x){glue({"Aantal {x}"})})),
                        "Totaal aantal ongewogen",
                        "Totaal aantal gewogen",
                        
                        #1 percentage rij als het om dichotoom gaat
                        if(is_dichotoom){
                          
                          #Namen die langer zijn dan 99-tekens worden niet geaccepteerd door Swing
                          #Substr om 1e 99 tekens mee te nemen & zorgen dat afgeknipte variabelnamen worden bijgehoudeh
                          if(nchar(variabel_namen) > max_char_labels){
                            namen_te_lang <<- rbind(namen_te_lang, 
                                                    cbind("variabele" = variabele,
                                                          "naam" = variabel_namen))
                          }
                           
                          substr(variabel_namen,1,max_char_labels)
                          
                          
                          }else{
                          #Percentage rij voor Alle niveaus  als het om niet-dichotoom gaat
                          unlist(lapply(names(variabel_labels), function(x){
                            
                            #Indicator opbouwen uit; Var-label + val_label
                            var_label <- var_label(data_totaal[[variabele]])
                            
                            naam_indicator <- glue("{var_label}, {x}") 
                            
                            #Als de automatische naam indicator te lang is voor Swing: Opknippen & vastleggen
                            if(nchar(naam_indicator) > max_char_labels){
                              
                              #Indicator opslaan voor melding na uitvoeren wizard
                              namen_te_lang <<- rbind(namen_te_lang,
                                                       cbind("variabele" = variabele,
                                                             "naam" = naam_indicator))
                              #Label inkorten door beide onderdelen in te korten
                              var_label_kort <- substr(var_label,1,(max_char_labels/2)-2)
                              val_label_kort <- substr(x,1,max_char_labels/2)
                              
                              naam_indicator <- glue("{var_label_kort}, {val_label_kort}")
                            }
                            
                            
                            naam_indicator
                            
                            }))
                        }),
                    #Percentage of personen
                    #aantal rijen voor personen = levels variabele + 2(gewogen/ongewogen)
                    "Unit" = c(rep("personen",n_labels+2),
                               #aantal rijen voor percentages = levels variabele
                               if(is_dichotoom){
                                 "percentage"
                               }else{
                                 rep("percentage",n_labels)}),
                    
                    "Aggregation indicator" = 
                      #Aggregation indicator is alleen voor percentages relevant; het gewogen totaal
                      c(rep("",n_labels+2),
                        
                        if(is_dichotoom){
                          glue("{variabele}_GEW")
                        }else{
                          #aantal rijen voor percentages = levels variabele
                          rep(glue("{variabele}_GEW"),n_labels)
                        }),
                    
                    
                    "Formula" = c(
                      #Er zijn twee soorten formules in het indicator tabblad; 
                      #de berekening van de gewogen totalen obv de gewogen aantallen per level.
                      #de berekening van het percentage per level obv de gewogen aantallen
                      
                      #Lege rijen voor de rij van gewogen totaal = var_levels + 1
                      rep("",n_labels+1),
                      
                      #Totaal gewogen = Alle variabel lvls bij elkaar opgeteld
                      str_c(glue("{variabele}_{variabel_labels}"), collapse = "+"),
                      
                      #ALs variabele dichtoom is hebben we maar 1 percentage
                      if(is_dichotoom){
                        glue("({variabele}_1/({str_c(glue('{variabele}_{variabel_labels}'), collapse = '+')}))*100 ")
                      }else{
                        #Vector met alle indicator codes
                        indicator_codes <- unlist(lapply(unname(variabel_labels),function(x){glue('{variabele}_{x}')}))
                        #Nu percentages voor alle indicator codes -> indicator code / alle_levels * 100
                        
                        #Formule om in Swing een percentage uit te rekenen.
                        #Glue voor iedere indicator code een string met de structuur: 
                        #({huidige_indicator})/({indicator_1 + indicator_2 + ...) * 100}
                        
                        glue("({indicator_codes}/({str_c(glue('{variabele}_{variabel_labels}'), collapse = '+')}))*100 ")
                      }),
                    #Zelfde logica als "Unit"
                    "Data type" = c(rep("Numeric",n_labels+2),
                                    #aantal rijen voor percentages = levels variabele
                                    if(is_dichotoom){
                                      "Percentage"
                                    }else{
                                      rep("Percentage",n_labels)}),
                    #Alleen de percentages moeten visibile zijn
                    "Visible" = c(rep(0,n_labels+2),
                                  #aantal rijen voor percentages = levels variabele
                                  if(is_dichotoom){
                                    1
                                  }else{
                                    rep(1,n_labels)}),
                    #Treshold opgeven voor percentages. Als een selectie van
                    #dimensies/crossings tot een groepsindeling leidt met minder observaties treshold 
                    #Wordt dit afgeschermd in de mozaieken / viewer van Swing
                    "Threshold value" = c(rep("",n_labels+2),
                                          #aantal rijen voor percentages = levels variabele
                                          if(is_dichotoom){
                                            min_observaties_antwoord
                                          }else{
                                            rep(min_observaties_antwoord,n_labels)}),
                    #Cube is overal 1
                    "Cube" = c(rep(1,n_labels+2),
                               if(is_dichotoom){
                                 1
                               }else{
                                 rep(1,n_labels)}),
                    #Source is overal hetzelfde
                    "Source" = c(rep(bron,n_labels+2),
                                 if(is_dichotoom){
                                   bron
                                 }else{
                                   rep(bron,n_labels)})
                    
              ))
    }
    #SaveWorkbook in een trycatch zodat applicatie niet crasht bij mislukken van opslaan.
    #variabelnaam opslaan voor een foutbericht na het uitvoeren van een configuratie
    tryCatch({
      
      saveWorkbook(workbook, file = glue("{gekozen_map}/kubus_{variabele}.xlsx"), overwrite = TRUE)
      
    },
    error = function(cond){
      message(glue("Er ging iets mis met het excelbestand maken van kubus_{variabele}.xlsx"))
    },
    warning = function(cond){
      message(cond)
      
      message(glue("\n Bestand kon niet overschreven worden, mogelijk is het bestand nog geopend"))
      
      var_kubusdata_niet_opgeslagen <<- c(var_kubusdata_niet_opgeslagen, glue("{variabele}"))
    })


    #Voortgang bijhouden voor progressbar
    huidige_variabele <<- huidige_variabele + 1
    
    if(shiny::isRunning()){
      updateProgressBar(session = session, "voortgang_rij", huidige_variabele/totaal_variabelen*100)
    }
    
  })

  #Modal sluiten & nieuwe modal sturen met alle waarschuwingen/foutmeldingen over uitvoeren
  #van configuratie
  removeModal()
  
  #HTML meldingen maken
  melding_alleen_maar_missing <- ""
  if(!is.null(missing_variabele)){
    
    melding_alleen_maar_missing <- glue("
      
        <strong style= 'color:red'> FOUT! </strong>
        <p>Er zijn combinaties van variabelen opgegeven die alleen maar missings opleveren.
        Hier is geen kubusdata voor gemaakt. Controleer de configuratie en/of het SPSS databestand.</p>
        
        <strong> Deze inhoudelijke variabelen zijn overgeslagen: </strong>
        <ul><li>{str_c(missing_variabele, collapse = '</li><li>')}</li></ul>
        
        <strong> Omdat ze alleen maar missings opleverde i.c.m deze variabelen: </strong>
        <ul><li>{gebiedsindeling}</li> 
            <li>{jaarvariabele} (waarden: {str_c(jaren_voor_analyse,collapse = ',')})</li>
            <li>{str_c(crossings,collapse = '</li><li>')}</li>
            </ul>
                 
        <p> Controleer of de dataset compleet is en de codering van de gebruikte variabelen klopt.</p>")
  }
  
  melding_niet_opgeslagen <- ""
  
  if(length(var_kubusdata_niet_opgeslagen) > 0){
    melding_niet_opgeslagen <-  glue("
      
      <strong style= 'color:red'> FOUT! </strong>
      <p>De kubusdata van de volgende variabelen kon niet opgeslagen worden in de gekozen map <strong>{gekozen_map}</strong>:<p>
      <ul><li>{str_c(var_kubusdata_niet_opgeslagen, collapse = '</li> <li>')}</li></ul>
      
      <p>Controleer of de bijbehorende bestanden geopend zijn en sluit deze.
      Bestanden die in gebruik zijn kunnen niet overschreven worden.</p>
       ")
    
  } 
  
  melding_ontbrekende_jaren <- "" 
  if(!is.null(ontbrekende_jaren)){
    
    melding_ontbrekende_jaren <- glue("
      <strong style= 'color:red'> LET OP! </strong> 
      
      <p>Bij een aantal variabelen is er geen data gevonden voor de opgegeven jaren 
      ({str_c(jaren_voor_analyse, collapse = ',')}): </p>
      <ul><li>{str_c(ontbrekende_jaren, collapse = '</li> <li>')}</li></ul></p>")
  }
  
  melding_lege_groepen <- ""
  if(lege_rijen > 0){
    print(1)
    melding_lege_groepen <- glue(
      "<strong style= 'color:red'>LET OP!</strong>
      <p>Voor <strong>{lege_rijen}</strong> uit de <strong>{totaal_rijen}</strong> rijen zijn er te weinig of geen observaties gevonden.
      Groepsindelingen met te weinig observaties worden als missend weergeven. Dit kan betekenen dat percentages
      van die groepen niet kloppen.</p>
      
      </p>Het minimum aantal observaties per groep is ingesteld op: <strong>{minimum_obs_per_rij}</strong>
      Controleer de instellingen van je kruisvariabelen en probeer te kleine groepen te voorkomen.</p>"
    )
  }
  
  melding_lange_namen <- ""
  
  if(length(namen_te_lang) > 0){
    melding_lange_namen <- glue("
      <strong style= 'color:red'>LET OP!</strong>
      <p>De labels van de volgende variabelen hadden meer dan 100 tekens en zijn daarom afgeknipt:</p>
      <ul><li>{str_c(namen_te_lang[,1], collapse = '</li><li>')}</li></ul>
      
      <p>Maak kortere labels om dit te voorkomen</p>")
  }
  
  
  
  showModal(
    modalDialog(
      HTML(
        glue("<h3><strong> KLAAR! </h3></strong> <br>
                <p><strong> Lees onderstaande meldingen over de uitvoering van de configuratie.</strong></p>
               {melding_alleen_maar_missing}
               {melding_niet_opgeslagen}
               {melding_ontbrekende_jaren}
               {melding_lege_groepen}
               {melding_lange_namen}
                          
               <strong> Kubusbestanden zijn opgeslagen voor de volgende variabelen:</strong>
               <ul><li>{str_c(variabelen[!variabelen %in% c(missing_variabele,var_kubusdata_niet_opgeslagen)],collapse = '</li><li>')}</li></ul>
               <p>Kubusdata is te vinden in de map <strong>{gekozen_map}</strong> </p>"))))


  
}
