# swing_kubusdata_wizard
Shiny applicatie om monitordata uit SPSS om te zetten naar kubusdata voor Swing. Bedoeld om lokaal uit te voeren.

Benodigdheden
- R & Rstudio
- Een SPSS .sav-bestand met monitordata

Voordat je begint moet het volgende in orde zijn:

• Zorg ervoor dat alle benodigde libraries geïnstalleerd zijn en er een recente versie van R 
is geïnstalleerd.

• Alle gebruikte variabelen moeten volledige variabel-labels & value-labels hebben.
Het script werkt niet wanneer er variabelen / waarden zonder label zijn.

• Alle gebruikte variabelen moeten beschrijvende variabel-labels & value-labels hebben.
Deze labels worden gebruikt voor de naamgeving in Swing. Als de labels kloppen, weet 
je wat je geüpload hebt en hoef je achteraf niet handmatig alle namen aan te passen.

• Swing kan indicatornamen aan die maximaal 100 tekens hebben. Als een indicator een 
langere naam zou krijgen wordt deze naam bij het 100e teken afgeknipt door de Wizard.
Als dit is gebeurd zal dat gemeld worden in de popup die aangeeft dat de Wizard klaar 
is.

Automatisch worden namen van indicatoren als volgt toegewezen:
- Dichotoom: {variabel_label}
- Niet dichotoom: {variabel_label} + ‘,’ {value_label}

• Om te zorgen dat een variabele als dichotoom/binair herkend wordt, moeten ze met de 
waarden 0/1 gecodeerd zijn. Het script maakt bij een dichotome variabele slechts één 
indicator aan die het percentage positieven (1) geeft. De naam van de indicator is het 
variabel-label (i.p.v. normaal gesproken het value label)

 o Voorbeeld dichotoom:
 - Variabellabel: Heeft iets vervelends meegemaakt
 - Waarden: 1 = Ja, 0 = Nee
 - Indicator in Swing: Percentage leerlingen dat iets vervelend heeft meegemaakt.

  o Voorbeeld niet-dichotoom:
  - Varriabellabel: Hoe gelukkig ben je?
  - Waarden: 1= Ongelukkig, 2 =Tussen ongelukkig en gelukkig, 3 = Gelukkig
  - Indicatoren in Swing: Percentage Ongelukkig, Percentage Tussen ongelukkig en 
  gelukkig, Percentage gelukkig

 o Voorbeeld dichotoom fout gecodeerd:
   - Variabellabel: Heeft iets vervelends meegemaakt
   - Waarden: 1 = Ja, 2 = Nee
   - Indicatoren in Swing: Percentage Ja, Percentage Nee

• Zorg ervoor dat variabelen alleen dezelfde variabelnaam(wordt de ‘variabelcode’ in 
swing) hebben, wanneer deze dezelfde betekenis hebben. Swing ziet alle variabelen 
met dezelfde code als hetzelfde. Bij dezelfde code zal de oude data met nieuwe data 
aangevuld / overschreven worden.

  o Voorbeeld waarbij het misgaat:

Er is een kubusbestand met de kruisvariabele geslacht geüpload uit de kindermonitor. Deze variabele kent twee waarden: Jongens & Meisjes.
Op een later moment wordt nog een kubusbestand met de kruisvariabele geslacht geüpload uit de volwassen-ouderenmonitor. 
Deze variabele kent ook twee waarden: Mannen en Vrouwen. Er kunnen nu twee dingen fout gaan bij het overschrijven data van een bestaande variabelnaam.
		
   - Er is overlap in de numerieke waarden achter de valuelabels. De labels Jongens en/of Meisje worden overschreven door Mannen en/of Vrouwen.
   - Er is geen overlap in de numerieke waarden achter de valuelabels. Volgens swing zijn er 4 nu geslachten in de variabele geslacht vastgelegd: Jongens, 
    Meisjes, Mannen en Vrouwen.
   - Indicatoren in Swing: Percentage Ja, Percentage Nee

 Mogelijke oplossing: Naamconventies. Bijvoorbeeld per monitor; ‘km_geslacht’ en ‘vo_geslacht’.

Wizard Configureren
Bovenaan global.R worden een aantal instellingen bepaald die specifiek zullen zijn aan de regio/gebruiker. Pas dit aan t/m R12.

   
Wizard gebruiken

1. Kopieer een .sav-bestand naar de map "P:/SPSS data" (of wat er in global.R is ingesteld bij **basismap_spss_bestanden**)

2. Open global.R in Rstudio

3. Opstarten
  a. Eerste keer: Klik op het drop-down pijltje naast Run App en selecteer “Run external”
  b. Klik op het op het groene pijltje
  4. Er wordt nu een venster geopend in je standaardbrowser. Hier kan je kiezen voor Maak een 
     configuratie en lees een bestaande configuratie

5. Een configuratie maken
  a. Klik op Maak een configuratie
  b. Klik bij Lees een SPSS .sav bestand in op Browse en selecteer een SPSS .sav. Het laden van de dataset kan enkele seconden duren.
  c. Doorloop vervolgens alle stappen. Lees instructies en maak selecties. 
  d. Wanneer alles is ingevoerd klik je op Configuratie opslaan. 

6. Een configuratie lezen
  a. Klik op Lees een bestaande configuratie
  b. Klik bij Selecteer een configuratiebestand op Browse en selecteer een .xlsx bestand. Het laden van de configuratie & bijbehorende dataset kan enkele seconden duren.
    i. (Optioneel) Kies Configuratie bewerken om naar 'Maak een configuratie' te gaan en de huidige instellingen te wijzigen.

7. Een configuratie uitvoeren (na stap 5 of 6)
  a. Klik op Selecteer een map voor kubusdata en kies een map waar de output opgeslagen 
  moet worden.
  b. Kies Maak een Swing kubusbestand om de configuratie uit te voeren
  c. Wanneer de configuratie is uitgevoerd verschijnt een popup met meldingen over
  het verwerken van de configuratie. Lees dit goed.
  d. De kubusdata is nu te vinden in de gekozen map onder P:\0. Beveiligd\27. Swing\Data 
  voor Swing als .xlsx bestanden voor elke variabele. Controleer de resultaten.
