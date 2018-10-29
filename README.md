# RealLifeRiskR
Een op R gebaseerde versie van het turn-based strategie spel Real Life Risk.

###############################################################################
# SPEELINSTRUCTIES
###############################################################################

RealLifeRisk wordt niet op een computer gespeeld, maar op een fysiek speelbord.
RealLifeRiskR is daarom ook slechts een hulpmiddel. Daarom is er voor gekozen om
het gebruik van de software zo simpel mogelijk te maken voor spelers en leiding.

De kern van RealLifeRiskR is de ActieInvoerValidatieDienst, AIVD. 
Dit is de manier waarop je acties doorgeeft aan de computer. RealLifeRiskR 
interpreteert AIVD commando's, rekent deze door en rapporteert terug. Een
AIVD bericht bestaat uit vier componenenten:

> wie.wat.waar.waarheen  
> - wie: een "Player" uit maak_spelers.csv  
> - wat: een "Unit" uit maak_units.csv + de hoeveelheid  
> - waar: een valide coördinaat  
> - waarheen: een valide route (in N/E/S/W of U/R/D/L)  

Voorbeelden:  
H.T1.A13    = Herten kopen 1 tank op A13    
S.P4.J1.UUR = Sperwers verplaatsen 4 peletons van J1 omhoog.omhoog.rechts  
V.B.E9      = Vossen voeren bombardement uit op E9  

Spelverloop:
Het doel van RealLifeRiskR is om zo veel mogelijk land en steden te bezetten en je tegenstanders te verslaan.
Alle spelers/teams spelen op dezelfde kaart, maar hebben alleen overzicht over hun eigen gebieden. De spelleider/spelleiding 
coördineert het spel en is de enige speler met volledig overzicht. 
Aan de start van het spel krijgt elke speler een kaart met daarop hun startposities en units. 
Tijdens elke beurt kan elke speler maximaal drie zetten doorgeven. Er bestaan drie typen acties:

- Aankoop units
- Verplaatsing units
- Uitvoeren speciale actie

Deze acties kunnen via papier worden doorgegeven aan de spelleiding, of via een tablet/computer. 
Aan het eind van elke beurt wordt berekend wat het resultaat van de acties is. De speler ontvangt
een rapport met de uitkomst van zijn eigen acties en overige relevante informatie zoals verloren units of verkregen goud.
De spelleiding houdt het spel continu in de gaten en kan waar nodig bijsturen om het spelplezier te verhogen.
Het totale spel bestaat uit een serie van beurten. De spelleiding kan van tevoren wincondities bepalen t.a.v.
landbezit, stedenbezit, score of bestaande units/tegenstanders.

###############################################################################
# INSTALLATIE
###############################################################################

RealLifeRiskR maakt gebruik van R, een simpele programmeertaal, en een webbrowser. 
Van beide zijn standalone versies beschikbaar, die je gewoon vanaf een USB stick kan draaien. 
Om RealLifeRisk te spelen moet je een map maken met daarin de volgende bestanden:

> RealLifeRisk/  
> - GoogleChromePortable/  
> - R-Portable/  
> - shiny/  
> - RealLifeRisk.vbs  

Je start het spel met RealLifeRisk.vbs. De makkelijkste manier om dit te krijgen 
is om iemand te zoeken die alle bestanden al op een USB stick heeft staan. 
De op één na makkelijkste manier is om zelf een map te maken:

1. Maak een map 'RealLifeRisk/' aan
2. Download RealLifeRisk.vbs en de 'shiny/' map van [GitHub]{https://github.com/Zaklamp02/RealLifeRisk}
3. Download [GoogleChromePortable]{https://portableapps.com/apps/internet/google_chrome_portable} en installeer in 'RealLifeRisk/'
4. Download [R-Portable]{https://sourceforge.net/projects/rportable/} en installeer in 'RealLifeRisk/'
5. Open R-Portable.exe en typ acter elkaar de volgende commando's:
   - install.packages('shiny')
   - install.packages('shinydashboard')
   - install.packages('shinywidgets')
   - install.packages('shinyjs')
   - install.packages('png')
   - install.packages('DT')
   - install.packages('magick')
   - install.packages('readxl')
   - install.packages('raster')
   - install.packages('rgeos')

###############################################################################
# AANPASSINGEN MAKEN
###############################################################################

RealLifeRiskR is geschreven in R, een vrij simpele en intuïtieve taal. Dus in principe kan iedereen het spel aanpassen. Maar om het de gebruiker makkelijk te maken
zijn een aantal opties in een excel bestand 'Settings.xls' gevat. Daarmee kun je gemakkelijk het volgende aanpassen:

1. Speelbord
   - Steden aanpassen
   - Water aanpassen
   - Startlocatie aanpassen
   - Namen van de X en Y as labels aanpassen
2. Units
   - Units aanpassen
   - Units toevoegen/verwijderen
3. Spelers
   - Spelers aanpassen
   - Spelers toevoegen/verwijderen
4. Settings
   - Game settings 
   - Scherm settings

Elke tab in 'Settings.xls' heeft een instructieveld hoe settings aangepast kunnen worden.
   
###############################################################################
# @author: S.V. den Boer | Aug 27, 2018
###############################################################################