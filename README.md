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
> wie: een "Player" uit maak_spelers.csv  
> wat: een "Unit" uit maak_units.csv + de hoeveelheid  
> waar: een valide coördinaat  
> waarheen: een valide route (in N/E/S/W of U/R/D/L)  

Voorbeelden: 
H.T1.A13    = Herten kopen 1 tank op A13  
S.P4.J1.UUR = Sperwers verplaatsen 4 peletons van J1 omhoog.omhoog.rechts  
V.B.E9      = Vossen voeren bombardement uit op E9  

###############################################################################
# INSTALLATIE
###############################################################################

RealLifeRiskR maakt gebruik van R, een simpele programmeertaal, en een webbrowser. 
Van beide zijn standalone versies beschikbaar, die je gewoon vanaf een USB stick kan draaien. 
Om RealLifeRisk te spelen moet je een map maken met daarin de volgende bestanden:

> RealLifeRisk/  
>   GoogleChromePortable/  
>   R-Portable/  
>   shiny/  
>   RealLifeRisk.vbs  

Je start het spel met RealLifeRisk.vbs. De makkelijkste manier om dit te krijgen 
is om iemand te zoeken die alle bestanden al op een USB stick heeft staan. 
De op één na makkelijkste manier is om zelf een map te maken:

1. Maak een map 'RealLifeRisk/' aan
2. Download RealLifeRisk.vbs en de 'shiny/' map van [GitHub]{www.github.com}
3. Download [GoogleChromePortable]{www.google.com} en installeer in 'RealLifeRisk/'
4. Download [R-Portable]{www.r.com} en installeer in 'RealLifeRisk/'
5. Open R-Portable.exe en typ acter elkaar de volgende commando's:
   - install.packages('shiny')
   - install.packages('shinydashboard')
   - install.packages('shinywidgets')
   - install.packages('shinyjs')
   - install.packages('png')
   - install.packages('DT')
   - install.packages('magick')

###############################################################################
# @author: S.V. den Boer | Aug 27, 2018
###############################################################################