# RealLifeRisk
Een op R gebaseerde versie van het turn-based strategie spel Real Life Risk.

###############################################################################
# INSTALLATIE
###############################################################################

Op dit moment is het nog niet mogelijk om RealLifeRisk in één keer te installeren op een nieuw systeem. De huidige vorm van de code draait op R. Voor onervaren gebruikers wordt aangeraden om daarnaast RStudio te gebruiken. Dit is een programma 'rondom' R dat de taal wat makkelijker maakt in het gebruik. Tot slot is het nodig om (éénmalig) de juiste 'packages' te installeren in R.

Stappenplan:
1) installeer R (https://cran.r-project.org/bin/windows/base/)
2) installeer RStudio (https://www.rstudio.com/products/rstudio/download/)
3) open RStudio, en installeer éénmalig de benodigde packages als volgt:
    install.packages('shiny');
    install.packages('shinydashboard');
    install.packages('png');
    install.packages('DT');
4) open RealLifeRisk.proj
    als het goed is opent RStudio nu ui.r, app.r, server.r en global.r
    zo niet, klik file->open en open app.r
5) klik nu 'run app' om RealLifeRisk te openen. Done!

###############################################################################
# @author: S.V. den Boer | Aug 27, 2018
###############################################################################