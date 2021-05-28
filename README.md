# 2D3D
2D3D registration

# Binder
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/bas-1994/2D3D/master)

IMPORTANT --> people following the course R reproducibility that are checking my code.
Only review the parts #Packages #Data exploration and #multivariate analysis!!!! Multivariate analysis is NOT finished, still working on it. Have to do all these assumptions. If you have experience with this. You could be of great help to me. Currently fighting this statistical war all alone.
The other parts I might delete in the future or recode.
It is work in progress so nothing is definitive yet.

# Open and/or update the project correctly in Github
hoe het project updaten in die console:
pwd # waar ben je?
cd # zetten naar juiste map
git init # zo dat git weet dat ie moet opletten
git add . # dat moet je toevoegen #-A betekens: all. Kan ook
git commit -m "update voor jezelluf" #ff updaten waarom wanneer enzz #als ie om -d quote vraagt, gewoon die quote invullen met ""
git remote add origin git@github.com:bas-1994/2D3D.git #goeie repository kiezen
git push -u origin master # pushen, -f kan erbij tussen -u en master als er shit overwrite moet worden

hoe het project uit github halen
alles herhalen hierboven tot git init
git pull #pulll

# Which program?
R version 4.0.3 (2020-10-10)

# What packages are necessary (this is addressed in the R code as well)
#install these
install.packages("readxl")
install.packages("tidyverse")
install.packages("tidyselect")
install.packages("tableone")
install.packages("olsrr")
