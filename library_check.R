# install (if required) and load libraries

if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if (!require("lubridate")) {install.packages("lubridate"); require("lubridate")}
if (!require("sf")) {install.packages("sf"); require("sf")}

if (!require("mapview")) {install.packages("mapview"); require("mapview")}
if (!require("viridisLite")) {install.packages("viridisLite"); require("viridisLite")}


if (!require("knitr")) {install.packages("knitr"); require("knitr")}
if (!require("kableExtra")) {install.packages("kableExtra"); require("kableExtra")}
if (!require("leafem")) {install.packages("leafem"); require("leafem")}
