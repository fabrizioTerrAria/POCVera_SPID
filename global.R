library(jsonlite)
library(uuid)

userUUID <- UUIDgenerate()

SCENARIO_DEFINITION_CHOICES <- c("Modifica i dati sul grafo stradale", "Definisci una politica urbana", "Applica riduzioni emissive")
SCENARIO_TIPO_POLITICA_CHOICES <- c("Converti il parco attuale", "Modifica il parco attuale")

DELTA_ABS_CHOICHES <- c("Assoluti","Delta", "Delta %")
EMI_DENS_CHOICHES <- c("Totali [ton]", "Densità [ton/km\u00b2]")
SPATIAL_AGGREGATION_CHOICES <- c("Comuni","Griglia")
SPATIAL_AGGREGATION_SHP <- c("Comuni")
SPATIAL_AGGREGATION_FIELD <- c("FK_ISTAT_COMUNE","COD_PRO","COD_REG")

POLLUTANT <- c("NO2","CH4","NH3","FC","SO2","CO2","PM10","PM2.5","COV")

PRECURSORS <- c("NO2","COV","NH3","PM10","PM2.5","SO2")
PRECURSORS_DESC <- c("NO\u2082","COV","NH\u2083","PM10","PM2.5","SO\u2082")

SECTORS <- c("Automobili", "Comm. Leggeri", "Comm. Pesanti", "Ciclomotori", "Motocicli", "Autobus")
SECTORS_ID <- c(55,56,57,58,59,85)

COMB <- c("Diesel", "Metano", "GPL", "Benzina", "Elettricità")
COMB_ID <- c(25,46,48,10001,10099)

ROAD_TYPE <- c("Autostrade", "Statali", "Statali", "Provinciali", "Provinciali", "Comunali", "Comunali", "Tangenziali", "Svincoli", "Urbane")
ROAD_TYPE_ID <- as.character(0:9)

SR_POLLUTANT_CHOICES <- c("NO\u2082", "PM10", "PM2.5")
SR_POLLUTANT <- c("NO2", "PM10", "PM2.5")
SR_UNIT <- c("[\u00b5g/m\u00b3]","[\u00b5g/m\u00b3]","[\u00b5g/m\u00b3]")

CLASSE_CLIMA_CHOICES <- c("< Tutte le tipologie >","0 ÷ 150m","150m ÷ 300m","300 ÷ 500m","500m - 1000m","1000m-2000m")
CLASSE_CLIMA_ID <- c("All","1","2","3","4","5")
CLASSE_RESIDENTI_CHOICES <- c("< Tutte le tipologie >","< 10'000 abt","10'000 ÷ 20'000 abt","20'000 ÷ 30'000 abt","30'000 ÷ 50'000 abt","> 50'000 abt")
CLASSE_RESIDENTI_ID <- c("All","1","2","3","4","5")
ZONE_CHOICES <- c("< Tutte le zone >","Appennino","Pianura Est","Pianura Ovest","Agglomerato")



if (Sys.info()['sysname']=="Linux") {
  DATAPATH <- "/data"
} else {
  DATAPATH <- "./data"
}

SCENDIR <- paste0(DATAPATH,"/SCENARIO-SPID")
CONFDIR <- paste0(DATAPATH,"/CONFIG")
ANAGDIR <- paste0(DATAPATH,"/ANAGRAFICA")
TEMPLATEDIR <- paste0(DATAPATH,"/TEMPLATE")
COMDIR <- paste0(DATAPATH,"/COMUNI")
PARCODIR <- paste0(DATAPATH,"/PARCO")
TRAFDIR <- paste0(DATAPATH,"/TRAFFICO")
TEMPDIR <- paste0(DATAPATH,"/TEMPORARY")
TABELLEDIR <- paste0(DATAPATH,"/TABELLE")
SRFUNC <- paste0(DATAPATH,"/SRFUNCTIONS")
GRIDDIR <- paste0(DATAPATH,"/GRIGLIA")
EMISSDIR <- paste0(DATAPATH,"/EMISSIONI")
MLMODELS <- paste0(DATAPATH,"/MLMODELS")

traffic <- base::get(load(paste0(TRAFDIR, "/", "traffic.rda")))
trafficDT <- base::get(load(paste0(TRAFDIR, "/", "trafficDT.rda")))
trafficComuni <- base::get(load(paste0(TRAFDIR, "/", "trafficComuni.rda")))
comuni <- base::get(load(paste0(COMDIR, "/", "comuni.rda")))
comuniDT <- base::get(load(paste0(COMDIR, "/", "comuniDT.rda")))
parco <- base::get(load(paste0(PARCODIR, "/", "parco.rda")))
emissioniDT <- base::get(load(paste0(EMISSDIR, "/", "emissioniDT.rda")))

