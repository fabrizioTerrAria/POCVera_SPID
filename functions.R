library(dplyr)
library(tidyr)
library(tools)
library(gtools)
library(sf)
library(ncdf4)
library(highcharter)
library(raster)
library(exactextractr)
library(stringr)
library(keras3)
library(jsonlite)
library(digest)
library(httr)

`%!in%` <- Negate(`%in%`)


callAttributeAuthority <- function(codice_fiscale) {
  
  headers = c(
    `Content-Type` = "application/json"
  )
  
  url <- paste0("https://attributi.lepida.it","/",digest(toupper(codice_fiscale), algo="md5", serialize = FALSE))
  
  res <- httr::GET(url = url, 
                   httr::add_headers(.headers=headers)
  )
  
  print(paste0(Sys.time()," - Get attribute authority. Status: ", res$status_code))
  
  if (res$status_code == 200) {
    
    result <- fromJSON(httr::content(res, as = 'text', encoding = "UTF-8"))
    
    if (result$abilitazione_poc_vera == 1) {
      
      print(paste0(Sys.time()," - Utente autorizzato ad accedere al POC VERA"))
      return("POC_USER")
      
    } else{
      
      print(paste0(Sys.time()," - Utente NON autorizzato ad accedere al POC VERA"))
      return("NO_AUTH")
      
    }
    
  } else {
    
    return(NULL)
    
  }
  
}



readVarNetCDF <- function(ncdName,varName) {
  
  ncin <- nc_open(ncdName)
  r <- ncvar_get(ncin, varName)
  nc_close(ncin)
  
  return(r)
  
}

readGlobalAttNetCDF <- function(ncdName,attName) {
  
  ncin <- nc_open(ncdName)
  r <- ncatt_get(ncin, 0, attName)
  nc_close(ncin)
  
  return(r$value)
  
}


clearDirectoryName <- function(scenarioName) {
  
  directory <- gsub(" ", "",path_sanitize(scenarioName, replacement = "_"),)
  directory <- gsub("-", "",directory,)
  return(directory)
   
}

caricaTabellaOriginale <- function(nomeTabella) {
  
  print(paste0(Sys.time(), " - Carico la tabella originale: ", nomeTabella))
  TABELLA <- read.table(paste0(TABELLEDIR,"/",nomeTabella,".csv"), sep = ";", dec = ".", header = TRUE)
  return(TABELLA)
  
}

caricaTabellaScenario <- function(scenario, nomeTabella) {
  
  print(paste0(Sys.time(), " - Carico la tabella dello scenario: ", nomeTabella))
  directory <- getScenarioDirectoryInDB(scenario)
  TABELLA <- read.table(paste0(SCENDIR,"/",directory,"/", "TABELLE/",nomeTabella,".csv"), sep = ";", dec = ".", header = TRUE)
  return(TABELLA)
  
}

salvaTabella <- function(tabella, nomeTabella, directory) {
  
  print(paste0(Sys.time(), " - Salvo la tabella: ", nomeTabella))
  write.table(tabella, paste0(directory,"/",nomeTabella,".csv"), sep = ";", dec = ".", row.names = FALSE)
  
}


valutaModificheGrafo <- function(scenario) {
  
  TL_ARCO_VEICOLI <- read.table(paste0(TABELLEDIR,"/","TL_ARCO_VEICOLI.csv"), sep = ";", dec = ".", header = TRUE)
  
  directory <- getScenarioDirectoryInDB(scenario)
  grafoFile <- paste0(SCENDIR,"/",directory,"/",directory,"_grafo.csv")
  newGrafo <- read.table(grafoFile, sep = ";", dec = ".", header = TRUE) %>%
    left_join(trafficComuni, join_by(ID_GRAFO)) %>%
    rename("VOL55" = "Automobili") %>%
    rename("VOL56" = "Leggeri") %>%
    rename("VOL57" = "Pesanti") %>%
    dplyr::select(ID_ARCO, starts_with("VOL")) %>%
    pivot_longer(starts_with("VOL"), names_to = "SECTORS", values_to = "NUMERO_VEICOLI_NEW", names_prefix = "VOL") %>%
    mutate(SECTORS = as.numeric(SECTORS))
  
  listaArchi <- TL_ARCO_VEICOLI %>%
    left_join(newGrafo, join_by(FK_ID_ARCO == ID_ARCO, FK_ID_SETTORE == SECTORS)) %>%
    filter(NUMERO_VEICOLI != NUMERO_VEICOLI_NEW) %>%
    distinct(FK_ID_ARCO) %>% 
    pull()
  
  return(listaArchi)
  
}

calcolaEmissioniLineari <- function(scenario,opzioni,setProgress,progressStart,progressEnd) {
  
  directory <- getScenarioDirectoryInDB(scenario)
  scenarioDir <- paste0(SCENDIR,"/",directory,"/")
  tabelleDir <- paste0(scenarioDir,"/","TABELLE","/")
  
  progressInc <- progressStart
  progressFile <- paste0(SCENDIR,"/",directory,"/","progress.txt")
  
  statusFile <- paste0(SCENDIR,"/",directory,"/","status.txt")
  write.table("Calcolo Emissioni Lineari",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  
  TLD_TIPO_VEICOLO <- caricaTabellaOriginale("TLD_TIPO_VEICOLO")
  TL_VEICOLI_EQUIVALENTI <- caricaTabellaOriginale("TL_VEICOLI_EQUIVALENTI")
  TL_CURVA_DISTRIBUZIONE <- caricaTabellaOriginale("TL_CURVA_DISTRIBUZIONE")
  TLD_FERIALI_FESTIVI <- caricaTabellaOriginale("TLD_FERIALI_FESTIVI")
  TL_FASCIA <- caricaTabellaOriginale("TL_FASCIA")
  TL_CURVA_DEFLUSSO <- caricaTabellaOriginale("TL_CURVA_DEFLUSSO")
  TLD_COMBUSTIBILI_RVP <- caricaTabellaOriginale("TLD_COMBUSTIBILI_RVP")
  TLD_TEMPERATURA <- caricaTabellaOriginale("TLD_TEMPERATURA")
  TLD_INQUINANTI_COMB <- caricaTabellaOriginale("TLD_INQUINANTI_COMB")
  TD_VELOCITA <- caricaTabellaOriginale("TD_VELOCITA")
  TDI_PROXY_PERC_REG <- caricaTabellaOriginale("TDI_PROXY_PERC_REG")
  
  TLD_FATTORI_COPERT_INQ <- caricaTabellaOriginale("TLD_FATTORI_COPERT_INQ")
  TLD_FATTORI_USURA_INQ <- caricaTabellaOriginale("TLD_FATTORI_USURA_INQ")
  TLD_FATTORI_EVAPORATIVE_INQ <- caricaTabellaOriginale("TLD_FATTORI_EVAPORATIVE_INQ")
  TLD_FATTORI_EVAPORATIVE_DIFFUSE_INQ <- caricaTabellaOriginale("TLD_FATTORI_EVAPORATIVE_DIFFUSE_INQ")
  TLD_FATTORI_EMISSIONI_FREDDO_INQ <- caricaTabellaOriginale("TLD_FATTORI_EMISSIONI_FREDDO_INQ")
  
  ISTAT_COMUNI <- caricaTabellaOriginale("ISTAT_COMUNI")
  RESIDENTI_REGIONE <- sum(ISTAT_COMUNI$RESIDENTI)
  
  if (is.null(opzioni)) {
    
    nSteps <- 13
    
    grafoFile <- paste0(SCENDIR,"/",directory,"/",directory,"_grafo.csv")
    TL_ARCO_VEICOLI <- read.table(grafoFile, sep = ";", dec = ".", header = TRUE) %>%
      left_join(trafficComuni, join_by(ID_GRAFO)) %>%
      rename("VOL55" = "Automobili") %>%
      rename("VOL56" = "Leggeri") %>%
      rename("VOL57" = "Pesanti") %>%
      rename("FK_ID_ARCO" = "ID_ARCO") %>%
      dplyr::select(FK_ID_ARCO, starts_with("VOL")) %>%
      # filter(FK_ID_ARCO %in% listaArchiDaSimulare) %>%
      pivot_longer(starts_with("VOL"), names_to = "FK_ID_SETTORE", values_to = "NUMERO_VEICOLI", names_prefix = "VOL") %>%
      mutate(FK_ID_SETTORE = as.numeric(FK_ID_SETTORE))
    
    TL_PARCO_REG <- caricaTabellaOriginale("TL_PARCO_REG")
    TL_ARCO <- caricaTabellaOriginale("TL_ARCO") # %>% filter(ID_ARCO %in% listaArchiDaSimulare)
    
    
    TL_SOMMATORIA_PERC_REG <- TL_PARCO_REG %>%
      left_join(TLD_TIPO_VEICOLO, join_by(CODICE_COPART)) %>%
      group_by(FK_ID_SETTORE) %>%
      mutate(NUMERO_VEICOLI_COPART = as.numeric(NUMERO_VEICOLI))  %>%  # converto in double per evitare integer overflow
      mutate(PERCORRENZA_MEDIA_LIN = as.numeric(PERCORRENZA_MEDIA_LIN))  %>% # converto in double per evitare integer overflow
      summarise(PERCORRENZA_REG = sum(NUMERO_VEICOLI_COPART * PERCORRENZA_MEDIA_LIN))
    
    salvaTabella(TL_SOMMATORIA_PERC_REG, "TL_SOMMATORIA_PERC_REG", tabelleDir)
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    if (setProgress) {
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    }
    
    TLI_NVE_SETT <- TL_ARCO %>%
      filter(CAPACITA_MAX > 0) %>%
      left_join(TL_ARCO_VEICOLI, join_by(ID_ARCO == FK_ID_ARCO)) %>%
      left_join(TL_VEICOLI_EQUIVALENTI, join_by(FK_ID_SETTORE == FK_ID_SETTORE)) %>%
      left_join(TL_CURVA_DISTRIBUZIONE, join_by(FK_ID_SETTORE, ID_PROFILO_TEMPORALE), relationship = "many-to-many") %>%
      left_join(TLD_FERIALI_FESTIVI, join_by(FK_ID_STAGIONE_GIORNO == ID_STAGIONE_GIORNO)) %>%
      left_join(TL_FASCIA, join_by(FK_ID_FASCIA_ORARIA  == ID_FASCIA_ORARIA)) %>%
      mutate(NV_ARCO_TEMPORALE = NUMERO_VEICOLI * COEFFICIENTE_TEMPORALE) %>%
      mutate(NVE_SETT = NV_ARCO_TEMPORALE * NUM_VEICOLI_EQUIVALENTI ) %>%
      dplyr::select(ID_ARCO, ID_PROFILO_TEMPORALE, FK_ID_STAGIONE_GIORNO, MESE, CODICE_CURVA, FK_ID_TIPO_STRADA, CAPACITA_MAX, VELOCITA_MAX, NUMERO_ORE, FK_ID_SETTORE, FK_ID_FASCIA_ORARIA, LUNGHEZZA, NR_GIORNI, COEFFICIENTE_TEMPORALE, NV_ARCO_TEMPORALE, NVE_SETT, FK_ISTAT_COMUNE)
    
    salvaTabella(TLI_NVE_SETT, "TLI_NVE_SETT", tabelleDir)
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    if (setProgress) {
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    }
    
    
    TD_PERC_VKM <- TLI_NVE_SETT %>%
      mutate(ORE_PER_GIORNI = NUMERO_ORE * NR_GIORNI) %>%
      mutate(PERC_VKM = NV_ARCO_TEMPORALE * LUNGHEZZA * ORE_PER_GIORNI) %>%
      group_by(FK_ID_SETTORE)  %>%
      summarise(PERC_VKM = sum(PERC_VKM, na.rm = TRUE))
    
    salvaTabella(TD_PERC_VKM, "TD_PERC_VKM", tabelleDir)
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    if (setProgress) {
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    }
    
    
    TD_PERC_LINEARI <-  TL_PARCO_REG %>%
      left_join(TLD_TIPO_VEICOLO, join_by(CODICE_COPART)) %>%
      left_join(TL_SOMMATORIA_PERC_REG, join_by(FK_ID_SETTORE)) %>%
      left_join(TD_PERC_VKM, join_by(FK_ID_SETTORE)) %>%
      mutate(PERCORRENZA = (PERC_VKM * as.numeric(NUMERO_VEICOLI) * as.numeric(PERCORRENZA_MEDIA_LIN) / as.numeric(PERCORRENZA_REG))) %>%
      group_by(CODICE_COPART) %>%
      summarise(PERCORRENZA = sum(PERCORRENZA, na.rm = TRUE))
    
    salvaTabella(TD_PERC_LINEARI, "TD_PERC_LINEARI", tabelleDir)
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    if (setProgress) {
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    }
    
    
    TLI_VELOCITA_EFFETTIVA <- TLI_NVE_SETT %>%
      group_by(ID_ARCO,FK_ID_STAGIONE_GIORNO,FK_ID_FASCIA_ORARIA,CODICE_CURVA,CAPACITA_MAX,VELOCITA_MAX) %>%
      summarise(NVE = sum(NVE_SETT)) %>%
      mutate(PC = NVE / CAPACITA_MAX) %>%  # Frazione di capacità
      left_join(TL_CURVA_DEFLUSSO, join_by(CODICE_CURVA), relationship = "many-to-many") %>%
      filter(round(PC, digits = 1) == FRAZIONE_CAPACITA) %>%
      mutate(VELOCITA_EFFETTIVA = max(round(FRAZIONE_VELOCITA * VELOCITA_MAX, digits = 0), 10)) %>%
      dplyr::select(ID_ARCO, FK_ID_STAGIONE_GIORNO, FK_ID_FASCIA_ORARIA, CODICE_CURVA, CAPACITA_MAX, VELOCITA_MAX, NVE, VELOCITA_EFFETTIVA)
    
    salvaTabella(TLI_VELOCITA_EFFETTIVA, "TLI_VELOCITA_EFFETTIVA", tabelleDir)
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    if (setProgress) {
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    }
    
  } else {
    
    nSteps <- 8
    
    parcoFile <- paste0(SCENDIR,"/",directory,"/",directory,"_parco.csv")
    TL_PARCO_REG <- read.table(parcoFile, sep = ";", dec = ".", header = TRUE) %>%
      dplyr::select(CODICE_COPART, NUMERO_VEICOLI)
    
    comuniFile <- paste0(SCENDIR,"/",directory,"/",directory,"_comuni.csv")
    comuniSelezionati <- read.table(comuniFile, sep = ";", dec = ".", header = TRUE)
    ISTAT_COMUNI <- caricaTabellaOriginale("ISTAT_COMUNI") %>% filter(ISTAT_COMUNE %in% comuniSelezionati$PRO_COM)
    
    tipoStrada <- ROAD_TYPE_ID[which(ROAD_TYPE %in% opzioni)]
    
    # TL_ARCO <- caricaTabellaOriginale("TL_ARCO") %>% filter(FK_ID_TIPO_STRADA %in% tipoStrada & FK_ISTAT_COMUNE %in% ISTAT_COMUNI$ISTAT_COMUNE)
    # TL_ARCO_VEICOLI <- caricaTabellaOriginale("TL_ARCO_VEICOLI") %>% filter(FK_ID_ARCO %in% TL_ARCO$ID_ARCO)
    TL_SOMMATORIA_PERC_REG <- caricaTabellaOriginale("TL_SOMMATORIA_PERC_REG")
    TLI_NVE_SETT <- caricaTabellaOriginale("TLI_NVE_SETT") %>% filter(FK_ID_TIPO_STRADA %in% tipoStrada & FK_ISTAT_COMUNE %in% ISTAT_COMUNI$ISTAT_COMUNE)
    TD_PERC_VKM <- caricaTabellaOriginale("TD_PERC_VKM")
    TD_PERC_LINEARI <- caricaTabellaOriginale("TD_PERC_LINEARI")
    TLI_VELOCITA_EFFETTIVA <- caricaTabellaOriginale("TLI_VELOCITA_EFFETTIVA")
    
    elencoArchi <- unique(TLI_NVE_SETT$ID_ARCO)
    
  }
  
  
  #### CALCOLO EMISSIONI ALLO SCARICO #### 
  
  TL_FATTORI_COPERT_MEDI <-  TL_PARCO_REG %>%
    left_join(TLD_TIPO_VEICOLO, join_by(CODICE_COPART)) %>%
    left_join(TLD_FATTORI_COPERT_INQ, join_by(CODICE_COPART)) %>%
    filter(VELOCITA_EFFETTIVA %in% unique(TLI_VELOCITA_EFFETTIVA$VELOCITA_EFFETTIVA)) %>%
    left_join(TL_SOMMATORIA_PERC_REG, join_by(FK_ID_SETTORE)) %>%
    dplyr::select(CODICE_COPART, NUMERO_VEICOLI, FK_ID_SETTORE, FK_ID_COMBUSTIBILE, PERCORRENZA_MEDIA_LIN, PERCORRENZA_REG, VELOCITA_EFFETTIVA, starts_with("FE_"))  %>%
    mutate(PERC_VKM = (as.numeric(NUMERO_VEICOLI) * as.numeric(PERCORRENZA_MEDIA_LIN)) / as.numeric(PERCORRENZA_REG)) %>%
    mutate_at(vars(starts_with("FE_")),  ~. * PERC_VKM) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,VELOCITA_EFFETTIVA) %>%
    summarise_at(vars(starts_with("FE_")), ~ sum(.x, na.rm = TRUE))
  
  salvaTabella(TL_FATTORI_COPERT_MEDI, "TL_FATTORI_COPERT_MEDI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  TL_EMISSIONI_SCARICO <- TLI_NVE_SETT %>%
    left_join(TLI_VELOCITA_EFFETTIVA, join_by(ID_ARCO, FK_ID_STAGIONE_GIORNO,FK_ID_FASCIA_ORARIA )) %>%
    left_join(TL_FATTORI_COPERT_MEDI, join_by(FK_ID_SETTORE,VELOCITA_EFFETTIVA), relationship = "many-to-many") %>%
    mutate(ORE_PER_GIORNI = NUMERO_ORE * NR_GIORNI) %>%
    mutate_at(vars(starts_with("FE_")), list(EMI_ANNO = ~. * NV_ARCO_TEMPORALE * LUNGHEZZA * ORE_PER_GIORNI / 1000000)) %>%
    group_by(ID_ARCO,FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    rename_with(., ~ sub("FE_", "", .), starts_with("FE_")) %>%
    mutate(EMI_TIPO = "LSC")

  # calcolo inquinanti derivati
  TL_EMISSIONI_SCARICO <- TL_EMISSIONI_SCARICO %>%
    ungroup() %>%
    left_join(TLD_INQUINANTI_COMB[TLD_INQUINANTI_COMB$FK_ID_INQUINANTE %in% c(1),], join_by(FK_ID_COMBUSTIBILE)) %>%
    mutate(SO2_EMI_ANNO = FC_EMI_ANNO * VALORE) %>%
    dplyr::select(ID_ARCO,FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,ends_with("EMI_ANNO"),EMI_TIPO) %>%
    left_join(TLD_INQUINANTI_COMB[TLD_INQUINANTI_COMB$FK_ID_INQUINANTE %in% c(6),], join_by(FK_ID_COMBUSTIBILE)) %>%
    mutate(CO2_EMI_ANNO = FC_EMI_ANNO * VALORE) %>%
    dplyr::select(ID_ARCO,FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,ends_with("EMI_ANNO"),EMI_TIPO) %>%
    mutate(PM10_EMI_ANNO = PTS_EMI_ANNO) %>%
    mutate(PM2.5_EMI_ANNO = PTS_EMI_ANNO) %>%
    mutate(COV_EMI_ANNO = if_else(SOV_EMI_ANNO - CH4_EMI_ANNO < 0,0,SOV_EMI_ANNO - CH4_EMI_ANNO) ) 
  
  salvaTabella(TL_EMISSIONI_SCARICO, "TL_EMISSIONI_SCARICO", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  #### CALCOLO EMISSIONI USURA #### 
  
  TL_FATTORI_USURA_MEDI <-  TL_PARCO_REG %>%
    left_join(TLD_TIPO_VEICOLO, join_by(CODICE_COPART)) %>%
    left_join(TLD_FATTORI_USURA_INQ, join_by(CODICE_COPART)) %>%
    filter(VELOCITA_EFFETTIVA %in% unique(TLI_VELOCITA_EFFETTIVA$VELOCITA_EFFETTIVA)) %>%
    left_join(TL_SOMMATORIA_PERC_REG, join_by(FK_ID_SETTORE)) %>%
    mutate(PERC_VKM = (as.numeric(NUMERO_VEICOLI) * as.numeric(PERCORRENZA_MEDIA_LIN)) / as.numeric(PERCORRENZA_REG)) %>%
    mutate_at(vars(starts_with("FE_")),  ~. * PERC_VKM) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,VELOCITA_EFFETTIVA) %>%
    summarise_at(vars(starts_with("FE_")), ~ sum(.x, na.rm = TRUE))
  
  salvaTabella(TL_FATTORI_USURA_MEDI, "TL_FATTORI_USURA_MEDI", tabelleDir)
  
  TL_EMISSIONI_USURA <- TLI_NVE_SETT %>%
    left_join(TLI_VELOCITA_EFFETTIVA, join_by(ID_ARCO, FK_ID_STAGIONE_GIORNO,FK_ID_FASCIA_ORARIA )) %>%
    left_join(TL_FATTORI_USURA_MEDI, join_by(FK_ID_SETTORE,VELOCITA_EFFETTIVA), relationship = "many-to-many") %>%
    mutate(ORE_PER_GIORNI = NUMERO_ORE * NR_GIORNI) %>%
    mutate_at(vars(starts_with("FE_")), list(EMI_ANNO = ~. * NV_ARCO_TEMPORALE * LUNGHEZZA * ORE_PER_GIORNI / 1000000)) %>%
    group_by(ID_ARCO,FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    rename_with(., ~ sub("FE_", "", .), starts_with("FE_")) %>%
    mutate(EMI_TIPO = "LU")
  
  salvaTabella(TL_EMISSIONI_USURA, "TL_EMISSIONI_USURA", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  #### CALCOLO EMISSIONI EVAPORATIVE #### 
  
  TL_FATTORI_EVAPORATIVE_MEDI <-  TL_PARCO_REG %>%
    left_join(TLD_TIPO_VEICOLO, join_by(CODICE_COPART)) %>%
    left_join(TLD_FATTORI_EVAPORATIVE_INQ, join_by(CODICE_COPART)) %>%
    filter(TEMPERATURA %in% unique(round(TLD_TEMPERATURA$TEMP_MEDIA, digits = 0))) %>%
    left_join(TL_SOMMATORIA_PERC_REG, join_by(FK_ID_SETTORE)) %>%
    mutate(PERC_VKM = (as.numeric(NUMERO_VEICOLI) * as.numeric(PERCORRENZA_MEDIA_LIN)) / as.numeric(PERCORRENZA_REG)) %>%
    mutate_at(vars(starts_with("FE_")),  ~. * PERC_VKM) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,TEMPERATURA,RVP) %>%
    summarise_at(vars(starts_with("FE_")), ~ sum(.x, na.rm = TRUE))
  
  salvaTabella(TL_FATTORI_EVAPORATIVE_MEDI, "TL_FATTORI_EVAPORATIVE_MEDI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  TL_EMISSIONI_EVAPORATIVE <- TLI_NVE_SETT %>%
    left_join(ISTAT_COMUNI, join_by(FK_ISTAT_COMUNE == ISTAT_COMUNE)) %>%
    filter(RESIDENTI>0) %>%
    left_join(TLD_TEMPERATURA, join_by(MESE, FK_ID_CLASSE_CLIMATICA )) %>% 
    left_join(TLD_COMBUSTIBILI_RVP, join_by(MESE)) %>% 
    mutate(TEMPERATURA = round(TEMP_MEDIA, digits = 0)) %>%
    right_join(TL_FATTORI_EVAPORATIVE_MEDI, join_by(FK_ID_SETTORE,TEMPERATURA,RVP), relationship = "many-to-many") %>%
    mutate(ORE_PER_GIORNI = NUMERO_ORE * NR_GIORNI) %>%
    mutate(SOV_EMI_ANNO = FE_SOV * NV_ARCO_TEMPORALE * LUNGHEZZA * ORE_PER_GIORNI / 1000000) %>%
    group_by(ID_ARCO,FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    mutate(EMI_TIPO = "LE")
  
  # calcolo inquinanti derivati
  TL_EMISSIONI_EVAPORATIVE <- TL_EMISSIONI_EVAPORATIVE %>% 
    mutate(COV_EMI_ANNO = SOV_EMI_ANNO) 
  
  salvaTabella(TL_EMISSIONI_EVAPORATIVE, "TL_EMISSIONI_EVAPORATIVE", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  #### CALCOLO EMISSIONI A FREDDO #### 
  
  TL_FATTORI_FREDDO_MEDI <-  TD_PERC_LINEARI %>%
    left_join(TLD_TIPO_VEICOLO, join_by(CODICE_COPART)) %>%
    # left_join(TLD_FATTORI_EMISSIONI_FREDDO_INQ, join_by(COD_COPART_EMIFREDDO == CODICE_COPART)) %>%
    left_join(TLD_FATTORI_EMISSIONI_FREDDO_INQ, join_by(CODICE_COPART)) %>%
    filter(TEMPERATURA %in% unique(round(TLD_TEMPERATURA$TEMP_MEDIA, digits = 0))) %>%
    left_join(TDI_PROXY_PERC_REG[TDI_PROXY_PERC_REG$FK_ID_PROXY == 1,], join_by(CODICE_COPART == FK_CODICE_COPART)) %>%
    left_join(TL_PARCO_REG, join_by(CODICE_COPART)) %>%
    # mutate(PERC_VKM = (as.numeric(PERCORRENZA) * as.numeric(SOMMA_PROXY_PERC_INV))) %>%
    mutate(PERC_VKM = (as.numeric(PERCORRENZA) * as.numeric(NUMERO_VEICOLI) * as.numeric(PERCORRENZA_MEDIA_LIN) / RESIDENTI_REGIONE * as.numeric(SOMMA_PROXY_PERC_INV))) %>%
    mutate_at(vars(starts_with("FE_FREDDO_")),  ~. * PERC_VKM) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,VELOCITA,TEMPERATURA) %>%
    summarise_at(vars(starts_with("FE_FREDDO_")), ~ sum(.x, na.rm = TRUE))
  
  salvaTabella(TL_FATTORI_FREDDO_MEDI, "TL_FATTORI_FREDDO_MEDI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  TL_EMISSIONI_FREDDO <- ISTAT_COMUNI %>%
    filter(RESIDENTI>0) %>%
    left_join(TD_VELOCITA, join_by(FK_CLASSE_COMUNE_TD), relationship = "many-to-many") %>% 
    left_join(TLD_FERIALI_FESTIVI, join_by(FK_ID_STAGIONE_GIORNO == ID_STAGIONE_GIORNO )) %>% 
    # left_join(TDI_BETA, join_by(ISTAT_COMUNE == FK_ISTAT_COMUNE, MESE)) %>% 
    left_join(TLD_TEMPERATURA, join_by(MESE, FK_ID_CLASSE_CLIMATICA )) %>% 
    mutate(TEMPERATURA = round(TEMP_MEDIA, digits = 0)) %>%
    mutate(VELOCITA = round(VELOCITA_MEDIA , digits = 0)) %>%
    left_join(TL_FATTORI_FREDDO_MEDI, join_by(FK_ID_SETTORE,VELOCITA,TEMPERATURA), relationship = "many-to-many") %>%
    filter(!is.na(FK_ID_COMBUSTIBILE )) %>%
    mutate_at(vars(starts_with("FE_FREDDO_")), list(EMI_ANNO = ~. * RESIDENTI  * FRAZIONE_PERCORRENZA * NR_GIORNI / 1000000)) %>%
    group_by(ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    mutate_at(vars(starts_with("FE_FREDDO_")), ~ if_else(.x < 0, 0, .x) ) %>%
    rename_with(., ~ sub("FE_FREDDO_", "", .), starts_with("FE_FREDDO_")) %>%
    rename(FK_ISTAT_COMUNE = ISTAT_COMUNE ) %>%
    mutate(EMI_TIPO = "LSF")
  
  
  # calcolo inquinanti derivati
  TL_EMISSIONI_FREDDO <- TL_EMISSIONI_FREDDO %>%
    ungroup() %>%
    left_join(TLD_INQUINANTI_COMB[TLD_INQUINANTI_COMB$FK_ID_INQUINANTE %in% c(1),], join_by(FK_ID_COMBUSTIBILE)) %>%
    mutate(SO2_EMI_ANNO = FC_EMI_ANNO * VALORE) %>%
    dplyr::select(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,ends_with("EMI_ANNO"),EMI_TIPO) %>%
    left_join(TLD_INQUINANTI_COMB[TLD_INQUINANTI_COMB$FK_ID_INQUINANTE %in% c(6),], join_by(FK_ID_COMBUSTIBILE)) %>%
    mutate(CO2_EMI_ANNO = FC_EMI_ANNO * VALORE) %>%
    dplyr::select(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,ends_with("EMI_ANNO"),EMI_TIPO) %>%
    mutate(PM10_EMI_ANNO = PTS_EMI_ANNO) %>%
    mutate(PM2.5_EMI_ANNO = PTS_EMI_ANNO) %>%
    mutate(COV_EMI_ANNO = if_else(SOV_EMI_ANNO - CH4_EMI_ANNO < 0,0,SOV_EMI_ANNO - CH4_EMI_ANNO) ) 
  
  salvaTabella(TL_EMISSIONI_FREDDO, "TL_EMISSIONI_FREDDO", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  #### CALCOLO EMISSIONI TOTALI LINEARI #### 
  
  if (is.null(opzioni)) {
    
    TL_EMISSIONI_TOTALI <- TL_EMISSIONI_SCARICO %>%
      bind_rows(TL_EMISSIONI_USURA) %>%
      bind_rows(TL_EMISSIONI_EVAPORATIVE) %>%
      bind_rows(TL_EMISSIONI_FREDDO) 
    
    salvaTabella(TL_EMISSIONI_TOTALI, "TL_EMISSIONI_TOTALI", tabelleDir)
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    if (setProgress) {
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    }
    
    
  } else {
    
    
    TL_EMISSIONI_TOTALI_SCEN <- TL_EMISSIONI_SCARICO %>%
      bind_rows(TL_EMISSIONI_USURA) %>%
      bind_rows(TL_EMISSIONI_EVAPORATIVE) %>%
      bind_rows(TL_EMISSIONI_FREDDO) 
    
    TL_EMISSIONI_TOTALI_ALTRE <- caricaTabellaOriginale("TL_EMISSIONI_TOTALI") %>%
      filter(EMI_TIPO %in% c("LSC","LE","LU") & !ID_ARCO %in% elencoArchi)
    
    TL_EMISSIONI_TOTALI_ALTRE_FREDDO <- caricaTabellaOriginale("TL_EMISSIONI_TOTALI") %>%
      filter(EMI_TIPO == "LSF" & !FK_ISTAT_COMUNE %in% ISTAT_COMUNI$ISTAT_COMUNE)
    
    TL_EMISSIONI_TOTALI <- TL_EMISSIONI_TOTALI_SCEN %>%
      bind_rows(TL_EMISSIONI_TOTALI_ALTRE) %>%
      bind_rows(TL_EMISSIONI_TOTALI_ALTRE_FREDDO)
    
    salvaTabella(TL_EMISSIONI_TOTALI, "TL_EMISSIONI_TOTALI", tabelleDir)
    
    progressInc <- progressInc + (progressEnd - progressStart) / nSteps
    if (setProgress) {
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    }
    
    
  }
  
  
  return(TL_EMISSIONI_TOTALI)
  
}

calcolaEmissioniDiffuse <- function(scenario,setProgress,progressStart,progressEnd) {
  
  directory <- getScenarioDirectoryInDB(scenario)
  scenarioDir <- paste0(SCENDIR,"/",directory,"/")
  tabelleDir <- paste0(scenarioDir,"/","TABELLE","/")
  
  nSteps <- 12
  progressInc <- progressStart
  progressFile <- paste0(SCENDIR,"/",directory,"/","progress.txt")
  
  statusFile <- paste0(SCENDIR,"/",directory,"/","status.txt")
  write.table("Calcolo Emissioni Diffuse",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  
  
  # if (!dir.exists(tabelleDir)) dir.create(tabelleDir)
  
  parcoFile <- paste0(SCENDIR,"/",directory,"/",directory,"_parco.csv")
  TL_PARCO_REG <- read.table(parcoFile, sep = ";", dec = ".", header = TRUE) %>%
    dplyr::select(CODICE_COPART, NUMERO_VEICOLI)

  comuniFile <- paste0(SCENDIR,"/",directory,"/",directory,"_comuni.csv")
  comuniSelezionati <- read.table(comuniFile, sep = ";", dec = ".", header = TRUE)
  
  ISTAT_COMUNI <- caricaTabellaOriginale("ISTAT_COMUNI")
  RESIDENTI_REGIONE <- sum(ISTAT_COMUNI$RESIDENTI)
  
  ISTAT_COMUNI <- ISTAT_COMUNI %>% filter(ISTAT_COMUNE %in% comuniSelezionati$PRO_COM)
    
  TLD_TIPO_VEICOLO <- caricaTabellaOriginale("TLD_TIPO_VEICOLO")
  
  TLD_FERIALI_FESTIVI <- caricaTabellaOriginale("TLD_FERIALI_FESTIVI")
  TLD_TEMPERATURA <- caricaTabellaOriginale("TLD_TEMPERATURA")
  TLD_INQUINANTI_COMB <- caricaTabellaOriginale("TLD_INQUINANTI_COMB")
  TD_VELOCITA <- caricaTabellaOriginale("TD_VELOCITA")
  TDI_PROXY_PERC_REG <- caricaTabellaOriginale("TDI_PROXY_PERC_REG")
  TDI_SUM_PROXY_COMB <- caricaTabellaOriginale("TDI_SUM_PROXY_COMB")
  TLD_CARBURANTI <- caricaTabellaOriginale("TLD_CARBURANTI")
  
  TLD_FATTORI_COPERT_INQ <- caricaTabellaOriginale("TLD_FATTORI_COPERT_INQ")
  TLD_FATTORI_USURA_INQ <- caricaTabellaOriginale("TLD_FATTORI_USURA_INQ")
  TLD_FATTORI_EVAPORATIVE_DIFFUSE_INQ <- caricaTabellaOriginale("TLD_FATTORI_EVAPORATIVE_DIFFUSE_INQ")
  TLD_FATTORI_EMISSIONI_FREDDO_INQ <- caricaTabellaOriginale("TLD_FATTORI_EMISSIONI_FREDDO_INQ")
  
  TD_EMISSIONI_TOTALI <- caricaTabellaOriginale("TD_EMISSIONI_TOTALI")
  
  
  
  TD_FATTORI_MEDI <- TLD_TIPO_VEICOLO %>%
    left_join(TLD_FATTORI_COPERT_INQ, join_by(CODICE_COPART)) %>%
    left_join(TDI_SUM_PROXY_COMB[TDI_SUM_PROXY_COMB$FK_ID_PROXY == 2,], join_by(FK_ID_COMBUSTIBILE)) %>%
    left_join(TDI_PROXY_PERC_REG[TDI_PROXY_PERC_REG$FK_ID_PROXY == 2,], join_by(CODICE_COPART == FK_CODICE_COPART)) %>%
    left_join(TL_PARCO_REG, join_by(CODICE_COPART)) %>%
    left_join(TLD_CARBURANTI, join_by(FK_ID_COMBUSTIBILE)) %>%
    left_join(TLD_FATTORI_EMISSIONI_FREDDO_INQ, join_by(CODICE_COPART,VELOCITA_EFFETTIVA == VELOCITA), relationship = "many-to-many") %>%
    filter(!is.na(TEMPERATURA )) %>%
    mutate(PERC_CONS = (as.numeric(CONSUMO_CARB_REG) * as.numeric(NUMERO_VEICOLI) * (as.numeric(PERCORRENZA_MEDIA_TOT) - as.numeric(PERCORRENZA_MEDIA_LIN)) * FCU / RESIDENTI_REGIONE * as.numeric(SOMMA_PROXY_INV) / FATTORE_CONSUMO)) %>%
    mutate_at(vars(starts_with("FE_") & !starts_with("FE_FREDDO_")),  ~. * PERC_CONS) %>%
    rename(VELOCITA = VELOCITA_EFFETTIVA) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,VELOCITA,TEMPERATURA) %>%
    summarise_at(vars(starts_with("FE_") & !starts_with("FE_FREDDO_")), ~ sum(.x, na.rm = TRUE))
  
  salvaTabella(TD_FATTORI_MEDI, "TD_FATTORI_MEDI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  TD_EMISSIONI_SCARICO <- ISTAT_COMUNI %>%
    filter(RESIDENTI>0) %>%
    left_join(TD_VELOCITA, join_by(FK_CLASSE_COMUNE_TD), relationship = "many-to-many") %>% 
    left_join(TLD_FERIALI_FESTIVI, join_by(FK_ID_STAGIONE_GIORNO == ID_STAGIONE_GIORNO )) %>% 
    left_join(TLD_TEMPERATURA, join_by(MESE, FK_ID_CLASSE_CLIMATICA )) %>% 
    mutate(TEMPERATURA = round(TEMP_MEDIA, digits = 0)) %>%
    mutate(VELOCITA = round(VELOCITA_MEDIA , digits = 0)) %>%
    left_join(TD_FATTORI_MEDI, join_by(FK_ID_SETTORE,VELOCITA,TEMPERATURA), relationship = "many-to-many") %>%
    filter(!is.na(FK_ID_COMBUSTIBILE )) %>%
    mutate_at(vars(starts_with("FE_")), list(EMI_ANNO = ~. * RESIDENTI  * FRAZIONE_PERCORRENZA * NR_GIORNI)) %>%
    group_by(ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    mutate_at(vars(starts_with("FE_")), ~ if_else(.x < 0, 0, .x) ) %>%
    rename_with(., ~ sub("FE_", "", .), starts_with("FE_")) %>%
    rename(FK_ISTAT_COMUNE = ISTAT_COMUNE ) %>%
    mutate(EMI_TIPO = "DSC")
  
  # calcolo inquinanti derivati
  TD_EMISSIONI_SCARICO <- TD_EMISSIONI_SCARICO %>%
    ungroup() %>%
    left_join(TLD_INQUINANTI_COMB[TLD_INQUINANTI_COMB$FK_ID_INQUINANTE %in% c(1),], join_by(FK_ID_COMBUSTIBILE)) %>%
    mutate(SO2_EMI_ANNO = FC_EMI_ANNO * VALORE) %>%
    dplyr::select(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,ends_with("EMI_ANNO"),EMI_TIPO) %>%
    left_join(TLD_INQUINANTI_COMB[TLD_INQUINANTI_COMB$FK_ID_INQUINANTE %in% c(6),], join_by(FK_ID_COMBUSTIBILE)) %>%
    mutate(CO2_EMI_ANNO = FC_EMI_ANNO * VALORE) %>%
    dplyr::select(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,ends_with("EMI_ANNO"),EMI_TIPO) %>%
    mutate(PM10_EMI_ANNO = PTS_EMI_ANNO) %>%
    mutate(PM2.5_EMI_ANNO = PTS_EMI_ANNO) %>%
    mutate(COV_EMI_ANNO = if_else(SOV_EMI_ANNO - CH4_EMI_ANNO < 0,0,SOV_EMI_ANNO - CH4_EMI_ANNO) ) 
  
  salvaTabella(TD_EMISSIONI_SCARICO, "TD_EMISSIONI_SCARICO", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  #### CALCOLO EMISSIONI A FREDDO - DIFFUSE #### 
  
  TD_FATTORI_FREDDO_MEDI <-  TLD_TIPO_VEICOLO %>%
    left_join(TDI_SUM_PROXY_COMB[TDI_SUM_PROXY_COMB$FK_ID_PROXY == 2,], join_by(FK_ID_COMBUSTIBILE)) %>%
    left_join(TDI_PROXY_PERC_REG[TDI_PROXY_PERC_REG$FK_ID_PROXY == 2,], join_by(CODICE_COPART == FK_CODICE_COPART)) %>%
    right_join(TL_PARCO_REG, join_by(CODICE_COPART)) %>%
    left_join(TLD_CARBURANTI, join_by(FK_ID_COMBUSTIBILE)) %>%
    left_join(TLD_FATTORI_EMISSIONI_FREDDO_INQ, join_by(CODICE_COPART), relationship = "many-to-many") %>%
    mutate(PERC_CONS = (as.numeric(CONSUMO_CARB_REG) * as.numeric(NUMERO_VEICOLI) * (as.numeric(PERCORRENZA_MEDIA_TOT) - as.numeric(PERCORRENZA_MEDIA_LIN)) * FCU / RESIDENTI_REGIONE * as.numeric(SOMMA_PROXY_INV) / FATTORE_CONSUMO)) %>%
    mutate_at(vars(starts_with("FE_FREDDO_")),  ~. * PERC_CONS) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,VELOCITA,TEMPERATURA) %>%
    summarise_at(vars(starts_with("FE_FREDDO_")), ~ sum(.x, na.rm = TRUE))
  
  salvaTabella(TD_FATTORI_FREDDO_MEDI, "TD_FATTORI_FREDDO_MEDI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  TD_EMISSIONI_FREDDO <- ISTAT_COMUNI %>%
    filter(RESIDENTI>0) %>%
    left_join(TD_VELOCITA, join_by(FK_CLASSE_COMUNE_TD), relationship = "many-to-many") %>% 
    left_join(TLD_FERIALI_FESTIVI, join_by(FK_ID_STAGIONE_GIORNO == ID_STAGIONE_GIORNO )) %>% 
    # left_join(TDI_BETA, join_by(ISTAT_COMUNE == FK_ISTAT_COMUNE, MESE)) %>% 
    left_join(TLD_TEMPERATURA, join_by(MESE, FK_ID_CLASSE_CLIMATICA )) %>% 
    mutate(TEMPERATURA = round(TEMP_MEDIA, digits = 0)) %>%
    mutate(VELOCITA = round(VELOCITA_MEDIA , digits = 0)) %>%
    left_join(TD_FATTORI_FREDDO_MEDI, join_by(FK_ID_SETTORE,VELOCITA,TEMPERATURA), relationship = "many-to-many") %>%
    filter(!is.na(FK_ID_COMBUSTIBILE )) %>%
    mutate_at(vars(starts_with("FE_FREDDO_")), list(EMI_ANNO = ~. * RESIDENTI  * FRAZIONE_PERCORRENZA * NR_GIORNI)) %>%
    group_by(ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    mutate_at(vars(starts_with("FE_FREDDO_")), ~ if_else(.x < 0, 0, .x) ) %>%
    rename_with(., ~ sub("FE_FREDDO_", "", .), starts_with("FE_FREDDO_")) %>%
    rename(FK_ISTAT_COMUNE = ISTAT_COMUNE ) %>%
    mutate(EMI_TIPO = "DSF")
  
  # calcolo inquinanti derivati
  TD_EMISSIONI_FREDDO <- TD_EMISSIONI_FREDDO %>%
    ungroup() %>%
    left_join(TLD_INQUINANTI_COMB[TLD_INQUINANTI_COMB$FK_ID_INQUINANTE %in% c(1),], join_by(FK_ID_COMBUSTIBILE)) %>%
    mutate(SO2_EMI_ANNO = FC_EMI_ANNO * VALORE) %>%
    dplyr::select(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,ends_with("EMI_ANNO"),EMI_TIPO) %>%
    left_join(TLD_INQUINANTI_COMB[TLD_INQUINANTI_COMB$FK_ID_INQUINANTE %in% c(6),], join_by(FK_ID_COMBUSTIBILE)) %>%
    mutate(CO2_EMI_ANNO = FC_EMI_ANNO * VALORE) %>%
    dplyr::select(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,ends_with("EMI_ANNO"),EMI_TIPO) %>%
    mutate(PM10_EMI_ANNO = PTS_EMI_ANNO) %>%
    mutate(PM2.5_EMI_ANNO = PTS_EMI_ANNO) %>%
    mutate(COV_EMI_ANNO = if_else(SOV_EMI_ANNO - CH4_EMI_ANNO < 0,0,SOV_EMI_ANNO - CH4_EMI_ANNO) ) 
  
  salvaTabella(TD_EMISSIONI_FREDDO, "TD_EMISSIONI_FREDDO", tabelleDir)

  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  #### CALCOLO EMISSIONI USURA - DIFFUSE #### 
  
  TD_FATTORI_USURA_MEDI <-  TLD_TIPO_VEICOLO %>%
    left_join(TDI_SUM_PROXY_COMB[TDI_SUM_PROXY_COMB$FK_ID_PROXY == 2,], join_by(FK_ID_COMBUSTIBILE)) %>%
    left_join(TDI_PROXY_PERC_REG[TDI_PROXY_PERC_REG$FK_ID_PROXY == 2,], join_by(CODICE_COPART == FK_CODICE_COPART)) %>%
    right_join(TL_PARCO_REG, join_by(CODICE_COPART)) %>%
    left_join(TLD_CARBURANTI, join_by(FK_ID_COMBUSTIBILE)) %>%
    left_join(TLD_FATTORI_USURA_INQ, join_by(CODICE_COPART), relationship = "many-to-many") %>%
    left_join(TLD_FATTORI_EMISSIONI_FREDDO_INQ, join_by(CODICE_COPART,VELOCITA_EFFETTIVA == VELOCITA), relationship = "many-to-many") %>%
    filter(!is.na(TEMPERATURA )) %>%
    mutate(PERC_CONS = (as.numeric(CONSUMO_CARB_REG) * as.numeric(NUMERO_VEICOLI) * (as.numeric(PERCORRENZA_MEDIA_TOT) - as.numeric(PERCORRENZA_MEDIA_LIN)) * FCU / RESIDENTI_REGIONE * as.numeric(SOMMA_PROXY_INV) / FATTORE_CONSUMO)) %>%
    mutate_at(vars(starts_with("FE_") & !starts_with("FE_FREDDO_")),  ~. * PERC_CONS) %>%
    rename(VELOCITA = VELOCITA_EFFETTIVA) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,VELOCITA,TEMPERATURA) %>%
    summarise_at(vars(starts_with("FE_") & !starts_with("FE_FREDDO_")), ~ sum(.x, na.rm = TRUE))

  salvaTabella(TD_FATTORI_USURA_MEDI, "TD_FATTORI_USURA_MEDI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  TD_EMISSIONI_USURA <- ISTAT_COMUNI %>%
    filter(RESIDENTI>0) %>%
    left_join(TD_VELOCITA, join_by(FK_CLASSE_COMUNE_TD), relationship = "many-to-many") %>% 
    left_join(TLD_FERIALI_FESTIVI, join_by(FK_ID_STAGIONE_GIORNO == ID_STAGIONE_GIORNO )) %>% 
    left_join(TLD_TEMPERATURA, join_by(MESE, FK_ID_CLASSE_CLIMATICA )) %>% 
    mutate(TEMPERATURA = round(TEMP_MEDIA, digits = 0)) %>%
    mutate(VELOCITA = round(VELOCITA_MEDIA , digits = 0)) %>%
    left_join(TD_FATTORI_USURA_MEDI, join_by(FK_ID_SETTORE,VELOCITA,TEMPERATURA), relationship = "many-to-many") %>%
    filter(!is.na(FK_ID_COMBUSTIBILE )) %>%
    mutate_at(vars(starts_with("FE_")), list(EMI_ANNO = ~. * RESIDENTI  * FRAZIONE_PERCORRENZA * NR_GIORNI)) %>%
    group_by(ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    mutate_at(vars(starts_with("FE_")), ~ if_else(.x < 0, 0, .x) ) %>%
    rename_with(., ~ sub("FE_", "", .), starts_with("FE_")) %>%
    rename(FK_ISTAT_COMUNE = ISTAT_COMUNE ) %>%
    mutate(EMI_TIPO = "DU")
  
  salvaTabella(TD_EMISSIONI_USURA, "TD_EMISSIONI_USURA", tabelleDir)
  
  #### CALCOLO EMISSIONI EVAPORATIVE - DIFFUSE #### 
  
  TD_FATTORI_EVAPORATIVI_MOVIMENTO_MEDI <- TLD_TIPO_VEICOLO %>%
    left_join(TDI_SUM_PROXY_COMB[TDI_SUM_PROXY_COMB$FK_ID_PROXY == 2,], join_by(FK_ID_COMBUSTIBILE)) %>%
    left_join(TDI_PROXY_PERC_REG[TDI_PROXY_PERC_REG$FK_ID_PROXY == 2,], join_by(CODICE_COPART == FK_CODICE_COPART)) %>%
    right_join(TL_PARCO_REG, join_by(CODICE_COPART)) %>%
    left_join(TLD_CARBURANTI, join_by(FK_ID_COMBUSTIBILE)) %>%
    left_join(TLD_FATTORI_EVAPORATIVE_DIFFUSE_INQ, join_by(CODICE_COPART), relationship = "many-to-many") %>%
    mutate(TEMPERATURA = round(TEMP_MEDIA,digits = 0)) %>%
    left_join(TLD_FATTORI_EMISSIONI_FREDDO_INQ, join_by(CODICE_COPART,TEMPERATURA), relationship = "many-to-many") %>%
    filter(!is.na(TEMPERATURA )) %>%
    filter(FK_ID_COMBUSTIBILE == 10001 )   %>%
    mutate(PERC_CONS = (as.numeric(CONSUMO_CARB_REG) * as.numeric(NUMERO_VEICOLI) * (as.numeric(PERCORRENZA_MEDIA_TOT) - as.numeric(PERCORRENZA_MEDIA_LIN)) * FCU / RESIDENTI_REGIONE * as.numeric(SOMMA_PROXY_INV) / FATTORE_CONSUMO)) %>%
    mutate(FE_EVAPOR_MOVIM = FATTORE_EMISS_EVAPOR_MOVIM * PERC_CONS) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,VELOCITA,TEMPERATURA,FK_ID_CLASSE_CLIMATICA, MESE) %>%
    summarise(FE_EVAPOR_MOVIM = sum(FE_EVAPOR_MOVIM, na.rm = TRUE))
  
  salvaTabella(TD_FATTORI_EVAPORATIVI_MOVIMENTO_MEDI, "TD_FATTORI_EVAPORATIVI_MOVIMENTO_MEDI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  TD_EMISSIONI_EVAPORATIVE_MOVIMENTO <- ISTAT_COMUNI %>%
    filter(RESIDENTI>0) %>%
    left_join(TD_VELOCITA, join_by(FK_CLASSE_COMUNE_TD), relationship = "many-to-many") %>% 
    left_join(TLD_FERIALI_FESTIVI, join_by(FK_ID_STAGIONE_GIORNO == ID_STAGIONE_GIORNO )) %>% 
    left_join(TLD_TEMPERATURA, join_by(MESE, FK_ID_CLASSE_CLIMATICA )) %>% 
    mutate(VELOCITA = round(VELOCITA_MEDIA , digits = 0)) %>%
    left_join(TD_FATTORI_EVAPORATIVI_MOVIMENTO_MEDI, join_by(FK_ID_SETTORE,VELOCITA,FK_ID_CLASSE_CLIMATICA, MESE), relationship = "many-to-many") %>%
    filter(!is.na(FK_ID_COMBUSTIBILE )) %>%
    mutate(EMI_ANNO_EVAPOR_MOVIM = FE_EVAPOR_MOVIM * RESIDENTI * FRAZIONE_PERCORRENZA * NR_GIORNI) %>%
    group_by(ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise(EMI_ANNO_EVAPOR_MOVIM = sum(EMI_ANNO_EVAPOR_MOVIM, na.rm = TRUE))  %>%
    rename(FK_ISTAT_COMUNE = ISTAT_COMUNE ) 
  
  salvaTabella(TD_EMISSIONI_EVAPORATIVE_MOVIMENTO, "TD_EMISSIONI_EVAPORATIVE_MOVIMENTO", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  TD_FATTORI_EVAPORATIVI_ALTRE_MEDI <- TLD_TIPO_VEICOLO %>%
    right_join(TL_PARCO_REG, join_by(CODICE_COPART)) %>%
    left_join(TLD_FATTORI_EVAPORATIVE_DIFFUSE_INQ, join_by(CODICE_COPART), relationship = "many-to-many") %>%
    rename(TEMPERATURA = TEMP_MEDIA) %>%
    filter(FK_ID_COMBUSTIBILE == 10001 )   %>%
    mutate(PERC_CONS = (as.numeric(NUMERO_VEICOLI) / RESIDENTI_REGIONE)) %>%
    mutate(FE_EVAPOR_DIURNE = FATTORE_EMISS_DIURNE * PERC_CONS) %>%
    mutate(FE_EVAPOR_SOAK = FATTORE_EMISS_EVAPOR_SOAK * PERC_CONS) %>%
    group_by(FK_ID_SETTORE,FK_ID_COMBUSTIBILE,TEMPERATURA,FK_ID_CLASSE_CLIMATICA, MESE) %>%
    summarise(FE_EVAPOR_DIURNE = sum(FE_EVAPOR_DIURNE, na.rm = TRUE),
              FE_EVAPOR_SOAK = sum(FE_EVAPOR_SOAK, na.rm = TRUE))
  
  salvaTabella(TD_FATTORI_EVAPORATIVI_ALTRE_MEDI, "TD_FATTORI_EVAPORATIVI_ALTRE_MEDI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  TD_EMISSIONI_EVAPORATIVE_ALTRE <- ISTAT_COMUNI %>%
    filter(RESIDENTI>0) %>%
    left_join(TD_VELOCITA, join_by(FK_CLASSE_COMUNE_TD), relationship = "many-to-many") %>% 
    left_join(TLD_FERIALI_FESTIVI, join_by(FK_ID_STAGIONE_GIORNO == ID_STAGIONE_GIORNO )) %>% 
    # left_join(TLD_TEMPERATURA, join_by(MESE, FK_ID_CLASSE_CLIMATICA )) %>% 
    # mutate(VELOCITA = round(VELOCITA_MEDIA , digits = 0)) %>%
    left_join(TD_FATTORI_EVAPORATIVI_ALTRE_MEDI, join_by(FK_ID_SETTORE,FK_ID_CLASSE_CLIMATICA, MESE), relationship = "many-to-many") %>%
    filter(!is.na(FK_ID_COMBUSTIBILE )) %>%
    mutate(EMI_ANNO_EVAPOR_DIURNE = FE_EVAPOR_DIURNE * RESIDENTI * NR_GIORNI / 1000000) %>%
    mutate(EMI_ANNO_EVAPOR_SOAK = FE_EVAPOR_SOAK * RESIDENTI* NR_GIORNI / 1000000) %>%
    group_by(ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise(EMI_ANNO_EVAPOR_DIURNE = sum(EMI_ANNO_EVAPOR_DIURNE, na.rm = TRUE),
              EMI_ANNO_EVAPOR_SOAK = sum(EMI_ANNO_EVAPOR_SOAK, na.rm = TRUE))  %>%
    rename(FK_ISTAT_COMUNE = ISTAT_COMUNE ) 
  
  salvaTabella(TD_EMISSIONI_EVAPORATIVE_ALTRE, "TD_EMISSIONI_EVAPORATIVE_ALTRE", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  TD_EMISSIONI_EVAPORATIVE <- TD_EMISSIONI_EVAPORATIVE_MOVIMENTO %>%
    left_join(TD_EMISSIONI_EVAPORATIVE_ALTRE, join_by(FK_ISTAT_COMUNE, FK_ID_SETTORE, FK_ID_COMBUSTIBILE)) %>%
    mutate(SOV_EMI_ANNO = EMI_ANNO_EVAPOR_MOVIM + EMI_ANNO_EVAPOR_DIURNE + EMI_ANNO_EVAPOR_SOAK ) %>%
    dplyr::select(FK_ISTAT_COMUNE, FK_ID_SETTORE, FK_ID_COMBUSTIBILE, SOV_EMI_ANNO) %>%
    mutate(EMI_TIPO = "DE")
  
  # calcolo inquinanti derivati
  TD_EMISSIONI_EVAPORATIVE <- TD_EMISSIONI_EVAPORATIVE %>% 
    mutate(COV_EMI_ANNO = SOV_EMI_ANNO) 
  
  salvaTabella(TD_EMISSIONI_EVAPORATIVE, "TD_EMISSIONI_EVAPORATIVE", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  #### CALCOLO EMISSIONI TOTALI DIFFUSE #### 
  
  TD_EMISSIONI_TOTALI <- TD_EMISSIONI_TOTALI %>%
    filter(!FK_ISTAT_COMUNE %in% comuniSelezionati$PRO_COM) %>%
    bind_rows(TD_EMISSIONI_SCARICO) %>%
    bind_rows(TD_EMISSIONI_USURA) %>%
    bind_rows(TD_EMISSIONI_EVAPORATIVE) %>%
    bind_rows(TD_EMISSIONI_FREDDO)

  salvaTabella(TD_EMISSIONI_TOTALI, "TD_EMISSIONI_TOTALI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  
  return(TD_EMISSIONI_TOTALI)
  
}



calcolaRiduzioniEmissiveLineari <- function(scenario,setProgress,progressStart,progressEnd) {
  
  directory <- getScenarioDirectoryInDB(scenario)
  scenarioDir <- paste0(SCENDIR,"/",directory,"/")
  tabelleDir <- paste0(scenarioDir,"/","TABELLE","/")
  
  nSteps <- 3
  progressInc <- progressStart
  progressFile <- paste0(SCENDIR,"/",directory,"/","progress.txt")
  
  statusFile <- paste0(SCENDIR,"/",directory,"/","status.txt")
  write.table("Calcolo Emissioni Lineari",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  
  TL_EMISSIONI_TOTALI <- caricaTabellaOriginale("TL_EMISSIONI_TOTALI")

  emissioniFile <- paste0(SCENDIR,"/",directory,"/",directory,"_emissioni.csv")
  
  emissScen <- read.table(emissioniFile, sep = ";", dec = ".", header = TRUE) %>%
    dplyr::select(-all_of(PRECURSORS)) %>%
    pivot_longer(cols = starts_with(PRECURSORS), names_sep = "_", names_to = c("POLL",".value")) 
  
  emiss <- TL_EMISSIONI_TOTALI %>%
    filter(!is.na(TL_EMISSIONI_TOTALI$FK_ISTAT_COMUNE)) %>%
    rename_with(., ~ sub("_EMI_ANNO", "", .), ends_with("_EMI_ANNO")) %>%
    dplyr::select(ID_ARCO, FK_ISTAT_COMUNE, FK_ID_SETTORE, FK_ID_COMBUSTIBILE,EMI_TIPO,all_of(PRECURSORS)) %>%
    pivot_longer(cols = starts_with(PRECURSORS), names_to = "POLL", values_to = "EMI" )
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  TL_EMISSIONI_TOTALI <- emiss %>%
    left_join(emissScen, join_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,POLL)) %>%
    # mutate(NUMERO_VEICOLI = if_else(is.na(NEW_VEIC),NUMERO_VEICOLI,NEW_VEIC)) %>%
    mutate(EMI = if_else(is.na(VAR),EMI,VAR * EMI)) %>%
    dplyr::select(-c(VAR,ZONA,COMUNE,SET_NOME,ABBREVIAZIONE_COMBUST)) %>%
    pivot_wider(names_from = POLL, values_from = EMI, names_glue = "{POLL}_EMI_ANNO") 

  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  salvaTabella(TL_EMISSIONI_TOTALI, "TL_EMISSIONI_TOTALI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  return(TL_EMISSIONI_TOTALI)
  
  
}


calcolaRiduzioniEmissiveDiffuse <- function(scenario,setProgress,progressStart,progressEnd) {
  
  directory <- getScenarioDirectoryInDB(scenario)
  scenarioDir <- paste0(SCENDIR,"/",directory,"/")
  tabelleDir <- paste0(scenarioDir,"/","TABELLE","/")
  
  nSteps <- 3
  progressInc <- progressStart
  progressFile <- paste0(SCENDIR,"/",directory,"/","progress.txt")
  
  statusFile <- paste0(SCENDIR,"/",directory,"/","status.txt")
  write.table("Calcolo Emissioni Diffuse",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  
  TD_EMISSIONI_TOTALI <- caricaTabellaOriginale("TD_EMISSIONI_TOTALI")

  emissioniFile <- paste0(SCENDIR,"/",directory,"/",directory,"_emissioni.csv")
  
  emissScen <- read.table(emissioniFile, sep = ";", dec = ".", header = TRUE) %>%
    dplyr::select(-all_of(PRECURSORS)) %>%
    pivot_longer(cols = starts_with(PRECURSORS), names_sep = "_", names_to = c("POLL",".value")) 
  
  emiss <- TD_EMISSIONI_TOTALI %>%
    filter(!is.na(TD_EMISSIONI_TOTALI$FK_ISTAT_COMUNE)) %>%
    rename_with(., ~ sub("_EMI_ANNO", "", .), ends_with("_EMI_ANNO")) %>%
    dplyr::select(FK_ISTAT_COMUNE, FK_ID_SETTORE, FK_ID_COMBUSTIBILE,EMI_TIPO,all_of(PRECURSORS)) %>%
    pivot_longer(cols = starts_with(PRECURSORS), names_to = "POLL", values_to = "EMI" )
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  TD_EMISSIONI_TOTALI <- emiss %>%
    left_join(emissScen, join_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,POLL)) %>%
    # mutate(NUMERO_VEICOLI = if_else(is.na(NEW_VEIC),NUMERO_VEICOLI,NEW_VEIC)) %>%
    mutate(EMI = if_else(is.na(VAR),EMI,VAR * EMI)) %>%
    dplyr::select(-c(VAR,ZONA,COMUNE,SET_NOME,ABBREVIAZIONE_COMBUST)) %>%
    pivot_wider(names_from = POLL, values_from = EMI, names_glue = "{POLL}_EMI_ANNO") 
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  salvaTabella(TD_EMISSIONI_TOTALI, "TD_EMISSIONI_TOTALI", tabelleDir)
  
  progressInc <- progressInc + (progressEnd - progressStart) / nSteps
  if (setProgress) {
    write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  }
  
  return(TD_EMISSIONI_TOTALI)
  
}

calcolaEmissioniTotali <- function(scenario,tipoScenario,opzioni,setProgress,progressStart,progressEnd) {
  
  directory <- getScenarioDirectoryInDB(scenario)
  scenarioDir <- paste0(SCENDIR,"/",directory,"/")
  tabelleDir <- paste0(scenarioDir,"/","TABELLE","/")
  
  if (!dir.exists(tabelleDir)) dir.create(tabelleDir)
  
  
  if (tipoScenario == 1) {
    
    ## scenario di modifica del traffico su grafo stradale
    
    listaArchiModificati <- valutaModificheGrafo(scenario)
    opzioni <- NULL
    
    if (length(listaArchiModificati) == 0) {
      TL_EMISSIONI_TOTALI <- caricaTabellaOriginale("TL_EMISSIONI_TOTALI")
      salvaTabella(TL_EMISSIONI_TOTALI, "TL_EMISSIONI_TOTALI", tabelleDir)
    } else {
      TL_EMISSIONI_TOTALI <- calcolaEmissioniLineari(scenario,opzioni,setProgress,progressStart,progressEnd)
    }
    
    TD_EMISSIONI_TOTALI <- caricaTabellaOriginale("TD_EMISSIONI_TOTALI")
    salvaTabella(TD_EMISSIONI_TOTALI, "TD_EMISSIONI_TOTALI", tabelleDir)
    
  } else if (tipoScenario == 2) { 
    
    ## scenario di politica urbana
    
    if (is.null(opzioni)) {
      
      TL_EMISSIONI_TOTALI <- caricaTabellaOriginale("TL_EMISSIONI_TOTALI")
      salvaTabella(TL_EMISSIONI_TOTALI, "TL_EMISSIONI_TOTALI", tabelleDir)
      
      TD_EMISSIONI_TOTALI <- calcolaEmissioniDiffuse(scenario,setProgress,progressStart,progressEnd)
      
    } else {
      
      TL_EMISSIONI_TOTALI <- calcolaEmissioniLineari(scenario,opzioni,setProgress,progressStart,progressEnd/2)
      TD_EMISSIONI_TOTALI <- calcolaEmissioniDiffuse(scenario,setProgress,progressEnd/2,progressEnd)
      
    }
  
    
  } else {
    
    ## scenario di modifica delle emissioni comunali
    
    TL_EMISSIONI_TOTALI <- calcolaRiduzioniEmissiveLineari(scenario,setProgress,progressStart,progressEnd/2)
    TD_EMISSIONI_TOTALI <- calcolaRiduzioniEmissiveDiffuse(scenario,setProgress,progressEnd/2,progressEnd)
    
  }
  
  EMISSIONI_TOTALI <- TL_EMISSIONI_TOTALI %>%
    bind_rows(TD_EMISSIONI_TOTALI)
  
  salvaTabella(EMISSIONI_TOTALI, "EMISSIONI_TOTALI", tabelleDir)

}


calcolaEmissioniPerGrafici <- function(scenario) {
  
  EMI_TOTALI <- caricaTabellaOriginale("EMISSIONI_TOTALI") %>%
    group_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    rename_at(vars(ends_with("_ANNO")), ~ gsub("_EMI_ANNO", "", .x)) %>%
    pivot_longer(cols = -c(FK_ISTAT_COMUNE, FK_ID_SETTORE, FK_ID_COMBUSTIBILE), names_to = "POLL", values_to = "EMI") %>%
    mutate(FK_ID_SETTORE = as.character(FK_ID_SETTORE))
  
  EMI_TOTALI_SCENARIO <- caricaTabellaScenario(scenario, "EMISSIONI_TOTALI") %>%
    group_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    rename_at(vars(ends_with("_ANNO")), ~ gsub("_EMI_ANNO", "", .x)) %>%
    pivot_longer(cols = -c(FK_ISTAT_COMUNE, FK_ID_SETTORE, FK_ID_COMBUSTIBILE), names_to = "POLL", values_to = "EMI_SCEN") %>%
    mutate(FK_ID_SETTORE = as.character(FK_ID_SETTORE))
  
  EMI_DELTA <- EMI_TOTALI %>%
    left_join(EMI_TOTALI_SCENARIO, join_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,FK_ID_COMBUSTIBILE,POLL)) %>%
    mutate(DELTA = EMI_SCEN - EMI) %>%
    dplyr::select(-EMI, -EMI_SCEN) %>%
    pivot_wider(names_from = POLL, values_from = DELTA) 
  
  EMI_TOT <- EMI_TOTALI_SCENARIO %>%
    pivot_wider(names_from = POLL, values_from = EMI_SCEN)
  
  return(list(EMI_TOT,EMI_DELTA))
  
}

calcolaEmissioniTotaliPerComune <- function(scenario) {
  
  EMI_TOTALI <- caricaTabellaOriginale("EMISSIONI_TOTALI") %>%
    group_by(FK_ISTAT_COMUNE,FK_ID_SETTORE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    rename_at(vars(ends_with("_ANNO")), ~ gsub("_EMI_ANNO", "", .x)) %>%
    pivot_longer(cols = -c(FK_ISTAT_COMUNE, FK_ID_SETTORE), names_to = "POLL", values_to = "EMI") %>%
    mutate(FK_ID_SETTORE = as.character(FK_ID_SETTORE))
  
  EMI_TOTALIsumAllMs <- EMI_TOTALI %>% 
    group_by(FK_ISTAT_COMUNE, POLL) %>%
    summarise(EMI = sum(EMI, na.rm = T)) %>%
    mutate(FK_ID_SETTORE = "All")
  
  EMI_TOTALI <- EMI_TOTALI %>% 
    bind_rows(EMI_TOTALIsumAllMs)

  EMI_TOTALI_SCENARIO <- caricaTabellaScenario(scenario, "EMISSIONI_TOTALI") %>%
    group_by(FK_ISTAT_COMUNE,FK_ID_SETTORE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    rename_at(vars(ends_with("_ANNO")), ~ gsub("_EMI_ANNO", "", .x)) %>%
    pivot_longer(cols = -c(FK_ISTAT_COMUNE, FK_ID_SETTORE), names_to = "POLL", values_to = "EMI_SCEN") %>%
    mutate(FK_ID_SETTORE = as.character(FK_ID_SETTORE))
  
  EMI_TOTALI_SCENARIOsumAllMs <- EMI_TOTALI_SCENARIO %>% 
    group_by(FK_ISTAT_COMUNE, POLL) %>%
    summarise(EMI_SCEN = sum(EMI_SCEN, na.rm = T)) %>%
    mutate(FK_ID_SETTORE = "All")
  
  EMI_TOTALI_SCENARIO <- EMI_TOTALI_SCENARIO %>% 
    bind_rows(EMI_TOTALI_SCENARIOsumAllMs)
  
  EMI_DELTA <- EMI_TOTALI %>%
    left_join(EMI_TOTALI_SCENARIO, join_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,POLL)) %>%
    mutate(DELTA = round(EMI_SCEN - EMI, digits = 2)) %>%
    dplyr::select(-EMI, -EMI_SCEN) %>%
    pivot_wider(names_from = POLL, values_from = DELTA) 
    
  EMI_DELTA_PERC <- EMI_TOTALI %>%
    left_join(EMI_TOTALI_SCENARIO, join_by(FK_ISTAT_COMUNE,FK_ID_SETTORE,POLL)) %>%
    mutate(DELTA_PERC = if_else(EMI == 0, 0, round((EMI_SCEN - EMI) / EMI * 100 , digits = 2))) %>%
    dplyr::select(-EMI, -EMI_SCEN) %>%
    pivot_wider(names_from = POLL, values_from = DELTA_PERC) 
  
  EMI_TOT <- EMI_TOTALI_SCENARIO %>%
    pivot_wider(names_from = POLL, values_from = EMI_SCEN)
  
  return(list(EMI_TOT,EMI_DELTA,EMI_DELTA_PERC))
  
}


calcolaEmissioniTotaliPerGriglia <- function(scenario,setProgress,progressStart,progressEnd) {
  
  directory <- getScenarioDirectoryInDB(scenario)
  scenarioDir <- paste0(SCENDIR,"/",directory,"/")
  tabelleDir <- paste0(scenarioDir,"/","TABELLE","/")
  
  proxyLineari <- read.table(paste0(GRIDDIR, "/", "proxyLineare.csv"), sep = ";", dec = ".", header = TRUE)
  proxyDiffuse <- read.table(paste0(GRIDDIR, "/", "proxyDiffuse.csv"), sep = ";", dec = ".", header = TRUE)
  grigliaPoly <- read_sf(paste0(GRIDDIR, "/", "gridDomain.shp")) %>% st_drop_geometry()
  grigliaRaster <- raster(paste0(GRIDDIR, "/", "griglia.tiff"))
  
  nSteps <- length(PRECURSORS) * length(SECTORS)
  progressInc <- progressStart
  progressFile <- paste0(SCENDIR,"/",directory,"/","progress.txt")
  
  statusFile <- paste0(SCENDIR,"/",directory,"/","status.txt")
  write.table("Disaggrego le emissioni",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  
  
  EMISSIONI_GRIGLIATE <- caricaTabellaOriginale("EMISSIONI_GRIGLIATE")
  TL_EMISSIONI_TOTALI <- caricaTabellaScenario(scenario,"TL_EMISSIONI_TOTALI")
  TD_EMISSIONI_TOTALI <- caricaTabellaScenario(scenario,"TD_EMISSIONI_TOTALI")
  
  TL_EMISSIONI_GRIGLIATE <- TL_EMISSIONI_TOTALI %>%
    filter(!is.na(FK_ID_COMBUSTIBILE)) %>%
    filter(EMI_TIPO %in% c("LSC","LU","LE")) %>%
    group_by(ID_ARCO,FK_ID_SETTORE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    left_join(proxyLineari, join_by("ID_ARCO"), relationship = "many-to-many") %>%
    mutate_at(vars(ends_with("_ANNO")),  ~. * PROXY) %>%
    group_by(ID_CELLA,FK_ID_SETTORE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE))
  
  salvaTabella(TL_EMISSIONI_GRIGLIATE, "TL_EMISSIONI_GRIGLIATE", tabelleDir)
  
  TD_EMISSIONI_GRIGLIATE <- TL_EMISSIONI_TOTALI[TL_EMISSIONI_TOTALI$EMI_TIPO == "LSF",] %>%
    bind_rows(TD_EMISSIONI_TOTALI) %>%
    filter(!is.na(FK_ID_COMBUSTIBILE)) %>%
    group_by(FK_ISTAT_COMUNE,FK_ID_SETTORE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    left_join(proxyDiffuse, join_by(FK_ISTAT_COMUNE == ISTAT_COMUNE), relationship = "many-to-many") %>%
    mutate_at(vars(ends_with("_ANNO")),  ~. * PROXY) %>%
    group_by(ID_CELLA,FK_ID_SETTORE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE))
  
  salvaTabella(TD_EMISSIONI_GRIGLIATE, "TD_EMISSIONI_GRIGLIATE", tabelleDir)
  
  EMISSIONI_GRIGLIATE_SCEN <- TL_EMISSIONI_GRIGLIATE %>%
    bind_rows(TD_EMISSIONI_GRIGLIATE) %>%
    group_by(ID_CELLA,FK_ID_SETTORE) %>%
    summarise_at(vars(ends_with("_ANNO")), ~ sum(.x, na.rm = TRUE)) %>%
    rename_at(vars(ends_with("_ANNO")), ~ gsub("_EMI_ANNO", "", .x)) %>%
    filter(!(is.na(ID_CELLA)))
  
  salvaTabella(EMISSIONI_GRIGLIATE_SCEN, "EMISSIONI_GRIGLIATE", tabelleDir)
  
  EMISSIONI_GRIGLIATE_SCEN <- EMISSIONI_GRIGLIATE_SCEN %>%
    left_join(grigliaPoly, join_by(ID_CELLA)) %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  EMISSIONI_GRIGLIATE <- EMISSIONI_GRIGLIATE %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) 
  
  abs <- raster::stack()
  delta <- raster::stack()
  deltaP <- raster::stack()
  
  sumAbs <- grigliaRaster
  sumDelta <- grigliaRaster
  sumBase <- grigliaRaster
  
  for (poll in PRECURSORS) {
    
    values(sumAbs) <- 0
    values(sumDelta) <- 0
    values(sumBase) <- 0
    
    for (sector in 1:(length(SECTORS))) {
      
      scen_point <- EMISSIONI_GRIGLIATE_SCEN[EMISSIONI_GRIGLIATE_SCEN$FK_ID_SETTORE == SECTORS_ID[sector],poll]
      base_point <- EMISSIONI_GRIGLIATE[EMISSIONI_GRIGLIATE$FK_ID_SETTORE == SECTORS_ID[sector],poll]
      
      scen <- rasterize(scen_point, grigliaRaster, field = poll, na.rm = TRUE)
      base <- rasterize(base_point, grigliaRaster, field = poll, na.rm = TRUE)
      
      deltaEmi <- scen - base
      deltaPerc <- deltaEmi / base
      
      abs <- addToStack(scen, abs)
      delta <- addToStack(deltaEmi, delta)
      deltaP <- addToStack(deltaPerc, deltaP)
      
      sumBase <- calc(stack(sumBase,base), fun=sum, na.rm = TRUE)
      sumAbs <- calc(stack(sumAbs,scen), fun=sum, na.rm = TRUE)
      sumDelta <- calc(stack(sumDelta,deltaEmi), fun=sum, na.rm = TRUE)
      
      progressInc <- progressInc + (progressEnd - progressStart) / nSteps
      if (setProgress) {
        write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
      }
      
    }
    
    sumPerc <- sumDelta / sumBase 
    
    abs <- addToStack(sumAbs, abs)
    delta <- addToStack(sumDelta, delta)
    deltaP <- addToStack(sumPerc, deltaP)
    
    
  }
  
  finalStack <- addLayer(abs, delta, deltaP * 100)
  finalStack <- round(finalStack, digits = 2)
  
  combNames <- paste0(rep(PRECURSORS, each = (length(SECTORS) + 1)), "_Sett",c(SECTORS_ID,"All"))
  names(finalStack) <- paste0(rep(c("","delta_","deltaP_"), each = length(combNames)), combNames)
  
  return(finalStack)
  
}


calcolaConcentrazioni <- function(scenario,emissionsStack,setProgress,progressStart,progressEnd) {
  
  print(paste0(Sys.time(), " - Lancio computeConcentrations per lo scenario: ", scenario))
  
  final <- stack()
  delta <- stack()
  deltaP <- stack()
  
  directory <- getScenarioDirectoryInDB(scenario)
  progressInc <- progressStart
  progressFile <- paste0(SCENDIR,"/",directory,"/","progress.txt")
  
  statusFile <- paste0(SCENDIR,"/",directory,"/","status.txt")
  write.table("Calcolo le concentrazioni",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)

  for (SRpoll in SR_POLLUTANT) {
    
    if (SRpoll == "NO2") {
      
      SRfunctionFile <- paste0(SRFUNC,"/","SR_NO2.nc")
      BCConcFile <- paste0(SRFUNC,"/","BC_conc_NO2.nc")
      prec <- -emissionsStack[[c("delta_NO2_SettAll")]]
      PrecToBeUsed <- c(1)
      
    } else if (SRpoll == "PM10") {
      
      SRfunctionFile <- paste0(SRFUNC,"/","SR_PM10.nc")
      BCConcFile <- paste0(SRFUNC,"/","BC_conc_PM10.nc")
      prec <- -emissionsStack[[c("delta_NO2_SettAll", "delta_COV_SettAll", "delta_NH3_SettAll", "delta_PM10_SettAll", "delta_SO2_SettAll")]]
      PrecToBeUsed <- c(1,2,3,4,5)
      
    } else if (SRpoll == "PM2.5") {

      SRfunctionFile <- paste0(SRFUNC,"/","SR_PM25.nc")
      BCConcFile <- paste0(SRFUNC,"/","BC_conc_PM25.nc")
      prec <- -emissionsStack[[c("delta_NO2_SettAll", "delta_COV_SettAll", "delta_NH3_SettAll", "delta_PM2.5_SettAll", "delta_SO2_SettAll")]]
      PrecToBeUsed <- c(1,2,3,4,5)

    }
    
    # calcolo la densità emissiva
    prec <- prec/area(prec)
    
    alpha <- brick(SRfunctionFile, var = "alpha")
    omega <- brick(SRfunctionFile, var = "omega")
    
    alpha@file@nodatavalue <- 0
    omega@file@nodatavalue <- 0
    
    BCconc <- raster(BCConcFile, var = "conc")
    # extent(BCconc) <- extent(prec[[1]])
    
    ratio <- 1
    
    radius <- readGlobalAttNetCDF(SRfunctionFile, "Radius_of_influence")
    # lat <- rev(readVarNetCDF(SRfunctionFile, var = "lat"))
    # lon <- readVarNetCDF(SRfunctionFile, var = "lon")
    
    nRows <- alpha@nrows
    nCols <- alpha@ncols
    dimrad <- radius*2+1
    X <- matrix(rep(-radius:radius, dimrad), nrow = dimrad, ncol = dimrad, byrow = T)
    Y <- matrix(rep(-radius:radius, dimrad), nrow = dimrad, ncol = dimrad)
    Ftmp <- 1 / ((1 + ((X / ratio) ** 2 + Y ** 2) ** 0.5))
    F <- array(0, c(dimrad,dimrad,length(PrecToBeUsed)))
    
    ###### CALCOLO
    
    prec2 <- extend(prec[[PrecToBeUsed]], c(radius,radius), value = 0)
    
    output <- BCconc
    values(output) <- 0
    
    # print(paste0(Sys.time(), " - Start"))
    for (ic in 1:nCols) {
      
      for (ir in 1:nRows) {
        
        PrecDummyQuad <- as.array(prec2)[ir:(ir+radius*2),ic:(ic+radius*2),]
        if (sum(PrecDummyQuad) == 0) next 
        
        coeff <- as.vector(omega[ir,ic,PrecToBeUsed])
        
        for (poll in 1:length(PrecToBeUsed)) {
          
          F[,,poll] <- Ftmp^coeff[poll]
          
        }
        
        if (length(PrecToBeUsed)==1) {
          
          PrecPatch <- sum(PrecDummyQuad * F[,,])
          
        } else {
          
          PrecDummyQuad <- as.array(prec2)[ir:(ir+radius*2),ic:(ic+radius*2),]
          PrecPatch <- colSums(PrecDummyQuad * F, dims = 2)
          
        }
        
        output[ir,ic] <- sum(alpha[[PrecToBeUsed]][ir,ic] * PrecPatch) 
        
      }
      
      progressInc <- progressInc + (progressEnd - progressStart) / (nCols * length(SR_POLLUTANT))
      # print(progressInc)
      if (setProgress) {
        write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
        # progress$set(progressInc/100, detail = paste(round(progressInc,1),"%"))	
      }
      
    }
    # print(paste0(Sys.time(), " - End"))
    
    finalConc <- BCconc - output
    deltaConc <- - output
    deltaPerc <- - output / BCconc
    
    final <- addToStack(finalConc, final)
    delta <- addToStack(deltaConc, delta)
    deltaP <- addToStack(deltaPerc, deltaP)
    
  }
  
  final <- addLayer(final, delta, deltaP * 100)
  final <- round(final, digits = 2)
  
  names(final) <- c(SR_POLLUTANT,paste0("delta_",SR_POLLUTANT),paste0("deltaP_",SR_POLLUTANT))
  return(final)
  
}


calcolaConcentrazioniML <- function(scenario,emissionsStack,setProgress,progressStart,progressEnd) {
  
  print(paste0(Sys.time(), " - Lancio computeConcentrations per lo scenario: ", scenario))
  
  final <- stack()
  delta <- stack()
  deltaP <- stack()
  
  directory <- getScenarioDirectoryInDB(scenario)
  progressInc <- progressStart
  progressFile <- paste0(SCENDIR,"/",directory,"/","progress.txt")
  
  statusFile <- paste0(SCENDIR,"/",directory,"/","status.txt")
  write.table("Calcolo le concentrazioni",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
  
  prec <- -emissionsStack[[c("delta_NO2_SettAll", "delta_NH3_SettAll", "delta_COV_SettAll", "delta_SO2_SettAll", "delta_PM10_SettAll", "delta_PM2.5_SettAll")]]
  # calcolo la densità emissiva
  prec <- prec/area(prec)
  
  configMLfile <- paste0(MLMODELS,"/","configML.json")
  configML <- fromJSON(configMLfile)
  
  
  for (SRpoll in SR_POLLUTANT) {
    
    if (SRpoll == "NO2") {
      
      MLmodelFile <- paste0(MLMODELS,"/",configML$NO2$model)
      BCConcFile <- paste0(MLMODELS,"/",configML$NO2$bcConc)
      window <- configML$NO2$window
      PrecToBeUsed <- c(1) # NOx
      
    } else if (SRpoll == "PM10") {
      
      MLmodelFile <- paste0(MLMODELS,"/",configML$PM10$model)
      BCConcFile <- paste0(MLMODELS,"/",configML$PM10$bcConc)
      window <- configML$PM10$window
      PrecToBeUsed <- c(1,2,3,4,5) # NOx, NH3, NMVOC, SOx, PM10
      
    } else if (SRpoll == "PM2.5") {
      
      MLmodelFile <- paste0(MLMODELS,"/",configML$PM2.5$model)
      BCConcFile <- paste0(MLMODELS,"/",configML$PM2.5$bcConc)
      window <- configML$PM2.5$window
      PrecToBeUsed <- c(1,2,3,4,6) # NOx, NH3, NMVOC, SOx, PM25
      
    }
    
    
    BCconc <- raster(BCConcFile, var = "conc")
    
    half_window <- floor(window/2)
    prec2 <- extend(prec[[PrecToBeUsed]], c(half_window,half_window), value = 0)
    
    prec2.df <- as.data.frame(prec2, xy = TRUE)
    dati <- prec2.df[order(prec2.df$y, prec2.df$x, decreasing = c(FALSE, FALSE)),]
    
    nrow <- length(unique(dati$y))
    ncol <- length(unique(dati$x))
    nprec <- length(PrecToBeUsed)
    
    t <- list()
    
    lon_coords <- matrix(as.numeric(unlist(dati[,"x"])), nrow = nrow, ncol = ncol, byrow = TRUE)
    lat_coords <- matrix(as.numeric(unlist(dati[,"y"])), nrow = nrow, ncol = ncol, byrow = TRUE)
    
    for (z in 1:nprec) {
      t[[z]] <- matrix(as.numeric(unlist(dati[,z+2])), nrow = nrow, ncol = ncol, byrow = TRUE)
    }
    
    dati_in_maps <- array(unlist(t), c(nrow,ncol,nprec))
    X <- array(0, dim=c((nrow-(half_window*2))*(ncol-(half_window*2)),window,window,nprec))
    
    
    lon <- c()
    lat <- c()
    
    index <- 1
    
    for (i in (half_window+1):(nrow-half_window)) {
      
      for (j in (half_window+1):(ncol-half_window)) {
        
        patch_list <- dati_in_maps[(i - half_window):(i + half_window), (j - half_window):(j + half_window), 1:nprec]
        X[index,,,] <- patch_list
        
        lon[index] <- lon_coords[i,j]
        lat[index] <- lat_coords[i,j]
        
        index <- index + 1
        
      }
      
    }
    
    model <- load_model(MLmodelFile)
    conc.df <- data.frame(conc = predict(model, X),
                          y = lat,
                          x = lon)
    
    conc.point <- conc.df %>%
      st_as_sf(coords = c("x", "y"), crs = 4326)
    
    output <- rasterize(conc.point, BCconc, field = "conc", na.rm = TRUE)
    
    finalConc <- BCconc - output
    deltaConc <- - output
    deltaPerc <- - output / BCconc
    
    final <- addToStack(finalConc, final)
    delta <- addToStack(deltaConc, delta)
    deltaP <- addToStack(deltaPerc, deltaP)
    
    progressInc <- progressInc + (progressEnd - progressStart) / (length(SR_POLLUTANT))
    if (setProgress) {
      write.table(progressInc,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
    }
    
    
  }
  
  final <- addLayer(final, delta, deltaP * 100)
  final <- round(final, digits = 2)
  
  names(final) <- c(SR_POLLUTANT,paste0("delta_",SR_POLLUTANT),paste0("deltaP_",SR_POLLUTANT))
  return(final)
  
}


addToStack <- function(rasterName,stackName) {
  # funzione che aggiunge un layer ad un raster stack
  # se il raster stack non esiste, lo crea
  
  if (nlayers(stackName) == 0) {
    stackName <- stack(rasterName)
  } else {
    stackName <- addLayer(stackName, rasterName)
  }
  
  return(stackName)
  
}


creaDatiGraficiEmissioni <- function(emissionsList, ScenarioName) {
  
  print(paste0(Sys.time(), " - Lancio createEmiPlotData per lo scenario: ", ScenarioName)) 
  
  ISTAT_COMUNI <- caricaTabellaOriginale("ISTAT_COMUNI")
  
  x <- emissionsList[[1]] %>%
    group_by(FK_ISTAT_COMUNE) %>%
    summarise_at(PRECURSORS, ~ sum(.x, na.rm = TRUE)) %>%
    left_join(ISTAT_COMUNI, join_by(FK_ISTAT_COMUNE == ISTAT_COMUNE)) %>%
    dplyr::select(COMUNE, all_of(PRECURSORS)) %>%
    pivot_longer(cols = PRECURSORS, names_to = "varName", values_to = "value") %>%
    filter(!is.na(COMUNE))
  
  xAll <- emissionsList[[1]] %>%
    ungroup() %>%
    dplyr::select(all_of(PRECURSORS)) %>%
    summarise_all(sum, na.rm = TRUE) %>% 
    mutate(COMUNE = "All") %>%
    pivot_longer(cols = PRECURSORS, names_to = "varName", values_to = "value") %>%
    bind_rows(x) %>%
    mutate(value = round(value, digits = 1))
  
  x <- cbind(category = ScenarioName, xAll)

  dat1 <- x %>% 
    group_by(category,COMUNE) %>%  
    do(data = list_parse2(data.frame(.$varName, .$value))) %>%
    ungroup() %>%
    rename(name = category)
  
  dat1$absDel <- "absolute"
  
  x <- emissionsList[[2]] %>%
    group_by(FK_ISTAT_COMUNE) %>%
    summarise_at(PRECURSORS, ~ sum(.x, na.rm = TRUE)) %>%
    left_join(ISTAT_COMUNI, join_by(FK_ISTAT_COMUNE == ISTAT_COMUNE)) %>%
    dplyr::select(COMUNE, all_of(PRECURSORS)) %>%
    pivot_longer(cols = PRECURSORS, names_to = "varName", values_to = "value") %>%
    filter(!is.na(COMUNE)) 
  
  xAll <- emissionsList[[2]] %>%
    ungroup() %>%
    dplyr::select(all_of(PRECURSORS)) %>%
    summarise_all(sum, na.rm = TRUE) %>% 
    mutate(COMUNE = "All") %>%
    pivot_longer(cols = PRECURSORS, names_to = "varName", values_to = "value") %>%
    bind_rows(x) %>%
    mutate(value = round(value, digits = 1))
  
  x <- cbind(category = ScenarioName, xAll)
  
  dat2 <- x %>% 
    group_by(category,COMUNE) %>%  
    do(data = list_parse2(data.frame(.$varName, .$value))) %>%
    ungroup() %>%
    rename(name = category)
  
  dat2$absDel <- "delta"

  dat <- rbind(dat1,dat2)
  dat$type <- "column"
  
  return(dat)
  
}


interpolaRaster <- function(rasterStack, fun, setProgress, progressStart, progressEnd) {
  
  export <- list()
  shape <- comuni[[1]]
  
  deltaVarNames <- names(rasterStack)[startsWith(names(rasterStack), "delta_")]
  deltaPVarNames <- names(rasterStack)[startsWith(names(rasterStack), "deltaP_")]
  varNames <- names(rasterStack)[names(rasterStack) %!in% c(deltaVarNames, deltaPVarNames)]
  
  computePerc <- TRUE
  
  # export <- lapply(1:length(shp_names), function(v) exact_extract(rasterStack, shp[[v]], 'mean', progress = FALSE))
  export[[1]] <- exact_extract(rasterStack, shape, fun, progress = FALSE, append_cols = c('COMUNE'))  %>%
    rename('NAME' = 'COMUNE') %>% 
    rename_with(~ gsub(paste0(fun,"."), "", .x, fixed = TRUE)) %>% 
    mutate(across(where(is.numeric), round, 1))
  
  if (computePerc) {
    
    export[[1]][,deltaPVarNames] <- export[[1]][,deltaVarNames] / (export[[1]][,varNames] - export[[1]][,deltaVarNames]) * 100
    
  } 
  
  return(export)
  
}

creaDatiGraficiConcentrazioni <- function(rasterStack, ScenarioName) {
  
  # shpIta <- st_transform(shp[[3]], crs(rasterStack))
  # rasterStack <- mask(rasterStack, shpIta)
  shape <- comuni[[1]]
  
  deltaVarNames <- names(rasterStack)[startsWith(names(rasterStack), "delta_")]
  deltaPVarNames <- names(rasterStack)[startsWith(names(rasterStack), "deltaP_")]
  varNames <- names(rasterStack)[names(rasterStack) %!in% c(deltaVarNames, deltaPVarNames)]
  
  extracted_values_tot <- exact_extract(rasterStack[[varNames]], st_union(shape), progress = FALSE)
  extracted_values_tot <- list(cbind(COMUNE = "All",extracted_values_tot[[1]]))
  extracted_values_com <- exact_extract(rasterStack[[varNames]], shape, progress = FALSE, include_cols = "COMUNE")
  
  extracted_values <- c(extracted_values_tot,extracted_values_com)

  x <- data.frame(
    values = as.vector(round(sapply(varNames, FUN = function(x) unlist(lapply(extracted_values, '[[', x))), digits = 1)),  # Extracted values from raster
    varName = rep(varNames, each = sum(sapply(extracted_values, nrow))),
    polygon_id = rep(rep(1:length(extracted_values), times = sapply(extracted_values, nrow)), length(varNames)),
    comune = rep(unlist(lapply(extracted_values, '[[', "COMUNE")), times = length(varNames))
  )
  
  dat1 <- data_to_boxplot(x, values, varName, comune, name = ScenarioName)
  dat1$absDel <- "absolute"
  

  extracted_values_tot <- exact_extract(rasterStack[[deltaVarNames]], st_union(shape), progress = FALSE)
  extracted_values_tot <- list(cbind(COMUNE = "All",extracted_values_tot[[1]]))
  extracted_values_com <- exact_extract(rasterStack[[deltaVarNames]], shape, progress = FALSE, include_cols = "COMUNE")
  
  extracted_values <- c(extracted_values_tot,extracted_values_com)
  
  x <- data.frame(
    values = as.vector(round(sapply(deltaVarNames, FUN = function(x) unlist(lapply(extracted_values, '[[', x))), digits = 1)),  # Extracted values from raster
    varName = rep(varNames, each = sum(sapply(extracted_values, nrow))),
    polygon_id = rep(rep(1:length(extracted_values), times = sapply(extracted_values, nrow)), length(varNames)),
    comune = rep(unlist(lapply(extracted_values, '[[', "COMUNE")), times = length(varNames))
  )
  
  dat2 <- data_to_boxplot(x, values, varName, comune, name = ScenarioName)
  dat2$absDel <- "delta"

  dat <- rbind(dat1,dat2)
  
  return(dat)
  
}

updateTrafficMap <- function(mapName,pal,labels,shapefile,values,minMaxVal,title) {

  leaflet::leafletProxy(mapId = mapName, data = shapefile ) %>%
    leaflet.extras2::addSpinner()  %>%
    leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 15)) %>%
    clearGroup("selectedLines") %>%
    clearGroup("trafficLines") %>%
    clearControls() %>%
    clearPopups() %>%
    addPolylines(
      # color = "#03F",
      color = ~pal(values),
      weight = 3,
      smoothFactor = 0.5,
      opacity = 0.5,
      fillOpacity = 0.5,
      layerId = traffic$ID,
      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      group = "trafficLines"
    ) %>%
    # Add the legend
    addLegend(
      "bottomright",
      pal = pal,
      values = minMaxVal,
      title = title,
      opacity = 1
    ) %>%
  leaflet.extras2::stopSpinner()
  
} 

updatePolyMap <- function(mapName,pal,labels,shapefile,values,minMaxVal,title,classSize,classToHide) {

  leafletProxy(mapName, data = shapefile) %>%
    leaflet.extras2::addSpinner()  %>%
    leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 15)) %>%
    clearShapes() %>%
    clearControls() %>%
    clearImages() %>%
    clearPopups() %>%
    addPolygons(
      group = "dati",
      fillColor = ~pal(values),
      weight = 0.5,
      opacity = 1,
      color = "grey",
      dashArray = "3",
      fillOpacity = 1,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      # layerId=~shpConc$NAME
    ) %>%
    addLegend(pal = pal, values = minMaxVal, opacity = 0.7, title = title, position = "bottomright")	%>%
    addEasyprint(options = easyprintOptions(
      # title = 'Print map',
      sizeModes = list("Custom Size"=list(
        width= 800,
        height= 800,
        name = "A custom landscape size tooltip",
        className = classSize)
      ),
      # position = 'topleft',
      hidden = TRUE,
      hideControlContainer = FALSE,
      hideClasses = classToHide,
      tileWait = 2000,
      exportOnly = TRUE)) %>%
    leaflet.extras2::stopSpinner() 
  
} 

updateRasterMap <- function(mapName,pal,rasterfile,values,minMaxVal,title,classSize,classToHide) {
  
  leafletProxy(mapName,data = st_union(comuni[[1]])) %>%
    leaflet.extras2::addSpinner()  %>%
    leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 15)) %>%
    clearShapes() %>%
    clearControls() %>%
    clearImages() %>%
    clearPopups() %>%
    clearTiles() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Positron (minimal)", providerTileOptions(zIndex=-100)) %>%
    addProviderTiles("CartoDB.Voyager", group = "Carto Voyager", providerTileOptions(zIndex=-100)) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)", providerTileOptions(zIndex=-100)) %>%
    addPolygons(weight = 3,
                color = "yellow",
                fill = FALSE,
                options = pathOptions(interactive = FALSE)) %>%
    addRasterImage(rasterfile, 
                   colors = pal, 
                   opacity = 0.7,
                   options = gridOptions(zIndex=100),
                   method = "bilinear") %>%
    addLegend(pal = pal, values = minMaxVal, opacity = 0.7, title = title, position = "bottomright")	%>%
    addEasyprint(options = easyprintOptions(
      # title = 'Print map',
      sizeModes = list("Custom Size"=list(
        width= 800,
        height= 800,
        name = "A custom landscape size tooltip",
        className = classSize)
      ),
      # position = 'topleft',
      hidden = TRUE,
      hideControlContainer = FALSE,
      hideClasses = classToHide,
      tileWait = 2000,
      exportOnly = TRUE)) %>%
    # Layers control
    addLayersControl(
      baseGroups = c(
        "OpenStreetMap",
        "Positron (minimal)",
        "Carto Voyager",
        "World Imagery (satellite)"
      ),
      position = c("topleft"),
      options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)
    ) %>%
    leaflet.extras2::stopSpinner() 
  
}

createBoxPlotData <- function(rasterStack, ScenarioName) {
  
  # shpIta <- st_transform(shp[[3]], crs(rasterStack))
  # rasterStack <- mask(rasterStack, shpIta)
  
  deltaVarNames <- names(rasterStack)[startsWith(names(rasterStack), "delta_")]
  deltaPVarNames <- names(rasterStack)[startsWith(names(rasterStack), "deltaP_")]
  varNames <- names(rasterStack)[names(rasterStack) %!in% c(deltaVarNames, deltaPVarNames)]
  
  x <- data.frame(raster::values(rasterStack)) %>%
    pivot_longer(all_of(varNames) ,names_to = "varName", values_to = "value") %>% 
    dplyr::select(c("varName","value")) 
  x <- x[!is.na(x$value),]
  
  dat1 <- data_to_boxplot(x, value, varName, name = ScenarioName)
  dat1$absDel <- "absolute"
  
  x <- data.frame(raster::values(rasterStack)) %>%
    pivot_longer(all_of(deltaVarNames) ,names_to = "varName", values_to = "value") %>% 
    dplyr::select(c("varName","value"))
  x <- x[!is.na(x$value),]
  x$varName <- str_replace(x$varName,"delta_","")
  
  dat2 <- data_to_boxplot(x, value, varName, name = ScenarioName)
  dat2$absDel <- "delta"
  
  dat <- rbind(dat1,dat2)
  
  return(dat)
  
}



modalDialogFunction <- function(failed, text) {
  
  if (failed) {
    
    style <- "color: red;"
    title <- "Errore"
    
  } else  {
    
    style <- "color: green;"
    title = "Ok"
    
  }
  
  modalDialog(
    title = title,
    div(tags$b(text, style = style)),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Close"),
    )
  )
  
}


# messaggio per check file input
sendAlert <- function(text) {
  
  showModal(
    modalDialogFunction(TRUE, text)
  )
  
}

# messaggio per reset password
resetPswModal <- function(failed, username) {
  
  if (is.null(failed)) {
    
    modalDialog(
      title = "Confirm",
      # paste0("Are you sure, you want to reset password for user ", username, "?\nAn email will be sent to the user!"),
      span("Are you sure, you want to reset password for user ", tags$b(username), "?"),
      div(tags$i("An email will be sent to the user!", style = "color: grey;")),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("resetPswUser", "OK")
      )
    )
    
  } else if (failed) {
    
    modalDialog(
      title = "Error",
      div(tags$b("Error: mail not sent!", style = "color: red;")),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
      )
    )
    
  } else  {
    
    modalDialog(
      title = "Great",
      div(tags$b("Mail sent successfully!", style = "color: green;")),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
      )
    )
    
  }
}

# messaggio per nuovo utente
newUserModal <- function(failed) {
  
  if (is.null(failed)) {
    
    modalDialog(
      title = "Create new user",
      textInput(inputId = "UserName",label = "Username"),
      textInput(inputId = "FirstName",label = "First Name"),
      textInput(inputId = "LastName",label = "Last Name"),
      textInput(inputId = "Email",label = "Email"),
      selectInput(inputId = "Group",label = "Group",choices = c("ADMIN","USER")),
      div(tags$i("An email will be sent to the user!", style = "color: grey;")),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveNewUser", "Save")
      )
    )
    
  } else if (failed) {
    
    modalDialogFunction(failed, "Errore nell'invio della mail!")
    
  } else  {
    
    modalDialogFunction(failed, "Mail inviata correttamente!")
    
  }
}

# messaggio per check upload configurazione
checkUploadModal <- function(failed) {

  if (failed) {
    
    modalDialogFunction(failed, "Si è verificato un errore nel caricamento della configurazione!")
    
  } else  {
    
    modalDialogFunction(failed, "Configurazione caricata correttamente!")
    
  }
  
}
