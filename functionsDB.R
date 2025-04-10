
updateDB <- function(scenarioDF) {
	
	scenarioDF <- scenarioDF[,c("scenarioName","directory", "description", "dateCreation", "userName", "progress", "status")]
	write.csv2(scenarioDF, paste0(SCENDIR,"/userScenario.csv"), row.names = FALSE, quote = TRUE)
	
}


initializeDB <- function() {
  
  print(paste0(Sys.time()," - Inizializzo file degli scenari"))
	
	if (!file.exists(paste0(SCENDIR,"/userScenario.csv"))) {
		
		userScenario <- data.frame(scenarioName = character(0),
		                           directory = character(0),
															 description = character(0),
															 dateCreation = character(0),
															 userName = character(0),
															 progress = character(0),
															 status = character(0))
		
		updateDB(userScenario)
		
	} else {
	  
	  updateProgressScenario()
	  updateStatusScenario()
	  
	  
	}
	
}

readScenarioFromDB <- function(userName, userGroup) {
  
  userScenario <- read.csv2(paste0(SCENDIR,"/userScenario.csv"))
  
  if (missing(userName) & missing(userGroup)) {
    
  } else if (userGroup != "ADMIN") {
    
  	userScenario <- userScenario[userScenario$userName == userName,]

  }
  
  if (nrow(userScenario)>0) {
   
    userScenario$UpdateBtn <- TRUE
    userScenario$DownloadBtn <- TRUE
    userScenario$DeleteBtn <- TRUE
    
    
  }
	
	return(userScenario)
	
}

scenarioListfromDB <- function(userName, userGroup) {
	
	scenarioDF <- readScenarioFromDB(userName, userGroup)
	scenarioDF <- scenarioDF[scenarioDF$status == "Ok",]
	return(scenarioDF$scenarioName)
	
}


scenarioListAllfromDB <- function(userName, userGroup) {
  
  scenarioDF <- readScenarioFromDB(userName, userGroup)
  return(scenarioDF$scenarioName)
  
}

getScenarioDescriptionInDB <- function(scenarioNameIN) {
	
  print(paste0(Sys.time()," - Recupero descrizione dello scenario: ", scenarioNameIN))
	scenarioDF <- readScenarioFromDB()
	desc <- scenarioDF$description[scenarioDF$scenarioName == scenarioNameIN]
	
	return(desc)
	
}

getScenarioDirectoryInDB <- function(scenarioNameIN) {
  
  print(paste0(Sys.time()," - Recupero la directory dello scenario: ", scenarioNameIN))
  scenarioDF <- readScenarioFromDB()
  desc <- scenarioDF$directory[scenarioDF$scenarioName %in% scenarioNameIN]
  
  return(desc)
  
}

getScenarioProgressInDB <- function(scenarioNameIN) {
  
  scenarioDF <- readScenarioFromDB()
  
  if (missing(scenarioNameIN)) {
    
    return(scenarioDF$progress)
    
  } else {
    
    progr <- scenarioDF$progress[scenarioDF$scenarioName == scenarioNameIN]  
    return(progr)
    
  }
  
}

updateProgressScenario <- function() { 
  
  scenarioDF <- readScenarioFromDB()
  
  progressFiles <- dir(SCENDIR, recursive=TRUE, full.names=TRUE, pattern="progress.txt")
  
  scenList <- list.dirs(path = SCENDIR, full.names = FALSE, recursive = FALSE)
  progressList <- as.numeric(sapply(progressFiles, function(f) read.table(f, col.names = FALSE)))
  
  scenarioDF$progress[match(scenList,scenarioDF$directory)] <- progressList
  
  updateDB(scenarioDF)
  
}

updateStatusScenario <- function() { 
  
  scenarioDF <- readScenarioFromDB()
  
  statusFiles <- dir(SCENDIR, recursive=TRUE, full.names=TRUE, pattern="status.txt")
  
  scenList <- list.dirs(path = SCENDIR, full.names = FALSE, recursive = FALSE)
  statusList <- as.character(sapply(statusFiles, function(f) read.table(f, col.names = FALSE)))
  
  scenarioDF$status[match(scenList,scenarioDF$directory)] <- statusList
  
  updateDB(scenarioDF)
  
}



updateScenarioInfoInDB <- function(scenarioNameIN,scenarioNameOUT,directoryOUT,scenarioDescOUT) {
	
  print(paste0(Sys.time()," - Aggiorno info dello scenario: ", scenarioNameIN))
	scenarioDF <- readScenarioFromDB()
	scenarioDF[scenarioDF$scenarioName == scenarioNameIN,1:3] <- list(scenarioNameOUT,directoryOUT,scenarioDescOUT)
	
	updateDB(scenarioDF)
	
}


deleteScenarioInDB <- function(scenarioNameIN) {
	
  print(paste0(Sys.time()," - Elimino lo scenario: ", scenarioNameIN))
	scenarioDF <- readScenarioFromDB()
	scenarioDF <- scenarioDF[!(scenarioDF$scenarioName == scenarioNameIN),] 
	
	updateDB(scenarioDF)
	
}

addScenarioInDB <- function(scenarioName, directory, scenarioDescription, userName) {
	
  print(paste0(Sys.time()," - Aggiungo lo scenario: ", scenarioName))
	dateCreation = strftime(Sys.time(),format = "%d %b %Y")
	scenarioDF <- readScenarioFromDB()
	progress <- 0
	status <- "Inizio calcolo"
	
	# aggiungo nuovo scenario
	scenarioDF[nrow(scenarioDF) + 1,] <- list(scenarioName, directory, scenarioDescription, dateCreation, userName, progress, status)
	
	updateDB(scenarioDF)
	
}

deleteUserInDB <- function(userName) {
	
  print(paste0(Sys.time()," - Elimino l' utente: ", userName))
	scenarioDF <- readScenarioFromDB()
	scenarioDF <- scenarioDF[!(scenarioDF$userName == userName),] 
	
	updateDB(scenarioDF)
	
}







