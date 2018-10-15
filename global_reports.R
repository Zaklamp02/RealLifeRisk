#-------------------------------------------#
# 1. GENERATE REPORT
#-------------------------------------------#
#
# This is a key function to provide consolidated
# reports to players. The idea is to use this
# function once each turn. The report provides
# information on action outcomes, results
# of special actions, units lost in battle etc.
#
#-------------------------------------------#

generate_report <- function(session,tbs,tland,player,input,output,msgBattle){
  
  bomb <- ''
  for(p in playerDef$Player){
    for(j in 1:3){
      bomb <- paste0(bomb,msglog[[paste0(p,'a',j)]]$bomb)
    }
  }
  bomb <- ifelse(grepl(playerDef$Label[playerDef$Player==player],bomb),bomb,'')
  battle <- ifelse(grepl(player,msgBattle),msgBattle,'')
  
  
  rapport <- paste0(
    l,b,
    'Rapport: ',playerDef$Label[playerDef$Player==player],b,
    sample(1:30,1),' ',month.abb[floor(turn/yearCycle*12)],' ',year,b
  )
  
  overzicht <- paste0(
    l,b,
    'Overzicht:',b,
    'Goud: ',playerDef$Gold[playerDef$Player==player],b,
    'Land: ',playerDef$Land[playerDef$Player==player],' (',round(100*playerDef$Land[playerDef$Player==player]/prod(gridSize)),'%)',b
  )
  
  acties <- paste0(
    l,b,
    'Uitslag acties:',b,
    "A1: ", msglog[[paste0(player,'a1')]]$type,' ',msglog[[paste0(player,'a1')]]$outcome,b,
    "A2: ", msglog[[paste0(player,'a2')]]$type,' ',msglog[[paste0(player,'a2')]]$outcome,b,
    "A3: ", msglog[[paste0(player,'a3')]]$type,' ',msglog[[paste0(player,'a3')]]$outcome,b
  )
  
  radar         <- generate_radar_report(player)
  gevecht       <- paste0(l,b,'Gevechtssituaties:',b,battle,b,bomb,b)
  unitoverzicht <- generate_unit_report(tbs,player)
  landoverzicht <- generate_land_report(tland,player)
  
  out <- paste(rapport,overzicht,acties,gevecht,radar,unitoverzicht,landoverzicht,l,sep='')
  game[[paste(turn)]][[paste0('report',player)]] <<- out
  
  return(out)
  
}

generate_land_report <- function(tland,player){
  i   <- which(playerDef$Player==player)
  lnd <- which(tland[,,1]  == playerDef$red[i]/255 & tland[,,2]  == playerDef$green[i]/255 & tland[,,3]  == playerDef$blue[i]/255,arr.ind=T)
  x   <- xNames[lnd[,2]]
  y   <- yNames[gridSize[2] - lnd[,1] + 1]
  msg <- paste(x,y,sep="")
  msg <- paste(msg,collapse = s)
  msg <- paste(l,b,"Land overzicht",b,msg,b)
  
  return(msg)
}

#-------------------------------------------#
# 1. GENERATE REPORT
#-------------------------------------------#
generate_unit_report <- function(tbs,player=NULL){
  
  if(!is.null(player)){tbs <- tbs[tbs$player==player,]}
  
  tbs <- tbs[order(tbs$xPos,tbs$yPos),]
  p1  <- paste(tbs$xPos,tbs$yPos,sep="")
  p2  <- paste(tbs$quantity,tbs$unit,sep="x ")
  msg <- paste(p1,p2,sep=": ")
  msg <- paste(msg,collapse=s)
  msg <- paste(l,b,"Unit overzicht",b,msg,b)
  
  return(msg)
}

generate_radar_report <- function(player){
  radar <- paste0(
    msglog[[paste0(player,'a1')]]$radar,
    msglog[[paste0(player,'a2')]]$radar,
    msglog[[paste0(player,'a3')]]$radar
  )
  if(length(radar)==0){
    msg <- ''
  } else {
    msg <- paste(l,b,"Radar Scans:",b,radar,b)
  }

  return(msg)
  
}
