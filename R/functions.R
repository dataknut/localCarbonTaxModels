# useful functions

getSoton <- function(dt){
  # selects all rows where LA name = Southampton
  # assumes data.table
  # assumes LA name
  res <- dt[LAD11NM %like% "Southampton"]
  return(res)
}

getSolent <- function(dt){
  # as above but all Solent
  res <- dt[LAD11NM == "Basingstoke and Deane" |
              LAD11NM == "East Hampshire" |
              LAD11NM == "Eastleigh" |
              LAD11NM == "Fareham" |
              LAD11NM == "Gosport" |
              LAD11NM == "Hart" |
              LAD11NM == "Havant" |
              LAD11NM == "Isle of Wight" |
              LAD11NM == "New Forest" |
              LAD11NM == "Portsmouth" |
              LAD11NM == "Rushmoor" |
              LAD11NM == "Southampton" | 
              LAD11NM == "Test Valley" |
              LAD11NM == "Winchester" ]
  return(res)
}