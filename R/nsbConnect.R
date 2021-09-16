nsbConnect <-
function(username, password, intern=FALSE, local=FALSE){
  host <- ifelse(local, "localhost", ifelse(intern, "192.168.101.133", "212.201.100.111"))
  dbConnect(Postgres(), user = username, password = password, host = host, dbname = "nsb", port = "5432")
  }
