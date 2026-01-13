cat(paste("PGHOST=", Sys.getenv("PGHOST"), "\n",
           "PGPORT=", Sys.getenv("PGPORT"), "\n",
           "PGUSER=", Sys.getenv("PGUSER"), "\n",
           "PGDATABASE=", Sys.getenv("PGDATABASE"), "\n",
           "PGPASSWORD=", Sys.getenv("PGPASSWORD"), "\n", sep=""))
