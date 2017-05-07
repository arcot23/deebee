.onLoad <- function(libname, pkgname){
  if (Sys.getenv("ojdbcfile") == "")
    Sys.setenv(ojdbcfile = "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc5.jar")
}

onAttach <- function(...) {
  print("attached")
}
