#' @title Download the CP figure
#'
#' @description Handle the CP figure downloading
#'
#' @param rv the reactive values list
#'
#' @return a download handler function
#'
#' @export
#'
downloadCPFig <- function(rv){
  downloadHandler(filename = "CP_fig.png",
      content = function(file){
        png(file, height = rv$figH_CP, width = rv$figW_CP, units = "px")
        plot(rv$modSet_CP, specificModel = rv$outCPdlsfig)
        dev.off()
      }
  )
}

#' @title Download the SE figure
#'
#' @description Handle the SE figure downloading
#'
#' @param rv the reactive values list
#'
#' @return a download handler function
#'
#' @export
#'
downloadSEFig <- function(rv){
  downloadHandler(filename = "SE_fig.png",
      content = function(file){
        png(file, height = rv$figH_SE, width = rv$figW_SE, units = "px")
        tryCatch(
          plot(rv$modSet_SE, specificModel = rv$outSEpk),
          error = function(x){plotNA()}
        )
        dev.off()
      }
  )
}

#' @title Download the g figure
#'
#' @description Handle the g figure downloading
#'
#' @param rv the reactive values list
#'
#' @param sc size class
#'
#' @return a download handler function
#'
#' @export
#'
downloadgFig <- function(rv, sc){
  downloadHandler(filename = "g_fig.png",
      content = function(file){
        png(file, height = rv$figH_g, width = rv$figW_g, units = "px")
        plot(rv$gGeneric[[sc]], CL = rv$CL)
        dev.off()
      }
  )
}

#' @title Download the M figure
#'
#' @description Handle the M figure downloading
#'
#' @param rv the reactive values list
#'
#' @param split logical indicator to use the split or not
#'
#' @param transpose logical indicator if to transpose the output or not
#'
#' @return a download handler function
#'
#' @export
#'
downloadMFig <- function(rv, split = TRUE, transpose = FALSE){

  if (split){
    if (transpose){
      downloadHandler(filename = "M_fig.png",
          content = function(file){
            png(file, height = rv$figH_M, width = rv$figW_M, units = "px")
            tryCatch(
              plot(transposeSplits(rv$Msplit)),
              error = function(x){plotNA()}
            )
            dev.off()
          }
      )
    } else {
      downloadHandler(filename = "M_fig.png",
          content = function(file){
            png(file, height = rv$figH_M, width = rv$figW_M, units = "px")
            tryCatch(
              plot(rv$Msplit),
              error = function(x){plotNA()}
            )
            dev.off()
          }
      )
    }
  } else{
    downloadHandler(filename = "M_fig.png",
        content = function(file){
          png(file, height = rv$figH_M, width = rv$figW_M, units = "px")
          tryCatch(
            plot(rv$M),
            error = function(x){plotNA()}
          )
          dev.off()
        }
    )
  }
}

#' @title Download a table
#'
#' @description Handle the downloading of a table
#'
#' @param filename the name for the file writing out
#'
#' @param tablename the name of the table in the rv list
#'
#' @param csvformat format for .csv file: "" or NULL for comma-separated, 2 
#'  for semi-colon separated
#'
#' @return a download handler function
#'
#' @export
#'
downloadTable <- function(filename, tablename, csvformat){
  if (Sys.info()['sysname'] == "Windows"){
    colnames(tablename) <- gsub("\u0394", "delta", colnames(tablename))
  }
  downloadHandler(filename = filename, content = function(file){
    get(paste0("write.csv", csvformat))(x = tablename, file = file)
  })
}

#' @title Download a zipped data set
#'
#' @description Handle the downloading of a data set
#'
#' @param set the name of the data set to download
#' @param csvformat Format of .csv files to download. For comma field 
#'  separator and period decimal separator, use \code{csvformat = NULL} or "".
#'  For semicolon field separator and comma decimal separator, use
#'  \code{csvformat = 2}.
#' @return a download handler function
#'
#' @export
#'
downloadData <- function(set, csvformat = NULL){
  fpre <- switch(set, "mock" = "", 
                      "powerTower" = "solar_", 
                      "PV" = "solar_",
                      "trough" = "solar_", 
                      "cleared" = "wind_",
                      "RP" = "wind_", 
                      "RPbat" = "wind_")
  filename <- paste0(fpre, set, ".zip")
  exob <- get(paste0(fpre, set))
  pth <- tempdir()
  writef <- get(paste0("write.csv", csvformat))
  for (seti in names(exob)){
    writef(exob[[seti]], file = paste0(pth, "/", seti, "_", set, ".csv"),
      row.names = FALSE)
  }
  downloadHandler(
    filename = filename,
    content = function(file)  {
      tozip <- paste0(pth, "/", names(exob), "_", set, ".csv")
      zip(zipfile = file, files = tozip, flags = c("-q", "-j"))
    },
    contentType = "application/zip"
  )
}

