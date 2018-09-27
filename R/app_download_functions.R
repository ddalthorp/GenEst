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
#' @return a download handler function
#'
#' @export
#'
downloadTable <- function(filename, tablename){
  downloadHandler(filename = filename,
                  content = function(file){
                              write.csv(tablename, file, row.names = FALSE)
                            }
                  )
}

#' @title Download a zipped data set
#'
#' @description Handle the downloading of a data set
#'
#' @param set the name of the data set to download
#'
#' @return a download handler function
#'
#' @export
#'
downloadData <- function(set){

  SE <- paste0("SE_", set, ".csv")
  CP <- paste0("CP_", set, ".csv")
  DWP <- paste0("DWP_", set, ".csv")
  SS <- paste0("SS_", set, ".csv")
  CO <- paste0("CO_", set, ".csv")

  fpre <- switch(set, "mock" = "", "mock2" = "", "powerTower" = "solar_", 
            "PV" = "solar_", "trough" = "solar_", "cleared" = "wind_", 
            "RP" = "wind_", "RPbat" = "wind_"
          )
  filename <- paste0(fpre, set, ".zip")
  foldername <- paste0("../extdata/", fpre, set, "/")

  downloadHandler(
    filename = filename,
    content = function(file)  {
      pth = foldername
      dir.create(tmp <- tempfile())
      cat(paste0(pth, SE), file = file.path(tmp, SE))
      cat(paste0(pth, CP), file = file.path(tmp, CP))
      cat(paste0(pth, DWP), file = file.path(tmp, DWP))
      cat(paste0(pth, SS), file = file.path(tmp, SS))
      cat(paste0(pth, CO), file = file.path(tmp, CO))
      zip(zipfile = file, files = tmp)
    },
    contentType = "application/zip"
  )
}


