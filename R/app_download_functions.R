
downloadSEFig <- function(rv){
  downloadHandler(filename = "SE_fig.png",
      content = function(file){
        png(file, height = rv$figH_SE, width = rv$figW_SE, units = "px")
        plot(rv$modSet_SE, specificModel = rv$best_SE)
        dev.off()
      }
  )
}

downloadTable <- function(filename, tablename){
  downloadHandler(filename = filename,
                  content = function(file){
                              write.csv(tablename, file, row.names = FALSE)
                            }
                  )
}

