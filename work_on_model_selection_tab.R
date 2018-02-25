    isolate({

      output$SE_model_select_inputs <- renderUI({
        
        w <- "HI, HOW ARE YOU"
        rv$n_sizeclasses <- length(rv$sizeclasses)

        if(length(rv$sizeclasses) == 1){
          rv$SE_AICc_tab <- pkm_set_aicc_tab(rv$SE_mods) 
          rv$SE_mod_order <- as.numeric(row.names(rv$SE_AICc_tab))
          rv$SE_mod_names <- names(rv$SE_mods)[rv$SE_mod_order]
          w <- paste(w,  rv$SE_mod_names)
          #       selectizeInput(paste("SE_models_to_use", 1, sep = ""),
           #                     rv$sizeclasses[1], 
            #                    choices = rv$SE_mod_names))
        }else{
          for(i in 1:rv$n_sizeclasses){
            rv$SE_AICc_tab <- pkm_set_aicc_tab(rv$SE_mods[[i]])
            rv$SE_mod_order <- as.numeric(row.names(rv$SE_AICc_tab))
            rv$SE_mod_names <- names(rv$SE_mods[[1]])[rv$SE_mod_order]
            w <- paste(w, rv$SE_mod_names)
             #     selectizeInput(paste("SE_models_to_use", i, sep = ""),
              #                   rv$sizeclasses[i], 
               #                  choices = rv$SE_mod_names))
          }
        }
  
        HTML(w)
      })
    })