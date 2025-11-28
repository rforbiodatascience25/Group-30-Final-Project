char_type_col <- function(df) {
  df |>
    mutate(across(c(project, 
                    duration, 
                    rep, 
                    instrument), 
                  
                  as.character))
}
