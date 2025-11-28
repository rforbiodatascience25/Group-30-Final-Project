char_type_col <- function(df) {
  df |>
    mutate(across(c(project, 
                    duration, 
                    rep, 
                    instrument), 
                  
                  as.character))
}

saa_rule_155 <- function(column) {
  column <- unlist(column)
  n_above <- sum(column >= 5000, na.rm = TRUE)
  
  case_when(
    n_above == 3 ~ "Positive",
    n_above <= 1 ~ "Negative",
    n_above == 2 ~ "Inconclusive",
    TRUE ~ NA_character_
  )
}

saa_rule_237 <- function(column) {
  column <- unlist(column)
  
  n_above_45000 <- sum(column >= 45000, na.rm = TRUE)
  n_above_3000  <- sum(column >= 3000,  na.rm = TRUE)
  n_between  <- sum(column >= 3000 & column < 45000, na.rm = TRUE)
  n_below_3000  <- sum(column < 3000, na.rm = TRUE)
  
  # Positive
  if (n_above_45000 == 3) return("Positive")
  if (n_between == 2 | n_between == 3) return("Positive")
  if (n_above_45000 == 2 & n_between == 1) return("Positive")
  
  # Negative
  if (n_above_3000 <= 1) return("Negative")
  
  # Inconclusive
  if (n_above_45000 == 2 & n_below_3000 == 1) return("Inconclusive")
  if (n_above_45000 == 1 & n_between == 1 & n_below_3000 == 1) return("Inconclusive")
  
  NA_character_
}