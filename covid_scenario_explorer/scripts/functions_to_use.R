adjust_Mx <- function(df, truePrevalence, ratioTrueToConfirmed, hrConfirmed, hrUnconfirmed = 1){
  
  share_uninfected  <- 1 - truePrevalence
  share_confirmed   <- truePrevalence / ratioTrueToConfirmed
  share_unconfirmed <- 1 - share_uninfected - share_confirmed
  
  df %>% 
    mutate(mx_adjusted = 
             (mx * share_uninfected)                    + 
             ((mx * hrUnconfirmed) * share_unconfirmed) +
             ((mx * hrConfirmed)   * share_confirmed)
    ) %>% 
    mutate(
      n_control = mx * N,
      n_covid = mx_adjusted * N
    )
}