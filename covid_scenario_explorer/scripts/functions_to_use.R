adjust_Mx <- function(
  df, 
  P_r, # Prevalence (without shielding)
  R_d, # Ratio of true to confirmed cases
  HR_S_y,  # Hazard rate for symptomatic (confirmed) cases
  HR_A_s = 1, # Hazard rate for asymptomatic (unconfirmed) cases
  Sh_r = NULL  # Shielding rate for 70+
  
  ){
  

  U_n               <- 1 - P_r # Uninfected 
  S_y               <- P_r / (1 + R_d) # Symptomatic (confirmed) share
  A_s               <- P_r * R_d / (1 + R_d) # Asymptomatic (unconfirmed) share
  
  P_r_sh            <- Sh_r * P_r # Prevalence in shielded
  
  U_n_sh            <- 1 - P_r_sh # Uninfected in shielded
  S_y_sh            <- P_r_sh / (1 + R_d)  # Symptomatic (confirmed) share in shielded
  A_s_sh            <- P_r_sh * R_d / (1 + R_d) # Asymptomatic (unconfirmed) share in shielded
  

  if(!is.null(Sh_r)){
    out <- 
      df %>% 
      mutate(mx_adjusted = case_when(
        age < 70 ~                (mx * U_n)                    + 
          ((mx * HR_A_s) * A_s) +
          ((mx * HR_S_y) * S_y),
        age >= 70 ~ (mx * U_n_sh)                    + 
          ((mx * HR_A_s) * A_s_sh) +
          ((mx * HR_S_y) * S_y_sh),
        TRUE  ~ NA_real_
        )
      ) %>% 
      mutate(
        n_control = mx * N,
        n_covid = mx_adjusted * N
      )    
    
  } else {
    out <- 
    df %>% 
      mutate(mx_adjusted = 
               (mx * U_n)                    + 
               ((mx * HR_A_s) * A_s) +
               ((mx * HR_S_y) * S_y)
      ) %>% 
      mutate(
        n_control = mx * N,
        n_covid = mx_adjusted * N
      )    
  }
  
  return(out)
}