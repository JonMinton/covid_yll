#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(glue)

dta <- read_rds("data/mx_and_N.rds")


source("scripts/functions_to_use.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
   
   # Application title
   dashboardHeader(title = "Covid Mortality Explorer"),
   
   # Sidebar with a slider input for number of bins 
   dashboardSidebar(
      sidebarMenu(
        selectInput(
          inputId = "population", 
          label = "Select population of interest",
          choices = c("England", "Wales", "Scotland")
        ),
        sliderInput(
          inputId = "true_prevalence", 
          label = "Select true prevalence (0 to 1)", 
          value = 0.6, min = 0, max = 1
        ),
        numericInput(
          inputId = "ratio_true_conf",
          label = "Select ratio of true to confirmed cases",
          value = 5, min = 1, max = 1000
        ),
        numericInput(
          inputId = "hr_confirmed",
          label = "Select mortality hazard for confirmed cases",
          value = 2, min = 0, max = 1000
        ),
        numericInput(
          inputId = "hr_unconfirmed",
          label = "Select mortality hazard for true but unconfirmed cases",
          value = 1.10, min = 0, max = 1000
        ),
        checkboxInput("allow_shielding", "Allow shielding of 70+?"),
        sliderInput("shielding_effectiveness", "Effectiveness of shielding (0 to 1)", min = 0, max = 1, value = 0),
        actionButton(
          "run", 
          "Click to run with selected parameters"
        )
        )
      ),
      
      # Show a plot of the generated distribution
      dashboardBody(
        fluidRow(
               tabBox(
                 title = "Main tabbox", width = 12,
                 tabPanel(title = "Main Tab Panel",
                    box(width = 12,
                        plotly::plotlyOutput("plot"),
                        tableOutput("table")

                    )
                  ),
                  tabPanel(
                      title = "Detailed",
                      tableOutput("detailed_table")
                  ),
                  tabPanel(
                      title = "Help",
                      {
                        fluidRow(
                          column(width = 12,
                            box(
                              title = "Introduction",
                              HTML("This app shows how the estimates for the mortality effect of covid 19 in England, Wales and Scotland depend on a number
                                   of assumptions about which good data driven parameters are not currently available. The app allows the user to enter a number of key parameters, and produces 
                                   an estimate of the total number of additional deaths which might be expected in 2020 in different UK nations which follow from these assumptions.")
                              )
                          ), 
                          column(width = 12,
                            box(
                              title = "Parameters",
                              tags$ol(
                                tags$li(HTML("<b>Prevalence</b>: The true proportion of the population that will be infected in 2020")),
                                tags$li(HTML("<b>True to confirmed cases ratio</b>: How many true cases there will be for each confirmed cases")),
                                tags$li(HTML("<b>Hazard multiplier for confirmed Covid cases</b>: The multiple applied to someone's age-sex specific mortality risk in a year if they are a confirmed cases")),
                                tags$li(HTML("<b>Hazard multiplier for unconfirmed Covid cases</b>: The multiple applied to someone's age-sex specific mortality risk in a year if they were infected by covid but not a confirmed case"))
                              )
                            )
                          ),
                          column(width = 12,
                            box(
                              title = "Covid-19: Use of Hazard Ratio instead of Case-Fatality Rates",
HTML(
"A key summary statistic often presented to try to summarise the mortality impact of Covid-19 
is the <a href = 'https://ourworldindata.org/coronavirus#the-case-fatality-rate-cfr'>case fatality rate (CFR)</a>, 
which is usually calculated as the ratio of confirmed deaths to confirmed cases. 
CFR estimates have varied greatly, <a href= 'https://ourworldindata.org/coronavirus#the-case-fatality-rate-cfr'>from under 0.5% to over 10%.</a> 
"),
HTML("
The CFR as reported is highly dependent on a number of contextual factors, including:
<br>
<br>
<ul>
  <li> <b>Testing protocol</b>: Influencing both the confirmed cases and confirmed deaths, and the selectivity/severity/exhaustiveness of both. </li>
  <li> <b>Demography</b>: The age and sex distribution of the affected population. </li>
</ul>
<br>
The effects of the <b>testing protocol</b> can be explored through the <b>true-to-confirmed cases ratio</b> and the <b>hazard multiplier for unconfirmed cases</b>.
<br>
The importance of demography on covid-related mortality has been highlighted from data on the case fatality rate disaggregated by sex and age group, with the mortality risk increasing exponentially with age
much as the all-cause mortality risk does. This has led David Spiegelhalter to conclude that <a href = 'https://medium.com/wintoncentre/how-much-normal-risk-does-covid-represent-4539118e1196'>the short-term mortality risk of catching SARS-CoV2 is equivalent to someone's annual 
age-sex adjusted all-cause mortality risk.</a> This is the reason the hazard multiplier conditional on a confirmed case is set by default at 2.0 in this scenario modelling tool. 
<a href = 'https://osf.io/abx7s/'>This preprint</a> has attempted to present the interaction between Western European population structures and the age-sex specific mortality burden of SARS-CoV-2 using a choropleth, with darker orange colours indicating NUTS-3 regions with older populations more at risk of death from SARS-CoV-2. 
 This scenario modelling tool attempts to do something similar.")
                              
                            )
                          ),
column(width = 12,
       box(
         title = "Estimating scenario-dependent deaths",
HTML(
"
This scenario modelling tool takes the user's parameters to subdivide a population into three distinct population subgroups:
<br>
<br>
<ul>
  <li><b>Uninfected</b>: This subpopulation experienced a usual age-sex specific annual mortality risk.</li>
  <li><b>Infected-and-confirmed</b>: This population experiences by default a doubling of usual age-sex specific annual mortality risk (HR = 2.0),</li>
  <li><b>Infected-but-unconfirmed</b>: This population experiences by default only a slightly increased age-sex specific annual mortality risk (HR = 1.1),</li>
</ul>
<br>
The simplifying assumption is made that prevalence of infection, or severity conditional on being a confirmed case, does not depend on age or sex, 
and that the true prevalence is equally distributed throughout the population. This can be revisited in later versions of the tool.
"
)
         )
       
       
         ),
column(width = 12,
       box(
         title = "Data sources",
         HTML(
"
<ul>
  <li><b><a href = 'https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/singleyearlifetablesuk1980to2018'>Lifetables for England, Wales, and Scotland</a></b></li>
  <li><b><a href = '(https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestool'>Population structure for England and Wales</a></b></li>
  <li><b><a href = 'https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/c505f490-c201-44bd-abd1-1bd7a64285ee/download/dz2011-pop-est_30082019.csv'>Population structure for Scotland</a></b></li>
</ul>
<br><br>
Note the latest available year for population structure and lifetables is 2018, so this is what was used.
"
         )
         )
       
       
         ),
column(width = 12,
       box(
         title = "Shielding rate",
         HTML(
"
The scenario tool includes an option to look at scenarios in which persons aged 70 and over are 'shielded' from 
the full infection rate affecting the rest of the population. If the checkbox is ticked, the share of the infected population
for persons aged 70 and over is reduced by the shielding rate specified in the slider above. As the baseline 
mortality hazard increases exponentially with age (from around age 30) effective shielding can therefore disproportionately
reduce the number of expected deaths from SARS-CoV-2.
<br>
Note that in shielding scenarios the overall population prevalence will now be below that specified by the user. 

"
         )
       )
       
       
)


                        )
                      }
                    )
                    
                    
                 )
                 
               )
          
        )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    observeEvent(
      input$run,
      {
        print(dta)
      }
    )
  
    observeEvent(
      input$run,
      {
        R_d               <- input$ratio_true_conf
        P_r               <- input$true_prevalence
        Sh_r              <- 1 - input$shielding_effectiveness 
        
        U_n               <- 1 - P_r # Uninfected 
        S_y               <- P_r / (1 + R_d) # Symptomatic (confirmed) share
        A_s               <- P_r * R_d / (1 + R_d) # Asymptomatic (unconfirmed) share
        
        P_r_sh            <- Sh_r * P_r # Prevalence in shielded
        
        U_n_sh            <- 1 - P_r_sh # Uninfected in shielded
        S_y_sh            <- P_r_sh / (1 + R_d)  # Symptomatic (confirmed) share in shielded
        A_s_sh            <- P_r_sh * R_d / (1 + R_d) # Asymptomatic (unconfirmed) share in shielded
        
        print(glue("Inputs: R_d: {R_d}; P_r: {P_r}; Sh_r: {Sh_r}"))
        
        print(glue("Unshielded: U_n: {U_n}; S_y: {S_y}; A_s: {A_s} (Sum: {U_n + S_y + A_s})"))
        print(glue("Shielded: U_n_sh: {U_n_sh}; S_y_sh: {S_y_sh}; A_s_sh: {A_s_sh} (Sum: {U_n_sh + S_y_sh + A_s_sh})"))
      }
    )
   calc_adj_lt <- eventReactive(
     input$run,
     {

       if(!input$allow_shielding){
         out <-
           dta %>% 
           filter(population == input$population)  %>%
           adjust_Mx(
             P_r       = input$true_prevalence,
             R_d       = input$ratio_true_conf,
             HR_S_y    = input$hr_confirmed,
             HR_A_s    = input$hr_unconfirmed
           )
       } else {
         out <-
           dta %>% 
           filter(population == input$population)  %>%
           adjust_Mx(
             P_r       = input$true_prevalence,
             R_d       = input$ratio_true_conf,
             HR_S_y    = input$hr_confirmed,
             HR_A_s    = input$hr_unconfirmed,
             Sh_r      = 1 - input$shielding_effectiveness
           )
       }
       return(out)
     }
  )
  
  output$plot <- renderPlotly({
    calc_adj_lt() %>% 
      mutate(n_diff = n_covid - n_control) %>%
      select(age, sex, n_diff) %>% 
      spread(sex, n_diff) %>% 
      arrange(age) %>% 
      mutate(
        cumulative_diff_males   = cumsum(male),
        cumulative_diff_females = cumsum(female)
      ) %>% 
      mutate(text = glue("At age {age}, {round(female + male, 0)} more deaths (F: {round(female, 0)}; M: {round(male, 0)})
Cumulative deaths by age {age}: {round(cumulative_diff_males + cumulative_diff_females, 0)} (F: {round(cumulative_diff_females, 0)}; M: {round(cumulative_diff_males, 0)})")) %>% 
      plot_ly(x = ~age, hoverinfo = 'text') %>% 
      add_trace(y = ~female,  text = ~text, name = "Females", type = 'bar', marker = list(color = 'rgb(255, 0, 0)')) %>% 
      add_trace(y = ~male, text = ~text, name = "Males", type = 'bar', marker = list(color = 'rgb(0, 0, 255)')) %>% 
      layout(
        yaxis = list(title = 'Number of deaths'), 
        xaxis = list(title = 'Age in single years'), 
        barmode = 'stack'
        )
      
  }) 
   
  output$table <- renderTable({
    tmp <- 
    calc_adj_lt() %>% 
      mutate(n_diff = n_covid - n_control) %>% 
      group_by(sex) %>% 
      summarise(
        n_control = sum(n_control),
        n_covid   = sum(n_covid),
        n_diff    = sum(n_diff)
      ) %>% 
      ungroup()
    
    bind_rows(
      tmp,
      tibble(
        sex = "total",
        n_control = sum(tmp$n_control),
        n_covid   = sum(tmp$n_covid),
        n_diff    = sum(tmp$n_diff)
      )
    )
    
    
  })
  output$detailed_table <- renderTable({
     calc_adj_lt()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

