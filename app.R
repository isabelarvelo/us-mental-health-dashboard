library(shiny)
library(fivethirtyeight)
library(ggplot2)
library(plotly)
library(survey)
library(ggsurvey)
library(shinyjs)
library(Matrix)
library(shinydashboard)
library(shinythemes)
library("scales")
library(data.table)
library(DT)
library(dplyr)
library(sf)
library(RColorBrewer)
library(sf)
library(leaflet)
library(tigris)
library(htmltools)
library(miscTools)
library(rsconnect)
library(ggthemes)
library(formattable)
library(rlang)
library(later)
library(shinybusy)
library(leafsync)
library(tidyr)
library(purrr)
library(quantreg)
library(shinydisconnect)
library(spsComps)

?options(tigris_use_cache = TRUE)
options(survey.lonely.psu = "certainty")

load("data/NSDUH_survey_adults_RC.RData")
load("data/BRFSS_survey_all.RData")
load("data/GH_O.RData")
load("data/GH_Race_split.RData")
load("data/GH_Age_split.RData")
load("data/GH_HI_split.RData")
load("data/GH_EDU_split.RData")
load("data/Dep_O.RData")
load("data/Dep_Age_split.RData")
load("data/Dep_HI_split.RData")
load("data/Dep_EDU_split.RData")

load("data/dep_edu_table .RData")
load("data/dep_age_table.RData")
load("data/dep_HI_table.RData")

load("data/gh_edu_table.RData")
load("data/gh_age_table.RData")
load("data/gh_HI_table.RData")
load("data/gh_race_table.RData")

rexp2 <- function(x) {
  round(exp(x), digits = 3)
}

addparens <- function(x){paste("(", x, ")")}

demVars <-
  c(
    "gender",
    "education",
    "marital_status",
    "age",
    "race",
    "employment",
    "income",
    "poverty_level"
  )

quantVars <-
  c("psych_distress_pm",
    "worst_psych_distress_py",
    "srs_mhi_prob")

catVars <-
  c(
    "srs_psych_distress_pm",
    "suicidal_thoughts",
    "suicide_plan",
    "suicide_attempt",
    "mi_cat",
    "lifetime_mde",
    "mde_py"
  )

allVars <-
  c(
    "psych_distress_pm",
    "worst_psych_distress_py",
    "srs_mhi_prob",
    "srs_psych_distress_pm",
    "suicidal_thoughts",
    "suicide_plan",
    "suicide_attempt",
    "mi_cat",
    "lifetime_mde",
    "mde_py"
  )



states_shape <- tigris::states(cb = TRUE, resolution='500k', class = "sf")

states_shape_simple <- rmapshaper::ms_simplify(states_shape, keep = 0.05, keep_shapes = TRUE)
states_shape_simple <- st_transform(states_shape_simple, 4326)

ui <-  fluidPage(
  
  shinybusy::add_busy_spinner(
    spin = "double-bounce",
    color = "#112446",
    timeout = 100,
    position = c("bottom-right"),
    margins = c(10, 10),
    height = "50px",
    width = "50px"
  ),
  
  theme = shinytheme("cosmo"),
  
  titlePanel("U.S. Mental Health Dashboard"),
  
  navbarPage(
    
    tags$head(tags$style(
      HTML(
        "
         .navbar {
                          height: 50px;
                          min-height:30px !important;
        }
                        
                      
                      .navbar-brand {
                            padding-top:2px !important;
                            padding-bottom:2px !important;
                            height: 30px;
                            font-size: 30px;
                      }
                            
                     .navbar .navbar-header {float: left;
                           color: #ff3368;
                           font-weight: 120px;
                           font-size: 30px;
                           }

                     .navbar .navbar-nav {float: right;}
                      
                  .tab-content { height: 83vh; overflow-y: auto !important; } "
        
      ))), 
    
    
    tabPanel(
      "National",
        sidebarPanel(
          width = 3, 
          selectInput(
            inputId = "var_select",
            label = "Select Response Variable",
            choices =   c(
              "Psychological Distress: PM " = "psych_distress_pm",
              "Worst Psychological Distress: PY" = "worst_psych_distress_py",
              "Probability of Serious Mental Health Illness" = "srs_mhi_prob",
              "Serious Psychological Distress: PM" = "srs_psych_distress_pm",
              "Serious Suicidal Thoughts PY" = "suicidal_thoughts",
              "Made Suicidal Plan PYs" = "suicide_plan",
              "Attempted Suicide PY" = "suicide_attempt",
              "Mental Illness Category" = "mi_cat",
              "Lifetime Major Depressive Episode" = "lifetime_mde",
              "Major Depressive Episode PY" = "mde_py"
            )
          ),
          
          em("PM: Past Month, ", style = "font-size:14px"),
          em("PY: Past Year", style = "font-size:14px"),
          
          p(),
          
          checkboxInput("LogY", "Log Transform Y", FALSE),
          
          
          
          radioButtons(
            "type",
            "Analysis Type",
            c("Overall Population" = "overall",
              "Subpopulation" = "subpop")
          ),
          
          uiOutput("secondSelection"),
          actionButton('show_about_NSDUH', 'About the Data')

          

          ), 

      mainPanel(
        strong("Response:"),
        em(textOutput("survey_q")),
        
        tabsetPanel(
          tabPanel(
            "Visualization",
            plotOutput("top_boxplot"),
            plotOutput("bottom_hist")
          ) ,
          tabPanel("Table",
                   p(),
                   p(),
                   p(),
                   DTOutput('table'), 
                   textOutput("confint")
                   
                   )
                  
        )
      ) ), 

    tabPanel(
      "State",
      sidebarPanel(
        width = 3,
        p(),
          selectInput(
            inputId = "st_var_select",
            label = "Select Response Variable",
            choices =   c(
              "Depressive Diagnosis" = "dep_recode",
              "General Health Status" = "GENHLTH"
              
            )
          ),
          
          selectInput(
            inputId = "st_dem_select",
            label = "View by",
            choices =   c(
              "Have any health insurance" = "HLTHPLN",
              "Age" = "AGE_G",
              "Education" = "EDUCAG",
              "Race" = "IMPRACE"
            )

          ),
          
          em("Depression Diagnosis by Race not available", style = "font-size:13px"),
          
          br(),
          br(), 
          
          actionButton('show_about_BRFSS', 'About the Data'),
        p(),
        br(),
        
        
        em(
          "Please note that data set includes more than 438,000 observations and
                  output will not be rendered immediately",
          style = "font-size:13px"
        )
      ),
      
      mainPanel(strong("Response:"),
                em(textOutput("survey_q_2")),
                tabsetPanel(
                  tabPanel(
                    "Visualization",
                    uiOutput("plots", label = "test")
                  ) ,
                  tabPanel("Table",
                           p(), 
                           strong("Table 1: Marginal Distribution"), 
                           DTOutput('state_table'), 
                           p(), 
                           strong("Table 2: State Level Estimates"),
                      DTOutput("grouped_state_table") )
                  
                )
                
      )
      
    ), 
    
    footer = strong("1-800-662-HELP (4357) is a confidential and free information service for individuals and family members facing mental and/or substance use disorders.
                    If you or someone you know is in crisis, call or text 988 ")

)

)              





server <- function(input, output) {

  filename <- normalizePath(file.path('./www/images','national-helpline-card.png'))
  tabfilename<-normalizePath(file.path('./www/images','988_Suicide_&_Crisis_Lifeline_logo.png'))
  
  output$img1<-renderImage({
    list(src = filename,
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  output$img2<-renderImage({
    list(src = tabfilename,
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  nsduh_url <-
    a("NSDUH Homepage", href = "https://nsduhweb.rti.org/respweb/homepage.cfm")
  brfss_url <-
    a("BRFSS Homepage", href = "https://www.cdc.gov/brfss/about/index.htm")
  
  observeEvent(input$show_about_NSDUH, {
    showModal(modalDialog(
      c(
        "The National Survey on Drug Use and Health (NSDUH) is a national study that collects 
        information on several health-related issues including tobacco, alcohol, drug use, and
        mental health in the United States. The population of interest is civilian, non-institutionalized 
        population aged 12 or older at the time of the survey [excludes active military personnel, 
        people living in institutional group quarters, homeless population]. This analysis only uses 
        data on adults ages 18 and older.",
        renderUI({
          tagList("URL link:", nsduh_url)
        })
      ) ,
      title = 'About the Data'
    ))
  })
  
  observeEvent(input$show_about_BRFSS, {
    showModal(modalDialog(
      c(
        "The Behavioral Risk Factor Surveillance System (BRFSS) is a nationwide system of health-related 
        telephone surveys that provide state-level information about health-related risk behaviors, 
        chronic health conditions, and use of preventive services among U.S. residents. The population 
        of interest is the noninstitutionalized adult population (18 years or older) residing in private
        residences or college housing in the United States or participating areas who have a working
        cellular telephone",
        renderUI({
          tagList("URL link:", brfss_url)
        })
      ),
      title = 'About the Data'
    ))
    
  })
  
  
  output$secondSelection <- renderUI({
    if (input$type == "subpop") {
      selectInput(
        inputId = "dem_select",
        label = "View by",
        choices =   c(
          "Gender" = "gender",
          "Education" = "education",
          "Marital Status" = "marital_status",
          "Age" = "age",
          "Race" = "race",
          "Employment" = "employment",
          "Income" = "income",
          "Poverty Level" = "poverty_level"
        )
      )
    }
  })
  
  
  
  output$top_boxplot <- renderPlot({
    if (input$type == "overall") {
      if (input$var_select %in% quantVars) {
        if (isTRUE(input$LogY)) {
          if (input$var_select == "srs_mhi_prob") {
            probs <-
              seq(
                min(NSDUH_survey_adults_RC$variables$srs_mhi_prob) * .95,
                max(NSDUH_survey_adults_RC$variables$srs_mhi_prob) *
                  1.05,
                length.out = 10
              )
            
            
          } else if (input$var_select == "psych_distress_pm") {
            probs <-
              seq(
                min(
                  NSDUH_survey_adults_RC$variables$psych_distress_pm
                ) * .95,
                max(
                  NSDUH_survey_adults_RC$variables$psych_distress_pm
                ) * 1.05,
                length.out = 10
              )
            
            
          } else {
            probs <-
              seq(0,
                (24 * 1.05),
                length.out = 10
              )
            
          }
          
          ggboxweight3d_svy(NSDUH_survey_adults_RC, log(.data[[input$var_select]])) +
            scale_y_continuous(labels = rexp2, breaks = log(probs)) +
            ggtitle(paste0("Boxplot of log ", input$var_select)) +
            scale_color_fivethirtyeight()  + theme_fivethirtyeight() +
            theme(axis.title = element_text()) + ylab(paste("exp(log(", input$var_select, "))"))
          
          
        } else {
          
          ggboxweight3d_svy(NSDUH_survey_adults_RC, .data[[input$var_select]])  +
            ggtitle(paste0("Boxplot of ", input$var_select)) + scale_color_fivethirtyeight()  +
            theme_fivethirtyeight() + 
            theme(axis.title = element_text()) + ylab(input$var_select)
        }
        
      } else {
        ggbarweight_svy(NSDUH_survey_adults_RC, .data[[input$var_select]], fill = TRUE) +
          labs(y = "Prevalence") +
          ggtitle(paste0("Bar Chart of ", input$var_select)) + scale_color_fivethirtyeight()  + 
          theme_fivethirtyeight()
      }
      
      
    } else {
      req(input$dem_select)
      
      if (input$var_select %in% quantVars) {
        if (isTRUE(input$LogY)) {
          if (input$var_select == "srs_mhi_prob") {
            probs <- seq(0.002540972 * .95,
                         0.9291205 * 1.05,
                         length.out = 10)
            
            
          } else {
            probs <- seq(0,
                         (24 * 1.05),
                         length.out = 10)
            
            
          } 
          
          ggboxweight3d_svy(NSDUH_survey_adults_RC, log(.data[[input$var_select]]), .data[[input$dem_select]]) +
            scale_y_continuous(labels = rexp2, breaks = log(probs)) +
            ggtitle(paste0("Boxplot of log ", input$var_select)) +
            scale_color_fivethirtyeight()  + theme_fivethirtyeight() + 
            theme(axis.title = element_text()) + ylab(paste("exp(log(", input$var_select, "))"))
          
          
        } else {
          ggboxweight3d_svy(NSDUH_survey_adults_RC, .data[[input$var_select]], .data[[input$dem_select]])  +
            ggtitle(paste0(
              "Boxplot of ",
              input$var_select,
              " by ",
              input$dem_select
            )) + scale_color_fivethirtyeight()  + theme_fivethirtyeight() +
             theme(axis.title = element_text()) + ylab(input$var_select)
        }
        
      } else {
        ggbarcrosstabs_svy(NSDUH_survey_adults_RC, .data[[input$var_select]], .data[[input$dem_select]], fill = TRUE) +
          labs(y = "Prevalence")  +
          ggtitle(paste0(
            "Bar Chart of ",
            input$var_select,
            " by ",
            input$dem_select
          )) + scale_color_fivethirtyeight()  + theme_fivethirtyeight()
      }
    }
    
    
  })
  
  
  output$bottom_hist <- renderPlot({
    req(input$var_select)
    
    if (input$var_select %in% quantVars) {
      if (input$type == "overall") {
        if (isTRUE(input$LogY)) {
          if (input$var_select == "srs_mhi_prob") {
            probs <-
              seq(
                min(NSDUH_survey_adults_RC$variables$srs_mhi_prob) * .95,
                max(NSDUH_survey_adults_RC$variables$srs_mhi_prob) *
                  1.05,
                length.out = 10
              )
            
            
          } else if (input$var_select == "psych_distress_pm") {
            probs <-
              seq(
                min(
                  NSDUH_survey_adults_RC$variables$psych_distress_pm
                ) * .95,
                max(
                  NSDUH_survey_adults_RC$variables$psych_distress_pm
                ) * 1.05,
                length.out = 10
              )
            
            
          } else {
            probs <-
              seq(

                  0* .95,
                24 * 1.05,
                length.out = 10
              )
            
          }
          
          if (input$var_select == "srs_mhi_prob") {
            gghistweight_svy(NSDUH_survey_adults_RC,
                             x = log(eval(as.name(
                               input$var_select
                             ))),
                             binwidth = .1) +
              scale_x_continuous(labels = rexp2, breaks = log(probs)) +
              scale_y_continuous(labels = function(l) { trans = l / 1000000 ; paste0(trans, "M")} )  + 
              ggtitle(paste0("Histogram of log ",
                             input$var_select)) + scale_color_fivethirtyeight()  + theme_fivethirtyeight() +
              theme(axis.title = element_text()) + xlab(paste("exp(log(", input$var_select, "))"))
            
          } else {
            gghistweight_svy(NSDUH_survey_adults_RC,
                             x = log(eval(as.name(
                               input$var_select
                             ))),
                             binwidth = 0.25) +
              scale_x_continuous(labels = rexp2, breaks = log(probs)) +
              scale_y_continuous(labels = function(l) { trans = l / 1000000 ; paste0(trans, "M")} )  + 
              ggtitle(paste0("Histogram of log ",
                             input$var_select)) + scale_color_fivethirtyeight()  +
              theme_fivethirtyeight() +
              theme(axis.title = element_text()) + xlab(paste("exp(log(", input$var_select, "))"))
            
          }
          
        }  else {
          if (input$var_select == "srs_mhi_prob") {
            gghistweight_svy(NSDUH_survey_adults_RC,
                             x = eval(as.name(input$var_select)),
                             binwidth = .001) +
              scale_y_continuous(labels = function(l) { trans = l / 1000000 ; paste0(trans, "M")} )  + 
              ggtitle(paste0("Histogram of ",
                             input$var_select)) + scale_color_fivethirtyeight()  + 
              theme_fivethirtyeight() +
              theme(axis.title = element_text()) + xlab( input$var_select )
              
            
          } else {
            gghistweight_svy(NSDUH_survey_adults_RC,
                             x = eval(as.name(input$var_select)),
                             binwidth = 1) +
              scale_y_continuous(labels = function(l) { trans = l / 1000000 ; paste0(trans, "M")} )  + 
              ggtitle(paste0("Histogram of ",
                             input$var_select)) + scale_color_fivethirtyeight()  +
              theme_fivethirtyeight() + 
              theme(axis.title = element_text()) + xlab( input$var_select )
            
          }
          
        }
        
      } else {
        req(input$dem_select)
        
        if (isTRUE(input$LogY)) {
          if (input$var_select == "srs_mhi_prob") {
            gghistweight2d_svy(
              NSDUH_survey_adults_RC,
              x = log(eval(as.name(
                input$var_select
              ))),
              y = eval(as.name(input$dem_select)),
              binwidth = .1
            ) + scale_x_continuous(labels = rexp2) +
              ggtitle(paste0(
                "Histogram of log ",
                input$var_select,
                " by ",
                input$dem_select
              )) + scale_y_continuous(labels = function(l) { trans = l / 1000000 ; paste0(trans, "M")} )  + 
              scale_color_fivethirtyeight()  + theme_fivethirtyeight() +
              theme(axis.title = element_text()) + xlab(paste("exp(log(", input$var_select, "))"))
            
          } else {
            gghistweight2d_svy(
              NSDUH_survey_adults_RC,
              x = log(eval(as.name(
                input$var_select
              ))),
              y = eval(as.name(input$dem_select)),
              binwidth = .25
            ) +  scale_x_continuous(labels = rexp2) +
              ggtitle(paste0(
                "Histogram of log ",
                input$var_select,
                " by ",
                input$dem_select
              )) + scale_y_continuous(labels = function(l) { trans = l / 1000000 ; paste0(trans, "M")} )  + 
              scale_color_fivethirtyeight()  + theme_fivethirtyeight() + 
              theme(axis.title = element_text()) + xlab(paste("exp(log(", input$var_select, "))"))
            
          }
          
        } else {
          if (input$var_select == "srs_mhi_prob") {
            gghistweight2d_svy(
              NSDUH_survey_adults_RC,
              x = eval(as.name(input$var_select)),
              y = eval(as.name(input$dem_select)),
              binwidth = .01
            ) + scale_y_continuous(labels = function(l) { trans = l / 1000000 ; paste0(trans, "M")} )  + 
              ggtitle(paste0(
                "Histogram of ",
                input$var_select,
                " by ",
                input$dem_select
              )) + scale_color_fivethirtyeight()  + theme_fivethirtyeight() + 
              theme(axis.title = element_text()) + xlab( input$var_select)
            
          } else {
            gghistweight2d_svy(
              NSDUH_survey_adults_RC,
              x = eval(as.name(input$var_select)),
              y = eval(as.name(input$dem_select)),
              binwidth = 1
            ) +
              ggtitle(paste0(
                "Histogram of ",
                input$var_select,
                " by ",
                input$dem_select
              )) + scale_y_continuous(labels = function(l) { trans = l / 1000000 ; paste0(trans, "M")} )  + 
              scale_color_fivethirtyeight()  + theme_fivethirtyeight() + 
              theme(axis.title = element_text()) + xlab( input$var_select)
            
          }
          
        }
        
      }
      
      
    } else {
      
    }
    
  })
  
  
  
  tableInput <- reactive({

    response_formula <- paste0("~", input$var_select, collapse = "")

    if (input$type == "overall") {
      
      results <- svymean(as.formula(response_formula),
                         design = NSDUH_survey_adults_RC,
                         na.rm = TRUE)
      
      conf_int <- as.data.frame(confint(results))
      conf_int$concat <- paste("(", round(conf_int$`2.5 %`, 3), "," ,round(conf_int$`97.5 %`, 3),")")
      int <- data.frame(CONF.INT = conf_int$concat )
      
      as_df <- as.data.frame(results)
      colnames(as_df) <- c("Mean", "SE")
      
      if (input$var_select %in% quantVars) {
        svy_subset <-
          paste0("NSDUH_survey_adults_RC$variables$",
                 input$var_select,
                 collapse = "")
        TOTAL <-
          sum(NSDUH_survey_adults_RC$variables[!is.na(svy_subset),]$ANALWTQ1Q4_C)
        TOTAL <- round(TOTAL)
        names(as_df) <- gsub("[[:punct:]]", " ", names(as_df))
        names(as_df) <- toupper(names(as_df))
        
        
        if (input$var_select == "srs_mhi_prob") {
          
          if (isTRUE(input$LogY)) {
            
            as.data.frame(cbind(as_df, int, TOTAL = as.data.frame(TOTAL))) %>%
              mutate_if(is.numeric,
                        round,
                        digits = 3)
          } else {
            
            conf_int$concat <- paste("(", round(conf_int$`2.5 %`, 4)*100,"%", "," ,round(conf_int$`97.5 %`, 3)*100,"%)")
            int <- data.frame(CONF.INT = conf_int$concat )
            
            as.data.frame(cbind(as_df, int, TOTAL = as.data.frame(TOTAL))) %>%
              mutate_if(is.numeric,
                        round,
                        digits = 3)

          }
          

          
          
        } else {
          
          
          
          as.data.frame(cbind(as_df, int, TOTAL = as.data.frame(TOTAL))) %>%
            mutate_if(is.numeric,
                      round,
                      digits = 3)
        }
        
      } else {
        rownames(as_df) <- sub(".*None", "None", rownames(as_df))
        rownames(as_df) <- sub(".*Mild", "Mild", rownames(as_df))
        rownames(as_df) <-
          sub(".*Moderate", "Moderate", rownames(as_df))
        rownames(as_df) <-
          sub(".*Serious", "Serious", rownames(as_df))
        
        rownames(as_df) <- sub(".*Yes", "Yes", rownames(as_df))
        rownames(as_df) <- sub(".*No", "No", rownames(as_df))
        
        names(as_df) <- toupper(names(as_df))
        
        
        svytable_df <-
          as.data.frame(svytable(as.formula(response_formula), design = NSDUH_survey_adults_RC))
        TOTAL = svytable_df[, 2]
        TOTAL = round(TOTAL)
        
        ordered_ci <- as.data.frame(conf_int)[order(rownames(conf_int)), ][nrow(conf_int),]
        
        int <- data.frame(CONF.INT = c(rep("-", nrow(conf_int) - 1), ordered_ci$concat) )
        
        as_df <- as_df[order(rownames(as_df)), ]
        
        as.data.frame(cbind(as_df, int, TOTAL))  %>%
          mutate_if(is.numeric,
                    round,
                    digits = 3)

      }
    } else {
      req(input$dem_select)
      
      subpop_formula <-
        paste0("~", input$dem_select,  collapse = "")
      
      results <-
        svyby(
          as.formula(response_formula),
          by = as.formula(subpop_formula) ,
          design = NSDUH_survey_adults_RC,
          svymean,
          na.rm = TRUE,
          keep.names = FALSE
        )
      

      df1 <- as.data.frame(results)
      as_df <- as.data.frame(results)
      conf_int <- as.data.frame(confint(results))
      conf_int$concat <- paste("(", round(conf_int$`2.5 %`, 3), "," ,round(conf_int$`97.5 %`, 3),")")
      int <- data.frame(CONF.INT = conf_int$concat )
      
      
      coldiff <- ((ncol(df1)-1)/2)
      
      df1 <- df1 %>% mutate_if(is.numeric,
                           round,
                           digits = 3)
      
      se_df <- df1[ ,(coldiff + 1 + 1):ncol(df1)]
      
      new_df <- data.frame(df1[ ,1:coldiff+1], lapply(se_df, addparens) )
      
      df2 <- data.frame(matrix(NA, nrow = nrow(df1), ncol = (ncol(df1)-1)/2 + 1 ))
      
      df2[, 1] <- df1[ ,1]
      
      for (i in 1:coldiff) {
        
        newcol <- paste(new_df[ , i],new_df[ , i + coldiff])
        df2[ ,i+1] <- newcol 
      }
      
      colnames(df2) <- colnames(df1)[0:coldiff+1]
      
      
      if (input$var_select %in% quantVars) {
        names(as_df) <- gsub("[[:punct:]]", " ", names(as_df))
        names(as_df) <- toupper(names(as_df))
        
        svytable_df <-
          as.data.frame(svytable(as.formula(subpop_formula), design = NSDUH_survey_adults_RC))
        
        TOTAL = svytable_df[, 2]
        TOTAL = round(TOTAL)
        
        if (input$var_select == "srs_mhi_prob") { 

          if (isTRUE(input$LogY)) {
            
            as.data.frame(cbind(as_df, int, TOTAL = as.data.frame(TOTAL))) %>%
              mutate_if(is.numeric,
                        round,
                        digits = 3)
          } else {
            
            conf_int$concat <- paste("(", round(conf_int$`2.5 %`, 4)*100,"%", "," ,round(conf_int$`97.5 %`, 3)*100,"%)")
            int <- data.frame(CONF.INT = conf_int$concat )
            
            as.data.frame(cbind(as_df, int, TOTAL = as.data.frame(TOTAL))) %>%
              mutate_if(is.numeric,
                        round,
                        digits = 3)
            
          }

          } else {
          
          as.data.frame(cbind(as_df, int, TOTAL))  %>%
            mutate_if(is.numeric,
                      round,
                      digits = 3)
        }
        

      } else {

        var_str_tb <-
          paste0("~",
                 input$var_select,
                 "+",
                 input$dem_select,
                 collapse = "")
        
        svytable_df <-
          as.data.frame(svytable(as.formula(var_str_tb), design = NSDUH_survey_adults_RC))
        
        col_totals <- svytable_df %>%
          group_by(svytable_df[, 2]) %>%
          summarise(TOTAL = sum(Freq))
        
        row_totals <- svytable_df %>%
          group_by(svytable_df[, 1]) %>%
          summarise(Test2 = sum(Freq))

        results2 <-
          cbind(df2, round(as.data.frame(col_totals[, 2])))
        

        row_totals <- as.data.frame(row_totals)
        row_totals[, 1] <-
          paste(names(svytable_df)[1], row_totals[, 1], sep = "")
        
        rownames(row_totals) <- row_totals[, 1]
        row_totals[, 1] <- NULL
        
        l <- list(results2, as.data.frame(round(t(row_totals))))
        
        dt <- rbindlist(l, fill = TRUE)
        
        dt[nrow(as_df) + 1, 1] <- "Total"
        
        dt <- dt %>%  mutate_if(is.numeric,
                                round,
                                digits = 3)
        
        
        colnames(dt) <- sub("^m.*None", "None", colnames(dt))
        colnames(dt) <- sub("^m.*Mild", "Mild", colnames(dt))
        colnames(dt) <-
          sub("^m.*Moderate", "Moderate", colnames(dt))
        colnames(dt) <- sub("^m.*Serious", "Serious", colnames(dt))
        
        colnames(dt) <- sub("^m.*Yes", "yes", colnames(dt))
        colnames(dt) <- sub("^m.*No", "no", colnames(dt))
        
        colnames(dt) <- sub("^l.*Yes", "yes", colnames(dt))
        colnames(dt) <- sub("^l.*No", "no", colnames(dt))
        
        colnames(dt) <- sub("^su.*Yes", "yes", colnames(dt))
        colnames(dt) <- sub("^su.*No", "no", colnames(dt))
        
        colnames(dt) <- sub("^sr.*Yes", "yes", colnames(dt))
        colnames(dt) <- sub("^sr.*No", "no", colnames(dt))
        
        
        names(dt) <- gsub(".mi_cat", " ", names(dt))
        names(dt) <- gsub(".mde_py", " ", names(dt))
        names(dt) <- gsub(".lifetime_mde", " ", names(dt))
        names(dt) <- gsub(".suicide_attempt", " ", names(dt))
        names(dt) <- gsub(".suicide_plan", " ", names(dt))
        names(dt) <- gsub(".suicidal_thoughts", " ", names(dt))
        names(dt) <- gsub(".srs_psych_distress_pm", " ", names(dt))
        names(dt) <- toupper(names(dt))
        
        dt <- replace(dt, is.na(dt), "")
        
        half_int <- int[(nrow(as_df) + 1):(nrow(as_df)*2), ]
        
        
        
        as.data.frame(dt) %>%
          mutate_if(is.numeric,
                    round,
                    digits = 3)
      }
    }
    
  })
  
  
  
  output$table <- renderDT({
    if (national_var() == "srs_mhi_prob" ) {
      if (isTRUE(input$LogY)) {
        datatable(tableInput()) 
      } else {
        if (input$type == "overall") {
          datatable(tableInput()) %>% formatPercentage(1:2, digits=2)
        }
        else {
          datatable(tableInput()) %>% formatPercentage(2:3, digits=2)
        }
      }
    }
    else {
      if (national_var() %in% catVars) {
        datatable(tableInput()) 
      } else {
        datatable(tableInput()) 
      }
    }
    
  })
  
  national_var <- reactive({
    input$var_select 
  })
  
  
  state_var <- reactive({
    input$st_var_select
  })
  
  state_dem_var <- reactive({
    input$st_dem_select
  })
  
  
  observeEvent(input$st_dem_select, {
    if (state_var() == "dep_recode"  &&  state_dem_var() == "IMPRACE") {
      showModal(modalDialog(
        "Depression Diagnosis by Race not available", 
        title = "Warning"
      ) 
      )
    } else {
      
    }
    
  })
  
  
  state_tableInput <- reactive({
    
    
    if (state_var() == "GENHLTH") {
      if (state_dem_var() == "EDUCAG") {
        dt <- as.data.frame(gh_edu_table)  
        colnames(dt) <- sub("EDUCAG", "Level of education completed", colnames(dt))
      } else if (state_dem_var() == "AGE_G") {
        dt <- as.data.frame(gh_age_table) 
        colnames(dt) <- sub("AGE_G", "Age Category", colnames(dt))
      } else if (state_dem_var() == "HLTHPLN") {
        dt <- as.data.frame(gh_HI_table) 
        colnames(dt) <- sub("HLTHPLN", "Adults who had some form of health insurance", colnames(dt))
      } else  {
        dt <- as.data.frame(gh_race_table) 
        colnames(dt) <- sub("IMPRACE", "Race/Ethnicity", colnames(dt))
      }
    } else {
      if (state_dem_var() == "EDUCAG") {
        dt <- as.data.frame(dep_edu_table) 
        colnames(dt) <- sub("EDUCAG", "Level of education completed", colnames(dt))
      } else if (state_dem_var() == "AGE_G") {
        dt <- as.data.frame(dep_age_table) 
        colnames(dt) <- sub("AGE_G", "Age Category", colnames(dt))
      } else  {
        dt <-  as.data.frame(dep_HI_table)
        colnames(dt) <- sub("HLTHPLN", "Adults who had some form of health insurance", colnames(dt))
      }
    }
    
    dt%>%
      mutate_if(is.numeric,
                round,
                digits = 0)
  })
  
  
  output$state_table <- renderDT({
    state_tableInput()
  })
  
  
  stategroups <- reactive({
    
    
    if (state_var()  == "GENHLTH") {
      if (state_dem_var()== "HLTHPLN") {
        GH_HI_split
      } else if (state_dem_var() == "IMPRACE") {
        GH_Race_split
      } else if (state_dem_var() == "AGE_G") {
        GH_Age_split
      } else {
        GH_EDU_split
      }
    }  else if (state_var()  == "dep_recode") {
      if (state_dem_var() == "HLTHPLN") {
        Dep_HI_split
      }  else if (state_dem_var() == "AGE_G") {
        Dep_Age_split
      } else {
        Dep_EDU_split
      }
      
    }
    
  })
  
  
  grouplevels <- reactive({
    
    if (state_var() == "GENHLTH") {
      if (isTRUE(all.equal(stategroups(), GH_HI_split))) {
        c(
          "Have some form of insurance",
          "Do not have some form of health insurance",
          "Don´t know, refused or missing
            insurance
response"
        )
      } else if (isTRUE(all.equal(stategroups(), GH_Race_split))) {
        c(
          "White, Non-Hispanic",
          "Black, Non-Hispanic",
          "Asian, Non-Hispanic",
          "AI/AN, Non-Hispanic",
          "Hispanic",
          "Other Race"
        )
      } else if (isTRUE(all.equal(stategroups(), GH_Age_split))) {
        c(
          "Age 18 to 24",
          "Age 25 to 34",
          "Age 35 to 44",
          "Age 45 to 54",
          "Age 55 to 64",
          "Age 65 or older"
        )
      } else  {
        c(
          "Did not graduate High School",
          "Graduated High School",
          "Attended College or Technical School",
          "Graduated from College or Technical School"
        )
      }
      
    } else {
      if (isTRUE(all.equal(stategroups(), Dep_HI_split))) {
        c(
          "Have some form of insurance",
          "Do not have some form of health insurance",
          "Don´t know, refused or missing
            insurance
response"
        )
      }  else if (isTRUE(all.equal(stategroups(), Dep_Age_split))) {
        c(
          "Age 18 to 24",
          "Age 25 to 34",
          "Age 35 to 44",
          "Age 45 to 54",
          "Age 55 to 64",
          "Age 65 or older"
        )
      } else  {
        c(
          "Did not graduate High School",
          "Graduated High School",
          "Attended College or Technical School",
          "Graduated from College or Technical School"
        )
      }
      
    }
    
  })
  
  
  observeEvent(input$st_dem_select, {
    removeUI(
      selector = "div:has(> #test)"
    )
  }, suspended = TRUE, priority = 1)
  
  
  
  map_groups <- reactive({
    
    lapply(1:length(stategroups()), function(i) {

      states_shape_simple %>%
      mutate_at("STATEFP", as.numeric) %>%
      as("sf") %>%
      left_join(as.data.frame(stategroups()[[i]]), by = c("STATEFP" = "X_STATE")) %>%
      na.omit()
    
  })
    
  })
  
  
  output$grouped_state_table <- renderDT({
    
    merged_group_dfs <- NULL
    spread_df <- NULL
    reduced_group_dfs <- NULL
    
    if (state_var() == "dep_recode") {
      reduced_group_dfs <- lapply(1:length(map_groups()), function(i) {
        as.data.frame(map_groups()[[i]])[ , c("NAME", "STATEFP", state_dem_var(), "dep_recode1")]
      })
      
      merged_group_dfs <- Reduce(full_join, reduced_group_dfs )
      
      spread_df <- merged_group_dfs  %>% spread(state_dem_var(), dep_recode1)
      
      colnames(spread_df) <- c("NAME", "STATEFP", grouplevels() )
      
      spread_df %>%
        mutate_if(is.numeric,
                  round,
                  digits = 3)
      
    } else {
      
      reduced_group_dfs <- lapply(1:length(map_groups()), function(i) {
        as.data.frame(map_groups()[[i]])[ , c("NAME", "STATEFP", state_dem_var(), "GENHLTH")]
      })
      
      merged_group_dfs <-Reduce(full_join,reduced_group_dfs )
      
      spread_df <-  merged_group_dfs %>% spread(state_dem_var(), GENHLTH)
      
      colnames(spread_df) <- c("NAME", "STATEFP", grouplevels() )
      
      spread_df %>%
        mutate_if(is.numeric,
                  round,
                  digits = 3)
      
    }

  })
  
  output$plots <- renderUI({
    
    if (state_var() == "GENHLTH") {
      
      plot_output_list_Whole <- NULL
      tag.map.title <- NULL
      title <- NULL
      states_GENHLTH <- NULL
    
      
      plot_output_list_Whole <- lapply(1:length(stategroups()), function(i) {
        
        states_GENHLTH <- map_groups()[[i]]
        
        tag.map.title <- tags$style(
          HTML(
            "
  .leaflet-control.map-title {
    position: absolute;
    top: 5px;
    left: 100px;
    width: 200px;
    text-align: center;
    padding-left: 5px;
    padding-right: 5px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"
          )
        )
        title <-
          tags$div(tag.map.title, htmlEscape(grouplevels()[i]))
        
        plotnameWhole <- paste("plotwhole", i, sep = "")
        
        mytext <- paste(
          "State ", states_GENHLTH$NAME, "<br/>",
          "Mean: ", round(states_GENHLTH$GENHLTH, 3), "<br/>",
          "SE: ", round(states_GENHLTH$se, 3),
          sep="") %>%
          lapply(htmltools::HTML)
        
        output[[plotnameWhole]] <- renderLeaflet({
          
          pal_GH <-
            colorBin("YlOrRd",
                     domain = c(1, 5),
                     na.color = "#5A5A5A",
                     bins = 9)
          
          leaflet(states_GENHLTH) %>%
            setView(-98.58, 39.82, zoom = 2) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~ pal_GH(GENHLTH),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ), 
              label = mytext, 
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              )
            ) %>%
            leaflet::addLegend(
              "bottomright",
              pal = pal_GH,
              values = ~ GENHLTH,
              title = "General Health",
              opacity = .5
            ) %>% addControl(title, position = "topleft", className =
                               "map-title")
          
        })
        
        leafletOutput(plotnameWhole, width = "450px", height = 200)
        
      })
      
    }
    
    else if (state_var() == "dep_recode"){
      
      plot_output_list_Whole <- NULL
      tag.map.title <- NULL
      title <- NULL
      states_DEP <- NULL
      
      
      plot_output_list_Whole <- lapply(1:length(stategroups()), function(i) {
        
        states_DEP<- map_groups()[[i]]
        
        tag.map.title <- tags$style(
          HTML(
            "
  .leaflet-control.map-title {
    position: absolute;
    top: 5px;
    left: 100px;
    width: 200px;
    text-align: center;
    padding-left: 5px;
    padding-right: 5px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"
          )
        )
        
        title <-
          tags$div(tag.map.title, htmlEscape(grouplevels()[i]))
        
        plotnameWhole <- paste("plotwhole", i, sep = "")
        
        mytext <- paste(
          "State: ", states_DEP$NAME, "<br/>",
          "Mean: ", round(states_DEP$dep_recode1, 3), "<br/>",
          "SE: ", round(states_DEP$se.dep_recode1, 3),
          sep="") %>%
          lapply(htmltools::HTML)
        
        output[[plotnameWhole]] <- renderLeaflet({
          
          pal_DEP <-
            colorBin(
              "YlOrRd",
              domain = c(0, 0.4),
              na.color = "#5A5A5A",
              bins = 9
            )
          
          leaflet(states_DEP) %>%
            setView(-98.58, 39.82, zoom = 2) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~ pal_DEP(dep_recode1),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ), 
              label = mytext, 
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              )
            ) %>%
            leaflet::addLegend(
              "bottomright",
              pal = pal_DEP,
              values = ~ dep_recode1,
              title = "Dep Diagnosis",
              opacity = .5
            ) %>% addControl(title, position = "topleft", className =
                               "map-title")
          
        })
        
        leafletOutput(plotnameWhole, width = "450px", height = 200)
        
      })
      
    }
    
    do.call(tagList, plot_output_list_Whole)
    
    
  })
  
  
  output$response_variable_name <-
    renderText({
      paste0(input$var_select, ":")
    })
  
  survey_question <- reactive({
    if (input$var_select == "psych_distress_pm") {
      "A score with values ranging from 0 to 24 indicating the level of psychological distress over the past 30 days"
    } else if (input$var_select ==  "worst_psych_distress_py") {
      "A score with values ranging from 0 to 24 indicating the level of psychological distress during the worst month
of the past year, that was not the past 30 days."
    } else if (input$var_select == "srs_psych_distress_pm") {
      "Past Month Serious Psychological Distress Indicator "
    } else if (input$var_select == "suicidal_thoughts") {
      "Adult seriously thought about killing self in past year "
    } else if (input$var_select == "suicide_plan") {
      "Adult Made Plans to Kill Self in Past Year"
    }  else if (input$var_select == "suicide_attempt") {
      "Adult Attempted to Kill Self in Past Year"
    }  else if (input$var_select == "mi_cat") {
      "Categorical Mental Illness Indicator"
    }  else if (input$var_select == "lifetime_mde") {
      "Lifetime Major Depressive Episode Indicator: An adult reported experiencing at least five
out of the nine criteria used to define an adult as having had MDE in their lifetime, where at least one of the criteria is a depressed mood or loss of
interest or pleasure in daily activities"
    } else {
      "Identifies an adult as having Major Depressive Episode in the prior year: An adult reported experiencing at least five
out of the nine criteria used to define an adult as having had MDE in the past year, where at least one of the criteria is a depressed mood or loss of
interest or pleasure in daily activities"
    }
  })
  
  output$survey_q <- renderText({
    req(input$var_select)
    survey_question()
  })
  
  output$response_variable_name_2 <-
    renderText({
      paste0(input$st_var_select, ":")
    })
  
  survey_question_2 <- reactive({
    if (input$st_var_select == "GENHLTH") {
      "Self Assessment of general health on a scale of 1 to 5; 1 being 'Excellent' and 5 being 'Poor'"
    } else {
      "Percentage of individuals ever told they had a depressive disorder (including depression, major
          depression, dysthymia, or minor depression)"
    }
  })
  
  output$survey_q_2 <- renderText({
    req(input$st_var_select)
    survey_question_2()
  })
  
  output$confint <- renderText({
    if (input$var_select %in% quantVars) {
      "*CONF.INT represents a 95% confidence interval"
    }
    else {
      "*Data represented as Mean Estimate (SE)"
    }
    
  })
  
  
  output$HotlineText <- renderText("SAMHSA’s National Helpline, 1-800-662-HELP (4357) is a confidential, free, 24-hour-a-day, 365-day-a-year, information service, in English and Spanish, for individuals and 
                                   family members facing mental and/or substance use disorders. If you or someone you know is  in crisis, call or text 988.")
  
}

shinyApp(ui = ui, server = server)