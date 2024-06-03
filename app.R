library(ggplot2)
library(DT)
library(plotly)
library(moz.utils)
library(countrycode)
library(tidyverse)
library(shinybrowser)

zenodo_version <- 10844137

all_data <- readxl::read_excel("data/2024-03-20_key-population-collated-data.xlsx", sheet = "Data") %>%
  mutate(indicator = case_when(
    indicator == "pse" ~ "PSE",
    indicator == "prevalence" ~ "HIV prevalence",
    indicator == "art_coverage" ~ "ART coverage/VLS"
  ),
  method = case_when(
    method %in% c("mapping", "PLACE") ~ "PLACE/Mapping",
    method == "lab" & indicator == "HIV prevalence" ~ "Diagnostically confirmed",
    method == "lab" & indicator == "ART coverage/VLS" ~ "ART metabolite testing",
    method == "self-report" ~ "Self-report",
    method == "vls" ~ "VLS",
    method %in% c("Unique object multiplier", "Object Multiplier") ~ "Object multiplier",
    method == "Service Multiplier" ~ "Service multiplier",
    TRUE ~ method
  ),
  raw_estimate = ifelse(indicator == "PSE", count_estimate, proportion_estimate),
  raw_lower = ifelse(indicator == "PSE", count_lower, proportion_lower),
  raw_upper = ifelse(indicator == "PSE", count_upper, proportion_upper),
  provincial_value = ifelse(indicator == "PSE", population, provincial_value),
  ratio = ifelse(indicator == "PSE", proportion_estimate, ratio),
  ratio_lower = raw_lower/provincial_value,
  ratio_upper = raw_upper/provincial_value,

  ratio_text = ifelse(indicator == "PSE",
                      str_trim(sprintf("%0.1f (%0.1f, %0.1f)", 100*ratio, 100*ratio_lower, 100*ratio_upper)),
                      str_trim(sprintf("%0.1f (%0.1f, %0.1f)", ratio, ratio_lower, ratio_upper))),
         # ratio_text = str_trim(str_remove(ratio_text, "NA")),
  raw_text = ifelse(indicator == "PSE",
                           str_trim(sprintf("%1.0f (%1.0f, %1.0f)", raw_estimate, raw_lower, raw_upper)),
                           str_trim(sprintf("%0.1f (%0.1f, %0.1f)", 100*raw_estimate, 100*raw_lower, 100*raw_upper))),
         # raw_text = str_trim(str_remove(raw_text, "\\(NA, NA\\)")),
  provincial_value_text = ifelse(indicator == "PSE",
                                        str_trim(sprintf("%1.0f", provincial_value)),
                                        str_trim(sprintf("%0.1f", 100*provincial_value))),
         # provincial_value_text = str_trim(str_remove(provincial_value_text, "\\(NA, NA\\)")),
  across(c(ratio_text, raw_text, provincial_value_text), ~str_remove_all(.x, "NA|\\(\\, \\)|\\(NA, NA\\)|")),

  provincial_value_text = case_when(
    indicator == "PSE" & is.na(provincial_value) & is.na(ratio) ~ "-",
    indicator == "PSE" & is.na(provincial_value) & !is.na(ratio) ~ "PSE proportion from report",
    TRUE ~ provincial_value_text),

  ratio_text = case_when(
    indicator == "PSE" & is.na(provincial_value) & is.na(ratio) ~ "Unmatched area",
    TRUE ~ ratio_text),

  raw_text = ifelse(is.na(raw_estimate), 'Private data', raw_text)


  # ratio_text = ifelse(is.na(ratio), "", ratio_text)
  )

all_data <- all_data %>%
  mutate(study_idx = ifelse(observation_idx %in% c(2081, 2082, 2083, 2084, 2085), 373, study_idx)) ## why is this happening?


sources <- read_csv("data/sources.csv")

if(length(all_data$study_idx[!all_data$study_idx %in% sources$study_idx]))
  stop("Study IDs in data not in source sheet")

# "Population size, HIV prevalence, and antiretroviral therapy coverage among key populations in sub-Saharan Africa: collation and synthesis of survey data 2010-2023"

# Define UI
ui <- navbarPage("KP Data",
                 tags$head(tags$style(HTML('
                     .navbar-nav {
                        font-size: 18px;
                     }
                     .navbar-header {
                        font-size: 20px;
                     }
                     .shiny-output-error-validation {
                        color: "black";
                        font-size: 20px;
                        font-weight: bold;
                        width: 80%;
                        margin-left: 40%;
                        margin-top: 10%
                     }
                     #download_data, #download_full_data {
                        # width: 200px;
                        # height: 50px;
                        font-size: 20px;
                        margin: 5px;
                        text-align: center

                     }
                                           '))),

  tabPanel(
                        "Introduction",
                        fluidPage(
                          column(12,
                                 tags$div(style = "width: 80%; margin-left: 10%",
                                      h2("Population size, HIV prevalence, and antiretroviral therapy coverage among key populations in sub-Saharan Africa: collation and synthesis of survey data 2010-2023"),
                                      tags$div(style = "font-size:16px",
                                        HTML("<p>Key population HIV programmes in sub-Saharan Africa (SSA) require epidemiologic information to ensure equitable and equal access to services. We consolidated survey data among female sex workers (FSW), men-who-have-sex-with-men (MSM), people who inject drugs (PWID), and transgender people to estimate national-level <b>key population size, HIV prevalence, and antiretroviral therapy (ART) coverage</b> for mainland SSA.</p>

                             <p>Key population size estimates (KPSE), HIV prevalence, and ART coverage data from 39 SSA countries between 2010-2023 were collated from existing databases and verified against source documents. We used Bayesian mixed-effects spatial regression to model urban KPSE as a proportion of the gender/year/area-matched 15-49 years adult population. We modelled subnational key population HIV prevalence and ART coverage with age/gender/year/province-matched total population estimates as predictors.</p>

                             <p><b>We extracted 2065 key population size, 1183 HIV prevalence, and 259 ART coverage data points.</b> Across national <u>urban</u> populations, a median of <b>1.65% of adult cisgender women were FSW, 0.89% of men were MSM, 0.32% of men injected drugs, and 0.10% of women were transgender</b>. HIV prevalence among key populations was, on average, <b>4 to 6 times higher than matched total population prevalence</b>, and ART coverage was correlated with, but lower than, total population ART coverage with wide heterogeneity in relative ART coverage across studies. <b>Across SSA, key populations were estimated as 1.2% of the total population aged 15-49 years but 6.1% of people living with HIV.</b> </p>

                            <p>Key populations in SSA experience higher HIV prevalence and lower ART coverage, underscoring the need for focused prevention and treatment services. <b>In 2024, limited data availability and heterogeneity constrain precise estimates for programming and monitoring trends.</b> Strengthening key population surveys and routine data within national HIV strategic information systems would support more precise estimates.</p>")
                                      ),
                                      br(),
                                      h3("Availability of key population surveys"),
                                      selectInput("kp_survey_select", "Key Population:", choices = c("FSW", "MSM", "PWID", "TGW"), selected = "FSW"),
                                      plotOutput("dotPlot", height = "800px")
                            )
                          )
                        )
                      ),
                      tabPanel(
                        "Data",
                        fluidPage(
                          column(12,
                                 tags$div(style = "width: 60%; margin-left: 20%",
                                          fluidRow(
                                            column(3, selectInput("country_select", "Country:", choices = sort(unique(all_data$country)), selected = "Angola")),
                                            column(3, selectInput("kp_select", "Key Population:", choices = c("FSW", "MSM", "PWID", "TGW"), selected = "FSW")),
                                            column(3, selectInput("indicator_select", "Indicator:", choices = c("PSE", "HIV prevalence", "ART coverage/VLS"))),
                                            column(3, selectInput("toggle_select", "Data display:", choices = c("Raw data", "Relative to total population"), selected = "Relative to total population"))
                                          ),
                                          plotlyOutput("results_plot", height = "30%"),
                                          fluidRow(
                                            column(5, downloadButton("download_data", "Download filtered dataset"), offset = 2),
                                            column(5, downloadButton("download_full_data", "Download complete dataset"))
                                          ),
                                 ),
                                 tags$div(style = "width: 80%; margin-left: 10%",
                                          DTOutput("results_table")
                                 )
                                )
                          )
                        ),
                      tabPanel(
                        "Sources",
                        fluidPage(
                          column(12,
                                 tags$div(style = "width: 80%; margin-left: 10%",
                                  tags$div(style = "font-size: 16px", HTML("<p>Any publicly available sources without hyperlinks below can be requested from <b>kpestimates@unaids.org</b></p>")),
                                  br(),
                                  dataTableOutput("sources_table")

                          )
                        )
                      )),
                      tabPanel(
                        "About",
                        fluidPage(
                          fluidRow(
                            tags$div(style = "font-size: 16px",
                              HTML("<p>This dataset should be cited as Stevens et al. (2024). Population size, HIV prevalence, and antiretroviral therapy coverage among key populations in sub-Saharan Africa: collation and synthesis of survey data 2010-2023. MedRxiv, 2022.07.27.22278071. <a href = https://doi.org/10.1101/2022.07.27.22278071 target = '_blank'>https://doi.org/10.1101/2022.07.27.22278071</a>, where further information on data collation and analysis can be found.</p>

                                   <p>All data used in this analysis can be <a href = 'https://zenodo.org/records/10844137' target = '_blank'>downloaded here</a></p>")
                            )
                          )
                        )
                      )

)

# Define server logic
server <- function(input, output, session) {

  observe({
    key_populations <- unique(all_data %>% filter(country == input$country_select) %>% pull(kp))
    y <- c("FSW", "MSM", "PWID", "TGW")
    key_populations <- key_populations[order(match(key_populations, y))]
    updateSelectInput(session, "kp_select", choices = key_populations)
  })

  observe({
    indicators <- unique(all_data %>% filter(country == input$country_select, kp == input$kp_select) %>% pull(indicator))
    y <- c("PSE", "HIV prevalence", "ART coverage/VLS")
    indicators <- indicators[order(match(indicators, y))]
    updateSelectInput(session, "indicator_select", choices = indicators)
  })

  survey_availability <- reactive({
    all_data %>%
      filter(kp == input$kp_survey_select) %>%
      distinct(iso3, indicator, year) %>%
      mutate(iso3 = factor(iso3, levels = ssa_iso3()))
  })

  data <- reactive({
    all_data %>%
      filter(
        country %in% input$country_select,
        kp %in% input$kp_select,
        indicator %in% input$indicator_select
      )
  })

  output$dotPlot <- renderPlot({
    # browser()
    survey_availability() %>%
      ggplot(aes(x = year, y = fct_rev(iso3))) +
      geom_point(size = 2.5, color = wesanderson::wes_palette("Zissou1")[1]) +
      scale_x_continuous(breaks = c(2012, 2016, 2020), limits = c(2010, 2023)) +
      scale_y_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name", custom_match = cc_plot()),
                       drop = F) +
      facet_wrap(~factor(indicator, levels = c("PSE", "HIV prevalence", "ART coverage/VLS"),
                         labels = c("Population size estimate", "HIV prevalence", "ART coverage/VLS"))) +
      # ggh4x::facet_grid2(kp~factor(indicator, levels = c("Population size estimate", "HIV prevalence", "ART coverage/VLS")), remove_labels = "y") +
      no_labels() +
      standard_theme() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            strip.text = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 20, face = "bold"),
            axis.text.y = element_text(size = 18))
  })

  output$results_plot <- renderPlotly({

    if(input$toggle_select == "Raw data") {

      validate(
        need(nrow(data() %>% filter(!is.na(raw_estimate))) != 0,
             "No data available"
             )
      )

      if(input$indicator_select == "HIV prevalence") {
        y_lab <- "HIV prevalence"
      } else if(input$indicator_select == "ART coverage/VLS") {
        y_lab <- "ART coverage/VLS"
      } else {
        y_lab <- "Population size estimate"
      }

       p <- data() %>%
          ggplot(aes(x=year, y=raw_estimate, color = method,
                     text = paste("Area Name: ", study_area, "<br>Estimate (95%CI): ", raw_text, "<br>Study ID: ", study_idx))) +
            geom_pointrange(aes(ymin = raw_lower, ymax = raw_upper), position = position_dodge2(width = 0.5), size = 1) +
            labs(x=element_blank(), y=y_lab) +
            expand_limits(y=0) +
            scale_x_continuous(limits = c(2010, 2023)) +
            standard_theme()

       if(input$indicator_select != "PSE")
         p <- p + scale_percent()

       ggplotly(p, tooltip = "text")

    } else {

      validate(
        need(nrow(data() %>% filter(!is.na(ratio))) != 0,
             "No data available"
        )
      )

      if(input$indicator_select == "HIV prevalence") {
        y_lab <- "KP:total population\nHIV prevalence ratio"
      } else if(input$indicator_select == "ART coverage/VLS") {
        y_lab <- "KP:total population\nART coverage ratio"
      } else {
        y_lab <- "PSE proportion (%)"
      }

      p <- data() %>%
        ggplot(aes(x=year, y=ratio, color = method,
                   text = paste("Area Name: ", study_area, "<br>Estimate (95%CI): ", ratio_text, "<br>Study ID: ", study_idx))) +
          geom_pointrange(aes(ymin = ratio_lower, ymax = ratio_upper), position = position_dodge2(width = 0.5), size = 1) +
          labs(x=element_blank(), y=y_lab) +
          expand_limits(y=0) +
          scale_x_continuous(limits = c(2010, 2023)) +
          standard_theme()

      if(input$indicator_select == "PSE")
        p <- p + scale_percent()
      else
        p <- p + geom_hline(aes(yintercept = 1), linetype = 2)

      ggplotly(p, tooltip = "text")

    }

  })

  # dt <- reactiveVal()

  dt <- reactive({

    # browser()
    if (input$indicator_select == "PSE") {
      match <- "Total population size"
      rel_est <- "PSE proportion (%)"
    } else if (input$indicator_select == "HIV prevalence") {
      match <- "Total population HIV prevalence (%)"
      rel_est <- "HIV prevalence ratio"

    } else {
      match <- "Total population ART coverage (%)"
      rel_est <- "ART coverage ratio"
    }

    data() %>%
      select(KP = kp, Area = study_area, Year = year, Indicator = indicator, Method = method, `Estimate (95% CI)` = raw_text, provincial_value_text, ratio_text, Denominator = sample_size, `Study ID` = study_idx) %>%
      rename_with(~paste(match), starts_with("provincial_value")) %>%
      rename_with(~paste(rel_est), starts_with("ratio"))
  })


  output$results_table <- renderDT({

    if (input$indicator_select == "PSE")
      out <- dt() %>%
        select(-Denominator)
    else
      out <- dt()

    datatable(out, options = list(pageLength = 100))

  })

  function(text, link) {
    as.character(tags$a(text, href=link))
  }

  output$sources_table <- renderDataTable({

    sources <- sources %>%
      mutate(study = ifelse(is.na(link), study, paste0("<a href='",sources$link, "' target = '_blank'>",sources$study,"</a>")),
             study_idx = as.character(study_idx),
             kp = str_replace(kp, "TG", "TGW")) %>%
      select(`Study ID` = study_idx, `ISO-3 code` = iso3, Country = country, KP = kp, Year = year, Author = author, `Study name` = study, `Publicly available\nreport` = public_report)

    datatable(sources, options = list(pageLength = 100), escape = F, filter = "top", selection = "multiple", rownames = F)
  })

  output$download_data <- downloadHandler(

    filename = function() {
      paste("kp_data_", zenodo_version, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(dt() %>%
                  left_join(sources %>% select(`Study ID` = study_idx, study, link)), file, na = "")
    }
  )

  # ncol() <- observe({
  #   if(get_width() > 2000)
  #     6
  #   else
  #     8
  # })
  #
  # output$ncol <- renderPrint(
  #   if(is.null(ncol()))
  #     12
  #   else
  #     ncol()
  # )

}

# Run the application
shinyApp(ui = ui, server = server)
