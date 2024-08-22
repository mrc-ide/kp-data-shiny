library(ggplot2)
library(DT)
library(plotly)
library(moz.utils)
library(countrycode)
library(tidyverse)
library(sf)
library(shinyjs)
library(wesanderson)

zenodo_version <- 13144705
data_path <- "data/2024-07-31_key-population-collated-data.xlsx"

all_data <- readxl::read_excel(data_path, sheet = "Data") %>%
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

  raw_text = ifelse(is.na(raw_estimate), 'Private data', raw_text),

  provincial_value_text = case_when(
    indicator == "PSE" & is.na(provincial_value) & is.na(ratio) & raw_text != "Private data" ~ "-",
    indicator == "PSE" & is.na(provincial_value) & !is.na(ratio) & raw_text != "Private data" ~ "PSE proportion from report",
    TRUE ~ provincial_value_text),

  ratio_text = case_when(
    indicator == "PSE" & is.na(provincial_value) & is.na(ratio) & raw_text != "Private data" ~ "Unmatched area",
    TRUE ~ ratio_text),


  # ratio_text = ifelse(is.na(ratio), "", ratio_text)
  )

all_data <- all_data %>%
  mutate(study_idx = ifelse(observation_idx %in% c(2081, 2082, 2083, 2084, 2085), 373, study_idx)) ## why is this happening?

estimates <- readxl::read_excel(data_path, sheet = "Estimates") %>%
  mutate(indicator = case_when(
    indicator == "pse" ~ "PSE",
    indicator == "kpart" ~ "Number on ART",
    indicator == "kplhiv" ~ "Number living with HIV",
    indicator == "pse_count" ~ "PSE count (total)",
    indicator == "pse_urban_count" ~ "PSE count (urban areas)",
    indicator == "pse_prop" ~ "PSE proportion (total)",
    indicator == "pse_urban_prop" ~ "PSE proportion (urban areas)",
    indicator == "plhiv_prop" ~ "Proportion of total PLHIV among KP",
    indicator == "prevalence" ~ "HIV prevalence",
    indicator == "art_coverage" ~ "ART coverage")
    )

has_data <- crossing(kp = unique(all_data$kp),
         indicator = unique(all_data$indicator),
         iso3 = ssa_iso3()
) %>%
  left_join(
    all_data %>%
      distinct(iso3, indicator, kp) %>%
      mutate(has_data = 1)
) %>%
  mutate(has_data = ifelse(is.na(has_data), 0, 1),
         has_data = factor(has_data, labels = c("No", "Yes")),
         indicator = case_when(
           indicator == "ART coverage/VLS" ~ "ART coverage",
           indicator == "PSE" ~ "PSE proportion (urban areas)",
           TRUE ~ indicator
           ))

iso3_sort <- estimates %>%
  filter(indicator == "PSE proportion (urban areas)") %>%
  distinct(country, iso3) %>%
  left_join(region()) %>%
  arrange(four_region) %>%
  mutate(iso3_idx = as.numeric(fct_inorder(country)))

estimates <- estimates %>%
  left_join(has_data) %>%
  left_join(iso3_sort %>% select(iso3, iso3_idx)) %>%
  mutate(xmin = iso3_idx - 0.48,
         xmax = iso3_idx + 0.48)

all_data <- all_data %>%
  left_join(iso3_sort %>% select(iso3, iso3_idx)) %>%
  left_join(region())

grey <- read_sf(grey_areas())

geographies <- read_sf(national_areas()) %>%
  st_make_valid()

bbox <- c(xmin = -17.5327797,
          ymin = -35, ## -46.9697266,
          xmax = 51.4113159179688,
          ymax = 37.3404121398926)

geographies <- st_crop(geographies, bbox) %>%
  mutate(iso3 = area_id)

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")


sources <- read_csv("data/sources.csv", show_col_types = F)

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
                     @media screen and (min-width: 992px) and (max-width: 1199px) {
                       .plot_div {
                        width: 80%;
                        margin-left: 10%
                       }
                     }
                     @media screen and (min-width: 1200px) and (max-width: 1599px) {
                       .plot_div {
                        width: 70%;
                        margin-left: 15%
                       }
                     }
                     @media screen and (min-width: 1600px) {
                       .plot_div {
                        width: 60%;
                        margin-left: 20%
                       }
                     }
                                           '))),
                 useShinyjs(),

  tabPanel(
                        "Introduction",
                        fluidPage(
                          column(12,
                                 tags$div(class = "plot_div",
                                      h2("Population size, HIV prevalence, and antiretroviral therapy coverage among key populations in sub-Saharan Africa: collation and synthesis of survey data 2010-2023"),
                                      tags$div(style = "font-size:16px",
                                        HTML(
                                        "<p><a href = 'https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(24)00236-5/fulltext' target = '_blank'>Stevens, O., Sabin, K., Garcia, S. A., Anderson, … Imai-Eaton, J. W. (2024). Key population size, HIV prevalence, and ART coverage in sub-Saharan Africa: systematic collation and synthesis of survey data, 2010-23. Lancet Global Health, 12. https://doi.org/10.1016/S2214-109X(24)00236-5</a></p>
                                        <p>All data used in this analysis can be <b><a href = 'https://zenodo.org/doi/10.5281/zenodo.10838437' target = '_blank'>downloaded here</a></b></p>

                                        <p>Key population HIV programmes in sub-Saharan Africa (SSA) require epidemiologic information to ensure equitable and equal access to services. We consolidated survey data among female sex workers (FSW), men-who-have-sex-with-men (MSM), people who inject drugs (PWID), and transgender people to estimate national-level <b>key population size, HIV prevalence, and antiretroviral therapy (ART) coverage</b> for mainland SSA.</p>

                             <p>Key population size estimates (KPSE), HIV prevalence, and ART coverage data from 39 SSA countries between 2010-2023 were collated from existing databases and verified against source documents. We used Bayesian mixed-effects spatial regression to model urban KPSE as a proportion of the gender/year/area-matched 15-49 years adult population. We modelled subnational key population HIV prevalence and ART coverage with age/gender/year/province-matched total population estimates as predictors.</p>

                             <p><b>We extracted 2065 key population size, 1183 HIV prevalence, and 259 ART coverage data points.</b> Across national <u>urban</u> populations, a median of <b>1.65% of adult cisgender women were FSW, 0.89% of men were MSM, 0.32% of men injected drugs, and 0.10% of women were transgender</b>. HIV prevalence among key populations was, on average, <b>4 to 6 times higher than matched total population prevalence</b>, and ART coverage was correlated with, but lower than, total population ART coverage with wide heterogeneity in relative ART coverage across studies. <b>Across SSA, key populations were estimated as 1.2% of the total population aged 15-49 years but 6.1% of people living with HIV.</b> </p>

                            <p>Key populations in SSA experience higher HIV prevalence and lower ART coverage, underscoring the need for focused prevention and treatment services. <b>In 2024, limited data availability and heterogeneity constrain precise estimates for programming and monitoring trends.</b> Strengthening key population surveys and routine data within national HIV strategic information systems would support more precise estimates.</p>")
                                      ),
                                      br(),
                                      h3("Availability of key population surveys"),
                                      selectInput("kp_survey_select", "Key Population:", choices = c("FSW", "MSM", "PWID", "TGW"), selected = "FSW"),
                                      plotOutput("dotPlot", height = "750px"),
                                      br()
                            )
                          )
                        )
                      ),
                      tabPanel(
                        "Data",
                        fluidPage(
                          column(12,
                                 tags$div(class = "plot_div",
                                          fluidRow(
                                            column(3, selectInput("country_select", "Country:", choices = sort(unique(all_data$country)), selected = "Angola")),
                                            column(3, selectInput("kp_select", "Key Population:", choices = c("FSW", "MSM", "PWID", "TGW"), selected = "FSW")),
                                            column(3, selectInput("indicator_select", "Indicator:", choices = c("PSE", "HIV prevalence", "ART coverage/VLS"))),
                                            column(3, selectInput("toggle_select", "Data display:", choices = c("Raw data", "Relative to total population"), selected = "Relative to total population"))
                                          ),
                                          plotlyOutput("results_plot", height = "30%"),
                                          fluidRow(
                                            column(5, downloadButton("download_data", "Download table"), offset = 2),
                                            column(5, actionButton("download_full_data", "Download complete dataset", icon("download"), onclick ="window.open('https://zenodo.org/doi/10.5281/zenodo.10838437', '_blank')"
))
                                          ),
                                 ),
                                 br(),
                                 tags$div(class = "plot_div",
                                          DTOutput("results_table")
                                 ),
                                 br()
                                )
                          )
                        ),
                      tabPanel(
                          "Estimates",
                          fluidPage(
                            column(1),
                            column(10,
                                            fluidRow(
                                              div(style = 'display: flex; align-items = center;',
                                              column(3, selectInput("country_select_est", "Country:", choices = c("All countries", sort(unique(all_data$country))), selected = "All countries")),
                                              column(3, selectInput("kp_select_est", "Key Population:", choices = c("FSW", "MSM", "PWID", "TGW"), selected = "FSW")),
                                              column(3, selectInput("indicator_select_est", "Indicator:", choices = unique(estimates$indicator), selected = "HIV prevalence")),
                                              column(2, downloadButton("download_estimates", label = "Download table"))
                                            )),
                                            fluidRow(
                                              column(6,
                                                     br(),
                                                     plotOutput("estimates_plot", height = "70vh", width = "40vw")),
                                              column(6, DTOutput("estimates_table"))
                                            ),
                                            br(),
                                            br()


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
                          column(12,
                            tags$div(class = "plot_div", style = "font-size: 16px",
                              HTML("<p>This dataset should be cited as <a href = 'https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(24)00236-5/fulltext' target = '_blank'>Stevens, O., Sabin, K., Garcia, S. A., Anderson, … Imai-Eaton, J. W. (2024). Key population size, HIV prevalence, and ART coverage in sub-Saharan Africa: systematic collation and synthesis of survey data, 2010-23. Lancet Global Health, 12. https://doi.org/10.1016/S2214-109X(24)00236-5</a>, where further information on data collation and analysis can be found.</p>

                                   <p>All data used in this analysis can be <a href = 'https://zenodo.org/doi/10.5281/zenodo.10838437' target = '_blank'>downloaded here</a></p>")
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

  observe({
    req(input$country_select_est)
    toggleState("kp_select_est", input$country_select_est == "All countries")
  })

  est <- reactive({
    browser()
    estimates %>%
      filter(
        country %in% input$country_select_est,
        kp %in% input$kp_select_est,
        indicator %in% input$indicator_select_est
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
            geom_pointrange(aes(ymin = raw_lower, ymax = raw_upper), position = position_dodge2(width = 0.5), size = 1.5) +
            labs(x=element_blank(), y=y_lab, color = "") +
            expand_limits(y=0) +
            scale_x_continuous(limits = c(2009.8, 2023), breaks = seq(2011, 2023, 2), expand = expansion(add = c(0,0.2))) +
            scale_y_continuous(expand = expansion(mult = c(0,0.02))) +
            geom_hline(aes(yintercept = 0), linewidth = 0.5) +
            geom_vline(aes(xintercept = 2009.8), linewidth = 0.5) +
            standard_theme() +
            theme(panel.grid = element_blank())


       if(input$indicator_select != "PSE")
         p <- p + scale_y_continuous(labels = scales::label_percent(), expand = expansion(c(0,0)))

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
          geom_pointrange(aes(ymin = ratio_lower, ymax = ratio_upper), position = position_dodge2(width = 0.5), size = 1.5) +
          labs(x=element_blank(), y=y_lab, color = "") +
          expand_limits(y=0) +
          scale_x_continuous(limits = c(2009.8, 2023), breaks = seq(2011, 2023, 2), expand = expansion(add = c(0,0.2))) +
          scale_y_continuous(expand = expansion(mult = c(0,0.02))) +
          geom_hline(aes(yintercept = 0), linewidth = 0.5) +
          geom_vline(aes(xintercept = 2009.8), linewidth = 0.5) +
          standard_theme() +
          theme(panel.grid = element_blank())

      if(input$indicator_select == "PSE")
        p <- p +
              scale_y_continuous(labels = scales::label_percent(), expand = expansion(c(0,0.02)))
      else
        p <- p +
              geom_hline(aes(yintercept = 1), linetype = 2) +
              scale_y_continuous(breaks = sort(c(pretty(c(1, data()$ratio_lower, data()$ratio, data()$ratio_upper), min.n = 3), 1)), expand = expansion(mult = c(0,0.02)))

      ggplotly(p, tooltip = "text")

    }

  })

  ##### DATA TABLE

  data_table <- reactive({

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
      left_join(sources %>%
                    filter(kp %in% c(input$kp_select, "ALL"),
                           country == input$country_select) %>%
                    select(iso3, study_idx, link) %>%
                    mutate(link_study = ifelse(is.na(link), study_idx, paste0("<a href='",link, "' target = '_blank'>",study_idx,"</a>"))) %>%
                    select(study_idx, link_study) %>%
                    group_by(study_idx) %>%
                    summarise(link_study = toString(link_study))) %>%
      arrange(year, study_idx) %>%
      select(KP = kp, Area = study_area, Year = year, Indicator = indicator, Method = method, `Estimate (95% CI)` = raw_text, provincial_value_text, ratio_text, Denominator = sample_size, `Study ID` = link_study) %>%
      rename_with(~paste(match), starts_with("provincial_value")) %>%
      rename_with(~paste(rel_est), starts_with("ratio"))
  })

  output$results_table <- renderDT({

    if (input$indicator_select == "PSE")
      out <- data_table() %>%
        select(-Denominator)
    else
      out <- data_table()

    datatable(out,
              options = list(dom = "t",
                                  pageLength = 10000),
              escape = F,
              rownames = F)

  })

  output$download_data <- downloadHandler(

    filename = function() {
      paste("kp_data_", zenodo_version, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(data_table() %>%
                  left_join(sources %>% select(`Study ID` = study_idx, study, link)), file, na = "")
    }
  )

  ##### ESTIMATES PLOT

  type_df <- distinct(estimates, indicator) %>%
    mutate(type = ifelse(indicator %in% c("Number on ART", "Number living with HIV", "PSE count (total)", "PSE count (urban areas)"),
                         "count",
                         "proportion"))

  output$estimates_plot <- renderPlot({

    if(input$country_select_est == "All countries") {

      if(input$indicator_select_est != "Proportion of total PLHIV among KP") {

        kp_accuracy <-  c("FSW" = 0.1,  "MSM" = 0.1, "PWID" = 0.01 , "TGW" = 0.01)
        accuracy <- kp_accuracy[input$kp_select_est]

        p <- estimates %>%
          filter(kp == input$kp_select_est,
                 indicator == input$indicator_select_est) %>%
          left_join(select(geographies, iso3)) %>%
          ggplot() +
          geom_sf(data = grey, aes(geometry = geometry), fill="darkgrey", size = 0.15) +
          geom_sf(aes(geometry = geometry, fill=median), size = 0.15) +
          # viridis::scale_fill_viridis(labels = scales::label_percent()) +
          labs(fill = element_blank()) +
          coord_sf(expand = FALSE) +
          theme_minimal(6) +
          theme(
            axis.text = element_blank(),
            # plot.margin(0, 0, 0, 0),
            legend.position = "bottom",
            legend.key.width = unit(4, "lines"),
            legend.key.height = unit(2, "lines"),
            legend.text = element_text(size = rel(2.4)),
            legend.title = element_text(size = rel(2.2), face = "bold"),
            legend.box.spacing = unit(0, "points"),
            legend.margin = margin(1, 0, 0, 0, "lines"),
            panel.grid.major = element_line(colour = 'transparent'),
            plot.background = element_rect(color = NA)
          )

        type <- filter(type_df, indicator == input$indicator_select_est)$type

        if(type == "count") {

          p <- p +
            scale_fill_gradientn(colours = rev(pal), trans = "log", labels = scales::label_number(accuracy = 100, big.mark = ","))

        } else {

          if(input$indicator_select_est == "PSE proportion (total)" || input$indicator_select_est == "PSE proportion (urban areas)") {
            kp_accuracy <-  c("FSW" = 0.1,  "MSM" = 0.1, "PWID" = 0.01 , "TGW" = 0.01)
            accuracy <- kp_accuracy[input$kp_select_est]
          } else {
            accuracy = 1
          }

          p <- p +
            scale_fill_gradientn(colours = rev(pal), labels = scales::label_percent(accuracy = accuracy), limits = c(0, NA))
        }

      } else {

        plot_order <- c("SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ERI", "ETH", "GAB", "COG", "GNQ", "COD", "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "BWA", "ZWE", "NAM", "SWZ", "LSO", "ZAF")

        p <- estimates %>%
          filter(indicator == "Proportion of total PLHIV among KP") %>%
          name_kp(F) %>%
          # filter(iso3 != "ZAF",
          #        indicator == "kplhiv") %>%
          ggplot(aes(x=fct_rev(fct_relevel(iso3, plot_order)), y=median, fill=fct_rev(kp))) +
          geom_col(position = "stack", show.legend = F) +
          standard_theme() +
          scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, .05))) +
          scale_x_discrete(labels = ~countrycode::countrycode(sourcevar = .x, origin = "iso3c", destination = "country.name", custom_match = moz.utils::cc_plot()), expand = expansion(mult = c(0.02, .02))) +
          scale_manual("fill", 4) +
          labs(x=element_blank(), y="Proportion of total people living with HIV", fill=element_blank()) +
          coord_flip() +
          theme(legend.position = "right",
                axis.text.x = element_text(size = rel(1.3)),
                axis.text.y = element_text(size = rel(1.3)),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank()
          )

      }
    } else {

      # browser()

      type <- filter(type_df, indicator == input$indicator_select_est)$type

      country_est <- estimates %>%
        filter(country == input$country_select_est,
               indicator == input$indicator_select_est) %>%
        name_kp(T)

      if(type == "count") {
        p <- country_est %>%
          ggplot(aes(x=kp, y=median)) +
          geom_col(fill = wesanderson::wes_palette("Zissou1")[1]) +
          geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(.9)) +
          standard_theme() +
          scale_y_continuous(labels = scales::label_number(big.mark = ","), expand = expansion(c(0, 0.05))) +
          labs(y=unique(country_est$indicator), x=element_blank()) +
          theme(panel.grid = element_blank(),
                axis.text.x = element_text(size = rel(1.3), face = "bold"),
                axis.title.y = element_text(size = rel(1.3), face = "bold"),
                axis.text.y = element_text(size = rel(1.4), face = "bold"),
                plot.margin = margin(0,2,0,0, "lines"))
      } else {
        p <- country_est %>%
          ggplot(aes(x=kp, y=median)) +
          geom_col(fill = wesanderson::wes_palette("Zissou1")[1]) +
          geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(.9)) +
          standard_theme() +
          scale_y_continuous(labels = scales::label_percent(), expand = expansion(c(0, 0.05))) +
          labs(y=unique(country_est$indicator), x=element_blank()) +
          theme(panel.grid = element_blank(),
                axis.text.x = element_text(size = rel(1.3), face = "bold"),
                axis.title.y = element_text(size = rel(1.3), face = "bold"),
                axis.text.y = element_text(size = rel(1.4), face = "bold"),
                plot.margin = margin(0,2,0,0, "lines"))
      }



    }

    p
      # ggplotly(p) %>%
      #   style(hoveron = 'fill')


    })

    ###### ESTIMATES TABLE

  estimates_table <- reactive({
    type <- filter(type_df, indicator == input$indicator_select_est)$type

    if(input$country_select_est == "All countries") {
      dat <- estimates %>%
        filter(kp == input$kp_select_est,
               indicator == input$indicator_select_est)

    } else {
      dat <- estimates %>%
        filter(
          country == input$country_select_est,
          indicator == input$indicator_select_est)
    }

    dat <- dat %>%
      rowwise() %>%
      mutate(
        across(c(median, lower, upper), ~ifelse(type == "proportion", .x*100, round(.x, -2))),
        txt = ifelse(type == "proportion",
                     sprintf("%0.2f (%0.2f, %0.2f)", median, lower, upper),
                     sprintf("%d (%d, %d)", median, lower, upper)
        )
      ) %>%
      select(Country = country, KP = kp, Indicator = indicator, `Estimate\n(95%CI)` = txt)

  })

    output$estimates_table <- renderDT({

      datatable(estimates_table(),
                options = list(pageLength = 20,
                               dom = "tp"),
                escape = F,
                rownames = F)
    })

    output$download_estimates <- downloadHandler(

      filename = function() {
        paste("kp_estimates_", zenodo_version, ".csv", sep = "")
      },
      content = function(file) {
        write_csv(estimates_table(), file, na = "")
      }
    )


  output$sources_table <- renderDataTable({

    sources <- sources %>%
      mutate(study = ifelse(is.na(link), study, paste0("<a href='", link, "' target = '_blank'>", study,"</a>")),
             study_idx = as.character(study_idx),
             kp = str_replace(kp, "\\bTG\\b", "TGW")) %>%
      select(`Study ID` = study_idx, `ISO-3 code` = iso3, Country = country, KP = kp, Year = year, Author = author, `Study name` = study, `Publicly available\nreport` = public_report)

    datatable(sources, options = list(pageLength = 10000), escape = F, filter = "top", selection = "multiple", rownames = F)
  })

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
