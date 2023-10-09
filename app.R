library(shiny)
library(bslib)
library(DT)
library(plotly)
library(crosstalk)
library(dplyr)
library(lubridate)
library(swimplot)
library(clinUtils)
library(ggplot2)
data(dataADaMCDISCP01)

dataADaM <- dataADaMCDISCP01
names(dataADaM)
labelVarsADaM <- attr(dataADaM, "labelVars")
adsl <- dataADaM$ADSL
adsl <-
  adsl %>% select(c(
    "USUBJID",
    "SEX",
    "RACE",
    "ETHNIC",
    "TRT01P",
    "TRT01A",
    "TRTSDT",
    "TRTEDT"
  ))
adsl <- as.data.frame(unclass(adsl), stringsAsFactors = TRUE)

adae <- dataADaM$ADAE %>%
  mutate(
    ASTDY = if_else(ASTDY < 0, 0, ASTDY),
    AENDY = if_else(is.na(AENDY), as.numeric(TRTEDT - TRTSDT) + 1, AENDY),
    AEONGO = if_else(is.na(AENDY), 'Y', 'N')
  ) %>%
  group_by(USUBJID,
           AEDECOD,
           ASTDY,
           AENDY,
           ADURN,
           AESEV,
           AESER,
           AEREL,
           TRTSDT,
           TRTEDT) %>%
  slice(1) %>%
  ungroup()

adcm <- dataADaM$ADCM %>% 
  # filter(!CMDECOD %in% 'UNCODED') %>% 
  mutate(
    ASTDY = if_else(ASTDY < 0, 0, ASTDY),
    AENDY = if_else(is.na(AENDY), as.numeric(TRTEDT - TRTSDT) + 1, AENDY),
    ADURN = AENDY - ASTDY +1
  ) %>%
  group_by(USUBJID,
           CMDECOD,
           CMTRT,
           ASTDY,
           AENDY,
           ADURN,
           CMCLAS,
           TRTSDT,
           TRTEDT) %>%
  slice(1) %>%
  ungroup()

card_dm <- card(card_header('Demographics'),
                # fill = TRUE,
                # height = 160,
                min_height = 50,
                max_height = 160,
                # card_body(
                  tableOutput('dm_data'),
                  # min_height = 100)
                )


card_ae <- card(card_header('Plots'),
                min_height = 200,
                max_height = 500,
                fill = F,
                card_body(
                  min_height = 200,
                  max_height = 400,
                  # fill = T,
                  plotlyOutput('ae_plot')
                ),
                card_body(
                  min_height = 200,
                  max_height = 400,
                  # fill = T,
                  plotlyOutput('cm_plot')
                ),
                  # plotlyOutput('ae_plot'),
                  # plotlyOutput('cm_plot'),
                
                  # height = 450
                # )
                )

# card_cm <- card(
#   'CM Plot',
#   fill = F,
#   card_body(
#     min_height = 200,
#     max_height = 400,
#     # fill = T,
#     plotlyOutput('cm_plota')
#   ),
# )

card_acc <- card(
  card_header('Metadata'),
  min_height = 400,
  max_height = 800,
  fill = TRUE,
  accordion(open = FALSE,
            accordion_panel('AE Metadata',
                            tableOutput("ae_meta"))),
  accordion(open = FALSE,
            accordion_panel('CM Metadata',
                            tableOutput("cm_meta"))),
)

ui <- page_navbar(
  id = 'tabs',
  theme = bslib::bs_theme(version = 5),
  title = "Patient Profile           ",
  fillable = TRUE,
  fillable_mobile = TRUE,
  selected = 'Demographic',
  
  # sidebar = sidebar(
  #   title = "Subject Select ",
  #   textInput("usubjid_input", "Enter USUBJID:"),
  #   actionButton("go_button", "Go")
  # ),
  nav_panel(
    "Demographic",
     card(
        card_header(h4('Demographic Table')),
        layout_sidebar(
          sidebar = sidebar(title = "Subject Select ",
                            textInput("usubjid_input", "Enter USUBJID:"),),
          DTOutput("demographics_table")
        )
      )),
  
  nav_panel(
    "Plots",
    layout_column_wrap(width = 1,
                       gap = 1,
                       # height = 2000,
                       # card_ex,
                       card_dm,
                       card_ae,
                       # card_cm,
                       card_acc,
                       fill = T,
                       fillable = F
                       ),
    ),
  )
  
server <- function(input, output, session) {
  
  bslib::bs_themer() # Add the theme customizer
  
  # Filtered data with the USUBJID user clicked
  filtered_data <- reactive({
    # If input$usubjid_input is empty, return all data; otherwise, filter based on partial match to input$usubjid_input
    if (input$usubjid_input == "" | is.null(input$usubjid_input) | is.na(input$usubjid_input)) {
      return(adsl)
    } else {
      return(adsl %>% filter(grepl(trimws(input$usubjid_input), USUBJID, ignore.case = TRUE)))
    }
  })
  
  
  # Render demographics table
  output$demographics_table <- renderDT({
    datatable(
      filtered_data(),
      selection = 'single',
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis'),
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        searchHighlight = TRUE,
        columnDefs = list(list(
          targets = 1,
          render = JS(
            "function(data, type, row, meta) {
                return '<a href=\"pdfs/subjectProfile-' + data + '.pdf\" download>' + data + '</a>';
              }"
          )
        ))
      ),
      filter = 'top',
      escape = FALSE
    )
  })
  # output$demographics_table <- renderDT({
  #   datatable(filtered_data(), selection = "single", rownames = FALSE)
  # })

  # Reactive value to store clicked USUBJID
  clicked_usubjid <- reactiveVal()
  
  observeEvent(input$demographics_table_cell_clicked, {
    # Get the clicked USUBJID
    clicked_usubjid(filtered_data()[input$demographics_table_rows_selected, "USUBJID"])
    
    updateTabsetPanel(session, "tabs", selected = "Plots")
    
  })
  
  # Reactive expression for AE data
  ae_data <- reactive({
    ae <- adae %>%
      filter(USUBJID == clicked_usubjid()) %>%
      mutate(TOOLTIP = paste0('USUBJID', USUBJID, '<br>',
                              'TRTSDT', TRTSDT, '<br>'))
    return(ae)
  })
  
  # Reactive expression for CM data
  cm_data <- reactive({
    # browser()
    cm <- adcm %>%
      filter(USUBJID == clicked_usubjid()) %>%
      mutate(TOOLTIP = paste0('USUBJID', USUBJID, '<br>',
                              'TRTSDT', TRTSDT, '<br>'))
    # print(cm)
    return(cm)
    
  })
  # Render DM table
  output$dm_data <- renderTable({
    # Get AE data
    dmdata <- ae_data() %>%
      select(
        USUBJID,
        Treatment = TRTA,
        Age = AGE,
        Race = RACE,
        Gender = SEX,
        TRTSDT,
        TRTEDT
      ) %>%
      slice(1) %>%
      mutate(
        Age = as.integer(Age),
        TRTSDT = as.character(as.Date(TRTSDT, "%Y-%m-%d")),
        TRTEDT = as.character(as.Date(TRTEDT, "%Y-%m-%d"))
      )
    
    return(dmdata)
  })
  
  # Extract range
  xmin <- reactive({
    return(min(c(ae_data()$ASTDY, cm_data()$ASTDY)))
  })
  
  xmax <- reactive({
    return(max(c(ae_data()$AENDY, cm_data()$AENDY)))
  })
  
  # Render AE plot
  output$ae_plot <- renderPlotly({
    # Get AE data
    ae <- ae_data()
    # -----------ggplotly approach----------------------------------
    # Create ggplot object
    ae <- ae %>%
      arrange(AEDECOD, ASTDY) %>%
      mutate(
        AEDECOD = factor(AEDECOD, levels = unique(AEDECOD)),
        AESEV = factor(AESEV, levels = unique(AESEV))
      ) 
    # ae$AESEV <- as.factor(as.character(ae$AESEV))
    # Create a factor variable with levels ordered by the start time of the first event for each ID
    # ae$AEDECOD <- factor(ae$AEDECOD, levels = unique(ae$AEDECOD))
    # ae$AESEV <- factor(ae$AESEV, levels = unique(ae$AESEV))
    # ae_levels <- levels(ae$AEDECOD)
    # ae$y_position <- as.numeric(ae$AEDECOD)

    # Create the plot
    p <-
      ggplot(
        ae,
        aes(
          xmin = ASTDY,
          xmax = AENDY,
          ymin = as.numeric(AEDECOD) - 0.3,
          ymax = as.numeric(AEDECOD) + 0.3,
          fill = AESEV,
          text = TOOLTIP
        )
      ) +
      geom_rect() +
      scale_x_continuous(limits = c(xmin(), xmax()))+
      scale_y_continuous(breaks = 1:length(levels(ae$AEDECOD)),
                         labels = levels(ae$AEDECOD)) +
      labs(y = "Adverse Event", x = "Study Day") 

    # Convert to plotly object
    ggplotly(p, tooltip = 'text') %>% 
      plotly::layout(
        legend = list(orientation = 'h')
      )


    # --------------Use plotly directly--------
    # p <- plot_ly(data = ae, type = "bar", x = ~ASTDY, y = ~AEDECOD,
    #              width = ~AENDY - ASTDY,
    #              color = ~AESEV,
    #              text = ~TOOLTIP,
    #              hoverinfo = "text",
    #              hoverlabel = list(bgcolor = "white")) %>%
    #   layout(barmode = 'stack', yaxis = list(type = 'category'),
    #     xaxis = list(title = "Study Day"),
    #     yaxis = list(title = "AEDECOD", categoryorder = "total ascending"),
    #     barmode = 'stack'
    #   )
    # return(p)

    # -----------Use swimplot ------
    # p <- swimmer_plot(df = ae, id = 'AEDECOD', start = 'ASTDY', end = 'AENDY', name_fill = 'AESEV' )
    # return(p)
  })

  
  # Render CM plot
  output$cm_plot <- renderPlotly({
    # browser()  
    # Get CM data
    cm <- cm_data()
    
    # -----------ggplotly approach----------------------------------
    # Create ggplot object
    cm <- cm %>%
      arrange(CMTRT, ASTDY) %>%
      mutate(
        CMTRT = factor(CMTRT, levels = unique(CMTRT)),
        CMCLAS = factor(CMCLAS, levels = unique(CMCLAS)),
        TRTSDT = as.character(as.Date(TRTSDT, "%Y-%m-%d")),
        TRTEDT = as.character(as.Date(TRTEDT, "%Y-%m-%d"))
      )

        # Create the plot
    p1 <-
      ggplot(
        cm,
        aes(
          xmin = ASTDY,
          xmax = AENDY,
          ymin = as.numeric(CMTRT) - 0.3,
          ymax = as.numeric(CMTRT) + 0.3,
          fill = CMCLAS,
          text = TOOLTIP
        )
      ) +
      geom_rect() +
      scale_x_continuous(limits = c(xmin(), xmax()))+
      scale_y_continuous(breaks = 1:length(levels(cm$CMTRT)),
                         labels = levels(cm$CMTRT)) +
      labs(y = "Concomitant Medication", x = "Study Day") 

    # Convert to plotly object
    ggplotly(p1, tooltip = 'text') %>% 
      plotly::layout(
        legend = list(orientation = 'h')
      )
    # return(p1)
    # p <- swimmer_plot(df = cm, id = 'CMDECOD', start = 'ASTDY', end = 'AENDY', name_fill = 'CMCLAS' )
    # return(p)
  })
  
  # Render metadata table
  output$ae_meta <- renderTable({
    # Get AE data
    ae <- ae_data()
    
    # Select metadata columns (adjust this to select the columns you want to display)
    aemeta <-
      ae %>% select(USUBJID, TRTA, TRTSDT, TRTEDT, ASTDY, AENDY, AEDECOD, AESEV)
    
    return(aemeta)
  })
  
  
  output$cm_meta <- renderTable({
    # Get CM data
    cm <- cm_data()
    
    # Select metadata columns (adjust this to select the columns you want to display)
    cmmeta <-
      cm %>% select(USUBJID, TRTA, TRTSDT, TRTEDT, ASTDY, AENDY, CMTRT,CMDECOD, CMCLAS)
    
    return(cmmeta)
  })
  
}

shinyApp(ui = ui, server = server)
