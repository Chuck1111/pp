library(shiny)
library(bslib)
library(DT)
library(plotly)
library(crosstalk)
library(dplyr)
library(swimplot)
library(clinUtils)
library(ggplot2)
data(dataADaMCDISCP01)

dataADaM <- dataADaMCDISCP01
names(dataADaM)
labelVarsADaM <- attr(dataADaM, "labelVars")
adsl <- dataADaM$ADSL
adsl <- adsl %>% select(c("USUBJID", "SEX", "RACE", "ETHNIC", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT"))
adsl <- as.data.frame(unclass(adsl), stringsAsFactors = TRUE)

adae <- dataADaM$ADAE
adcm <- dataADaM$ADCM

# card_ex <- card(
#   card_header('Exposure'),
#   plotlyOutput('ex_plot')
# )
card_ae <- card(
  card_header('Plots'),
  plotlyOutput('ae_plot',
               height = "300px"),
  
  plotlyOutput('cm_plot',
               height = '300px'),
  
  min_height = '50%',
  max_height = '70%'
  
) 

card_acc <- card(
  card_header('Metadata'),
  accordion(
    open = FALSE,
    accordion_panel(
      'AE Metadata', 
      tableOutput("meta_ae")
    )),
  accordion(
    open = FALSE,
    accordion_panel(
      'CM Metadata', 
      tableOutput("meta_cm")
    )),
)
  
# card_cm <- card(
#   card_header('Concomitant Medications'),
#   plotlyOutput('cm_plot')
# )


ui <- page_navbar(
  id = 'tabs',
  theme = bslib::bs_theme(version = 5),
  title = "Patient Profile           ",
  fillable = TRUE,
  fillable_mobile = TRUE,
  # sidebar = sidebar(
  #   title = "Subject Select ",
  #   textInput("usubjid_input", "Enter USUBJID:"),
  #   actionButton("go_button", "Go")
  # ),
  nav_panel("Demographic", 
            card(
              card_header(h4('Demographic Table')),
              layout_sidebar(
                sidebar = sidebar(
                  title = "Subject Select ",
                  textInput("usubjid_input", "Enter USUBJID:"),
                
              ),
              DTOutput("demographics_table")
            )
            )),
  
  nav_panel(
    "Plots",
   layout_column_wrap(
     width = 1,
     height = 2000,
     # card_ex,
     card_ae,
     card_acc,
     fill = TRUE,
     fillable = TRUE,
     ),
   
    
  )
)

server <- function(input, output, session) {
  bslib::bs_themer() # Add the theme customizer
  
# Filtered data with the USUBJID user clicked
filtered_data <- reactive({
  # If input$usubjid_input is empty, return all data; otherwise, filter based on partial match to input$usubjid_input
  if (input$usubjid_input == "" || is.null(input$usubjid_input)) {
    return(adsl)
  } else {
    return(adsl %>% filter(grepl(input$usubjid_input, USUBJID, ignore.case = TRUE)))
  }
})
  
  
# Render demographics table
output$demographics_table <- renderDT({
  datatable(filtered_data(),
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
            filter = 'top', escape = FALSE) 
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
      mutate(
        TOOLTIP = paste0(
          'USUBJID', USUBJID, '\n',
          'TRTSDT', TRTSDT, '\n'
        )
      )
    
    return(ae)
  })
  
  # Reactive expression for CM data
  # cm_data <- reactive({
  #   # Filter CM data for the selected USUBJID
  #   cm <- adcm %>% filter(USUBJID == clicked_usubjid(), 
  #                         !CMDECOD == 'UNCODED')
  #   # Calculate duration for each AE
  #   cm <- cm %>% mutate(DURATION = AENDY - ASTDY + 1)
  #   return(cm)
  # })
  
  # Render the plotly plots (you need to replace this with your actual plot rendering logic)
  output$ae_plot <- renderPlotly({
    # Get AE data
    ae <- ae_data()

    # Create ggplot object
    p <- ggplot(ae, aes(xmin = ASTDY, 
                        xmax = AENDY, 
                        ymin = as.numeric(factor(AEDECOD)), 
                        ymax = as.numeric(factor(AEDECOD)) + 0.8, 
                        # fill = as.factor(AESEV), 
                        text = TOOLTIP
                        )) +
      # geom_bar(stat = "identity", position = position_dodge(0.5)) +
      geom_rect(aes(fill = as.factor(AESEV))) +
      scale_fill_manual(values = c("red", "yellow", "green"))+
      scale_y_continuous(breaks = seq_along(levels(factor(ae$AEDECOD))), labels = levels(factor(ae$AEDECOD)))+
      labs(y = 'Adverse Event', x = 'Study Day')
    ggplotly(p, tooltip = text) # Convert to plotly object
  })
  
  # Render metadata table
  output$meta_ae <- renderTable({
    # Get AE data
    ae <- ae_data()
    
    # Select metadata columns (adjust this to select the columns you want to display)
    metadata <- ae %>% select(USUBJID,TRTA,TRTSDT, TRTEDT, ASTDY, AENDY, AEDECOD, AESEV, DURATION)
    
    return(metadata)
  })
  output$ex_plot <- renderPlotly({
    # Render exposure plot for clicked_usubjid
  })
  output$cm_plot <- renderPlotly({
    # Render medication plot for clicked_usubjid
  })
}

shinyApp(ui = ui, server = server)
