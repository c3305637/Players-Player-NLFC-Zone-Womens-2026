library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)

# ---- data ----
data <- read_excel("321s_final.xlsx")  

totals_round <- data %>%
  group_by(Round, Nominated, Grade) %>%
  summarise(TotalPoints = sum(Points), .groups = "drop") %>%
  group_by(Round, Nominated) %>%
  mutate(TotalAll = sum(TotalPoints)) %>%
  ungroup()

season_totals <- totals_round %>%
  group_by(Nominated, Grade) %>%
  summarise(TotalPoints = sum(TotalPoints), .groups = "drop") %>%
  group_by(Nominated) %>%
  mutate(TotalAll = sum(TotalPoints)) %>%
  ungroup()

grade_cols <- c(First = "#F8766D", Reserve = "#00BFC4")
max_round  <- max(totals_round$TotalAll)
max_season <- max(season_totals$TotalAll)

# ---- dropdown ----
present <- totals_round %>% filter(TotalPoints > 0) %>% distinct(Round, Grade)

round_opts <- bind_rows(
  present %>% count(Round, name = "n") %>% filter(n > 1) %>%
    transmute(Round, Grade = "Total"),
  present
) %>%
  mutate(scope = "round",
         Grade = factor(Grade, levels = c("Total", "First", "Reserve"))) %>%
  arrange(Round, Grade)

season_opts <- tibble(
  Round = NA,
  Grade = factor(c("Total", "First", "Reserve"),
                 levels = c("Total", "First", "Reserve")),
  scope = "season"
)

choices_df <- bind_rows(season_opts, round_opts) %>%
  mutate(
    id = as.character(row_number()),
    label = case_when(
      scope == "season" & Grade == "Total" ~ "All rounds - total",
      scope == "season" ~ paste0("All rounds - ", Grade, " grade"),
      Grade == "Total"  ~ paste0("Round ", Round, " - total"),
      TRUE              ~ paste0("Round ", Round, " - ", Grade, " grade")
    )
  )

# ---- ui ----
ui <- fluidPage(
  selectInput("sel", "View",
              choices = setNames(choices_df$id, choices_df$label)),
  uiOutput("plot_ui"),
  
  tags$hr(),
  tags$div(
    style = "font-size: 0.85em; color: #666; padding: 4px 0 20px;",
    HTML("Created by Brooke Harvey using <em>ggplot2</em>, <em>plotly</em> and
          <em>shiny</em> in R. Last updated 7 September 2026 04:16:55 AEST.<br>
          For data queries and code, contact
          <a href='mailto:you@example.com'>brooke.harvey@newcastle.edu.au</a>.")
  )
)

# ---- server ----
server <- function(input, output) {
  
  sel <- reactive(choices_df[choices_df$id == input$sel, ])
  
  plot_data <- reactive({
    s <- sel()
    d <- if (s$scope == "season") {
      season_totals
    } else {
      totals_round %>% filter(Round == s$Round)
    }
    if (s$Grade == "Total") {
      d <- d %>% mutate(sort_key = TotalAll)
    } else {
      d <- d %>% filter(Grade == s$Grade) %>% mutate(sort_key = TotalPoints)
    }
    d %>% filter(sort_key > 0)
  })
  
  output$plot_ui <- renderUI({
    n <- n_distinct(plot_data()$Nominated)
    plotlyOutput("plot", height = paste0(max(400, n * 22), "px"))
  })
  
  output$plot <- renderPlotly({
    s <- sel()
    
    p <- plot_data() %>%
      ggplot(aes(x = reorder(Nominated, sort_key),
                 y = TotalPoints, fill = Grade,
                 text = paste0(Nominated, "<br>",
                               Grade, ": ", TotalPoints, " points",
                               "<br>Total: ", TotalAll))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = grade_cols, drop = FALSE) +
      expand_limits(y = if (s$scope == "season") max_season else max_round) +
      labs(x = "Player", y = "Total Points", title = s$label)
    
  })
}

shinyApp(ui, server)
