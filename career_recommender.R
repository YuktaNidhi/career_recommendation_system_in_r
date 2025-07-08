library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(DBI)
library(RMySQL)
library(dplyr)
library(proxy)
library(ggplot2)
library(reshape2)
library(shinyWidgets)
library(bslib)


con <- dbConnect(MySQL(),
                 user = "root", password = "your_password",
                 dbname = "career_db", host = "localhost")


score_cols <- c("STEM_Score", "Business_Finance_Score", "Arts_Media_Score", 
                "Healthcare_Score", "Education_Score", "Social_Services_Score", 
                "Trades_Manufacturing_Score", "Government_Law_Score")


normalize <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}


data <- dbGetQuery(con, "SELECT * FROM career_data")
student_scores <- data[, c("StudentID", score_cols)]
student_scores_norm <- student_scores
student_scores_norm[score_cols] <- lapply(student_scores[score_cols], normalize)

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(".main-container { margin-left: 220px; padding: 20px; } .sidebar { position: fixed; width: 220px; top: 0; left: 0; height: 100%; background-color: #2C3E50; color: white; padding-top: 20px; } .sidebar h4, .sidebar a { color: white; text-decoration: none; padding: 10px; display: block; } .sidebar a:hover { background-color: #1A252F; } .tab-content { margin-left: 220px; padding: 20px; }"))),
  
  div(class = "sidebar",
      h4("Career Recommender"),
      actionLink("goto_about", "About"),
      actionLink("goto_dataset", "Dataset View"),
      actionLink("goto_recommend", "Recommendation"),
      actionLink("goto_viz", "Data Visualization")
  ),
  
  div(class = "tab-content",
      uiOutput("main_ui")
  )
)

server <- function(input, output, session) {
  current_tab <- reactiveVal("about")
  
  observeEvent(input$goto_about, { current_tab("about") })
  observeEvent(input$goto_dataset, { current_tab("dataset") })
  observeEvent(input$goto_recommend, { current_tab("recommend") })
  observeEvent(input$goto_viz, { current_tab("viz") })
  
  user_input <- reactiveValues()
  
  output$main_ui <- renderUI({
    switch(current_tab(),
           "about" = tagList(
             h2("About the Career Recommender System", style = "color:#2c3e50;"),
             
             br(),
             h4("🔍 How It Works"),
             p("This intelligent system uses your subject-specific scores (like STEM, Arts, Business, etc.) to match you with similar students in the database. It then recommends careers based on the most frequent career paths among your top matches. Cosine similarity is used to measure how close your profile is to others."),
             
             br(),
             h4("✨ Key Features"),
             tags$ul(
               tags$li("🔗 Career recommendations based on peer similarity"),
               tags$li("📊 Interactive data visualization with heatmaps"),
               tags$li("📥 MySQL database integration for live, scalable data"),
               tags$li("🔄 Cosine similarity-based profile matching"),
               tags$li("🎛️ User-friendly, modern interface with sidebar navigation")
             ),
             
             br(),
             h4("🛠️ Technical Details"),
             tags$ul(
               tags$li("Backend in R using libraries like ", strong("shiny, dplyr, DBI, RMySQL, proxy")),
               tags$li("Frontend built using ", strong("shinydashboard, shinyWidgets, shinyjs, ggplot2")),
               tags$li("Cosine similarity calculated with ", code("proxy::dist(method = 'cosine')")),
               tags$li("Normalization ensures fair comparison of score scales"),
               tags$li("Heatmaps plotted with ", strong("ggplot2"), " and ", strong("reshape2"))
             ),
             
             br(),
             h4("📚 Data Source"),
             p("The system retrieves data from a MySQL database named ", strong("career_db"), ". The main table is ", strong("career_data"), ", which contains:"),
             tags$ul(
               tags$li(code("StudentID"), " – Unique identifier for each student"),
               tags$li(code(paste(score_cols, collapse = ", "))),
               tags$li(code("Primary_Career_Recommendation"), " – The career suggested for each student")
             ),
             
             br(),
             h4("📈 Matching and Recommendations"),
             p("Once you enter your scores, the system normalizes them and computes cosine similarity with all students in the database. Based on the top matches, it shows you the most likely career fit, as well as how your skills compare to others through visualizations."),
             
             br(),
             h5("🎯 Designed to empower students with data-driven career insights.", style = "color:#2980b9;")
           ),
           "dataset" = tagList(
             radioButtons("view_mode", "View Students Based On:",
                          choices = c("Similar Students", "Job Recommendation"),
                          selected = "Similar Students"),
             conditionalPanel(
               condition = "input.view_mode == 'Job Recommendation'",
               selectInput("job_filter", "Select Job Title:", choices = unique(data$Primary_Career_Recommendation))
             ),
             tableOutput("dataset_view")
           ),
           "recommend" = tagList(
             h3("Enter Your Scores to Get Career Recommendations"),
             fluidRow(
               column(6,
                      numericInput("STEM", "STEM Score", 80, min = 0, max = 100),
                      numericInput("Business", "Business/Finance Score", 75, min = 0, max = 100),
                      numericInput("Arts", "Arts/Media Score", 60, min = 0, max = 100),
                      numericInput("Health", "Healthcare Score", 70, min = 0, max = 100)
               ),
               column(6,
                      numericInput("Education", "Education Score", 65, min = 0, max = 100),
                      numericInput("Social", "Social Services Score", 60, min = 0, max = 100),
                      numericInput("Trades", "Trades/Manufacturing Score", 50, min = 0, max = 100),
                      numericInput("Law", "Government & Law Score", 55, min = 0, max = 100),
                      numericInput("topn", "Top N Recommendations", 3, min = 1, max = 10),
                      actionButton("recommend", "Get Recommendations")
               )
             ),
             h4("Top Recommendations:"),
             tableOutput("recommendations")
           ),
           "viz" = tagList(
             h3("Compare Your Scores with Others"),
             radioButtons("viz_type", "Visualize against:",
                          choices = c("Similar Students", "Job-Based Students"),
                          selected = "Similar Students"),
             conditionalPanel(
               condition = "input.viz_type == 'Job-Based Students'",
               selectInput("viz_job", "Select Job Title:", choices = unique(data$Primary_Career_Recommendation))
             ),
             plotOutput("heatmap")
           )
    )
  })
  
  observeEvent(input$recommend, {
    new_user <- data.frame(
      STEM_Score = input$STEM,
      Business_Finance_Score = input$Business,
      Arts_Media_Score = input$Arts,
      Healthcare_Score = input$Health,
      Education_Score = input$Education,
      Social_Services_Score = input$Social,
      Trades_Manufacturing_Score = input$Trades,
      Government_Law_Score = input$Law
    )
    
    for (col in score_cols) {
      rng <- range(student_scores[[col]], na.rm = TRUE)
      new_user[[col]] <- ifelse(rng[1] == rng[2], 0, (new_user[[col]] - rng[1]) / (rng[2] - rng[1]))
    }
    
    sim <- proxy::dist(as.matrix(student_scores_norm[, score_cols]), 
                       as.matrix(new_user), method = "cosine")
    sim_df <- data.frame(StudentID = student_scores$StudentID,
                         similarity = 1 - as.vector(sim))
    
    top_students <- sim_df %>% arrange(desc(similarity)) %>% slice_head(n = input$topn)
    quoted_ids <- paste0("'", top_students$StudentID, "'", collapse = ", ")
    query <- paste0("SELECT * FROM career_data WHERE StudentID IN (", quoted_ids, ")")
    top_data <- dbGetQuery(con, query)
    
    user_input$data <- top_data
    user_input$new_user <- new_user
    
    if ("Primary_Career_Recommendation" %in% names(top_data)) {
      career_recs <- top_data %>%
        inner_join(top_students, by = "StudentID") %>%
        count(Primary_Career_Recommendation, sort = TRUE) %>%
        slice_head(n = input$topn)
      output$recommendations <- renderTable(career_recs)
    }
  })
  
  output$heatmap <- renderPlot({
    if (!is.null(user_input$new_user)) {
      if (input$viz_type == "Similar Students" && !is.null(user_input$data)) {
        top_rows <- student_scores_norm %>%
          filter(StudentID %in% user_input$data$StudentID) %>%
          mutate(ID = paste0("S_", StudentID)) %>%
          select(ID, all_of(score_cols))
      } else {
        job_data <- data %>% filter(Primary_Career_Recommendation == input$viz_job) %>% slice_head(n = 5)
        top_rows <- job_data[, c("StudentID", score_cols)]
        top_rows[score_cols] <- lapply(top_rows[score_cols], normalize)
        top_rows <- top_rows %>% mutate(ID = paste0("J_", StudentID)) %>% select(ID, all_of(score_cols))
      }
      
      new_row <- user_input$new_user
      new_row$ID <- "New_User"
      heat_df <- rbind(new_row[, c("ID", score_cols)], top_rows)
      heat_melt <- melt(heat_df, id.vars = "ID")
      
      ggplot(heat_melt, aes(x = variable, y = ID, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "#3498db") +
        theme_minimal() +
        labs(x = "Skill", y = "User", fill = "Normalized Score")
    }
  })
  
  output$dataset_view <- renderTable({
    if (input$view_mode == "Similar Students" && !is.null(user_input$data)) {
      return(user_input$data[, c("StudentID", score_cols, "Primary_Career_Recommendation")])
    }
    if (input$view_mode == "Job Recommendation") {
      job_data <- dbGetQuery(con, paste0("SELECT * FROM career_data WHERE Primary_Career_Recommendation LIKE '%", input$job_filter, "%'"))
      return(job_data[, c("StudentID", score_cols, "Primary_Career_Recommendation")])
    }
    return(NULL)
  })
}

shinyApp(ui, server)
