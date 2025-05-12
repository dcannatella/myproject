library(shiny)
library(collapsibleTree)
library(data.tree)
library(Matrix)

source("R/ahp_calculations.R")
source("R/consistency_check.R")
# source("R/sensitivity.R")

ui <- navbarPage("AHP Decision Tool",

                 # Tab 1: Hierarchy Setup
                 tabPanel("1. Hierarchy Setup",
                          tabsetPanel(
                            tabPanel("Objective",
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("objective", "Overall Objective:", value = "Choose the best laptop")
                                       ),
                                       mainPanel(
                                         h4("Instructions"),
                                         p("Define your overall decision-making goal.")
                                       )
                                     )
                            ),

                            tabPanel("Sub-objectives",
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("n_sub", "Number of Sub-objectives:", min = 1, max = 5, value = 2),
                                         uiOutput("subobjectives_ui")
                                       ),
                                       mainPanel(
                                         p("Name the sub-objectives that contribute to your overall goal.")
                                       )
                                     )
                            ),

                            tabPanel("Criteria",
                                     sidebarLayout(
                                       sidebarPanel(
                                         uiOutput("criteria_input_ui")
                                       ),
                                       mainPanel(
                                         p("For each sub-objective, enter the number of criteria and name them.")
                                       )
                                     )
                            ),

                            tabPanel("Hierarchy Visualization",
                                     fluidRow(
                                       column(12,
                                              h4("Your Decision Hierarchy"),
                                              collapsibleTreeOutput("tree_plot", height = "600px")
                                       )
                                     )
                            )
                          )
                 ),

                 # Tab 2: Pairwise Comparison
                 tabPanel("2. Pairwise Comparison",
                          tabsetPanel(
                            tabPanel("Input Preferences",
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("n_criteria", "Number of Criteria for Pairwise Comparison", min = 1, max = 5, value = 3),
                                         uiOutput("pairwise_inputs_ui"),
                                         actionButton("generate_matrix", "Generate Pairwise Matrix")
                                       ),
                                       mainPanel(
                                         p("Use the sliders to input your preferences between criteria at each level.")
                                       )
                                     )
                            ),

                            tabPanel("Pairwise Matrix",
                                     fluidRow(
                                       column(12,
                                              h4("Pairwise Comparison Matrix"),
                                              tableOutput("pairwise_matrix")
                                       )
                                     )
                            ),

                            tabPanel("Consistency & Coefficients",
                                     sidebarLayout(
                                       sidebarPanel(
                                         actionButton("compute_consistency", "Check Consistency")
                                       ),
                                       mainPanel(
                                         verbatimTextOutput("consistency_result"),
                                         plotOutput("coefficient_plot")
                                       )
                                     )
                            )
                          )
                 )
)




server <- function(input, output, session) {

  # Step 1: Hierarchical Structure (Sub-objectives and Criteria)
  output$subobjectives_ui <- renderUI({
    req(input$n_sub)
    lapply(1:input$n_sub, function(i) {
      textInput(paste0("sub_", i), paste("Name for Sub-objective", i), value = paste("Sub", i))
    })
  })

  output$criteria_input_ui <- renderUI({
    req(input$n_sub)
    ui_list <- list()

    for (i in 1:input$n_sub) {
      sub_name <- input[[paste0("sub_", i)]]
      if (is.null(sub_name) || sub_name == "") next
      n_crit_input_id <- paste0("n_crit_", i)
      ui_list[[length(ui_list) + 1]] <- numericInput(n_crit_input_id, paste("Number of Criteria under", sub_name), value = 2, min = 1, max = 6)
      n_crit_val <- input[[n_crit_input_id]]
      if (!is.null(n_crit_val)) {
        for (j in 1:n_crit_val) {
          id <- paste0("crit_", i, "_", j)
          ui_list[[length(ui_list) + 1]] <- textInput(id, paste("Name of Criterion", j, "for", sub_name), value = paste("Criterion", i, j))
        }
      }
    }
    do.call(tagList, ui_list)
  })

  output$tree_plot <- renderCollapsibleTree({
    req(input$objective)
    req(input$n_sub)
    obj <- input$objective
    sub_objs <- lapply(1:input$n_sub, function(i) {
      sub_name <- input[[paste0("sub_", i)]]
      n_crit <- input[[paste0("n_crit_", i)]]
      crits <- lapply(1:n_crit, function(j) {
        list(name = input[[paste0("crit_", i, "_", j)]])
      })
      list(name = sub_name, children = crits)
    })
    tree_data <- list(name = obj, children = sub_objs)
    tree_node <- data.tree::FromListExplicit(tree_data)

    # Compute random weights for illustration purposes
    tree_node$Do(function(node) {
      node$weight <- runif(1, 0.1, 1)  # This will be replaced by actual weight calculation
    })

    # Visualize with node size representing weight
    collapsibleTree::collapsibleTree(tree_node, root = obj, hierarchyAttribute = "name", zoomable = TRUE,
                                     nodeSize = "weight")
  })

  # Step 2: Pairwise Comparison Input for Each Branch
  output$pairwise_inputs_ui <- renderUI({
    req(input$n_criteria)  # Ensure n_criteria is selected
    n_criteria <- input$n_criteria
    ui_list <- list()

    # Generate sliders for pairwise comparisons
    for (i in 1:(n_criteria - 1)) {
      for (j in (i + 1):n_criteria) {
        slider_id <- paste0("slider_", i, "_", j)
        ui_list[[length(ui_list) + 1]] <- sliderInput(slider_id,
                                                      paste("Preference between Criterion", i, "and Criterion", j),
                                                      min = 1, max = 9, value = 5, step = 1)
      }
    }
    do.call(tagList, ui_list)
  })

  pairwise_matrix <- reactive({
    n_criteria <- input$n_criteria
    matrix_data <- matrix(1, nrow = n_criteria, ncol = n_criteria)

    # Fill pairwise comparison matrix based on slider inputs
    for (i in 1:(n_criteria - 1)) {
      for (j in (i + 1):n_criteria) {
        value <- input[[paste0("slider_", i, "_", j)]]
        matrix_data[i, j] <- value
        matrix_data[j, i] <- 1 / value
      }
    }
    matrix_data
  })

  output$pairwise_matrix <- renderTable({
    pairwise_matrix()
  })

  # Consistency and weight calculation
  output$consistency_result <- renderPrint({
    matrix_data <- pairwise_matrix()
    n <- nrow(matrix_data)

    # Step 1: Eigenvalue method to calculate the weights
    eigenvalues <- eigen(matrix_data)$values
    lambda_max <- max(eigenvalues)  # Largest eigenvalue

    # Step 2: Consistency index (CI)
    CI <- (lambda_max - n) / (n - 1)

    # Step 3: Random Index (RI) based on matrix size
    RI_values <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32)  # RI values for matrix size 1-7
    RI <- RI_values[n]  # RI for the current matrix size

    # Step 4: Consistency ratio (CR)
    CR <- CI / RI
    cat("Consistency Ratio (CR):", CR, "\n")

    if (CR > 0.1) {
      cat("The matrix is inconsistent, please adjust your pairwise comparisons.")
    } else {
      cat("The matrix is consistent.")
    }

    # Step 5: Eigenvector-based weights
    eigenvector <- eigen(matrix_data)$vectors[, 1]
    weights <- eigenvector / sum(eigenvector)

    cat("Weights from the eigenvector method:\n")
    print(weights)
  })

  # Plot coefficients (weights)
  output$coefficient_plot <- renderPlot({
    matrix_data <- pairwise_matrix()
    eigenvector <- eigen(matrix_data)$vectors[, 1]
    weights <- eigenvector / sum(eigenvector)

    # Plot weights for each criterion (using dot size)
    barplot(weights, main = "Criterion Weights", col = "blue", ylim = c(0, 1))
  })
}




shinyApp(ui, server)
