



#rm(list=ls(all=TRUE))

#install.packages("plotly")
#install.packages("shinyalert")
#install.packages("rsconnect")
# install.packages("vroom")

library(shiny)
library(DT)
library(tidyverse)
library(data.table)
library(nloptr)
library(plotly)
library(shinyalert)
library(vroom)




#setwd("G:/DI_Projects/Dimer_Interactive/DI_Meal_Planner/RStudio/Shiny/JG_Data_Science_Meal_Planner")




# ---- UI ----
ui <- fluidPage(
  titlePanel(h2("Joel Gafford's Meal Planner")),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "height: 90vh; 
      overflow-y: auto; 
      background-color: #d0ebf9;
      border: 1px solid black;",
      h4("Food Portions"),
      
      # Food selection and portion adjuster.
      fluidRow(
        column(5,
               uiOutput("food_ui1")
        ),
        column(6,
               sliderInput("food_slider1", "Food 1 - grams", 
                           min = 0, max = 500, value = 100, step = 1)
        )
      ),
      fluidRow(
        column(5,
               uiOutput("food_ui2")
        ),
        column(6,
               sliderInput("food_slider2", "Food 2 - grams", 
                           min = 0, max = 500, value = 100, step = 1)
        )
      ),
      fluidRow(
        column(5,
               uiOutput("food_ui3")
        ),
        column(6,
               sliderInput("food_slider3", "Food 3 - grams", 
                           min = 0, max = 500, value = 100, step = 1)
        )
      ),
      fluidRow(
        # Prevents scrollbars:
        # overflow: hidden; 
        style = "height: 300px;
        background-color: #d0ebf9;
        border: 1px solid black;
        padding: 20px;
        margin: 0px 0px 0px 0px;",
        h4("Select 3 Random Foods"),
        # Calculate optimal portion sizes based on all nutrient RDA's.
        actionButton("randomize", "CLICK ME 1st",
                     style = "float: left; 
                     background-color: red; 
                     color: white;
                     font-size: 34px;
                     font-weight: bold;"),
        br(),
        br(),
        br(),
        h4("Calculate Portions and Graph Them"),
        actionButton("calc_portions", "CLICK ME 2nd", 
                     style = "float: left; 
                     background-color: red; 
                     color: white;
                     font-size: 34px;
                     font-weight: bold;"),
        br(),
        br(),
        br(),
        h4("Repeat!"),
      )
      ),
    
    mainPanel(
      width = 9,
      style = "height: 90vh;
      overflow-y: auto;
      background-color: #d0ebf9;
      border: 1px solid black;",
      h4("Nutrient RDAs"),
      tabsetPanel(tabPanel("Graph", plotOutput("Meal_Plot_A")),
                  tabPanel("Spreadsheet", DTOutput("Dataframe_A"))
        )#,
      # fluidRow(
      #   downloadButton("download_csv", "Download .csv",
      #                  style = "float: right;")
      #   )
      )
  )
)




# ---- Server ----
server <- function(input, output, session) {
  
  # Show the popup only once when the app starts
  shinyalert(
    title = "JG Meal Planner App",
    text = "This RStudio Shiny app calculates optimal food portions by targeting 100% RDA values for each recorded nutrient. The horizontal red dotted line is the 100% mark.
    <br>
    <br>
    1. Click <span style='color: red; font-weight: bold;'>'CLICK ME 1st'</span>
    <br>
    2. Click 
    <span style='color: red; font-weight: bold;'>'CLICK ME 2nd'</span>
    <br>
    3. Repeat!
    <br>
    <br>
    This app is a proof of concept and is not to be taken as medical advice. Data sourced from the USDA food database. Made by Joel Gafford as a data science portfolio piece.",
    type = "info",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    confirmButtonText = "Got it!"
  )
  
  
  
  
  # Load and process nutrient RDA data
  nutrient_rda <- reactive({
    df <- read_csv("data/nutrient_rda.csv")
    
    df$adult_upper <- ifelse(is.na(df$adult_upper), df$adult_rda * 10, df$adult_upper)
    df$adult_rda <- ifelse(is.na(df$adult_rda), df$adult_upper / 10, df$adult_rda)
    
    df
  })
  
  # Load food data and sort it
  food_01_found <- reactive({
    vroom("data/food_01_found.csv") %>%
      arrange(description)
  })
  
  
  
  
  output$food_ui1 <- renderUI({
    selectInput("food_choice1", "Food 1",
                choices = food_01_found()$description,
                selected = NULL, selectize = TRUE)
  })
  
  output$food_ui2 <- renderUI({
    selectInput("food_choice2", "Food 2",
                choices = food_01_found()$description,
                selected = NULL, selectize = TRUE)
  })
  
  output$food_ui3 <- renderUI({
    selectInput("food_choice3", "Food 3",
                choices = food_01_found()$description,
                selected = NULL, selectize = TRUE)
  })
  
  
  
  
  
  
  # Load nutrient join data (switch to vroom for better performance)
  food_nutrient_01_join_found <- reactive({
    vroom("data/food_nutrient_01_join_found.csv")
  })
  
  # Randomly select 3 foods to initialize sliderInputs.
  food_01_found_random <- reactive({
    req(food_01_found())
    sample(food_01_found()$description, 3)
  })
  
  
  
  

  # Create a reactiveVal to hold the random foods
  random_foods <- reactiveVal()
  
  # Function to pick 3 random foods
  pick_random_foods <- function() {
    sample(food_01_found()$description, 3)
  }
  
  # Run once at app startup
  observe({
    random_foods(pick_random_foods())
  })
  
  # Also update when the user clicks the button
  observeEvent(input$randomize, {
    random_foods(pick_random_foods())
  })
  
  # Update selectInputs whenever random_foods changes
  observeEvent(random_foods(), {
    updateSelectInput(session, "food_choice1", selected = random_foods()[1])
    updateSelectInput(session, "food_choice2", selected = random_foods()[2])
    updateSelectInput(session, "food_choice3", selected = random_foods()[3])
  })
  
  
  
  
  # Select foods by description
  food_nutrient_02_desc <- reactive({
    c(input$food_choice1, input$food_choice2, input$food_choice3)
  })
  
  food_nutrient_02_ordered <- reactive({
    req(food_01_found())
    descriptions_sel <- food_nutrient_02_desc()
    food_01_found() %>% 
      filter(description %in% descriptions_sel) %>%
      # Set factor levels to selection to preserve order of foods.
      mutate(description = factor(description, levels = descriptions_sel)) %>%
      arrange(description) %>%
      pull(fdc_id) %>%
      data.frame(fdc_id = .)
  })
  
  food_number <- reactive({
    req(food_nutrient_02_ordered)
    nrow(food_nutrient_02_ordered())
  })
  
  food_nutrient_02_filter <- reactive({
    food_nutrient_01_join_found() %>%
      filter(fdc_id %in% food_nutrient_02_ordered()$fdc_id) %>%
      # Arrange the foods by their original order.
      mutate(fdc_id = factor(fdc_id, levels = food_nutrient_02_ordered()$fdc_id)) %>%
      arrange(fdc_id)
  })
  
  food_nutrient_03_wide <- reactive({
    food_nutrient_02_filter() %>%
      pivot_wider(names_from = nutrient_id,
                  values_from = amount,
                  id_cols = description) %>%
      mutate_all(~ replace(., is.na(.), 0)) %>%
      select(-1) %>%
      as.matrix() %>%
      `rownames<-`(food_nutrient_02_desc())
  })
  
  food_nutrient_04_colnames <- reactive({
    data.frame(nutrient_id = as.integer(colnames(food_nutrient_03_wide()))) %>%
      left_join(nutrient_rda(), by = c("nutrient_id" = "id")) %>%
      as.list()
  })
  
  food_nutrient_04_rda_combine <- reactive({
    # Combine RDAs with wide dataframe.
    rbind(food_nutrient_04_colnames()$adult_rda, food_nutrient_03_wide()) %>%
      as.data.frame(.) %>%
      # Eliminate NA columns.
      select(where(~ !is.na(.x[1]))) %>%
      as.matrix(.)
  })

  # Get only nutrients
  food_nutrient_04_nutrients <- reactive({
    food_nutrient_04_rda_combine()[-1,] %>%
    as.data.frame(.)
  })
  
  # Get RDA values for relevant nutrients.
  food_nutrient_04_rda <- reactive({
    food_nutrient_04_rda_combine()[1,] %>%
      as.data.frame(.) %>%
      t(.)
  })
  
  
  
  
  optimize_servings <- reactive({
    req(input$calc_portions) # Trigger on button click
    
    # Objective function
    objective_function <- function(serving_sizes, food_nutrient_matrix, rda_values) {
      total_nutrients <- colSums(food_nutrient_matrix * serving_sizes)
      sum((total_nutrients - rda_values)^2)
    }
    
    # Placeholder data
    food_nutrient_matrix <- food_nutrient_04_nutrients() %>%
      as.matrix()
    rda_values <- food_nutrient_04_rda() %>%
      as.matrix()
    
    # Initial serving sizes from sliders
    initial_serving_sizes <- as.numeric(c(input$food_slider1, 
                                          input$food_slider2, 
                                          input$food_slider3))/100
    
    # Optimization
    result <- nloptr(
      x0 = initial_serving_sizes,
      #x0 = rep(100, nrow(food_nutrient_matrix)),
      eval_f = objective_function,  
      lb = rep(0, nrow(food_nutrient_matrix)),  
      ub = rep(5, nrow(food_nutrient_matrix)),  
      opts = list("algorithm" = "NLOPT_LN_COBYLA", 
                  "xtol_rel" = 1.0e-15,    # Decrease for higher precision
                  "ftol_rel" = 1.0e-15,    # Decrease for higher precision
                  "maxeval" = 5000),      # Increase the number of iterations
      food_nutrient_matrix = food_nutrient_matrix,
      rda_values = rda_values
    )
    
    # Return optimal serving sizes scaled to 0-5 range (scale to the 0-5 range)
    round(result$solution, 2)
  })
  
  
  
  
  # Reactive value to store the slider values
  # >>> This used to be set at: c(100, 100, 100)
  slider_values <- reactiveVal(c(1, 1, 1))  # Initial values
  
  # Update slider values immediately when sliders change
  observe({
    normalized_sliders <- c(input$food_slider1, input$food_slider2, input$food_slider3) / 100
    slider_values(normalized_sliders)
  })
  
  # Update sliders after optimization (still necessary)
  observeEvent(input$calc_portions, {
    servings <- optimize_servings()  # Get optimized servings
    
    # Convert the optimized servings back to the original 0-500 gram scale
    optimized_servings_grams <- servings * 100
    
    updateSliderInput(session, "food_slider1", value = optimized_servings_grams[1])
    updateSliderInput(session, "food_slider2", value = optimized_servings_grams[2])
    updateSliderInput(session, "food_slider3", value = optimized_servings_grams[3])
    
    slider_values(servings)  # Update the reactive value with optimized servings
  })

  
  
  
  # Modify food_nutrient_05_serve to use immediate slider values or optimized ones
  food_nutrient_05_serve <- 
    eventReactive(c(input$food_slider1, input$food_slider2, input$food_slider3, input$calc_portions), 
                  {updated_serving_sizes <- slider_values()  # Get the current slider values
    
    # Calculate the nutrient amounts based on the current slider values
    result <- sweep(food_nutrient_04_nutrients(), 1, updated_serving_sizes * 1, "*") %>%
      as.data.frame()
    
    return(result)
  })
  
  
  
  
  # Get nutrient sums as dataframe.
  food_nutrient_05_sums <- reactive({
    food_nutrient_05_serve() %>%               
    summarise_all(sum) %>%
    round(., 2)
  })
  
  # Combine nutrient serving sizes with sums.
  food_nutrient_05_serve_sum <- reactive({
    rbind(food_nutrient_05_sums(), food_nutrient_05_serve()) %>%
    {rownames(.)[1] <- "Total_Nutrients"; .}
  })


  
  # Extract column names to get Nutrient_IDs.
  food_nutrient_06_colnames <- reactive({
    colnames(food_nutrient_05_serve_sum()) %>%
    t(.) %>%
    as.data.frame(.) %>%
    # Rename all new columns to previous dataframe columns.
    rename_with(~ names(food_nutrient_05_serve_sum()), everything())
  })
  
  # Add Nutrient_ID to new dataframe.
  food_nutrient_06_IDs <- reactive({
    rbind(food_nutrient_06_colnames(), food_nutrient_05_serve_sum()) %>%
      {rownames(.)[1] <- "Nutrient_ID"; .}
  })
  
  # Convert new dataframe to long format.
  food_nutrient_06_long <- reactive({
    food_nutrient_06_IDs() %>% 
    t(.) %>% 
    as.data.frame(.) %>%
    mutate(Nutrient_ID = as.numeric(Nutrient_ID))
  })
  
  nutrient_rda_02_df <- reactive({
    req(nutrient_rda())
    nutrient_rda() %>% as.data.frame()
  })
  
  food_nutrient_07_merged <- reactive({
    food_nutrient_06_long() %>%
      left_join(nutrient_rda(), by = c("Nutrient_ID" = "id")) %>%
      
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      # Set order for nutrient type.
      mutate(nutrient_type = factor(nutrient_type,
                                    levels = c("Proximate", "Mineral", "Vitamin", 
                                               "Lipid", "Amino Acid"))) %>%
      # Set order for name by the "order" column. 
      # It's a coincidence that the command to do this is named "order".
      # The "(order)" is the column, "[order]" is the command.
      mutate(name = factor(name, levels = name[order(order)])) %>%
      mutate(
        Total_Nutrients = as.numeric(Total_Nutrients),
        adult_rda = as.numeric(adult_rda)) %>%
      mutate(Perc_Daily = round((Total_Nutrients / adult_rda * 100), 0)) %>%
      select(order, nutrient_type, name, unit_name, Total_Nutrients, adult_rda, Perc_Daily, (all_of(3:5))) %>%
      rename(
        Order = order,
        Name = name,
        Unit = unit_name,
        Type = nutrient_type,
        Total = Total_Nutrients,
        RDA = adult_rda,
        Percent = Perc_Daily
      ) %>%
      arrange(Order) %>%
      select(-Order)
  })
  
  
  
  
  food_nutrient_08_long <- reactive({
    food_nutrient_07_merged() %>%
    pivot_longer(cols = all_of(7:9),
                 names_to = "Food",
                 values_to = "Nutrient_Amount") %>%
    mutate(Percent_Each = round((as.numeric(Nutrient_Amount) / as.numeric(RDA)) * 100, 2)) %>%
    # Set the order for the food names to be the original order from "food_nutrient_02_desc()".
    mutate(Food = factor(Food, levels = food_nutrient_02_desc()))
  })
  
  
  
  
  
  output$Dataframe_A <- renderDT({
    datatable(
      food_nutrient_07_merged() %>%
        as.data.frame(),
    options = list(
      scrollX = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      stateSave = TRUE,
      #scrollY = 200,
      paging = FALSE#,
      #columnDefs = list(
      #  list(width = '60px', targets = "_all")  # Set column width (adjust as needed)
      #)
      #searching = FALSE
      )
    )
  })
  
  
  
  
  output$Meal_Plot_A <- renderPlot({
    req(input$calc_portions)
    data <- food_nutrient_08_long()  # If food_nutrient_08_long() is a reactive expression
    ggplot(data, aes(x = Name, y = Percent_Each, fill = Food)) + 
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ Type, scales = "free_x", ncol = 5) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "red", linewidth = 1.0) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 16),  
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        axis.text.x = element_text(angle = 55, hjust = 1, face = "bold", size = 12),
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "grey50", fill = NA, linewidth = 1),
        strip.background = element_rect(fill = "lightgrey"),
        plot.margin = margin(10, 10, 10, 100),
        legend.position = "top",  # Place the legend below the plot
        legend.box = "horizontal"    # Make the legend horizontal
      )
  }, height = 700, width = 1275)
  
  
  
  
  
  
  
  
  
  total_reactive <- reactive({
    # Initialize sum.
    total <- 0
    
    # Add sliders together if their checkboxes are true.
    if (input$food_check1) total <- total + input$food_slider1
    if (input$food_check2) total <- total + input$food_slider2
    if (input$food_check3) total <- total + input$food_slider3
    
    # Return total sum as a string.
    total
  })
  
  # Display results
  output$result <- renderText({
    req(input$calc_portions)  # Ensure the button was clicked
    servings <- optimize_servings() * 100
    paste("Optimal Servings (grams):", paste(servings, collapse = ", "))
  })
  
  # Download button logic.
  output$download_csv <- downloadHandler(
    filename = function() {
      "food.csv"
    },
    content = function(file) {
      write.csv(total_reactive(), file)
    }
  )
}




# ---- Shiny app ----
shinyApp(ui, server)



