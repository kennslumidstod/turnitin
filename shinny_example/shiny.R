#By Pétur Stefánsson

library("shiny")
library("ggplot2")
library("tidyverse")
library(reshape2)
library(plotly)

class_df <- read.csv("shiny_example_gogn/Classes_comma.csv", sep = ",")
integration_df <- read.csv("shiny_example_gogn/Integration_comma.csv", sep = ",", check.names = FALSE)
instructors_df <- read.csv("shiny_example_gogn/Instructor_comma.csv", sep = ",", check.names = FALSE)

class_df.skyear.hi <- filter(class_df, Parent.Account.Name == "Háskóli Íslands")
class_df.skyear.hr <- filter(class_df, Parent.Account.Name == "Háskólinn í Reykjavík")
class_df.skyear.haskolar <- filter(class_df, Skolastig == "Háskólar")
class_df.skyear.fram <- filter(class_df, Skolastig == "Framhaldsskólar")

integration_df.skyear.hi <- filter(integration_df, `Parent Account Name` == "Háskóli Íslands")
integration_df.skyear.hr <- filter(integration_df, `Parent Account Name` == "Háskólinn í Reykjavík")
integration_df.skyear.haskolar <- filter(integration_df, Skolastig == "Háskólar")
integration_df.skyear.fram <- filter(integration_df, Skolastig == "Framhaldsskólar")

instructors_df.skyear.hi <- filter(instructors_df, `Parent Account Name` == "Háskóli Íslands")
instructors_df.skyear.hr <- filter(instructors_df, `Parent Account Name` == "Háskólinn í Reykjavík")
instructors_df.skyear.haskolar <- filter(instructors_df, Skolastig == "Háskólar")
instructors_df.skyear.fram <- filter(instructors_df, Skolastig == "Framhaldsskólar")

# the UI bit:
ui <- fluidPage(
  titlePanel("plots by school year"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "sk_year",label = "Select a school year from", choices = get_skolar_in_range(range(class_df$Skolaar)[1], range(class_df$Skolaar)[2])),
      selectInput(inputId = "sk_year_end",label = "Select a school year to", choices = get_skolar_in_range(range(class_df$Skolaar)[1], range(class_df$Skolaar)[2])),
      selectInput(inputId = "integration_plots", label = "Select plot", choices = c("Total feedback","Total feedback per schoolyear", "Total Submissions","Total Submissions per schoolyear","Total similarity reports","Total similarity reports per schoolyear","Total active classes", "Active classes per school year","Total active instructors","Total active instructors per schoolyear","Types of Submissions","Types of Submissions per schoolyear", "Types of integration", "School's Integration","Integration by shoolyear", "Feedback eftir sviðum","Feedback efti skólaárum", "Simmilarity eftir sviðum", "Simmilarity efti skólaárum")),
      selectInput(inputId = "school_type",label = "Select a school", choices = c("Háskóli Íslands", "Háskólinn í Reykjavík", "Háskólar", "Háskólar", "Framhaldsskólar")),
      actionButton(inputId = "res", label = "Result")
    ),
    
    mainPanel(
      plotlyOutput("myplot", height = 700)
    )
  )
)

# the server bit:
server <- function(input, output) {
  
  
  
  observeEvent(input$res, {
    
    syear <- input$sk_year
    eyear <- input$sk_year_end
    intfunc <- input$integration_plots
    school_type <- input$school_type
    useparent <- ifelse(school_type %in% c("Háskólar", "Framhaldsskólar") ,TRUE, FALSE)
    df_school <- switch (school_type, "Háskóli Íslands" = class_df.skyear.hi, "Háskólinn í Reykjavík" = class_df.skyear.hr, "Háskólar" = class_df.skyear.haskolar, "Framhaldsskólar" = class_df.skyear.fram)
    integ_df <- switch (school_type, "Háskóli Íslands" = integration_df.skyear.hi, "Háskólinn í Reykjavík" = integration_df.skyear.hr, "Háskólar" = integration_df.skyear.haskolar, "Framhaldsskólar" = integration_df.skyear.fram)
    inst_df <- switch (school_type, "Háskóli Íslands" = instructors_df.skyear.hi, "Háskólinn í Reykjavík" = instructors_df.skyear.hr, "Háskólar" = instructors_df.skyear.haskolar, "Framhaldsskólar" = instructors_df.skyear.fram)
    
    output$myplot <- renderPlotly({
      
        
      switch(intfunc, "Total feedback" = total_feedback_eftir_skolum_bar_plot_df_plotty(df_school, get_skolar_in_range(syear,eyear), TRUE, use_parent = useparent),
             "Total feedback per schoolyear" = total_feedback_milli_ara_bar_plot_df_plotty(df_school,get_skolar_in_range(syear, eyear), FALSE),
             "Total Submissions" = total_submissins_eftir_skolum_hi_bar_plot_df_plottly(df_school,get_skolar_in_range(syear, eyear), TRUE, use_parent = useparent),
             "Total Submissions per schoolyear" = total_submissins_milli_skola_ara_bar_plot_df(df_school,get_skolar_in_range(syear, eyear), FALSE),
             "Total similarity reports" = total_symilarity_reports_df(df_school,get_skolar_in_range(syear, eyear), TRUE, use_parent = useparent),
             "Total similarity reports per schoolyear" = total_symilarity_reports__milli_skola_ara_df(df_school,get_skolar_in_range(syear, eyear), FALSE),
             "Total active classes" = total_active_classes_eftir_skolum_df(df_school,get_skolar_in_range(syear, eyear), TRUE, use_parent = useparent),
             "Active classes per school year" = namsked_kend_milli_ara_barplot_df_plottly(df_school,get_skolar_in_range(syear, eyear), FALSE),
             "Total active instructors" = total_active_instructors_eftir_skolum_df(inst_df,get_skolar_in_range(syear, eyear), TRUE, use_parent = useparent),
             "Total active instructors per schoolyear" = kennarar_milli_ara_bar_plot_df_plottly(inst_df,get_skolar_in_range(syear, eyear), FALSE),
             "Types of Submissions" = types_of_submissions_df_plotly(df_school,get_skolar_in_range(syear,eyear), TRUE, use_parent = useparent),
             "Types of Submissions per schoolyear"= types_of_submissions_milli__skola_ara_df_plotly(df_school,get_skolar_in_range(syear, eyear), FALSE),
             "Types of integration" = types_of_intergration_eftir_skola_ari_bar_plot_df_plotly(integ_df, get_skolar_in_range(syear,eyear), TRUE),
             "School's Integration" = skipting_a_intergration_eftir_skolum_bar_plot_df_plotly(integ_df, get_skolar_in_range(syear,eyear), TRUE, use_parent = useparent),
             "Integration by shoolyear" = skipting_a_intergration_milli_skola_ara_bar_plot_df_plotly(integ_df,get_skolar_in_range(syear, eyear), FALSE),
             "Feedback eftir sviðum" = feedback_skolar_bar_plots_df_plotly(df_school,get_skolar_in_range(syear, eyear),TRUE, use_parent = useparent),
             "Feedback efti skólaárum" = feedback_skolar_milli_skola_ara_bar_plots_df_plotly(df_school,get_skolar_in_range(syear, eyear), FALSE),
             "Simmilarity eftir sviðum" = skolar_similarity_bar_plots_eftir_arum_df_plotly(df_school,get_skolar_in_range(syear, eyear),TRUE, use_parent = useparent),
             "Simmilarity efti skólaárum" = skolar_similarity_bar_plots_eftir_skola_arum_df_plotly(df_school,get_skolar_in_range(syear, eyear), FALSE)
             )
      
    })
  
    
  })
}

# Run it 
shinyApp(ui = ui, server = server)
