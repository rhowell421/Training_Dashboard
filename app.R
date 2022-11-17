#### Reference Guides ####
#
# https://r4ds.had.co.nz/
# https://cprobe.army.mil/rsconnect/CAA_Intro_to_R/
# https://mastering-shiny.org/


# This code is broken down into 5 sections:
# 1. Libraries
# 2. Reading and tidying data
# 3. User Interface (UI)
# 4. Server
# 5. Plotting data

# The GitLab URL is: https://gitlab.devforce.disa.mil/rhowell/training-stats.git
# Lesd_training_stats is now merged with MASTER, find the app.R file in the FOLDER LESD_Training_Stats under MASTER branch

#### 1. Libraries ####

library(tidyverse)    # readability of code
library(lubridate)    # dates
library(readxl)    # reading excel files
library(shiny)    # reactivity for user input
library(shinydashboard)    # dashboard layout
library(shinyWidgets)    # for the colored value boxes on the first tab
library(shinyalert)    # mandatory IS warning message per the RMF

#### 2. Reading and tidying data ####

Sys.setenv(TZ="America/New_York")    # sets timezone, shouldn't have to adjust

# Read in file
freq <- read_excel("NSCTrainingStats-FY21_courses.xlsm", 
  sheet = "Course List") %>% 
  select(!"...1")

# Separate into different year requirements
course_list_1 <- freq %>%
  filter(Frequency == "Annual") %>%
  select("Training Name") %>%
  unlist()

course_list_3 <- freq %>%
  filter(Frequency == "3 Years") %>%
  select("Training Name") %>%
  unlist()

course_list_5 <- freq %>%
  filter(Frequency == "5 Years") %>%
  select("Training Name") %>%
  unlist()

freq_missing <- freq %>%
  filter(`Required Audience` == "ALL") %>%
  mutate("Training" = `Training Name`)

df_list <- read_excel("NSCTrainingStats-FY21.xlsm", 
  sheet = "FY22 Training", skip = 5) %>%
  filter(Directorate == "LESD") %>%
  pivot_longer(TARP:`Mil: Telework Supervisor Training`, names_to = "Training", values_to = "Date") %>%
  unite(col = "Name", `Last Name`, `First Name`, sep = ", ") 

# Add year requirement from last complete to future due date
df1 <- df_list %>%
  filter(Training %in% c(course_list_1)) %>%
  mutate(Date = Date + dyears(1)) %>%
  filter(Date >= (Sys.Date() - ddays(365))) %>%
  mutate(Name = str_to_title(Name))

df3 <- df_list %>%
  filter(Training %in% c(course_list_3)) %>%
  mutate(Date = Date + dyears(3)) %>%
  filter(Date >= (Sys.Date() - ddays(365))) %>%
  mutate(Name = str_to_title(Name))

df5 <- df_list %>%
  filter(Training %in% c(course_list_5)) %>%
  mutate(Date = Date + dyears(5)) %>%
  filter(Date >= (Sys.Date() - ddays(365))) %>%
  mutate(Name = str_to_title(Name))


# Read in names for LESD
roster <- read_excel("NSCTrainingStats-FY21_names.xlsx", sheet = "Roster", col_types = c("text", "text", "text", "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip")) %>%
  mutate(Name = `Name List`) %>%
  filter(Division != "Vacant")

roster$Name <-  gsub("  ", " ", roster$Name)     # reformats names to match Exercise Workbook


# Begin merging back the separate year requirements
df_join_unfiltered <- full_join(df1, df3) %>%
  full_join(df5) %>%
  full_join(roster, by = "Name") %>%
  select(Division, Name, Training, Date) %>%
  arrange(Division, Name, Date)

# Removes blank dates
df_join <- df_join_unfiltered %>%
  filter(!is.na(Date))

# Creating a separate dataframe for missing trainings (its hard to see what isn't there)
df_join_missing <- df_list %>%
  mutate(Name = str_to_title(Name)) %>%
  right_join(freq_missing) %>%
  full_join(roster, by = "Name") %>%
  filter(!is.na(Name)) %>%
  filter(!is.na(Training)) %>%
  filter(is.na(Date)) %>%
  select(Division, Name, Training, Date) %>%
  arrange(Division, Name, Date)

df_join$Date <- format(df_join$Date,'%Y-%m-%d')


#### 3. UI #### 
ui <- fluidPage(
  
  useShinydashboard(),

    titlePanel("LESD Training Due"),

    sidebarLayout(
        sidebarPanel(
          selectInput("tab_division", "Division", choices = c("All", unique(roster$Division)), selected = "Exercise"),
          h5(HTML("This page is updated on Mondays")),
          h5(str_wrap(paste(" Last update was", format(file.mtime("NSCTrainingStats-FY21.xlsm"), "%b %d, %Y at %H:%m"), sep = " "))),
          h5(HTML("Color Scale: <br/> G > 98% <br/> A = 95% - 98% <br/> R < 95%"))
    ),

    mainPanel(
      
      fluidRow(box("CUI", width = 12, align="center", background = "purple")),     # Mandatory for RMF
      
        tabsetPanel(
            br(),    # break, creates a horizontal space
            
            # Tab 1
            tabPanel("Division",
            uiOutput("hyperlink"),         
            br(),
            downloadButton("df_download", "Download"),
            br(),
            br(),
            fluidRow(
              valueBoxOutput("PerExp"),
              valueBoxOutput("BoxExp"),
              valueBoxOutput("Box30"),
              ),
            br(),
            uiOutput("days_missing"),
            br(),
            uiOutput("days_exp_test"),
            br(),
            uiOutput("days_30"),
            br(),
            textOutput("txt_60"),
            tableOutput("days_60"),
            ),
           
            # Tab 2
            tabPanel("Person",
            downloadButton("person_download", "Download"),
            br(),
            br(),selectInput("list_name", "Name", choices = unique(df_join$Name), selected = NULL, multiple = FALSE),
            tableOutput("datatable_person"),
            ),
           
           # Tab 3
           tabPanel("Expiring Numbers",
           br(),
           textOutput("txt_due_table"),
           br(),
           tableOutput("tables_due"),
           hr(),
           textOutput("txt_30_table"),
           br(),
           tableOutput("tables_30"),
           hr(),
           textOutput("txt_60_table"),
           br(),
           tableOutput("tables_60")
           ),
      
          # Tab 4
           tabPanel("Course List",
                 tableOutput("freq")
          )
          
          ),
     
         fluidRow(box("CUI", width = 12, align="center", background = "purple"))     # Mandatory for RMF
      )
    )
)

#### 4. Server ####

server <- function(input, output) {

# Mandatory for RMF
  
  observe({ shinyalert(

"You are accessing a U.S. Government (USG) Information System (IS) that is provided for USG-authorized use only.", "Mandatory DoD Notice and Consent

By using this IS (which includes any device attached to this IS), you consent to the following conditions:

The USG routinely intercepts and monitors communications on this IS for purposes including, but not limited to, penetration testing, COMSEC monitoring, network operations and defense, personnel misconduct (PM), law enforcement (LE), and counterintelligence (CI) investigations.
At any time, the USG may inspect and seize data stored on this IS.
Communications using, or data stored on, this IS are not private, are subject to routine monitoring, interception, and search, and may be disclosed or used for any USG authorized purpose.
This IS includes security measures (e.g., authentication and access controls) to protect USG interests - not for your personal benefit or privacy.
Notwithstanding the above, using this IS does not constitute consent to PM, LE or CI investigative searching or monitoring of the content of privileged communications, or work product, related to personal representation or services by attorneys, psychotherapists, or clergy, and their assistants. Such communications and work product are private and confidential. See User Agreement for details.
Acknowledgement Of Responsibilities Of Receiving And Maintaining Privacy Act Data
DATA YOU ARE ABOUT TO ACCESS COULD POTENTIALLY BE PROTECTED BY THE PRIVACY ACT OF 1974. You must:

Have completed the necessary training with regards to Security Awareness and safe-guarding Personally Identifiable Information.
Ensure that data is not posted, stored or available in any way for uncontrolled access on any media.
Ensure that data is protected at all times as required by the Privacy Act of 1974 (5 USC 552a(I)(3)) as amended and other applicable DOD regulatory and statutory authority; data will not be shared with offshore contractors; data from the application, or any information derived from the application, shall not be published, disclosed, released, revealed, shown, sold, rented, leased or loaned to anyone outside of the performance of official duties without prior DMDC approval.
Delete or destroy data from downloaded reports upon completion of the requirement for their use on individual projects.
Ensure data will not be used for marketing purposes.
Ensure distribution of data from a DMDC application is restricted to those with a need-to-know. In no case shall data be shared with persons or entities that do not provide documented proof of a need-to-know.
Be aware that criminal penalties under section 1106(a) of the Social Security Act (42 USC 1306(a)), including possible imprisonment, may apply with respect to any disclosure of information in the application(s) that is inconsistent with the terms of application access. The user further acknowledges that criminal penalties under the Privacy Act (5 USC 552a(I)(3)) may apply if it is determined that the user has knowingly and willfully obtained access to the application(s) under false pretenses.", 
      
      type = "warning")
  })
  
  # Updates the name in the Name tab when the user changes the division drop down
  observeEvent(input$tab_division, {
    updateSelectInput(inputId = "list_name", choices = unique(na.omit(
      if (input$tab_division == "All") {
        sort(df_join$Name) 
    } else {
      df_join$Name[df_join$Division == input$tab_division]
      }), 
     selected = unique(df_join$Name)))
  })
  
  # Since there is no All divisions on the spreadsheet, we have to create it
  df <- reactive({
    if (input$tab_division == "All") {
      df_join 
    } else {
      df_join %>% filter(Division == input$tab_division)
    }
  })
  
  # Creating the all for the missing training
  df_missing <- reactive({
    if (input$tab_division == "All") {
      df_join_missing 
    } else {
      df_join_missing %>% filter(Division == input$tab_division)
    }
  })
  
  # Creating variables for Expired, 30, and 60 days out
  table_expiry <- reactive({
    filter(df(), Date <= Sys.Date())
  })
  
  table_30 <- reactive({
    filter(df(), Date <= (Sys.Date() + ddays(30)))
  })
  
  table_60 <- reactive({
    filter(df(), Date <= (Sys.Date() + ddays(60)))
  })
  
 # Creates the file that is downloaded from the Division tab
  df_dl <- reactive({df() %>% 
      mutate(`Days until Expiration` = round(difftime(Date, Sys.Date(), units = "days"))) %>%
#      filter(Date >= Sys.Date() & Date <= (Sys.Date() + ddays(90))) %>%
      filter(Date <= (Sys.Date() + ddays(90))) %>%
      arrange(Name, Date)
    })
  
  # Creates a variable for updating the table on the Name tab
  df_person <- reactive({
    df() %>%
    filter(Name == input$list_name) %>%
    select(Training, Date) %>%
    arrange(Date)
  })
  
  # Counting rows for the boxes at the top of the Division tab
  count_exp <- reactive({
    nrow(df() %>% filter(Date <= Sys.Date()))
  })
  
  count_tot <- reactive({
    nrow(df())
  })

#### 5. Plotting Data ####
  
# Tab 1   

  url <- a("NSC Training Portal Page", href="https://cacmdc.army.mil/cact/NSC/Training/SitePages/Home.aspx")

  output$hyperlink <- renderUI({
    tagList("Update training on the", url)
  })
  
  output$df_download <- downloadHandler(
    filename = function() {
      paste(input$tab_division, "Training Due", Sys.Date(), ".csv", sep = " ")
    },
    content = function(file) {
      write.csv(df_dl(), file)
    }
  )
  
# Value Boxes logic
  
  output$PerExp <- shinydashboard::renderValueBox({
    t <- paste0(floor(100 - (100*(as.integer(count_exp())/as.integer(count_tot())))), "%")
      
    if (t  == "100%") {
      shinydashboard::valueBox(t,"Compliant", icon = icon("thumbs-up"), color = "green")
    } else if (t  >= 98) {
      shinydashboard::valueBox(t,"Compliant", icon = icon("thumbs-up"), color = "green")
    } else if (t <= 95) {
      shinydashboard::valueBox(t,"Compliant", icon = icon("thumbs-down"), color = "red")
    } else {
      shinydashboard::valueBox(t,"Compliant", icon = icon("list"), color = "orange")
    }
  })
  
  output$BoxExp <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      paste0(nrow(df() %>%
                    filter(Date <= Sys.Date()))), 
      "Expired Trainings", icon = icon("list")
    )
  })
  
  output$Box30 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      paste0(nrow(table_30())),
      "Trainings Expire in 30 Days", icon = icon("clock")
      )
  })
  

# Tables logic

    output$txt_60 <- renderText("60 Days")
 
    output$days_exp_test <- renderUI({
      
      overdue <- df() %>% filter(Date <= Sys.Date())
      
      if(nrow(overdue) >= 1){
        tagList(
          renderText("Expired Training"),
          renderTable(overdue)
        )
      } else {
         tagList(
           renderText("No Expired Training - All Caught Up!"),
           renderImage(
             return(
             list(
               src = "smiley.jpg",
               contentType = "image/jpg",
               alt = "Smiley"))
        ))
      }
    })
     
  output$days_missing <- renderUI({
      
      if(nrow(df_missing()) >= 1){
        tagList(
        renderText("Missing Training for 'ALL'"),
        renderTable(df_missing())
        )
      } else {
        renderText("No Missing Training for 'ALL'!")
      }
          
    })
  
  output$days_30 <- renderUI({
      
      if(nrow(table_30()) >= 1){
        tagList(
          renderText("30 Days"),
          renderTable(table_30())
        )
      } else {
        renderText("No Training Expiring in 30 Days - Ahead of the Curve!")
      }
      
    })
    
    output$days_60 <- renderTable({
       filter(df(), Date > (Sys.Date() + days(30)) & Date < (Sys.Date() + days(60)))
    })
    
# Tab 2
    
    output$person_download <- downloadHandler(
      filename = function() {
        paste(input$list_name, "Training Due", Sys.Date(), ".csv", sep = " ")
      },
      content = function(file) {
        write.csv(df_person(), file)
      }
    )
    
    output$datatable_person <- renderTable({
        df_person()
      })

    
# Tab 3      
    
    output$txt_due_table <- renderText("Expired Roll Up")
    output$tables_due <- renderTable(table_expiry() %>% count(Training) %>% arrange(desc(n)), colnames = FALSE)
    
    output$txt_30_table <- renderText("30 Days Out Roll Up")
    table_30_expiry <- reactive(rbind(table_expiry(), table_30()))
    output$tables_30 <- renderTable((table_30_expiry() %>% count(Training) %>% arrange(desc(n))), colnames = FALSE)
    
    output$txt_60_table <- renderText("60 Days Out Roll Up")
    table_60_expiry <- reactive(rbind(table_expiry(), table_60()))
    output$tables_60 <- renderTable(table_60_expiry() %>% count(Training) %>% arrange(desc(n)), colnames = FALSE)
    
    
# Tab 4      
    
    output$freq <- renderTable(freq)
}

# Run the application 
shinyApp(ui = ui, server = server)
