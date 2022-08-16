
school_value_box_output <- function(id, school_type) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  # Value Box Output
  tagList(
    valueBoxOutput(ns("attendance"), width = 4),
    
    if(school_type == "Primary") {
    valueBoxOutput(ns("average_class"), width = 4)
    },
    
    if(school_type %in% c("Primary", "Secondary")) {
    valueBoxOutput(ns("pe"), width = 4)
    },
    
    if(school_type %in% c("Secondary", "Special")) {
      valueBoxOutput(ns("condition"), width = 4)
    },
    
    if(school_type == "Special") {
      valueBoxOutput(ns("denomination"), width = 4)
    },
    
    valueBoxOutput(ns("pup_num"), width = 4),
    valueBoxOutput(ns("teach_num"), width = 4),
    valueBoxOutput(ns("ptr"), width = 4)
  )
  
}

school_value_box_server <- function(input, output, session, data) {
  
  # Attendance Value Box
  output$attendance <- renderValueBox({
    valueBox(value = p("Attendance", style = "font-size: 75%;"),
             subtitle = h3(data()$attendance),
             icon = icon("line-chart"), 
             color = "teal")
  })
  
  onclick('attendance', showModal(modalDialog(
    title = "Attendance",
    ("This shows you the attendance rate for your chosen school/area"),
    p("Attendance and absence data is collected from publicly funded schools every two years"),
    p("More information can be found here:",
      a("Schools Summary Statistics Link", href= "https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland"))
  ))
  )
  
  # Average Class
  output$average_class <- renderValueBox({
    valueBox(value = p("Average Class Size", style = "font-size: 75%;"),
             subtitle = h3(data()$average_class),
             icon = icon("user"), 
             color = "teal")
  })
  
  onclick('average_class', showModal(modalDialog(
    title = "Average Class Size",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, 
    as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions or you can find the data publication here:",
      a(href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/"))
  ))
  )
  
  # PE Target
  output$pe <- renderValueBox({
    valueBox(value = p("Meeting PE Target", style = "font-size: 75%;"),
             subtitle = h3(data()$pe_target),
             icon = icon("fa-solid fa-bullseye"), 
             color = "teal")
  })
  
  onclick('pe', showModal(modalDialog(
    title = "Meeting PE Target",
    p("Primary schools have a target to provide 120 minutes of Physical Education (PE) a week for pupils."),
    p("This shows whether the school (or the percetnage of schools in your chosen area) is meeting this target or not."),
    p("Information is collected in February every year in the Healthy Living Survey. A link to the Healthy Living Survey can be found here:",
      a(href="https://www.gov.scot/publications/attainment-leaver-destinations-healthy-living-summary-statistics/pages/2/)"))
    
    
  ))
  )
  
  # Pupil Numbers
  output$pup_num <- renderValueBox({
    valueBox(value = p("Pupil Numbers", style = "font-size: 75%;"),
             subtitle = h3(data()$roll),
             icon = icon("bar-chart-o"), 
             color = "teal")
  })
  
  onclick('pup_num', showModal(modalDialog(
    title = "Pupil Numbers",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions or you can find the data publication here:",
      a(href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/"))
  ))
  )
  
  # Teacher Numbers
  output$teach_num <- renderValueBox({
    valueBox(value = p("Teacher Numbers", style = "font-size: 75%;"),
             subtitle = h3(data()$fte_teacher_numbers),
             icon = icon("fa-regular fa-user-graduate"), 
             color = "teal")
  })
  
  onclick('teach_num', showModal(modalDialog(
    title = "Teacher Numbers",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions or you can find the data publication here:",
      a(href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/"))
  ))
  )
  
  # Pupil Teacher Ratio
  output$ptr <- renderValueBox({
    valueBox(value = p("Pupil Teacher Ratio", style = "font-size: 75%;"),
             subtitle = h3(data()$ptr),
             icon = icon("fa-solid fa-chart-pie"), 
             color = "teal")
  })
  
  onclick('ptr', showModal(modalDialog(
    title = "Pupil Teacher Ratio",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questionsor you can find the data publication here:",
      a(href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/"))
  ))
  )
  
  # Condition
  output$condition <- renderValueBox({
    valueBox(value = p("School Condition", style = "font-size: 75%;"),
             subtitle = h3(paste(data()$condition, 
                                 ifelse(str_starts(data()$school_name, "All "),
                                        " in A or B",
                                        ""))),
             icon = icon("fa-solid fa-chart-pie"), 
             color = "teal")
  })
  
  onclick('condition', showModal(modalDialog(
    title = "School Condition",
    p("school condition: shows the recorded condition of your chosen school/area."),
    p("A - Goood"),
    p("B - Satisfactory"),
    p("C - Poor"),
    p("D - Bad"),
    p("Information is collected in April every year as part of the schols estate collection which can be found using this link:",
      a(href="https://www.gov.scot/collections/school-education-statistics/#schoolestatesstatistics"))
  ))
  )
  
  # Denomination
  output$denomination <- renderValueBox({
    valueBox(value = p("Denomination", style = "font-size: 75%;"),
             subtitle = h3(ifelse(str_starts(data()$school_name, "All "),
                                  "z",
                                  data()$denomination)),
             icon = icon("fa-solid fa-chart-pie"), 
             color = "teal")
  })
  
  onclick('denomination', showModal(modalDialog(
    title = "Denomination",
    p("This reports the denomintion of the school"),
    p("The publication can be found using this link:",
      a(href="https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland"))
  ))
  )
  
}