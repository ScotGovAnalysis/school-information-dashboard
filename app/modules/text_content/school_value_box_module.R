
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
  
  # Attendance Value Box ----
  
  output$attendance <- renderValueBox({
    valueBox(value = p("Attendance", style = "font-size: 75%;"),
             subtitle = h3(data()$attendance),
             icon = icon("line-chart"), 
             color = "teal")
  })
  
  onclick(
    "attendance", 
    showModal(modal_with_x(
      title = "Attendance",
      content = tagList(
        p("The attendance rate for your chosen school/area. Attendance and 
          absence data is collected from publicly funded schools every 
          two years"),
        p("More information can be found here:",
          a("Schools Summary Statistics Link", 
            href= "https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland",
            target = "_blank"))
      )
    ))
  )
  
  # Average Class ----
  
  output$average_class <- renderValueBox({
    valueBox(value = p("Average Class Size", style = "font-size: 75%;"),
             subtitle = h3(data()$average_class),
             icon = icon("user"), 
             color = "teal")
  })
  
  onclick(
    "average_class", 
    showModal(modal_with_x(
      title = "Average Class Size",
      content = tagList(
        p("Information on the pupil numbers, teacher numbers and class sizes 
          are collected from publicly funded schools every year."),
        p("The information shown here is from the Pupil and Teacher Census a 
          link to which is here:",
          a("School Education Statistics",
            href = "https://www.gov.scot/collections/school-education-statistics/",
            target = "_blank")),
        p("Note that overall local authority level data may not be the same as 
          the sum of all school data within that local authority, 
          as local authorities may have teachers recorded at virtual schools."),
        p("For more information please see Section 7 in the Frequently Asked 
          Questions or you can find the data publication here:",
          a("Schools Summary Statistics Link",
            href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/",
            target = "_blank")
        )
      )
    ))
  )
  
  # PE Target ----
  
  output$pe <- renderValueBox({
    valueBox(value = p("Meeting PE Target", style = "font-size: 75%;"),
             subtitle = h3(data()$pe_target),
             icon = icon("fa-solid fa-bullseye"), 
             color = "teal")
  })
  
  onclick(
    "pe", 
    showModal(modal_with_x(
      title = "Meeting PE Target",
      content = tagList(
        p("Primary schools have a target to provide 120 minutes of Physical 
          Education (PE) a week for pupils. This shows whether the school 
          (or the percentage of schools in your chosen area) is meeting this 
          target or not."),
        p("Information is collected in February every year in the Healthy Living 
          Survey. A link to the Healthy Living Survey can be found here:",
          a("Healthy Living Survey Link", 
            href="https://www.gov.scot/collections/school-education-statistics/#schoolhealthylivingsurveystatistics(mealsandpe)",
            target = "_blank")
        )
      )
    ))
  )
  
  # Pupil Numbers ----
  
  output$pup_num <- renderValueBox({
    valueBox(value = p("Pupil Numbers", style = "font-size: 75%;"),
             subtitle = h3(data()$roll),
             icon = icon("bar-chart-o"), 
             color = "teal")
  })
  
  onclick(
    "pup_num", 
    showModal(modal_with_x(
      title = "Pupil Numbers",
      content = tagList(
        p("Information on the pupil numbers, teacher numbers and Class sizes are 
        collected from publicly funded schools every year."),
        p("The information shown here is from the Pupil and Teacher Census a 
        link to which is here:",
          a("School Education Statistics",
            href = "https://www.gov.scot/collections/school-education-statistics/",
            target = "_blank")
        ),
        p("Note that overall local authority level data may not be the same as 
          the sum of all school data within that local authority, as local 
          authorities may have teachers recorded at virtual schools."),
        p("For more information please see Section 7 in the Frequently Asked 
        Questions or you can find the data publication here:",
          a("Schools Summary Statistics Link", 
            href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/",
            target = "_blank")
        )
      )
    ))
  )
  
  # Teacher Numbers ----
  
  output$teach_num <- renderValueBox({
    valueBox(value = p("Teacher Numbers", style = "font-size: 75%;"),
             subtitle = h3(data()$fte_teacher_numbers),
             icon = icon("fa-regular fa-user-graduate"), 
             color = "teal")
  })
  
  onclick(
    "teach_num", 
    showModal(modal_with_x(
      title = "Teacher Numbers",
      content = tagList(
        p("Information on the pupil numbers, teacher numbers and Class sizes are 
        collected from publicly funded schools every year."),
        p("The information shown here is from the Pupil and Teacher Census a link 
        to which is here:",
          a("School Education Statistics",
            href = "https://www.gov.scot/collections/school-education-statistics/",
            target = "_blank")
        ),
        p("Note that overall local authority level data may not be the same as the 
        sum of all school data within that local authority, as local authorities 
        may have teachers recorded at virtual schools."),
        p("For more information please see Section 7 in the Frequently Asked 
        Questions or you can find the data publication here:",
          a("Schools Summary Statistics Link", href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/"))
      )
    ))
  )
  
  # Pupil Teacher Ratio ----
  
  output$ptr <- renderValueBox({
    valueBox(value = p("Pupil Teacher Ratio", style = "font-size: 75%;"),
             subtitle = h3(data()$ptr),
             icon = icon("fa-solid fa-chart-pie"), 
             color = "teal")
  })
  
  onclick(
    'ptr', 
    showModal(modal_with_x(
      title = "Pupil Teacher Ratio",
      content = tagList(
        p("Information on the pupil numbers, teacher numbers and Class sizes are 
          collected from publicly funded schools every year."),
        p("The information shown here is from the Pupil and Teacher Census a link 
          to which is here:",
          a("School Education Statistics",
            href = "https://www.gov.scot/collections/school-education-statistics/",
            target = "_blank")
        ),
        p("Note that overall local authority level data may not be the same as 
          the sum of all school data within that local authority, as local 
          authorities may have teachers recorded at virtual schools."),
        p("For more information please see Section 7 in the Frequently Asked 
          Questions or you can find the data publication here:",
          a("Schools Summary Statistics Link", 
            href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/",
            target = "_blank")
        )
      )
    ))
  )
  
  # Condition ----
  
  output$condition <- renderValueBox({
    valueBox(
      value = p("School Condition", style = "font-size: 75%;"),
      subtitle = h3(paste(data()$condition, 
                          ifelse(str_starts(data()$school_name, "All "),
                                 "in A or B",
                                 ""))),
      icon = icon("fa-solid fa-school"), 
      color = "teal")
  })
  
  onclick(
    "condition", 
    showModal(modal_with_x(
      title = "School Condition",
      content = tagList(
        p("The recorded condition of your chosen school/area."),
        tags$ul(
          tags$li("A - Good"),
          tags$li("B - Satisfactory"),
          tags$li("C - Poor"),
          tags$li("D - Bad"),
        ),
        p("Information is collected in April every year as part of the schools 
          estate collection which can be found using this link:",
          a("School Estate Statistics Link",
            href="https://www.gov.scot/collections/school-education-statistics/#schoolestatesstatistics",
            target = "_blank")
        )
      )
    ))
  )
  
  # Denomination ----
  
  output$denomination <- renderValueBox({
    valueBox(
      value = p("Denomination", style = "font-size: 75%;"),
      subtitle = h3(ifelse(str_starts(data()$school_name, "All "),
                           "z",
                           data()$denomination)),
      icon = icon("fa-solid fa-users"), 
      color = "teal")
  })
  
  onclick(
    "denomination", 
    showModal(modal_with_x(
      title = "Denomination",
      content = tagList(
        p("This reports the denomination of the school."),
        p("The publication can be found using this link:",
          a("Schools Summary Statistics Link", 
            href="https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland",
            target = "_blank")
        )
      )
    ))
  )
  
}