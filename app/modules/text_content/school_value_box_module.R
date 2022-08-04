
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
      valueBoxOutput(ns("condtion"), width = 4)
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
    valueBox(value = tags$p("Attendance", style = "font-size: 75%;"),
             subtitle = tags$p(h3(paste(data()$attendance))),
             icon = icon("line-chart"), 
             color = "teal")
  })
  
  onclick('attendance', showModal(modalDialog(
    title = "Attendance",
    p("This shows you the attendance rate for your chosen school/area"),
    p("Attendance and absence data is collected from publicly funded schools every two years")
  ))
  )
  
  # Average Class
  output$average_class <- renderValueBox({
    valueBox(value = tags$p("Average Class Size", style = "font-size: 75%;"),
             tags$p(h3(data()$average_class)),
             icon = icon("user"), 
             color = "teal")
  })
  
  onclick('average_class', showModal(modalDialog(
    title = "Average Class Size",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  # PE Target
  output$pe <- renderValueBox({
    valueBox(value = tags$p("Meeting PE Target", style = "font-size: 75%;"),
             tags$p(h3(data()$pe_target)),
             icon = icon("fa-solid fa-bullseye"), 
             color = "teal")
  })
  
  onclick('pe', showModal(modalDialog(
    title = "Meeting PE Target",
    p("Primary schools have a target to provide 120 minutes of Physical Education (PE) a week for pupils."),
    p("This shows whether the school (or the percetnage of schools in your chosen area) is meeting this target or not."),
    p("Information is collected in February every year in the Healthy Living Survey. A link to the Halthy Living Survey can be found here: ")
  ))
  )
  
  # Pupil Numbers
  output$pup_num <- renderValueBox({
    valueBox(value = tags$p("Pupil Numbers", style = "font-size: 75%;"),
             tags$p(h3(data()$roll)),
             icon = icon("bar-chart-o"), 
             color = "teal")
  })
  
  onclick('pup_num', showModal(modalDialog(
    title = "Pupil Numbers",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  # Teacher Numbers
  output$teach_num <- renderValueBox({
    valueBox(value = tags$p("Teacher Numbers", style = "font-size: 75%;"),
             tags$p(h3(data()$fte_teacher_numbers)),
             icon = icon("fa-regular fa-user-graduate"), 
             color = "teal")
  })
  
  onclick('teach_num', showModal(modalDialog(
    title = "Teacher Numbers",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  # Pupil Teacher Ratio
  output$ptr <- renderValueBox({
    valueBox(value = tags$p("Pupil Teacher Ratio", style = "font-size: 75%;"),
             tags$p(h3(data()$ptr)),
             icon = icon("fa-solid fa-chart-pie"), 
             color = "teal")
  })
  
  onclick('ptr', showModal(modalDialog(
    title = "Pupil Teacher Ratio",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  # Condition
  output$condition <- renderValueBox({
    valueBox(value = tags$p("School Condition", style = "font-size: 75%;"),
             tags$p(h3(data()$condition)),
             icon = icon("fa-solid fa-chart-pie"), 
             color = "teal")
  })
  
  onclick('condition', showModal(modalDialog(
    title = "School Condition",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  # Denomination
  output$denomination <- renderValueBox({
    valueBox(value = tags$p("Denomination", style = "font-size: 75%;"),
             tags$p(h3(data()$denomination)),
             icon = icon("fa-solid fa-chart-pie"), 
             color = "teal")
  })
  
  onclick('denomination', showModal(modalDialog(
    title = "Denomination",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
}