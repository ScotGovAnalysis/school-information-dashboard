introduction_server <- function(input, output, session) {
  
  showModal(modal_with_x(
    
    title = "Introduction", 
    
    content = tagList(
      
      p("The School Information Dashboards bring together a range of published",
        "information."),
      
      p("There are three dashboards - ",
        a("Primary,",
          href = "https://scotland.shinyapps.io/sg-primary_school_information_dashboard",
          target = "_blank"),
        a("Secondary,",
          href = "https://scotland.shinyapps.io/sg-secondary_school_information_dashboard",
          target = "_blank"),
        "and",
        a("Special",
          href = "https://scotland.shinyapps.io/sg-special_school_information_dashboard",
          target = "_blank"),
        "School Dashboards."),
      
      p("The School Information Dashboards add to the information already ",
        "provided by schools. This information is designed to help better ",
        "understand schools and encourage communication between parents/",
        "carers and schools."),
      
      p("It is important to remember that statistical data alone is not a ",
        "measure of how well a school is doing. All schools are unique. To ",
        "understand how well a school is doing it is important to look at a ",
        "range of different data sources."),
      
      p("For more detail on the importance of gathering and sharing accurate",
        "information follow this link: ",
        a("National Improvement Framework in a Nutshell - National Parent ",
          "Forum of Scotland",
          href="https://www.npfs.org.uk",
          target = "_blank")),
      
      p("If you have questions about the information on the dashboard for a ",
        "particular school, then you should contact that school directly. ",
        "School contact details are available on the dashboard. School ",
        "website details are available on Parentzone Scotland.")
      
    )
    
  ))
  
}