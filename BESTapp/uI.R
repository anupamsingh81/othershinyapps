library(shiny)

shinyUI(pageWithSidebar(
  
  
  headerPanel("Comparing Two Independent Samples"),
  
  
  sidebarPanel(
    
    p('Input values can be separated by', br(),
      'newlines, spaces, commas, or tabs.'),
    
    p(strong("Data 1:")),
    tags$textarea(id="data1", rows=20, cols=10, "50\n56\n79\n99\n56\n66\n67\n81\n55\n44\n45\n43\n77\n72\n60\n37\n39\n56\n66\n85\n55"),
    
    p(br()),
    
    p(strong("Data 2:")),
    tags$textarea(id="data2", rows=20, cols=10, "22\n100\n45\n66\n77\n88\n76\n79\n44\n55\n65\n76\n66\n44\n32\n55\n56\n57\n77\n65\n40\n41\n49\n60"),
    
    p(br())
    
    
    ),
  
  mainPanel(
    
      h1("This displays a box plot of your data"),
     verbatimTextOutput("summary.out"))
    )
  )




  