library(shiny)
library(ggforce)
library(colourpicker)
# library(shinyCopy2clipboard)

xyaba <- sample(1:20, 5, replace = TRUE)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sketchy"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x", "x",
                  min = 1,
                  max = 20,
                  value = xyaba[1]),
      sliderInput("y", "y",
                  min = 1,
                  max = 20,
                  value = xyaba[2]),
      sliderInput("a", "a",
                  min = 1,
                  max = 20,
                  value = xyaba[3]),
      sliderInput("b", "b",
                  min = 1,
                  max = 20,
                  value = xyaba[4]),
      sliderInput("angle", "angle",
                  min = 1,
                  max = 20,
                  value = xyaba[5]),
      sliderInput("s", "s",
                  min = 0,
                  max = 1000,
                  value = 500, 
                  step = 100),
      colourInput("col", "Color", "black"),
      colourInput("bg", "Background", "white"),
      br(),
      splitLayout(
        actionButton("randButt", "Random"),
        downloadButton("downButt", "Download")
      )
    ),
    
    mainPanel(
      plotOutput("plot"),
      br(),
      htmlOutput("code")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$randButt, {
    xyaba <- reactiveValues(
      x = sample(1:20, 1),
      y = sample(1:20, 1),
      a = sample(1:20, 1),
      b = sample(1:20, 1),
      angle = sample(1:20, 1)
    )
    
    updateSliderInput(session, "x", value = xyaba$x)
    updateSliderInput(session, "y", value = xyaba$y)
    updateSliderInput(session, "a", value = xyaba$a)
    updateSliderInput(session, "b", value = xyaba$b)
    updateSliderInput(session, "angle", value = xyaba$angle)
  })
  
  p <- reactive({
    ggplot() + 
      geom_ellipse(aes(
        x0 = sin(seq(0, input$x * pi, length.out = input$s)),
        y0 = cos(seq(0, input$y * pi, length.out = input$s)),
        a = cos(seq(0, input$a * pi, length.out = input$s)),
        b = cos(seq(0, input$b * pi, length.out = input$s)),
        angle = cos(seq(0, input$angle * pi, length.out = input$s))
      ), n = 50, size = 0.1, color = input$col) + 
      coord_fixed() + 
      scale_color_viridis_c() +
      theme_void() +
      theme(
        plot.background = element_rect(fill = input$bg, color = NA)
      )
  })
  
  output$plot <- renderPlot({
    p()
  })
  
  output$code <- renderText(
    HTML(paste0(
      "library(ggforce)</br>
        </br>
        ggplot() +</br>
        &emsp;geom_ellipse(aes(</br>
        &emsp;&emsp;x0 = sin(seq(0, <b>", input$x, "</b> * pi, length.out = <b>", input$s, "</b>)),</br>
        &emsp;&emsp;y0 = cos(seq(0, <b>", input$y, "</b> * pi, length.out = <b>", input$s, "</b>)),</br>
        &emsp;&emsp;a = cos(seq(0, <b>", input$a, "</b> * pi, length.out = <b>", input$s, "</b>)),</br>
        &emsp;&emsp;b = cos(seq(0, <b>", input$b, "</b> * pi, length.out = <b>", input$s, "</b>)),</br>
        &emsp;&emsp;angle = cos(seq(0, <b>", input$angle, "</b> * pi, length.out = <b>", input$s, "</b>))</br>
        &emsp;), n = 50, size = 0.1, color = <b>\"", input$col, "\"</b>) + </br>
        &emsp;coord_fixed() + </br>
        &emsp;theme_void() + </br>
        &emsp;theme(plot.background = element_rect(fill = <b>\"", input$bg, "\"</b>, color = NA))"
    ))
  )
  
  output$downButt = downloadHandler(
    filename = "ellipses.png",
    content = function(file){
      ggsave(file, p(), device = "png")
    })
}

shinyApp(ui = ui, server = server)
