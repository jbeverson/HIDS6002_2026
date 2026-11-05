library(shiny)
library(httr2)

BASE <- "https://hapi.fhir.org/baseR4"
id   <- "hids6002-p01"


req <- request(BASE) |> ## drafts a request whose address is the server URL,
  req_url_path_append("Patient", id) |> ## Adds path pieces to the end of the address, separated by slashes. 
  req_url_query(`_format` = "json") ## Specifies a format for the response, in this case JSON

print(req$url)
resp <- req_perform(req) ## Executes the request and returns a response object

print(resp)

patient <- resp_body_json(resp)     # JSON becomes an R list
## Let's look and see what that looks like. It is a list that matches the JSON structure we saw in the browser.

patient[["name"]][[1]][["family"]]


ui <- fluidPage(
  titlePanel("FHIR: Patient family name"),
  textInput("id", "Patient ID", width = "400px"),
  actionButton("go", "Look up"), ## inputID is what we use to refer to 
                                 ## the input in the server function.
                                 ## label is what displays in the app.
  h4("Family name"),
  textOutput("family")
)

server <- function(input, output, session) {
  
  patient <- eventReactive(input$go, {       
    id_fhir   <- input$id
    req <- request(BASE) |> ## drafts a request whose address is the server URL,
    req_url_path_append("Patient",  id_fhir) |> ## Adds path pieces to the end of the address, separated by slashes. 
    req_url_query(`_format` = "json") ## Specifies a format for the response, in this case JSON
    print(req$url)
    resp <- req_perform(req) ## Executes the request and returns a response object
    print(resp)
    patient <- resp_body_json(resp)     # JSON becomes an R list 
  })
  
  ## So now we call our patient function instead of using the patient variable when this was standalone.
  ## That creates the patient on the fly - nothing is stored.
  ## But we can still access the family name in the same way, just using patient() instead of patient.
  output$family <- renderText({
    patient()[["name"]][[1]][["family"]] 
  })
}

shinyApp(ui, server)