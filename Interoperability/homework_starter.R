# FHIR homework: pull lab values in a script, then in a Shiny app.
# Needs: install.packages(c("shiny", "httr2"))
#
# Parts 1 and 3 and the server's patient code are from class.
# Your work is marked HOMEWORK. Run each part before starting the next.

library(shiny)
library(httr2)

# To use a local server instead: Sys.setenv(FHIR_BASE = "http://localhost:8080/fhir")
BASE <- "https://hapi.fhir.org/baseR4"
id   <- "hids6002-p01"     


# ==== PART 1. The patient in a script (from class) ======================

# Build the request: server address + /Patient/[id] + ?_format=json
req <- request(BASE) |>
  req_url_path_append("Patient", id) |>
  req_url_query(`_format` = "json")

req$url                          # the URL we are about to call
resp <- req_perform(req)         # send it
print(resp)

patient <- resp_body_json(resp)  # JSON becomes an R list
patient[["name"]][[1]][["family"]]


# ==== PART 2. Two labs in a script (HOMEWORK) ===========================
# Follow Part 2 of the handout. Copy the Part 1 steps, but ask for
# Observations instead of a Patient. Do the first lab completely, then
# repeat the same steps for the second lab.

# ---- Lab 1: Hemoglobin A1c (LOINC 4548-4) ----


# ---- Lab 2: Creatinine (LOINC 2160-0) ----



# ==== PART 3. The Shiny app ==============================================

# HOMEWORK (Part 3 of the handout): the dropdown choices.
# Names are what the user sees. Values are what input$lab returns.


ui <- fluidPage(
  titlePanel("FHIR: Patient family name"),
  # First argument = the input's name, used in the server as input$id.
  # Second argument = the label shown in the app.
  textInput("id", "Patient ID", width = "400px"),
  # HOMEWORK (Part 3 of the handout): add the dropdown here.
  actionButton("go", "Look up"),
  h4("Family name"),
  textOutput("family")
  # HOMEWORK (Part 3 of the handout): add a heading and a text output for the lab here.
)

server <- function(input, output, session) {

  # eventReactive runs only when the button is clicked (input$go).
  # What it returns is read later as patient().
  patient <- eventReactive(input$go, {
    id_fhir <- input$id
    req <- request(BASE) |>
      req_url_path_append("Patient", id_fhir) |>
      req_url_query(`_format` = "json")
    print(req$url)               # shows in the R console, not in the app
    resp <- req_perform(req)
    print(resp)
    resp_body_json(resp)         # the last line is what the reactive returns
  })

  # patient has parentheses because it is a reactive.
  output$family <- renderText({
    patient()[["name"]][[1]][["family"]]
  })

  # HOMEWORK (Part 4 of the handout): the lab reactive and its output go here.

}

shinyApp(ui, server)
