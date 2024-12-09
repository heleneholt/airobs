library(httr)
library(rvest)
library(dplyr)
library(DBI)
library(RMariaDB)

# Opret forbindelse til databasen
con <- dbConnect(MariaDB(),
                 db = "airflow",
                 host = "13.60.21.240",
                 port = 3306,
                 user = "dalremote",
                 password = "Alfred1403")

# Definer funktion til at hente data fra en side
fetch_air_quality_data <- function(base_url, main_url, referer_url) {
  # GET-request for at hente CSRF-token og cookies
  resraw <- GET(url = base_url)
  if (status_code(resraw) != 200) {
    stop("Fejl ved GET-request til ", base_url, ": Status ", status_code(resraw))
  }
  
  # Ekstrakter cookies og CSRF-token
  mycookie <- resraw$cookies
  cookie_string <- paste(mycookie$name, mycookie$value, sep = "=", collapse = "; ")
  
  hhc <- content(resraw, as = "text")
  csrf_token <- read_html(hhc) %>%
    html_node("input[name='__RequestVerificationToken']") %>%
    html_attr("value")
  
  # POST-body og headers
  body <- list(
    `__RequestVerificationToken` = csrf_token
  )
  
  headers <- c(
    "accept" = "*/*",
    "accept-encoding" = "gzip, deflate, br, zstd",
    "accept-language" = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
    "cookie" = cookie_string,
    "referer" = referer_url,
    "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  )
  
  # POST-request for at hente tabellen
  response <- POST(main_url, body = body, encode = "form", add_headers(.headers = headers))
  
  if (status_code(response) == 200) {
    cat("Anmodning til ", main_url, " lykkedes. Henter data...\n")
    
    # Parse HTML og find tabellen
    content_html <- content(response, as = "text")
    page <- read_html(content_html)
    table <- page %>% html_node("table") %>% html_table()
    
    # Returner tabellen
    return(table)
  } else {
    stop("Anmodning fejlede med status: ", status_code(response))
  }
}

# URLs for de fire sider
sites <- list(
  list(base_url = "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB",
       main_url = "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB",
       referer_url = "https://envs.au.dk/"),
  list(base_url = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE",
       main_url = "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/RISOE",
       referer_url = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"),
  list(base_url = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO",
       main_url = "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/ANHO",
       referer_url = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO"),
  list(base_url = "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3",
       main_url = "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Aarhus/AARH3",
       referer_url = "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3")
)

# Iterer gennem siderne og hent data
tables <- lapply(sites, function(site) {
  fetch_air_quality_data(site$base_url, site$main_url, site$referer_url)
})

# Udskriv tabellerne
names(tables) <- c("HC Andersen Boulevard", "Risø", "Anholt", "Århus Banegårdsgade")
print(tables)
