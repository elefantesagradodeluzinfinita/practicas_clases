install.packages("httr")
install.packages("XML")
library(httr)
library(XML)

response <- GET("https://www.mediawiki.org/wiki/MediaWiki")

status_code(response)

content <- content(response, as = "text")
parsedHtml <- htmlParse(content, asText = TRUE)
title <- xpathSApply(parsedHtml, "//title", xmlValue)
                        
#xml_content <- htmlParse(content(response), asText = TRUE)

#page_content <- content(response, as = "text")
print(title)

links_text <- xpathSApply(parsedHtml, "//a", xmlValue)
links_url <- xpathSApply(parsedHtml, "//a", xmlGetAttr, 'href')

print(links_text)
print(links_url)

#creacion de la tabla, pregunta 1, sector 4

tabla <- data.frame(links_text = character(),
                    links_url = character(),
                    repeticiones = numeric(),
                    scraps = character(),
                    stringsAsFactors = FALSE)

frecuencia <- table(links_url)

for (i in 1:length(links_text)) {
  Sys.sleep(4)
  print("round done")
  tabla[i, "links_text"] <- links_text[i]
  tabla[i, "links_url"] <- links_url[i]
  tabla[i, "repeticiones"] <- frecuencia[links_url[i]]
  
  #si inicia con /wiki/
  validation_wiki <- startsWith(links_url[i], "/wiki/")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org", links_url[i])
  }
  
  #si inicia con /https/
  validation_wiki <- startsWith(links_url[i], "https:")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("", links_url[i])
  }
  
  #si inicia con //
  validation_wiki <- startsWith(links_url[i], "//")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https:", links_url[i])
  }
  
  #si inicia con /w/
  validation_wiki <- startsWith(links_url[i], "/w/")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org", links_url[i])
  }
  
  #si inicia con /#/
  validation_wiki <- startsWith(links_url[i], "#")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org/wiki/MediaWiki", links_url[i])
  }
  
  print(paste0("TEST> " , links_url[i]))
  
  #obtencion de STATUS CODE
  code <- status_code(HEAD(tabla[i, "links_url"]))
  tabla[i, "scraps"] <- code
  print(code)
}

head(tabla)

#pare ver el contenido de todos los elementos, ver tabla

tabla

#######################################################################

install.packages("ggplot2")
library(ggplot2)

# Crear las listas de valores
url_name <- c("https://www.ejemplo1.com", "https://www.ejemplo2.com", "https://www.ejemplo3.com", "https://www.ejemplo4.com")
url_count <- c(10, 20, 30, 40)

a <- tabla["links_url"]
b <- tabla["repeticiones"]

# Crear un data frame con las listas de valores
datos <- data.frame(c(a), url_count)

# Crear un objeto de gráfico utilizando ggplot()
grafico <- ggplot(datos, aes(x = c(a), y = url_count))

# Agregar una capa de barras utilizando geom_bar()
grafico <- grafico + geom_bar(stat = "identity")

# Personalizar el gráfico utilizando labs() y theme()
grafico <- grafico + labs(x = "URL", y = "Conteo") + theme_bw()

# Imprimir el gráfico utilizando print()
print(grafico)