

sd %>% reactable(
  columns = list(
    geometry = colDef(show = F), 
    pcon19cd = colDef(show = F)),
  
  
  
)

#ADA931 main green
#DCCC59 contrast green
#C3CDC8 light grey
#8B9C98 darker grey


tbl <- reactable(sd, selection = "multiple",
                 onClick = "select",
                 rowStyle = list(cursor = "pointer"),
                 minRows = 10,filterable = F,searchable = F, wrap = T , defaultPageSize = 15, striped = T, highlight = T,
                 defaultSorted = list("Constituency" = "asc"),
                 columns = list(`Seagrass location within 1000m` = colDef(filterable = T),
                                `Coastal resoration sites flag` = colDef(filterable = T),
                                `Share of woodland (cumulative %)` = colDef(filterable = T),
                                `Within 10k of great north bog` = colDef(filterable = T),
                                `Coastal resoration priority sites flag` = colDef(filterable = T),
                                geometry = colDef(show = F), 
                                pcon19cd = colDef(show = F)
                                ),
                 #`COVID-19 deaths per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0)),
                 #`COVID-19 deaths age adjusted per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0))),
                 theme = reactableTheme(
                   stripedColor = "#f3f5f4",
                   highlightColor = "#8B9C98",
                   cellPadding = "6px 10px",
                   style = list(fontFamily = "Franklin Gothic Book", fontSize = "12px"),
                   #searchInputStyle = list(width = "100%", fontWeight = "400"),
                   headerStyle = list(color = "white",background = "#ADA931",
                                      "&:hover[aria-sort]" = list(background = "#DCCC59"),
                                      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#DCCC59"),
                                      borderColor = "#555"
                   )
                 )) 
tbl



# df <- st_drop_geometry(df.sf)

dl.button <- df %>%
  download_this(
    output_name = "Greening the Economy",
    output_extension = ".csv",
    button_label = "Download data as csv",
    button_type = "default",
    has_icon = TRUE,
    icon = "fa fa-save"
  )

html_print(dl.button)
