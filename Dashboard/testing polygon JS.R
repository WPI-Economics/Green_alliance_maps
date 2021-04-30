library(sf)
library(plotly)
library(leaflet)
library(crosstalk)
library(htmltools)
library(reactable)



pcon.bounds <- readRDS("Dashboard/GA_DB.RDS")
pcon.bounds <- st_transform(pcon.bounds, 4326)


boroughs_sd <- SharedData$new(
  pcon.bounds,
  key=~Constituency,
  group = "boroughs"

)

# boroughs_sd <- SharedData$new(
#   boroughs,
#   key=~BoroCode,
#   # provide explicit group so we can easily refer to this later
#   group = "boroughs"
# )

map <- leaflet(boroughs_sd) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data=pcon.bounds,
    layerId = ~Constituency,
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 0,
    fillColor = "white"#,
    #  turn off highlight since it interferes with selection styling
    #   if careful with styling could have both highlight and select
    #    highlightOptions = highlightOptions(color = "white", weight = 2)
  )






# borrow from https://github.com/r-spatial/mapedit/blob/master/R/query.R#L73-L132
#   to select/deselect features but instead of Shiny.onInputChange
#   use crosstalk to manage state
add_select_script <- function(lf, styleFalse, styleTrue, ns="") {
  ## check for existing onRender jsHook?
  
  htmlwidgets::onRender(
    lf,
    sprintf(
      "
function(el,x) {
  var lf = this;
  var style_obj = {
    'false': %s,
    'true': %s
  }

  // instead of shiny input as our state manager
  //   use crosstalk
  if(crosstalk) {
    var ct_sel = new crosstalk.SelectionHandle()
    ct_sel.setGroup('boroughs')
    ct_sel.on('change', function(x){
      if(x.sender !== ct_sel) { //ignore select from this map
        lf.eachLayer(function(lyr){
          if(lyr.options && lyr.options.layerId) {
            var id = String(lyr.options.layerId)
            if(
              !x.value  ||
              (Array.isArray(x.value) && x.value.indexOf(id) === -1)
            ) {
              toggle_state(lyr, false)
              toggle_style(lyr, style_obj.false)
            }
            if(Array.isArray(x.value) && x.value.indexOf(id) > -1) {
              toggle_state(lyr, true)
              toggle_style(lyr, style_obj.true)
            }
          }
        })
      }
    })
  }

  // define our functions for toggling
  function toggle_style(layer, style_obj) {
    layer.setStyle(style_obj);
  };
  function toggle_state(layer, selected, init) {
    if(typeof(selected) !== 'undefined') {
      layer._mapedit_selected = selected;
    } else {
      selected = !layer._mapedit_selected;
      layer._mapedit_selected = selected;
    }
    if(typeof(Shiny) !== 'undefined' && Shiny.onInputChange && !init) {
      Shiny.onInputChange(
        '%s-mapedit_selected',
        {
          'group': layer.options.group,
          'id': layer.options.layerId,
          'selected': selected
        }
      )
    }

    if(ct_sel) {
      var ct_values = ct_sel.value
      var id = String(layer.options.layerId)
      if(selected) {
        if(!ct_values) {
          ct_sel.set([id])
        }
        if(Array.isArray(ct_values) && ct_values.indexOf(id) === -1) {
          ct_sel.set(ct_values.concat(id))
        }
      }

      if(ct_values && !selected) {
        ct_values.length > 1 ?
          ct_sel.set(
            ct_values.filter(function(d) {
              return d !== id
            })
          ) :
          ct_sel.set(null) // select all if nothing selected
      }
    }

    return selected;
  };
  // set up click handler on each layer with a group name
  lf.eachLayer(function(lyr){
    if(lyr.on && lyr.options && lyr.options.layerId) {
      // start with all unselected ?
      toggle_state(lyr, false, init=true);
      toggle_style(lyr, style_obj[lyr._mapedit_selected]);
      lyr.on('click',function(e){
        var selected = toggle_state(e.target);
        toggle_style(e.target, style_obj[String(selected)]);
      });
    }
  });
}
",
jsonlite::toJSON(styleFalse, auto_unbox=TRUE),
jsonlite::toJSON(styleTrue, auto_unbox=TRUE),
ns
    )
  )
}

# browsable(
#   tagList(
#     tags$div(
#       style = "float:left; width: 49%;",
#       add_select_script(
#         map,
#         styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0.4, color="black"),
#         styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7, color="blue")
#       )
#     ),
#     tags$div(
#       style = "float:left; width: 49%;",
#       plot_ly(boroughs_sd, x = ~x, y = ~y) %>%
#         add_markers(alpha = 0.5,text = ~paste('Borough: ', BoroName)) %>%
#         highlight(on = "plotly_selected")
#     )
#   )
# )


# tbl <- reactable(boroughs_sd,
#                  rowStyle = list(cursor = "pointer"),onClick = "select",
#                  #minRows = 10,
#                  filterable = F,
#                  searchable = F, wrap = T, pagination = FALSE, striped = T, highlight = T)


tbl <- reactable(boroughs_sd, #selection = "single",
                 onClick = "select",
                 rowStyle = list(cursor = "pointer"),
                 #minRows = 10,
                 filterable = F,
                 searchable = F, wrap = T, pagination = FALSE, striped = T, highlight = T,
                 defaultSorted = list("Constituency" = "asc"),
                 defaultColDef = colDef(align = "center"),
                 columns = list(
                   pcon19cd = colDef(show = F),
                   Constituency = colDef(align = "left"),
                   #`Labour market challenge score (100 = average)`= colDef(filterable = F),
                   #`Forecast change in employments 2019-2025 (%)`= colDef(filterable = F),
                   #`Underemployment Sept 2019 (% 16-64)`= colDef(filterable = F),
                   #`Underemployment change Sept 2019 - Sept 2020 (%)`= colDef(filterable = F),
                   #`Known Seagrass location within 1000m` = colDef(filterable = T),s
                   `Woodland area (ha)` = colDef(filterable = F, format = colFormat(separators = T)),
                   `Share of woodland` = colDef(filterable = F, show = F),
                   
                   `Priority sites coastal restoration sites (count)` = colDef(show = F),
                   `All coastal restoration sites (count)` = colDef(show = F),
                   
                   #`Coastal restoration sites flag` = colDef(filterable = T),
                   #`Coastal restoration priority sites flag` = colDef(filterable = T),
                   #`Within 20 miles of Great North Bog` = colDef(filterable = T),
                   
                   geometry = colDef(show = F),
                   `Coastal restoration priority sites flag`  = colDef(show = F),
                   cols = colDef(show = F),
                   LM2cols = colDef(show = F),
                   LM3cols = colDef(show = F),
                   LM4cols = colDef(show = F)
                 ),
                 
                 
                 
                 #`COVID-19 deaths per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0)),
                 #`COVID-19 deaths age adjusted per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0))),
                 theme = reactableTheme(
                   stripedColor = "#f3f5f4",
                   highlightColor = "#C3CDC8",
                   cellPadding = "4px 8px",
                   style = list(fontFamily = "Franklin Gothic Book", fontSize = "10px"),
                   #searchInputStyle = list(width = "100%", fontWeight = "400"),
                   headerStyle = list(color = "white",background = "#54716E",
                                      "&:hover[aria-sort]" = list(background = "#C3CDC8"),
                                      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#C3CDC8"),
                                      borderColor = "#00946E"
                   )
                 ))


browsable(
  tagList(
    tags$div(
      style = "float:left; width: 49%;",
      add_select_script(
        map,
        styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0, color="white"),
        styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7, color="blue")
      )
    ),
    tags$div(
      style = "float:left; width: 49%;",
      tbl 
    )
  )
)



