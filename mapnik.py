# uses mapnik 2
import mapnik2
symbolizer = mapnik2.PolygonSymbolizer(
mapnik2.Color("darkgrey"))
rule = mapnik2.Rule()
rule.symbols.append(symbolizer)
style = mapnik2.Style()
style.rules.append(rule)
layer = mapnik2.Layer("mapLayer")
layer.datasource = mapnik2.Shapefile(
file="/data/82945364-10m-admin-0-countries.shp")
layer.styles.append("mapStyle")
map = mapnik2.Map(2400, 1200)
map.background = mapnik2.Color("black")
map.append_style("mapStyle", style)
map.layers.append(layer)
map.zoom_all()
mapnik2.render_to_file(map, "map.png", "png")



