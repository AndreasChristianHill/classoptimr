#' create_qml
#'
#' @description
#' Creates a qml-file of a given classification scheme produced by \code{\link{classAccuracy}}
#' for visualization of the respective prediction map in QGIS.
#'
#' @param x  object of class \code{'classaccur'} containing an evaluated classification scheme.
#' @param out.file destination-folder were the qml-file should be stored
#' @param pallete.name code of color-palette to be used for visualization
#'        (see \code{\link[RColorBrewer]{display.brewer.all}}).
#'        For an overview type \code{display.brewer.all()} into prompt.
#'        Defaults to "YlOrRd".
#' @param ...  additional arguments, so far ignored.
#'
#' @export
#'
create_qml<- function(x, ...){
  UseMethod("create_qml")
}


#' @rdname create_qml
#' @examples
#'
#'
#' ## -- create qml-file based on 'classaccur'-object:
#'
#' # 1) get optimal classification scheme:
#'
#' hsm<- HSMclass(refdata.gr, predictions.gr, nclasses = 6,
#'               iterations = 1000, coolfactor=0.99, InitTemp = 80,
#'               weight.norefs = 2, weight.classwidth = 2)
#'
#' # 2) evaluate classification scheme:
#' acc.opti<- classAccuracy(refdata.gr, predictions.gr, def.int = hsm$best.classbreaks)
#'
#' # 3) write qml-file for classification scheme:
#' \dontrun{
#' create_qml(acc.opti, out.file = "opti_class.qml", pallete.name = "YlOrRd")
#' }
#'
#' @import XML RColorBrewer
#' @importFrom grDevices col2rgb
#'
#'
#' @export
#' @method create_qml classaccur


create_qml.classaccur<- function(x, out.file, pallete.name = "YlOrRd", ...){

  stopifnot(inherits(x, "classaccur"))

  # Function creates qml.- Visualization file of a classification scheme created
  # by function "preval_volmap_rankextended.R". qml.-file can be used to visualized
  # polygonized prediction map in QGIS 2.10.1 "Pisa"

  # INPUTS:
  # x:        classification-object created by function "preval_volmap_rankextended.R"
  # out.file:     character specifying the destination of qml.-file to be saved
  # pallete.name: code of color-palette to be used for visualization (for overview type "display.brewer.all()" into prompt

  # Autor: Andreas Hill, 16.09.2015


  # ------------------------------------ #
  # create visualization table:

  if(!is.na(x["def.classbreaks"])){
     breaks<- "def.classbreaks"
  } else {
     breaks<- "equal.classbreaks"
  }

  n.class<- length(x[["classwidth"]])
  df.vis<- data.frame(matrix(data = NA, nrow = n.class, ncol = 5,
                             dimnames = list(c(seq(1,n.class,1)), c("lower","upper", "use.acc", "label","rgb"))))
  df.vis$lower<- x[[breaks]][- length(x[[breaks]])]
  df.vis$upper<- x[[breaks]][- 1]
  df.vis$use.acc<- round(as.numeric((x$usersaccuracy[,2])))
  df.vis$label<- apply(df.vis, 1, function(x){paste("(", x[1], " - ", x[2], "] (", x[3], " %)",sep = "")})
  mypalette<-brewer.pal(n = n.class, name = pallete.name)
  rgb.codes<- cbind(t(col2rgb(mypalette)), 255)
  df.vis$rgb<- apply(rgb.codes, 1, function(x){paste(x[1],",", x[2], ",",  x[3], ",",  x[4], sep = "")})


  # ------------------------------------ #
  # settings:

  # set alpha
  alpha<- 1

  # ------------------------------------ #
  # set up html-document:
  # ------------------------------------ #

  # ---------
  # 1: "qgis"- 1st Line
  base<- newXMLNode("qgis")
  addAttributes(base,version="2.10.1-Pisa",minimumScale="0", maximumScale="1e+08", simplifyDrawingHints="1",
                minLabelScale="0", maxLabelScale="1e+08", simplifyDrawingTol="1", simplifyMaxScale="1",
                hasScaleBasedVisibilityFlag="0", simplifyLocal="1", scaleBasedLabelVisibilityFlag="0")

  # ---------
  # 2: edittypes:

  edittypes<- newXMLNode("edittypes")
  edittype<- newXMLNode("edittype", attrs = c(widgetv2type="TextEdit", name="value"))
  widgetv2config<- newXMLNode("widgetv2config", attrs = c(IsMultiline="0", fieldEditable="1", UseHtml="0", labelOnTop="0"))

  addChildren(edittype, widgetv2config)
  addChildren(edittypes, edittype)



  # ---------
  # 3: renderer

  rend <- newXMLNode("renderer-v2", attrs = c(attr="value", symbollevels="0", type="graduatedSymbol",
                                              graduatedMethod="GraduatedColor"))

  # ---------
  # 4: ranges
  ranges <- newXMLNode("ranges")
  range <- lapply(seq_along(df.vis$lower), function(x){newXMLNode("range",
                                                            attrs = c(render="true", symbol = as.character(x-1),
                                                                          lower = df.vis$lower[x], upper = df.vis$upper[x],
                                                                          label = df.vis$label[x]))})
  addChildren(ranges, range)


  # ---------
  # 5: symbols
  symbols <- newXMLNode("symbols")


  # ---------
  # 6: symbol-block --> the loop over n_classes [seq_along(df.vis$lower)] starts here:

  symbol.block <- lapply(seq_along(df.vis$lower),function(x){

    symbol <- newXMLNode("symbol", attrs = c(alpha=alpha, clip_to_extent="1", type="fill", name=as.character(x-1)))

    layer <- newXMLNode("layer", attrs =c(pass="0",class="SimpleFill",locked="0"))

    prop<- list()
    prop[[1]] <- newXMLNode("prop", attrs =c(k="border_width_map_unit_scale", v="0,0"))
    prop[[2]] <- newXMLNode("prop", attrs =c(k="color",v= df.vis$rgb[x]))
    prop[[3]] <- newXMLNode("prop", attrs =c(k="joinstyle", v="bevel"))
    prop[[4]] <- newXMLNode("prop", attrs =c(k="offset", v="0,0"))
    prop[[5]] <- newXMLNode("prop", attrs =c(k="offset_map_unit_scale", v="0,0"))
    prop[[6]] <- newXMLNode("prop", attrs =c(k="offset_unit", v="MM"))
    prop[[7]] <- newXMLNode("prop", attrs =c(k="outline_color", v="0,0,0,255"))
    prop[[8]] <- newXMLNode("prop", attrs =c(k="outline_style", v="solid"))
    prop[[9]] <- newXMLNode("prop", attrs =c(k="outline_width", v="0.05"))
    prop[[10]] <- newXMLNode("prop", attrs =c(k="outline_width_unit", v="MM"))
    prop[[11]] <- newXMLNode("prop", attrs =c(k="style", v="solid"))

    effect_main <- newXMLNode("effect", attrs =c(enabled="0", type="effectStack"))

    # "effect"-nodes
    effect_1<- newXMLNode("effect", attrs =c(type="dropShadow"))
    prop.effect_1<- list()
    prop.effect_1[[1]]<- newXMLNode("prop", attrs =c(k="blend_mode", v="13"))
    prop.effect_1[[2]]<- newXMLNode("prop", attrs =c(k="blur_level", v="10"))
    prop.effect_1[[3]]<- newXMLNode("prop", attrs =c(k="color", v="0,0,0,255"))
    prop.effect_1[[4]]<- newXMLNode("prop", attrs =c(k="draw_mode", v="2"))
    prop.effect_1[[5]]<- newXMLNode("prop", attrs =c(k="enabled", v="0"))
    prop.effect_1[[6]]<- newXMLNode("prop", attrs =c(k="offset_angle", v="135"))
    prop.effect_1[[7]]<- newXMLNode("prop", attrs =c(k="offset_distance", v="2"))
    prop.effect_1[[8]]<- newXMLNode("prop", attrs =c(k="offset_unit", v="MM"))
    prop.effect_1[[9]]<- newXMLNode("prop", attrs =c(k="offset_unit_scale", v="0,0"))
    prop.effect_1[[10]]<- newXMLNode("prop", attrs =c(k="transparency", v="0"))

    effect_2<- newXMLNode("effect", attrs =c(type="outerGlow"))
    prop.effect_2<- list()
    prop.effect_2[[1]]<- newXMLNode("prop", attrs =c(k="blend_mode", v="0"))
    prop.effect_2[[2]]<- newXMLNode("prop", attrs =c(k="blur_level", v="3"))
    prop.effect_2[[3]]<- newXMLNode("prop", attrs =c(k="color1", v="0,0,255,255"))
    prop.effect_2[[4]]<- newXMLNode("prop", attrs =c(k="color2", v="0,255,0,255"))
    prop.effect_2[[5]]<- newXMLNode("prop", attrs =c(k="color_type", v="0"))
    prop.effect_2[[6]]<- newXMLNode("prop", attrs =c(k="discrete", v="0"))
    prop.effect_2[[7]]<- newXMLNode("prop", attrs =c(k="draw_mode", v="2"))
    prop.effect_2[[8]]<- newXMLNode("prop", attrs =c(k="enabled", v="0"))
    prop.effect_2[[9]]<- newXMLNode("prop", attrs =c(k="single_color", v="255,255,255,255"))
    prop.effect_2[[10]]<- newXMLNode("prop", attrs =c(k="spread", v="2"))
    prop.effect_2[[11]]<- newXMLNode("prop", attrs =c(k="spread_unit", v="MM"))
    prop.effect_2[[12]]<- newXMLNode("prop", attrs =c(k="spread_unit_scale", v="0,0"))
    prop.effect_2[[13]]<- newXMLNode("prop", attrs =c(k="transparency", v="0.5"))


    effect_3<- newXMLNode("effect", attrs =c(type="drawSource"))
    prop.effect_3<- list()
    prop.effect_3[[1]]<- newXMLNode("prop", attrs =c(k="blend_mode", v="0"))
    prop.effect_3[[2]]<- newXMLNode("prop", attrs =c(k="draw_mode", v="2"))
    prop.effect_3[[3]]<- newXMLNode("prop", attrs =c(k="enabled", v="1"))
    prop.effect_3[[4]]<- newXMLNode("prop", attrs =c(k="transparency", v="0"))

    effect_4<- newXMLNode("effect", attrs =c(type="innerShadow"))
    prop.effect_4<- list()
    prop.effect_4[[1]]<- newXMLNode("prop", attrs =c(k="blend_mode", v="13"))
    prop.effect_4[[2]]<- newXMLNode("prop", attrs =c(k="blur_level", v="10"))
    prop.effect_4[[3]]<- newXMLNode("prop", attrs =c(k="color", v="0,0,0,255"))
    prop.effect_4[[4]]<- newXMLNode("prop", attrs =c(k="draw_mode", v="2"))
    prop.effect_4[[5]]<- newXMLNode("prop", attrs =c(k="enabled", v="0"))
    prop.effect_4[[6]]<- newXMLNode("prop", attrs =c(k="offset_angle", v="135"))
    prop.effect_4[[7]]<- newXMLNode("prop", attrs =c(k="offset_distance", v="2"))
    prop.effect_4[[8]]<- newXMLNode("prop", attrs =c(k="offset_unit", v="MM"))
    prop.effect_4[[9]]<- newXMLNode("prop", attrs =c(k="offset_unit_scale", v="0,0"))
    prop.effect_4[[10]]<- newXMLNode("prop", attrs =c(k="transparency", v="0"))

    effect_5<- newXMLNode("effect", attrs =c(type="innerGlow"))
    prop.effect_5<- list()
    prop.effect_5[[1]]<- newXMLNode("prop", attrs = c(k="blend_mode", v="0"))
    prop.effect_5[[2]]<- newXMLNode("prop", attrs = c(k="blur_level", v="3"))
    prop.effect_5[[3]]<- newXMLNode("prop", attrs = c(k="color1", v="0,0,255,255"))
    prop.effect_5[[4]]<- newXMLNode("prop", attrs = c(k="color2", v="0,255,0,255"))
    prop.effect_5[[5]]<- newXMLNode("prop", attrs = c(k="color_type", v="0"))
    prop.effect_5[[6]]<- newXMLNode("prop", attrs = c(k="discrete", v="0"))
    prop.effect_5[[7]]<- newXMLNode("prop", attrs = c(k="draw_mode", v="2"))
    prop.effect_5[[8]]<- newXMLNode("prop", attrs = c(k="enabled", v="0"))
    prop.effect_5[[9]]<- newXMLNode("prop", attrs = c(k="single_color", v="255,255,255,255"))
    prop.effect_5[[10]]<- newXMLNode("prop", attrs =c(k="spread", v="2"))
    prop.effect_5[[11]]<- newXMLNode("prop", attrs =c(k="spread_unit", v="MM"))
    prop.effect_5[[12]]<- newXMLNode("prop", attrs =c(k="spread_unit_scale", v="0,0"))
    prop.effect_5[[13]]<- newXMLNode("prop", attrs =c(k="transparency", v="0.5"))


    addChildren(effect_1, prop.effect_1)
    addChildren(effect_2, prop.effect_2)
    addChildren(effect_3, prop.effect_3)
    addChildren(effect_4, prop.effect_4)
    addChildren(effect_5, prop.effect_5)

    addChildren(effect_main, list(effect_1, effect_2, effect_3, effect_4, effect_5))

    addChildren(layer, prop)

    addChildren(layer, effect_main)

    addChildren(symbol, layer)
  })


  # ---------
  # 7: Add Pieces together:

  addChildren(symbols, symbol.block)

  addChildren(rend, list(ranges, symbols))

  addChildren(base, rend)


  # ---------
  # 8: save to qml-file
  writeLines(saveXML(base), out.file)
  cat("qml-file created")
  }

