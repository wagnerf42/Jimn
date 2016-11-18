import Point

colors = ["red", "green", "blue"]
tycat :: [(Box, ViewPort -> String)] -> IO ()
tycat objects = do
  filename <- saveTemporary svgString
  display filename where
    boxes = map fst objects
    globalBox = fold1l' fuseBoxes boxes
    viewport = view [800, 600] globalBox
    svgString = "<svg width=\"800\" height=\"600\">\n"++svgStrings++"</svg>"

