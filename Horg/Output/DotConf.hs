module Horg.Output.DotConf where


data Conf = Conf {
        global :: GlobalConf,
        tag :: NodeConf,
        content :: NodeConf,
        header :: NodeConf }


data GlobalConf = GlobalConf {
        layout :: Layout,
        rankdir :: Rankdir }

instance Show GlobalConf where
    show gc = show (layout gc) ++ "\n"
           ++ show (rankdir gc) ++ "\n"

data Layout = Dot | Neato | Fdp | Circo
instance Show Layout where
    show l = "layout=" ++ case l of
                              Dot -> "dot"
                              Neato -> "neato"
                              Fdp -> "fdp"
                              Circo -> "circo"

data Rankdir = TB | LR
instance Show Rankdir where
    show r = "rankdir=" ++ case r of
                               TB -> "TB"
                               LR -> "LR"


data NodeConf = NodeConf {
        shape :: Shape,
        fontsize :: Int,
        style :: Style,
        fillcolor :: String,
        fontcolor :: String }
instance Show NodeConf where
    show nc = "[" ++ show (shape nc) ++ ","
                  ++ "fontsize=" ++ show (fontsize nc) ++ ","
                  ++ show (style nc) ++ ","
                  ++ "fillcolor=\"" ++ fillcolor nc ++ "\","
                  ++ "fontcolor=\"" ++ fontcolor nc ++ "\"]"


data Shape = Box | Note | Ellipse
instance Show Shape where
    show s = "shape=" ++ case s of
                             Box -> "box"
                             Note -> "note"
                             Ellipse -> "ellipse"
data Style = Filled
instance Show Style where
    show s = "style=" ++ case s of
                             Filled -> "filled"


conf :: Conf
conf = Conf globalConf nodeConf nodeConf nodeConf


defaultConf :: Conf
defaultConf = conf { global = defaultGlobalConf,
                     tag = defaultTagConf,
                     header = defaultHeaderConf,
                     content = defaultContentConf }


globalConf :: GlobalConf
globalConf = GlobalConf Neato LR


defaultGlobalConf :: GlobalConf
defaultGlobalConf = GlobalConf Fdp LR


nodeConf :: NodeConf
nodeConf = NodeConf Box 10 Filled "#000000" "#ffffff"


defaultTagConf :: NodeConf
defaultTagConf = nodeConf { shape = Ellipse,
                            fontsize = 20,
                            style = Filled,
                            fillcolor = "#333333",
                            fontcolor = "#ccccff" }


defaultHeaderConf :: NodeConf
defaultHeaderConf = nodeConf { shape = Box,
                               fontsize = 17,
                               style = Filled,
                               fillcolor = "#ffffff",
                               fontcolor = "#000000" }


defaultContentConf :: NodeConf
defaultContentConf = nodeConf { shape = Note,
                                fontsize = 12,
                                style = Filled,
                                fillcolor = "#ffffaa",
                                fontcolor = "#0000bb" }
