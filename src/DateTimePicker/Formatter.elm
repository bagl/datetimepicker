module DateTimePicker.Formatter exposing
    ( dateFormatter
    , datePattern
    , dateTimeFormatter
    , dateTimePattern
    , footerFormatter
    , footerPattern
    , timeFormatter
    , timePattern
    , titleFormatter
    , titlePattern
    )

import DateTime exposing (DateTime)


titleFormatter : DateTime -> String
titleFormatter =
    DateTime.format titlePattern


titlePattern : String
titlePattern =
    "MMMM yyyy"


dateFormatter : DateTime -> String
dateFormatter =
    DateTime.format datePattern


datePattern : String
datePattern =
    "yyyy-MM-dd"


footerFormatter : DateTime -> String
footerFormatter =
    DateTime.format footerPattern


footerPattern : String
footerPattern =
    "E yyyy-MM-dd"


dateTimeFormatter : DateTime -> String
dateTimeFormatter =
    DateTime.format dateTimePattern


dateTimePattern : String
dateTimePattern =
    "yyyy-MM-dd HH:mm:ss"


timeFormatter : DateTime -> String
timeFormatter =
    DateTime.format timePattern


timePattern : String
timePattern =
    "HH:mm:ss"
