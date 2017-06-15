module Types exposing (Country, emptyCountry, Countries)



type alias Country =
    { name : String
    , capital : String
    , fr : String
    }


emptyCountry =
    { name = ""
    , capital = ""
    , fr = ""
    }


type alias Countries =
    List Country
