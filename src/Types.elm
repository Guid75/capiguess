module Types exposing (Country, emptyCountry, Countries)



type alias Country =
    { name : String
    , capital : String
    }


emptyCountry =
    { name = ""
    , capital = ""
    }


type alias Countries =
    List Country
