module Config exposing (..)


apiRoot : String
apiRoot =
    "https://quiver.bowst.com/api"


makeApiUrl : String -> String
makeApiUrl =
    (++) apiRoot
