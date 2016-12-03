module Parser exposing (..)

output = testList
  -- = List.map List.unzip testList
  -- |> List.map Tuple.second
  -- |> List.map4 (,)


testList = [ [(1,"yellow"),(2,"19"),(6,"cat"),(11,"sex"),(14,"tv"),(15,"7")]
           , [(1,"red"),(2,"13"),(6,"dog"),(11,"drugs"),(14,"movie"),(15,"7")]
           , [(1,"blue"),(2,"12"),(6,"rat"),(11,"rock"),(14,"book"),(15,"7")]
           , [(1,"orange"),(2,"1"),(6,"bear"),(11,"roll"),(14,"comic"),(15,"7")]
           ]
