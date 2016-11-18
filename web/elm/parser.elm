import Html exposing (Html, text, div, p, pre)
import Html.Attributes exposing (style)
import String


quote = "\""
indentChars = "{"
outdentChars = "}"
newLineChars = ","
uniqueHead = "##FORMAT##"
incr = 20

model = "[{\"id\":91541985,\"time\":\"2016-10-29 01:48:04 UTC\",\"anon_visitor_id\":\"a86adf6b-910b-2b08-e291-c682\",\"ip_address\":\"76.20.48.125\",\"identity\":null,\"page\":\"https://trueme.goodhire.com/member/report-shared?candidateid=4402330f-4636-4323-a049-5a43643e69f9\",\"referrer\":null,\"user_agent\":\"Mozilla/5.0 (iPad; CPU OS 9_3_4 like Mac OS X) AppleWebKit/601.1.46 (KHTML, like Gecko) Mobile/13G35\",\"nudge_id\":167540,\"nudge_name\":\"Candidate Satisfaction\",\"answered_questions\":{\"321141\":{\"question_id\":321141,\"question_title\":\"How satisfied are you with your experience with GoodHire?\",\"question_type\":\"radio\",\"answer\":\"Very Satisfied\",\"selected_option_id\":919755}}}]"

viewModel : String -> Html msg
viewModel model =
  let
    lines =
      model
      |> formatString False 0
      |> String.split uniqueHead
  in
  pre [] <| List.map viewLine lines

viewLine : String -> Html msg
viewLine lineStr =
  let
    (indent, lineTxt) = splitLine lineStr
  in
    p [style
        [ ("paddingLeft", px (indent))
        , ("marginTop", "0px")
        , ("marginBottom", "0px")
        ]
      ]
      [ text lineTxt ]

px : Int -> String
px int =
  toString int
  ++ "px"

formatString : Bool -> Int -> String -> String
formatString isInQuotes indent str =
  case String.left 1 str of
    "" -> ""

    firstChar ->
      if isInQuotes then
        if firstChar == quote then
          firstChar
          ++ formatString (not isInQuotes) indent (String.dropLeft 1 str)
        else
          firstChar
          ++ formatString isInQuotes indent (String.dropLeft 1 str)
      else
        if String.contains firstChar newLineChars then
          uniqueHead ++ pad indent ++ firstChar
          ++ formatString isInQuotes indent (String.dropLeft 1 str)
        else if String.contains firstChar indentChars then
          uniqueHead ++ pad (indent + incr) ++ firstChar
          ++ formatString isInQuotes (indent + incr) (String.dropLeft 1 str)
        else if String.contains firstChar outdentChars then
          firstChar ++ uniqueHead ++ pad (indent - incr)
          ++ formatString isInQuotes (indent - incr) (String.dropLeft 1 str)
        else if firstChar == quote then
          firstChar
          ++ formatString (not isInQuotes) indent (String.dropLeft 1 str)
        else
          firstChar
          ++ formatString isInQuotes indent (String.dropLeft 1 str)

pad : Int -> String
pad indent =
  String.padLeft 5 '0' <| toString indent

splitLine : String -> (Int, String)
splitLine line =
  let
    indent =
      String.left 5 line
      |> String.toInt
      |> Result.withDefault 0
    newLine =
      String.dropLeft 5 line
  in
    (indent, newLine)

main =
  viewModel model
