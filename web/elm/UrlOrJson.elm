module UrlOrJson exposing (..)

import Parser exposing (..)
import Regex exposing (..)

fun = run (keyword "http") "https://api.github.com/users/mralexgray/repos"

fun2 str =
  if (contains (regex "{") str) then
    2
  else
    1
