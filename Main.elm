module Main exposing (main)

import Browser
import Html exposing (Html)
import Svg exposing (svg)
import Svg.Attributes as SAttr
import Time exposing (Posix, Zone)
import Task exposing (perform)
import Random


type alias Model =
  { time : Posix
  , tileColor : Int
  , timeZone : Zone
  }


init : Model
init =
  { time = Time.millisToPosix 0
  , tileColor = 0
  , timeZone = Time.utc
  }


type Msg
  = InitTime (Zone,Posix)
  | GetTime Posix
  | GetColorSet Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    InitTime (zone, posix) ->
      let
        newModel =
          { model
          | time = posix
          , timeZone = zone
          }
      in
        ( newModel
        , case getColorSets False newModel |> Debug.log "" of
            hd :: tl ->
              Random.uniform hd tl
                |> Random.generate GetColorSet
            _ -> Cmd.none
        )

    GetTime posix ->
      let
        newModel =
          { model
          | time = posix
          }
      in
        ( newModel
        , case getColorSets True newModel of
            hd :: tl ->
              Random.uniform hd tl
                |> Random.generate GetColorSet
            _ -> Cmd.none
        )

    GetColorSet n ->
      ( { model | tileColor = n }
      , Cmd.none
      )

getColorSets b model =
  let
    current = model.tileColor

    h =
      Time.toHour model.timeZone model.time
        |> modBy 12

    min =
      Time.toMinute model.timeZone model.time

    m5 = min // 5

    getHour n m =
      if n == 0
      then 0
      else
        case modBy 4 n of
          1 -> (fib m) + getHour (n//4) (m+1)
          3 -> (fib m) + getHour (n//4) (m+1)
          _ -> getHour (n//4) (m+1)

    getMinute n m =
      if n == 0
      then 0
      else
        case modBy 4 n of
          2 -> (fib m) + getMinute (n//4) (m+1)
          3 -> (fib m) + getMinute (n//4) (m+1)
          _ -> getMinute (n//4) (m+1)

    helper n list =
      let
        debug =
          (getHour n 0,getMinute n 0)
      in
        if n == 1023
        then list
        else
          case (getHour n 0 == h, getMinute n 0 == m5) of
            (True, True) -> helper (n+1) (n::list)
            _ ->            helper (n+1) list

  in
    if b
    then
      case (getHour current 0 == h, getMinute current 0 == m5) of
        (True, True) -> [current]
        _ -> helper 0 []
    else
      helper 0 []

view : Model -> Html Msg
view model =
  svg
    [ SAttr.width  "800"
    , SAttr.height "500"
    , SAttr.viewBox "0 0 800 500"
    ]
    [ createRect 200 0 0 model.tileColor
    , createRect 200 100 1 model.tileColor
    , createRect 0 0 2 model.tileColor
    , createRect 0 200 3 model.tileColor
    , createRect 300 0 4 model.tileColor
    ]

i2s = String.fromInt

createRect x y n set =
  Svg.rect
    [ SAttr.x <| i2s x
    , SAttr.y <| i2s y
    , SAttr.width  <| i2s ((fib n)*100)
    , SAttr.height <| i2s ((fib n)*100)
    , SAttr.stroke "black"
    , SAttr.strokeWidth "5"
    , SAttr.fill <| getColor n set
    , SAttr.fillOpacity "0.6"
    ][]

fib n =
  case n of
    0 -> 1
    1 -> 1
    _ -> fib (n-1) + fib (n-2)

getColor n set =
  case modBy 4 (set//(4^n)) of
    1 -> "red"
    2 -> "green"
    3 -> "blue"
    _ -> "white"

main : Program () Model Msg
main =
  Browser.element
    { init =
        \_ ->
          ( init
          , Task.map2
              (\z t -> (z,t))
              Time.here
              Time.now
                |> perform InitTime
          )
    , view = view
    , update = update
    , subscriptions =
        \_ -> Time.every 1000 GetTime
    }