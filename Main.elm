import Common exposing (Keys)
import Fighter exposing (Fighter)
import Stars exposing (Stars)

import Color exposing (black)
import Graphics.Collage as Collage exposing (filled, collage, rect)
import Graphics.Element exposing (Element)
import Keyboard
import Random exposing (Seed)
import Time exposing (every, fps, second)


-- MODEL

type Status = Stopped | Playing


type alias State =
  { status : Status
  , fighter : Fighter
  , stars : Stars
  }


state0 : State
state0 =
  { status = Stopped
  , fighter = Fighter.state0
  , stars = Stars.state0
  }


-- UPDATE

update : (Float, Keys, Seed) -> State -> State
update (dt, keys, seed) state =
  state
      |> spaceToStart keys
      |> stopGame
      |> (\state' -> if state'.status == Playing then step (dt, keys, seed) state' else state')


spaceToStart : Keys -> State -> State
spaceToStart keys state =
  if state.status == Stopped && keys.space then
    { state |
        status <- Playing,
        fighter <- Fighter.state0,
        stars <- Stars.state0
    }
  else state


isGameOver : State -> Bool
isGameOver state =
  let
    hitStar fighter star =
      sqrt ((abs (fighter.x - star.x))^2 + (abs (fighter.y - star.y))^2) < 3
  in
    Fighter.isOffScreen state.fighter ||
      List.any (hitStar state.fighter) state.stars.list


stopGame : State -> State
stopGame state =
  { state | status <-
    if state.status == Playing && isGameOver state then Stopped
    else state.status
  }


step : (Float, Keys, Seed) -> State -> State
step (dt, keys, seed) state =
  { state |
      fighter <- Fighter.update (dt, keys) state.fighter,
      stars <- Stars.update (dt, seed) state.stars
  }


-- VIEW

view : State -> Element
view model =
  let
    (width, height) = (512, 480)

    background = rect width height |> filled black

    fighter = Fighter.view (width, height) model.fighter

    stars = Stars.view (width, height) model.stars
  in
    collage width height
      [ background
      , fighter
      , stars ]


-- MAIN

main : Signal Element
main =
  Signal.map view (Signal.foldp update state0 input)


input : Signal (Float, Keys, Seed)
input =
  let
    timestampAndDelta = Time.timestamp (Signal.map (\t -> t/20) (fps 30))
    timestamp = Signal.map fst timestampAndDelta
    delta = Signal.map snd timestampAndDelta

    -- convert keyboard presses to up, left, and right
    keys =
      Signal.map2 (\arrows -> \space -> {
        up = arrows.y > 0,
        left = arrows.x < 0,
        right = arrows.x > 0,
        space = space
      }) Keyboard.arrows Keyboard.space

    seed = Signal.map (round >> Random.initialSeed) timestamp
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta keys seed)
