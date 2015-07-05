module Stars
  ( Stars, state0, update, view
  ) where

import Color exposing (white)
import Common exposing (Positioned)
import Debug
import Graphics.Collage as Collage exposing (Form, circle, group, filled)
import List exposing (filter, length, map)
import Random exposing (Seed)


-- MODEL

type alias Stars =
  { list : List Star
  , timeSinceLast : Float
  }


type alias Star = Positioned {}


state0 : Stars
state0 =
  { list = []
  , timeSinceLast = 0
  }


-- UPDATE

update : (Float, Seed) -> Stars -> Stars
update (dt, seed) stars =
  let
    (shouldCreateStar', seed') = shouldCreateStar seed stars
  in
    Debug.watch "stars"
    { stars | list <- stars.list
        |> (if shouldCreateStar' then addStars seed' else identity)
        |> moveRight dt
        |> filterTooFarRight,
      timeSinceLast <- if shouldCreateStar' then 0 else stars.timeSinceLast + dt
    }


shouldCreateStar : Seed -> Stars -> (Bool, Seed)
shouldCreateStar seed stars =
  let
    thresh = stars.timeSinceLast * toFloat (20 - length stars.list)

    (rand, seed') = Random.generate (Random.float 0 thresh) seed
  in
    (rand > 1000, seed')


addStars : Seed -> List Star -> List Star
addStars seed stars =
  let
    newStar =
      { x = 0
      , y = fst (Random.generate (Random.float 0 100) seed)
      }
  in
    newStar :: stars


moveRight : Float -> List Star -> List Star
moveRight dt stars =
  map (\star -> { star | x <- star.x + dt/10 }) stars


filterTooFarRight : List Star -> List Star
filterTooFarRight stars =
  filter (\star -> star.x <= 100) stars


-- VIEW

view : (Float, Float) -> Stars -> Form
view dims stars =
  let
    outputStar star = circle 5
        |> filled white
        |> Common.move dims star
  in
    group (map outputStar stars.list)
