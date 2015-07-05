module Fighter
  ( Fighter
  , state0, isOffScreen
  , update, view
  ) where

import Common exposing (Keys, Positioned)

import Color exposing (red)
import Debug
import Graphics.Collage as Collage exposing (Form, circle, filled)


-- MODEL

type alias Fighter = Positioned
  { vx : Float
  , vy : Float
  }


state0 : Fighter
state0 =
  { x = 80, y = 80 -- start user in top right
  , vx = 0, vy = 0
  }


isOffScreen : Fighter -> Bool
isOffScreen fighter =
  fighter.x < 0 || fighter.x > 100 || fighter.y < 0 || fighter.y > 100


-- UPDATE

update : (Float, Keys) -> Fighter -> Fighter
update (dt, keys) fighter =
  Debug.watch "fighter"
  fighter
      |> move keys
      |> friction dt
      |> gravity dt
      |> physics dt


move : Keys -> Fighter -> Fighter
move {up, left, right} fighter =
  { fighter |
      vx <-
        if  | left && not right -> fighter.vx - 0.1
            | right && not left -> fighter.vx + 0.1
            | otherwise -> fighter.vx,
      vy <-
        if up then fighter.vy + 0.2 else fighter.vy
  }


gravity : Float -> Fighter -> Fighter
gravity dt fighter =
  { fighter |
      vy <- fighter.vy - dt/30
  }


friction : Float -> Fighter -> Fighter
friction dt fighter =
  { fighter |
      vx <- fighter.vx * 0.97
  }


physics : Float -> Fighter -> Fighter
physics dt fighter =
  { fighter |
      x <- fighter.x + dt * fighter.vx,
      y <- fighter.y + dt * fighter.vy
  }


-- VIEW

view : (Float, Float) -> Fighter -> Form
view dims fighter =
  circle 15
      |> filled red
      |> Common.move dims fighter
