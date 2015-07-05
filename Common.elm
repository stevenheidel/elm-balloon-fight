module Common
  ( Keys, Positioned
  , move
  ) where

import Graphics.Collage as Collage exposing (Form)


type alias Keys =
  { up : Bool
  , left : Bool
  , right : Bool
  , space : Bool
  }


type alias Positioned a =
  { a |
      x : Float,
      y : Float
  }


move : (Float, Float) -> Positioned a -> Form -> Form
move (width, height) {x, y} =
  let
    normalize total dim = (dim - 50) * total / 100
  in
    Collage.move (normalize width x, normalize height y)
