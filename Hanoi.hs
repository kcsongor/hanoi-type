{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hanoi where

import GHC.TypeLits
import Data.Proxy
import Prelude hiding (Either(..))

data Towers (f :: Nat) (s :: Nat) (t :: Nat) = Towers

data Which = Left | Center | Right

-- Reversed list
data RList a = Nil | (RList a) :> a

type family Head (xs :: RList k) :: k where
  Head (xs ':> x) = x

type family Length (xs :: RList k) :: Nat where
  Length 'Nil = 0
  Length (xs ':> x) = 1 + Length xs

-- Solver
type family Solve tower where
  Solve (Towers f s t)
    = Move f 'Left 'Right 'Center ('Nil ':> Towers f s t)

type family Move n (from :: Which) (to :: Which) (tmp :: Which) ph where
  Move 1 from to tmp (ts ':> t)
    = ts ':> t ':> Dec (Inc t to) from
  Move n from to tmp ph
    = Move (n - 1) tmp to from
        (Move 1 from to tmp
          (Move (n - 1) from tmp to ph))

type family Inc tower (at :: Which) where
  Inc (Towers f s t) 'Left   = Towers (f + 1) s t
  Inc (Towers f s t) 'Center = Towers f (s + 1) t
  Inc (Towers f s t) 'Right  = Towers f s (t + 1)

type family Dec tower (at :: Which) where
  Dec (Towers f s t) 'Left   = Towers (f - 1) s t
  Dec (Towers f s t) 'Center = Towers f (s - 1) t
  Dec (Towers f s t) 'Right  = Towers f s (t - 1)

solve :: (t ~ Towers x 0 0, (h ':> solution) ~ Solve t) => t -> solution
solve _ = undefined

-- Advanced property based testing!
test :: Towers 4 0 0 -> Towers 0 0 4
test = solve

-- An example
steps :: (t ~ Towers 5 0 0, history ~ Solve t, n ~ (Length history - 1))
         => Proxy '(history, n)
steps = Proxy
