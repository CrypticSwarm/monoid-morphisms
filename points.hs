import Data.Monoid (Monoid(..))
import Control.Category

data Point = Point Float Float deriving Show

plus :: Point -> Point -> Point
plus (Point x y) (Point a b) = Point (x+a) (y+b)

newtype (Monoid m) => MonoidMorphism m = MonoidMorphism (m -> m)

toMorphism :: (Monoid a) => a -> MonoidMorphism a
toMorphism x = MonoidMorphism $ mappend x

applyMorphism :: (Monoid m) => MonoidMorphism m -> m -> m
applyMorphism (MonoidMorphism (m)) = m

instance (Monoid m) => Monoid (MonoidMorphism m) where
  mempty = toMorphism (mempty :: (Monoid m) => m)
  mappend a b = toMorphism $ applyMorphism a $ applyMorphism b (mempty :: (Monoid m) => m)

class Monoid m => MonoidCat m where
  morph :: m -> MonoidMorphism m
  apply :: MonoidMorphism m -> m -> m
  extract :: MonoidMorphism m -> m
  (<>) ::  MonoidMorphism m -> MonoidMorphism m -> MonoidMorphism m

  morph = toMorphism
  apply = applyMorphism
  extract a = apply a mempty
  (<>) = mappend

instance Monoid Point where
  mempty = Point 0 0
  mappend = plus

instance MonoidCat Point

