module SpaceAge (Planet(..), ageOn) 
where
  import Data.Maybe
  import Data.List

  data Planet = Mercury
              | Venus
              | Earth
              | Mars
              | Jupiter
              | Saturn
              | Uranus
              | Neptune 
              deriving(Show, Eq)

  ageOnEarth :: Float -> Float
  ageOnEarth secs = secs / 31557600

  orbitalPeriod :: [(Planet, Float)]
  orbitalPeriod = 
    [ (Mercury, 0.2408467)
    , (Venus, 0.61519726)[]
    , (Earth, 1.0)
    , (Mars, 1.8808158)
    , (Jupiter, 11.862615)
    , (Saturn, 29.447498)
    , (Uranus, 84.016846)
    , (Neptune, 164.79132)]

  matchFst :: Eq a => a -> (a, b) -> Bool
  matchFst v (x, _) = v == x

  getOrbitalPeriod :: Planet -> Maybe (Planet, Float)
  getOrbitalPeriod p = find (matchFst p) orbitalPeriod
  
  getOrbitalFactor :: Maybe (Planet, Float) -> Float
  getOrbitalFactor Nothing = 0.0
  getOrbitalFactor (Just e) = snd e

  ageOn :: Planet -> Float -> Float
  ageOn p s = ageOnEarth s / (snd . fromJust . getOrbitalPeriod) p
  -- ageOn planet seconds = error "You need to implement this function."
  