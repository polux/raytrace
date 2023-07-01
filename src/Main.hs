{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Main where

import Codec.Picture
import Control.Lens hiding (transform)
import Control.Monad
import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import Data.Maybe
import Data.Ord
import GHC.Conc (numCapabilities)
import Linear.Epsilon
import Linear.Matrix
import Linear.Metric
import Linear.Quaternion hiding (rotate)
import Linear.V3
import Linear.V4
import Linear.Vector
import Safe.Foldable
import Text.Printf

type S = Double

type V = V3 S

type M = M44 S

data Ray = Ray
  { origin :: !V
  , direction :: !V
  }
  deriving (Show)

data Scene = Scene
  { backgroundColor :: S
  , lights :: [Light]
  , object :: Object
  }

data Object
  = Sphere
      { center :: !V
      , radius :: !S
      }
  | Plane
      { center :: !V
      , normal :: !V
      }
  | Disk
      { center :: !V
      , normal :: !V
      , radius :: !S
      }
  | Composite [Object]
  deriving (Show)

sphere :: V -> S -> Object
sphere = Sphere

plane :: V -> V -> Object
plane center normal = Plane center (normalize normal)

disk :: V -> V -> S -> Object
disk center normal radius = Disk center (normalize normal) radius

composite :: [Object] -> Object
composite = Composite

transformPoint :: M -> V -> V
transformPoint m p = normalizePoint (m !* point p)

transformVec :: M -> V -> V
transformVec m v = (m !* vector v) ^. _xyz

transformNormal :: M -> V -> V
transformNormal m v = (transpose (inv44 m) !* vector v) ^. _xyz

transform :: M -> Object -> Object
transform m (Sphere center radius) = Sphere (transformPoint m center) radius
transform m (Plane center normal) =
  Plane (transformPoint m center) (transformNormal m normal)
transform m (Disk center normal radius) =
  Disk (transformPoint m center) (transformNormal m normal) radius
transform m (Composite scenes) = Composite (map (transform m) scenes)

rotate :: V -> S -> M
rotate axis angle = mkTransformation (axisAngle axis angle) (V3 0 0 0)

translate :: V -> M
translate translation = mkTransformationMat identity translation

data Intersection = Intersection
  { location :: !V
  , normal :: !V
  }
  deriving (Show)

data Light
  = Sun
      { direction :: !V
      , intensity :: !S
      }
  | LightPoint
      { location :: !V
      , intensity :: !S
      }
  deriving (Show)

sun :: V -> S -> Light
sun direction intensity = Sun (normalize direction) intensity

lightPoint :: V -> S -> Light
lightPoint location intensity = LightPoint location intensity

solveQuadratic :: S -> S -> S -> Maybe (S, S)
solveQuadratic a b c
  | delta < 0 = Nothing
  | delta == 0 = Just (-b / (2 * a), -b / (2 * a))
  | otherwise = Just ((-b + sqrt delta) / (2 * a), (-b - sqrt delta) / (2 * a))
 where
  delta = b * b - 4 * a * c

intersect :: Ray -> Object -> Maybe Intersection
intersect (Ray origin direction) scene = intersect' scene
 where
  intersect' (Sphere center radius) =
    fmap mkIntersection (solveQuadratic a b c >>= pickTime)
   where
    l = origin - center
    a = quadrance direction
    b = 2 * (direction `dot` l)
    c = quadrance l - (radius * radius)
    pickTime (t0, t1) =
      if smallest < 0
        then Nothing
        else Just smallest
     where
      smallest = min t0 t1
    mkIntersection t =
      Intersection location (normalize (location ^-^ center))
     where
      location = origin ^+^ direction ^* t
  intersect' (Plane center normal)
    | denominator > 0 || nearZero denominator = Nothing
    | otherwise = Just (Intersection location normal)
   where
    numerator = (center ^-^ origin) `dot` normal
    denominator = direction `dot` normal
    location = origin ^+^ direction ^* (numerator / denominator)
  intersect' (Disk center normal radius) =
    intersect' (Plane center normal) >>= restrict
   where
    restrict inter@(Intersection point _) =
      if quadrance (point ^-^ center) < radius * radius
        then Just inter
        else Nothing
  intersect' (Composite scenes)
    | null scenes = Nothing
    | otherwise = minimumByMay closest (mapMaybe intersect' scenes)
   where
    closest = comparing (distance origin . (.location))

intersects :: Ray -> Object -> Bool
intersects ray scene = isJust (ray `intersect` scene)

screenSpaceToWorldSpace ::
  Int -> -- width
  Int -> -- height
  Int -> -- i
  Int -> -- j
  (S, S) -- (x,y)
screenSpaceToWorldSpace width height i j = (x, y)
 where
  aspectRatio = fromIntegral width / fromIntegral height
  x = (2 * (fromIntegral i + 0.5) / fromIntegral width - 1) * aspectRatio
  y = 1 - 2 * (fromIntegral j + 0.5) / fromIntegral height

render ::
  Int -> -- width
  Int -> -- height
  Int -> -- reflections
  M -> -- camera
  Scene ->
  Image Pixel8
render width height reflections camera (Scene backgroundColor lights object) =
  generateImage renderPixel width height
 where
  origin = transformPoint camera (V3 0 0 0)
  renderPixel i j = toPixel8 $ renderRay reflections (Ray origin direction)
   where
    toPixel8 d = round (d * 255)
    direction = normalize (transformVec camera (V3 x y (-1)))
    (x, y) = screenSpaceToWorldSpace width height i j
  renderRay n ray@(Ray _ direction) =
    case ray `intersect` object of
      Nothing -> backgroundColor
      Just (Intersection location normal) ->
        if n == 0
          then properShading
          else (properShading + 2 * renderRay (n - 1) mirroredRay) / 3
       where
        properShading = shade lights object location normal
        mirroredRay = Ray location mirrorDirection
        mirrorDirection =
          direction ^-^ 2 * (direction `dot` normal) *^ normal

shade ::
  [Light] -> -- lights
  Object -> -- scene
  V -> -- location
  V -> -- normal
  Double
shade lights object location normal = realToFrac $ sum (map shade' lights)
 where
  shade' (Sun direction intensity) = shade'' (-direction) intensity
  shade' (LightPoint lightLocation intensity) =
    shade'' (normalize locToLight) intensity'
   where
    locToLight = lightLocation - location
    intensity' = intensity / (4 * pi * quadrance location)
  shade'' direction intensity =
    if intersects (Ray location direction) object
      then 0
      else intensity * max 0 (direction `dot` normal)

cameraToWorld :: M
cameraToWorld = translate (V3 0 2 3)

scene i n = Scene backgroundColor lights object
 where
  backgroundColor = 0.1
  lights = [sun (V3 1 (-1) (-1)) 1, lightPoint (V3 (-10) 2 0) 20]
  object = transform (transformation i n) (composite [spheres, disk1])
   where
    spheres =
      composite
        [ sphere (V3 x y z) 0.4
        | x <- [-1, 0, 1]
        , y <- [1, 2, 3]
        , z <- [-1, 0, 1]
        ]
    disk1 = disk (V3 0 0 0) (V3 0 1 0) 4
    transformation i n =
      rotate (V3 0 1 0) (fromIntegral i * pi / (2 * fromIntegral n))

image i n = render 640 480 10 cameraToWorld (scene i n)

filename :: Int -> String
filename i = printf "/tmp/out_%04d.bmp" i

numImages = 50

animation :: [Image Pixel8]
animation =
  [image i numImages | i <- [0 .. numImages - 1]]
    `using` parBuffer numCapabilities rdeepseq

main :: IO ()
main = do
  printf "rendering using %d cores\n" numCapabilities
  zipWithM_ toDisk [0 ..] animation
 where
  toDisk i img = do
    writeBitmap (filename i) img
    print i
