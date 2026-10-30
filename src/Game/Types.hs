{-# LANGUAGE BangPatterns #-}

-- | Oyunun ortak tipleri: dünya, oyuncu, kamera, katı cisimler.
module Game.Types
  ( -- * Renk yardımcıları
    rgbf, rgba, mixC, scaleC, brighten
    -- * Seviye geometrisi
  , SolidKind (..), Mover (..), Solid (..), solidPosAt, solidAABBAt
  , AABB (..), aabbOf, aabbMin, aabbMax, aabbOverlap, aabbCenter
  , isSolidKind, isTriggerKind
    -- * Oyuncu
  , Player (..), newPlayer, playerHalf, playerAABB
    -- * Kamera
  , Cam (..), newCam
    -- * Parçacık
  , Particle (..)
    -- * Tuşlar
  , Key (..)
    -- * Dünya
  , Screen (..), World (..)
  ) where

import Game.Math
import Graphics.Gloss.Data.Color (Color, makeColor, rgbaOfColor)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Renkler
--------------------------------------------------------------------------------

rgbf :: Float -> Float -> Float -> Color
rgbf r g b = makeColor r g b 1

rgba :: Float -> Float -> Float -> Float -> Color
rgba = makeColor

mixC :: Float -> Color -> Color -> Color
mixC t a b =
  let (ar, ag, ab, aa) = rgbaOfColor a
      (br, bg, bb, ba) = rgbaOfColor b
  in makeColor (lerpF t ar br) (lerpF t ag bg) (lerpF t ab bb) (lerpF t aa ba)

scaleC :: Float -> Color -> Color
scaleC k c =
  let (r, g, b, a) = rgbaOfColor c
  in makeColor (clamp 0 1 (r * k)) (clamp 0 1 (g * k)) (clamp 0 1 (b * k)) a

brighten :: Float -> Color -> Color
brighten d = mixC d (rgbf 1 1 1)

--------------------------------------------------------------------------------
-- Katı cisimler
--------------------------------------------------------------------------------

data SolidKind
  = Grass
  | Stone
  | Metal
  | Wood
  | Ice
  | Bounce
  | Hazard
  | Gold
  deriving (Eq, Show)

-- | Hareket eden platformlar: yön * genlik, açısal hız, faz.
data Mover
  = Still
  | Osc !V3 !Float !Float
  deriving (Eq, Show)

data Solid = Solid
  { solId :: !Int
  , solPos :: !V3
  , solSize :: !V3
  , solKind :: !SolidKind
  , solMover :: !Mover
  } deriving (Show)

-- | t anındaki merkez konumu.
solidPosAt :: Float -> Solid -> V3
solidPosAt t s = case solMover s of
  Still -> solPos s
  Osc amp w ph -> solPos s + vscale (sin (w * t + ph)) amp

isSolidKind :: SolidKind -> Bool
isSolidKind k = case k of
  Bounce -> False
  Hazard -> False
  _ -> True

isTriggerKind :: SolidKind -> Bool
isTriggerKind k = not (isSolidKind k)

--------------------------------------------------------------------------------
-- Eksen hizalı kutu
--------------------------------------------------------------------------------

data AABB = AABB !V3 !V3
  deriving (Show)

aabbOf :: V3 -> V3 -> AABB
aabbOf c s = AABB (c - vscale 0.5 s) (c + vscale 0.5 s)

aabbMin, aabbMax, aabbCenter :: AABB -> V3
aabbMin (AABB a _) = a
aabbMax (AABB _ b) = b
aabbCenter (AABB a b) = vscale 0.5 (a + b)

aabbOverlap :: AABB -> AABB -> Bool
aabbOverlap (AABB a1 b1) (AABB a2 b2) =
  vx a1 <= vx b2 && vx b1 >= vx a2 &&
  vy a1 <= vy b2 && vy b1 >= vy a2 &&
  vz a1 <= vz b2 && vz b1 >= vz a2

solidAABBAt :: Float -> Solid -> AABB
solidAABBAt t s = aabbOf (solidPosAt t s) (solSize s)

--------------------------------------------------------------------------------
-- Oyuncu
--------------------------------------------------------------------------------

-- | Konum = kapsülün merkezi (ayaklar pos.y - 0.9).
data Player = Player
  { plPos :: !V3
  , plVel :: !V3
  , plYaw :: !Float
  , plGrounded :: !Bool
  , plCoyote :: !Float
  , plJumpBuf :: !Float
  , plJumps :: !Int
  , plWalk :: !Float
  , plSquash :: !Float
  , plGround :: !(Maybe Int)
  , plBounceCool :: !Float
  , plAirTime :: !Float
  } deriving (Show)

playerHalf :: V3
playerHalf = V3 0.38 0.9 0.38

playerAABB :: V3 -> AABB
playerAABB p = aabbOf p (vscale 2 playerHalf)

newPlayer :: V3 -> Player
newPlayer p = Player
  { plPos = p
  , plVel = V3 0 0 0
  , plYaw = pi
  , plGrounded = True
  , plCoyote = 0
  , plJumpBuf = 0
  , plJumps = 1
  , plWalk = 0
  , plSquash = 0
  , plGround = Nothing
  , plBounceCool = 0
  , plAirTime = 0
  }

--------------------------------------------------------------------------------
-- Kamera
--------------------------------------------------------------------------------

data Cam = Cam
  { camYaw :: !Float
  , camPitch :: !Float
  , camDist :: !Float
  , camPos :: !V3
  , camLook :: !V3
  , camRoll :: !Float
  } deriving (Show)

newCam :: Float -> Cam
newCam yaw = Cam
  { camYaw = yaw
  , camPitch = 0.42
  , camDist = 9.5
  , camPos = V3 0 5 14
  , camLook = V3 0 1.6 0
  , camRoll = 0
  }

--------------------------------------------------------------------------------
-- Parçacıklar
--------------------------------------------------------------------------------

data Particle = Particle
  { parPos :: !V3
  , parVel :: !V3
  , parLife :: !Float
  , parMax :: !Float
  , parCol :: !Color
  , parSize :: !Float
  , parGrav :: !Float
  } deriving (Show)

--------------------------------------------------------------------------------
-- Tuşlar
--------------------------------------------------------------------------------

data Key
  = KW | KA | KS | KD
  | KSpace | KShift | KCtrl
  | KUp | KDown | KLeft | KRight
  | KQ | KE | KR | KP | KM | KF
  | KEsc | KEnter | KTab
  deriving (Eq, Ord, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- Ekranlar ve dünya durumu
--------------------------------------------------------------------------------

data Screen = Title | Playing | Paused | Won | Lost
  deriving (Eq, Show)

data World = World
  { wScreen :: !Screen
  , wTime :: !Float            -- toplam süre (animasyonlar)
  , wPlayTime :: !Float        -- bölüm süresi
  , wPlayer :: !Player
  , wCam :: !Cam
  , wKeys :: !(Set.Set Key)
  , wMouse :: !(Float, Float)
  , wDrag :: !Bool
  , wCrystals :: ![Bool]
  , wCheckpoints :: ![Bool]
  , wRespawn :: !V3
  , wRespawnYaw :: !Float
  , wLives :: !Int
  , wScore :: !Int
  , wMsg :: !String
  , wMsgT :: !Float
  , wParts :: ![Particle]
  , wShake :: !Float
  , wAcc :: !Float
  , wWidth :: !Int
  , wHeight :: !Int
  , wHintT :: !Float
  , wFlash :: !Float
  , wLandFx :: !Float
  , wDebug :: !Bool
  } deriving (Show)
