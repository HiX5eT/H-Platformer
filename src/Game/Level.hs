-- | "Gökyüzü Adaları" bölümü: platform yerleşimi, kristaller, kontrol noktaları.
module Game.Level
  ( levelSolids
  , levelCrystals
  , totalCrystals
  , Checkpoint (..)
  , levelCheckpoints
  , levelPortalPos
  , spawnPoint
  , levelTrees
  , levelRocks
  , levelDecoIslands
  , levelSigns
  ) where

import Game.Math
import Game.Types

--------------------------------------------------------------------------------
-- Yardımcılar
--------------------------------------------------------------------------------

-- | Üst yüzeyi @topY@ olan, 1 birim kalınlığında kutu platform.
p :: Float -> Float -> Float -> Float -> Float -> SolidKind -> (V3, V3, SolidKind, Mover)
p x topY z sx sz kind = (V3 x (topY - 0.5) z, V3 sx 1 sz, kind, Still)

-- | Kalınlığı ve hareketi özelleştirilebilen platform.
pm :: Float -> Float -> Float -> Float -> Float -> Float -> SolidKind -> Mover
   -> (V3, V3, SolidKind, Mover)
pm x topY z sx sz th kind mv = (V3 x (topY - th / 2) z, V3 sx th sz, kind, mv)

--------------------------------------------------------------------------------
-- Bölüm geometrisi
--------------------------------------------------------------------------------

-- | Tüm katı platformlar (id'ler liste sırasına göre atanır).
levelSolids :: [Solid]
levelSolids = zipWith mk [0 ..] specs
  where
    mk i (pos, size, kind, mv) = Solid i pos size kind mv

    specs :: [(V3, V3, SolidKind, Mover)]
    specs =
      [ -- 1) Başlangıç adası
        p 0 0 0 13 13 Grass

        -- 2) İlk basamaklar
      , p 0 1.0 (-13) 5 5 Grass
      , p 2.5 2.0 (-20) 4.5 4.5 Stone

        -- 3) Yatay hareket eden platform
      , pm 0 2.0 (-27.5) 4.5 4.5 0.6 Metal (Osc (V3 5 0 0) 0.55 0)

        -- 4) Çim ada + trambolin adası
      , p 7 2.8 (-36) 6 6 Grass
      , p 7 2.8 (-44) 5 5 Grass
      , pm 7 2.95 (-44) 4.6 4.6 0.3 Bounce Still

        -- 5) Yüksek ada ve dar kiriş
      , p 7 9.0 (-52) 7 7 Grass
      , p 2 9.0 (-58) 10 1.2 Wood

        -- 6) Kontrol noktası adası
      , p (-5) 9.0 (-64) 5 5 Grass
      , pm (-5) 9.05 (-64.5) 4.4 1.4 0.12 Hazard Still

        -- 7) Dikey hareket eden platform
      , pm (-5) 12.5 (-71) 4 4 0.6 Metal (Osc (V3 0 3 0) 0.7 0)

        -- 8) Yüksek ada + tehlike
      , p (-5) 15.0 (-78) 6 6 Grass
      , pm (-5) 15.06 (-78.5) 5.4 1.3 0.12 Hazard Still

        -- 9) Dar köprü
      , p (-1) 15.0 (-86) 2 10 Stone

        -- 10) Basamaklar
      , p 3 15.0 (-95) 4.5 4.5 Grass
      , pm 3 15.06 (-95.5) 4.0 1.4 0.12 Hazard Still
      , p 8 14.2 (-101) 4 4 Stone

        -- 11) Son kontrol noktası adası + trambolin
      , p 14 13.5 (-107) 7 7 Grass
      , pm 14 13.65 (-109) 3.2 3.2 0.3 Bounce Still

        -- 12) Final adası
      , p 14 20.0 (-116) 9 9 Grass

        -- 13) Dekoratif yan adalar (üzerine çıkılabilir, yol dışı)
      , p (-13) 4.5 (-24) 5 5 Grass
      , p 18 7.0 (-56) 5 5 Grass
      , p (-14) 12.0 (-84) 5 5 Grass
      ]

--------------------------------------------------------------------------------
-- Kristaller
--------------------------------------------------------------------------------

levelCrystals :: [V3]
levelCrystals =
  [ V3 0 2.3 (-13)
  , V3 2.5 3.3 (-20)
  , V3 0 3.3 (-27.5)
  , V3 7 4.1 (-36)
  , V3 7 10.4 (-52)
  , V3 2 10.3 (-58)
  , V3 (-5) 10.3 (-64)
  , V3 (-5) 16.3 (-78)
  , V3 3 16.3 (-95)
  , V3 14 21.4 (-116)
  ]

totalCrystals :: Int
totalCrystals = length levelCrystals

--------------------------------------------------------------------------------
-- Kontrol noktaları
--------------------------------------------------------------------------------

data Checkpoint = Checkpoint
  { cpPos :: !V3      -- bayrak direğinin tabanı
  , cpSpawn :: !V3    -- yeniden doğma konumu (oyuncu merkezi)
  , cpYaw :: !Float   -- bayrağın baktığı yön
  } deriving (Show)

levelCheckpoints :: [Checkpoint]
levelCheckpoints =
  [ Checkpoint (V3 (-5) 9.0 (-62.2)) (V3 (-5) 9.95 (-63.0)) 0.4
  , Checkpoint (V3 14 13.5 (-104.4)) (V3 14 14.45 (-105.2)) (-0.6)
  ]

--------------------------------------------------------------------------------
-- Bitiş portalı
--------------------------------------------------------------------------------

levelPortalPos :: V3
levelPortalPos = V3 14 21.5 (-118)

--------------------------------------------------------------------------------
-- Başlangıç
--------------------------------------------------------------------------------

spawnPoint :: V3
spawnPoint = V3 0 0.95 4

--------------------------------------------------------------------------------
-- Dekorasyon
--------------------------------------------------------------------------------

-- | (taban konumu, ölçek)
levelTrees :: [(V3, Float)]
levelTrees =
  [ (V3 (-5.5) 0 (-5), 1.15)
  , (V3 5.5 0 (-4.5), 1.0)
  , (V3 (-6) 0 5, 0.9)
  , (V3 5.8 0 5.5, 1.25)
  , (V3 4.6 2.8 (-34.2), 0.85)
  , (V3 9.4 2.8 (-37.8), 1.05)
  , (V3 (-6.6) 9.0 (-62.4), 0.95)
  , (V3 (-3.4) 9.0 (-66), 0.8)
  , (V3 (-6.8) 15.0 (-80.4), 1.1)
  , (V3 16.4 13.5 (-104.6), 1.15)
  , (V3 11.8 13.5 (-109.2), 0.9)
  , (V3 17 20.0 (-113), 1.3)
  , (V3 11 20.0 (-118.5), 1.0)
  , (V3 (-13) 4.5 (-22), 1.2)
  , (V3 18 7.0 (-54), 1.1)
  ]

-- | Havada süzülen kayalar: (konum, ölçek)
levelRocks :: [(V3, Float)]
levelRocks =
  [ (V3 (-11) 2.0 (-14), 0.9)
  , (V3 12 4.5 (-22), 1.3)
  , (V3 (-9) 7.0 (-40), 1.0)
  , (V3 15 12.0 (-44), 1.6)
  , (V3 (-12) 14.0 (-58), 1.2)
  , (V3 10 18.0 (-70), 1.4)
  , (V3 (-16) 19.0 (-92), 1.8)
  , (V3 20 16.0 (-94), 1.3)
  , (V3 4 24.0 (-110), 1.5)
  , (V3 24 24.0 (-120), 2.0)
  , (V3 (-22) 22.0 (-110), 2.2)
  , (V3 0 28.0 (-140), 2.6)
  ]

-- | Uzakta süzülen dekoratif adalar: (konum, ölçek)
levelDecoIslands :: [(V3, Float)]
levelDecoIslands =
  [ (V3 (-34) 8 (-40), 2.4)
  , (V3 38 14 (-70), 3.0)
  , (V3 (-40) 22 (-110), 3.4)
  , (V3 30 4 (-10), 2.0)
  , (V3 (-26) (-2) (-150), 2.6)
  , (V3 46 26 (-150), 3.8)
  ]

-- | Yol gösteren tabelalar: (konum, yazı yönü)
levelSigns :: [(V3, Float)]
levelSigns =
  [ (V3 2.6 0 1.0, 0.2)
  , (V3 4.2 2.8 (-33.6), 0.5)
  , (V3 (-2.6) 15.0 (-82.0), 0.0)
  ]
