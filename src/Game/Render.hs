{-# LANGUAGE BangPatterns #-}

-- | 3B sahneyi gloss çizim komutlarına çeviren yazılım render katmanı.
--   Boru hattı: dünya üçgenleri -> kamera uzayı -> yakın düzlem kırpma ->
--   perspektif projeksiyon -> derinliğe göre sıralama -> 2B poligonlar.
module Game.Render (renderWorld) where

import Game.Math
import Game.Types
import Game.Level
import Game.Physics (groundProbe)
import Graphics.Gloss
import Data.List (sortOn)
import Text.Printf (printf)

showF :: Float -> String
showF x = printf "%.2f" x

--------------------------------------------------------------------------------
-- Sabitler
--------------------------------------------------------------------------------

fovY :: Float
fovY = 0.95

sunDir :: V3
sunDir = vnorm (V3 0.55 0.85 0.35)

skyTop :: Color
skyTop = rgbf 0.16 0.42 0.82

skyHorizon :: Color
skyHorizon = rgbf 0.74 0.89 0.98

--------------------------------------------------------------------------------
-- Kamera uzayı
--------------------------------------------------------------------------------

data Basis = Basis
  { bsRight :: !V3
  , bsUp :: !V3
  , bsFwd :: !V3
  , bsPos :: !V3
  }

mkBasis :: V3 -> V3 -> Basis
mkBasis pos look =
  let f = vnorm (look - pos)
      r = vnorm (vcross f (V3 0 1 0))
      u = vcross r f
  in Basis r u f pos

toCam :: Basis -> V3 -> V3
toCam b p =
  let d = p - bsPos b
  in V3 (vdot d (bsRight b)) (vdot d (bsUp b)) (negate (vdot d (bsFwd b)))

projectPt :: Float -> Float -> V3 -> (Float, Float)
projectPt w h p =
  let f = 1 / tan (fovY * 0.5)
      iz = negate (1 / vz p)
      aspect = w / h
  in (vx p * f / aspect * iz * w / 2, vy p * f * iz * h / 2)

clipNear :: Float -> [V3] -> [V3]
clipNear n poly
  | length poly < 3 = []
  | otherwise = concatMap step (zip poly (tail poly ++ [head poly]))
  where
    inside p = vz p <= negate n
    step (a, b)
      | inside a && inside b = [b]
      | inside a = [inter a b]
      | inside b = [inter a b, b]
      | otherwise = []
    inter a b =
      let t = (negate n - vz a) / (vz b - vz a)
      in a + vscale t (b - a)

--------------------------------------------------------------------------------
-- Yüzler
--------------------------------------------------------------------------------

data Face = Face !V3 !V3 !V3 !Color

faceNormal :: Face -> V3
faceNormal (Face a b c _) = vnorm (vcross (b - a) (c - a))

-- | Güneşe göre düz gölgeleme.
lit :: V3 -> Color -> Color
lit n c = scaleC (0.6 + 0.6 * max 0 (vdot n sunDir)) c

triFace :: Color -> V3 -> V3 -> V3 -> [Face]
triFace c a b d =
  let n = vnorm (vcross (b - a) (d - a))
  in [Face a b d (lit n c)]

quadFace :: Color -> V3 -> V3 -> V3 -> V3 -> [Face]
quadFace c a b d e =
  let n = vnorm (vcross (b - a) (d - a))
      c' = lit n c
  in [Face a b d c', Face a d e c']

-- | Dönüşüm uygulanmış kutu.
boxM :: Color -> Color -> Color -> M4 -> V3 -> [Face]
boxM topC sideC botC m size =
  let V3 sx sy sz = size
      hx = sx / 2; hy = sy / 2; hz = sz / 2
      p i j k = mApply m (V3 (i * hx) (j * hy) (k * hz))
      top = quadFace topC (p (-1) 1 (-1)) (p (-1) 1 1) (p 1 1 1) (p 1 1 (-1))
      bot = quadFace botC (p (-1) (-1) 1) (p (-1) (-1) (-1)) (p 1 (-1) (-1)) (p 1 (-1) 1)
      px = quadFace sideC (p 1 (-1) (-1)) (p 1 1 (-1)) (p 1 1 1) (p 1 (-1) 1)
      nx = quadFace sideC (p (-1) (-1) 1) (p (-1) 1 1) (p (-1) 1 (-1)) (p (-1) (-1) (-1))
      pz = quadFace sideC (p (-1) (-1) 1) (p 1 (-1) 1) (p 1 1 1) (p (-1) 1 1)
      nz = quadFace sideC (p 1 (-1) (-1)) (p (-1) (-1) (-1)) (p (-1) 1 (-1)) (p 1 1 (-1))
  in top ++ bot ++ px ++ nx ++ pz ++ nz

boxAt :: Color -> Color -> Color -> V3 -> V3 -> [Face]
boxAt t s b c size = boxM t s b (mTrans c) size

-- | Koni (ağaç yaprağı).
coneFaces :: V3 -> Float -> Float -> Color -> [Face]
coneFaces c r h col =
  let n = 7
      ang i = 2 * pi * fromIntegral i / fromIntegral n
      base i = c + V3 (r * cos (ang i)) 0 (r * sin (ang i))
      apex = c + V3 0 h 0
      sides = [ Face (base (i + 1)) (base i) apex (lit (vnorm (vcross (base i - base (i + 1)) (apex - base (i + 1)))) col) | i <- [0 .. n - 1] ]
      bottom = [ Face c (base i) (base (i + 1)) (scaleC 0.55 col) | i <- [0 .. n - 1] ]
  in sides ++ bottom

-- | Sekiz yüzlü (kristal / kaya).
octaFaces :: M4 -> Color -> [Face]
octaFaces m col =
  let v = mApply m
      top = v (V3 0 1 0)
      bot = v (V3 0 (-1) 0)
      ring = [v (V3 1 0 0), v (V3 0 0 1), v (V3 (-1) 0 0), v (V3 0 0 (-1))]
      up = [ triFace col top (ring !! ((i + 1) `mod` 4)) (ring !! i) | i <- [0 .. 3] ]
      dn = [ triFace col bot (ring !! i) (ring !! ((i + 1) `mod` 4)) | i <- [0 .. 3] ]
  in concat up ++ concat dn

--------------------------------------------------------------------------------
-- Seviye nesneleri
--------------------------------------------------------------------------------

kindColors :: SolidKind -> (Color, Color, Color)
kindColors k = case k of
  Grass -> (rgbf 0.44 0.79 0.38, rgbf 0.48 0.35 0.23, rgbf 0.27 0.19 0.13)
  Stone -> (rgbf 0.76 0.79 0.85, rgbf 0.56 0.59 0.67, rgbf 0.34 0.36 0.43)
  Metal -> (rgbf 0.63 0.72 0.84, rgbf 0.37 0.43 0.55, rgbf 0.23 0.28 0.36)
  Wood -> (rgbf 0.74 0.57 0.35, rgbf 0.52 0.36 0.20, rgbf 0.29 0.19 0.11)
  Ice -> (rgbf 0.82 0.95 1.00, rgbf 0.55 0.81 0.96, rgbf 0.33 0.55 0.76)
  Bounce -> (rgbf 1.00 0.88 0.28, rgbf 0.96 0.62 0.14, rgbf 0.55 0.32 0.08)
  Hazard -> (rgbf 1.00 0.42 0.22, rgbf 0.90 0.16 0.12, rgbf 0.48 0.09 0.07)
  Gold -> (rgbf 1.00 0.91 0.50, rgbf 0.92 0.72 0.22, rgbf 0.55 0.40 0.10)

solidFaces :: Float -> Solid -> [Face]
solidFaces t s =
  let p = solidPosAt t s
      (tc, sc, bc) = kindColors (solKind s)
      V3 sx sy sz = solSize s
      glowC = rgbf 0.45 0.95 1.0
      base = boxAt tc sc bc p (solSize s)
      extra
        | solKind s == Metal && solMover s /= Still =
            concat
              [ boxAt glowC glowC glowC
                  (p + V3 dx (sy / 2 + 0.03) dz) (V3 0.22 0.12 0.22)
              | dx <- [-sx / 2 + 0.25, sx / 2 - 0.25]
              , dz <- [-sz / 2 + 0.25, sz / 2 - 0.25]
              ]
        | solKind s == Bounce =
            boxAt (rgbf 1 0.95 0.55) (rgbf 0.95 0.6 0.15) (rgbf 0.6 0.35 0.1)
              (p + V3 0 (sy / 2 + 0.02) 0)
              (V3 (sx * 0.55) 0.06 (sz * 0.55))
        | otherwise = []
  in base ++ extra

treeFaces :: (V3, Float) -> [Face]
treeFaces (base, sc) =
  let V3 x y z = base
      trunk = boxAt (rgbf 0.47 0.32 0.19) (rgbf 0.40 0.26 0.15) (rgbf 0.24 0.15 0.09)
                (V3 x (y + 0.75 * sc) z) (V3 (0.34 * sc) (1.5 * sc) (0.34 * sc))
      leaf1 = coneFaces (V3 x (y + 1.15 * sc) z) (1.45 * sc) (2.1 * sc) (rgbf 0.27 0.62 0.33)
      leaf2 = coneFaces (V3 x (y + 2.25 * sc) z) (1.05 * sc) (1.7 * sc) (rgbf 0.34 0.74 0.38)
  in trunk ++ leaf1 ++ leaf2

rockFaces :: Float -> (V3, Float) -> [Face]
rockFaces t (p, s) =
  let bob = sin (t * 0.5 + vx p) * 0.15
      m = mTrans (p + V3 0 bob 0) `mMul` mRotY (vx p * 0.3) `mMul` mScaleM s (s * 0.9) s
  in octaFaces m (rgbf 0.52 0.55 0.62)

decoFaces :: Float -> (V3, Float) -> [Face]
decoFaces t (p, s) =
  let bob = sin (t * 0.35 + vx p * 0.2) * 0.35
      c = p + V3 0 bob 0
  in boxAt (rgbf 0.40 0.72 0.42) (rgbf 0.44 0.36 0.28) (rgbf 0.26 0.22 0.18)
       (c - V3 0 (s * 1.4) 0) (V3 (s * 4.2) (s * 1.2) (s * 4.2))
     ++ octaFaces (mTrans (c - V3 0 (s * 2.6) 0) `mMul` mScaleM (s * 1.6) (s * 2.2) (s * 1.6))
          (rgbf 0.42 0.40 0.42)

signFaces :: (V3, Float) -> [Face]
signFaces (p, yaw) =
  let m = mTrans p `mMul` mRotY yaw
  in boxAt (rgbf 0.55 0.40 0.24) (rgbf 0.45 0.32 0.19) (rgbf 0.3 0.2 0.12)
       (p + V3 0 1.0 0) (V3 0.12 2.0 0.12)
     ++ boxM (rgbf 0.86 0.74 0.50) (rgbf 0.72 0.60 0.38) (rgbf 0.5 0.4 0.25)
          (m `mMul` mTrans (V3 0 1.75 0)) (V3 1.5 0.75 0.08)

crystalFaces :: Float -> V3 -> [Face]
crystalFaces t p =
  let bob = sin (t * 2.2 + vx p * 0.7) * 0.14
      m = mTrans (p + V3 0 bob 0) `mMul` mRotY (t * 1.9) `mMul` mScaleM 0.34 0.55 0.34
  in octaFaces m (rgbf 0.45 0.96 1.0)

checkpointFaces :: Float -> Checkpoint -> Bool -> [Face]
checkpointFaces t cp on =
  let base = cpPos cp
      pole = boxAt (rgbf 0.80 0.83 0.90) (rgbf 0.62 0.65 0.72) (rgbf 0.4 0.42 0.5)
               (base + V3 0 1.15 0) (V3 0.13 2.3 0.13)
      ball = octaFaces (mTrans (base + V3 0 2.35 0) `mMul` mScaleM 0.22 0.22 0.22)
               (if on then rgbf 0.3 1.0 0.55 else rgbf 0.7 0.72 0.78)
      flagCol = if on then rgbf 0.25 0.95 0.5 else rgbf 0.72 0.74 0.80
      m = mTrans (base + V3 0 1.85 0) `mMul` mRotY (cpYaw cp)
      flag = boxM flagCol (scaleC 0.8 flagCol) (scaleC 0.6 flagCol)
               (m `mMul` mTrans (V3 0.62 0 0)) (V3 1.15 0.62 0.07)
  in pole ++ ball ++ flag

portalFaces :: Float -> [Face]
portalFaces t =
  let c = levelPortalPos
      n = 14
      r = 1.7
      seg i =
        let a = t * 0.9 + 2 * pi * fromIntegral i / fromIntegral n
            p = c + V3 (r * cos a) (r * sin a) 0
            m = mTrans p `mMul` mRotZ a `mMul` mRotY (t * 2.5 + fromIntegral i)
        in boxM (rgbf 1 0.96 0.78) (rgbf 0.55 0.95 1) (rgbf 0.25 0.7 1) m (V3 0.55 0.3 0.3)
  in concatMap seg [0 .. n - 1]

--------------------------------------------------------------------------------
-- Oyuncu modeli
--------------------------------------------------------------------------------

playerFaces :: Player -> [Face]
playerFaces p =
  let base = plPos p - V3 0 0.9 0
      sq = plSquash p
      sy = 1 - sq * 0.45
      sxz = 1 + sq * 0.3
      root = mTrans base `mMul` mScaleM sxz sy sxz `mMul` mRotY (plYaw p)
      spd = vlen (vxz (plVel p))
      swing = clamp 0 1 (spd / 7.5) * 0.75
      w = sin (plWalk p)
      grounded = plGrounded p
      legA = if grounded then w * swing else 0.55
      legB = if grounded then negate (w * swing) else negate 0.4
      armA = if grounded then negate (w * swing * 0.9) else negate 1.5
      armB = if grounded then w * swing * 0.9 else negate 1.5
      bodyC = rgbf 0.22 0.55 0.86
      bodyD = rgbf 0.15 0.40 0.68
      skinC = rgbf 0.98 0.87 0.72
      darkC = rgbf 0.16 0.20 0.30
      bootC = rgbf 0.95 0.45 0.20
      torso = boxM (scaleC 1.05 bodyC) bodyC bodyD
                (root `mMul` mTrans (V3 0 0.92 0)) (V3 0.62 0.64 0.44)
      pack = boxM bodyD (scaleC 0.85 bodyD) (scaleC 0.6 bodyD)
                (root `mMul` mTrans (V3 0 0.98 0.29)) (V3 0.36 0.4 0.16)
      head = boxM (brighten 0.1 skinC) skinC (scaleC 0.72 skinC)
                (root `mMul` mTrans (V3 0 1.45 (sin (plWalk p * 2) * 0.012))) (V3 0.52 0.46 0.46)
      eyeL = boxAt (rgbf 0.1 0.12 0.18) (rgbf 0.1 0.12 0.18) (rgbf 0.1 0.12 0.18)
                (mApply root (V3 (-0.13) 1.47 (-0.235))) (V3 0.1 0.14 0.03)
      eyeR = boxAt (rgbf 0.1 0.12 0.18) (rgbf 0.1 0.12 0.18) (rgbf 0.1 0.12 0.18)
                (mApply root (V3 0.13 1.47 (-0.235))) (V3 0.1 0.14 0.03)
      antenna = boxM bootC (scaleC 0.8 bootC) (scaleC 0.6 bootC)
                  (root `mMul` mTrans (V3 0 1.78 0)) (V3 0.05 0.26 0.05)
      bulb = octaFaces (root `mMul` mTrans (V3 0 1.96 0) `mMul` mScaleM 0.09 0.09 0.09) (rgbf 1 0.85 0.35)
      limb ang tx ty tz size col =
        boxM (scaleC 1.05 col) col (scaleC 0.7 col)
          (root `mMul` mTrans (V3 tx ty tz) `mMul` mRotX ang `mMul` mTrans (V3 0 (negate (vy size) / 2) 0))
          size
      armL = limb armA (-0.39) 1.16 0 (V3 0.16 0.5 0.16) bodyC
      armR = limb armB 0.39 1.16 0 (V3 0.16 0.5 0.16) bodyC
      legL = limb legA (-0.16) 0.62 0 (V3 0.2 0.5 0.22) bodyD
      legR = limb legB 0.16 0.62 0 (V3 0.2 0.5 0.22) bodyD
      bootL = boxM bootC (scaleC 0.8 bootC) (scaleC 0.6 bootC)
                (root `mMul` mTrans (V3 (-0.16) 0.62 0) `mMul` mRotX legA `mMul` mTrans (V3 0 (-0.52) (-0.03)))
                (V3 0.22 0.12 0.3)
      bootR = boxM bootC (scaleC 0.8 bootC) (scaleC 0.6 bootC)
                (root `mMul` mTrans (V3 0.16 0.62 0) `mMul` mRotX legB `mMul` mTrans (V3 0 (-0.52) (-0.03)))
                (V3 0.22 0.12 0.3)
  in concat [torso, pack, head, eyeL, eyeR, antenna, bulb, armL, armR, legL, legR, bootL, bootR]

--------------------------------------------------------------------------------
-- Gölge ve parçacıklar
--------------------------------------------------------------------------------

shadowFaces :: Float -> Player -> [Face]
shadowFaces t p =
  case groundProbe t levelSolids (plPos p) of
    Nothing -> []
    Just (_, gy) ->
      let c = V3 (vx (plPos p)) (gy + 0.035) (vz (plPos p))
          h = max 0 (vy (plPos p) - 0.9 - gy)
          s = clamp 0.35 0.8 (0.85 - h * 0.07)
          col = rgba 0.05 0.07 0.12 (0.34 * clamp 0.2 1 (1 - h * 0.1))
          n = 12
          ring = [ c + V3 (s * cos a) 0 (s * sin a) | i <- [0 .. n - 1], let a = 2 * pi * fromIntegral i / fromIntegral n ]
          tris = [ Face (ring !! ((i + 1) `mod` n)) (ring !! i) c col | i <- [0 .. n - 1] ]
      in tris

billboard :: Basis -> V3 -> Float -> Color -> [Face]
billboard b c r col =
  let rt = vscale r (bsRight b)
      up = vscale r (bsUp b)
  in [ Face (c - rt - up) (c - rt + up) (c + rt + up) col
     , Face (c - rt - up) (c + rt + up) (c + rt - up) col
     ]

particleFaces :: Basis -> Particle -> [Face]
particleFaces b p =
  let k = clamp 0 1 (parLife p / max 0.001 (parMax p))
      c = withAlpha (k * k) (parCol p)
      s = parSize p * (0.4 + 0.6 * k)
  in billboard b (parPos p) s c

glowFaces :: Basis -> World -> [Face]
glowFaces b w =
  concat
    [ billboard b (levelPortalPos + V3 0 0 0) (2.1 + sin (wTime w * 3) * 0.12) (rgba 0.4 0.95 1 (0.28 + 0.06 * sin (wTime w * 3)))
    , billboard b (levelPortalPos + V3 0 0 0) 1.25 (rgba 0.85 1 1 0.35)
    ]
    ++ concat
      [ billboard b (V3 x y z) 0.9 (rgba 1 0.3 0.15 (0.18 + 0.08 * sin (wTime w * 4 + x)))
      | Solid _ (V3 x y z) _ Hazard _ <- levelSolids
      ]
    ++ concat
      [ billboard b (p + V3 0 (sin (wTime w * 2.2 + vx p * 0.7) * 0.14) 0) 0.85 (rgba 0.4 0.95 1 0.22)
      | (i, False) <- zip [0 ..] (wCrystals w)
      , let p = levelCrystals !! i
      ]

--------------------------------------------------------------------------------
-- Render
--------------------------------------------------------------------------------

renderWorld :: World -> IO Picture
renderWorld w = return $ pictures (skyLayer w ++ worldLayer w ++ hudLayer w)

worldLayer :: World -> [Picture]
worldLayer w =
  let c = wCam w
      b = mkBasis (camPos c) (camLook c)
      t = wTime w
      faces = concat
        [ concatMap (solidFaces t) levelSolids
        , concatMap treeFaces levelTrees
        , concatMap (rockFaces t) levelRocks
        , concatMap (decoFaces t) levelDecoIslands
        , concatMap signFaces levelSigns
        , concat [ crystalFaces t p | (i, False) <- zip [0 ..] (wCrystals w), let p = levelCrystals !! i ]
        , concat [ checkpointFaces t cp (wCheckpoints w !! i) | (i, cp) <- zip [0 ..] levelCheckpoints ]
        , portalFaces t
        , playerFaces (wPlayer w)
        , shadowFaces t (wPlayer w)
        , glowFaces b w
        , concatMap (particleFaces b) (wParts w)
        ]
      tris = concatMap (projectFace w b) faces
      sorted = sortOn (\(d, _, _) -> negate d) tris
  in map triPicture sorted

projectFace :: World -> Basis -> Face -> [(Float, [(Float, Float)], Color)]
projectFace w b f@(Face va vb vc col)
  | vdot (faceNormal f) (bsPos b - va) <= 0 = []
  | otherwise =
      let poly = clipNear 0.15 [toCam b va, toCam b vb, toCam b vc]
      in if length poly < 3
           then []
           else
             let depth = sum (map (negate . vz) poly) / fromIntegral (length poly)
                 wf = fromIntegral (wWidth w)
                 hf = fromIntegral (wHeight w)
                 pts = map (projectPt wf hf) poly
             in [(depth, pts, fogC depth col)]

fogC :: Float -> Color -> Color
fogC d c =
  let f = 1 - exp (negate ((d / 150) ** 1.6))
  in mixC (clamp 0 0.92 f) c skyHorizon

triPicture :: (Float, [(Float, Float)], Color) -> Picture
triPicture (_, pts, col) = color col (Polygon (expandPoly pts))

expandPoly :: [(Float, Float)] -> [(Float, Float)]
expandPoly pts =
  let n = fromIntegral (length pts)
      (sx, sy) = foldl' (\(ax, ay) (x, y) -> (ax + x, ay + y)) (0, 0) pts
      (cx, cy) = (sx / n, sy / n)
      out (x, y) =
        let dx = x - cx; dy = y - cy; l = sqrt (dx * dx + dy * dy)
        in if l < 1.0e-4 then (x, y) else (x + dx / l * 0.5, y + dy / l * 0.5)
  in map out pts

--------------------------------------------------------------------------------
-- Gökyüzü
--------------------------------------------------------------------------------

skyLayer :: World -> [Picture]
skyLayer w =
  let hw = fromIntegral (wWidth w) / 2
      hh = fromIntegral (wHeight w) / 2
      n = 36
      bandH = 2 * hh / fromIntegral n
      bands =
        [ translate 0 (hh - (fromIntegral i + 0.5) * bandH)
            (color (skyBand i) (rectangleSolid (2 * hw + 4) (bandH + 1.6)))
        | i <- [0 .. n - 1]
        ]
      c = wCam w
      b = mkBasis (camPos c) (camLook c)
      sunWorld = camPos c + vscale 600 (vnorm (V3 0.62 0.42 (-0.75)))
      sunCam = toCam b sunWorld
      sunPic
        | vz sunCam < -1 =
            let (sx, sy) = projectPt (fromIntegral (wWidth w)) (fromIntegral (wHeight w)) sunCam
            in [ translate sx sy (color (rgba 1 0.96 0.82 0.18) (circleSolid 120))
               , translate sx sy (color (rgba 1 0.97 0.86 0.35) (circleSolid 68))
               , translate sx sy (color (rgba 1 1 0.94 0.95) (circleSolid 40))
               ]
        | otherwise = []
      clouds = concatMap (cloudPic (camYaw c) hw hh) cloudDefs
  in bands ++ sunPic ++ clouds

skyBand :: Int -> Color
skyBand i =
  let t = fromIntegral i / 35 :: Float
  in mixC (t ** 0.75) skyTop skyHorizon

cloudDefs :: [(Float, Float, Float)]
cloudDefs =
  [ (0.35, 0.62, 1.0), (1.55, 0.78, 0.75), (2.75, 0.5, 1.2)
  , (-1.0, 0.68, 0.9), (-2.3, 0.84, 0.7), (3.6, 0.58, 1.05)
  , (-3.9, 0.52, 0.85), (0.95, 0.9, 0.6), (-0.35, 0.44, 1.1)
  ]

cloudPic :: Float -> Float -> Float -> (Float, Float, Float) -> [Picture]
cloudPic yaw hw hh (a, y, s) =
  let rel = wrapAngle (a - yaw)
  in if abs rel > 1.2
       then []
       else
         let x = rel / 1.2 * (hw * 1.3)
             cy = hh * y
             col = rgba 1 1 1 0.55
             puffs =
               [ (0, 0, 62), (-52, -10, 44), (54, -8, 48), (-20, 22, 46), (26, 20, 42) ]
         in [ translate (x + dx * s) (cy + dy * s) (color col (circleSolid (r * s)))
            | (dx, dy, r) <- puffs
            ]

--------------------------------------------------------------------------------
-- HUD
--------------------------------------------------------------------------------

text' :: Float -> Color -> String -> Picture
text' sz c s = color c (scale (sz / 152) (sz / 152) (text s))

-- | Yatayda ortalanmis metin (gloss metni sola dayali cizer).
textC :: Float -> Color -> String -> Picture
textC sz c s = translate (negate (0.345 * sz * fromIntegral (length s))) 0 (text' sz c s)

diamond :: Float -> Color -> Picture
diamond s c = color c (polygon [(0, s), (s * 0.62, 0), (0, negate s), (negate (s * 0.62), 0)])

heartPic :: Float -> Color -> Picture
heartPic s c = color c $ pictures
  [ translate (negate (s * 0.34)) (s * 0.24) (circleSolid (s * 0.37))
  , translate (s * 0.34) (s * 0.24) (circleSolid (s * 0.37))
  , polygon [(negate (s * 0.7), s * 0.24), (s * 0.7, s * 0.24), (0, negate (s * 0.72))]
  ]

fmtTime :: Float -> String
fmtTime t =
  let total = max 0 (floor t :: Int)
      m = total `div` 60
      s = total `mod` 60
      pad2 n = if n < 10 then '0' : show n else show n
  in pad2 m ++ ":" ++ pad2 s

panel :: Float -> Float -> Float -> Float -> Picture
panel x y w h =
  translate x y (color (rgba 0.02 0.04 0.08 0.42) (rectangleSolid w h))

hudLayer :: World -> [Picture]
hudLayer w = case wScreen w of
  Title -> titleScreen w
  Playing -> hudBase w ++ centerMessage w
  Paused -> hudBase w ++ pauseScreen w
  Won -> hudBase w ++ winScreen w
  Lost -> hudBase w ++ loseScreen w

hudBase :: World -> [Picture]
hudBase w =
  let hw = fromIntegral (wWidth w) / 2
      hh = fromIntegral (wHeight w) / 2
      n = length (filter id (wCrystals w))
      px = negate hw + 22
      py = hh - 22
      panelW = 268
      panelH = 48
      leftPanel =
        [ panel (px + panelW / 2) (py - panelH / 2) panelW panelH
        , translate (px + 26) (py - 24) (diamond 13 (rgbf 0.45 0.96 1))
        , translate (px + 48) (py - 33) (text' 22 (rgbf 0.92 0.98 1) ("KRISTAL  " ++ show n ++ " / " ++ show totalCrystals))
        ]
      tw = 196
      timePanel =
        [ panel (hw - 22 - tw / 2) (py - panelH / 2) tw panelH
        , translate (hw - 22 - tw + 16) (py - 33) (text' 22 (rgbf 1 0.94 0.75) ("SURE  " ++ fmtTime (wPlayTime w)))
        ]
      hearts = [ translate (hw - 26 - fromIntegral i * 30) (py - panelH - 26) (heartPic 13 (rgbf 1 0.35 0.42))
               | i <- [0 .. wLives w - 1] ]
      hint
        | wHintT w > 0 && wScreen w == Playing =
            [ translate 0 (negate hh + 26)
                (textC 18 (rgba 1 1 1 (clamp 0 0.9 (wHintT w / 3)))
                  "WASD: HAREKET   SPACE: ZIPLA   FARE: KAMERA   SHIFT: KOS   P: DURAKLAT")
            ]
        | otherwise = []
      flash
        | wFlash w > 0 =
            [ color (rgba 1 0.3 0.25 (clamp 0 0.55 (wFlash w))) (rectangleSolid (2 * hw) (2 * hh)) ]
        | otherwise = []
      dbg
        | wDebug w =
            let p = wPlayer w
                ps = plPos p
                vs = plVel p
                ls =
                  [ "POS  " ++ showF (vx ps) ++ " " ++ showF (vy ps) ++ " " ++ showF (vz ps)
                  , "VEL  " ++ showF (vx vs) ++ " " ++ showF (vy vs) ++ " " ++ showF (vz vs)
                  , "DUZ  " ++ show (plGrounded p) ++ "  YER " ++ show (plGround p) ++ "  ZIPLA " ++ show (plJumps p) ++ "  BUF " ++ showF (plJumpBuf p)
                  , "KAMR " ++ showF (camYaw (wCam w)) ++ "  AGI " ++ showF (camPitch (wCam w))
                  , "SIST " ++ show (wTime w) ++ "  ADIM " ++ show (wAcc w)
                  ]
                linePos n = -(hh) + 64 + fromIntegral n * 20
            in [ translate (negate hw + 22) (linePos n) (text' 15 (rgba 0.9 1 0.95 0.9) s)
               | (n, s) <- zip [0 :: Int ..] ls
               ]
        | otherwise = []
  in leftPanel ++ timePanel ++ hearts ++ hint ++ flash ++ dbg

centerMessage :: World -> [Picture]
centerMessage w
  | wMsgT w <= 0 = []
  | otherwise =
      let a = clamp 0 1 (wMsgT w)
      in [ translate 0 120 (textC 30 (rgba 1 0.95 0.6 a) (wMsg w)) ]

titleScreen :: World -> [Picture]
titleScreen w =
  let hw = fromIntegral (wWidth w) / 2
      hh = fromIntegral (wHeight w) / 2
      blinkA = 0.55 + 0.45 * sin (wTime w * 4)
  in [ color (rgba 0.02 0.05 0.12 0.35) (rectangleSolid (2 * hw) (2 * hh))
     , translate 0 96 (textC 62 (rgbf 1 1 1) "GOKYUZU ADALARI")
     , translate 0 40 (textC 26 (rgbf 0.62 0.9 1) "3D PLATFORM MACERASI")
     , translate 0 (negate 62) (textC 34 (rgba 1 0.9 0.45 blinkA) "BASLAMAK ICIN ENTER")
     , translate 0 (negate 140) (textC 19 (rgbf 0.88 0.94 1) "WASD: HAREKET    SPACE: ZIPLA    FARE: KAMERA    SHIFT: KOS")
     , translate 0 (negate 172) (textC 19 (rgbf 0.88 0.94 1) "10 KRISTAL TOPLA VE PORTALA ULAS")
     , translate 0 (negate 232) (textC 16 (rgba 0.85 0.92 1 0.75) "Hazirlayan: Haskell + OpenGL (gloss)")
     ]

pauseScreen :: World -> [Picture]
pauseScreen w =
  let hw = fromIntegral (wWidth w) / 2
      hh = fromIntegral (wHeight w) / 2
  in [ color (rgba 0.02 0.05 0.1 0.6) (rectangleSolid (2 * hw) (2 * hh))
     , translate 0 60 (textC 54 (rgbf 1 1 1) "DURAKLATILDI")
     , translate 0 (negate 20) (textC 24 (rgbf 0.85 0.93 1) "Devam etmek icin P veya ESC")
     , translate 0 (negate 60) (textC 20 (rgbf 0.8 0.88 1) "R: Basa don")
     ]

winScreen :: World -> [Picture]
winScreen w =
  let hw = fromIntegral (wWidth w) / 2
      hh = fromIntegral (wHeight w) / 2
      n = length (filter id (wCrystals w))
  in [ color (rgba 0.02 0.06 0.12 0.55) (rectangleSolid (2 * hw) (2 * hh))
     , translate 0 120 (textC 60 (rgbf 1 0.93 0.5) "TEBRIKLER!")
     , translate 0 60 (textC 28 (rgbf 0.9 0.98 1) ("Tum kristaller: " ++ show n ++ " / " ++ show totalCrystals))
     , translate 0 20 (textC 28 (rgbf 0.9 0.98 1) ("Sure: " ++ fmtTime (wPlayTime w)))
     , translate 0 (negate 24) (textC 32 (rgbf 1 0.86 0.35) ("PUAN: " ++ show (wScore w)))
     , translate 0 (negate 100) (textC 22 (rgbf 0.85 0.93 1) "R: Tekrar oyna      ESC: Ana menu")
     ]

loseScreen :: World -> [Picture]
loseScreen w =
  let hw = fromIntegral (wWidth w) / 2
      hh = fromIntegral (wHeight w) / 2
  in [ color (rgba 0.08 0.02 0.03 0.6) (rectangleSolid (2 * hw) (2 * hh))
     , translate 0 70 (textC 56 (rgbf 1 0.45 0.4) "OYUN BITTI")
     , translate 0 0 (textC 26 (rgbf 0.95 0.92 0.9) ("Toplanan kristal: " ++ show (length (filter id (wCrystals w))) ++ " / " ++ show totalCrystals))
     , translate 0 (negate 70) (textC 24 (rgbf 0.9 0.9 0.95) "R: Tekrar dene      ESC: Ana menu")
     ]
