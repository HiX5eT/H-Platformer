{-# LANGUAGE BangPatterns #-}

-- | Oyun döngüsü: girdi, fizik adımları, toplama, kamera, ekran geçişleri.
module Game.Update
  ( initialWorld
  , handleEvent
  , updateWorld
  ) where

import Game.Math
import Game.Types
import Game.Level
import Game.Physics
import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Data.Color (Color)
import qualified Data.Set as Set
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
-- Başlangıç
--------------------------------------------------------------------------------

initialWorld :: World
initialWorld = World
  { wScreen = Title
  , wTime = 0
  , wPlayTime = 0
  , wPlayer = newPlayer spawnPoint
  , wCam = newCam 0
  , wKeys = Set.empty
  , wMouse = (0, 0)
  , wDrag = False
  , wCrystals = replicate totalCrystals False
  , wCheckpoints = replicate (length levelCheckpoints) False
  , wRespawn = spawnPoint
  , wRespawnYaw = 0
  , wLives = 3
  , wScore = 0
  , wMsg = ""
  , wMsgT = 0
  , wParts = []
  , wShake = 0
  , wAcc = 0
  , wWidth = 1280
  , wHeight = 720
  , wHintT = 12
  , wFlash = 0
  , wLandFx = 0
  , wDebug = False
  }

--------------------------------------------------------------------------------
-- Olaylar
--------------------------------------------------------------------------------

handleEvent :: G.Event -> World -> IO World
handleEvent ev w = return $ case ev of
  G.EventKey k st mods _ -> keyEvent k st mods w
  G.EventMotion (x, y) -> motionEvent x y w
  G.EventResize (x, y) -> w { wWidth = max 480 x, wHeight = max 360 y }
  _ -> w

keyEvent :: G.Key -> G.KeyState -> G.Modifiers -> World -> World
keyEvent gk st mods w0 =
  let w = w0 { wKeys = if G.shift mods == G.Down
                         then Set.insert KShift (wKeys w0)
                         else Set.delete KShift (wKeys w0) }
  in case gk of
       G.Char c
         | st == G.Down -> charDown (toLower c) w
         | otherwise -> charUp (toLower c) w
       G.SpecialKey sk
         | st == G.Down -> specialDown sk w
         | otherwise -> specialUp sk w
       G.MouseButton mb
         | st == G.Down -> mouseDown mb w
         | otherwise -> w { wDrag = False }
       _ -> w

press :: Key -> World -> World
press k w = w { wKeys = Set.insert k (wKeys w) }

release :: Key -> World -> World
release k w = w { wKeys = Set.delete k (wKeys w) }

charDown :: Char -> World -> World
charDown c w = case c of
  'w' -> press KW w
  'a' -> press KA w
  's' -> press KS w
  'd' -> press KD w
  'q' -> press KQ w
  'e' -> press KE w
  'p' -> press KP (togglePause w)
  'r' -> press KR (restartIfAny w)
  'm' -> press KM w
  'f' -> press KF (w { wDebug = not (wDebug w) })
  ' ' -> jumpPressed (press KSpace w)
  _ -> w

charUp :: Char -> World -> World
charUp c w = case c of
  'w' -> release KW w
  'a' -> release KA w
  's' -> release KS w
  'd' -> release KD w
  'q' -> release KQ w
  'e' -> release KE w
  'p' -> release KP w
  'r' -> release KR w
  'm' -> release KM w
  'f' -> release KF w
  ' ' -> release KSpace w
  _ -> w

specialDown :: G.SpecialKey -> World -> World
specialDown sk w = case sk of
  G.KeySpace -> jumpPressed (press KSpace w)
  G.KeyEnter -> startOrRestart w
  G.KeyEsc -> escPressed w
  G.KeyUp -> press KUp w
  G.KeyDown -> press KDown w
  G.KeyLeft -> press KLeft w
  G.KeyRight -> press KRight w
  _ -> w

specialUp :: G.SpecialKey -> World -> World
specialUp sk w = case sk of
  G.KeySpace -> release KSpace w
  G.KeyUp -> release KUp w
  G.KeyDown -> release KDown w
  G.KeyLeft -> release KLeft w
  G.KeyRight -> release KRight w
  _ -> w

mouseDown :: G.MouseButton -> World -> World
mouseDown G.LeftButton w = w { wDrag = True }
mouseDown G.RightButton w = w { wDrag = True }
mouseDown _ w = w

motionEvent :: Float -> Float -> World -> World
motionEvent x y w =
  let (lx, ly) = wMouse w
      dx = x - lx
      dy = y - ly
      cam = wCam w
      cam'
        | wDrag w =
            cam { camYaw = camYaw cam + dx * 0.006
                , camPitch = clamp 0.03 1.22 (camPitch cam + dy * 0.004)
                }
        | otherwise = cam
  in w { wMouse = (x, y), wCam = cam' }

--------------------------------------------------------------------------------
-- Ekran geçişleri
--------------------------------------------------------------------------------

startOrRestart :: World -> World
startOrRestart w = case wScreen w of
  Title -> resetLevel w
  Won -> resetLevel w
  Lost -> resetLevel w
  _ -> w

escPressed :: World -> World
escPressed w = case wScreen w of
  Playing -> w { wScreen = Paused, wKeys = Set.empty }
  Paused -> w { wScreen = Title, wKeys = Set.empty }
  Won -> w { wScreen = Title, wKeys = Set.empty }
  Lost -> w { wScreen = Title, wKeys = Set.empty }
  Title -> w
  _ -> w

togglePause :: World -> World
togglePause w = case wScreen w of
  Playing -> w { wScreen = Paused, wKeys = Set.empty }
  Paused -> w { wScreen = Playing }
  _ -> w

restartIfAny :: World -> World
restartIfAny w = case wScreen w of
  Playing -> resetLevel w
  Paused -> resetLevel w
  Won -> resetLevel w
  Lost -> resetLevel w
  Title -> w

jumpPressed :: World -> World
jumpPressed w = case wScreen w of
  Title -> resetLevel w
  Playing -> w { wPlayer = (wPlayer w) { plJumpBuf = 0.16 } }
  _ -> w

resetLevel :: World -> World
resetLevel w = w
  { wScreen = Playing
  , wPlayTime = 0
  , wPlayer = newPlayer spawnPoint
  , wCam = newCam 0
  , wCrystals = replicate totalCrystals False
  , wCheckpoints = replicate (length levelCheckpoints) False
  , wRespawn = spawnPoint
  , wRespawnYaw = 0
  , wLives = 3
  , wScore = 0
  , wMsg = ""
  , wMsgT = 0
  , wParts = []
  , wShake = 0
  , wAcc = 0
  , wHintT = 12
  , wFlash = 0
  , wKeys = Set.empty
  , wLandFx = 0
  , wDebug = False
  }

--------------------------------------------------------------------------------
-- Güncelleme
--------------------------------------------------------------------------------

updateWorld :: Float -> World -> IO World
updateWorld dt w0 = return $ case wScreen w0 of
  Title -> titleTick dt w0
  Paused -> w0
  Won -> idleTick dt w0
  Lost -> idleTick dt w0
  Playing -> playTick dt w0

idleTick :: Float -> World -> World
idleTick dt w = w
  { wTime = wTime w + dt
  , wParts = updateParts dt (wParts w)
  , wShake = max 0 (wShake w - dt * 2)
  , wFlash = max 0 (wFlash w - dt * 1.5)
  }

titleTick :: Float -> World -> World
titleTick dt w =
  let t = wTime w + dt
      ang = t * 0.14
      target = V3 0 4.5 (-18)
      dist = 30
      camPos' = target + V3 (sin ang * dist) (13 + sin (t * 0.32) * 1.5) (cos ang * dist)
      cam' = (wCam w)
        { camPos = camPos'
        , camLook = target
        , camYaw = ang
        , camPitch = 0.3
        }
  in w { wTime = t, wCam = cam', wParts = updateParts dt (wParts w) }

playTick :: Float -> World -> World
playTick dt0 w =
  let dt = min 0.05 dt0
      w1 = w
        { wTime = wTime w + dt
        , wPlayTime = wPlayTime w + dt
        , wAcc = wAcc w + dt
        , wHintT = max 0 (wHintT w - dt)
        , wMsgT = max 0 (wMsgT w - dt)
        , wFlash = max 0 (wFlash w - dt * 1.7)
        , wShake = max 0 (wShake w - dt * 2.2)
        , wCam = rotateCam dt (wCam w) (wKeys w)
        }
      h = 1 / 120
      steps = min 12 (floor (wAcc w1 / h) :: Int)
      w2 = iterate (physStep h) w1 !! steps
      w3 = w2 { wAcc = wAcc w1 - fromIntegral steps * h }
      -- bosluga dusem bolirse kontrol noktasina don
      w4 = if vy (plPos (wPlayer w3)) < -45.0 then hurtPlayer w3 else w3
      w5 = collectPickups (checkCheckpoints w4)
      w6 = checkPortal w5
      w7 = updateCam dt w6
      w8 = w7 { wParts = updateParts dt (wParts w7), wLandFx = max 0 (wLandFx w7 - dt * 3) }
  in w8

rotateCam :: Float -> Cam -> Set.Set Key -> Cam
rotateCam dt cam keys =
  let has k = Set.member k keys
      d = (if has KE then 1 else 0) - (if has KQ then 1 else 0)
  in cam { camYaw = camYaw cam + d * 2.0 * dt }

physStep :: Float -> World -> World
physStep h w =
  let t = wTime w
      inp = readInput w
      p0 = wPlayer w
      p1 = stepPlayer h t levelSolids inp p0
      (p2, bounced) = bounceTriggers t p1
      w1 = w { wPlayer = p2 }
  in if bounced
       then w1 { wParts = wParts w1 ++ burst 12 (plPos p2 - V3 0 0.9 0) (rgbf 1 0.85 0.35) }
       else if hazardHit t p2
         then hurtPlayer w1
         else w1

readInput :: World -> Input
readInput w =
  let k = wKeys w
      has key = Set.member key k
      yaw = camYaw (wCam w)
      fwd = V3 (negate (sin yaw)) 0 (negate (cos yaw))
      right = V3 (cos yaw) 0 (negate (sin yaw))
      f = (if has KW || has KUp then 1 else 0) - (if has KS || has KDown then 1 else 0)
      r = (if has KD || has KRight then 1 else 0) - (if has KA || has KLeft then 1 else 0)
      mv = vscale (fromIntegral f) fwd + vscale (fromIntegral r) right
      p = wPlayer w
  in Input mv (plJumpBuf p > 0) (has KSpace) (has KShift)

--------------------------------------------------------------------------------
-- Trambolin / tehlike
--------------------------------------------------------------------------------

bounceTriggers :: Float -> Player -> (Player, Bool)
bounceTriggers t p
  | plBounceCool p > 0 = (p, False)
  | vy (plVel p) > 1.0 = (p, False)
  | otherwise =
      let box = playerAABB (plPos p)
          hit = [ s | s <- levelSolids, solKind s == Bounce, aabbOverlap box (solidAABBAt t s) ]
      in if null hit
           then (p, False)
           else ( p { plVel = V3 (vx (plVel p)) bounceSpeed (vz (plVel p))
                    , plBounceCool = 0.45
                    , plSquash = negate 0.38
                    , plJumps = 1
                    }
                , True
                )

hazardHit :: Float -> Player -> Bool
hazardHit t p =
  let h = vscale 2 (playerHalf - V3 0.12 0.12 0.12)
      box = aabbOf (plPos p) h
  in any (\s -> solKind s == Hazard && aabbOverlap box (solidAABBAt t s)) levelSolids

hurtPlayer :: World -> World
hurtPlayer w =
  let lives' = wLives w - 1
      w1 = w
        { wLives = lives'
        , wShake = 0.9
        , wFlash = 0.6
        , wParts = wParts w ++ burst 22 (plPos (wPlayer w)) (rgbf 1 0.4 0.28)
        , wMsg = if lives' > 0 then "AH! Tekrar dene" else "Can kalmadi"
        , wMsgT = 1.6
        }
  in if lives' <= 0
       then w1 { wScreen = Lost, wPlayer = newPlayer (wRespawn w1) }
       else respawn w1

respawn :: World -> World
respawn w =
  let p = newPlayer (wRespawn w)
      yaw = wRespawnYaw w
      cam = newCam yaw
  in w { wPlayer = p { plYaw = 0 }, wCam = cam, wKeys = Set.empty }

--------------------------------------------------------------------------------
-- Toplama
--------------------------------------------------------------------------------

collectPickups :: World -> World
collectPickups w =
  let p = plPos (wPlayer w)
      idxs = [ i | (i, False) <- zip [0 ..] (wCrystals w), vdist p (levelCrystals !! i) < 1.4 ]
  in if null idxs
       then w
       else w
         { wCrystals = [ i `elem` idxs || taken | (i, taken) <- zip [0 ..] (wCrystals w) ]
         , wScore = wScore w + 100 * length idxs
         , wParts = wParts w ++ concatMap (\i -> burst 14 (levelCrystals !! i) (rgbf 0.45 0.96 1)) idxs
         , wMsg = "KRISTAL +100"
         , wMsgT = 1.1
         }

checkCheckpoints :: World -> World
checkCheckpoints w =
  let p = plPos (wPlayer w)
      hits =
        [ (i, cp)
        | (i, cp) <- zip [0 ..] levelCheckpoints
        , not (wCheckpoints w !! i)
        , vdist (vxz p) (vxz (cpPos cp)) < 2.4
        , abs (vy p - vy (cpPos cp)) < 3.0
        ]
  in case hits of
       [] -> w
       ((i, cp) : _) -> w
         { wCheckpoints = [ j == i || taken | (j, taken) <- zip [0 ..] (wCheckpoints w) ]
         , wRespawn = cpSpawn cp
         , wMsg = "KONTROL NOKTASI"
         , wMsgT = 1.8
         , wScore = wScore w + 50
         , wParts = wParts w ++ burst 20 (cpPos cp + V3 0 1.6 0) (rgbf 0.35 1 0.6)
         }

checkPortal :: World -> World
checkPortal w
  | wScreen w /= Playing = w
  | vdist (plPos (wPlayer w)) levelPortalPos < 2.3 =
      let n = length (filter id (wCrystals w))
          timeBonus = max 0 (400 - floor (wPlayTime w) * 3)
          finalScore = wScore w + n * 50 + timeBonus + wLives w * 150
          w1 = w { wScreen = Won, wScore = finalScore, wMsg = "", wMsgT = 0, wFlash = 0.35 }
      in w1 { wParts = wParts w1 ++ burst 40 (levelPortalPos) (rgbf 0.5 0.95 1) }
  | otherwise = w

--------------------------------------------------------------------------------
-- Kamera
--------------------------------------------------------------------------------

updateCam :: Float -> World -> World
updateCam dt w =
  let p = wPlayer w
      cam = wCam w
      focus = plPos p + V3 0 0.6 0
      yaw = camYaw cam
      pitch = camPitch cam
      dist = camDist cam
      offset = V3 (sin yaw * cos pitch * dist) (sin pitch * dist) (cos yaw * cos pitch * dist)
      desired0 = focus + offset
      desired = V3 (vx desired0) (max 1.0 (vy desired0)) (vz desired0)
      safe = cameraSafe (wTime w) focus desired
      k = damp 10 dt
      pos' = vlerp k (camPos cam) safe
      look' = vlerp (damp 16 dt) (camLook cam) focus
      sh = wShake w
      sx = sin (wTime w * 61) * sh * 0.45
      sy = sin (wTime w * 73 + 1.3) * sh * 0.35
  in w { wCam = cam { camPos = pos' + V3 sx sy 0, camLook = look' + V3 (sx * 0.5) (sy * 0.5) 0 } }

-- | Kamera ile odak arasında katı cisim varsa kamerayı yaklaştırır.
cameraSafe :: Float -> V3 -> V3 -> V3
cameraSafe t from to =
  let hit = [ f | i <- [3 .. 16 :: Int]
                , let f = fromIntegral i / 16
                , let p = vlerp f from to
                , blocked p
              ]
      frac = case hit of
               (f : _) -> max 0.30 (f * 0.93)
               [] -> 1.0
  in vlerp frac from to
  where
    blocked p =
      let box = aabbOf p (V3 0.45 0.45 0.45)
      in any (\s -> isSolidKind (solKind s) && aabbOverlap box (solidAABBAt t s)) levelSolids

--------------------------------------------------------------------------------
-- Parçacıklar
--------------------------------------------------------------------------------

updateParts :: Float -> [Particle] -> [Particle]
updateParts dt = concatMap step
  where
    step p =
      let life' = parLife p - dt
      in if life' <= 0
           then []
           else
             let vel' = parVel p - V3 0 (parGrav p * dt) 0
                 pos' = parPos p + vscale dt vel'
             in [p { parLife = life', parPos = pos', parVel = vel' }]

burst :: Int -> V3 -> Color -> [Particle]
burst n pos col =
  [ Particle
      (pos + V3 0 0.3 0)
      (V3 (cos a * s) (1.2 + 2.6 * sin b) (sin a * s))
      0.85 0.85 col (0.09 + 0.05 * sin b) 11
  | i <- [0 .. n - 1]
  , let a = 2 * pi * fromIntegral i / fromIntegral n + 0.7
        s = 2.2 + 1.9 * sin (fromIntegral i * 1.7)
        b = fromIntegral i * 2.1
  ]
