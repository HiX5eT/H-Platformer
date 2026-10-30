{-# LANGUAGE BangPatterns #-}

-- | Oyuncu fiziği: AABB çarpışması, zıplama, hareket eden platformlara binme.
module Game.Physics
  ( Input (..)
  , stepPlayer
  , groundProbe
  , playerSpeed
  , moveAxis
  , gravity, jumpSpeed, doubleJumpSpeed, bounceSpeed
  ) where

import Game.Math
import Game.Types
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | Fizik sabitleri.
gravity :: Float
gravity = 34

jumpSpeed :: Float
jumpSpeed = 12.4

doubleJumpSpeed :: Float
doubleJumpSpeed = 10.8

bounceSpeed :: Float
bounceSpeed = 22.5

runSpeed :: Float
runSpeed = 8.4

sprintSpeed :: Float
sprintSpeed = 11.2

playerSpeed :: Bool -> Float
playerSpeed sprint = if sprint then sprintSpeed else runSpeed

data Input = Input
  { inMove :: !V3
  , inJump :: !Bool        -- bu karede zıplama istendi mi (buffer)
  , inJumpHeld :: !Bool
  , inSprint :: !Bool
  } deriving (Show)

--------------------------------------------------------------------------------
-- Eksen yardımcıları
--------------------------------------------------------------------------------

getC :: Int -> V3 -> Float
getC 0 (V3 x _ _) = x
getC 1 (V3 _ y _) = y
getC _ (V3 _ _ z) = z

setC :: Int -> Float -> V3 -> V3
setC 0 v (V3 _ y z) = V3 v y z
setC 1 v (V3 x _ z) = V3 x v z
setC _ v (V3 x y _) = V3 x y v

halfC :: Int -> Float
halfC ax = getC ax playerHalf

--------------------------------------------------------------------------------
-- Zemin sondası
--------------------------------------------------------------------------------

-- | Oyuncunun hemen altındaki en yüksek katı yüzey (id, y).
groundProbe :: Float -> [Solid] -> V3 -> Maybe (Int, Float)
groundProbe t solids pos =
  let feetY = vy pos - vy playerHalf
      ph = playerHalf
      box = AABB
        (V3 (vx pos - vx ph) (feetY - 0.14) (vz pos - vz ph))
        (V3 (vx pos + vx ph) feetY (vz pos + vz ph))
      cands =
        [ (solId s, vy (aabbMax b))
        | s <- solids
        , isSolidKind (solKind s)
        , let b = solidAABBAt t s
        , vy (aabbMax b) <= feetY + 0.16
        , aabbOverlap box b
        ]
  in case cands of
       [] -> Nothing
       _ -> Just (maximumBy (comparing snd) cands)

--------------------------------------------------------------------------------
-- Eksen boyunca hareket + çarpışma çözümü
--------------------------------------------------------------------------------

-- | Bir eksende hareket eder, çakışma varsa geri iter.
--   Dönen değer: yeni konum ve (çarptığı cisim, yüzey koordinatı).
moveAxis :: Int -> Float -> Float -> [Solid] -> V3 -> (V3, Maybe (Int, Float))
moveAxis ax d t solids pos
  | abs d < 1.0e-7 = (pos, Nothing)
  | otherwise =
      let pos1 = setC ax (getC ax pos + d) pos
          box = playerAABB pos1
          hits =
            [ (solId s, b)
            | s <- solids
            , isSolidKind (solKind s)
            , let b = solidAABBAt t s
            , aabbOverlap box b
            ]
      in if null hits
           then (pos1, Nothing)
           else
             let h = halfC ax
                 edges =
                   [ (i, if d > 0 then getC ax (aabbMin b) else getC ax (aabbMax b))
                   | (i, b) <- hits
                   ]
                 edge = if d > 0 then minimum (map snd edges) else maximum (map snd edges)
                 newC = if d > 0 then edge - h - 0.001 else edge + h + 0.001
                 hitId = fst (head [ e | e <- edges, abs (snd e - edge) < 1.0e-4 ])
             in (setC ax newC pos1, Just (hitId, edge))

--------------------------------------------------------------------------------
-- Oyuncu adımı
--------------------------------------------------------------------------------

stepPlayer :: Float -> Float -> [Solid] -> Input -> Player -> Player
stepPlayer dt t solids inp pl =
  let -- hareketli platform taşıması
      carry = case plGround pl of
        Just gid
          | gid >= 0 && gid < length solids ->
              let s = solids !! gid
              in solidPosAt t s - solidPosAt (t - dt) s
        _ -> V3 0 0 0

      p1 = pl
        { plPos = plPos pl + carry
        , plCoyote = max 0 (plCoyote pl - dt)
        , plJumpBuf = max 0 (plJumpBuf pl - dt)
        , plBounceCool = max 0 (plBounceCool pl - dt)
        , plSquash = approach (3.2 * dt) 0 (plSquash pl)
        }

      ground = groundProbe t solids (plPos p1)
      grounded0 = case ground of
        Just _ -> True
        Nothing -> False
      gid0 = fst <$> ground
      coyote1 = if grounded0 then 0.13 else plCoyote p1
      jumps1 = if grounded0 then 1 else plJumps p1

      -- yatay hız
      wishV = inMove inp
      spd = playerSpeed (inSprint inp)
      wish = if vlen2 wishV < 1.0e-4 then V3 0 0 0 else vscale spd (vnorm wishV)
      accel = if grounded0 then 85 else 38
      hv0 = V3 (vx (plVel p1)) 0 (vz (plVel p1))
      hv1
        | vlen2 wish < 1.0e-4 && grounded0 = approachV (62 * dt) (V3 0 0 0) hv0
        | otherwise = approachV (accel * dt) wish hv0

      -- dikey hız
      vy0 = vy (plVel p1)
      vyG = vy0 - gravity * dt
      wantJump = inJump inp && plJumpBuf p1 > 0
      canGround = grounded0 || coyote1 > 0
      (vy1, jumps2, jumped)
        | wantJump && canGround = (jumpSpeed, 1, True)
        | wantJump && jumps1 > 0 = (doubleJumpSpeed, jumps1 - 1, True)
        | otherwise = (vyG, jumps1, False)
      -- tuş bırakılınca zıplamayı kısalt (trambolin hariç)
      vy2
        | not (inJumpHeld inp) && vy1 > 0 && vy0 > 0 && plBounceCool p1 <= 0 = vy1 * 0.55
        | otherwise = vy1

      vel1 = V3 (vx hv1) vy2 (vz hv1)

      -- çarpışmalı entegrasyon
      (posX, hitX) = moveAxis 0 (vx vel1 * dt) t solids (plPos p1)
      (posZ, hitZ) = moveAxis 2 (vz vel1 * dt) t solids posX
      vx' = if hitX == Nothing then vx vel1 else 0
      vz' = if hitZ == Nothing then vz vel1 else 0

      (posY, vy3, grounded1, gid1, landed)
        | grounded0 && vy2 <= 0 =
            (V3 (vx posZ) (vy (plPos p1)) (vz posZ), 0, True, gid0, False)
        | otherwise =
            let (py, hitY) = moveAxis 1 (vy2 * dt) t solids posZ
            in case hitY of
                 Just (i, _)
                   | vy2 <= 0 -> (py, 0, True, Just i, vy2 < -11)
                   | otherwise -> (py, 0, False, Nothing, False)
                 Nothing -> (py, vy2, False, Nothing, False)

      speedH = vlen (V3 vx' 0 vz')
      yawTarget = atan2 (negate vx') (negate vz')
      yaw' = if speedH > 0.5 then angleLerp (damp 16 dt) (plYaw p1) yawTarget else plYaw p1
  in Player
       { plPos = posY
       , plVel = V3 vx' vy3 vz'
       , plYaw = yaw'
       , plGrounded = grounded1
       , plCoyote = coyote1
       , plJumpBuf = if jumped then 0 else plJumpBuf p1
       , plJumps = jumps2
       , plWalk = plWalk p1 + dt * speedH * 1.7
       , plSquash = if landed then 0.4 else plSquash p1
       , plGround = gid1
       , plBounceCool = plBounceCool p1
       , plAirTime = if grounded1 then 0 else plAirTime p1 + dt
       }
