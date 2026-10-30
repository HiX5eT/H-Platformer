{-# LANGUAGE BangPatterns #-}

-- | Küçük vektör / matris kütüphanesi.
--   3B oyun için gereken her şey burada: nokta, yön, afin dönüşüm.
module Game.Math
  ( V3 (..)
  , vx, vy, vz
  , v3, vadd, vsub, vneg, vscale, vdot, vcross, vlen, vlen2, vnorm, vnormOr
  , vlerp, vdist, vdist2, vxz, vmap
  , M4 (..), mId, mMul, mTrans, mScaleM, mRotX, mRotY, mRotZ, mApply, mApplyDir
  , clamp, lerpF, approach, approachV, damp, wrapAngle, angleLerp
  ) where

infixl 6 `vadd`
infixl 6 `vsub`

-- | 3 bileşenli vektör.
data V3 = V3 !Float !Float !Float
  deriving (Eq, Show)

instance Num V3 where
  V3 a b c + V3 x y z = V3 (a + x) (b + y) (c + z)
  V3 a b c - V3 x y z = V3 (a - x) (b - y) (c - z)
  V3 a b c * V3 x y z = V3 (a * x) (b * y) (c * z)
  negate (V3 a b c) = V3 (negate a) (negate b) (negate c)
  abs (V3 a b c) = V3 (abs a) (abs b) (abs c)
  signum (V3 a b c) = V3 (signum a) (signum b) (signum c)
  fromInteger n = V3 (fromInteger n) 0 0

v3 :: Float -> Float -> Float -> V3
v3 = V3

vx, vy, vz :: V3 -> Float
vx (V3 x _ _) = x
vy (V3 _ y _) = y
vz (V3 _ _ z) = z

vadd, vsub :: V3 -> V3 -> V3
vadd = (+)
vsub = (-)

vneg :: V3 -> V3
vneg = negate

vscale :: Float -> V3 -> V3
vscale s (V3 x y z) = V3 (s * x) (s * y) (s * z)

vdot :: V3 -> V3 -> Float
vdot (V3 a b c) (V3 x y z) = a * x + b * y + c * z

vcross :: V3 -> V3 -> V3
vcross (V3 a b c) (V3 x y z) =
  V3 (b * z - c * y) (c * x - a * z) (a * y - b * x)

vlen2 :: V3 -> Float
vlen2 v = vdot v v

vlen :: V3 -> Float
vlen = sqrt . vlen2

vnorm :: V3 -> V3
vnorm v =
  let l = vlen v
  in if l < 1.0e-8 then V3 0 0 0 else vscale (1 / l) v

-- | Sıfır vektörde verilen yönü döndürür.
vnormOr :: V3 -> V3 -> V3
vnormOr fallback v =
  let l = vlen v
  in if l < 1.0e-8 then fallback else vscale (1 / l) v

vlerp :: Float -> V3 -> V3 -> V3
vlerp t a b = a + vscale t (b - a)

vdist :: V3 -> V3 -> Float
vdist a b = vlen (a - b)

vdist2 :: V3 -> V3 -> Float
vdist2 a b = vlen2 (a - b)

-- | Y bileşenini sıfırlar (yatay düzlem).
vxz :: V3 -> V3
vxz (V3 x _ z) = V3 x 0 z

vmap :: (Float -> Float) -> V3 -> V3
vmap f (V3 x y z) = V3 (f x) (f y) (f z)

-- | Afin dönüşüm: sağ / yukarı / ileri eksenleri ve konum.
--   p' = right * x + up * y + fwd * z + pos
data M4 = M4 !V3 !V3 !V3 !V3
  deriving (Show)

mId :: M4
mId = M4 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) (V3 0 0 0)

mApplyDir :: M4 -> V3 -> V3
mApplyDir (M4 r u f _) (V3 x y z) = r * V3 x 0 0 + u * V3 0 y 0 + f * V3 0 0 z

mApply :: M4 -> V3 -> V3
mApply (M4 r u f p) (V3 x y z) = r * V3 x 0 0 + u * V3 0 y 0 + f * V3 0 0 z + p

-- | a * b : önce b, sonra a uygulanır.
mMul :: M4 -> M4 -> M4
mMul a b =
  M4 (mApplyDir a (mRight b))
     (mApplyDir a (mUp b))
     (mApplyDir a (mFwd b))
     (mApply a (mPos b))

mRight, mUp, mFwd, mPos :: M4 -> V3
mRight (M4 r _ _ _) = r
mUp (M4 _ u _ _) = u
mFwd (M4 _ _ f _) = f
mPos (M4 _ _ _ p) = p

mTrans :: V3 -> M4
mTrans p = M4 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) p

mScaleM :: Float -> Float -> Float -> M4
mScaleM x y z = M4 (V3 x 0 0) (V3 0 y 0) (V3 0 0 z) (V3 0 0 0)

mRotX :: Float -> M4
mRotX a = M4 (V3 1 0 0) (V3 0 c s) (V3 0 (-s) c) (V3 0 0 0)
  where (s, c) = (sin a, cos a)

mRotY :: Float -> M4
mRotY a = M4 (V3 c 0 (-s)) (V3 0 1 0) (V3 s 0 c) (V3 0 0 0)
  where (s, c) = (sin a, cos a)

mRotZ :: Float -> M4
mRotZ a = M4 (V3 c s 0) (V3 (-s) c 0) (V3 0 0 1) (V3 0 0 0)
  where (s, c) = (sin a, cos a)

clamp :: Float -> Float -> Float -> Float
clamp lo hi x = max lo (min hi x)

lerpF :: Float -> Float -> Float -> Float
lerpF t a b = a + t * (b - a)

-- | Hedefe sabit adımla yaklaş.
approach :: Float -> Float -> Float -> Float
approach maxDelta target cur = cur + clamp (-maxDelta) maxDelta (target - cur)

approachV :: Float -> V3 -> V3 -> V3
approachV maxDelta target cur =
  let d = target - cur
      l = vlen d
  in if l <= maxDelta || l < 1.0e-6
       then target
       else cur + vscale (maxDelta / l) d

-- | Üstel yumuşatma katsayısı (kare hızından bağımsız).
damp :: Float -> Float -> Float
damp rate dt = 1 - exp (negate rate * dt)

wrapAngle :: Float -> Float
wrapAngle a =
  let x = (a + pi) / (2 * pi)
      f = x - fromIntegral (floor x :: Int)
  in f * 2 * pi - pi

angleLerp :: Float -> Float -> Float -> Float
angleLerp t a b = a + t * wrapAngle (b - a)
