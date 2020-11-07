{-# LANGUAGE RankNTypes #-}

module Morphism where

data NELF a = NELF Int (Maybe a)
instance Functor NELF where
  fmap _ (NELF x Nothing)   = NELF x Nothing
  fmap f (NELF x (Just xs)) = NELF x (Just (f xs))
newtype Fix f = InF { outF :: f (Fix f) }
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)
cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata phi = phi . fmap (cata phi) . outF
ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana psi = InF . fmap (ana psi) . psi
hylo :: Functor f => (f a -> a) -> (b -> f b) -> (b -> a)
hylo phi psi = phi . fmap (hylo phi psi) . psi
meta :: (Functor f, Functor g) => (f a -> a) -> (a -> b) -> (b -> g b) -> (Fix f -> Fix g)
meta phi chi psi = ana psi . chi . cata phi
para :: Functor f => (f (Fix f, a) -> a) -> (Fix f -> a)
para phi = phi . fmap ((,) <*> para phi) . outF
apo :: Functor f => (a -> f (Either (Fix f) a)) -> (a -> Fix f)
apo psi = InF . fmap (uncurry either (id, apo psi)) . psi
zygo :: Functor f => (f b -> b) -> (f (b, a) -> a) -> (Fix f -> a)
zygo phi phi' = snd . cata (pair (phi . fmap fst, phi'))
cozygo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> (b -> Fix f)
cozygo psi psi' = ana (uncurry either (fmap Left . psi, psi')) . Right
mutu :: Functor f => (a -> b) -> (f a -> a) -> (Fix f -> b)
mutu chi phi = chi . cata phi
comutu :: Functor f => (b -> a) -> (a -> f a) -> (b -> Fix f)
comutu chi psi = ana psi . chi
data Fx f a x =  Fx { unFx :: Either a (f x) }
data Hx f a x =  Hx { unHx :: (a, f x) }
instance Functor f => Functor (Fx f a) where
  fmap _ (Fx (Left x))  = Fx (Left x)
  fmap f (Fx (Right x)) = Fx (Right (fmap f x))
instance Functor f => Functor (Hx f a) where
  fmap f (Hx (x, y)) = Hx (x, fmap f y)
newtype Free f a = Free { unFree :: Fix (Fx f a) }
newtype CoFree f a = CoFree { unCoFree :: Fix (Hx f a) }
instance Functor f => Functor (Free f) where
  fmap f = Free . cata (InF . phi) . unFree
    where
      phi (Fx (Left a))  = Fx (Left (f a))
      phi (Fx (Right b)) = Fx (Right b)
instance Functor f => Functor (CoFree f) where
  fmap f = CoFree . ana (psi . outF) . unCoFree
    where
      psi (Hx (a, x)) = Hx (f a, x)
extract :: Functor f => CoFree f t -> t
extract cf = case outF (unCoFree cf) of
  Hx (a, _) -> a
sub :: Functor f => CoFree f a -> f (CoFree f a)
sub cf = case outF (unCoFree cf) of
  Hx (_, b) -> fmap CoFree b
inject :: Functor f => a -> Free f a
inject = Free . InF . Fx . Left
histo :: Functor f => (f (CoFree f t) -> t) -> (Fix f -> t)
histo phi = extract . cata (CoFree . InF . fmap unCoFree . Hx . pair (phi, id))
futu :: Functor f => (t -> f (Free f t)) -> (t -> Fix f)
futu psi = ana (uncurry either (psi, id) . unFx . fmap Free . outF . unFree) . inject
chrono :: Functor f => (f (CoFree f b) -> b) -> (a -> f (Free f a)) -> (a -> b)
chrono phi psi = extract . hylo phi' psi' . inject
  where
    phi' = CoFree . InF . fmap unCoFree . Hx . pair (phi, id)
    psi' = uncurry either (psi, id) . unFx . fmap Free . outF . unFree
cochrono :: Functor f => (f (CoFree f t) -> t) -> (t -> f (Free f t)) -> (Fix f -> Fix f)
cochrono phi psi = futu psi . histo phi
dyna :: Functor f => (f (CoFree f b) -> b) -> (a -> f a) -> (a -> b)
dyna phi psi = chrono phi (fmap inject . psi)
codyna :: Functor f => (f b -> b) -> (a -> f (Free f a)) -> (a -> b)
codyna phi psi = cata phi . futu psi
exo :: Functor h => (m b -> b, b -> n b) -> (h b -> m b) -> (h a -> h (g a)) -> (f a -> a, g a -> h a) -> (g a -> b)
exo c f g d = hylo (fst c . f) (g . snd d)
prepro :: Functor f => (forall a. f a -> f a) -> (f a -> a) -> (Fix f -> a)
prepro chi phi = phi . fmap (prepro chi phi . cata (InF . chi)) . outF
postpro :: Functor f => (forall a. f a -> f a) -> (a -> f a) -> (a -> Fix f)
postpro chi psi = InF . fmap (ana (chi . outF) . postpro chi psi) . psi
mcata :: (forall b. (b -> a) -> f b -> a) -> (Fix f -> a)
mcata phi = phi (mcata phi) . outF
mana :: (forall b. (a -> b) -> a -> f b) -> (a -> Fix f)
mana psi = InF . psi (mana psi)
mhisto :: (forall b. (b -> a) -> (b -> f b) -> f b -> a) -> (Fix f -> a)
mhisto psi = psi (mhisto psi) outF . outF