{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | an IO effect. not using generic monadic lift.
module IOEffect where

import Control.Monad.Freer
import Control.Monad.Freer.Internal

data IOEffect a where
  IOEffect :: IO a -> IOEffect a

lameRunIO :: Eff ('[IOEffect]) w -> IO w
lameRunIO = error "NOTIMPL"

-- interpreter for IO effects (and no other
-- effects lower in the stack) ... looks like
-- a free monad interpreter (and in fact, is)
-- but with that decomp layer in there.
runIO :: Eff ('[IOEffect]) w -> IO w
runIO = loop
  where
    loop (Val x) = putStrLn "runIO: Encountered a pure value." >> return x
    loop (E u' q) = case decomp u' of
      Left _ -> error "impossible: decomposed to non-IOEffect"
      Right (IOEffect act) -> do
        putStrLn "runIO: IO effect"
        v <- act
        -- we have a continuation queue, q, which is an arrow from a->b
        -- where v :: a
        -- handleRelay forms the new loop by providing a continuation k
        -- where k = qComp q loop, which will take an a and do something,
        -- followed by loop (? or prefixed by?)
        -- but the types dont' work here, because loop does not end as
        -- and effectful type...
        -- what we need to do to loop is somehow evaluate apply v to q
        -- to give us the next effectful stuff?
        -- and then loop in IO?
        -- we can apply v to q
        let k = q `qApp` v
        r <- loop k
        putStrLn "runIO: end of this interpreter loop" -- this kills tail call elimination, presumably...
        return r

doIO :: Member IOEffect r => IO a -> Eff r a
doIO act = send (IOEffect act)
