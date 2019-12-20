{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Reasoner.Tickless.Database
    ( PhaseChange(..)
    , MonadReasonerDB(..)
    , EventTreeT(..)
    , runEventTreeT
    , runEventTreeT'
    , T.TimeInterval(..)
    , T.Extended(..)
    , T.upperBounded
    ) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text)
import Data.Expr
import Data.Time.Clock

import qualified Data.EventTree as T
import qualified Language.LARS as LARS

data PhaseChange = Positive | Negative
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

class MonadReasonerDB m where
    type Connection m
    -- | Query the database for whether a given atom is always true inside a
    -- window.
    always :: UTCTime -> LARS.GroundBasicAtom -> LARS.Window -> m Bool
    -- | Query the database for whether a given atom is true at some point in
    -- the window. Note that this respects the entire interval over which the
    -- atom was observed (or considered) true, not just the moment it became
    -- true.
    happened :: UTCTime -> LARS.GroundBasicAtom -> LARS.Window -> m Bool
    -- | Query the database for all time points at which the atom /became/
    -- true.
    atW :: UTCTime -> LARS.GroundBasicAtom -> LARS.Window -> m [UTCTime]
    -- | Query the database for whether an atom is true at a given point in
    -- time.
    atT :: LARS.GroundBasicAtom -> UTCTime -> m Bool
    -- | Obtain a snapshot for replicating the database connection in the
    -- injector function
    snapshot :: m (Connection m)
    -- | Return the last occurrence of an atom.
    latest :: LARS.GroundBasicAtom -> m (Maybe T.TimeInterval)
    -- | Report new events given the current time and a list of events
    events ::
           Foldable t
        => UTCTime
        -> t (LARS.GroundBasicAtom, PhaseChange)
        -> m ()
    -- | Trigger a database cleanup given a limit timestamp
    cleanup :: UTCTime -> m ()

instance (Monad m, MonadReasonerDB m) => MonadReasonerDB (ExceptT e m) where
    type Connection (ExceptT e m) = Connection m
    always t a w = lift (always t a w)
    happened t a w = lift (happened t a w)
    atW t a w = lift (atW t a w)
    atT a t = lift (atT a t)
    snapshot = lift snapshot
    latest = lift . latest
    events t xs = lift (events t xs)
    cleanup = lift . cleanup

newtype EventTreeT m a = EventTreeT
    { unEventTreeT :: StateT (T.EventTree Text) m a
    } deriving ( Functor
               , Monad
               , Applicative
               , MonadIO
               , MonadMask
               , MonadCatch
               , MonadThrow
               , MonadLogger
               , MonadTrans
               )

instance MonadReader r m => MonadReader r (EventTreeT m) where
    ask = lift ask
    local f (EventTreeT k) = EventTreeT (local f k)

instance MonadState s m => MonadState s (EventTreeT m) where
    get = lift get
    put = lift . put
    state = lift . state

runEventTreeT :: Monad m => EventTreeT m a -> m a
runEventTreeT (EventTreeT k) = evalStateT k mempty

runEventTreeT' :: Monad m => T.EventTree Text -> EventTreeT m a -> m a
runEventTreeT' t (EventTreeT k) = evalStateT k t

instance Monad m => MonadReasonerDB (EventTreeT m) where
    type Connection (EventTreeT m) = T.EventTree Text
    always now a w =
        EventTreeT $
        T.always (windowInterval now w) (LARS.atomText . LARS.unground $ a) <$>
        get
    happened now a w =
        EventTreeT $
        T.happened (windowInterval now w) (LARS.atomText . LARS.unground $ a) <$>
        get
    atW now a w =
        EventTreeT $
        T.atW (windowInterval now w) (LARS.atomText . LARS.unground $ a) <$> get
    atT a t = EventTreeT $ T.atT t (LARS.atomText . LARS.unground $ a) <$> get
    snapshot = EventTreeT $ get
    latest a = EventTreeT $ T.latest (LARS.atomText . LARS.unground $ a) <$> get
    cleanup t = EventTreeT $ modify (T.restrict (T.TimeInterval t T.Infinite))
    events t es =
        forM_ es $ \(a, phase) ->
            case phase of
                Negative ->
                    EventTreeT $
                    modify (T.close t (LARS.atomText . LARS.unground $ a))
                Positive ->
                    EventTreeT $
                    modify (T.insert t (LARS.atomText . LARS.unground $ a))

windowInterval :: UTCTime -> LARS.Window -> T.TimeInterval
windowInterval now (LARS.SlidingTimeWindow spec (Constant x)) =
    let d = fromIntegral $ LARS.toSec spec (truncate x)
        s = addUTCTime (negate d) now
     in T.TimeInterval
            s
            (T.Simple now)
windowInterval _ _ = error "FATAL: Malformed window"
