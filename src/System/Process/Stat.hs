{-# LANGUAGE ForeignFunctionInterface #-}

module System.Process.Stat
    ( ProcTimes(..)
    , prettyProcTimes
    , getProcTimes'
    , getProcTimes
    ) where

import Data.Time
import System.FilePath
import System.IO.Unsafe
import System.Posix.Types

import Foreign.C

statFile :: ProcessID -> FilePath
statFile pid = "/proc" </> show pid </> "stat"

data ProcTimes = ProcTimes
    { procUTime :: DiffTime
    , procSTime :: DiffTime
    , procCUTime :: DiffTime
    , procCSTime :: DiffTime
    } deriving (Show, Eq, Ord)

prettyProcTimes :: ProcTimes -> String
prettyProcTimes (ProcTimes u s cu cs) =
    unwords
        [ "user:"
        , show u
        , "sys:"
        , show s
        , "child-user:"
        , show cu
        , "child-sys:"
        , show cs
        ]

instance Semigroup ProcTimes where
    ProcTimes u s cu cs <> ProcTimes u' s' cu' cs' =
        ProcTimes (u + u') (s + s') (cu + cu') (cs + cs')

instance Monoid ProcTimes where
    mempty = ProcTimes 0 0 0 0

getProcTimes' :: ProcessID -> IO (Int, Int, Int, Int)
getProcTimes' pid = do
    xs <- words <$> readFile (statFile pid)
    pure $ (read (xs !! 13), read (xs !! 14), read (xs !! 15), read (xs !! 16))

getProcTimes :: ProcessID -> IO ProcTimes
getProcTimes pid = do
    (utime, stime, cutime, cstime) <- getProcTimes' pid
    let toDT x = fromIntegral x / fromIntegral scClkTck
    pure $ ProcTimes (toDT utime) (toDT stime) (toDT cutime) (toDT cstime)

foreign import ccall "sysconf" c_sysconf :: CInt -> IO CLong

-- | Result of @sysconf(_SC_CLK_TCK)@. The argument is hardcoded, if it changes
-- in the C library, this breaks! But pulling in c2hs for a single constant
-- seems excessive too.
scClkTck :: Int
scClkTck = fromIntegral $ unsafePerformIO (c_sysconf 2)

{-# NOINLINE scClkTck #-}
