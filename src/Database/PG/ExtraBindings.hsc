{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns               #-}

module Database.PG.ExtraBindings (unsafeClampInOutBufferSpace) where

import Database.PostgreSQL.LibPQ.Internal 
import Foreign.Ptr

#include <libpq-fe.h>
#include <libpq/libpq-fs.h>
#include "libpq-bindings.h"


-- | libpq grows the buffers it uses for input and output, as needed,
-- exponentially, but it never shrinks them. This means that if we try to keep
-- connections open in a thread pool for as long as possible we should expect
-- RES to grow and grow as e.g. a new biggest result query comes in.
--
-- This function shrinks buffers back down to an acceptable size if necessary,
-- and should be called at least periodically on long-lived connections.
--
-- This is safe to call at the point just before we release a connection back
-- to the pool, and should be considered unsafe in other situations.
--
-- See libpq-bindings.c for details.
unsafeClampInOutBufferSpace :: Connection -> IO ()
unsafeClampInOutBufferSpace = flip withConn c_PQclampInOutBufferSpace

foreign import ccall        "libpq-bindings.h PQclampInOutBufferSpace"
    c_PQclampInOutBufferSpace :: Ptr PGconn -> IO ()
