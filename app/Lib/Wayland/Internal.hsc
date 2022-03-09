{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Wayland.Internal (
    Wlr_LogImportance,
    Wl_Display,
    Wlr_Backend,
    wlrContext
) where

import Data.Map as Map

-- import Data.Monoid

import qualified Language.Haskell.TH as TH

import Language.C.Inline
import Language.C.Inline.Context
import qualified Language.C.Types as C

import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))

#include <wlr/util/log.h>
#include <wayland-server-core.h>

type Wlr_LogImportance = CInt

data Wl_Display

-- instance Storable Wl_Display where
--     sizeOf _ = (#size "struct wl_display")
--     alignment _ = alignment (undefined :: Ptr ())
--     peek = error "peek not implemented for Wl_Display"
--     poke _ _ = error "poke not implemented for wl_display"

data Wlr_Backend


wlrContext :: Context
wlrContext = baseCtx <> ctx
  where
    ctx =
        mempty
            { ctxTypesTable = wlrTypesTable
            }

wlrTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
wlrTypesTable =
    Map.fromList
        [ (C.Enum "wlr_log_importance", [t|Wlr_LogImportance|]),
          (C.Struct "wl_display", [t|Wl_Display|]),
          (C.Struct "wlr_backend", [t|Wlr_Backend|])
        ]

