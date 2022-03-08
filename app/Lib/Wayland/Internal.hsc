{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Wayland.Internal (
    Wlr_LogImportance,
    wlrContext,
) where

import Data.Map as Map

-- import Data.Monoid

import qualified Language.Haskell.TH as TH

import Language.C.Inline
import Language.C.Inline.Context
import qualified Language.C.Types as C

#include <wlr/util/log.h>

type Wlr_LogImportance = CInt

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
        [ (C.TypeName "wlr_log_importance", [t|Wlr_LogImportance|])
        ]
