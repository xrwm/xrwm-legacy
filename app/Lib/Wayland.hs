{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Wayland where

import Language.C.Inline
import Lib.Wayland.Internal
import Prelude hiding (exp)

context wlrContext

include "<wlr/util/log.h>"
include "<wayland-server-core.h>"
include "<wlr/backend.h>"
include "<wlr/render/allocator.h>"
include "<wlr/render/wlr_renderer.h>"

data LogImportance = Silent | Error | Info | Debug
    deriving (Enum)

initLog :: Wlr_LogImportance -> IO ()
initLog l = do
    [exp| void { wlr_log_init($(int l), NULL); } |]
    [exp| void { wlr_renderer_autocreate(wlr_backend_autocreate(wl_display_create())); } |]
