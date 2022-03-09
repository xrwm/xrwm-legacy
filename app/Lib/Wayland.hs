{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Wayland where

import Language.C.Inline
import Lib.Wayland.Internal
import Prelude hiding (exp)
import Foreign.Ptr
import Control.Exception

context wlrContext

include "<wlr/util/log.h>"
include "<wayland-server-core.h>"
include "<wlr/backend.h>"
include "<wlr/render/allocator.h>"
include "<wlr/render/wlr_renderer.h>"


data LogImportance = Silent | Error | Info | Debug
    deriving (Eq, Show, Read, Ord, Enum)

initLog :: LogImportance -> IO ()
initLog level = do
    let l = fromIntegral $ fromEnum level
    [exp| void { wlr_log_init($(int l), NULL); } |]

withWlDisplay :: (Ptr Wl_Display -> IO a) -> IO a
withWlDisplay action = do
    [exp| void { wlr_log_init(WLR_DEBUG, NULL) } |]

    display <- [exp| struct wl_display * { wl_display_create() } |]
    finally (action display) $ [exp| void { wl_display_destroy($(struct wl_display * display)) } |]

withWlrBackend :: Ptr Wl_Display -> (Ptr Wlr_Backend -> IO a) -> IO a
withWlrBackend display action = do
    backend <- [exp| struct wlr_backend * { wlr_backend_autocreate($(struct wl_display * display)) }|]
    finally (action backend) $ [exp| void { wlr_backend_destroy($(struct wlr_backend * backend)) } |]

