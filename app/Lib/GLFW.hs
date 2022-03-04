{-# LANGUAGE Strict #-}

module Lib.GLFW (
    withGLFWVulkanInstance,
    withGLFWWindow,
    glfwMainLoop,
) where

import Control.Exception
import Control.Monad (unless)
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan

import Lib.Utils
import Lib.Vulkan

withGLFWWindow :: Int -> Int -> String -> (GLFW.Window -> IO a) -> IO a
withGLFWWindow w h n action = do
    GLFW.init
        >>= flip
            unless
            (throwVKMsg "Failed to initialize GLFW.")

    flip finally (GLFW.terminate >> putStrLn "Terminated GLFW.") $ do
        GLFW.getVersionString >>= mapM_ (putStrLn . ("GLFW version: " ++))

        GLFW.vulkanSupported
            >>= flip
                unless
                (throwVKMsg "GLFW reports that vulkan is not supported!")

        GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
        GLFW.windowHint $ WindowHint'Resizable False

        mw <- GLFW.createWindow w h n Nothing Nothing
        case mw of
            Nothing -> throwVKMsg "Failed to initialize GLFW window."
            Just window -> do
                putStrLn "Initialized GLFW window."
                finally
                    (action window)
                    (GLFW.destroyWindow window >> putStrLn "Closed GLFW window.")

glfwMainLoop :: GLFW.Window -> IO () -> IO ()
glfwMainLoop w action = do
    should <- GLFW.windowShouldClose w
    unless should $ GLFW.pollEvents >> action >> go

withGLFWVulkanInstance :: String -> (VkInstance -> IO a) -> IO a
withGLFWVulkanInstance progName action = do
    glfwReqExts <- GLFW.getRequiredInstanceExtensions
    withVulkanInstance
        progName
        glfwReqExts
        ["VK_LAYER_LUNARG_standard_validation"]
        action

