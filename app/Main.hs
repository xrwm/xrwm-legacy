module Main (main) where

import Control.Exception
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

import Lib.Utils
import Lib.Vulkan (withVulkanInstance)

import Lib.Wayland

main :: IO ()
main = do
    initLog 3

mainGLFW :: IO ()
mainGLFW = withGLFWWindow $ \window ->
    withVulkanInstanceExt $ \vulkanInstance -> do
        dev <- pickPhysicalDevice vulkanInstance
        putStrLn $ "Selected device: " ++ show dev
        glfwMainLoop window (return ())

withGLFWWindow :: (GLFW.Window -> IO ()) -> IO ()
withGLFWWindow action = do
    GLFW.init
        >>= flip
            unless
            (throwVKMsg "Failed to initialize GLFW.")

    -- even if something bad happens, we need to terminate GLFW
    flip finally (GLFW.terminate >> putStrLn "Terminated GLFW.") $ do
        GLFW.getVersionString >>= mapM_ (putStrLn . ("GLFW version: " ++))

        GLFW.vulkanSupported
            >>= flip
                unless
                (throwVKMsg "GLFW reports that vulkan is not supported!")

        GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
        GLFW.windowHint $ WindowHint'Resizable False

        mw <- GLFW.createWindow 420 420 "Vulkan window" Nothing Nothing
        case mw of
            Nothing -> throwVKMsg "Failed to initialize GLFW window."
            Just w -> do
                putStrLn "Initialized GLFW window."
                finally
                    (action w)
                    (GLFW.destroyWindow w >> putStrLn "Closed GLFW window.")

glfwMainLoop :: GLFW.Window -> IO () -> IO ()
glfwMainLoop w action = go
  where
    go = do
        should <- GLFW.windowShouldClose w
        unless should $ GLFW.pollEvents >> action >> go

withVulkanInstanceExt :: (VkInstance -> IO ()) -> IO ()
withVulkanInstanceExt action = do
    glfwReqExts <- GLFW.getRequiredInstanceExtensions
    withVulkanInstance
        "02-GLFWWindow"
        glfwReqExts
        ["VK_LAYER_KHRONOS_validation"]
        action

pickPhysicalDevice :: VkInstance -> IO VkPhysicalDevice
pickPhysicalDevice vkInstance = do
    devs <- alloca $ \deviceCountPtr -> do
        throwingVK "pickPhysicalDevice: Failed to enumerate physical devices." $
            vkEnumeratePhysicalDevices vkInstance deviceCountPtr VK_NULL_HANDLE
        devCount <- fromIntegral <$> peek deviceCountPtr
        when (devCount <= 0) $ throwVKMsg "Zero device count!"
        putStrLn $ "Found " ++ show devCount ++ " devices."

        allocaArray devCount $ \devicesPtr -> do
            throwingVK "pickPhysicalDevice: Failed to enumerate physical devices." $
                vkEnumeratePhysicalDevices vkInstance deviceCountPtr devicesPtr
            peekArray devCount devicesPtr

    selectFirstSuitable devs
  where
    selectFirstSuitable [] = throwVKMsg "No suitable devices!"
    selectFirstSuitable (x : xs) =
        isDeviceSuitable x >>= \yes ->
            if yes
                then pure x
                else selectFirstSuitable xs

isDeviceSuitable :: VkPhysicalDevice -> IO Bool
isDeviceSuitable _ = pure True
