{-# LANGUAGE Strict #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Paracletus.Data where
-- some general data structures are defined
import Prelude()
import UPrelude

data GraphicsLayer = GLUnknown | Vulkan | OpenGL | OpenGLES deriving (Show, Eq)
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError | VulkanSuccess | VulkanError deriving (Show, Eq)

