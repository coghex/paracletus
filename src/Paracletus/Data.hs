{-# LANGUAGE Strict #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Paracletus.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import Numeric.DataFrame ( DataFrame, XN )
import Graphics.Vulkan hiding ( Window(..) )
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Artos.Var ( TVar )
import Epiklesis.Data ( Window(..), LinkAction(..) )
import Paracletus.Vulkan.Vertex ( Vertex )
import Paracletus.Vulkan.Data ( GQData )
import qualified Paracletus.Oblatum.GLFW as GLFW

-- possible graphics layers to use
data GraphicsLayer = GLUnknown | Vulkan | OpenGL | OpenGLES deriving (Show, Eq)
-- possible results of a paracletus evaluation
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError | VulkanSuccess | VulkanError deriving (Show, Eq)

-- fundamental vertex data
data Verts = Verts (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])

-- draw state is kept in a seperate thread
-- and calculated into verticies
data DrawState = DrawState { dsStatus  ∷ DSStatus
                           , dsTiles   ∷ [Tile]
                           , dsBuff    ∷ [Dyns]
                           , dsFPS     ∷ FPS
                           , dsNDefTex ∷ Int
                           , dsWins    ∷ [Window]
                           }

-- fps defined as actual and desired,
-- and whether or not to display
data FPS = FPS Double Int Bool deriving (Show, Eq)

-- mapping of buffer to tiles
data DynMap = DMBuff Int Int
            | DMFPS Int
            | DMSlider Int
            | DMSliderVal Int Int
            | DMNULL deriving (Show, Eq)

-- gtiles represent abstact tiles
data Tile = GTile { tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int
                  }
          | DTile { tDyn   ∷ DynMap
                  , tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int
                  } deriving (Show, Eq)

-- data for dynamic object transformations
data DynData = DynData
       { ddTex    ∷ Int
       , ddPos    ∷ (Float,Float)
       , ddScale  ∷ (Float,Float)
       , ddTIndex ∷ (Int,Int)
       } deriving (Show, Eq)

-- collection of dyndatas
data Dyns = Dyns [DynData] deriving (Show, Eq)

-- status of the loading thread, allowing
-- us to return results of deeply nested
-- pure functions
data DSStatus = DSSLogDebug String
              | DSSRecreate
              | DSSLoadVerts
              | DSSLoadInput LinkAction
              | DSSExit
              | DSSNULL deriving (Show, Eq)

-- result of the loading thread
data LoadResult = ResSuccess | ResError String | ResDrawState DrawState | ResNULL

-- all the data required for a set of textures
data TextureData = TextureData
         { descSetLayout  ∷ VkDescriptorSetLayout
         , pipelineLayout ∷ VkPipelineLayout
         , nimages        ∷ Int
         , descTexInfo    ∷ [VkDescriptorImageInfo]
         , depthFormat    ∷ VkFormat }

-- the main data for the vulkan loop
data VulkanLoopData = VulkanLoopData
         { gqdata             ∷ GQData
         , queues             ∷ DevQueues
         , scsd               ∷ SwapchainSupportDetails
         , window             ∷ GLFW.Window
         , vulkanSurface      ∷ VkSurfaceKHR
         , texData            ∷ TextureData
         , msaaSamples        ∷ VkSampleCountFlagBits
         , shaderVert         ∷ VkPipelineShaderStageCreateInfo
         , shaderFrag         ∷ VkPipelineShaderStageCreateInfo
         , imgIndexPtr        ∷ Ptr Word32
         , windowSizeChanged  ∷ TVar Bool
         , frameIndexRef      ∷ TVar Int
         , renderFinishedSems ∷ Ptr VkSemaphore
         , imageAvailableSems ∷ Ptr VkSemaphore
         , inFlightFences     ∷ Ptr VkFence }

data DevQueues = DevQueues { graphicsQueue  ∷ VkQueue
                           , presentQueue   ∷ VkQueue
                           , qFamIndices    ∷ Ptr Word32
                           , graphicsFamIdx ∷ Word32
                           , presentFamIdx  ∷ Word32
                           } deriving (Eq, Show)

data SwapchainSupportDetails = SwapchainSupportDetails
  { capabilities ∷ VkSurfaceCapabilitiesKHR
  , formats      ∷ [VkSurfaceFormatKHR]
  , presentModes ∷ [VkPresentModeKHR]
  } deriving (Eq, Show)
