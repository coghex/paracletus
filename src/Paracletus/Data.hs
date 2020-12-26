{-# LANGUAGE Strict #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Paracletus.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Numeric.DataFrame
import Paracletus.Vulkan.Vertex
import Artos.Var
import Paracletus.Vulkan.Data
import Paracletus.Vulkan.Vertex
import qualified Paracletus.Oblatum.GLFW as GLFW

data GraphicsLayer = GLUnknown | Vulkan | OpenGL | OpenGLES deriving (Show, Eq)
data ParacResult = ParacSuccess | ParacError | GLFWSuccess | GLFWError | VulkanSuccess | VulkanError deriving (Show, Eq)

-- fundamental vertex data
data Verts = VertsNULL | VertsDF (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])

-- data for dynamic object transformations
data DynData = DynData
         { ddRef      ∷ DDRef
         , ddPosition ∷ (Float,Float)
         , ddTIndex   ∷ (Int,Int)
         } deriving (Show, Eq)

-- corresponding reference to lua state
data DDRef = DDSlider Int | DDNULL deriving (Show, Eq)

-- fps defined as actual and desired,
-- and whether or not to display
data FPS = FPS Double Int Bool deriving (Show, Eq)

-- input data
data InputState = InputState { mouse1   ∷ Maybe (Float,Float)
                             , mouse2   ∷ Maybe (Float,Float)
                             , mouse3   ∷ Maybe (Float,Float)
                             , isElems  ∷ [InputElem]
                             , inpCap   ∷ Bool
                             , keyUp    ∷ Bool
                             , keyLeft  ∷ Bool
                             , keyDown  ∷ Bool
                             , keyRight ∷ Bool
                             , keyAccel ∷ (Float,Float)
                             } deriving (Show, Eq)

-- input state related to various winelems
data InputElem = IESlider Bool Int | IESelect Bool Int | IENULL deriving (Show, Eq)

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

-- no need to load in every concievable size
data TextSize = TextSize16px | TextSize30px deriving (Show, Eq)
-- TTF characters come in different sizes
-- and offsets, this is that data
data TTFData = TTFData
  { chIndex ∷ Int
  , chW     ∷ Double
  , chH     ∷ Double
  , chX     ∷ Double
  , chY     ∷ Double
  } deriving (Show, Eq)
-- these pattern matches translate to
-- a giant case statement where we find
-- the index and horizontal offset of
-- each character that is in the
-- standart tileset
fontIndex ∷ Char → (Int, Int)
fontIndex 'a'  = ( 0,0)
fontIndex 'b'  = ( 1,0)
fontIndex 'c'  = ( 2,0)
fontIndex 'd'  = ( 3,0)
fontIndex 'e'  = ( 4,0)
fontIndex 'f'  = ( 5,0)
fontIndex 'g'  = ( 6,0)
fontIndex 'h'  = ( 7,0)
fontIndex 'i'  = ( 8,0)
fontIndex 'j'  = ( 9,0)
fontIndex 'k'  = (10,0)
fontIndex 'l'  = (11,0)
fontIndex 'm'  = (12,0)
fontIndex 'n'  = (13,0)
fontIndex 'o'  = (14,0)
fontIndex 'p'  = (15,0)
fontIndex 'q'  = ( 0,1)
fontIndex 'r'  = ( 1,1)
fontIndex 's'  = ( 2,1)
fontIndex 't'  = ( 3,1)
fontIndex 'u'  = ( 4,1)
fontIndex 'v'  = ( 5,1)
fontIndex 'w'  = ( 6,1)
fontIndex 'x'  = ( 7,1)
fontIndex 'y'  = ( 8,1)
fontIndex 'z'  = ( 9,1)
fontIndex '?'  = (10,1)
fontIndex '!'  = (11,1)
fontIndex '('  = (12,1)
fontIndex ')'  = (13,1)
fontIndex '\'' = (14,1)
fontIndex '"'  = (15,1)
fontIndex 'A'  = ( 0,2)
fontIndex 'B'  = ( 1,2)
fontIndex 'C'  = ( 2,2)
fontIndex 'D'  = ( 3,2)
fontIndex 'E'  = ( 4,2)
fontIndex 'F'  = ( 5,2)
fontIndex 'G'  = ( 6,2)
fontIndex 'H'  = ( 7,2)
fontIndex 'I'  = ( 8,2)
fontIndex 'J'  = ( 9,2)
fontIndex 'K'  = (10,2)
fontIndex 'L'  = (11,2)
fontIndex 'M'  = (12,2)
fontIndex 'N'  = (13,2)
fontIndex 'O'  = (14,2)
fontIndex 'P'  = (15,2)
fontIndex 'Q'  = ( 0,3)
fontIndex 'R'  = ( 1,3)
fontIndex 'S'  = ( 2,3)
fontIndex 'T'  = ( 3,3)
fontIndex 'U'  = ( 4,3)
fontIndex 'V'  = ( 5,3)
fontIndex 'W'  = ( 6,3)
fontIndex 'X'  = ( 7,3)
fontIndex 'Y'  = ( 8,3)
fontIndex 'Z'  = ( 9,3)
fontIndex '.'  = (10,3)
fontIndex ':'  = (11,3)
fontIndex ','  = (12,3)
fontIndex ';'  = (13,3)
fontIndex '+'  = (14,3)
fontIndex '-'  = (15,3)
fontIndex '*'  = ( 0,4)
fontIndex '/'  = ( 1,4)
fontIndex '='  = ( 2,4)
fontIndex '1'  = ( 3,4)
fontIndex '2'  = ( 4,4)
fontIndex '3'  = ( 5,4)
fontIndex '4'  = ( 6,4)
fontIndex '5'  = ( 7,4)
fontIndex '6'  = ( 8,4)
fontIndex '7'  = ( 9,4)
fontIndex '8'  = (10,4)
fontIndex '9'  = (11,4)
fontIndex '0'  = (12,4)
fontIndex '%'  = (13,4)
fontIndex '&'  = (14,4)
--fontIndex ''   = (15,4)
fontIndex '`'  = ( 0,5)
fontIndex '~'  = ( 1,5)
fontIndex '@'  = ( 2,5)
fontIndex '#'  = ( 3,5)
fontIndex '$'  = ( 4,5)
fontIndex '^'  = ( 5,5)
fontIndex '_'  = ( 6,5)
fontIndex '\\' = ( 7,5)
fontIndex '|'  = ( 8,5)
fontIndex '<'  = ( 9,5)
fontIndex '>'  = (10,5)
fontIndex _    = (15,4)
fontOffset ∷ Char → Double
fontOffset 'a'  = 0.2
fontOffset 'b'  = 0.2
fontOffset 'c'  = 0.2
fontOffset 'd'  = 0.2
fontOffset 'e'  = 0.2
fontOffset 'f'  = 0.2
fontOffset 'g'  = 0.2
fontOffset 'h'  = 0.2
fontOffset 'i'  = 0.1
fontOffset 'j'  = 0.1
fontOffset 'k'  = 0.2
fontOffset 'l'  = 0.1
fontOffset 'm'  = 0.3
fontOffset 'n'  = 0.2
fontOffset 'o'  = 0.2
fontOffset 'p'  = 0.2
fontOffset 'q'  = 0.2
fontOffset 'r'  = 0.2
fontOffset 's'  = 0.2
fontOffset 't'  = 0.2
fontOffset 'u'  = 0.2
fontOffset 'v'  = 0.2
fontOffset 'w'  = 0.3
fontOffset 'x'  = 0.2
fontOffset 'y'  = 0.2
fontOffset 'z'  = 0.2
fontOffset '?'  = 0.2
fontOffset '!'  = 0.1
fontOffset '('  = 0.1
fontOffset ')'  = 0.1
fontOffset '\'' = 0.2
fontOffset '"'  = 0.1
fontOffset 'A'  = 0.3
fontOffset 'B'  = 0.3
fontOffset 'C'  = 0.3
fontOffset 'D'  = 0.3
fontOffset 'E'  = 0.3
fontOffset 'F'  = 0.3
fontOffset 'G'  = 0.3
fontOffset 'H'  = 0.3
fontOffset 'I'  = 0.2
fontOffset 'J'  = 0.3
fontOffset 'K'  = 0.3
fontOffset 'L'  = 0.3
fontOffset 'M'  = 0.3
fontOffset 'N'  = 0.3
fontOffset 'O'  = 0.3
fontOffset 'P'  = 0.3
fontOffset 'Q'  = 0.3
fontOffset 'R'  = 0.3
fontOffset 'S'  = 0.3
fontOffset 'T'  = 0.3
fontOffset 'U'  = 0.3
fontOffset 'V'  = 0.3
fontOffset 'W'  = 0.3
fontOffset 'X'  = 0.3
fontOffset 'Y'  = 0.3
fontOffset 'Z'  = 0.3
fontOffset '.'  = 0.05
fontOffset ':'  = 0.1
fontOffset ','  = 0.05
fontOffset ';'  = 0.1
fontOffset '+'  = 0.2
fontOffset '-'  = 0.2
fontOffset '*'  = 0.2
fontOffset '/'  = 0.1
fontOffset '='  = 0.2
fontOffset '1'  = 0.2
fontOffset '2'  = 0.2
fontOffset '3'  = 0.2
fontOffset '4'  = 0.2
fontOffset '5'  = 0.2
fontOffset '6'  = 0.2
fontOffset '7'  = 0.2
fontOffset '8'  = 0.2
fontOffset '9'  = 0.2
fontOffset '0'  = 0.2
fontOffset '%'  = 0.2
fontOffset '&'  = 0.2
--fontOffset ''   = 0.0
fontOffset '`'  = 0.2
fontOffset '~'  = 0.3
fontOffset '@'  = 0.2
fontOffset '#'  = 0.2
fontOffset '$'  = 0.2
fontOffset '^'  = 0.1
fontOffset '_'  = 0.2
fontOffset '\\' = 0.1
fontOffset '|'  = 0.1
fontOffset '<'  = 0.2
fontOffset '>'  = 0.2
fontOffset _    = 0.2
