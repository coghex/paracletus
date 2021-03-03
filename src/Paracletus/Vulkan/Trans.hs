{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Trans where
import Prelude()
import UPrelude
import Control.Monad (replicateM)
import Foreign.Ptr (castPtr,plusPtr)
import GHC.Generics (Generic)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Numeric.DataFrame
import Anamnesis
import Anamnesis.Foreign
import Paracletus.Vulkan.Foreign
import Paracletus.Vulkan.Buffer
import Paracletus.Data

data TransformationObject = TransformationObject
  { model ∷ Mat44f
  , view  ∷ Mat44f
  , proj  ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes TransformationObject

data DynTexTransObject = DynTexTransObject
  { dtexi ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes DynTexTransObject

data DynTransObject = DynTransObject
  { move ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes DynTransObject

updateTransObj ∷ (Float,Float,Float) → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransObj cam device extent uniBuf = do
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (bSizeOf @TransformationObject undefined) VK_ZERO_FLAGS
  let model = DF4
                (DF4 32 0 0 0)
                (DF4 0 32 0 0)
                (DF4 0 0 32 0)
                (DF4 0 0 0  1)
  poke (castPtr uboPtr) (scalar $ TransformationObject {..})
  liftIO $ vkUnmapMemory device uniBuf
  -- these commands are all backwards
  -- ortho near far w h
  where view = translate3 (vec3 x y z)
        (x,y,z) = cam
        proj  = proj' %* clip
        proj' = orthogonal (0.1) (500) (fromIntegral width) (fromIntegral height)
        --proj' = perspective 0.1 500 (45/360*2*pi) aspectRatio
        clip = DF4
          (DF4 1   0   0   0)
          (DF4 0 (-1)  0   0)
          (DF4 0   0  0.5  0)
          (DF4 0   0  0.5  1)
        width = getField @"width" extent
        height = getField @"height" extent
        --aspectRatio = fromIntegral width / fromIntegral height

updateTransDyn ∷ Int → [DynData] → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransDyn _    []       _      _      _      = return ()
updateTransDyn nDyn dyns device _      uniBuf = do
  let nDyn'   = (fromIntegral nDyn)
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (nDyn'*(bSizeOf @DynTransObject undefined)) VK_ZERO_FLAGS
  let updateTransDynFunc ∷ Int → [DynData] → Ptr α → Anamnesis ε σ ()
      updateTransDynFunc _    []       _        = return ()
      updateTransDynFunc nDyn0 (dd:dds) uboPtr0 = do
        let move = DF4
              (DF4 w 0 0 0)
              (DF4 0 h 0 0)
              (DF4 0 0 1 0)
              (DF4 x y 0 1)
            (x ,y)  = (realToFrac x', realToFrac y')
            (x',y') = ddPos dd
            (w ,h)  = (realToFrac w', realToFrac h')
            (w',h') = ddScale dd
            nDyn0'  = nDyn0 - 1
        poke (plusPtr (castPtr uboPtr0) (nDyn0'*(bSizeOf @DynTransObject undefined))) (scalar $ DynTransObject move)
        updateTransDynFunc nDyn0' dds uboPtr0
  updateTransDynFunc nDyn dyns uboPtr
  liftIO $ vkUnmapMemory device uniBuf

updateTransTex ∷ Int → [DynData] → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransTex _    []   _      _      _      = return ()
updateTransTex nDyn dyns device _      uniBuf = do
  let nDyn'   = (fromIntegral nDyn)
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (nDyn'*(bSizeOf @DynTexTransObject undefined)) VK_ZERO_FLAGS
  let updateTransTexFunc ∷ Int → [DynData] → Ptr α → Anamnesis ε σ ()
      updateTransTexFunc _     []       _       = return ()
      updateTransTexFunc nDyn0 (dd:dds) uboPtr0 = do
        let dtexi = DF4
              (DF4 1 0 0 0)
              (DF4 0 1 0 0)
              (DF4 0 0 1 0)
              (DF4 x y n 1)
            (x ,y)  = (fromIntegral x', fromIntegral y')
            (x',y') = ddTIndex dd
            n       = fromIntegral $ ddTex dd
            nDyn0'  = nDyn0 - 1
        poke (plusPtr (castPtr uboPtr0) (nDyn0'*(bSizeOf @DynTexTransObject undefined))) (scalar $ DynTexTransObject dtexi)
        updateTransTexFunc nDyn0' dds uboPtr0
  updateTransTexFunc nDyn dyns uboPtr
  liftIO $ vkUnmapMemory device uniBuf

createTransObjBuffers ∷ VkPhysicalDevice → VkDevice → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransObjBuffers pdev dev n = replicateM n $ createBuffer pdev dev (bSizeOf @TransformationObject undefined) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

transObjBufferInfo ∷ VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transObjBufferInfo uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (bSizeOf @TransformationObject undefined)

createTransDynBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransDynBuffers pdev dev n nDyn = replicateM n $ createBuffer pdev dev (nDyn'*(bSizeOf @DynTransObject undefined)) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

transDynBufferInfo ∷ Int → VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transDynBufferInfo nDyn uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (nDyn'*(bSizeOf @DynTransObject undefined))
  where nDyn' = max 1 $ fromIntegral nDyn

createTransTexBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransTexBuffers pdev dev n nDyn = replicateM n $ createBuffer pdev dev (nDyn'*(bSizeOf @DynTexTransObject undefined)) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

transTexBufferInfo ∷ Int → VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transTexBufferInfo nDyn uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (nDyn'*(bSizeOf @DynTexTransObject undefined))
  where nDyn' = max 1 $ fromIntegral nDyn
