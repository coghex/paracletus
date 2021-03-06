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

data DynTexTransObject = DynTexTransObject
  { dtexi ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes DynTexTransObject

data DynTransObject = DynTransObject
  { move ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes DynTransObject

data CamTexTransObject = CamTexTransObject
  { ctexi ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes CamTexTransObject

data CamTransObject = CamTransObject
  { cmov ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes CamTransObject

data AuxTexTransObject = AuxTexTransObject
  { atexi ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes AuxTexTransObject

data AuxTransObject = AuxTransObject
  { amov ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes AuxTransObject

data TransformationObject = TransformationObject
  { model ∷ Mat44f
  , view  ∷ Mat44f
  , proj  ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes TransformationObject

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
            (x',y') = ddPosition dd
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

updateTransCam ∷ Int → [DynData] → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransCam _    []   _      _      _      = return ()
updateTransCam nDyn dyns device _      uniBuf = do
  let nDyn'   = (fromIntegral nDyn)
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (nDyn'*(bSizeOf @CamTransObject undefined)) VK_ZERO_FLAGS
  let updateTransCamFunc ∷ Int → [DynData] → Ptr α → Anamnesis ε σ ()
      updateTransCamFunc _     []       _       = return ()
      updateTransCamFunc nDyn0 (dd:dds) uboPtr0 = do
        let cmov = DF4
                      (DF4 w 0 0 0)
                      (DF4 0 h 0 0)
                      (DF4 0 0 1 0)
                      (DF4 x y 0 1)
            (x ,y)  = (realToFrac x', realToFrac y')
            (x',y') = ddPosition dd
            (w ,h)  = (realToFrac w', realToFrac h')
            (w',h') = ddScale dd
            nDyn0'  = nDyn0 - 1
        poke (plusPtr (castPtr uboPtr0) (nDyn0'*(bSizeOf @CamTransObject undefined))) (scalar $ CamTransObject cmov)
        updateTransCamFunc nDyn0' dds uboPtr0
  updateTransCamFunc nDyn dyns uboPtr
  liftIO $ vkUnmapMemory device uniBuf

updateTransCamTex ∷ Int → [DynData] → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransCamTex _    []   _      _      _      = return ()
updateTransCamTex nDyn dyns device _      uniBuf = do
  let nDyn'   = (fromIntegral nDyn)
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (nDyn'*(bSizeOf @CamTexTransObject undefined)) VK_ZERO_FLAGS
  let updateTransCamTexFunc ∷ Int → [DynData] → Ptr α → Anamnesis ε σ ()
      updateTransCamTexFunc _     []       _       = return ()
      updateTransCamTexFunc nDyn0 (dd:dds) uboPtr0 = do
        let ctexi = DF4
                       (DF4 1 0 0 0)
                       (DF4 0 1 0 0)
                       (DF4 0 0 1 0)
                       (DF4 x y n 1)
            (x ,y)  = (fromIntegral x', fromIntegral y')
            (x',y') = ddTIndex dd
            n       = fromIntegral $ ddTex dd
            nDyn0'  = nDyn0 - 1
        poke (plusPtr (castPtr uboPtr0) (nDyn0'*(bSizeOf @CamTexTransObject undefined))) (scalar $ CamTexTransObject ctexi)
        updateTransCamTexFunc nDyn0' dds uboPtr0
  updateTransCamTexFunc nDyn dyns uboPtr
  liftIO $ vkUnmapMemory device uniBuf

updateTransAux ∷ Int → [DynData] → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransAux _    []   _      _      _      = return ()
updateTransAux nDyn dyns device _      uniBuf = do
  let nDyn'   = (fromIntegral nDyn)
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (nDyn'*(bSizeOf @AuxTransObject undefined)) VK_ZERO_FLAGS
  let updateTransAuxFunc ∷ Int → [DynData] → Ptr α → Anamnesis ε σ ()
      updateTransAuxFunc _     []       _       = return ()
      updateTransAuxFunc nDyn0 (dd:dds) uboPtr0 = do
        let cmov = DF4
                      (DF4 w 0 0 0)
                      (DF4 0 h 0 0)
                      (DF4 0 0 1 0)
                      (DF4 x y 0 1)
            (x ,y)  = (realToFrac x', realToFrac y')
            (x',y') = ddPosition dd
            (w ,h)  = (realToFrac w', realToFrac h')
            (w',h') = ddScale dd
            nDyn0'  = nDyn0 - 1
        poke (plusPtr (castPtr uboPtr0) (nDyn0'*(bSizeOf @AuxTransObject undefined))) (scalar $ AuxTransObject cmov)
        updateTransAuxFunc nDyn0' dds uboPtr0
  updateTransAuxFunc nDyn dyns uboPtr
  liftIO $ vkUnmapMemory device uniBuf

updateTransAuxTex ∷ Int → [DynData] → VkDevice → VkExtent2D → VkDeviceMemory → Anamnesis ε σ ()
updateTransAuxTex _    []   _      _      _      = return ()
updateTransAuxTex nDyn dyns device _      uniBuf = do
  let nDyn'   = (fromIntegral nDyn)
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0 (nDyn'*(bSizeOf @AuxTexTransObject undefined)) VK_ZERO_FLAGS
  let updateTransAuxTexFunc ∷ Int → [DynData] → Ptr α → Anamnesis ε σ ()
      updateTransAuxTexFunc _     []       _       = return ()
      updateTransAuxTexFunc nDyn0 (dd:dds) uboPtr0 = do
        let ctexi = DF4
                       (DF4 1 0 0 0)
                       (DF4 0 1 0 0)
                       (DF4 0 0 1 0)
                       (DF4 x y n 1)
            (x ,y)  = (fromIntegral x', fromIntegral y')
            (x',y') = ddTIndex dd
            n       = fromIntegral $ ddTex dd
            nDyn0'  = nDyn0 - 1
        poke (plusPtr (castPtr uboPtr0) (nDyn0'*(bSizeOf @AuxTexTransObject undefined))) (scalar $ AuxTexTransObject ctexi)
        updateTransAuxTexFunc nDyn0' dds uboPtr0
  updateTransAuxTexFunc nDyn dyns uboPtr
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

createTransCamBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransCamBuffers pdev dev n nDyn = replicateM n $ createBuffer pdev dev (nDyn'*(bSizeOf @CamTransObject undefined)) VK_BUFFER_USAGE_STORAGE_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

transCamBufferInfo ∷ Int → VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transCamBufferInfo nDyn uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (nDyn'*(bSizeOf @CamTransObject undefined))
  where nDyn' = max 1 $ fromIntegral nDyn

createTransCamTexBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransCamTexBuffers pdev dev n nDyn = replicateM n $ createBuffer pdev dev (nDyn'*(bSizeOf @CamTexTransObject undefined)) VK_BUFFER_USAGE_STORAGE_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

transCamTexBufferInfo ∷ Int → VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transCamTexBufferInfo nDyn uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (nDyn'*(bSizeOf @CamTexTransObject undefined))
  where nDyn' = max 1 $ fromIntegral nDyn

createTransAuxBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransAuxBuffers pdev dev n nDyn = replicateM n $ createBuffer pdev dev (nDyn'*(bSizeOf @AuxTransObject undefined)) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

transAuxBufferInfo ∷ Int → VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transAuxBufferInfo nDyn uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (nDyn'*(bSizeOf @AuxTransObject undefined))
  where nDyn' = max 1 $ fromIntegral nDyn

createTransAuxTexBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int → Anamnesis ε σ [(VkDeviceMemory, VkBuffer)]
createTransAuxTexBuffers pdev dev n nDyn = replicateM n $ createBuffer pdev dev (nDyn'*(bSizeOf @AuxTexTransObject undefined)) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

transAuxTexBufferInfo ∷ Int → VkBuffer → Anamnesis ε σ VkDescriptorBufferInfo
transAuxTexBufferInfo nDyn uniformBuffer = return $ createVk @VkDescriptorBufferInfo
  $  set @"buffer" uniformBuffer
  &* set @"offset" 0
  &* set @"range" (nDyn'*(bSizeOf @AuxTexTransObject undefined))
  where nDyn' = max 1 $ fromIntegral nDyn
