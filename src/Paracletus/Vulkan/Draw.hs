{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Draw where
import Prelude()
import UPrelude
import Control.Monad (forM_, replicateM)
import Foreign.Ptr (castPtr)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
    ( (&*), createVk, set, setListCountAndRef, setListRef, setVk )
import Graphics.Vulkan.Marshal.Create.DataFrame ( setVec )
import Numeric.DataFrame ( Vector4(vec4) )
import Anamnesis ( MonadIO(liftIO), MonadState(get), Anamnesis )
import Anamnesis.Data ( State(stStatus) )
import Anamnesis.Foreign
    ( allocaPeek,
      mallocArrayRes,
      newArrayRes,
      peek,
      peekArray,
      ptrAtIndex )
import Anamnesis.Util ( allocResource )
import Artos.Except ( testEx )
import Artos.Var ( atomically, readTVar, writeTVar, TVar )
import Paracletus.Data ( DevQueues(..) )
import Paracletus.Vulkan.Data ( SwapchainInfo(..) )
import Paracletus.Vulkan.Foreign
    ( runVk, withVkArrayLen, withVkPtr )
import Paracletus.Vulkan.Sync ( createFence, createSemaphore )

_MAX_FRAMES_IN_FLIGHT ∷ Int
_MAX_FRAMES_IN_FLIGHT = 2

createFramebuffers ∷ VkDevice → VkRenderPass → SwapchainInfo → [VkImageView] → VkImageView → VkImageView → Anamnesis ε σ [VkFramebuffer]
createFramebuffers dev renderPass SwapchainInfo{swapExtent} swapImgViews depthImgView colorImgView = allocResource
    (liftIO ∘ mapM_ (\fb → vkDestroyFramebuffer dev fb VK_NULL))
    (mapM createFB swapImgViews)
  where createFB swapImgView =
          let fbci = createVk @VkFramebufferCreateInfo
                $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                &* set @"pNext" VK_NULL
                &* set @"flags" VK_ZERO_FLAGS
                &* set @"renderPass" renderPass
                &* setListCountAndRef @"attachmentCount" @"pAttachments" [colorImgView, depthImgView, swapImgView]
                &* set @"width" (getField @"width" swapExtent)
                &* set @"height" (getField @"height" swapExtent)
                &* set @"layers" 1
           in  allocaPeek $ \fbPtr → withVkPtr fbci $ \fbciPtr → runVk $ vkCreateFramebuffer dev fbciPtr VK_NULL fbPtr

createCommandBuffers ∷ VkDevice → VkPipeline → VkCommandPool → VkRenderPass → VkPipelineLayout → SwapchainInfo → VkBuffer → (Word32, VkBuffer) → [VkFramebuffer] → [VkDescriptorSet] → Anamnesis ε σ (Ptr VkCommandBuffer)
createCommandBuffers dev pipeline commandPool rpass pipelineLayout SwapchainInfo{swapExtent} vertexBuffer (nIndices, indexBuffer) fbs descriptorSets
  | buffersCount ← length fbs = do
  cbsPtr ← mallocArrayRes buffersCount
  vertexBufArr ← newArrayRes [vertexBuffer]
  vertexOffArr ← newArrayRes [0]
  allocResource
    (const $ liftIO $ vkFreeCommandBuffers dev commandPool (fromIntegral buffersCount) cbsPtr)
    $ do
   let allocInfo = createVk @VkCommandBufferAllocateInfo
         $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
         &* set @"pNext" VK_NULL
         &* set @"commandPool" commandPool
         &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
         &* set @"commandBufferCount" (fromIntegral buffersCount)
   withVkPtr allocInfo $ \aiPtr → runVk $ vkAllocateCommandBuffers dev aiPtr cbsPtr
   commandBuffers ← peekArray buffersCount cbsPtr
   forM_ (zip3 fbs descriptorSets commandBuffers) $ \(frameBuffer, descriptorSet, cmdBuffer) → do
     let cmdBufBeginInfo = createVk @VkCommandBufferBeginInfo
           $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
           &* set @"pNext" VK_NULL
           &* set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
     withVkPtr cmdBufBeginInfo $ runVk ∘ vkBeginCommandBuffer cmdBuffer
     let renderPassBeginInfo = createVk @VkRenderPassBeginInfo
           $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
           &* set @"pNext" VK_NULL
           &* set @"renderPass" rpass
           &* set @"framebuffer" frameBuffer
           &* setVk @"renderArea"
                ( setVk @"offset"
                  ( set @"x" 0 &* set @"y" 0 )
                &* set @"extent" swapExtent )
           &* setListCountAndRef @"clearValueCount" @"pClearValues"
               [ createVk @VkClearValue $ setVk @"color" $ setVec @"float32" (vec4 0 0 0.2 1)
               , createVk @VkClearValue $ setVk @"depthStencil"
                   $  set @"depth" 1.0
                   &* set @"stencil" 0 ]
     withVkPtr renderPassBeginInfo $ \rpibPtr → liftIO $ vkCmdBeginRenderPass cmdBuffer rpibPtr VK_SUBPASS_CONTENTS_INLINE
     liftIO $ vkCmdBindPipeline cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
     liftIO $ vkCmdBindVertexBuffers cmdBuffer 0 1 vertexBufArr vertexOffArr
     liftIO $ vkCmdBindIndexBuffer cmdBuffer indexBuffer 0 VK_INDEX_TYPE_UINT32
     dsPtr ← newArrayRes [descriptorSet]
     liftIO $ vkCmdBindDescriptorSets cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 dsPtr 0 VK_NULL
     -- this code freezes with no messages
     --let pcData = PushConstantData (vec4 0 0 0 0)
     --pcPtr ← newArrayRes [pcData]
     --liftIO $ vkCmdPushConstantsSafe cmdBuffer pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 0 (bSizeOf @PushConstantData undefined) (castPtr pcPtr)
     liftIO $ vkCmdDrawIndexed cmdBuffer nIndices 1 0 0 0
     liftIO $ vkCmdEndRenderPass cmdBuffer
     runVk $ vkEndCommandBuffer cmdBuffer
   return cbsPtr

createPushConstants ∷ Int → Anamnesis ε σ (Ptr Void)
createPushConstants r = do
  pcPtr ← newArrayRes [range]
  return $ castPtr pcPtr
  where range = (fromIntegral r) :: Word32

createFrameSemaphores ∷ VkDevice → Anamnesis ε σ (Ptr VkSemaphore)
createFrameSemaphores dev = newArrayRes ⌫ replicateM _MAX_FRAMES_IN_FLIGHT (createSemaphore dev)

createFrameFences ∷ VkDevice → Anamnesis ε σ (Ptr VkFence)
createFrameFences dev = newArrayRes ⌫ replicateM _MAX_FRAMES_IN_FLIGHT (createFence dev True)

data RenderData = RenderData
  { dev                ∷ VkDevice
  , swapInfo           ∷ SwapchainInfo
  , queues             ∷ DevQueues
  , imgIndexPtr        ∷ Ptr Word32
  , frameIndexRef      ∷ TVar Int
  , renderFinishedSems ∷ Ptr VkSemaphore
  , imageAvailableSems ∷ Ptr VkSemaphore
  , inFlightFences     ∷ Ptr VkFence
  , cmdBuffersPtr      ∷ Ptr VkCommandBuffer
  , memories           ∷ Ptr VkDeviceMemory
  , dynMemories        ∷ Ptr VkDeviceMemory
  , texMemories        ∷ Ptr VkDeviceMemory
  , memoryMutator      ∷ ∀ ε σ. VkDeviceMemory → Anamnesis ε σ ()
  , dynMemoryMutator   ∷ ∀ ε σ. VkDeviceMemory → Anamnesis ε σ ()
  , texMemoryMutator   ∷ ∀ ε σ. VkDeviceMemory → Anamnesis ε σ () }

drawFrame ∷ RenderData → Anamnesis ε σ Bool
drawFrame RenderData{..} = do
  frameIndex ← liftIO $ atomically $ readTVar frameIndexRef
  let inFlightFencePtr = inFlightFences `ptrAtIndex` frameIndex
  runVk $ vkWaitForFences dev 1 inFlightFencePtr VK_TRUE (maxBound ∷ Word64)
  let SwapchainInfo{..} = swapInfo
      DevQueues{..} = queues
  imageAvailable ← peek (imageAvailableSems `ptrAtIndex` frameIndex)
  renderFinished ← peek (renderFinishedSems `ptrAtIndex` frameIndex)
  inFlightFence ← peek inFlightFencePtr
  runVk $ vkAcquireNextImageKHR dev swapchain maxBound imageAvailable VK_NULL_HANDLE imgIndexPtr
  imgIndex ← fromIntegral ⊚ peek imgIndexPtr
  let bufPtr  = cmdBuffersPtr `ptrAtIndex` imgIndex
      memPtr  = memories `ptrAtIndex` imgIndex
      dmemPtr = dynMemories `ptrAtIndex` imgIndex
      tmemPtr = texMemories `ptrAtIndex` imgIndex
  mem ← peek memPtr
  memoryMutator mem
  dmem ← peek dmemPtr
  dynMemoryMutator dmem
  tmem ← peek tmemPtr
  texMemoryMutator tmem
  let submitInfo = [ createVk @VkSubmitInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
          &* set @"pNext" VK_NULL
          &* set @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores" [imageAvailable]
          &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          &* set @"commandBufferCount" 1
          &* set @"pCommandBuffers" bufPtr
          &* set @"signalSemaphoreCount" 1
          &* setListRef @"pSignalSemaphores" [renderFinished] ]
  runVk $ vkResetFences dev 1 inFlightFencePtr
  runVk ∘ withVkArrayLen submitInfo $ \aLen siPtr → liftIO $ vkQueueSubmit graphicsQueue aLen siPtr inFlightFence
  let presentInfo = createVk @VkPresentInfoKHR
        $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
        &* set @"pNext" VK_NULL
        &* set @"pImageIndices" imgIndexPtr
        &* set @"waitSemaphoreCount" 1
        &* setListRef @"pWaitSemaphores" [renderFinished]
        &* set @"swapchainCount" 1
        &* setListRef @"pSwapchains" [swapchain]
  liftIO $ atomically $ writeTVar frameIndexRef $ (frameIndex + 1) `mod` _MAX_FRAMES_IN_FLIGHT
  withVkPtr presentInfo $ runVk ∘ vkQueuePresentKHR presentQueue
  st ← get
  return $ testEx (stStatus st) VK_SUBOPTIMAL_KHR
  --(≡ VK_SUBOPTIMAL_KHR) ∘ currentStatus ⊚ get
