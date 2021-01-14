{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan
  ( runParacVulkan ) where
-- vulkan specific calls are made
import Prelude()
import UPrelude
import Control.Concurrent (forkIO)
import Control.Monad (forM_, when)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Reader.Class (asks)
import Data.List (zip7)
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Anamnesis
import Anamnesis.Data
    ( Env(envLoadQ, envLoadCh),
      LoopControl(..),
      ReloadState(RSNULL, RSRecreate),
      State(stVerts, stFPS, stAuxData, stCamData, stDynData, stCam,
            stTick, stNDefTex, stModTexs, stReload, stWindow) )
import Anamnesis.Event ( processEvents )
import Anamnesis.Foreign ( mallocRes, newArrayRes )
import Anamnesis.Util
    ( getTime, logDebug, logExcept, logInfo, loop )
import Artos.Data
    ( LoadCmd(LoadCmdSetFPS, LoadCmdSetNDefTex), TState(TStart) )
import Artos.Except ( testEx, ExType(ExParacletus) )
import Artos.Queue ( writeChan, writeQueue )
import Artos.Var
    ( atomically, modifyTVar', newTVar, readTVar, writeTVar )
import Epiklesis ( loadEpiklesis )
import Epiklesis.Data
    ( DynMap(DMNULL), Tile(ATile, DMTile, GTile) )
import Paracletus.Data
    ( DevQueues(graphicsQueue),
      Dyns(Dyns),
      FPS(FPS),
      GraphicsLayer(Vulkan),
      ParacResult(ParacError),
      TextureData(pipelineLayout, depthFormat, descTexInfo,
                  descSetLayout, nimages),
      Verts(VertsNULL, VertsDF),
      VulkanLoopData(..) )
import Paracletus.Load ( loadParacletus )
import Paracletus.Vulkan.Buffer
    ( createIndexBuffer, createVertexBuffer )
import Paracletus.Vulkan.Calc ( calcVertices )
import Paracletus.Vulkan.Command ( createCommandPool )
import Paracletus.Vulkan.Data
    ( GQData(GQData),
      SwapchainInfo(swapExtent, swapImgFormat, swapImgs) )
import Paracletus.Vulkan.Desc
    ( createDescriptorPool,
      createDescriptorSets,
      prepareDescriptorSet )
import Paracletus.Vulkan.Device
    ( createGraphicsDevice,
      getMaxUsableSampleCount,
      pickPhysicalDevice,
      querySwapchainSupport )
import Paracletus.Vulkan.Draw
    ( createCommandBuffers,
      createFrameFences,
      createFrameSemaphores,
      createFramebuffers,
      drawFrame,
      RenderData(RenderData, auxTexMemoryMutator, auxMemoryMutator,
                 camTexMemoryMutator, camMemoryMutator, texMemoryMutator,
                 dynMemoryMutator, memoryMutator, auxTexMemories, auxMemories,
                 camTexMemories, camMemories, texMemories, dynMemories, memories,
                 cmdBuffersPtr, inFlightFences, imageAvailableSems,
                 renderFinishedSems, frameIndexRef, imgIndexPtr, queues, swapInfo,
                 dev) )
import Paracletus.Vulkan.Foreign ( runVk )
import Paracletus.Vulkan.Instance ( createGLFWVulkanInstance )
import Paracletus.Vulkan.Load ( loadVulkanTextures )
import Paracletus.Vulkan.Pres ( createSurface, createSwapchain )
import Paracletus.Vulkan.Pipeline
    ( createGraphicsPipeline, createRenderPass )
import Paracletus.Vulkan.Shader ( makeShader )
import Paracletus.Vulkan.Texture
    ( createColorAttImgView, createDepthAttImgView, createImageView )
import Paracletus.Vulkan.Trans
import Paracletus.Vulkan.Vertex ( dfLen, vertIADs, vertIBD )
import Paracletus.Oblatum
    ( getCurTick,
      glfwMainLoop,
      glfwWaitEventsMeanwhile,
      initGLFWWindow,
      loadLoop,
      processInput )
import Paracletus.Oblatum.GLFW (pollEvents)

runParacVulkan ∷ Anamnesis ε σ ()
runParacVulkan = do
  windowSizeChanged ← liftIO $ atomically $ newTVar False
  logInfo $ "beginning paracletus"
  window ← initGLFWWindow 1280 720 "paracletus" windowSizeChanged
  modify $ \s → s { stWindow = Just window }
  vulkanInstance ← createGLFWVulkanInstance "paracletus-instance"
  vulkanSurface ← createSurface vulkanInstance window
  -- forks GLFW as parent
  glfwWaitEventsMeanwhile $ do
    (_, pdev)    ← pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    msaaSamples  ← getMaxUsableSampleCount pdev
    (dev,queues) ← createGraphicsDevice pdev vulkanSurface
    (shaderVert, shaderFrag) ← makeShader dev
    frameIndexRef ← liftIO $ atomically $ newTVar 0
    renderFinishedSems ← createFrameSemaphores dev
    imageAvailableSems ← createFrameSemaphores dev
    inFlightFences     ← createFrameFences     dev
    commandPool        ← createCommandPool     dev queues
    imgIndexPtr ← mallocRes
    let gqdata = GQData pdev dev commandPool (graphicsQueue queues)
    texData ← loadVulkanTextures gqdata []
    env ← ask
    -- CHILD THREADS
    -- epiklesis is the lua engine
    _ ← liftIO $ forkIO $ loadEpiklesis env
    -- paracletus loads drawstate into verticies
    _ ← liftIO $ forkIO $ loadParacletus env Vulkan
    liftIO $ atomically $ writeChan (envLoadCh env) TStart
    -- swapchain recreation loop
    loop $ do
      newSt ← get
      firstTick ← liftIO getCurTick
      scsd ← querySwapchainSupport pdev vulkanSurface
      recr ← gets stReload
      case recr of
        -- recreation loads new textures
        RSRecreate → do
          let modTexs = stModTexs newSt
          --logDebug $ "creating swapchain.."
          newTexData ← loadVulkanTextures gqdata modTexs
          liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdSetNDefTex $ stNDefTex newSt
          modify $ \s → s { stReload = RSNULL
                          , stTick   = Just firstTick }
          let vulkLoopData' = VulkanLoopData {..}
              vulkLoopData  = vulkLoopData' { texData = newTexData }
          vulkLoop vulkLoopData
        _ → do
          let vulkLoopData = VulkanLoopData {..}
          vulkLoop vulkLoopData
            
vulkLoop ∷ VulkanLoopData → Anamnesis ε σ (LoopControl)
vulkLoop (VulkanLoopData (GQData pdev dev commandPool _) queues scsd window vulkanSurface texData msaaSamples shaderVert shaderFrag imgIndexPtr windowSizeChanged frameIndexRef renderFinishedSems imageAvailableSems inFlightFences) = do
  swapInfo ← createSwapchain dev scsd queues vulkanSurface
  let swapchainLen = length (swapImgs swapInfo)
  -- BUFFERS
  -- obj buffer contains global trans matricies
  (transObjMems, transObjBufs) ← unzip ⊚ createTransObjBuffers pdev dev swapchainLen
  transObjMemories ← newArrayRes transObjMems
  descriptorBufferInfos ← mapM transObjBufferInfo transObjBufs
  -- vulkan shaders dont allow dynamic ubo arrays
  let nDynObjs = 1000
      nCamObjs = 10000
      nAuxObjs = 1000
  -- dynamic object matricies stack on the global
  -- they all behave the same,
  -- the names are meaningless
  (transDynMems, transDynBufs) ← unzip ⊚ createTransDynBuffers pdev dev swapchainLen nDynObjs
  dynDescBufInfos    ← mapM (transDynBufferInfo nDynObjs) transDynBufs
  transDynMemories ← newArrayRes transDynMems
  (transTexMems, transTexBufs) ← unzip ⊚ createTransTexBuffers pdev dev swapchainLen nDynObjs
  dynTexDescBufInfos ← mapM (transTexBufferInfo nDynObjs) transTexBufs
  transTexMemories ← newArrayRes transTexMems
  -- camera buffer
  (transCamMems, transCamBufs) ← unzip ⊚ createTransCamBuffers pdev dev swapchainLen nCamObjs
  camDescBufInfos    ← mapM (transCamBufferInfo nCamObjs) transCamBufs
  transCamMemories ← newArrayRes transCamMems
  (transCamTexMems, transCamTexBufs) ← unzip ⊚ createTransCamTexBuffers pdev dev swapchainLen nCamObjs
  camTexDescBufInfos ← mapM (transCamTexBufferInfo nCamObjs) transCamTexBufs
  transCamTexMemories ← newArrayRes transCamTexMems
  -- auxilary buffer
  (transAuxMems, transAuxBufs) ← unzip ⊚ createTransAuxBuffers pdev dev swapchainLen nAuxObjs
  auxDescBufInfos    ← mapM (transAuxBufferInfo nAuxObjs) transAuxBufs
  transAuxMemories ← newArrayRes transAuxMems
  (transAuxTexMems, transAuxTexBufs) ← unzip ⊚ createTransAuxTexBuffers pdev dev swapchainLen nAuxObjs
  auxTexDescBufInfos ← mapM (transAuxTexBufferInfo nAuxObjs) transAuxTexBufs
  transAuxTexMemories ← newArrayRes transAuxTexMems
  -- DESCRIPTOR POOL
  descriptorPool ← createDescriptorPool dev swapchainLen (nimages texData)
  descriptorSetLayouts ← newArrayRes $ replicate swapchainLen $ descSetLayout texData
  descriptorSets ← createDescriptorSets dev descriptorPool swapchainLen descriptorSetLayouts
  let bufInfos = zip7 descriptorBufferInfos dynDescBufInfos dynTexDescBufInfos camDescBufInfos camTexDescBufInfos auxDescBufInfos auxTexDescBufInfos
  forM_ (zip bufInfos descriptorSets) $ \((bufInfo, dynBufInfo, dynTexDescBufInfo, camBufInfo, camTexBufInfo, auxBufInfo, auxTexBufInfo), dSet) → prepareDescriptorSet dev bufInfo dynBufInfo dynTexDescBufInfo camBufInfo camTexBufInfo auxBufInfo auxTexBufInfo (descTexInfo texData) dSet (nimages texData)
  -- PIPELINE
  imgViews ← mapM (\image → createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
  renderPass ← createRenderPass dev swapInfo (depthFormat texData) msaaSamples
  graphicsPipeline ← createGraphicsPipeline dev swapInfo vertIBD vertIADs [shaderVert,shaderFrag] renderPass (pipelineLayout texData) msaaSamples
  colorAttImgView ← createColorAttImgView pdev dev commandPool (graphicsQueue queues) (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
  depthAttImgView ← createDepthAttImgView pdev dev commandPool (graphicsQueue queues) (swapExtent swapInfo) msaaSamples
  framebuffers ← createFramebuffers dev renderPass swapInfo imgViews depthAttImgView colorAttImgView
  -- fps counter
  frameCount ← liftIO $ atomically $ newTVar @Int 0
  currentSec ← liftIO $ atomically $ newTVar @Int 0
  -- loop reloads commandBuffer, less of a stutter
  -- but still stutterry
  shouldExit ← loadLoop window $ do
    cmdBP0 ← genCommandBuffs dev pdev commandPool queues graphicsPipeline renderPass texData swapInfo framebuffers descriptorSets
    logDebug $ "loading swapchain.."
    -- main loop runs draw loop and trans functions
    --st ← get
    modify $ \s → s { stReload = RSNULL }
    --case (stReload st) of
    --  RSReload → modify $ \s → s { stReload = RSNULL }
    --  _        → return ()
    shouldLoad ← glfwMainLoop window $ do
      stNew ← get
      let camNew        = stCam stNew
          Dyns nDynData = stDynData stNew
          Dyns nCamData = stCamData stNew
          Dyns nAuxData = stAuxData stNew
          nDynNew       = length nDynData
          nCamNew       = length nCamData
          nAuxNew       = length nAuxData
          rdata         = RenderData { dev
                                , swapInfo
                                , queues
                                , imgIndexPtr
                                , frameIndexRef
                                , renderFinishedSems
                                , imageAvailableSems
                                , inFlightFences
                                , cmdBuffersPtr = cmdBP0
                                , memories = transObjMemories
                                , dynMemories = transDynMemories
                                , texMemories = transTexMemories
                                , camMemories = transCamMemories
                                , camTexMemories = transCamTexMemories
                                , auxMemories = transAuxMemories
                                , auxTexMemories = transAuxTexMemories
                                , memoryMutator = updateTransObj camNew dev (swapExtent swapInfo)
                                , dynMemoryMutator = updateTransDyn nDynNew nDynData dev (swapExtent swapInfo)
                                , texMemoryMutator = updateTransTex nDynNew nDynData dev (swapExtent swapInfo)
                                , camMemoryMutator = updateTransCam nCamNew nCamData dev (swapExtent swapInfo)
                                , camTexMemoryMutator = updateTransCamTex nCamNew nCamData dev (swapExtent swapInfo)
                                , auxMemoryMutator = updateTransAux nAuxNew nAuxData dev (swapExtent swapInfo)
                                , auxTexMemoryMutator = updateTransAuxTex nAuxNew nAuxData dev (swapExtent swapInfo) }

      liftIO pollEvents
      needRecreation ← drawFrame rdata `catchError` (\err → case (testEx err VK_ERROR_OUT_OF_DATE_KHR) of
        True  → do
          _ ← logDebug $ "vulkan khr out of date"
          return True
        False → logExcept ParacError ExParacletus "unknown drawFrame error" )
      sizeChanged ← liftIO $ atomically $ readTVar windowSizeChanged
      -- logic here
      processEvents
      processInput
      seconds ← getTime
      cur ← liftIO $ atomically $ readTVar currentSec
      if floor seconds ≠ cur then do
        count ← liftIO $ atomically $ readTVar frameCount
        when (cur ≠ 0) $ do
          FPS fpsTarget _ display ← gets stFPS
          loadQ ← asks envLoadQ
          liftIO $ atomically $ writeQueue loadQ $ LoadCmdSetFPS $ FPS fpsTarget count display
          modify $ \s → s { stFPS = FPS fpsTarget count display }
        liftIO $ do
          atomically $ writeTVar currentSec (floor seconds)
          atomically $ writeTVar frameCount 0
      else liftIO $ atomically $ modifyTVar' frameCount succ
      -- needed once every frame
      runVk $ vkDeviceWaitIdle dev
      -- recreation check
      stateRel ← gets stReload
      let stateReload = case (stateRel) of
                          RSNULL → False
                          _      → True
      return $ if needRecreation ∨ sizeChanged ∨ stateReload then AbortLoop else ContinueLoop
    stateRec ← gets stReload
    let stateRecreate = case (stateRec) of
                          RSRecreate → True
                          _          → False
    sizeChanged ← liftIO $ atomically $ readTVar windowSizeChanged
    return $ if shouldLoad ∨ sizeChanged ∨ stateRecreate then AbortLoop else ContinueLoop
  return $ if shouldExit then AbortLoop else ContinueLoop

genCommandBuffs ∷ VkDevice → VkPhysicalDevice → VkCommandPool → DevQueues → VkPipeline → VkRenderPass → TextureData → SwapchainInfo → [VkFramebuffer] → [VkDescriptorSet] → Anamnesis ε σ (Ptr VkCommandBuffer)
genCommandBuffs dev pdev commandPool queues graphicsPipeline renderPass texData swapInfo framebuffers descriptorSets = do
  stNew ← get
  case (stVerts stNew) of
    VertsDF (verts0, inds0) → do
      vertexBufferNew ← createVertexBuffer pdev dev commandPool (graphicsQueue queues) verts0
      indexBufferNew ← createIndexBuffer pdev dev commandPool (graphicsQueue queues) inds0
      newCmdBP ← createCommandBuffers dev graphicsPipeline commandPool renderPass (pipelineLayout texData) swapInfo vertexBufferNew (dfLen inds0, indexBufferNew) framebuffers descriptorSets
      return newCmdBP
    VertsNULL → do
    -- TODO: a loading screen could be put here
      let (verts0, inds0) = calcVertices $ [GTile (0,0) (1,1) (0,0) (1,1) 1,DMTile DMNULL (0,0) (1,1) (0,0) (1,1) 1,ATile DMNULL (0,0) (1,1) (0,0) (1,1) 1]
      vertexBufferNew ← createVertexBuffer pdev dev commandPool (graphicsQueue queues) verts0
      indexBufferNew ← createIndexBuffer pdev dev commandPool (graphicsQueue queues) inds0
      newCmdBP ← createCommandBuffers dev graphicsPipeline commandPool renderPass (pipelineLayout texData) swapInfo vertexBufferNew (dfLen inds0, indexBufferNew) framebuffers descriptorSets
      return newCmdBP
