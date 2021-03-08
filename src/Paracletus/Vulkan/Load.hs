{-# LANGUAGE Strict #-}
module Paracletus.Vulkan.Load where
-- loading textures from the lua files
-- is spun off as a child thread
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify)
import Anamnesis
import Anamnesis.Data
import Paracletus.Data
import Paracletus.Vulkan.Data
import Paracletus.Vulkan.Desc
import Paracletus.Vulkan.Texture
import Paracletus.Vulkan.Pipeline

-- loads all the textures into layouts of
-- the descriptor sets and pipeline. an
-- empty string will just load default textures
-- and filepaths added will be ammended to that
loadVulkanTextures ∷ GQData → [FilePath] → Anamnesis ε σ (TextureData)
loadVulkanTextures (GQData pdev dev cmdPool cmdQueue) fps = do
  -- the engine reserves the first few
  -- textures for default usage.
  let tex0Path     = "dat/tex/alpha.png"
      tex1Path     = "dat/tex/texture.jpg"
      fontPath     = "dat/font/asdf.ttf"
      texBoxPath   = "dat/tex/box"
  -- tex zero is just 32x32 alpha
  (textureView0, mipLevels0) ← createTextureImageView pdev dev cmdPool cmdQueue tex0Path
  textureSampler0 ← createTextureSampler dev mipLevels0
  -- tex one is the vulkan tutorial test photo
  (textureView1, mipLevels1) ← createTextureImageView pdev dev cmdPool cmdQueue tex1Path
  textureSampler1 ← createTextureSampler dev mipLevels1
  -- box texs are for info boxs
  boxTexs ← loadNTexs pdev dev cmdPool cmdQueue texBoxPath
  let (btexs, bsamps) = unzip boxTexs
  -- font texs are generated from ttf
  fontTexs ← createFontImageViews pdev dev cmdPool cmdQueue fontPath 30
  let (ftexs, fmipLvls) = unzip fontTexs
  fontSamplers ← createTextureSamplers dev fmipLvls
  -- mod texs are textures included by the lua files
  modTexViews ← createTextureImageViews pdev dev cmdPool cmdQueue fps
  texSamplersMod ← createTextureSamplers dev $ snd . unzip $ modTexViews
  let defaultTexs = ([textureView0, textureView1] ⧺ ftexs ⧺ btexs)
      texViews = defaultTexs ⧺ (fst (unzip modTexViews))
      texSamps = [textureSampler0, textureSampler1] ⧺ fontSamplers ⧺ bsamps ⧺ texSamplersMod
  descriptorTextureInfo ← textureImageInfos texViews texSamps
  depthFormat ← findDepthFormat pdev
  let nimages = length texViews
  descriptorSetLayout ← createDescriptorSetLayout dev nimages
  pipelineLayout ← createPipelineLayout dev descriptorSetLayout
  let texdata = TextureData descriptorSetLayout pipelineLayout nimages descriptorTextureInfo depthFormat
  return texdata
