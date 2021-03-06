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
  let tex1Path     = "dat/tex/alpha.png"
      texAlph      = "dat/tex/alph.png"
      texCursor    = "dat/tex/cursor.png"
      texboxPath   = "dat/tex/box"
      texmboxPath  = "dat/tex/mbox"
      textMenuPath = "dat/tex/menu"
      texFont      = "dat/font/asdf.ttf"
  boxTexs ← loadNTexs pdev dev cmdPool cmdQueue texboxPath
  mboxTexs ← loadNTexs pdev dev cmdPool cmdQueue texmboxPath
  (textureView1, mipLevels1) ← createTextureImageView pdev dev cmdPool cmdQueue tex1Path
  (texViewAlph, mipLevelsAlph) ← createTextureImageView pdev dev cmdPool cmdQueue texAlph
  (texViewCurs, mipLevelsCurs) ← createTextureImageView pdev dev cmdPool cmdQueue texCursor
  menuTexs ← loadNTexs pdev dev cmdPool cmdQueue textMenuPath
  --fontTexs16 ← createFontImageViews pdev dev cmdPool cmdQueue texFont 16
  fontTexs30 ← createFontImageViews pdev dev cmdPool cmdQueue texFont 30
  modTexViews ← createTextureImageViews pdev dev cmdPool cmdQueue fps
  textureSampler1 ← createTextureSampler dev mipLevels1
  texSamplerAlph  ← createTextureSampler dev mipLevelsAlph
  texSamplerCurs  ← createTextureSampler dev mipLevelsCurs
  texSamplersMod  ← createTextureSamplers dev $ snd . unzip $ modTexViews
  --let (ftexs16, fmipLvls16) = unzip fontTexs16
  let (ftexs30, fmipLvls30) = unzip fontTexs30
  --font16Samplers ← createTextureSamplers dev fmipLvls16
  font30Samplers ← createTextureSamplers dev fmipLvls30
  let (btexs, bsamps) = unzip boxTexs
      (mbtexs, mbsamps) = unzip mboxTexs
      (menutexs, menusamps) = unzip menuTexs
      defaultTexs = ([textureView1,texViewAlph] ⧺ btexs ⧺ mbtexs ⧺ menutexs ⧺ ftexs30 ⧺ [texViewCurs])-- ⧺ ftexs16)
      texViews = defaultTexs ⧺ (fst (unzip modTexViews))
      texSamps = [textureSampler1,texSamplerAlph] ⧺ bsamps ⧺ mbsamps ⧺ menusamps ⧺ font30Samplers ⧺ texSamplersMod ⧺ [texSamplerCurs]-- ⧺ font16Samplers ⧺ texSamplersMod
  modify $ \s → s { stNDefTex = length defaultTexs }
  descriptorTextureInfo ← textureImageInfos texViews texSamps
  depthFormat ← findDepthFormat pdev
  let nimages = length texViews
  descriptorSetLayout ← createDescriptorSetLayout dev nimages
  pipelineLayout ← createPipelineLayout dev descriptorSetLayout
  let texdata = TextureData descriptorSetLayout pipelineLayout nimages descriptorTextureInfo depthFormat
  return texdata
