{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan where
-- vulkan specific calls are made
import Anamnesis
import Anamnesis.Util

runParacVulkan ∷ Anamnesis ε σ ()
runParacVulkan = logDebug "parac vulkan"
