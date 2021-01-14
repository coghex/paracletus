{-# LANGUAGE Strict #-}
module Paracletus.Load
  ( loadParacletus )
where
-- we define the thread that helps
-- recreate the swapchain
import Prelude()
import UPrelude
import Artos.Data
import Artos.Var ( atomically )
import Artos.Queue
    ( readChan, tryReadChan, tryReadQueue, writeQueue )
import Anamnesis.Data
import Epiklesis.ArgV (changeWin)
import Epiklesis.Data
    ( DSStatus(DSSLogDebug, DSSLoadCap, DSSLoadDyns, DSSRecreate,
               DSSEvalShell, DSSLoadVerts, DSSLoadWorld, DSSLoadInput, DSSExit,
               DSSNULL),
      DrawState(DrawState, dsShell, dsTiles, dsNDefTex, dsWins, dsFPS,
                dsBuff, dsStatus),
      LinkAction(LinkSlider),
      PaneBit(PaneBitSlider),
      WinElem(WinElemLink),
      WinType(WinTypeGame),
      Window(winType, winScreen, winAccel, winElems, winCursor),
      WorldData(wdCam) )
import Epiklesis.Shell ( evalShCmds, evalShell, initShell )
import Epiklesis.Window
    ( calcWinModTexs,
      currentWin,
      findWin,
      findWinI,
      findWorldData,
      loadNewBit,
      replaceWin,
      replaceWorldWinElem )
import Epiklesis.World ( loadWorld )
import Paracletus.Buff ( genShBuff, initBuff, clearDyns)
import Paracletus.Data
    ( Cardinal(CardNULL),
      Dyns,
      FPS(..),
      GraphicsLayer(OpenGLES, OpenGL, Vulkan),
      PrintArg(PrintCam),
      Verts(VertsDF) )
import Paracletus.Draw ( loadTiles )
import Paracletus.Dyn ( loadDyns, moveSlider )
import Paracletus.Elem ( findBitPos )
import Paracletus.Vulkan.Calc ( calcVertices )
import Paracletus.Oblatum.Event (findDir, calcCam, decell, accelIS)
import Paracletus.Oblatum.Mouse (linkTest)
import Control.Concurrent (threadDelay)
import Data.Time.Clock ( diffUTCTime, getCurrentTime )

loadParacletus ∷ Env -> GraphicsLayer → IO ()
loadParacletus env Vulkan   = loadParacVulkan env
loadParacletus env OpenGL   = atomically $ writeQueue ec $ EventLogDebug "not yet implemented"
  where ec = envEventQ env
loadParacletus env OpenGLES = atomically $ writeQueue ec $ EventLogDebug "not yet implemented"
  where ec = envEventQ env
loadParacletus env _        = atomically $ writeQueue ec $ EventLogDebug "dont know graphics layer"
  where ec = envEventQ env

-- loop provides calculations for the main thread
-- so loading new objects doesnt stutter the window
loadParacVulkan ∷ Env → IO ()
loadParacVulkan env = do
  runLoadLoop env initDS TStop
  where initDS  = DrawState DSSNULL initShell cmds [] (-1) (-1) [] (FPS 30.0 30 False) initBuff 0
        cmds    = ["newWindow", "newText", "newMenu", "newMenuBit", "newLink", "newWorld", "switchWindow", "switchScreen", "setBackground", "luaModule", "newDynObj", "resizeWindow", "toggleFPS"]

-- load loop runs with a delay so that
-- it can sleep (ghc threads run like that)
runLoadLoop ∷ Env → DrawState → TState → IO ()
runLoadLoop env ds TStop = do
  --loop starts almost immediately
  let timerChan = envLoadCh env
  tsnew ← atomically $ readChan timerChan
  runLoadLoop env ds tsnew
runLoadLoop env ds TStart = do
  start ← getCurrentTime
  let timerChan = envLoadCh env
  timerstate ← atomically $ tryReadChan timerChan
  tsnew ← case (timerstate) of
    Nothing → return TStart
    Just x  → return x
  ds' ← processCommands env ds
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runLoadLoop env ds' tsnew
-- pause not needed for this timer
runLoadLoop _   _ TPause = return ()
runLoadLoop _   _ TNULL  = return ()

-- command queue processed every tick,
-- logging any errors of all commands
processCommands ∷ Env → DrawState → IO (DrawState)
processCommands env ds = do
    mcmd ← atomically $ tryReadQueue $ envLoadQ env
    case mcmd of
      Just cmd → do
        ret ← processCommand env ds cmd
        case ret of
          ResSuccess → processCommands env ds
          ResDrawState ds' → case (dsStatus ds') of
            DSSNULL → processCommands env ds'
            DSSExit → do
              let eventQ = envEventQ env
              atomically $ writeQueue eventQ $ EventExit
              return ds'
            DSSLoadInput link → do
              let eventQ = envEventQ env
                  loadQ  = envLoadQ  env
              atomically $ writeQueue eventQ $ EventInput link
              atomically $ writeQueue loadQ $ LoadCmdDyns
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLoadWorld → do
              atomically $ writeQueue (envLoadQ env) $ LoadCmdWorld
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLoadVerts → do
              atomically $ writeQueue (envLoadQ env) $ LoadCmdVerts
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL
                                 , dsBuff   = genDynBuffs ds' }
            DSSEvalShell → do
              ds'' ← evalShell env $ ds' { dsStatus = DSSNULL }
              processCommands env ds''
            DSSRecreate → do
              atomically $ writeQueue (envEventQ env) $ EventRecreate
              atomically $ writeQueue (envLoadQ env) $ LoadCmdVerts
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL
                                 , dsBuff   = genDynBuffs ds' }
            DSSLoadDyns → do
              atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL
                                 , dsBuff   = genDynBuffs ds' }
            DSSLoadCap cap → do
              atomically $ writeQueue (envEventQ env) $ EventCap cap
              atomically $ writeQueue (envLoadQ env) $ LoadCmdVerts
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLogDebug str → do
              let eventQ = envEventQ env
              atomically $ writeQueue eventQ $ EventLogDebug str
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
          ResError str → do
            atomically $ writeQueue (envEventQ env) $ EventLogDebug $ "load command error: " ⧺ str
            processCommands env ds
          ResNULL → do
            atomically $ writeQueue (envEventQ env) $ EventLogDebug $ "load null command"
            return ds
      Nothing → return ds

data LoadResult = ResSuccess | ResError String | ResDrawState DrawState | ResNULL

processCommand ∷ Env → DrawState → LoadCmd → IO LoadResult
processCommand env ds cmd = case cmd of
  LoadCmdToggleFPS → do
    let eventQ = envEventQ env
        ds'    = ds { dsFPS = toggleFPS (dsFPS ds) }
        toggleFPS ∷ FPS → FPS
        toggleFPS (FPS a b c) = FPS a b (not c)
    atomically $ writeQueue eventQ $ EventToggleFPS
    return $ ResDrawState ds'
  LoadCmdMoveSlider x n → case (currentWin ds) of
    Nothing → return $ ResError "no window"
    Just w  → do
      let eventQ = envEventQ env
          dyns   = loadDyns ds'
          ds'    = ds { dsWins = replaceWin win (dsWins ds) }
          win    = moveSlider x n w
      atomically $ writeQueue eventQ $ EventDyns dyns
      return $ ResDrawState ds'
  LoadCmdSetNDefTex nDefTex → do
    return $ ResDrawState ds'
    where ds' = ds { dsNDefTex = nDefTex }
  LoadCmdSetFPS fps → do
    let eventQ = envEventQ env
        loadQ  = envLoadQ  env
        dyns   = loadDyns ds'
        ds'    = ds { dsFPS = fps }
    atomically $ writeQueue eventQ $ EventDyns $ dyns
    atomically $ writeQueue loadQ  $ LoadCmdWorld
    return $ ResDrawState ds'
  LoadCmdMouseCam pos' oldPos → case (currentWin ds) of
    Nothing → return $ ResError $ "no world data"
    Just w  → case (findWorldData w) of
      Nothing → return $ ResError $ "no world data"
      Just (_,wd) → do
        let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
            diff = (((fst pos)-(fst oldPos)),((snd pos)-(snd oldPos)))
            oldCam = winCursor w
            newCam = moveCam oldCam diff
            moveCam ∷ (Float,Float,Float) → (Float,Float) → (Float,Float,Float)
            moveCam (x1,y1,z1) (x2,y2) = (x1+x2,y1-y2,z1)
            newSC = moveScreenCursor newCam
            moveScreenCursor ∷ (Float,Float,Float) → (Float,Float)
            moveScreenCursor (x,y,_) = (-0.05*x,-0.05*y)
            newWD = wd { wdCam = newSC }
            newWinE = replaceWorldWinElem newWD (winElems w)
            newWin  = w { winCursor = newCam
                        , winAccel  = (0.0,0.0)
                        , winElems  = newWinE }
            newWins = replaceWin newWin (dsWins ds)
            ds'     = ds { dsWins = newWins
                         , dsStatus = DSSLoadDyns }
        atomically $ writeQueue (envEventQ env) $ EventMoveCam newCam
        return $ ResDrawState ds'
            
  LoadCmdMoveCam ks → case (currentWin ds) of
    Nothing  → return $ ResError "no window"
    Just win → do
      let eventQ     = envEventQ env
          newAccel   = decell $ accelIS dir (winAccel win)
          dir        = case (findDir ks) of
                         Just d  → d
                         Nothing → CardNULL
          newCam     = calcCam newAccel $ winCursor win
          newWin     = win { winCursor = newCam
                           , winAccel  = newAccel }
          ds'        = ds { dsWins = replaceWin newWin (dsWins ds) }
      if (((abs (fst newAccel)) ≤ 0.0) ∧ ((abs (snd newAccel)) ≤ 0.0)) then atomically $ writeQueue eventQ $ EventAccel else return ()
      atomically $ writeQueue eventQ $ EventMoveCam newCam
      return $ ResDrawState ds'
  LoadCmdShell shCmd → return $ ResDrawState $ evalShCmds env shCmd ds
  LoadCmdNewWin win → return $ ResDrawState ds'
    where ds' = ds { dsWins = win:(dsWins ds) }
  LoadCmdSwitchScreen screen → case (currentWin ds) of
    Nothing → return $ ResError $ "no window"
    Just w  → return $ ResDrawState ds'
      where ds' = ds { dsWins = replaceWin win (dsWins ds)
                     , dsBuff = clearDyns (dsBuff ds) }
            win = w { winScreen = screen }
  -- TODO: get rid of winI, use head
  LoadCmdSwitchWin win → do
    case (findWinI win (dsWins ds)) of
      Just n  → do
        let ds'  = changeWin n ds
        atomically $ writeQueue (envEventQ env) $ EventModTexs $ calcWinModTexs $ head $ dsWins ds'
        return $ ResDrawState ds'
      Nothing → return $ ResError $ "window " ⧺ win ⧺ " not found"
  LoadCmdLink pos → return $ ResDrawState ds'
    where ds' = linkTest pos ds
  LoadCmdDyns → do
    let eventQ = envEventQ env
        dyns   = loadDyns ds
    atomically $ writeQueue eventQ $ EventDyns $ dyns
    return ResSuccess
  LoadCmdVerts → do
    let newVerts = VertsDF $ calcVertices $ loadTiles ds
        ds'      = ds { dsTiles = loadTiles ds }
        dyns   = loadDyns ds'
    atomically $ writeQueue (envEventQ env) $ EventVerts newVerts
    atomically $ writeQueue (envEventQ env) $ EventDyns $ dyns
    return $ ResDrawState ds'
  LoadCmdNewElem name el → do
    let wins = dsWins ds
    case (findWin name wins) of
      Nothing  → return $ ResError $ "no window " ⧺ name ⧺ " yet present"
      Just win → do
        let ds'    = ds { dsWins = replaceWin win' wins }
            win'   = win { winElems = els }
            els    = el:(winElems win)
        return $ ResDrawState ds'
  LoadCmdNewBit name pane bit → do
    let wins = dsWins ds
    case (findWin name wins) of
      Nothing  → return $ ResError $ "no window " ⧺ name ⧺ " yet present"
      Just win → do
        let ds'    = ds { dsWins = replaceWin win' wins }
            win'   = win { winElems = elems }
            elems  = loadNewBit pane (winElems win) bit
            box    = (2.0,1.0)
            loadQ  = envLoadQ env
            eventQ = envEventQ env
            pos'   = ((fst pos) + 6.5,(snd pos) + 0.5)
            (bitL,pos) = findBitPos pane elems
        case bit of
          PaneBitSlider _ _ _ _ → do
            atomically $ writeQueue loadQ $ LoadCmdNewElem name $ WinElemLink pos' box $ LinkSlider $ bitL
            atomically $ writeQueue eventQ $ EventNewInput $ LinkSlider $ bitL
          _ → return ()
        return $ ResDrawState ds'
  LoadCmdScroll y → case (currentWin ds) of
    Nothing  → return $ ResError "no window present"
    Just win → if ((winType win) ≡ WinTypeGame) then do
        let ds'        = ds { dsWins = replaceWin newWin (dsWins ds) }
            newWin     = win { winCursor = newCam }
            (cx,cy,cz) = (winCursor win)
            newCam     = (cx,cy,cz')
            cz'        = min -0.1 $ max -10 $ cz - (0.1*(realToFrac y))
            --z'         = 0.1*(realToFrac y)
            eventQ     = envEventQ env
        atomically $ writeQueue eventQ $ EventMoveCam newCam
        return $ ResDrawState ds'
      else return ResSuccess
  LoadCmdWorld → case (currentWin ds) of
    Nothing  → return $ ResError "no window present"
    Just win → do
      if ((winType win) ≡ WinTypeGame) then do
        let ds' = loadWorld ds
        atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
        return $ ResDrawState ds'
      else do
        atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
        return $ ResSuccess
  LoadCmdPrint PrintCam → case (currentWin ds) of
    Nothing → return $ ResError "no window"
    Just w  → do
      atomically $ writeQueue (envEventQ env) $ EventLogDebug $ show $ winCursor w
      return ResSuccess
  LoadCmdPrint arg      → return $ ResError $ "no arg " ⧺ (show arg) ⧺ " known"
  LoadCmdNULL → return ResNULL

genDynBuffs ∷ DrawState → [Dyns]
genDynBuffs ds = dyns1
  where dyns0 = dsBuff ds
        dyns1 = genShBuff dyns0 0 $ dsShell ds

