import XMonad
import XMonad.StackSet (greedyView, currentTag)
import Data.Map (union, fromList)

{- Use the default configuration with a few changes:
       * Use Windows key as modifier
       * Some adjusted key-bindings
       * Mouse scroll wheel cycles workspaces
   Basic enough that it does not require xmonad-contrib. -}
main :: IO ()
main = xmonad $ def
    { modMask       = mod4Mask
    , keys          = union newKeyBindings . keys def
    , mouseBindings = union newMouseBindings . mouseBindings def
    }
  where
    newKeyBindings = fromList $
        [ ((mod4Mask,               xK_f), spawn "firefox-bin")
        , ((mod4Mask,               xK_d), spawn "dmenu_run")
        , ((mod4Mask,               xK_p), spawn "passmenu")
        , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
        ]
    newMouseBindings = fromList $
        [ ((mod4Mask, button4), \w -> (cycleWorkspace True wks) >>= windows . greedyView)
        , ((mod4Mask, button5), \w -> (cycleWorkspace False wks) >>= windows . greedyView)
        ]
    wks = workspaces def

-- Get next workspace tag.
-- If @dir@ is False then cycle backwards.
cycleWorkspace :: Bool -> [WorkspaceId] -> X WorkspaceId
cycleWorkspace dir wks = do
    ws <- gets windowset
    let wks' = if dir then wks else reverse wks
        cur  = currentTag ws
        next = head . drop 1 . dropWhile (/= cur) $ wks' ++ (take 1 wks')
        -- Inspired by https://codereview.stackexchange.com/a/61039
        -- Current tag has to be present so we can safely take the head here
    return next
