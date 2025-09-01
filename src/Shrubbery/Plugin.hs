{- |
Copyright : Flipstone Technology Partners 2025
License   : BSD3
-}
module Shrubbery.Plugin (
    plugin
) where

import GHC.Plugins (Plugin, defaultPlugin)

plugin :: Plugin
plugin = defaultPlugin

