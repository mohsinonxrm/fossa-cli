module App.NewFossa.Config.Utils (
  configError,
) where

import GHC.Base (lazy)

configError :: a
configError = lazy $ error "Errors occurred while resolving configuration values. See above for details."
