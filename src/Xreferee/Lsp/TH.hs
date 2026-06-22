module Xreferee.Lsp.TH where

import ClassyPrelude
import Control.Lens
import Language.Haskell.TH qualified as TH

-- NOTE: the built-in `classyRules` uses `underscoreNoPrefixNamer` which strips the leading underscore from the field name.
-- Here, we're not renaming any fields.
--
-- This is meant to be used together with `NoFieldSelectors`, otherwise it'll generate duplicate declarations.
classyIdRules :: LensRules
classyIdRules = classyRules & lensField .~ noRenaming
  where
    noRenaming :: FieldNamer
    noRenaming _ _ n = do
      [TopName $ TH.mkName $ TH.nameBase n]
