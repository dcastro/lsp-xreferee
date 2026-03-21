module Data.IxSet.Typed.Util where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IxSet.Typed (Indexable, IxSet)
import Data.IxSet.Typed qualified as Ix

updateMany :: (Indexable ixs a) => IxSet ixs a -> (IxSet ixs a -> IxSet ixs a) -> (a -> a) -> IxSet ixs a
updateMany set selector update =
  let entriesToUpdate = selector set
      updatedEntries = entriesToUpdate & Ix.toList <&> update & Ix.fromList
   in Ix.union (Ix.difference set entriesToUpdate) updatedEntries

deleteMany :: (Indexable ixs a) => IxSet ixs a -> (IxSet ixs a -> IxSet ixs a) -> IxSet ixs a
deleteMany set selector =
  let entriesToDelete = selector set
   in Ix.difference set entriesToDelete
