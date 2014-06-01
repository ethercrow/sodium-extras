{-# LANGUAGE MonadComprehensions #-}
module FRP.Sodium.Extras
    ( uniqueUpdates
    , updatesSatisfyingRelation
    ) where

import qualified FRP.Sodium as S

-- | Like 'FRP.S.values' but consecutive values are guaranteed to be non-equal.
uniqueUpdates :: Eq a => S.Behavior a -> S.Reactive (S.Event a)
uniqueUpdates = updatesSatisfyingRelation (/=)

-- | Like 'FRP.S.values' but consecutive values are guaranteed to
--   satisfy the passed relation.
updatesSatisfyingRelation :: (a -> a -> Bool) -> S.Behavior a -> S.Reactive (S.Event a)
updatesSatisfyingRelation rel behavior = do
    initial <- S.sample behavior
    let possiblyDuplicateValues = S.updates behavior
    uniqueValues <- S.collectE (\new old -> ([new | rel old new], new))
                                    initial
                                    possiblyDuplicateValues
    return $! S.filterJust uniqueValues
