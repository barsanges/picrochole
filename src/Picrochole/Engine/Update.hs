{- |
   Module      : Picrochole.Engine.Update
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Met à jour les statistiques de l'unité courante.
-}

module Picrochole.Engine.Update
  ( runUpdate
  ) where

import Data.Time ( UTCTime, NominalDiffTime )
import qualified Data.Time as T
import Picrochole.Data.Keys
import Picrochole.Data.Stats
import qualified Picrochole.Utils.XsMap as Xs

-- | Met à jour les statistiques de l'unité courante.
runUpdate :: UTCTime
          -> UnitKey
          -> CStats
          -> CStats
runUpdate t' k cs = case Xs.lookupKey k cs of
    Nothing -> cs
    Just s -> Xs.insertKey k s' cs
      where
        dt = T.diffUTCTime t' (uLastUpdate s)
        s' = upSupply dt (upLastUpdate t' s)
-- FIXME : passer par une structure de type patch pour combiner et appliquer
-- les modifications ?

-- | Actualise la date de mise à jour de l'unité.
upLastUpdate :: UTCTime -> Stats -> Stats
upLastUpdate t' s = s { uLastUpdate = t' }

-- | Actualise le ravitaillement de l'unité.
upSupply :: NominalDiffTime -> Stats -> Stats
upSupply dt s = if supply' > 0
                then s { uSupply = supply' }
                else s { uSupply = 0
                       , uMorale = morale'
                       }
  where
    supply' = (uSupply s) - (uSupplyConsumption s) * (toSeconds dt)
    morale' = (uMorale s) - (supply' * (uSupplyImpactOnMorale s))

-- | Exprime un 'NominalDiffTime' comme un nombre de secondes.
toSeconds :: NominalDiffTime -> Double
toSeconds dt = realToFrac (T.nominalDiffTimeToSeconds dt)
-- FIXME : mettre ça dans 'Utils' ?
