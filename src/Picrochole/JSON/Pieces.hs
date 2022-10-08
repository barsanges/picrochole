{- |
   Module      : Picrochole.JSON.Pieces
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Tout ou partie des données du jeu.
-}

module Picrochole.JSON.Pieces
  ( atlasFileName
  , configFileName
  , currentTurnFileName
  , initiativeFileName
  , ordersFileName
  , planFileName
  , reportsFileName
  , unitsFileName
  , Some(..)
  , (&>)
  , loadSome
  , loadSomeDir
  , Everything(..)
  , loadEverything
  , loadEverythingDir
  ) where

import qualified Data.Set as S
import System.FilePath.Posix ( (</>) )
import System.Directory

import Picrochole.Data.Atlas
import Picrochole.Data.Base
import Picrochole.Data.Config
import Picrochole.Data.Initiative
import Picrochole.Data.Orders
import Picrochole.Data.Plan
import Picrochole.Data.Reports
import Picrochole.Data.Units

-- | Nom standard du fichier contenant l'atlas.
atlasFileName :: String
atlasFileName = "atlas.json"

-- | Nom standard du fichier de configuration.
configFileName :: String
configFileName = "config.json"

-- | Nom standard du fichier contenant l'identifiant du tour courant.
currentTurnFileName :: String
currentTurnFileName = "current-turn.json"

-- | Nom standard du fichier contenant l'ordre de jeu.
initiativeFileName :: String
initiativeFileName = "initiative.json"

-- | Nom standard du fichier contenant les messages d'ordres.
ordersFileName :: String
ordersFileName = "orders.json"

-- | Nom standard du fichier contenant le plan de l'IA.
planFileName :: String
planFileName = "ia-plan.json"

-- | Nom standard du fichier contenant les rapports des unités.
reportsFileName :: String
reportsFileName = "reports.json"

-- | Nom standard du fichier contenant les unités.
unitsFileName :: String
unitsFileName = "current-units.json"

-- | Concatène des valeurs optionnelles.
(&>) :: Maybe a -> Maybe b -> Maybe (a, b)
(&>) Nothing _ = Nothing
(&>) _ Nothing = Nothing
(&>) (Just x) (Just y) = Just (x, y)

-- | Une partie des données du jeu.
data Some = Some { satlas :: Maybe Atlas
                 , sconfig :: Maybe Config
                 , scurrentTurn :: Maybe TurnCount
                 , sinitiative :: Maybe [UnitKey]
                 , sorders :: Maybe (Register Order)
                 , splan :: Maybe Plan
                 , sreports :: Maybe (Register Report)
                 , sunits :: Maybe Units
                 }

-- | Toutes les données du jeu.
data Everything = Everything { eatlas :: Atlas
                             , econfig :: Config
                             , ecurrentTurn :: TurnCount
                             , einitiative :: [UnitKey]
                             , eorders :: (Register Order)
                             , eplan :: Plan
                             , ereports :: (Register Report)
                             , eunits :: Units
                             }

-- | Lit le contenu d'un fichier si un chemin est effectivement fourni.
maybeDecode :: (FilePath -> IO (Either String a))
            -> Maybe FilePath
            -> IO (Either String (Maybe a))
maybeDecode _ Nothing = return (Right Nothing)
maybeDecode f (Just fp) = do
    mx <- f fp
    case mx of
        Left m -> return (Left ("Error in file " ++ fp ++ ": " ++ m))
        Right x -> return (Right (Just x))

-- | Lit le contenu d'un fichier si un chemin est effectivement fourni.
justDecode :: (FilePath -> IO (Either String a))
            -> FilePath
            -> IO (Either String a)
justDecode f fp = do
    mx <- f fp
    case mx of
        Left m -> return (Left ("Error in file " ++ fp ++ ": " ++ m))
        Right x -> return (Right x)

-- | Charge une partie des données du jeu.
loadSome :: Maybe FilePath
         -> Maybe FilePath
         -> Maybe FilePath
         -> Maybe FilePath
         -> Maybe FilePath
         -> Maybe FilePath
         -> Maybe FilePath
         -> Maybe FilePath
         -> IO (Either String Some)
loadSome fAtlas fConfig fTurn fInit fOrders fPlan fReports fUnits = do
  atlas <- maybeDecode readAtlas fAtlas
  config <- maybeDecode readConfig fConfig
  currentTurn <- maybeDecode readCurrentTurn fTurn
  initiative <- maybeDecode readInitiative fInit
  orders <- maybeDecode readOrders fOrders
  plan <- maybeDecode readPlan fPlan
  reports <- maybeDecode readReports fReports
  units <- maybeDecode readUnits fUnits
  return ( Some
           <$> atlas
           <*> config
           <*> currentTurn
           <*> initiative
           <*> orders
           <*> plan
           <*> reports
           <*> units
         )

-- | Charge tout ou partie des données du jeu depuis un seul dossier.
loadSomeDir :: FilePath -> IO (Either String Some)
loadSomeDir dir = do
  atlas <- go atlasFileName
  config <- go configFileName
  currentTurn <- go currentTurnFileName
  initiative <- go initiativeFileName
  orders <- go ordersFileName
  plan <- go planFileName
  reports <- go reportsFileName
  units <- go unitsFileName
  loadSome atlas config currentTurn initiative orders plan reports units
  where
    go :: FilePath -> IO (Maybe FilePath)
    go fp = do
      cond <- doesPathExist (dir </> fp)
      if cond
        then return (Just (dir </> fp))
        else return (Nothing)

-- | Charge toutes les données du jeu.
loadEverything :: FilePath
               -> FilePath
               -> FilePath
               -> FilePath
               -> FilePath
               -> FilePath
               -> FilePath
               -> FilePath
               -> IO (Either String Everything)
loadEverything fAtlas fConfig fTurn fInit fOrders fPlan fReports fUnits = do
  atlas <- justDecode readAtlas fAtlas
  config <- justDecode readConfig fConfig
  currentTurn <- justDecode readCurrentTurn fTurn
  initiative <- justDecode readInitiative fInit
  orders <- justDecode readOrders fOrders
  plan <- justDecode readPlan fPlan
  reports <- justDecode readReports fReports
  units <- justDecode readUnits fUnits
  let mev = Everything
            <$> atlas
            <*> config
            <*> currentTurn
            <*> initiative
            <*> orders
            <*> plan
            <*> reports
            <*> units
  return (foldr go mev [ checkUnitsInitiative
                       , checkPlanLocations
                       , checkUnitsLocations
                       , checkUnitsPlan
                       ])
  where
    go _ (Left m) = Left m
    go f (Right x) = f x

-- | Vérifie que le fichier des unités et le fichier d'initiative sont
-- compatibles.
checkUnitsInitiative :: Everything -> Either String Everything
checkUnitsInitiative ev =
  if inUnits `S.isSubsetOf` inInitiative
  then Right ev
  else Left ("some units are not listed in the initiative file: "
             ++ (show $ S.toList (S.difference inUnits inInitiative)))
  where
    inUnits = (elemKeys $ eunits ev)
    inInitiative = (S.fromList $ einitiative ev)

-- | Vérifie que le plan de l'IA et l'atlas sont compatibles.
checkPlanLocations :: Everything -> Either String Everything
checkPlanLocations ev
  | not (null notLegit) = Left ("some targets of the IA are not legitimate locations: "
                                ++ (show notLegit))
  | not (null noCapacity) = Left ("some targets of the IA have no capacity: "
                                  ++ (show noCapacity))
  | otherwise = Right ev
  where
    grid = eatlas ev
    targets = (concentration (eplan ev)):(fmap target (objectives (eplan ev)))
    notLegit = filter (not . (isInsideGrid grid)) targets
    testCapacity x = capacity (getHex grid x) <= 0
    noCapacity = filter testCapacity targets

-- | Vérifie que le fichier des unités et l'atlas sont compatibles.
checkUnitsLocations :: Everything -> Either String Everything
checkUnitsLocations ev
  | not (null notLegit) = Left ("some units are not on legitimate locations: "
                                ++ (show notLegit))
  | not (null wrongCapacity) = Left ("some cells have more units than allowed: "
                                     ++ (show wrongCapacity))
  | otherwise = Right ev
  where
    grid = eatlas ev
    us = eunits ev
    locs = S.toList (locationKeys (eunits ev))
    notLegit = filter (not . (isInsideGrid grid)) locs
    testCapacity x = (go Blue) || (go Red)
      where
        go f = (capacityLeft grid us f x) < 0
    wrongCapacity = filter testCapacity locs

-- | Vérifie que le fichier des unités et le plan de l'IA sont compatibles.
checkUnitsPlan :: Everything -> Either String Everything
checkUnitsPlan ev =
  if inUnits `S.isSubsetOf` inPlan
  then Right ev
  else Left ("some IA units are not listed in the plan file: "
             ++ (show $ S.toList (S.difference inUnits inPlan)))
  where
    ia = iaFaction (econfig ev)
    inUnits = (unitKeys (eunits ev) ia)
    inPlan = allUnitKeys (eplan ev)

-- | Charge toutes les données depuis un seul dossier.
loadEverythingDir :: FilePath -> IO (Either String Everything)
loadEverythingDir dir = do
  cond <- doesPathExist dir
  if cond
    then loadEverything
         (dir </> atlasFileName)
         (dir </> configFileName)
         (dir </> currentTurnFileName)
         (dir </> initiativeFileName)
         (dir </> ordersFileName)
         (dir </> planFileName)
         (dir </> reportsFileName)
         (dir </> unitsFileName)
    else return (Left "unable to find the game directory")
