{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Spec
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Teste l'utilisation de Picrochole en ligne de commande.
-}

import Control.Monad ( filterM, forM, forM_, when )
import System.Directory
import System.Exit
import System.FilePath.Posix ( (</>), combine )
import System.Process
import Test.Hspec

import qualified Picrochole.Data.Atlas as A
import qualified Picrochole.Data.Orders as O
import qualified Picrochole.Data.Reports as R
import qualified Picrochole.Data.Units as U
import Picrochole.JSON.Pieces

instance Eq U.Unit where
  x == y = x `U.approxEq` y

instance Eq A.Tile where
  x == y = x `A.approxEq` y

main :: IO ()
main = do
  xs <- listTestDirs
  hspec $ forM_ xs test

-- | Renvoie l'ensemble des répertoires de test.
listTestDirs :: IO [FilePath]
listTestDirs = do
  wd <- getCurrentDirectory
  let dir = wd </> "test" </> "end-to-end"
  lst <- listDirectory dir
  filterM doesDirectoryExist (fmap (combine dir) lst)

-- | Renvoie l'ensemble des fichiers dans le répertoire indiqué.
listFiles :: FilePath -> IO [FilePath]
listFiles fp = do
  lst <- listDirectory fp
  filterM go lst
  where
    go x = doesFileExist (fp </> x)

-- | Teste l'exécutable sur le répertoire fourni.
test :: FilePath -> Spec
test fp = do
  it fp $ do
    clean fp
    prepare fp
    run fp
    comp fp

-- | Supprime, s'il existe, le répertoire contenant les sorties du dernier
-- lancement du test.
clean :: FilePath -> IO ()
clean fp = do
  let target = fp </> "result"
  hasResultDir <- doesDirectoryExist target
  when hasResultDir (removeDirectoryRecursive target)

-- | Prépare un répertoire pour héberger les sorties lors du lancement du
-- test.
prepare :: FilePath -> IO ()
prepare fp = do
  lst <- listFiles (fp </> "init")
  hasPastDir <- doesDirectoryExist (fp </> "init" </> "past")
  lst' <- if hasPastDir
          then fmap (fmap (combine "past")) (listFiles (fp </> "init" </> "past"))
          else pure []
  createDirectory (fp </> "result")
  when hasPastDir (createDirectory (fp </> "result" </> "past"))
  mapM_ go (lst ++ lst')
  where
    go :: FilePath -> IO ()
    go target = copyFile (fp </> "init" </> target) (fp </> "result" </> target)

-- | Lance l'exécutable sur les fichiers de configuration du répertoire.
run :: FilePath -> IO ()
run fp = do
  cmd <- readFile (fp </> "cmd.sh")
  (ex, stdOut, stdErr) <- readCreateProcessWithExitCode
                          (shell cmd)
                          ""
  case ex of
    ExitSuccess -> return ()
    ExitFailure n -> expectationFailure ("failed to run `picrochole`, got:\n\
                                         \ * exit code: " ++ (show n) ++ "\n \
                                         \ * std out: " ++ stdOut ++ "\n \
                                         \ * std err: " ++ stdErr)

-- | Compare les sorties obtenues lors du dernier lancement du test avec
-- les sorties de référence.
comp :: FilePath -> Expectation
comp fp = do
  -- Comparaison du dossier racine :
  mexpd <- loadEverythingDir (fp </> "expected")
  mres <- loadEverythingDir (fp </> "result")
  case (mexpd, mres) of
    (Left m, _) -> expectationFailure ("unable to parse the expected files, got: " ++ m)
    (_, Left m) -> expectationFailure ("unable to parse the result files, got: " ++ m)
    (Right expd, Right res) -> do
      (eatlas expd) `shouldBe` (eatlas res)
      (econfig expd) `shouldBe` (econfig res)
      (ecurrentTurn expd) `shouldBe` (ecurrentTurn res)
      (einitiative expd) `shouldBe` (einitiative res)
      (O.toList (eorders expd)) `shouldMatchList` (O.toList (eorders res))
      (eplan expd) `shouldBe` (eplan res)
      (R.toList (ereports expd)) `shouldMatchList` (R.toList (ereports res))
      (U.toList (eunits expd)) `shouldMatchList` (U.toList (eunits res))
  -- Comparaison du dossier "past" :
  fPastExpd <- listFiles (fp </> "expected" </> "past")
  fPastRes <- listFiles (fp </> "result" </> "past")
  fPastExpd `shouldBe` fPastRes
  mPastExp <- forM fPastExpd (\ x -> U.readUnits (fp </> "expected" </> "past" </> x))
  mPastRes <- forM fPastRes (\ x -> U.readUnits (fp </> "result" </> "past" </> x))
  mapM_ go (zip mPastExp mPastRes)
  where
    go (Left m, _) = expectationFailure ("unable to parse the expected file, got: " ++ m)
    go (_, Left m) = expectationFailure ("unable to parse the result file, got: " ++ m)
    go (Right x, Right y) = (U.toList x) `shouldMatchList` (U.toList y)
