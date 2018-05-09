{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin where

import GHC
import GHC.Paths (libdir)
import HscTypes (SourceError, srcErrorMessages)
import DynFlags
import Unsafe.Coerce
import Bag (bagToList)

ghcCall :: String -> String -> Ghc a
ghcCall modname fn = do
    setContext [IIModule (mkModuleName modname)]
    unsafeCoerce <$> compileExpr (modname ++ "." ++ fn)

ghcLoad :: String -> Ghc ()
ghcLoad path = do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags $ dflags
        { ghcLink      = LinkInMemory
        , hscTarget    = HscInterpreted
        , packageFlags = [ExposePackage "ghc" (PackageArg "ghc")
                             (ModRenaming True [])]
        }
    addTarget =<< guessTarget path Nothing
    load LoadAllTargets >>= \case
        Failed    -> error "Generic module load error"
        Succeeded -> return ()

  `gcatch` \(err :: SourceError) ->
      error $ concatMap show (bagToList (srcErrorMessages err))

example :: IO String
example = runGhc (Just libdir) $ do
    ghcLoad "Example.hs"
    ghcCall "Example" "giveMeString"

main :: IO ()
main = putStrLn "Hello"
