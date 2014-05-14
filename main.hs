import Control.Monad
import Control.Monad.IO.Class
import Outputable
import OccName
import GHC
import GHC.Paths (libdir)

-- | Just toying around with the GHC api for now.

main :: IO ()
main = do
  runGhc (Just libdir) $ do

            -- Initialize dynflags
            df <- getSessionDynFlags
            setSessionDynFlags df

            -- find all loadable modules
            mds <- packageDbModules False

            forM_ mds $ \mod -> do
              mmi <- getModuleInfo mod -- :: m (Maybe ModuleInfo). Why is it a maybe?
              case mmi of
                Nothing -> return () -- Dunno what this means...

                Just mi -> do
                  let exportedThings = modInfoExports mi
                  forM_ exportedThings $ \r -> do
                  let str = showSDoc df $ pprOccName $ getOccName r
                  mthing <- modInfoLookupName mi r
                  let msig = case mthing of
                              Just (AnId thing) -> Just $ showSDocOneLine df $ pprParendType $ idType thing
                              _ -> Nothing
                  case msig of
                    Just sig -> liftIO $ do
                      putStr str
                      putStr " --> "
                      putStr sig
                      putStr "; "
                      putStrLn $ moduleNameString $ moduleName mod
                    _ -> return () -- Not a value

