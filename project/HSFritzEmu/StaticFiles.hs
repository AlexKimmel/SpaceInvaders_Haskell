{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
 module StaticFiles (staticFiles, unpack) where

import Prelude hiding (catch)
import Language.Haskell.TH.Syntax
import qualified Data.Map as M
import qualified Data.ByteString as BS
import Control.Exception
import Control.Monad
import System.Directory
import System.IO

-- contains map from FilePath to their contents
staticFiles :: M.Map FilePath BS.ByteString
staticFiles = $(do
    -- Which folders/files to include
    initialPaths <- runIO $ fmap lines $ readFile "StaticFiles.files"
    let checkReadFile fn = runIO $ BS.readFile fn
    -- I shall hate myself for this...
        allFiles fns | null fns = return []
                     | otherwise = do
          (files, dirs) <- foldM (\(f, d) fn -> doesDirectoryExist fn >>= \de -> return $ if de then (f, fn : d) else (fn : f, d)) ([], []) fns
          res <- sequence $ flip map dirs $ \fn -> do
              de <- doesDirectoryExist fn
              if de then map (\x -> fn ++ "/" ++ x) `fmap` filter (\x -> x /= "." && x /= "..") `fmap` getDirectoryContents fn else return []
          filess <- allFiles $ concat res
          return $ files ++ filess
    files <- runIO $ allFiles initialPaths
    map <- foldM (\map fn -> checkReadFile fn >>= \cont -> return $ M.insert fn cont map) M.empty files
    reader <- [| read |]
    return $ AppE reader $ LitE $ StringL $ show map
    )

-- quine functionality
unpack = forM_ (M.assocs staticFiles) $ \(path, cont) -> do
    let path' = reverse $ dropWhile (/='/') $ reverse path
    createDirectoryIfMissing True path'
    BS.writeFile path cont
