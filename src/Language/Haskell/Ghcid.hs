
-- | The entry point of the library
module Language.Haskell.Ghcid
 ( T.Ghci
 , T.GhciError (..)
 , T.Severity (..)
 , T.Load (..)
 , startGhci
 , showModules
 , reload
 , T.exec
 , T.interrupt
 , stopGhci
 )
where

import System.IO
import System.IO.Error
import System.Process
import Control.Concurrent
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Function
import Data.List
import Data.IORef
import Control.Applicative

import System.Console.CmdArgs.Verbosity

import Language.Haskell.Ghcid.Parser
import Language.Haskell.Ghcid.Types as T
import Language.Haskell.Ghcid.Util
import Prelude

-- | Start GHCi, returning a function to perform further operation, as well as the result of the initial loading.
--   Pass True to write out messages produced while loading, useful if invoking something like "cabal repl"
--   which might compile dependent packages before really loading.
startGhci :: String -> Maybe FilePath -> Bool -> IO (Ghci, [Load])
startGhci cmd directory echo = do
    (Just inp, Just out, Just err, _) <-
        createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, cwd = directory}
    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    lock <- newMVar () -- ensure only one person talks to ghci at a time
    let prefix = "#~GHCID-START~#"
    let finish = "#~GHCID-FINISH~#"
    hPutStrLn inp $ ":set prompt " ++ prefix
    hPutStrLn inp ":set -fno-break-on-exception -fno-break-on-error" -- see #43
    echo <- newIORef echo

    -- consume from a handle, produce an MVar with either Just and a message, or Nothing (stream closed)
    let consume h name = do
            result <- newEmptyMVar -- the end result
            buffer <- newMVar [] -- the things to go in result
            forkIO $ fix $ \rec -> do
                el <- tryBool isEOFError $ hGetLine h
                case el of
                    Left _ -> putMVar result Nothing
                    Right l -> do
                        whenLoud $ outStrLn $ "%" ++ name ++ ": " ++ l
                        whenM (readIORef echo) $
                            unless (any (`isInfixOf` l) [prefix, finish]) $ outStrLn l
                        if finish `isInfixOf` l
                          then do
                            buf <- modifyMVar buffer $ \old -> return ([], reverse old)
                            putMVar result $ Just buf
                          else
                            modifyMVar_ buffer $ return . (dropPrefixRepeatedly prefix l:)
                        rec
            return result

    outs <- consume out "GHCOUT"
    errs <- consume err "GHCERR"

    let f s = withMVar lock $ const $ do
                whenLoud $ outStrLn $ "%GHCINP: " ++ s
                hPutStrLn inp $ s ++ "\nPrelude.putStrLn " ++ show finish ++ "\nPrelude.error " ++ show finish
                outC <- takeMVar outs
                errC <- takeMVar errs
                case liftM2 (++) outC errC of
                    Nothing  -> throwIO $ UnexpectedExit cmd s
                    Just msg -> return  msg
    r <- parseLoad <$> f ""
    writeIORef echo False
    return (Ghci f (putStrLn "TODO (SM): implement interrupt"), r)


-- | Show modules
showModules :: Ghci -> IO [(String,FilePath)]
showModules ghci = parseShowModules <$> exec ghci ":show modules"

-- | reload modules
reload :: Ghci -> IO [Load]
reload ghci = parseLoad <$> exec ghci ":reload"

-- | Stop GHCi
stopGhci :: Ghci -> IO ()
stopGhci ghci = handle (\UnexpectedExit{} -> return ()) $ void $ exec ghci ":quit"
