{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Simple.BuildPrograms
    ( -- * Introduction
      --
      -- $introduction

      -- * Why?
      --
      -- $why

      -- * Getting Started
      --
      -- $gettingstarted

      -- * Limitations
      --
      -- $limitations

      -- * Stack & HPack Gotchas
      --
      -- $stackHPackGotchas

      buildProgramsCustomField
    , defaultMain
    , buildProgramsUserHooks
    , localBuildInfoWithBuildPrograms
    , componentBuildPrograms
    , overlayUserHooks
    ) where

import Distribution.Simple(UserHooks(..),simpleUserHooks, defaultMainWithHooks)
import Distribution.Simple.Setup(BuildFlags(..), fromFlag,ReplFlags(..))
import Distribution.Types.LocalBuildInfo(LocalBuildInfo(..), neededTargetsInBuildOrder')
import Distribution.Types.PackageDescription(PackageDescription(..))
import Distribution.Types.Component(Component(..), componentBuildInfo, componentName)
import Distribution.Compat.Graph(nodeKey)
import Distribution.Types.BuildInfo(BuildInfo(customFieldsBI))
import Distribution.Types.TargetInfo(TargetInfo(..))
import Distribution.Simple.UserHooks(Args)
import Distribution.Simple.BuildTarget(readTargetInfos)
import Data.List(concatMap,unfoldr,nub,intersperse)
import Distribution.Verbosity(Verbosity)
import Control.Exception(catch,IOException)
import Control.Monad(foldM)
import Distribution.Pretty(prettyShow)
import Distribution.Simple.Program.Db(ProgramDb,requireProgram)
import Distribution.Simple.Program.Types(simpleProgram, Program(programName))
import Distribution.Simple.Utils(die')

{-|
The custom field to use in Cabal stanza to specify external programs.
In a ~.cabal~ file use:

@
x-build-programs: p1,p2,p3
@

whereas in the ~package.yaml~ of a <https://docs.haskellstack.org/en/stable/README/ Stack> or <https://hackage.haskell.org/package/hpack hpack> project you must use
(note that "p1,p2,p3" are quoted):

@
verbatim:
  x-build-programs: "p1,p2,p3"
@
-}
buildProgramsCustomField :: String
buildProgramsCustomField = "x-build-programs"

{-|
This function is the easiest way to use this library in your @Setup.hs@.

The default @Setup.hs@:

@
import Distribution.Simple
main = defaultMain
@

becomes:

@
import Distribution.Simple.BuildPrograms -- <- only change
main = defaultMain
@

-}
defaultMain :: IO ()
defaultMain = defaultMainWithHooks buildProgramsUserHooks

{-|
Directly access the 'UserHooks' that back 'defaultMain' when you need finer grained control, they are backed by 'simpleUserHooks':

@
import Distribution.Simple.BuildPrograms
import Distribution.Simple(defaultMainWithHooks)
main = defaultMainWithHooks buildProgramUserHooks
@
-}
buildProgramsUserHooks :: UserHooks
buildProgramsUserHooks = overlayUserHooks simpleUserHooks

{-|
If your @Setup.hs@ uses non-default or custom 'UserHooks' ,eg. 'autoconfUserHooks' this function will augment the 'buildHook' and 'replHook' to
first check for the external programs.

@
import Distribution.Simple.BuildPrograms
import Distribution.Simple(defaultMainWithHooks,autoconfUserHooks)
main = defaultMainWithHooks (overlayUserHooks autoconfUserHooks)
@
-}
overlayUserHooks :: UserHooks -> UserHooks
overlayUserHooks hooksToOverlay =
  let bh :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
      bh pd lbi uhs bfs = do
        newLbi <- localBuildInfoWithBuildPrograms (buildArgs bfs) (fromFlag (buildVerbosity bfs)) (hookedPrograms uhs) pd lbi
        case newLbi of
          Left err -> die' (fromFlag (buildVerbosity bfs)) err
          Right newLbi' -> (buildHook hooksToOverlay) pd newLbi' uhs bfs
      rh ::  PackageDescription -> LocalBuildInfo -> UserHooks -> ReplFlags -> [String] -> IO ()
      rh pd lbi uhs rfs args = do
        newLbi <- localBuildInfoWithBuildPrograms args (fromFlag (replVerbosity rfs)) (hookedPrograms uhs) pd lbi
        case newLbi of
          Left err -> die' (fromFlag (replVerbosity rfs)) err
          Right newLbi' -> (replHook hooksToOverlay) pd newLbi' uhs rfs args
  in hooksToOverlay { buildHook = bh , replHook = rh }

splitProgramField :: String -> [String]
splitProgramField =
  let isComma t = t == ','
      isCommaOrSpace t = t == ' ' || isComma t
      split s =
        if null s
        then Nothing
        else Just (break isComma (dropWhile isCommaOrSpace s))
  in unfoldr split

programs :: [(String,String)] -> [String]
programs = nub . concatMap (splitProgramField . snd) . filter (\(field,_) -> field == buildProgramsCustomField)

customFields :: TargetInfo -> [(String,String)]
customFields = customFieldsBI . componentBuildInfo . targetComponent

{-|
Used internally it figures out which components to build, extracts the required
external programs into a lookup table. To make debugging easier I made it public
so you print the table.
-}
componentBuildPrograms :: Verbosity -> PackageDescription -> LocalBuildInfo -> Args -> IO [(Component, [String])]
componentBuildPrograms verb pd lbi args =
  let programFields :: TargetInfo -> (Component,[String])
      programFields t = (targetComponent t, programs (customFields t))
  in (map programFields . neededTargetsInBuildOrder' pd lbi . map nodeKey) <$> readTargetInfos verb pd lbi args

{-|
This is the lowest level function provided by this library for use in a highly
customized @Setup.hs@ scripts. Most of the other functions wrap it in some way.

'LocalBuildInfo' is a large datastructure that holds all the information
required to build a project, this function gathers up only the components that
need to be built ( or loaded in GHCi ), extracts the programs in the
"x-build-programs" fields, checks that they exist and adds them as
'ConfiguredProgram' to the 'ProgramDb' of the provided 'LocalBuildInfo' and
returns a new 'LocalBuildInfo' with the new 'ProgramDb'.

If any of the programs are cannot be found a formatted error message is returned.

The expectation is that it is called from a 'buildHook' or a 'replHook' at some
point before the components are built.
-}
localBuildInfoWithBuildPrograms  ::
  Args -- ^ 'buildHook' ( extracted from 'BuildFlags') or 'replHook' arguments
  -> Verbosity
  -> [Program] -- ^ The 'hookedPrograms' from 'UserHooks', used to make sure we don't double check built-in build programs like 'c2hs' or 'ghc'
  -> PackageDescription
  -> LocalBuildInfo
  -> IO (Either String LocalBuildInfo) -- ^ Local build info with the external build programs configured or an error message
localBuildInfoWithBuildPrograms args verbosity hookedPs pd lbi =
  let pdFields = programs (customFieldsPD pd)
      combineAll :: [(Component, [String])] -> [(Maybe Component,[String])]
      combineAll cs = [(Nothing,nub pdFields)] ++ map (\(c,ps) -> (Just c,ps)) cs
      addOrLog :: [String] -> ProgramDb -> [String] -> IO ([String],ProgramDb)
      addOrLog errsSoFar programDb =
        foldM (\(errPs,db) p ->
                 let logError = pure (errPs ++ [p], db)
                 in
                 if p `elem` errsSoFar
                 then logError
                 else
                   if p `elem` (map programName hookedPs)
                   then pure (errPs,db)
                   else
                     catch
                       (do
                         (_,newDb) <- requireProgram verbosity (simpleProgram p) db
                         pure (errPs, newDb))
                       (\(_ :: IOException) -> logError))
              ([],programDb)
      check :: [(Maybe Component,[String])] -> IO ([(Maybe Component, [String])],ProgramDb)
      check =
        foldM (\(compErrs,db) (comp,ps) -> do
                  (errs,newDb) <- addOrLog (concatMap snd compErrs) db ps
                  pure (compErrs ++ [(comp,errs)], newDb))
              ([],withPrograms lbi)
      compLabel :: Maybe Component -> String
      compLabel Nothing = "global"
      compLabel (Just c) = prettyShow (componentName c)
      commaSep :: [String] -> String
      commaSep = concat . intersperse ","
      printErr :: (Maybe Component, [String]) -> String
      printErr (comp,missing) = "- " ++ compLabel comp ++ ": " ++ commaSep missing
  in do
    allPrograms <- combineAll <$> componentBuildPrograms verbosity pd lbi args
    (compErrs, newDb) <- check allPrograms
    let errs = filter (not . null . snd) compErrs
    pure $
      if (not (null errs))
      then Left $ "\n" ++ (unlines ("These build programs are required but could not be found:"
                          : ("- " ++ (commaSep . nub . concatMap snd) errs)
                          : "Broken down by build component: "
                          : map printErr errs))
      else Right $ lbi { withPrograms = newDb }

-- $introduction
-- This is a Cabal library that lets you annotate your
-- @.cabal@ (or @package.yaml@) file with external programs that must be
-- available at build time. It does this using a custom Cabal field,
-- @x-build-programs@ which takes a list of executables that are checked
-- whenever the project is built or when starting GHCi (unfortunately the
-- latter does not work with Stack, see "Stack & HPack Gotchas" below). It can
-- also be specified per build component (library, executables, benchmarks, test
-- suite etc.) so that, for instance, a library does not have to depend on an
-- external program used just for benchmarking (eg. 'gprof').

-- $why
-- This library exists because currently there is no field
-- in Cabal that checks for the existence of non-Haskell programs (eg. '7z.exe'
-- or 'cmake') at build time. The closest ,
-- <https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-build-tool-depends 'build-tool-depends'>
-- only allows Haskell-buildable exectuables like 'c2hs'.

-- $gettingstarted
-- First off you will need to set up a custom build in your @.cabal@:
--
-- @
-- build-type:     Custom
-- custom-setup
--   setup-depends:
--       Cabal >=2.2.0.0 && <4
--     , base >=4.7 && <5
--     , cabal-build-programs
-- @
--
-- or @package.yaml@ file:
--
-- @
-- build-type:          Custom
-- custom-setup:
--   dependencies:
--   - base >= 4.7 && < 5
--   - Cabal >= 2.2.0.0 && < 4
--   - cabal-build-programs
-- @
--
--
-- Then add the external program dependencies, as in this example @package.yaml@:
--
-- @
-- verbatim:
--   x-build-programs: "cmake"
--
-- library:
--   ...
--   verbatim:
--     x-build-programs: "llmv-config"
--   when:
--     - condition: os(windows)
--       then:
--         verbatim:
--           x-build-programs: "7z.exe"
--       else:
--         verbatim:
--           x-build-programs: "zip"
--
-- benchmarks:
--   my-benchmarks:
--     ...
--     verbatim:
--       x-build-programs: "gprof,valgrind"
-- @
--
--
-- A project that uses the snippet above always depends on @cmake@ no matter
-- which artifact is being built but only the library needs @llvm-config@. On
-- Windows the library needs @7z.exe@ for extracting archives, otherwise @zip@
-- and the benchmarks always need the GNU profiling @gprof@ tool and @valgrind@.
--
-- Now we need a custom @Setup.hs@ that can use this library to check those
-- dependencies.
--
-- The easiest way is to use the provided 'defaultMain'. For more advanced use
-- check the docs for 'buildProgramsUserHooks', 'overlayUserHooks' and
-- 'localBuildInfoWithBuildPrograms'.
--
--

-- $stackHPackGotchas
-- <https://docs.haskellstack.org/en/stable/README/ Stack> and <https://github.com/sol/hpack Hpack> have
-- are a few gotchas to keep in mind and since Stack uses HPack it inherits all the issues as well.
--
--    * Stack seems to completely ignore the 'preRepl' and 'replHook' so
--      when using Stack external programs are not checked when using @stack
--      ghci@ or @stack repl@, @stack build@ seems to work fine.
--
--   * HPack requires that all custom fields be in a "verbatim:" block
--     so the first snippet below is rejected but the second is not:
--
-- @
-- name:                my-awesome-program
-- ...
-- x-build-programs:    "cmake,gprof,someOtherDep"
-- ...
-- @
--
-- @
-- name:                my-awesome-program
-- ...
-- verbatim:
--   x-build-programs:  "cmake,gprof,someOtherDep"
-- ...
-- @
--
--   * Unlike Cabal, HPack will not accumulate across duplicate custom fields
--     and picks the last one it encounters but only at the same level. For
--     example in the first snippet (a) will be ignored by HPack and you will
--     only see (b) in your @.cabal@ file. But in the second snippet since (a)
--     is inside a conditional it does make it in. To be fair HPack
--     will warn in the first case but it's pretty easy to miss.
--
-- @
-- executables:
--   my-awesome-executable
--     main:                Main.hs
--     verbatim:
--       x-build-programs: "cmake,prof"   (a)
--     ghc-options:
--     - ...
--     verbatim:
--       x-build-programs: "someOtherDep" (b)
-- @
--
-- @
-- executables:
--   my-awesome-executable
--     main:                Main.hs
--     when:
--     - condition: os(linux)
--       verbatim:
--         x-build-programs: "cmake,prof" (a)
--     ghc-options:
--     - ...
--     verbatim:
--       x-build-programs: "someOtherDep" (b)
-- @
--

-- $limitations
-- The biggest limitations of this library is it cannot do any kind of version
-- checking of external programs. For example, it is currently not possible to
-- depend on eg. @cmake > 3.0.0@. It simply looks around in the environment for
-- the first executable that matches the name.
--
-- The 'Program' datastructure provided by Cabal is a
-- lot richer providing a way of extracting versions and doing some post
-- configuration, see the builtin
-- <https://hackage.haskell.org/package/Cabal-3.0.0.0/docs/Distribution-Simple-Program.html#v:tarProgram tar> as
-- an example. But it is also more complicated use.
