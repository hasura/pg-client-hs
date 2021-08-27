-- jberryman: This is adapted from 'postgresql-libpq'
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Version

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

import Data.Char (isSpace)
import Data.List (dropWhile,reverse)

import Distribution.Types.UnqualComponentName

flag :: String -> FlagName
flag = mkFlagName

unqualComponentName :: String -> UnqualComponentName
unqualComponentName = mkUnqualComponentName

main = defaultMainWithHooks simpleUserHooks {
  confHook = \pkg flags -> do
    -- -- jberryman: pkg-config option removed for now:
    -- if lookup (flag "use-pkg-config")
    --           (unFlagAssignment (configConfigurationsFlags flags)) == Just True
    -- then do
    --   confHook simpleUserHooks pkg flags
    -- else do
      lbi <- confHook simpleUserHooks pkg flags
      bi <- psqlBuildInfo lbi

      return lbi {
        localPkgDescr = updatePackageDescription
                          (Just bi, [(unqualComponentName "runtests", bi)]) (localPkgDescr lbi)
      }
}

psqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
psqlBuildInfo lbi = do
  (pgconfigProg, _) <- requireProgram verbosity
                         (simpleProgram "pg_config") (withPrograms lbi)
  let pgconfig = getProgramOutput verbosity pgconfigProg

  -- On Debian, --includedir and --pkgincludedir return the path
  -- "/usr/include/postgresql" but on Arch Linux, they are respectively
  -- "/usr/include" and "/usr/include/postgresql".  We need to have
  -- "/usr/include/postgresql" as part of our includes for "internal/postgres_fe.h"
  incDir <- pgconfig ["--includedir"]
  pkgIncDir <- pgconfig ["--pkgincludedir"]
  libDir <- pgconfig ["--libdir"]
  -- jberryman: this we need for libpq-int.h:
  cppflags <- pgconfig ["--cppflags"]
  cflags <- pgconfig ["--cflags"]

  return emptyBuildInfo {
    extraLibDirs = [strip libDir],
    includeDirs  = [strip incDir, strip pkgIncDir],
    -- jberryman: added:
    ccOptions = words cppflags <> words cflags
  }
  where
    verbosity = normal -- honestly, this is a hack
    strip x = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse x
