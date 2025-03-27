{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.Maybe

import Data.Aeson hiding ( encode )
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Key as K
import Data.Yaml

import qualified Data.ByteString as BS

import qualified Data.List as L

import System.Directory
import System.FilePath
import System.Environment

-------------------------------------------------------------------------------
-- Configuration parameters
-------------------------------------------------------------------------------

data Opsys
  = Linux Distro
  | Darwin
  | Windows deriving (Eq)

osName :: Opsys -> String
osName Darwin = "mac"
osName Windows = "windows"
osName (Linux d) = "linux-" ++ distroName d

data Distro
  = Debian9
  | Debian10
  | Debian11
  | Ubuntu1804
  | Ubuntu2004
  | Ubuntu2204
  | Mint193
  | Mint202
  | Fedora27
  | Fedora33
  | Centos7
  | Rocky8
  deriving (Eq, Enum, Bounded)

allDistros :: [Distro]
allDistros = [minBound .. maxBound]

data Arch = Amd64 | AArch64
archName :: Arch -> String
archName Amd64 = "x86_64"
archName AArch64 = "aarch64"

artifactName :: Arch -> Opsys -> String
artifactName arch opsys = archName arch ++ "-" ++ case opsys of
  Linux distro -> "linux-" ++ distroName distro
  Darwin -> "apple-darwin"
  Windows -> "mingw64"

data GHC
  = GHC948
  | GHC967
  | GHC984
  | GHC9101
  | GHC9122
  deriving (Eq, Enum, Bounded)

ghcVersion :: GHC -> String
ghcVersion GHC948 = "9.4.8"
ghcVersion GHC967 = "9.6.7"
ghcVersion GHC984 = "9.8.4"
ghcVersion GHC9101 = "9.10.1"
ghcVersion GHC9122 = "9.12.2"

ghcVersionIdent :: GHC -> String
ghcVersionIdent = filter (/= '.') . ghcVersion

allGHCs :: [GHC]
allGHCs = [minBound .. maxBound]

data Stage = Build GHC | Bindist | Test

-------------------------------------------------------------------------------
-- Distro Configuration
-------------------------------------------------------------------------------

distroImage :: Distro -> String
distroImage Debian9 = "debian:9"
distroImage Debian10 = "debian:10"
distroImage Debian11 = "debian:11"
distroImage Ubuntu1804 = "ubuntu:18.04"
distroImage Ubuntu2004 = "ubuntu:20.04"
distroImage Ubuntu2204 = "ubuntu:22.04"
distroImage Mint193 = "linuxmintd/mint19.3-amd64"
distroImage Mint202 = "linuxmintd/mint20.2-amd64"
distroImage Fedora27 = "fedora:27"
distroImage Fedora33 = "fedora:33"
distroImage Centos7 = "centos:7"
distroImage Rocky8 = "rockylinux:8"

distroName :: Distro -> String
distroName Debian9 = "deb9"
distroName Debian10 = "deb10"
distroName Debian11 = "deb11"
distroName Ubuntu1804 = "ubuntu1804"
distroName Ubuntu2004 = "ubuntu2004"
distroName Ubuntu2204 = "ubuntu2204"
distroName Mint193 = "mint193"
distroName Mint202 = "mint202"
distroName Fedora27 = "fedora27"
distroName Fedora33 = "fedora33"
distroName Centos7 = "centos7"
distroName Rocky8 = "unknown"

distroInstall :: Distro -> String
distroInstall Debian9    = "sed -i s/deb.debian.org/archive.debian.org/g /etc/apt/sources.list && sed -i 's|security.debian.org|archive.debian.org/|g' /etc/apt/sources.list && sed -i /-updates/d /etc/apt/sources.list && apt-get update && apt-get install -y"
distroInstall Debian10   = "apt-get update && apt-get install -y"
distroInstall Debian11   = "apt-get update && apt-get install -y"
distroInstall Ubuntu1804 = "apt-get update && apt-get install -y"
distroInstall Ubuntu2004 = "apt-get update && apt-get install -y"
distroInstall Ubuntu2204 = "apt-get update && apt-get install -y"
distroInstall Mint193    = "apt-get update && apt-get install -y"
distroInstall Mint202    = "apt-get update && apt-get install -y"
distroInstall Fedora27   = "dnf install -y"
distroInstall Fedora33   = "dnf install -y"
distroInstall Centos7    = "sed -i 's/mirrorlist/#mirrorlist/g' /etc/yum.repos.d/CentOS-* && sed -i 's|#baseurl=http://mirror.centos.org|baseurl=http://vault.centos.org|g' /etc/yum.repos.d/CentOS-* && yum -y install epel-release && yum install -y"
distroInstall Rocky8     = "yum -y install epel-release && yum install -y --allowerasing"

distroTools :: Distro -> String
distroTools Debian9    = "libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip libffi-dev libncurses-dev libncurses5 libtinfo5 patchelf"
distroTools Debian10   = "libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip libffi-dev libncurses-dev libncurses5 libtinfo5 patchelf"
distroTools Debian11   = "libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip libffi-dev libncurses-dev libncurses5 libtinfo5 patchelf"
distroTools Ubuntu1804 = "libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip libffi-dev libncurses-dev libncurses5 libtinfo5 patchelf"
distroTools Ubuntu2004 = "libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip libffi-dev libncurses-dev libncurses5 libtinfo5 patchelf"
distroTools Ubuntu2204 = "libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip libffi-dev libncurses-dev libncurses5 libtinfo5 patchelf"
distroTools Mint193    = "libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip libffi-dev libncurses-dev libncurses5 libtinfo5 patchelf"
distroTools Mint202    = "libnuma-dev zlib1g-dev libgmp-dev libgmp10 libssl-dev liblzma-dev libbz2-dev git wget lsb-release software-properties-common gnupg2 apt-transport-https gcc autoconf automake build-essential curl ghc gzip libffi-dev libncurses-dev libncurses5 libtinfo5 patchelf"
distroTools Fedora27   = "autoconf automake binutils bzip2 coreutils curl elfutils-devel elfutils-libs findutils gcc gcc-c++ git gmp gmp-devel jq lbzip2 make ncurses ncurses-compat-libs ncurses-devel openssh-clients patch perl pxz python3 sqlite sudo wget which xz zlib-devel patchelf"
distroTools Fedora33   = "autoconf automake binutils bzip2 coreutils curl elfutils-devel elfutils-libs findutils gcc gcc-c++ git gmp gmp-devel jq lbzip2 make ncurses ncurses-compat-libs ncurses-devel openssh-clients patch perl pxz python3 sqlite sudo wget which xz zlib-devel patchelf"
distroTools Centos7    = "autoconf automake binutils bzip2 coreutils curl elfutils-devel elfutils-libs findutils gcc gcc-c++ git gmp gmp-devel jq lbzip2 make ncurses ncurses-compat-libs ncurses-devel openssh-clients patch perl pxz python3 sqlite sudo wget which xz zlib-devel patchelf"
distroTools Rocky8     = "autoconf automake binutils bzip2 coreutils curl elfutils-devel elfutils-libs findutils gcc gcc-c++ git gmp gmp-devel jq lbzip2 make ncurses ncurses-compat-libs ncurses-devel openssh-clients patch perl pxz python3 sqlite sudo wget which xz zlib-devel patchelf"

-------------------------------------------------------------------------------
-- OS/runner Config
-------------------------------------------------------------------------------

baseEnv :: [(Key,Value)]
baseEnv = [ "AWS_SECRET_ACCESS_KEY" .= str "${{ secrets.AWS_SECRET_ACCESS_KEY }}"
          , "AWS_ACCESS_KEY_ID" .= str "${{ secrets.AWS_ACCESS_KEY_ID }}"
          , "S3_HOST" .= str "${{ secrets.S3_HOST }}"
          , "TZ" .= str "Asia/Singapore"
          ]

-- | Environment configuration
envVars :: Arch -> Opsys -> Value
envVars arch os = object $
     baseEnv
  ++ [ "TARBALL_EXT" .= str (case os of
          Windows -> "zip"
          _ -> "tar.xz")
     , "ARCH" .= str (case arch of
         Amd64 -> "64"
         AArch64 -> "ARM64")
     , "ADD_CABAL_ARGS" .= str (case (os,arch) of
        (Linux _, Amd64) -> "--enable-split-sections"
        _ -> "")
     , "ARTIFACT" .= artifactName arch os
     ]
  ++ [ "DEBIAN_FRONTEND" .= str "noninteractive"
     | Linux _ <- [os]
     ]
  ++ [ "MACOSX_DEPLOYMENT_TARGET" .= str "10.13"
     | Darwin <- [os]
     ]
  ++ [ "HOMEBREW_CHANGE_ARCH_TO_ARM" .= str "1"
     | Darwin <- [os], AArch64 <- [arch]
     ]

-- | Runner selection
runner :: Arch -> Opsys -> [Value]
runner Amd64 (Linux _) = ["ubuntu-latest"]
runner AArch64 (Linux _) = ["self-hosted", "Linux", "ARM64", "maerwald"]
runner Amd64 Darwin = ["macOS-13"]
runner AArch64 Darwin = ["self-hosted", "macOS", "ARM64"]
runner Amd64 Windows = ["windows-latest"]
runner AArch64 Windows = error "aarch64 windows not supported"

-- | Runner selection for bindist jobs
bindistRunner :: Arch -> Opsys -> [Value]
bindistRunner Amd64 (Linux _) = ["self-hosted", "linux-space", "maerwald"]
bindistRunner AArch64 (Linux _) = ["self-hosted", "Linux", "ARM64", "maerwald"]
bindistRunner Amd64 Darwin = ["macOS-13"]
bindistRunner AArch64 Darwin = ["self-hosted", "macOS", "ARM64"]
bindistRunner Amd64 Windows = ["windows-latest"]
bindistRunner AArch64 Windows = error "aarch64 windows not supported"

-------------------------------------------------------------------------------
-- Action generatation
-------------------------------------------------------------------------------
-- Each x86-linux job has its own action, living in a separate file
-- The contents of the file are derived from the 'Action' datatype
--
-- We do this so that we can run the build in the right kind of OS container,
-- but not be forced to run the checkout and upload artifact in the same container
--
-- This is because we want to use container images that are not supported by
-- github provided actions, see for instance https://github.com/actions/upload-artifact/issues/489
-------------------------------------------------------------------------------

-- | Container actions for x86-linux runners.
-- Each of these corresponds to a separate action file,
-- called 'actionName', located at 'actionPath'
data Action
  = Action
  { actionName  :: String
  , actionDistro :: Distro
  }

actionDir :: FilePath
actionDir = "./.github/actions/bindist-actions/"

actionPath :: Distro -> FilePath
actionPath d = actionDir ++ distroActionName d

instance ToJSON Action where
  toJSON Action{..} = object
    [ "name" .= actionName
    , "description" .= str ("Container for " ++ distroName actionDistro)
    , "inputs" .= object
      [ "stage" .= object
        [ "description" .= str "which stage to build"
        , "required" .= True
        ]
        , "version" .= object
        [ "description" .= str "which GHC version to build/test"
        , "required" .= False
        ]
      ]
    , "runs" .= object
       [ "using" .= str "docker"
       , "image" .= distroImage actionDistro
       , "entrypoint" .= str ".github/scripts/entrypoint.sh"
       , "env" .= object
          [ "STAGE" .= str "${{ inputs.stage }}"
          , "INSTALL" .= distroInstall actionDistro
          , "TOOLS" .= distroTools actionDistro
          , "GHC_VERSION" .= str "${{ inputs.version }}"
          ]
       ]
    ]

configAction :: Config -> Maybe Action
configAction (MkConfig Amd64 (Linux d) _) = Just $ Action (distroActionName d) d
configAction _ = Nothing

distroActionName :: Distro -> String
distroActionName d = "action-" ++ distroName d

customAction :: Distro -> Stage -> Value
customAction d st = flip (ghAction stepName (actionPath d)) [] $ case st of
  Build v ->
    [ "stage" .= str "BUILD"
    , "version" .= ghcVersion v
    ]
  Test ->
    [ "stage" .= str "TEST"
    ]
  Bindist ->
    [ "stage" .= str "BINDIST"
    ]
  where
    stepName = case st of
      Build v -> "Build " ++ ghcVersion v
      Test -> "Test"
      Bindist -> "Bindist"

-------------------------------------------------------------------------------
-- CI generation
-------------------------------------------------------------------------------
-- This is the code that generates the bindist workflow

-- | Global CI config type
data CI = CI [Config]

data Config = MkConfig Arch Opsys [GHC]

instance ToJSON CI where
  toJSON (CI cs) = object
    [ "name" .= str "Build and release"
    , "on" .= object [ "push" .=      object ["tags" .= [str "*"]]
                      , "schedule" .= [object ["cron" .= str "0 2 * * 1"]]
                     ]
    , "env" .= object
      [ "CABAL_CACHE_DISABLE" .= str "${{ vars.CABAL_CACHE_DISABLE }}"
      , "CABAL_CACHE_NONFATAL" .= str "${{ vars.CABAL_CACHE_NONFATAL }}"
      ]
    , "jobs" .= object (concatMap (getConfigJobs . makeJobs) cs ++ [releaseJob cs])
    ]

type Job = Pair

data ConfigJobs = ConfigJobs { buildJobs :: [Job], bindistJob :: Job, testJob :: Job}

getConfigJobs :: ConfigJobs -> [Job]
getConfigJobs ConfigJobs{..} = buildJobs ++ [bindistJob, testJob]

makeJobs :: Config -> ConfigJobs
makeJobs (MkConfig arch os vs) =
  ConfigJobs
  { buildJobs = [ buildJob arch os ver | ver <- vs ]
  , bindistJob = mkBindistJob arch os vs
  , testJob = mkTestJob arch os
  }

buildJobName :: Arch -> Opsys -> GHC -> String
buildJobName arch os version = L.intercalate "-" ["build",archName arch, osName os, ghcVersionIdent version]

testJobName :: Arch -> Opsys -> String
testJobName arch os = L.intercalate "-" ["test",archName arch, osName os]

bindistJobName :: Arch -> Opsys -> String
bindistJobName arch os = L.intercalate "-" ["bindist",archName arch, osName os]

bindistName :: Arch -> Opsys -> String
bindistName arch os = "bindist-" ++ artifactName arch os

setupAction :: Arch -> Opsys -> [Value]
-- some
setupAction AArch64 (Linux Ubuntu2004) =
  [ ghRun "clean and git config for aarch64-linux" "bash" [] $ unlines
      [ "find . -mindepth 1 -maxdepth 1 -exec rm -rf -- {} +"
      , "git config --global --get-all safe.directory | grep '^\\*$' || git config --global --add safe.directory \"*\""
      ]
  ]
setupAction _ _ = []

releaseJob :: [Config] -> Job
releaseJob cs =
  "release" .= object
    [ "name" .= str "release"
    , "runs-on" .= str "ubuntu-latest"
    , "needs" .= [testJobName arch os | MkConfig arch os _ <- cs]
    , "if" .= str "startsWith(github.ref, 'refs/tags/')"
    , "steps" .= ( [ checkoutAction ]
                ++ [ downloadArtifacts (bindistName arch os) "./out" | MkConfig arch os _ <- cs]
                ++ [ ghRun "Prepare release" "bash" [] $ unlines
                      [ "sudo apt-get update && sudo apt-get install -y tar xz-utils"
                      , "cd out/plan.json"
                      , "tar cf plan_json.tar *"
                      , "mv plan_json.tar ../"
                      , "cd ../.."
                      , "export RELEASE=$GITHUB_REF_NAME"
                      , "git archive --format=tar.gz -o \"out/haskell-language-server-${RELEASE}-src.tar.gz\" --prefix=\"haskell-language-server-${RELEASE}/\" HEAD"
                      ]
                   , ghAction "Release" "softprops/action-gh-release@v2"
                       [ "draft" .= True
                       , "files" .= unlines
                            [ "./out/*.zip"
                            , "./out/*.tar.xz"
                            , "./out/*.tar.gz"
                            , "./out/*.tar"
                            ]
                       ] []
                   ])
    ]



buildJob :: Arch -> Opsys -> GHC -> Job
buildJob arch os v =
  K.fromString (buildJobName arch os v) .= object
    [ "runs-on" .= runner arch os
    , "name" .= str (buildJobName arch os v ++ " (Build binaries)")
    , "environment" .= str "CI"
    , "env" .= thisEnv
    , "steps" .=
           ( setupAction arch os
          ++ [ checkoutAction ]
          ++ buildStep arch os
          ++ [uploadArtifacts ("artifacts-"++buildJobName arch os v) outputname])
    ]

  where thisEnv = envVars arch os
        art = artifactName arch os
        outputname
          | Windows <- os = "./out/*"
          | otherwise = ("out-"++art++"-"++ghcVersion v++".tar")
        buildStep Amd64 (Linux d) = [customAction d (Build v)]
        buildStep AArch64 (Linux Ubuntu2004) =
          [ ghAction "Build aarch64-linux binaries" "docker://hasufell/arm64v8-ubuntu-haskell:focal"
              [ "args" .= str "bash .github/scripts/build.sh" ]
              [ "GHC_VERSION" .= ghcVersion v ]
          , ghAction "Tar aarch64-linux binaries" "docker://hasufell/arm64v8-ubuntu-haskell:focal"
              [ "args" .= str "bash .github/scripts/tar.sh" ]
              [ "GHC_VERSION" .= ghcVersion v ]
          ]
        buildStep AArch64 (Linux _) = error "aarch64-linux non-ubuntu not supported"

        buildStep Amd64 Darwin = [ghRun "Run build" "sh" ["GHC_VERSION" .= ghcVersion v] $ unlines $
          [ "brew install coreutils tree"
          , "bash .github/scripts/build.sh"
          , "tar cf out-${ARTIFACT}-${GHC_VERSION}.tar out/ store/"
          ]
          ]
        buildStep AArch64 Darwin = [ghRun "Run build" "sh" ["GHC_VERSION" .= ghcVersion v] $ unlines $
          [ "bash .github/scripts/brew.sh git coreutils autoconf automake tree"
          , "export PATH=\"$HOME/.brew/bin:$HOME/.brew/sbin:$PATH\""
          , "export LD=ld"
          , "bash .github/scripts/build.sh"
          , "tar cf out-${ARTIFACT}-${GHC_VERSION}.tar out/ store/"
          ]
          ]

        buildStep Amd64 Windows = [ghRun "Run build" "pwsh" ["GHC_VERSION" .= ghcVersion v] $ unlines $
          [ "$env:CHERE_INVOKING = 1"
          , "$env:MSYS2_PATH_TYPE = \"inherit\""
          , "$ErrorActionPreference = \"Stop\""
          , "C:\\msys64\\usr\\bin\\bash -lc \"bash .github/scripts/build.sh\""
          ]
          ]
        buildStep AArch64 Windows = error "aarch64 windows not supported"

mkBindistJob :: Arch -> Opsys -> [GHC] -> Job
mkBindistJob arch os vs =
  K.fromString (bindistJobName arch os) .= object
      [ "runs-on" .= bindistRunner arch os
      , "name" .= (bindistJobName arch os ++ " (Prepare bindist)")
      , "needs" .= [buildJobName arch os ver | ver <- vs]
      , "env" .= thisEnv
      , "steps" .=
        (  setupAction arch os
        ++ [ checkoutAction ]
        ++ [downloadArtifacts ("artifacts-"++buildJobName arch os v) outputPath | v <- vs]
        ++ bindistStep arch os
        ++ [ uploadArtifacts (bindistName arch os) "./out/*.tar.xz\n./out/plan.json/*\n./out/*.zip" ])
      ]
  where thisEnv = envVars arch os

        outputPath
          | Windows <- os = "./out"
          | otherwise = "./"

        bindistStep Amd64 (Linux d) = [customAction d Bindist]
        bindistStep AArch64 (Linux Ubuntu2004) =
          [ ghAction "Unpack aarch64-linux binaries" "docker://hasufell/arm64v8-ubuntu-haskell:focal"
              [ "args" .= str "bash .github/scripts/untar.sh" ]
              [ ]
          , ghAction "Tar aarch64-linux binaries" "docker://hasufell/arm64v8-ubuntu-haskell:focal"
              [ "args" .= str "bash .github/scripts/bindist.sh" ]
              [ ]
          ]
        bindistStep AArch64 (Linux _) = error "aarch64-linux non-ubuntu not supported"

        bindistStep Amd64 Darwin = [ghRun "Create bindist" "sh" [] $ unlines $
          [ "brew install coreutils tree"
          , "for bindist in out-*.tar ; do"
          , "    tar xf \"${bindist}\""
          , "done"
          , "unset bindist"
          , "bash .github/scripts/bindist.sh"
          ]
          ]
        bindistStep AArch64 Darwin = [ghRun "Run build" "sh" [] $ unlines $
          [ "bash .github/scripts/brew.sh git coreutils llvm@13 autoconf automake tree"
          , "export PATH=\"$HOME/.brew/bin:$HOME/.brew/sbin:$HOME/.brew/opt/llvm@13/bin:$PATH\""
          , "export CC=\"$HOME/.brew/opt/llvm@13/bin/clang\""
          , "export CXX=\"$HOME/.brew/opt/llvm@13/bin/clang++\""
          , "export LD=ld"
          , "export AR=\"$HOME/.brew/opt/llvm@13/bin/llvm-ar\""
          , "export RANLIB=\"$HOME/.brew/opt/llvm@13/bin/llvm-ranlib\""
          , "for bindist in out-*.tar ; do"
          , "    tar xf \"${bindist}\""
          , "done"
          , "unset bindist"
          , "bash .github/scripts/bindist.sh"
          ]
          ]

        bindistStep Amd64 Windows = [ghRun "Run build" "pwsh" [] $ unlines $
          [ "C:\\msys64\\usr\\bin\\bash -lc \"pacman --disable-download-timeout --noconfirm -Syuu\""
          , "C:\\msys64\\usr\\bin\\bash -lc \"pacman --disable-download-timeout --noconfirm -Syuu\""
          , "C:\\msys64\\usr\\bin\\bash -lc \"pacman --disable-download-timeout --noconfirm -S unzip zip git\""
          , "taskkill /F /FI \"MODULES eq msys-2.0.dll\""
          , "$env:CHERE_INVOKING = 1"
          , "$env:MSYS2_PATH_TYPE = \"inherit\""
          , "C:\\msys64\\usr\\bin\\bash -lc \"bash .github/scripts/bindist.sh\""
          ]
          ]
        bindistStep AArch64 Windows = error "aarch64 windows not supported"

mkTestJob :: Arch -> Opsys -> Job
mkTestJob arch os =
  K.fromString (testJobName arch os) .= object
      [ "runs-on" .= runner arch os
      , "name" .= str (testJobName arch os ++ " (Test binaries)")
      , "needs" .= [bindistJobName arch os]
      , "environment" .= str "CI"
      , "env" .= thisEnv
      , "steps" .=
          (  setupAction arch os
          ++ [ checkoutAction , downloadArtifacts (bindistName arch os) "./out" ]
          ++ testStep arch os)
     ]
  where thisEnv = envVars arch os

        testStep Amd64 (Linux d) = [customAction d Test]
        testStep AArch64 (Linux Ubuntu2004) =
          [ ghAction "Run test" "docker://hasufell/arm64v8-ubuntu-haskell:focal"
              [ "args" .= str "bash .github/scripts/test.sh" ]
              [ ]
          ]
        testStep AArch64 (Linux _) = error "aarch64-linux non-ubuntu not supported"

        testStep Amd64 Darwin = [ghRun "Run test" "sh" [] $ unlines $
          [ "brew install coreutils tree"
          , "bash .github/scripts/test.sh"
          ]
          ]
        testStep AArch64 Darwin = [ghRun "Run test" "sh" [] $ unlines $
          [ "bash .github/scripts/brew.sh git coreutils llvm@13 autoconf automake tree"
          , "export PATH=\"$HOME/.brew/bin:$HOME/.brew/sbin:$HOME/.brew/opt/llvm@13/bin:$PATH\""
          , "export CC=\"$HOME/.brew/opt/llvm@13/bin/clang\""
          , "export CXX=\"$HOME/.brew/opt/llvm@13/bin/clang++\""
          , "export LD=ld"
          , "export AR=\"$HOME/.brew/opt/llvm@13/bin/llvm-ar\""
          , "export RANLIB=\"$HOME/.brew/opt/llvm@13/bin/llvm-ranlib\""
          , "bash .github/scripts/test.sh"
          ]
          ]

        testStep Amd64 Windows =
          [ ghRun "install windows deps" "pwsh" [] $ unlines $
              [ "C:\\msys64\\usr\\bin\\bash -lc \"pacman --disable-download-timeout --noconfirm -Syuu\""
              , "C:\\msys64\\usr\\bin\\bash -lc \"pacman --disable-download-timeout --noconfirm -Syuu\""
              , "C:\\msys64\\usr\\bin\\bash -lc \"pacman --disable-download-timeout --noconfirm -S make mingw-w64-x86_64-clang curl autoconf mingw-w64-x86_64-pkgconf ca-certificates base-devel gettext autoconf make libtool automake python p7zip patch unzip zip git\""
              , "taskkill /F /FI \"MODULES eq msys-2.0.dll\""
              ]
          , ghRun "Run test" "pwsh" [] $ unlines $
              [ "$env:CHERE_INVOKING = 1"
              , "$env:MSYS2_PATH_TYPE = \"inherit\""
              , "C:\\msys64\\usr\\bin\\bash -lc \"bash .github/scripts/test.sh\""
              ]
          ]
        testStep AArch64 Windows = error "aarch64 windows not supported"


ciConfigs :: [Config]
ciConfigs =
  [ MkConfig Amd64 Darwin allGHCs
  , MkConfig AArch64 Darwin allGHCs
  , MkConfig Amd64 Windows allGHCs
  , MkConfig AArch64 (Linux Ubuntu2004) allGHCs]
  ++ [ MkConfig Amd64 (Linux distro) allGHCs | distro <- allDistros ]

main :: IO ()
main = do
  [root] <- getArgs
  setCurrentDirectory root
  removeDirectoryRecursive actionDir
  createDirectoryIfMissing True actionDir
  forM_ (mapMaybe configAction ciConfigs) $ \a -> do
    let path = actionPath (actionDistro a)
    createDirectoryIfMissing True path
    BS.writeFile (path </> "action.yaml") $ encode a
  BS.putStr "### DO NOT EDIT - GENERATED FILE\n"
  BS.putStr "### This file was generated by ./.github/generate-ci/gen_ci.hs\n"
  BS.putStr "### Edit that file and run ./.github/generate-ci/generate-jobs to regenerate\n"
  BS.putStr $ encode $ CI ciConfigs


-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

str :: String -> String
str = id

ghAction :: String -> String -> [(Key,Value)] -> [(Key,Value)] -> Value
ghAction name uses args env = object $
  [ "name" .= name
  , "uses" .= uses
  ]
  ++ case args of
    [] -> []
    xs -> [ "with" .= object xs ]
  ++ case env of
       [] -> []
       xs -> [ "env" .= object xs ]

ghRun :: String -> String -> [(Key,Value)] -> String -> Value
ghRun name shell env script = object $
  [ "name" .= name
  , "shell" .= shell
  , "run" .= script
  ]
  ++ case env of
       [] -> []
       xs -> [ "env" .= object xs ]

checkoutAction :: Value
checkoutAction = ghAction "Checkout" "actions/checkout@v4" [] []

uploadArtifacts :: String -> String -> Value
uploadArtifacts name path = ghAction "Upload artifact" "actions/upload-artifact@v4"
  [ "if-no-files-found" .= str "error"
  , "retention-days" .= (2 :: Int)
  , "name" .= name
  , "path" .= path
  ] []

downloadArtifacts :: String -> String -> Value
downloadArtifacts name path = ghAction "Download artifacts" "actions/download-artifact@v4" [ "name" .= name, "path" .= path ] []
