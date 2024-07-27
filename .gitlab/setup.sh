case "$(uname -s)" in
    "Darwin"|"darwin")
        nix build -f $CI_PROJECT_DIR/.gitlab/darwin/toolchain.nix --argstr system "$NIX_SYSTEM" -o toolchain.sh
        cat toolchain.sh
        source toolchain.sh
        unset MACOSX_DEPLOYMENT_TARGET
        # Precautious since we want to use ghc from ghcup
        unset GHC
        ;;
esac

