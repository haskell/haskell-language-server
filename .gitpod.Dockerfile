FROM gitpod/workspace-full

RUN sudo install-packages build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 \
        libncurses-dev libncurses5 libtinfo5 && \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh && \
    echo 'source $HOME/.ghcup/env' >> $HOME/.bashrc && \
    echo 'export PATH=$HOME/.cabal/bin:$HOME/.local/bin:$PATH' >> $HOME/.bashrc && \
    . /home/gitpod/.ghcup/env && \
    ghcup install ghc 8.6.5 && \
    ghcup install ghc 8.8.4 && \
    ghcup install ghc 8.10.7 && \
    ghcup install ghc 9.0.2 && \
    ghcup install ghc 9.2.2 && \
    ghcup install ghc 9.2.3 --set && \
    ghcup install hls --set && \
    ghcup install cabal --set && \
    ghcup install stack --set && \
    cabal update && \
    cabal install --constraint "stylish-haskell +ghc-lib" stylish-haskell implicit-hie hoogle && \
    pip install pre-commit && \
    npm install -g http-server
