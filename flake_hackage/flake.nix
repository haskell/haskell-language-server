{
  description = "Sources from hackage";

  inputs = {
    lsp = {
      url = "https://hackage.haskell.org/package/lsp-1.4.0.0/lsp-1.4.0.0.tar.gz";
      flake = false;
    };
    lsp-types = {
      url = "https://hackage.haskell.org/package/lsp-types-1.4.0.0/lsp-types-1.4.0.0.tar.gz";
      flake = false;
    };
    lsp-test = {
      url = "https://hackage.haskell.org/package/lsp-test-0.14.0.2/lsp-test-0.14.0.2.tar.gz";
      flake = false;
    };
    ghc-exactprint = {
      url = "https://hackage.haskell.org/package/ghc-exactprint-1.4.1/ghc-exactprint-1.4.1.tar.gz";
      flake = false;
    };

    constraints-extras = {
      url = "https://hackage.haskell.org/package/constraints-extras-0.3.2.1/constraints-extras-0.3.2.1.tar.gz";
      flake = false;
    };

    retrie = {
      url = "https://hackage.haskell.org/package/retrie-1.2.0.1/retrie-1.2.0.1.tar.gz";
      flake = false;
    };

    fourmolu = {
      url = "https://hackage.haskell.org/package/fourmolu-0.5.0.1/fourmolu-0.5.0.1.tar.gz";
      flake = false;
    };

    hlint = {
      url = "https://hackage.haskell.org/package/hlint-3.3.6/hlint-3.3.6.tar.gz";
      flake = false;
    };
  };

  outputs = {self, ...}: {};
}
