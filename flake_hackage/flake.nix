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
  };

  outputs = {self, ...}: {};
}
