{ lib
, buildPythonPackage
, fetchPypi
, markdown-it-py
, mdit-py-plugins
, pyyaml
, docutils
, sphinx
}:

buildPythonPackage rec {
  pname = "myst-parser";
  version = "0.15.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "00qnpkfjrn7bqbazm20n3zcci05803cfncvrlvk0rhsbdjiphg3w";
  };

  propagatedBuildInputs = [
    markdown-it-py
    mdit-py-plugins
    pyyaml
    docutils
    sphinx
  ];

  meta = with lib; {
    description = "An extended commonmark compliant parser, with bridges to docutils & sphinx";
    homepage = https://github.com/executablebooks/MyST-Parser;
    license = licenses.mit;
  };
}
