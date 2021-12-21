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
  version = "0.16.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "14lzbhciw7ksi219lrcy9afglmg5mx0rmfvrr2x8ssghv4kf8cdy";
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
