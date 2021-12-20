# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))

import re
import sys

# -- Project information -----------------------------------------------------

project = 'haskell-language-server'

# We want to take some of the metadata from the Cabal file, especially the version.
# (otherwise it's very easy to forget to update it!)
release = None
copyright = None
author = None
versionPattern = re.compile("^version:\s*([\d.]+)")
copyrightPattern = re.compile("^copyright:\s*(.+)")
authorPattern = re.compile("^author:\s*(.+)")
for i, line in enumerate(open('../haskell-language-server.cabal')):
    versionMatch = re.search(versionPattern, line)
    if versionMatch:
        release = versionMatch.group(1)
    copyrightMatch = re.search(copyrightPattern, line)
    if copyrightMatch:
        copyright = copyrightMatch.group(1)
    authorMatch = re.search(authorPattern, line)
    if authorMatch:
        author = authorMatch.group(1)

if not release:
    print("Couldn't find version")
    sys.exit()
if not copyright:
    print("Couldn't find copyright")
    sys.exit()
if not author:
    print("Couldn't find author")
    sys.exit()

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'myst_parser',
    'sphinx_rtd_theme'
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'
html_logo = "logos/logo-64.png"
html_favicon = "logos/logo.svg"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
