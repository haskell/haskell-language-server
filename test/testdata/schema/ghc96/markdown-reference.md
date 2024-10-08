## ghcide-completions
| Property | Description | Default |
| --- | --- | --- |
| autoExtendOn | Extends the import list automatically when completing a out-of-scope identifier | No |
| snippetsOn | Inserts snippets when using code completions | No |

## semanticTokens
| Property | Description | Default |
| --- | --- | --- |
| variableToken | LSP semantic token type to use for variables | Yes
| functionToken | LSP semantic token type to use for functions | Yes
| dataConstructorToken | LSP semantic token type to use for data constructors | Yes
| typeVariableToken | LSP semantic token type to use for type variables | Yes
| classMethodToken | LSP semantic token type to use for typeclass methods | Yes
| patternSynonymToken | LSP semantic token type to use for pattern synonyms | Yes
| typeConstructorToken | LSP semantic token type to use for type constructors | Yes
| classToken | LSP semantic token type to use for typeclasses | Yes
| typeSynonymToken | LSP semantic token type to use for type synonyms | Yes
| typeFamilyToken | LSP semantic token type to use for type families | Yes
| recordFieldToken | LSP semantic token type to use for record fields | Yes
| operatorToken | LSP semantic token type to use for operators | Yes
| moduleToken | LSP semantic token type to use for modules | Yes

## fourmolu
| Property | Description | Default |
| --- | --- | --- |
| external | Call out to an external "fourmolu" executable, rather than using the bundled library. | No |
| path | Set path to executable (for "external" mode). | No |

## cabal-gild
| Property | Description | Default |
| --- | --- | --- |
| path | Set path to 'cabal-gild' executable | No |

## hlint
| Property | Description | Default |
| --- | --- | --- |
| flags | Flags used by hlint | No |

## ormolu
| Property | Description | Default |
| --- | --- | --- |
| external | Call out to an external "ormolu" executable, rather than using the bundled library | No |

## ghcide-type-lenses
| Property | Description | Default |
| --- | --- | --- |
| mode | Control how type lenses are shown | Yes

## cabal-fmt
| Property | Description | Default |
| --- | --- | --- |
| path | Set path to 'cabal-fmt' executable | No |

## eval
| Property | Description | Default |
| --- | --- | --- |
| exception | Enable marking exceptions with `*** Exception:` similarly to doctest and GHCi. | No |
| diff | Enable the diff output (WAS/NOW) of eval lenses | No |

## rename
| Property | Description | Default |
| --- | --- | --- |
| crossModule | Enable experimental cross-module renaming | No |


