# Qualify Imported Names

![Qualify Imported Names Demo](qualify-imported-names-demo.gif)

## Summary

Rewrite imported names to be qualified.

## Motivation

You've imported a number of modules, and have written a lot of code with unqualified names. You want to import a new module but you know there will be a number name clashes so you want to switch your current usage of names to be qualified.

It would be nice if you could change
```
import Blah
```
to
```
import Blah as Bloo
```
then magically qualify all the previous names imported from `Blah` with `Bloo`. After doing that you could then change
```
import Blah as Bloo
```
to
```
import qualified Blah as Bloo
```
and import your the new module using names from it without worry.

Well, now you can...

## Usage

1. Put cursor over the import declaration you want to qualify names from.
2. Initiate a Code Action.
3. Select `Qualify imported names`.

## Features
- Names are qualified on a per-import declaration basis.
- Names are qualified by the imported module's alias if it has one, otherwise by the imported module's name.
- If the import declaration has an explicit import list then the plugin will qualify only names on the list.
- If the import declaration has an explicit hiding list then the plugin will qualify names from the imported module that are not on the list.

## Future possibilities
- It may be possible to use the LSP rename functionality to ask for a name so that we don't have to do the `as Alias` dance.

## Change log
### 1.0.0.1
- GHC 9.2.1 compatibility
### 1.0.0.0
- Released...
