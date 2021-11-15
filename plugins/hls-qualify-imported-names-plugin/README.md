# Qualify Imported Names

## Demo

![Qualify Imported Names Demo](qualify-imported-names-demo.gif)

## Usage

1. Put cursor over the import declaration you want to qualify names from.
2. Initiate a code action.
3. Select "Qualify imported names".

## Features
- Names are qualified on a per import declaration basis.
- If the import declaration has an explicit import list then the plugin will qualify only names on that list.
- If an import declaration has an explicit hiding import list then the plugin will qualify names from the imported module not on that list.
- Names will be qualified by the imported module's alias, if the module import is not aliased, then by the module's name.
 
## Change log
### 1.0.0.0
- Released...
