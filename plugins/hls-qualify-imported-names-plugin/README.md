# Qualify Imported Names

![Qualify Imported Names Demo](qualify-imported-names-demo.gif)

## Usage

1. Put cursor over the import declaration you want to qualify names from.
2. Initiate a Code Action.
3. Select `Qualify imported names`.

## Features
- Names are qualified on a per-import declaration basis.
- Names are qualified by the imported module's alias if it has one, otherwise by the imported module's name.
- If the import declaration has an explicit import list then the plugin will qualify only names on the list.
- If the import declaration has an explicit hiding list then the plugin will qualify names from the imported module that are not on the list.
 
## Change log
### 1.0.0.0
- Released...
