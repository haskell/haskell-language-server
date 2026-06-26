# Export Plugin

The export plugin provides code actions for working with a module's export list. It provides code actions for:

- Export `...`: on top-level declaration symbols.
- Unexport `...`: on exported top-level declaration symbols.

Actions are only offered when the module has an explicit export list.

## Known limitations

Not yet supported:
- Class methods (`class C where { m :: ... }`)
- Type and data families (standalone or associated)
- Pattern synonyms (`pattern P :: ...`)
- Module re-exports (`module M`)
