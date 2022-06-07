# GADT Converter Plugin

The hls-gadt-plugin provides a code action that converts a datatype to GADT syntax.

## Demo

![GADT](gadt.gif)

## Design
The plugin works in the following steps:
1. Get data declarations and enabled pragmas from parsed source.
2. Response a code action with a command to convert to GADT syntax if given position is a H98 data declaration.
3. Convert every part of H98 declaration to corresponding GADT's.
4. Print converted declaration. (See `prettyGADTDecl` source code for details)
5. Send edit request to LSP, the edit includes replacing origin data declaration to GADT and inserting a `GADTs` pragma if necessary.

## Known limitations
- Currently all comments missed while converting to GADT syntax.

## Change log
### 1.0.0.0
- Release
