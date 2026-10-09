# Explicit Record Fields Plugin

`hls-explicit-record-fields-plugin` is a plugin to expand record wildcards, explicitly listing all record fields as field puns. It works in both record construction and pattern binding scenarios, and it works as you would expect regardless of whether there are explicitly provided fields or puns in addition to the wildcard.


## Demo

![Expand Wildcard Demo](wildcard.gif)


## Known limitations

One of the shortcomings of the current approach is that all fields of the record are expanded, whether they are actually used or not. This results in warnings of unused bindings, if the corresponding warning flag is enabled.


## Change log
### 1.0.0.0
- Release
