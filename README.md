# box-solver

Clojure breadth-first search of iOS Boxed-In game.

## Usage

CLI Usage:
```
Boxed-in solver (corona v1.0)
Options:
  -t, --timeout TIMEOUT  nil  Execution timeout in milliseconds (0-1099511627776)
  -l, --level LEVEL      1    Which level number to solve (1-150)
  -h, --help
```

## Example

```
$ lein trampoline run --level 5
solving level 5 with no millisecond timeout.
xxxxxxxxx
x R x   x
x@x ++  x
xx*   + x
'xxxGxxpx
'xg * + x
'xr     x
'xxxxxxxx
"Elapsed time: 1689.374703 msecs"
[:down :left :down :left :left :up :down :right :right :up :left :left :left :right :up :up :right :up :down :left :left :up :right :right :right :up :right :down :down :left :left :up :left :down :down :down :right :down :left :left :up :right :up :up :left :left :right :up :up :left :left :down]
52 steps
```

## License

Copyright © 2020 David Lynch

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
