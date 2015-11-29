# lisb

Write B in Clojure. Interact with the ProB constraint solver.


## Examples

See src/lisb/examples


### Bugs

- the pred macro messes up sets consisting of variables,
 e.g. ((pred foo [] #{:x :y})) yields an error.
 This is because flatten only works on sequential items, i.e. not sets.


### To Do

- [ ] painless conversion of Clojure data structures into B data
- [ ] painless conversion from B data into Clojure data structures
- [ ] special nodes
    - [x] if-then-else
    - [ ] let (B)
    - [ ] choose
- [ ] strings containing B code
- [ ] exhaustive implementation of B operators
- [ ] more examples

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
