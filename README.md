# REL
A research language designed to aid my Thesis paper in proving the implementation path of a Godel Machine.

## Testing
If you want to test the whole project I reccomend installing Stack from ghcup and running `stack test`
### Running specific tests
`stack test --ta '-p "<Name of TestGroup>"'`
- `<Name of TestGroup>` is the english name of the tests located in the test directory

### Compiling haddock
follow the steps bellow
`stack haddock`
`open .stack-work/dist/<system arch>/<ghc version>/doc/index.html`
