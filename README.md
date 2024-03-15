# REL
A research language designed to aid my Thesis paper in proving the implementation path of a Godel Machine.

## Testing
If you want to test the whole project I reccomend installing Stack from ghcup and running `stack test`
- Tests are located in `./test/*` each phase of the interpreter has its own test file.
    - `/Backend.hs` for backend tests, This is where tests for the growth rate should go <-
    - `/Middleware.hs` for middleware tests, best not to touch this as the middleware is finished
    - `/Parser.hs` parser tests heavily using Parsec
    - `/Spec.hs` This is the glue that attachs all the above tests, or tests for `./app/Main.hs`
    [here is an asciinema explaining adding data tests](https://asciinema.org/a/647135)

### Running specific tests
`stack test --ta '-p "<Name of TestGroup>"'`
- `<Name of TestGroup>` is the english name of the tests located in the test directory

### Compiling haddock
follow the steps bellow
`stack haddock`
`open .stack-work/dist/<system arch>/<ghc version>/doc/index.html`

### Adding new tests
- to add new tests we must understand the interp function
```Haskell
-- from file ./src/Interp.hs
interp :: RelIR -> String -> String

-- from file ./src/Structures.hs
data OpStruct = Generate [Token] [Token]
              | Replace  [Token] [Token] deriving (Show, Eq)

type RelIR = [OpStruct]
```
- The next obstical is understanding how to use my testing framework which one could infer from the current usage
    - I use `Tasty` with `HUnit` I primarly use the `HUnit` functionality in the form of a comparison operator `@?=`
    which runs the first (right) argument and assures the output matches the second (left) argument.
