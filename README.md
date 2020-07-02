# STL-IO
The `stl-io` library deals with reading and writing `.stl` files [1] from
internal Common Lisp arrays of vertices and faces.

## Feature coverage
* Binary stl format: ✓
* ASCII stl format: ⨯

Please note that the binary stl format involves `single-float` numbers
to describe the vertices and normal vectors.

## Implementation

## Exported functions

## Usage
See the file [example.lisp](doc/example.lisp).

### Run tests
To run the provided test suite:

```common-lisp
(asdf:test-system "stl-io")
```

## Caveats

## Dependencies
`stl-io`:
* [lisp-binary](https://github.com/j3pic/lisp-binary)

`stl-io/test`:

## References
1. https://en.wikipedia.org/wiki/STL_%28file\_format%29
2. http://www.fabbers.com/tech/STL\_Format
