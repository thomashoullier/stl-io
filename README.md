# STL-IO
The `stl-io` library deals with reading and writing binary `.stl` files [1].

## Feature coverage
* Binary stl format: ✓
* ASCII stl format: ⨯

## Implementation
Dead easy with [lisp-binary](https://github.com/j3pic/lisp-binary). We
simply described the binary structures:

```common-lisp
(defbinary facet (:export T :byte-order :little-endian)
           (nvec #() :type (simple-array single-float (3)))
           (v1 #() :type (simple-array single-float (3)))
           (v2 #() :type (simple-array single-float (3)))
           (v3 #() :type (simple-array single-float (3)))
           (attr 0 :type (unsigned-byte 16)))

(defbinary stl-bin (:export T :byte-order :little-endian)
           (header "" :type (fixed-length-string 80))
           (nfacets 0 :type (unsigned-byte 32))
           (facets #() :type (simple-array facet (nfacets))))
```

These structures are exported from `stl-io`, `stl-bin` is the structure
that is used to read to and write from binary stl files.

## Exported functions
* **read-stl** file-in => stl-struct

Read a binary stl file *file-in* into a 'stl-bin struct *stl-struct*.
* *file-in*: pathname designator to the binary stl file.
* *stl-struct*: a 'stl-bin struct (see above).

* **write-stl** stl-struct file-out

Write from a 'stl-bin struct *stl-struct* to a binary stl file *file-out*.
* *file-out*: pathname designator to the output binary stl file.
              Created if it did not exist, superseded otherwise.

* **nvec** v1 v2 v3 => norm-vec

Helper function to compute the unit normal vector *norm-vec* to a triangle
described by the three vertices *v1*, *v2*, *v3* according to their
order and the right hand rule.
* *v1*, *v2*, *v3*: the points of 3D space describing the triangle.
  In `#(x y z)` format with coordinates as `single-float`.
* *norm-vec*: the unit normal vector according to the right hand rule.
  Same format as vertices.

Formally, this is simply:

$$\mathbf{n} = \frac{\mathbf{u} \times \mathbf{v}}
                    {\lVert \mathbf{u} \times \mathbf{v} \rVert}$$

With:
* $\mathbf{u} = \mathbf{v_2} - \mathbf{v_1}$
* $\mathbf{v} = \mathbf{v_3} - \mathbf{v_1}$
* $\times$: the vector cross-product.

## Usage
See [example.lisp](doc/example.lisp) and the corresponding
write-up [example.md](doc/example.md) for detailed examples.

### Run tests
To run the provided test suite:

```common-lisp
(asdf:test-system "stl-io")
```

## STL format limitations
Keep in mind the stl format has inherent limitations:
* All coordinates are `single-float`.
* Only triangle facets can be stored.
* The only way to attach properties to facets is via the
  non standard attribute byte, which is just 16 bits.

## Caveats
* The structs are a little awkward to use. I haven't had a better
  idea for a general, standard, internal representation for stl
  objects.
* `stl-io` is mostly documentation. `lisp-binary` does everything really.

## TO DO
Optional checks for writing stl files:
* Check consistency of normals with right-hand rule.
* Check that normals are unit vectors.
* Check number of facets is consistent.

## Dependencies
`stl-io`:
* [lisp-binary](https://github.com/j3pic/lisp-binary)

`stl-io/test`:
* [rove](https://github.com/fukamachi/rove)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* `asdf` and `uiop` functions are used.

## References
1. https://en.wikipedia.org/wiki/STL_%28file\_format%29
2. http://www.fabbers.com/tech/STL\_Format
