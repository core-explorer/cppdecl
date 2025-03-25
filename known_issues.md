## Things we knowingly handle "incorrectly":

* `A::B` could be a constructor name given `using A = B;`, but we don't parse those as constructors.

  We could add this behind a flag, not supported now for simplicity.

* We always accept the C-only spelling `restrict` in addition to `__restrict` and `__restrict__`.

  There's currently no flag to disable this, for simplicity, but we could add one.

## Compiler-specific behavior that we don't imitate:

* Only GCC accepts trailing return type arrow in parentheses in some simple cases:
  ```cpp
  using T = auto(() -> int);
  ```
  Clang and MSVC reject this. We also reject this for simplicity.

* Only GCC accepts things like:
  ```cpp
  using A = int;
  struct B {};
  A::B::* x;
  ```
  Clang and MSVC reject this, and so do we.

  This is impossible to parse without semantic information or without producing a long list of ambiguities.

  Note that all compilers accept `int::A::* x;`, and we try to support this case too.
