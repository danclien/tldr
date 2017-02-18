# TLDR

I ended up hitting 3 hours with the code so I'm calling it done. Any feedback
appreciated.

Thanks for your time!
Dan

## Personal Notes

### Time Breakdown
* 15 minutes - Setting up the project and tests
* 75 minutes - Writing a simple parser (Hutton-Meijer)
* 90 minutes - Working through the problem

### If I had more time
* Look for a faster solution
* Error handlings for invalid parse results
* Clean up `import` and export lists
* Tests for the parser
* `hlint`
* Haddock documentation

### Cyclomatic complexity
* `normalizeUntilDone` is `O(n^2)`
  * It only removes one `..` in a continuous list of `..`. Worse case scenario is a path like ``/a/b/c/d/e/f/g/h/i/../../../../../../../../../j``.

### CPU cache effects
My best guest is there will be mostly cache misses due to the use of linked
lists.

## Problem

You're asked to create a helper function that _normalizes_ strings containing
filesystem paths. The file system is assumed to be unix, with slash-separated
components.

This function is going to be somewhat specialized for the webserver, so
`domain.com/../foo` is expected to be equivalent to `domain.com/foo`.

Examples of path normalization:

| Path              | Normalized        |
|-------------------|-------------------|
| `../bar`          | `/bar`            |
| `/foo/bar`        | `/foo/bar`        |
| `/foo/bar/../baz` | `/foo/baz`        |
| `/foo/bar/./baz/` | `/foo/bar/baz/`   |
| `/foo/../../baz`  | `/baz`            |

The solution that you are asked to write as part of this exercise should not
touch the actual filesystem.
The processing is done purely lexically on a string of bytes.
Assume ASCII character encoding.
Use no external dependencies for your code, just the `base` library. Some test
framework for your tests is fine.

The expected interface for the function is

```haskell
normalize :: String -> String
```

Create a github repo with your solution as a stack-based cabal project that's
built with `stack build` and tested via `stack test`. Put a README with any
additional explanations you want to give.

### Evaluation

How will we assess your code:

 * We require bug-free code
 * We also very much enjoy reasonably fast code
 * Tests should be there

Kudos if you can:
 * Explain cyclomatic complexity of the solution
 * Quantify the speed of the solution (for example, in Gbps per CPU core)
 * Assess the CPU cache effects
