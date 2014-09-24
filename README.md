Generating LLVM IR using Template Meta Programming in Haskell
=================
Implementing parallel computations is traditionally done in low level languages like C or FORTRAN.
This allows great control over the details of execution.
Unfortunately, this also comes with the complexities of using a low-level language.
To make this easier,  [Chakravarty et al.](https://github.com/AccelerateHS) have developed [Accelerate](https://github.com/AccelerateHS/accelerate), an embedded array language for computations for high-performance computing in Haskell.
The main execution target of this language is GPUs using the parallel computation framework CUDA.

[McDonell](https://github.com/tmcdonell) started working on an alternative implementation using the LLVM compiler framework.
I contribute to this work by implementing a different approach to code-generation for LLVM using quasi-quotation.
I use this quasi-quoter to implement missing parts in the code generation of the LLVM backend.

source:
https://github.com/tvh/llvm-general-quote
