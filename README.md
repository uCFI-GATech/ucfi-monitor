# uCFI-monitor

uCFI-monitor takes in the PT trace and analyzed the current path being executed to check whether each indirect call has only one target and its correctness.

## Build
1. Make sure llvm-3.6.0 is downloaded and is successfully compiled. Or you can directly reuse the ucfi-compiler.
2. Edit the Makefile to change `LLVM_BUILD` to the llvm installation directory.
3. Run `make` and the executable would be under `build`.

## How to use

Check [uCFI-compiler](https://github.com/uCFI-GATech/ucfi-compiler) to see how to use `uCFI-monitor` for a hello-world program
