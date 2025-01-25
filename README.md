# Abyss Lisp

## Notice

I am calling Abyss Lisp done.
Kind of late considering how long it has been since the last update.
There will be a few pushes where I publish some stuff I have but did not commit.

The reason is I had determined that first class macros to be lacking for what I had hoped.
The goal was to use them to construct DSLs that generate compilable code.
Without needing to create an alternative "evaluation" function.

Two additional features would be needed at a minimum.

First is operatives would need to receive the source expression that evaluated to the operative.
So for an operative `f` being called as `(f x)` would receive `(f x)` as arguments instead of `(x)`.
Annoying to implement but is palpable.

Second is to allow the equivalent of Common Lisp "symbol macros".
This is possible to implement, but causes problems.
It would mean operatives cannot be passed as parameters normally.
When a symbol evaluates to an operative and it is going to be passed it would be expanded.
So a special built in would be needed to get the operative from the environment without this step.

So it can be achieved, but at a cost.
The costs being increased complexity and the language being less "smooth".
The former makes the language's weakness of performance worse.
The latter harms the language's strength.

I have thoughts still for a Lisp dialect, but it would be a major departure from Abyss.
Therefore, I feel any such work should be a new project even if it is a fork of Abyss.

The first class macros and environments of a Kernel like language are a good experience to have.

As a parting note, there is an idea that I explored but never committed.
I mention it now in case anyone viewing Abyss in the future has the same hypothesis.

The idea is to manage environments through effects rather than objects bound to variables.
What this would be aiming for is to allow multithreading to be practical.

It almost works. But it unfortunately does not.

Managing environments this way would mean they cannot be modified once their dynamic scope exits.
If they are read only, then we do not need a lock to access them in the presence of multithreading.
The issue is environments captured by a combiner (function / macro) can be shared while in use.

So locking would still sometimes need to be used.
It would still make the situation much better.

However, there would be trade offs.
It means a decrease in expressive power.
And effects without compiler optimizations have a performance cost of their own.

## Description

A prototype of a Lisp dialect.

*Very much a work in progress at this time.*

It is primarily based on [Kernel](http://web.cs.wpi.edu/~jshutt/kernel.html).

The goal for Abyss Lisp is an interpreted language as a compiler.
The plan is to include what is effectively the internals of a compiler in the built ins.
And provide abstractions on top of the built ins to conveniently write code to be compiled.

## Building

Prerequisites:

- SBCL (developed on version 2.3.x)
- [re2c](https://re2c.org/index.html) (version 3.0)
- A C++ compiler with `char8_t` from C++20
- CMake or Meson
- [CFFI](https://github.com/cffi/cffi)
- A version of libffi supported by CFFI.
- (for unit tests only) [FiveAM](https://github.com/lispci/fiveam)

It is recommended to install and setup [quicklisp](https://www.quicklisp.org/beta/).

The source for Abyss will need to be in a location searched by ASDF.
With quicklisp setup, it will search the 'local-projects' subdirectory of the quicklisp
installation directory.

Configure and build the C++ portion of the project with CMake or Meson.
Example commands below assume the root of the Abyss source is the current working directory.

Example for CMake:

```sh
> cmake -B build -G Ninja -D CMAKE_BUILD_TYPE=Release
> cmake --build build
```

Example for Meson:

```sh
> meson setup build --buildtype release
> meson compile -C build
> ./build/abyss-ffi-enum-gen
```

The `abyss-ffi-enum-gen` program generates 'src/ffi/enums.lisp'.
CMake handles this step, Meson does not.

There are two environment variables that are used for the Lisp portion of the build.

- `ABYSS_BUILD_PATH`: the directory to place generated executables
- `ABYSS_LIB_PATH`: the directory containing the shared library for the parser
  When building the executable it should the the long term location of the library
  It is also used when the programs are run for an alternative location

If not given an absolute path, it will be assumed relative to the current working directory.

For SBCL:

```sh
> export ABYSS_BUILD_PATH="build/"
> export ABYSS_LIB_PATH="build/"
> sbcl --eval "(asdf:oos 'asdf:program-op :abyss/viewer)"
> sbcl --eval "(asdf:oos 'asdf:program-op :abyss/bench)"
```

The syntax for setting an environment variable in PowerShell is

```powershell
$Env:ABYSS_BUILD_PATH = "build\\"
```

The command line programs built are:

- abyss-lex: a command line lexer debug aid
- abyss-parse: a command line parser debug aid
- abyss-viewer: a command line debug aid to see the loaded code
- abyss-bench: a single script file runner that times evaluation

Usage:

```sh
> ./build/abyss-lex "examples/fib.abyss"
> ./build/abyss-parse "examples/fib.abyss"
> ./build/abyss-viewer "examples/fib.abyss"
> ./build/abyss-bench "examples/fib.abyss"
```

Note: if you set `ABYSS_LIB_PATH` to a location you plan to install to later,
the above usage examples would need it set to the build directory.

## Installation

Not applicable at this time.

## Usage

Not applicable at this time.

## Developers

- Adam Armstrong

## License

The project's code is under the ISC license, see the LICENSE file for details.

It is also included in files along side the source code.
