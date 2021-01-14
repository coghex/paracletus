# Paracletus

paracletus is a 2D graphics engine written in
haskell using the vulkan-api bindings and glfw,
all controlled by lua scripts.

## Prerequisites

pre-requisites for building include [vulkan](https://vulkan.lunarg.org/sdk/home), [glfw](https://www.glfw.org/download.html),
and [freetype2](https://download.savannah.gnu.org/releases/freetype/).  all of these should be in
most package managers.  works on windows, linux,
and mac; probably anything with GHC and vulkan.

## Building

to build use `cabal new-build paracletus` to
download dependencies and compile.
`glslangValidator` must be in your path or
passed as a cabal argument as `cabal
--extra-lib-dirs=...\glslangValidator.exe
new-build paracletus`.

## Usage

use `./dist-newstyle/build/[arch]/ghc-[ver]/
abfa-[ver]/x/abfa/opt/build/abfa/abfa +RTS
-s -M[x]m -N[n]` to run with x megabytes and
n cores.

performance is increased drastically by disabling
the development flag, but compilation takes
forever.

to create profiling files, use `cabal new-build
--enable-library-profiling --enable-profiling
abfa` to build, and `-prof -fprof-auto` in the
ghc-options of the cabal file.  run with flags
`+RTS -s -p -hy -M[x]m -N[n]`

the lua file located in `mod/game/` is an example
of how one would write a game in this engine.  UI
elements are organized by windows, when switching
all textures will be reloaded.  within a window
there can be a variety of alpha-blended squares.

all lua commands can be run in game through the
shell accessed from the \` key.  keys can be set
in `mod/base/config.lua`.

## Development

the code is written with unicode, some extra
unicode is defined in `src/UPrelude`. which is
imported in every file.

feel free to open any issues for any reasons, pull
requests, forks, and stars are all appreciated.
most of the code is original, the files in
`/src/Paracletus/Vulkan/` and `/src/Ananmnesis.hs`
are mostly from the [vulkan-api example](https://github.com/achirkin/vulkan/tree/master/vulkan-triangles).

the code structure is devided up into many parts
arbitrarily named:

* paracletus - the graphics engine itself.
* anamnesis - the continuation monad that
provides mutable state.
* epiklesis - the lua interpreter and code
to generate game objects from the lua scripts.
* artos - a collection of threading functions to
pass data around between the various components.
* oblatum - an interface to GLFW and a collection
of functions to layout fonts

