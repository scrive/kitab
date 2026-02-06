# 📦 Install
### Nightly pre-releases

Pre-release binaries are available for the following platforms:

* Linux-x86_64-musl (statically linked)
* macOS-arm64

They are available at https://github.com/scrive/kitab/releases/tag/kitab-head

## 🔧 Build from source

*kitab* is made in Haskell. To build it from source, use [ghcup](https://www.haskell.org/ghcup/) to install the following toolchains:
* `cabal` 3.16.1.0
* `ghc` 9.12.2

Run `$ cabal install exe:kitab` in order to install the executable.
