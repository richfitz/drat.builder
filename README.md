# drat.builder

[![Build Status](https://travis-ci.org/richfitz/drat.builder.png?branch=master)](https://travis-ci.org/richfitz/drat.builder)

The idea here is to make it extremely easy to keep a [drat](https://github.com/eddelbuettel/drat) repository up to date.

Suppose you have a drat that tracks a number of upstream github repos (here the *repos* are the package repositories and *drat repository* will be the drat itself).  Say, [reomoji](https://github.com/richfitz/remoji), [rfiglet](https://github.com/richfitz/rfiglet) and [cowsay](https://github.com/sckott/cowsay)

In the root of an existing drat repository or in a new empty directory, make a file called `packages.txt` containing:

```
richfitz/remoji
richfitz/rfiglet
sckott/cowsay
```

Then run

```r
drat.builder::build()
```

which will download the most recent sources for those packages, build them (source versions only, but will build vignettes) and add them to drat following the best practice commit log so that each log entry reads like

```
<package_name> <version> <sha> <url>
```

e.g.,

```
rfiglet 0.1.0 4b65d19 https://github.com/richfitz/rfiglet.git
```

## Command line use

Run

```
drat.builder::install_script("~/bin")
```

and then a shell script `drat.builder` is available.  It takes all the arguments that `drat.builder::build` takes, but is useful to run from the command line.  See

```
drat.builder --help
```

for help.

## Options

`drat.builder` takes options

* `install` -- installs packages before building so that vignettes can be built
* `install_local` -- implies `install`, and installs locally and temporarily rather than into a system-readable library
* `no_fetch` -- suppresses fetching packages

## packages.txt

The file `packages.txt` roughly follows the convention started by devtools;

* `username/repo/subdir` -- install package from a subdir
* `username/repo@ref` -- install a particular reference
* `username/repo/subdir@ref` -- both a subdir and a reference

The `@ref` syntax will be useful to anchor drat builds to particular versions, rather than constantly reading off `HEAD` of the upstream repo.  This will be easy when the upstream repo uses github releases and [semantic versioning](http://semver.org) as you can write:

```
richfitz/rfiglet@v0.1.0
```

## Avoiding rebuilds

To avoid polluting the drat repo, `drat.builder` will try to avoid rebuilding.  To do this it keeps a file `packages.json` with version numbers and sha values of the installed packages.

## TODO

* support for non-github packages (e.g., using full URL)
* support for preventing vignette build (e.g. long options to `R CMD build`)
