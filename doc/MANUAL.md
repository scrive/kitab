## NAME

kitab — Documentation and Infrastructure for service-oriented architectures

## SYNOPSIS

Usage: kitab [-q|--quiet] (-f|--format FORMAT) (-i|--input FILE) (-o|--output-dir DIRECTORY)

## DESCRIPTION

Kitab gathers service definition files and assembles them to create an infrastructure graph.
This graph can then be used to create network access policies and architecture diagrams.

## OPTIONS

<dl>
  <dt>-q,--quiet</dt>
  <dd style="margin-left: 3rem"> Make the program less verbose </dd>

  <dt>-f,--format=FORMAT</dt>
  <dd style="margin-left: 3rem"> Output format </dd>

  <dt>-i,--input=FILE</dt>
  <dd style="margin-left: 3rem"> input file, can be specified multiple times </dd>

  <dt>-o,--output-dir=DIRECTORY</dt>
  <dd style="margin-left: 3rem"> Output directory </dd>

  <dt>--version</dt>
  <dd style="margin-left: 3rem"> Show version information </dd>

  <dt>-h,--help</dt>
  <dd style="margin-left: 3rem"> Show this help text </dd>
</dl>

## CONFIGURATION

Configuration is made with [KDL](https://kdl.dev) files that describe services and their relationships.


## ENVIRONMENT

## BUGS

To report bugs, please visit https://github.com/scrive/kitab/issues.
