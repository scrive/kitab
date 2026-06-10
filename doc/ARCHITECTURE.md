# Architecture

Kitab is split between several sub-libraries to enforce proper separation between the various layers of the application.


## `lib:kitab`

This is the main library, which contains the CLI implementation,
the parsing logic, and the driver – code that handles the orchestration of
rendering and file system operations.

## `lib:kitab-core`

This library holds the code model definitions.

## `lib:kitab-puml`, `lib:kitab-cilium`, `lib:kitab-gexf`

Format-specific renderers.
