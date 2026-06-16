# Architecture

## Domain layer

### Core Model

**modules: Core.Model.\***


All core model definitions live in the `kitab-core` library, in `./src/core/`.

### Graph Building

**module: Core.Graph**

The graph connects references (services, tools, CIDR sets) between each-other,
tagged with their connection type.

### Graph Validation

**module: Core.Validation**

We perform checks on the graph to detect some known issues:
  * Mismatched: We want to avoid services that declare different ways of reaching
    each-other
  * Parallel connections: A service declares two different ways to reach another service.
  * Self-referential: A service declares a connection to itself.

## Parser

## Driver

## Renderers
