# Architecture

The project is split into several components in order to enforce dependencies between what are the Infrastructure, Domain, Application and Presentation layers

## Infrastructure

The *Infrastructure* layer provides foundations like:
  * File management operations
  * Parsing

### Components
  * `lib:kitab-core`

## Domain

It defines types and relationships described in the KDL files.

### Components
  * `lib:kitab`

## Presentation

The human interface of the program is defined in the Presentation layer.
This is where CLI and output formats like C4 are defined.

### Components
  * `lib:kitab-c4`

## Application

This layer provides high-level coordination of the other layers.

### Components
  * `exe:kitab`
