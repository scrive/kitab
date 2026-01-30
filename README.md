<p align="center">
  <picture>
    <img alt="Kitab" src="./doc/kitab.png" width=30%>
  </picture>
</p>

<h1 align=center> Kitab </h1>

Kitab gathers service definition files and assembles them to create an infrastructure graph.
This graph can then be used to create network access policies and architecture diagrams.

## 📖 Documentation

The project's architecture is documented in [doc/ARCHITECTURE.md](./doc/ARCHITECTURE.md)

Run `$ cabal haddock --open` to generate a reference for the API of the project.

## 🔧 Build

Run `$ cabal build` to build the project

## ⚙️ In Action

Let's take the following KDL document:

```kdl
service "media-proxy" {
	depends-on "main-app" "https"
}

service "main-app" {
	depends-on "s3" "https"
	depends-on "media-proxy" "https"
	depends-on "user-registry" "function-call"
}
```

We will get the following PlantUML syntax:

```puml
@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Container.puml

title System Architecture (C4 Container View)

' --- Systems ---
System(main_app, "main-app")
System(media_proxy, "media-proxy")
System(otel_tracing, "otel-tracing")
System(s3, "s3")
System(user_registry, "user-registry")

' --- Relationships ---
Rel(main_app, media_proxy, "Connects via", "HTTPS")
Rel(main_app, otel_tracing, "Connects via", "HTTPS")
Rel(main_app, s3, "Connects via", "HTTPS")
Rel(main_app, user_registry, "using", "Function call")
Rel(media_proxy, otel_tracing, "Connects via", "HTTPS")
@enduml
```

Which gives us this schema:

![system diagram](./doc/system-diagram.png)
