<div class="title-block" style="text-align: center;" align="center">

  <picture>
    <img alt="Kitab" src="./doc/kitab.png" width=30%>
  </picture>

<h1> Kitab — Bye-bye YAML and PlantUML! </h1>

**[Manual] &nbsp;&nbsp;&bull;&nbsp;&nbsp;**
**[Architecture] &nbsp;&nbsp;&bull;&nbsp;&nbsp;**
**[Installation]**

[Manual]: ./doc/MANUAL.md
[Architecture]: ./doc/ARCHITECTURE.md
[Installation]: ./doc/INSTALL.md

</div>

Kitab gathers service definition files and assembles them to create an infrastructure graph.
This graph can then be used to create network access policies and architecture diagrams.

The files are written in [KDL](https://kdl.dev), a pleasant document language that avoids the numerous footguns of YAML.

## ⚙️ In Action

Let's take the following KDL document:

```kdl
context "k8s"

service "otel-tracing" {
	fqdn "tracing.internal.network"
}

service "opensearch" {
	fqdn "opensearch.internal.network"
}

service "media-proxy" {
	context "k8s"
	depends-on "otel-tracing" "https"
}

service "user-registry" {
	context "k8s"
}

service "main-app" {
	context "k8s"
	depends-on "s3" "https"
	depends-on "media-proxy" "https"
	depends-on "user-registry" "function-call"
	depends-on "otel-tracing" "https"
}

```

We will get the following PlantUML syntax:

```puml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Container.puml

title System Architecture (C4 Container View)

System_Boundary(c1,k8s) {
  System(main_app, "main-app")
  System(media_proxy, "media-proxy")
  System(user_registry, "user-registry")
}

' --- Systems ---
System(otel_tracing, "otel-tracing")
System(s3, "s3")

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

And the following Cilium Network Policy for the `media-proxy` service (amongst others)

```yaml
apiVersion: "cilium.io/v2"
kind: CiliumNetworkPolicy
metadata:
  name: "media-proxy-networkpolicy"
spec:
  endpointSelector:
    matchLabels:
      app: "media-proxy"
  egress:
    - toEndpoints:
        - matchLabels:
            io.kubernetes.pod.namespace: "kube-system"
            k8s-app: "kube-dns"
      toPorts:
        - ports:
            - port: "53"
              protocol: UDP
          rules:
            dns:
              - matchPattern: "*"
    - toFQDNs:
        - matchName: "opensearch.internal.network"
        - ports:
          - port: "443"
            protocol: TCP
    - toFQDNs:
        - matchName: "tracing.internal.network"
        - ports:
          - port: "4317"
            protocol: TCP
    - toEndpoints:
        - matchLabels:
            app: "redis"
```
