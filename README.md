<div class="title-block" style="text-align: center;" align="center">

  <picture>
    <img alt="Kitab" src="./doc/kitab.png" width=30%>
  </picture>

<h1> Kitab — Document and enforce your service architecture </h1>

**[Manual] &nbsp;&nbsp;&bull;&nbsp;&nbsp;**
**[Architecture] &nbsp;&nbsp;&bull;&nbsp;&nbsp;**
**[Installation]**

[Manual]: ./doc/MANUAL.md
[Architecture]: ./doc/ARCHITECTURE.md
[Installation]: ./doc/INSTALL.md

</div>

## 📖 Architecture diagrams and Network Policies from the same source of truth.

Kitab gathers service definition files and creates a graph of your infrastructure

The files are written in [KDL](https://kdl.dev), a pleasant document language that avoids the numerous footguns of YAML.

<picture>
  <img alt="Data flow" src="./doc/kitab-diagram.png" width=70% height=70%>
</picture>

## ⚙️ In Action

Let's take the following KDL document:

```kdl
// A context represents a system boundary, like a Kubernetes cluster.
// When generating Cilium policies, services within the cluster
// will be mentioned by their name ("my-app", "media-proxy")
// whereas outside services will be referred to by their FQDN.
context "k8s"

// Services that we run outside of the cluster are not labelled
// with a context, and get a Fully Qualified Domain Name (FQDN) instead.
service "otel-tracing" {
	fqdn "tracing.internal.network"
}

service "opensearch" {
	fqdn "opensearch.internal.network"
	port 4317
	port 4318
}

// Services that live inside the cluster are labelled with "k8s"
// and declare their dependencies to other services.
service "media-proxy" {
	in-context "k8s"

  // This creates a edge between `media-proxy` and `opensearch`.
	depends-on "opensearch" {
    // And we label this edge with the connection method.
		via "https"
	}

	depends-on "otel-tracing" {
		via "https"
    // Ports are optional, and there can be many of them.
    // If no ports are specified by the caller, the callee's ports are used.
		port 4317
	}
}

service "user-registry" {
	in-context "k8s"
}

service "main-app" {
	in-context "k8s"

	depends-on "s3"  {
		via "https"
	}

	depends-on "media-proxy" {
		via "https"
	}

  // This is a sub-system of the `main-app` service,
  // which is accessed by function calls.
	depends-on "user-registry" {
		via "function-call"
	}

	depends-on "otel-tracing" {
		via "https"
	}
}
```

We will get the following PlantUML syntax:

```puml
@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Container.puml

title System Architecture (C4 Container View)

System_Boundary(c1,k8s) {
  System(main_app, "main-app")
  System(media_proxy, "media-proxy")
  System(user_registry, "user-registry")
}

' --- Systems ---
System(opensearch, "opensearch")
System(otel_tracing, "otel-tracing")
System(s3, "s3")

' --- Relationships ---
Rel(main_app, media_proxy, "Connects via", "Network")
Rel(main_app, otel_tracing, "Connects via", "Network")
Rel(main_app, s3, "Connects via", "Network")
Rel(main_app, user_registry, "using", "Function call")
Rel(media_proxy, opensearch, "Connects via", "Network")
Rel(media_proxy, otel_tracing, "Connects via", "Network")
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
```
