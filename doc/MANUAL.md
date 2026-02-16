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

  <dt>--context=CONTEXT</dt>
  <dd style="margin-left: 3rem"> Only output services belonging to a specific context </dd>

  <dt>--version</dt>
  <dd style="margin-left: 3rem"> Show version information </dd>

  <dt>-h,--help</dt>
  <dd style="margin-left: 3rem"> Show this help text </dd>
</dl>

## CONFIGURATION

Service definitions are written in [KDL](https://kdl.dev) files.

### <a name="context"></a> `context`

At the top-level, `context` node defines a system boundary, like a Kubernetes cluster.
Within a [`service`](#service) node, this indicates that the service belongs to the name context.

| Argument | Type | Description         |
|----------|------|---------------------|
| name     | text | Name of the context |

#### Example

```kdl
context "kubernetes"
```

### <a name="service"></a> `service`

A service node in your infrastructure.

This node can contain the following children
* [`fqdn`](#fqdn);
* [`context`](#context);
* [`depends-on`](#depends-on);
* [`cidr-set`](#cidr-set);

| Argument | Type |
|----------|------|
| name     | text |

#### Example

```kdl
service "opensearch" {
  fqdn "opensearch.internal.network"
}

service "media-proxy" {
  context "k8s"

  // This creates a edge between `media-proxy` and `opensearch`.
  depends-on "opensearch" {
    // And we label this edge with the connection method.
    via "https"
  }
}
```

### <a name="fqdn"></a> `fqdn`

The Fully Qualified Domain Name ([FQDN]) of a service.
It is used to allow egress connections to out-of-context services when generating Cilium policies.

It has no child nodes.

| Argument | Type |
|----------|------|
| name     | text |

#### Example

```kdl
service "opensearch" {
  fqdn "opensearch.internal.network"
}
```

### <a name="depends-on"></a> `depends-on`

Declare an outgoing connection to a service. This node can contain the following children:

* [`via`](#via);
* [`port`](#port).

| Argument | Type |
|----------|------|
| name     | text |

#### Example

```kdl
service "media-proxy" {
  context "k8s"

  depends-on "otel-tracing" {
    via "https"
    port 4317
    port 4318
  } 
}
```

### <a name="via"></a> `via`

Declare the connection method to the service being depended on.

It has no child nodes.

| Argument | Type |
|----------|------|
| method     | closed enum: "https" \| "function-call" |

#### Example

```kdl
depends-on "some-service" {
  via "https"
} 

depends-on "user-registry" {
  via "function-call"
}
```

### <a name="port"></a> `port`

Declare a port for an outgoing connection.
It is an optional node, and can be repeated within a [`depends-on`](#depends-on)
or a [`cidr-set`](#cidr-set) node.

It has no child nodes.

| Argument | Type |
|----------|------|
| port     | 16-bit unsigned integer (between 0 and 65535) |
| protocol | text (optional) |

#### Example

```kdl
depends-on "some-service" {
  via "https"
  port 4317
  port 4318
}

cidr-set {
	cidr "10.42.42.0/24" "NTP"
	port 123 "UDP"
}
```

### <a name="cidr-set"></a> `cidr-set`

Declare a set of ([CIDR]) IP addresses or ranges, used for broad coverage of a third-party network.
This is mainly used by the Cilium renderer. See https://docs.cilium.io/en/stable/security/policy/language/#ip-cidr-based

This node can contain the following children:

* [`cidr`](#cidr);
* [`except`](#except).
* [`port`](#port)

#### Examples

```kdl
service "my-app" {
  cidr-set {
    cidr "0.0.0.0/0" "Internet"
    except "10.0.0.0/8" "Internal network, to be refined further down"
  }

  cidr-set {
    cidr "10.147.128.0/24" "MySQL"
    port 3306
  }
}
```

### <a name="cidr"></a> `cidr`

Declare a CIDR IP range or address, and a comment to clarify what it represents.

It has no child nodes.

| Argument   | Type |
|------------|------|
| CIDR range | text |
| Comment    | text |

#### Examples

```kdl
cidr-set {
  cidr "10.147.128.0/24" "MySQL"
}
```

### <a name="except"></a> `except`

Declare an exception to a [`cidr`](#cidr) node in the same [`cidr-set`](#cidr-set)

It has no child nodes.

| Argument   | Type |
|------------|------|
| CIDR range | text |
| Comment    | text |

#### Examples

```kdl
cidr-set {
  cidr "0.0.0.0/0" "Internet"
  except "10.0.0.0/8" "Internal network, to be refined further down"
}
```

## ENVIRONMENT

## BUGS

To report bugs, please visit https://github.com/scrive/kitab/issues.

[FQDN]: https://en.wikipedia.org/wiki/Fully_qualified_domain_name
[CIDR]: https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing
