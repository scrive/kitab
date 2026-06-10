## NAME

kitab — Documentation and Infrastructure for service-oriented architectures

## SYNOPSIS

### Generate

Usage: kitab generate [-q|--quiet] (-f|--format FORMAT)
                      (-o|--output-dir DIRECTORY) [--context CONTEXT]
                      [--cloud CLOUD] [--region REGION] [--env ENVIRONMENT]
                      [-i|--inventory DIRECTORY] FILES

  Produce artifacts from definition files

### Dump

Usage: kitab dump FILE

  Dump the parsed Haskell AST of a single KDL file

## DESCRIPTION

Kitab gathers service definition files and assembles them to create an infrastructure graph.
This graph can then be used to create network access policies and architecture diagrams.

## OPTIONS

<dl>
  <dt>-q,--quiet</dt>
  <dd style="margin-left: 3rem"> Make the program less verbose </dd>

  <dt>-f,--format=FORMAT</dt>
  <dd style="margin-left: 3rem"> Output format </dd>

  <dt>-o,--output-dir=DIRECTORY</dt>
  <dd style="margin-left: 3rem"> Output directory </dd>

  <dt>--context=CONTEXT</dt>
  <dd style="margin-left: 3rem"> Only output services belonging to a specific context </dd>

  <dt>--cloud=CLOUD</dt>
  <dd style="margin-left: 3rem"> Specify inventory values for a specific cloud provider </dd>

  <dt>--region=REGION</dt>
  <dd style="margin-left: 3rem"> Specify inventory values for a specific cloud region </dd>

  <dt>--env=ENVIRONMENT</dt>
  <dd style="margin-left: 3rem"> Specify inventory values for a specific deployment environment </dd>

  <dt>-i,--inventory=DIRECTORY</dt>
  <dd style="margin-left: 3rem"> Path to an inventory directory</dd>

  <dt>--version</dt>
  <dd style="margin-left: 3rem"> Show version information </dd>

  <dt>-h,--help</dt>
  <dd style="margin-left: 3rem"> Show this help text </dd>
</dl>

## CONFIGURATION

Service definitions are written in [KDL](https://kdl.dev) files.

### <a name="context"></a> `context`

At the top-level, `context` node defines a system boundary, like a Kubernetes cluster.

This node can contain the following children

* [`entity`](#entity)

| Argument | Type | Description         |
|----------|------|---------------------|
| name     | text | Name of the context |


#### Example

```kdl
context "k8s"
```

### <a name="entity"></a> `entity`

Declare an abstract entity linked to a context, oftentimes used by specific renderers.

This node can contain the following children

* [`in-context`](#in-context)
* [`port`](#port)

| Argument | Type | Description         |
|----------|------|---------------------|
| name     | text | Name of the context |

#### Examples

This defines a `"host"` entity that will be used by the Cilium renderer
to emit a `toEntities` section.

```kdl
entity "host" {
  in-context "k8s"
  port 123 "UDP"
  port 23432 "TCP"
}
```

### <a name="in-context"></a> `in-context`

Within a [`service`](#service) or an [`entity`](#entity) node, this indicates that it belongs to the named context.

| Argument | Type | Description         |
|----------|------|---------------------|
| name     | text | Name of the context |

#### Example

```kdl
service "media-proxy" {
  in-context "cluster"
  depends-on "cluster:host"
}
```

### <a name="service"></a> `service`

A service node in your infrastructure.

This node can contain the following children
* [`fqdn`](#fqdn);
* [`port`](#port);
* [`context`](#context);
* [`depends-on`](#depends-on);

| Argument | Type |
|----------|------|
| name     | text |

#### Example

```kdl
service "opensearch" {
  fqdn "opensearch.internal.network"
  port 4317
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

Declare an outgoing connection to another service. This node can contain the following children:

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

Declare a port for an incoming or outgoing connection.
It is an optional node, and can be repeated within a [`depends-on`](#depends-on),
a [`service`](#service),  or a [`cidr-set`](#cidr-set) node.

It has no child nodes.

| Argument | Type |
|----------|------|
| port     | 16-bit unsigned integer (between 0 and 65535) |
| protocol | text (optional) |

#### Example

```kdl
cidr-set "ntp" {
	cidr-rule {
		cidr "10.42.42.0/24" "NTP"
	}
	port 123 "UDP"
}

service "some-service" {
  port 4317
  port 4318
  
}
depends-on "some-service" {
  via "https"
  port 4317
}

```

### <a name="cidr-set"></a> `cidr-set`

Top-level declaration of a set of ([CIDR]) IP addresses or ranges, used for broad coverage of a third-party network.
This is mainly used by the Cilium renderer. See https://docs.cilium.io/en/stable/security/policy/language/#ip-cidr-based

This node can contain the following children:

* [`cidr-rule`](#cidr-rule) (at least one is required);
* [`port`](#port)

#### Examples

```kdl
cidr-set "network" {
  cidr-rule {
    cidr "0.0.0.0/0" "Internet"
    except "10.0.0.0/8" "Internal network, to be refined further down"
  }
}

cidr-set "mysql" {
  cidr-rule {
    cidr "10.147.128.0/24" "MySQL"
  }
  port 3306
}

service "my-app" {
  connect "network"
  connect "mysql"
}
```

### <a name="cidr-rule"></a> `cidr-rule`

Declare a CIDR range and its exceptions inside a [`cidr-set`](#cidr-set).
A `cidr-set` can contain several `cidr-rule` nodes.

This node can contain the following children:

* [`cidr`](#cidr) (exactly one is required);
* [`except`](#except).

#### Examples

```kdl
cidr-set "integrations" {
  cidr-rule {
    cidr "10.23.32.0/24" "Integrations"
  }
  cidr-rule {
    cidr "10.23.31.0/24" "Internal shenanigans"
  }
}
```

### <a name="cidr"></a> `cidr`

Declare a CIDR IP range or address, and a comment to clarify what it represents.
Instead of a literal range and comment, a [variable](#inventory) reference can
be given; the comment is then taken from the variable's `description`.

It has no child nodes.

| Argument   | Type |
|------------|------|
| CIDR range | text or `(var)` reference |
| Comment    | text (only with a literal CIDR range) |

#### Examples

```kdl
cidr-set "mysql" {
  cidr-rule {
    cidr "10.147.128.0/24" "MySQL"
  }
}

cidr-set "opensearch" {
  cidr-rule {
    cidr (var)opensearch-cidr
  }
}
```

### <a name="except"></a> `except`

Declare an exception to the [`cidr`](#cidr) node of the same [`cidr-rule`](#cidr-rule).
A `cidr-rule` can contain several `except` nodes.

It has no child nodes.

| Argument   | Type |
|------------|------|
| CIDR range | text or `(var)` reference |
| Comment    | text (only with a literal CIDR range) |

#### Examples

```kdl
cidr-set "network" {
  cidr-rule {
    cidr "0.0.0.0/0" "Internet"
    except "10.0.0.0/8" "Internal network, to be refined further down"
    except "192.168.0.0/16" "Local network"
  }
}
```

### <a name="connect"></a> `connect`

Declare a connection to a [CIDR Set](#cidr-set).

```kdl
service "my-app" {
  connect "network"
}
```

### <a name="access"></a> `access`

Declare an access to an [`entity`](#entity).

| Argument    | Type |
|-------------|------|
| Entity name | text |

#### Examples

```kdl
service "media-proxy" {
	access "host"
}
```

### <a name="version"></a> `version`

Top-level declaration for the version of the configuration language that `kitab` should parse.

| Argument    | Type |
|-------------|------|
| Version number | number |

#### Examples

```kdl
version 1
```

## INVENTORY

In a scenario where some values change betweeen deployment environment and infrastructure providers, you can use the inventory system.

An inventory is a directory of KDL files that declare an `inventory` node
and attributes that will allow you to select the node: cloud, env, region.

Inventories are combined by order of specificity. If you have two inventories, one with only a `cloud` property, and another with both `cloud` and `env`,
then the second one, more specific, will overwrite the variables defined in the first one, but will leave the ones it does not replace.

### Example 1: FQDN

```kdl
inventory cloud=aws region=eu-west-1 env=prod {
	var "opensearch-fqdn" {
		value "opensearch.aws.internal.network"
		description "OpenSearch instance in AWS"
	}
}

service "opensearch" {
	fqdn (var)opensearch-fqdn
}

```

To pick the values from such an inventory, call `kitab` like this:

```bash
$ kitab -i ./inventory --cloud aws --region eu-west-1 --env prod
```

### Example 2: CIDR

```kdl
inventory cloud=aws region=eu-west-1 env=prod {
	var "mysql-cluster-cidr" {
		value "10.147.128.0/24"
		description "MySQL cluster"
	}
}

cidr-set "mysql" {
	cidr-rule {
		cidr (var)mysql-cluster-cidr
	}
	port 3306
}
```

When using the Cilium renderer, this gives us:

```yaml
- toCIDRSet:
    - cidr: "10.147.128.0/24" # MySQL cluster
  toPorts:
    - ports:
        - port: "3306"
          protocol: TCP
```

## ENVIRONMENT VARIABLES

* `NO_COLORS`: Disable terminal styling.
* `DEBUG`: Force verbosity. Takes priority over `--quiet`.

## BUGS

To report bugs, please visit https://github.com/scrive/kitab/issues.

[FQDN]: https://en.wikipedia.org/wiki/Fully_qualified_domain_name
[CIDR]: https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing
