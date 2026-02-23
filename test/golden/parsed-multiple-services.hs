[ EntityDeclaration
    ( Entity
        { entityName = "host"
        , entityInfo = EntityInfo
            { entityPorts = fromList
                [ PortNode
                    { port = 123
                    , protocol = "UDP"
                    }
                , PortNode
                    { port = 30928
                    , protocol = "TCP"
                    }
                ]
            , entityContext = Just "k8s"
            }
        }
    )
, ContextDeclaration
    ( ServiceContext
        { contextName = "k8s" }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "otel-tracing"
        , serviceInfo = ServiceInfo
            { serviceFqdns =
                [ FQDN
                    { fqdn = "tracing.internal.network"
                    , props = fromList []
                    }
                ]
            , serviceContext = Nothing
            , servicePorts = fromList
                [ PortNode
                    { port = 4317
                    , protocol = "TCP"
                    }
                , PortNode
                    { port = 4318
                    , protocol = "TCP"
                    }
                ]
            }
        , serviceConnections = []
        , cidrSets = []
        , entityAccesses = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "opensearch"
        , serviceInfo = ServiceInfo
            { serviceFqdns =
                [ FQDN
                    { fqdn = "opensearch.internal.network"
                    , props = fromList []
                    }
                ]
            , serviceContext = Nothing
            , servicePorts = fromList
                [ PortNode
                    { port = 443
                    , protocol = "TCP"
                    }
                ]
            }
        , serviceConnections = []
        , cidrSets = []
        , entityAccesses = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "s3"
        , serviceInfo = ServiceInfo
            { serviceFqdns =
                [ FQDN
                    { fqdn = "s3.amazonaws.com"
                    , props = fromList []
                    }
                ]
            , serviceContext = Nothing
            , servicePorts = fromList
                [ PortNode
                    { port = 443
                    , protocol = "TCP"
                    }
                ]
            }
        , serviceConnections = []
        , cidrSets = []
        , entityAccesses = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "media-proxy"
        , serviceInfo = ServiceInfo
            { serviceFqdns = []
            , serviceContext = Just "k8s"
            , servicePorts = fromList []
            }
        , serviceConnections =
            [ Connection
                { connectionWith = "opensearch"
                , connectionType = HTTPS
                , connectionPorts = fromList []
                }
            , Connection
                { connectionWith = "otel-tracing"
                , connectionType = HTTPS
                , connectionPorts = fromList []
                }
            ]
        , cidrSets = []
        , entityAccesses =
            [ EntityAccess
                { accessTarget = "host"
                , accessPorts = fromList []
                }
            ]
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "user-registry"
        , serviceInfo = ServiceInfo
            { serviceFqdns = []
            , serviceContext = Just "k8s"
            , servicePorts = fromList []
            }
        , serviceConnections = []
        , cidrSets = []
        , entityAccesses = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "main-app"
        , serviceInfo = ServiceInfo
            { serviceFqdns = []
            , serviceContext = Just "k8s"
            , servicePorts = fromList []
            }
        , serviceConnections =
            [ Connection
                { connectionWith = "s3"
                , connectionType = HTTPS
                , connectionPorts = fromList []
                }
            , Connection
                { connectionWith = "media-proxy"
                , connectionType = HTTPS
                , connectionPorts = fromList []
                }
            , Connection
                { connectionWith = "user-registry"
                , connectionType = FunctionCall
                , connectionPorts = fromList []
                }
            , Connection
                { connectionWith = "otel-tracing"
                , connectionType = HTTPS
                , connectionPorts = fromList
                    [ PortNode
                        { port = 4317
                        , protocol = "TCP"
                        }
                    ]
                }
            ]
        , cidrSets = []
        , entityAccesses = []
        }
    )
]