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
                    { port = 23424
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
            { serviceFqdn = Just "tracing.internal.network"
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
            { serviceFqdn = Just "opensearch.internal.network"
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
            { serviceFqdn = Just "s3.amazonaws.com"
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
            { serviceFqdn = Nothing
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
                { accessTarget = "host" }
            ]
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "user-registry"
        , serviceInfo = ServiceInfo
            { serviceFqdn = Nothing
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
            { serviceFqdn = Nothing
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