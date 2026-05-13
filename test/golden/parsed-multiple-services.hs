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
            { serviceFqdn = Just
                ( Right "tracing.internal.network" )
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
        , entityAccesses = []
        , cidrConnections = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "opensearch"
        , serviceInfo = ServiceInfo
            { serviceFqdn = Just
                ( Right "opensearch.internal.network" )
            , serviceContext = Nothing
            , servicePorts = fromList
                [ PortNode
                    { port = 443
                    , protocol = "TCP"
                    }
                ]
            }
        , serviceConnections = []
        , entityAccesses = []
        , cidrConnections = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "s3"
        , serviceInfo = ServiceInfo
            { serviceFqdn = Just
                ( Right "s3.amazonaws.com" )
            , serviceContext = Nothing
            , servicePorts = fromList
                [ PortNode
                    { port = 443
                    , protocol = "TCP"
                    }
                ]
            }
        , serviceConnections = []
        , entityAccesses = []
        , cidrConnections = []
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
                , connectionType = Network
                , connectionPorts = fromList []
                }
            , Connection
                { connectionWith = "otel-tracing"
                , connectionType = Network
                , connectionPorts = fromList []
                }
            ]
        , entityAccesses =
            [ EntityAccess
                { accessTarget = "host"
                , accessPorts = fromList []
                }
            ]
        , cidrConnections = []
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
        , entityAccesses = []
        , cidrConnections = []
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
                , connectionType = Network
                , connectionPorts = fromList []
                }
            , Connection
                { connectionWith = "media-proxy"
                , connectionType = Network
                , connectionPorts = fromList []
                }
            , Connection
                { connectionWith = "user-registry"
                , connectionType = FunctionCall
                , connectionPorts = fromList []
                }
            , Connection
                { connectionWith = "otel-tracing"
                , connectionType = Network
                , connectionPorts = fromList
                    [ PortNode
                        { port = 4317
                        , protocol = "TCP"
                        }
                    ]
                }
            ]
        , entityAccesses = []
        , cidrConnections = []
        }
    )
]