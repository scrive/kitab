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
        { contextName = "k8s"
        , subContexts = []
        }
    )
, ToolDeclaration "pngquant"
, ServiceDeclaration
    ( Service
        { serviceName = "media-proxy"
        , serviceInfo = ServiceInfo
            { serviceFqdn = Nothing
            , serviceContext = Just "k8s"
            , servicePorts = fromList []
            , rendererProps = fromList []
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
                , connectionPorts = fromList
                    [ PortNode
                        { port = 4317
                        , protocol = "TCP"
                        }
                    ]
                }
            , Connection
                { connectionWith = "mailgun"
                , connectionType = SMTPS
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
        , toolCalls = [ "pngquant" ]
        }
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
            , rendererProps = fromList []
            }
        , serviceConnections = []
        , entityAccesses = []
        , cidrConnections = []
        , toolCalls = []
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
            , rendererProps = fromList []
            }
        , serviceConnections = []
        , entityAccesses = []
        , cidrConnections = []
        , toolCalls = []
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
            , rendererProps = fromList []
            }
        , serviceConnections = []
        , entityAccesses = []
        , cidrConnections = []
        , toolCalls = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "mailgun"
        , serviceInfo = ServiceInfo
            { serviceFqdn = Just
                ( Right "smtp.eu.mailgun.org" )
            , serviceContext = Nothing
            , servicePorts = fromList
                [ PortNode
                    { port = 465
                    , protocol = "TCP"
                    }
                ]
            , rendererProps = fromList []
            }
        , serviceConnections = []
        , entityAccesses = []
        , cidrConnections = []
        , toolCalls = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "user-registry"
        , serviceInfo = ServiceInfo
            { serviceFqdn = Nothing
            , serviceContext = Just "k8s"
            , servicePorts = fromList []
            , rendererProps = fromList []
            }
        , serviceConnections = []
        , entityAccesses = []
        , cidrConnections = []
        , toolCalls = []
        }
    )
, ServiceDeclaration
    ( Service
        { serviceName = "main-app"
        , serviceInfo = ServiceInfo
            { serviceFqdn = Nothing
            , serviceContext = Just "k8s"
            , servicePorts = fromList []
            , rendererProps = fromList []
            }
        , serviceConnections =
            [ Connection
                { connectionWith = "user-registry"
                , connectionType = FunctionCall
                , connectionPorts = fromList []
                }
            , Connection
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
                { connectionWith = "otel-tracing"
                , connectionType = HTTPS
                , connectionPorts = fromList
                    [ PortNode
                        { port = 4317
                        , protocol = "TCP"
                        }
                    ]
                }
            , Connection
                { connectionWith = "mailgun"
                , connectionType = SMTPS
                , connectionPorts = fromList []
                }
            ]
        , entityAccesses = []
        , cidrConnections = []
        , toolCalls = []
        }
    )
]