[ ServiceDeclaration
    ( Service
        { serviceName = "media-proxy"
        , serviceInfo = ServiceInfo
            { serviceFqdn = Nothing
            , serviceContext = Nothing
            , servicePorts = fromList []
            , rendererProps = fromList []
            }
        , serviceConnections =
            [ Connection
                { connectionWith = "main-app"
                , connectionType = HTTPS
                , connectionPorts = fromList
                    [ PortNode
                        { port = 3833
                        , protocol = "TCP"
                        }
                    ]
                }
            ]
        , entityAccesses = []
        , cidrConnections = []
        , toolCalls = []
        }
    )
]