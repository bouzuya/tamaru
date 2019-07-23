{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "aff-promise"
    , "argonaut"
    , "bouzuya-http-method"
    , "bouzuya-http-server"
    , "bouzuya-http-status-code"
    , "formatters"
    , "halogen"
    , "node-fs"
    , "node-http"
    , "node-process"
    , "psci-support"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
