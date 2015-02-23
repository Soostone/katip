

# TODOs

- Log current PID in Item
- Add a "Maybe LocInfo" using a TH version of logF
- Rethink important fields
    - Maybe add a logF variant that uses the class, but it's not
      mandatory
    - Another variant that explicitly passes the fields of interest
- Add Verbosity to the mix
- AppName -> [Text]
    - LogEnv defines the base name-space
    - logF gets a [Text] argument for AppName namespace
    - Whe two are concatted for each message
- Add "modifyNamespace :: (Namespace -> Namespace) -> m a -> m a"
