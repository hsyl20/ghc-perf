# Roadmap

## Next

### Support builds of custom GHC
   
- Clone upstream Git repo in `$XDG_DATA_HOME/git/ghc`
- Support building GHC with specific config:
    - ghc commit (but not the repository)
    - hadrian flavour
    - target (not optional, detect it)
        - not optional because needed for fair comparisons we will need it
    - bignum backend
    - bootstrap environment (bootstrap GHC, Alex, Happy...)
        - use nix (ghc.nix?) to ensure we don't rely on arbitrary stuff?

- Store build artefacts in `$XDG_DATA_HOME/ghc/$commit_hash/$config_hash`
    - `$config_hash` is a hash of the config
    - Store readable config in `$XDG_DATA_HOME/ghc/$commit_hash/$options_hash/config`

- Store:
    - Add `ghc_config` table: `config_hash`, `config`
    - `config_hash` will be used later to tag results

- UI
    - Show commit history
    - Show GHC builds per commit

### Dump GHC build IRs

- Dump Core/STG/Cmm/Asm for built GHC
    - Will be used by ticky profiles, etc.
- Store them with other artefacts
- UI: allow navigating them


## Long term

See [Analyzers](Analyzers.md) and [Features](FEATURES.md) for ideas that need to
be distilled into actionable tasks.
