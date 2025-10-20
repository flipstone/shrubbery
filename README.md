# shrubbery

This branch has been prepared to recreate
https://github.com/haskell/haskell-language-server/issues/4674

The crash occurs with a few different messages, the most common being  

- `internal error: ARR_WORDS object (0x423160df40) entered`
- `internal error: evacuate: strange closure type -1965382883`

In all cases, disabling `hlint` via the editor language server settings
makes the crash go away.

## Setup

The crash has been reproduced on multiple editors (including VSCode,
Helix, and Emacs) with the ghc and hls versions below. This setup was
done on Ubuntu and Void Linux (glibc), but the crash is also reproducible
on Alpine Linux with a similar setup.

```sh
ghcup config set url-source vanilla
ghcup install ghc --set 9.10.3
ghcup install hls --set 2.12.0.0
```

The default VSCode setup (using the `Haskell` extension) crashes when
opening `src/Shrubbery/TaggedUnion.hs` and adding some characters at
the beginning of the file. 

### Alternative Setup

An alternative setup inside a docker container

```sh
./scripts/bootstrap
docker compose run dev
apt update
apt install hx
stack build
hx src/Shrubbery/TaggedUnion.hs
# Then enter some characters at the beginning of the file wait for
# 'Language Server Exited' to be display at the bottom then :q! to quit
tail ~/.cache/helix/helix.log
```

In `helix.log` you'll see an `ARR_WORDS` error like this:

```
2025-10-20T19:06:07.567 helix_lsp::transport [ERROR] haskell-language-server err <- "haskell-language-server-9.10.3: internal error: ARR_WORDS object (0x423160df40) entered!\n"
2025-10-20T19:06:07.567 helix_lsp::transport [ERROR] haskell-language-server err <- "    (GHC version 9.10.3 for x86_64_unknown_linux)\n"
2025-10-20T19:06:07.567 helix_lsp::transport [ERROR] haskell-language-server err <- "    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug\n"
```

Or possibly an alternative error like this:

```
2025-10-20T19:05:14.469 helix_lsp::transport [ERROR] haskell-language-server err <- "haskell-language-server-9.10.3: internal error: evacuate: strange closure type -1965382883\n"
2025-10-20T19:05:14.469 helix_lsp::transport [ERROR] haskell-language-server err <- "    (GHC version 9.10.3 for x86_64_unknown_linux)\n"
2025-10-20T19:05:14.469 helix_lsp::transport [ERROR] haskell-language-server err <- "    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug\n"
```
