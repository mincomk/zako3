# zakoctl

`zakoctl` is the official development CLI client for Zako services. It is designed with a modular architecture similar to `kubectl`, allowing for extensible management of various Zako microservices.

## Architecture

`zakoctl` is built with:
- **Rust**: For performance and reliability.
- **Tokio**: Asynchronous runtime.
- **Clap**: Robust command-line argument parsing.
- **Tonic**: gRPC client implementation.
- **Modular Design**: Each service (e.g., Audio Engine) is encapsulated in its own module under `src/services/`, making it easy to add new service integrations.

## Installation

```bash
cargo install --path .
```

## Features

- **Context Management**: Switch between different environments (local, dev, prod) using `config` commands.
- **Auto-completion**: Built-in shell completion generation for Bash, Zsh, Fish, PowerShell, and Elvish.
- **Rich Output**: Colored and formatted output for better readability.
- **Extensible**: Follows a strict service-module pattern for easy expansion.

## Services

### Audio Engine (`ae`)
Full control over the Zako Audio Engine via gRPC.
- **Session Management**: Join/Leave channels.
- **Playback Control**: Play, Stop, Skip, Set Volume.
- **State Inspection**: View current tracks and session status.
- **Advanced Filtering**: Stop specific types of audio (Music vs TTS).

## Configuration

Configuration is stored in `~/.config/zakoctl/config.toml` (Linux), `~/Library/Application Support/zakoctl/config.toml` (macOS), or `%APPDATA%\zakoctl\config.toml` (Windows).

Manage it via the CLI:
```bash
zakoctl config view
zakoctl config set-context ...
zakoctl config use-context ...
```

## Documentation

For detailed usage instructions, see [USAGE.md](USAGE.md).
