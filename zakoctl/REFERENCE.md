# zakoctl Reference Documentation

This document provides a comprehensive reference for the `zakoctl` command-line interface, its configuration, and environment variables.

## Global Options

These options apply to all commands.

| Flag | Env Variable | Default | Description |
|------|--------------|---------|-------------|
| `--ae-addr <URL>` | `ZAKO_AE_ADDR` | Config-defined or `http://[::1]:50051` | The address of the Audio Engine gRPC server. Overrides configuration settings. |
| `--help`, `-h` | N/A | N/A | Print help information. |
| `--version`, `-V` | N/A | N/A | Print version information. |

## Configuration

`zakoctl` uses a TOML configuration file to manage contexts (environments).

**Location:**
- **Linux:** `~/.config/zakoctl/config.toml`
- **macOS:** `~/Library/Application Support/zakoctl/config.toml`
- **Windows:** `%APPDATA%\zakoctl\config.toml`

**Schema:**

```toml
# The name of the context currently in use
current_context = "default"

# List of defined contexts
[[contexts]]
name = "default"
ae_addr = "http://[::1]:50051"

[[contexts]]
name = "prod"
ae_addr = "http://api.zako.prod:50051"
```

## Command Reference

### `audio-engine` (alias: `ae`)

Interact with the Zako Audio Engine service.

#### `join`
Connects the bot to a specific voice channel.
```bash
zakoctl ae join <GUILD_ID> <CHANNEL_ID>
```

#### `leave`
Disconnects the bot from a guild's voice channel.
```bash
zakoctl ae leave <GUILD_ID>
```

#### `play`
Queues an audio track for playback.
```bash
zakoctl ae play [OPTIONS] <GUILD_ID> <REQUEST>
```
- `<REQUEST>`: The input string (e.g., YouTube URL, search query).
- `--queue <NAME>`: Target queue (default: "default").
- `--tap <NAME>`: Audio source tap (default: "ytdl").
- `--volume <FLOAT>`: Initial volume 0.0-1.0 (default: 1.0).

#### `stop`
Stops a specific track by UUID.
```bash
zakoctl ae stop <GUILD_ID> <TRACK_ID>
```

#### `stop-many`
Stops multiple tracks based on a filter.
```bash
zakoctl ae stop-many --filter <TYPE> [OPTIONS] <GUILD_ID>
```
- `--filter <TYPE>`: One of `all`, `music`, `tts`.
- `--user-id <ID>`: Required if filter is `tts`. Stops TTS requested by this user.

#### `next-music`
Skips the current music track.
```bash
zakoctl ae next-music <GUILD_ID>
```

#### `set-volume`
Adjusts volume of a playing track.
```bash
zakoctl ae set-volume <GUILD_ID> <TRACK_ID> <VOLUME>
```

#### `get-session-state`
Retrieves current session info (connected channel, active tracks).
```bash
zakoctl ae get-session-state <GUILD_ID>
```

---

### `config`

Manage `zakoctl` configuration and contexts.

- **`view`**: Print the raw TOML configuration.
- **`current-context`**: Print the name of the active context.
- **`use-context <NAME>`**: Switch the active context.
- **`set-context <NAME> --ae-addr <URL>`**: Create or update a context.
- **`delete-context <NAME>`**: Remove a context.

---

### `completion`

Generate shell completion scripts.

```bash
zakoctl completion <SHELL>
```
**Supported Shells:** `bash`, `elvish`, `fish`, `powershell`, `zsh`.
