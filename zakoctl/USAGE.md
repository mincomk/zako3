# zakoctl Usage Guide

`zakoctl` is a command-line interface for managing and developing Zako services, specifically the Audio Engine. This guide provides common workflows and examples.

## Prerequisites

- **Zako Audio Engine** running (default: `http://[::1]:50051`)
- **zakoctl** installed or built

## Quick Start

### 1. Configuration (Optional)
By default, `zakoctl` connects to localhost. If your service is remote, set a context:

```bash
# Create a context for a remote server
zakoctl config set-context dev --ae-addr "http://192.168.1.100:50051"

# Switch to that context
zakoctl config use-context dev
```

### 2. Audio Engine Workflow

**Join a Voice Channel**
```bash
# Syntax: zakoctl ae join <guild_id> <channel_id>
zakoctl ae join 123456789 987654321
```

**Play Music**
```bash
# Syntax: zakoctl ae play <guild_id> <url> [options]
# Default tap is 'ytdl'
zakoctl ae play 123456789 "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
```

**Check Status**
```bash
# See what's playing
zakoctl ae get-session-state 123456789
```

**Control Playback**
```bash
# Skip current track
zakoctl ae next-music 123456789

# Stop all music
zakoctl ae stop-many 123456789 --filter music

# Leave channel
zakoctl ae leave 123456789
```

## Shell Auto-completion

Enable tab completion for a better experience.

**Bash**
```bash
source <(zakoctl completion bash)
```

**Zsh**
```bash
zakoctl completion zsh > /usr/local/share/zsh/site-functions/_zakoctl
# Or for one session:
source <(zakoctl completion zsh)
```

**Fish**
```bash
zakoctl completion fish | source
```

## Troubleshooting

- **Connection Refused**: Check if the Audio Engine is running and the address in `zakoctl config view` matches.
- **Color Output**: Uses ANSI colors. If output looks raw, ensure your terminal supports colors.
