# keyence-uplink-emu
Super tiny emulator of Keyence uplink protocol

## Usage

```
.stack-work/install/x86_64-osx/lts-12.5/8.4.3/bin/keyence-uplink-emu-exe < sample-response.txt
```

The emulator listens TCP port 8501 and returns a line from stdin on receiving RD command or RDS command.

The emulator only recognizes RD command and RDS command.  Any other commands result parse error.
On parse error, the emulator disconnects the session without recovering the parse error within the same TCP session.
