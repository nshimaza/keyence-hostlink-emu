# keyence-hostlink-emu
Super tiny emulator of Keyence HostLink protocol

## Build

You need [stack](https://docs.haskellstack.org/en/stable/README/) to build.

```shell-session
git clone https://github.com/nshimaza/keyence-hostlink-emu.git
cd keyence-hostlink-emu
stack build
```

## Usage

```
.stack-work/install/x86_64-osx/lts-13.17/8.6.4/bin/keyence-uplink-emu-exe < sample-response.txt
```

The emulator listens TCP port 8501 and returns a line from stdin on receiving RD command or RDS command.

The emulator only recognizes RD command and RDS command.  Any other commands result parse error.
On parse error, the emulator disconnects the session without recovering the parse error within the same TCP session.

Protocol specification can be found in manual for Keyence KV-7500 and EtherNet/IP modules.
Web site of Keyence is [https://www.keyence.co.jp/](https://www.keyence.co.jp/).
