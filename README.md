# keyence-uplink-emu
Super tiny emulator of Keyence uplink protocol

## Usage

```
cat sample-response.txt | .stack-work/install/x86_64-osx/nightly-2018-04-18/8.4.1/bin/keyence-uplink-emu-exe
```

The emulator listens TCP port 8501 and returns a line from stdin on receiving CR.
