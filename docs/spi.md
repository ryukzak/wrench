# SPI

Wrench has one universal SPI model for all architectures. It is implemented as software bitbang, so you should control SPI manually

## Quick Terms

- `MOSI` = `Master Out, Slave In`  
  Bit line from master to slave
- `MISO` = `Master In, Slave Out`  
  Bit line from slave to master
- `CLK` (or `SCLK`) is serial clock
- `CS` (chip select, also named `SS`) enables the slave

In this emulator, the **master** is the side that generates `CLK` transitions.
So assembly program, which toggles `CLK` via memory-mapped pins, acts as the SPI master.

## SPI Device Ports

In general SPI devices looks like this:

```text
                  +----------------------------+
   MOSI --------->|                            |
   CLK  --------->|        SPI DEVICE          |
   CS   --------->|                            |
                  |                            |
   MISO <---------|                            |
                  +----------------------------+
```

Signal meaning in the model:

- `CS=0`: transfer is active
- `CS=1`: transfer is inactive
- `CLK` edges move transfer state according to `mode`
- `MOSI`: bit currently driven by master (your program)
- `MISO`: bit currently returned by slave model

## Memory-Mapped View

Each SPI device has a numeric id in the `spi` config section. This id is used only to select the device in config and reports.

The actual memory-mapped pins are configured separately. Each pin has:

- `address`: memory word address
- `bit`: bit number inside that word

For example, this maps one SPI device to addresses `0x90` and `0x94`:

```yaml
spi:
  0:
    mode: 0
    cs_bit:
      address: 0x90
      bit: 0
    clk_bit:
      address: 0x90
      bit: 1
    mosi_bit:
      address: 0x90
      bit: 2
    miso_bit:
      address: 0x94
      bit: 0
```

The example uses two cells intentionally. `CS/CLK/MOSI` are written by the program and `MISO` is read by the program. Keeping them separate makes the direction of each signal clear and avoids read/write ambiguity in memory-mapped IO

When pins are placed on the same word address, the emulator still protects each pin by its exact `{ address, bit }` pair

Pin addresses are not limited by the device id. You may connect pins to any memory word address, but one `{ address, bit }` pair can be assigned only once. This means several SPI devices may share one memory word when they use different bits. For example, `spi: 0` may use bits `0..3` of address `0x90`, and `spi: 1` may use bits `4..7` of the same address

## Configuration

Example config:

```yaml
spi:
  0:
    mode: 0
    cs_bit:
      address: 0x90
      bit: 0
    clk_bit:
      address: 0x90
      bit: 1
    mosi_bit:
      address: 0x90
      bit: 2
    miso_bit:
      address: 0x94
      bit: 0
    input:
      - at: 0
        word: 0xA5
      - at: 40
        bytes: [0x11, 0x22, 0x33]
      - at: 100
        byte: 0x7F
```

### Required field

- `mode`: one of `0`, `1`, `2`, `3`
- `cs_bit`, `clk_bit`, `mosi_bit`, `miso_bit`

### Pin mapping

- `cs_bit`, `clk_bit`, `mosi_bit`, `miso_bit` use `{ address, bit }`
- Example:

```yaml
cs_bit:
  address: 0x90
  bit: 5
```

or

```yaml
cs_bit: { address: 0x90, bit: 5 }
```

Naturally occurring validation rules:

- one `{ address, bit }` pair can be assigned only once
- Bit index must fit word size
- device id is only a number for config/report lookup, not a memory address

### Input format

Each `input` item is an object:

- `at`: SPI tick when data becomes available.
- exactly one payload:
    - `byte`
    - `bytes`
    - `word`

Example:

```yaml
input:
  - at: 10
    byte: 0x12
  - at: 20
    bytes: [0xAA, 0xBB]
  - at: 30
    word: 0x12345678
```

## Modes (CPOL/CPHA)

| SPI mode | Clock Polarity (CPOL) | Clock Phase (CPHA) | Shift edge | Sample edge |
| :------- | :--: | :--: | :--------: | :---------: |
| 0        | 0    | 0    | falling + CS activate | rising |
| 1        | 0    | 1    | rising | falling |
| 2        | 1    | 0    | rising + CS activate | falling |
| 3        | 1    | 1    | falling | rising |

> [!TIP]
> mode number is just binary `CPOL:CPHA` \
> `0 = 00`, `1 = 01`, `2 = 10`, `3 = 11`

### Signal Diagram

One bit period always has two clock edges (`edge1`, `edge2`).
What changes between modes is:

- idle clock level (`CPOL`)
- which edge is `sample` and which is `shift` (`CPHA`)

```text
CPOL = 0 (modes 0,1)  idle LOW
CLK:   ___/‾‾‾‾‾‾‾‾\_________/‾‾‾‾‾‾‾‾‾\___
          ^        ^         ^         ^
          edge1    edge2     edge1     edge2
          rising   falling   rising    falling

mode 0 (CPHA=0): sample=edge1, shift=edge2
mode 1 (CPHA=1): shift=edge1, sample=edge2

CPOL = 1 (modes 2,3)  idle HIGH
CLK:   ‾‾‾\_________/‾‾‾‾‾‾‾‾‾\_________/‾‾‾
          ^         ^         ^         ^
          edge1     edge2     edge1     edge2
          falling   rising    falling   rising

mode 2 (CPHA=0): sample=edge1, shift=edge2
mode 3 (CPHA=1): shift=edge1, sample=edge2
```

Short memory rule:

- `CPOL` tells where clock rests when idle
- `CPHA` tells whether first active edge is `sample` (`0`) or `shift` (`1`)

More details: [Clock polarity and phase](https://en.wikipedia.org/wiki/Serial_Peripheral_Interface#Clock_polarity_and_phase)

## Tick Semantics

SPI tick increments on sample edges while `CS=0`.

- +1 per valid sample edge
- used by `input[].at`
- used for MOSI log timestamps

So `at` is not CPU instruction number; it is SPI edge-time in the transfer

## Report

Available placeholders:

```text
{spi:<device_id>:miso}
{spi:<device_id>:mosi}
{spi:<device_id>:status}
{spi:<device_id>:clock}
{spi:<device_id>:pins}
{spi:<device_id>:wave}
```

- `status`:
    - `miso_ready` if data is ready at current SPI tick (or already loaded in shift register)
    - `miso_empty` otherwise
- `clock` is current SPI tick
- `wave` is an ASCII diagram of pin changes collected during simulation:

```text
TICK: 0                      
CS  : ‾\___________________/‾
CLK : ______/‾‾\__/‾‾\_______
MOSI: _______________________
MISO: _/‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
```

In wave output:

- `TICK` marks every tenth SPI tick
- long diagrams are split into fixed-width blocks
- `_` means low level
- `‾` means high level
- `/` means rising edge
- `\` means falling edge

Example:

```yaml
reports:
  - name: Check SPI
    slice: last
    filter:
      - state
    view: |
      spi_miso[0]: {spi:0:miso}
      spi_mosi[0]: {spi:0:mosi}
      spi_status[0]: {spi:0:status}
      spi_clock[0]: {spi:0:clock}
      spi_pins[0]: {spi:0:pins}
      spi_wave[0]:
      {spi:0:wave}
```
