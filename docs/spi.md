# SPI

Wrench has one universal SPI model for all architectures. It implemented as software bitbang - you should control SPI maualy

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

For each SPI base address, the device uses two word cells:

- output cell (`PINS_OUT`) for `CS/CLK/MOSI`
- input cell (`PINS_IN`) for `MISO`

By default (32-bit, base `0x90`):

- `0x90` -> output cell
- `0x94` -> input cell

Default bit mapping:

- `CS`   -> `0x90:0`
- `CLK`  -> `0x90:1`
- `MOSI` -> `0x90:2`
- `MISO` -> `0x94:0`

## Configuration

Example config:

```yaml
spi:
  0x90:
    mode: 0
    cs_bit: 0x90:0
    clk_bit: 0x90:1
    mosi_bit: 0x90:2
    miso_bit: 0x94:0
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

### Optional pin remapping

- `cs_bit`, `clk_bit`, `mosi_bit`, `miso_bit` use format `<addr>:<bit>`
- Example: `cs_bit: 0x90:5`

Naturally occurring validation rules:

- Address is limited to device range: `base` or `base + word_size`
- `cs/clk/mosi` must point to one common output address
- Bit index must fit word size

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

## Tick Semantics

SPI tick increments on sample edges while `CS=0`.

- +1 per valid sample edge
- used by `input[].at`
- used for MOSI log timestamps

So `at` is not CPU instruction number; it is SPI edge-time in the transfer

## Report

Available placeholders:

```text
{spi:<base>:miso}
{spi:<base>:mosi}
{spi:<base>:status}
{spi:<base>:clock}
{spi:<base>:pins}
```

- `status`:
    - `miso_ready` if data is ready at current SPI tick (or already loaded in shift register)
    - `miso_empty` otherwise
- `clock` is current SPI tick

Example:

```yaml
reports:
  - name: Check SPI
    slice: last
    filter:
      - state
    view: |
      spi_miso[0x90]: {spi:0x90:miso}
      spi_mosi[0x90]: {spi:0x90:mosi}
      spi_status[0x90]: {spi:0x90:status}
      spi_clock[0x90]: {spi:0x90:clock}
      spi_pins[0x90]: {spi:0x90:pins}
```
