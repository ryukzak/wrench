# Example programs

## Naming convention

An example is a pair of files in the same folder:

- `<name>.s` — assembly source
- `<name>[-suffix].yaml` — simulation config for that source

Examples:

| Source | Config |
|--------|--------|
| `hello.s` | `hello.yaml` |
| `factorial.s` | `factorial-5.yaml` |
| `get-put-char.s` | `get-put-char-87.yaml`, `get-put-char-ABCD.yaml` |

One `.s` file may have several `.yaml` configs if each config name starts with the source stem.

## Optional metadata

To set the title and description on the `/examples` page, add `<config-stem>.meta.json` next to the yaml (e.g. `hello.meta.json` for `hello.yaml`):

```json
{
  "title": "Hello world",
  "description": "Minimal program that halts."
}
```

Both fields are optional. Without the file, the link text is `source.s + config.yaml`.
