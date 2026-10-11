GET http://localhost:8080/submit-form
HTTP/1.1 200
[Asserts]
xpath "normalize-space(//head/title)" startsWith "Wrench"
xpath "//select[@id='variant']/option[@value='factorial']" exists
xpath "//select[@id='isa']/option[@value='wasm32']" exists

POST http://localhost:8080/submit
[Form]
name: Bob
asm: .text \n _start: \n halt \n
config: {limit: 100, memory_size: 128}
comment: "This is a comment"
variant:
isa: acc32
HTTP/1.1 301
[Captures]
report_url: header "Location"
track_id: cookie "track_id"

GET http://localhost:8080{{report_url}}
HTTP/1.1 200
[Asserts]
header "Set-Cookie" startsWith "track_id={{track_id}}"
xpath "normalize-space(//head/title)" startsWith "Simulation Results"
xpath "normalize-space(//div[@id='simulation-config-text-element']//div[@class='code-content'])" contains "limit: 100, memory_size: 128"
xpath "normalize-space(//pre[@id='status-text-element'])" contains "ExitSuccess"
xpath "normalize-space(//div[@id='simulation-log-text-element']//div[@class='code-content'])" contains "Executed Instruction"
xpath "normalize-space(//div[@id='simulation-log-text-element']//div[@class='code-content'])" contains "Halt @_start"
xpath "normalize-space(//pre[@id='test-cases-status-element'])" isEmpty
xpath "normalize-space(//div[@id='test_cases_result-text-element']//div[@class='code-content'])" isEmpty
xpath "normalize-space(//div[@id='dump-text-element']//div[@class='code-content'])" contains "_start"
xpath "normalize-space(//div[@id='dump-text-element']//div[@class='code-content'])" contains "mem[0x00..0x00] (1 B): Halt"

# Same round-trip for wasm32, so the form's newest ISA is covered end to
# end and not just offered in the dropdown.
POST http://localhost:8080/submit
[Form]
name: Bob
asm: .text \n _start: \n i32.const 0x84 \n i32.const 65 \n i32.store \n halt \n
config: {limit: 100, memory_size: 0x200, memory_mapped_io: {0x84: []}}
comment: "wasm32 round-trip"
variant:
isa: wasm32
HTTP/1.1 301
[Captures]
wasm_report_url: header "Location"

GET http://localhost:8080{{wasm_report_url}}
HTTP/1.1 200
[Asserts]
xpath "normalize-space(//pre[@id='status-text-element'])" contains "ExitSuccess"
xpath "normalize-space(//div[@id='simulation-log-text-element']//div[@class='code-content'])" contains "I32Store"
xpath "normalize-space(//div[@id='simulation-log-text-element']//div[@class='code-content'])" contains "Halt"
xpath "normalize-space(//div[@id='dump-text-element']//div[@class='code-content'])" contains "I32Store 0"
