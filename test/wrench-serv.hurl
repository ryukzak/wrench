GET http://localhost:8080/submit-form
HTTP/1.1 200
[Asserts]
xpath "normalize-space(//head/title)" startsWith "Wrench"

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
xpath "normalize-space(//pre[@id='simulation-config-text-element'])" contains "limit: 100, memory_size: 128"
xpath "normalize-space(//pre[@id='status-text-element'])" contains "ExitSuccess"
xpath "normalize-space(//pre[@id='simulation-log-text-element'])" contains "# Executed Instruction Log 0: Halt @_start"
xpath "normalize-space(//pre[@id='test-cases-status-element'])" isEmpty
xpath "normalize-space(//pre[@id='test_cases_result-text-element'])" isEmpty
xpath "normalize-space(//pre[@id='dump-text-element'])" contains "_start --- mem[0..0]: Halt"
