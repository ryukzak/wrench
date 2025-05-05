GET http://localhost:8080/submit-form
HTTP/1.1 200
[Asserts]
xpath "normalize-space(//head/title)" startsWith "Wrench"
xpath "//select[@id='variant']/option[@value='factorial']" exists

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
xpath "normalize-space(//div[@id='dump-text-element']//div[@class='code-content'])" contains "mem[0..0]: Halt"
