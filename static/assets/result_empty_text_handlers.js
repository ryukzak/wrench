if (commentText.trim() === '') {
  document.getElementById('comment-header')?.classList.add('hidden')
  document.getElementById('empty-comment-header')?.classList.remove('hidden')
}

if (dumpText.trim() === '') {
  document.getElementById('accordion-collapse-dump')?.classList.add('hidden')
  document.getElementById('dump-empty-tag')?.classList.remove('hidden')
}

if (testCasesResultText.trim() === '') {
  document
    .getElementById('accordion-collapse-test-cases-result')
    ?.classList.add('hidden')
  document
    .getElementById('test-cases-result-empty-tag')
    ?.classList.remove('hidden')
}

if (simulationLogText.trim() === '') {
  document
    .getElementById('accordion-collapse-simulation-log')
    ?.classList.add('hidden')
  document
    .getElementById('simulation-log-empty-tag')
    ?.classList.remove('hidden')
}

const statusTextElement = document.getElementById('status-text-element')
if (statusTextElement?.innerText === '') {
  statusTextElement.innerHTML =
    'status = "" <span class="grey">/* nothing returned */</span>'
}

const testCasesStatusElement = document.getElementById(
  'test-cases-status-element',
)
if (testCasesStatusElement?.innerText === '') {
  testCasesStatusElement.innerHTML =
    'test_cases_status = "" <span class="grey">/* nothing returned */</span>'
}
