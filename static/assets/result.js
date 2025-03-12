export function getTexts() {
  const assemblerCodeTextElement = document.getElementById(
    'assembler-code-text-element',
  )
  const simulationConfigTextElement = document.getElementById(
    'simulation-config-text-element',
  )
  const dumpTextElement = document.getElementById('dump-text-element')
  const commentTextElement = document.getElementById('comment-text-element')
  const testCasesResultTextElement = document.getElementById(
    'test_cases_result-text-element',
  )
  const simulationLogTextElement = document.getElementById(
    'simulation-log-text-element',
  )

  let assemblerCodeText = assemblerCodeTextElement?.innerHTML ?? ''
  let simulationConfigText = simulationConfigTextElement?.innerHTML ?? ''
  let commentText = commentTextElement?.innerHTML ?? ''
  let dumpText = dumpTextElement?.innerHTML ?? ''
  let testCasesResultText = testCasesResultTextElement?.innerHTML ?? ''
  let simulationLogText = simulationLogTextElement?.innerHTML ?? ''

  return {
    assemblerCodeText,
    simulationConfigText,
    commentText,
    dumpText,
    testCasesResultText,
    simulationLogText,
  }
}

const {
  assemblerCodeText,
  simulationConfigText,
  commentText,
  dumpText,
  testCasesResultText,
  simulationLogText,
} = getTexts()

/* empty text handlers */

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

/* copy buttons */

const copyAssemblerButton = document.getElementById('copy-assembler-button')
const copySimulationConfigButton = document.getElementById(
  'copy-simulation-config-button',
)

copyAssemblerButton?.addEventListener('click', () => {
  copyTextAndChangeButtonText(copyAssemblerButton, assemblerCodeText)
})
copySimulationConfigButton?.addEventListener('click', () => {
  copyTextAndChangeButtonText(copySimulationConfigButton, simulationConfigText)
})

async function copyTextAndChangeButtonText(button, text) {
  await navigator.clipboard.writeText(text)
  let count = 0
  if (button.innerText == '[copied!]') {
    count = 1
  } else if (button.innerText.startsWith('[copied ')) {
    let count_text = button.innerText.substring('[copied '.length).split(' ')[0]

    count = Number(count_text)
  }
  count++

  let buttonText = ''
  if (count === 1) {
    buttonText = '[copied!]'
    button.classList.remove('white')
    button.classList.add('green')
  } else {
    buttonText = `[copied ${count} times!]`
  }

  button.innerText = buttonText
}
