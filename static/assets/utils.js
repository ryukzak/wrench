export function setupCopyButton(buttonId, sourceElementId) {
  const button = document.getElementById(buttonId)
  const sourceElement = document.getElementById(sourceElementId)

  if (!button || !sourceElement) return

  button.addEventListener('click', async () => {
    let text

    const codeContent = sourceElement.querySelector('.code-content')
    if (codeContent) {
      const lines = Array.from(codeContent.querySelectorAll('.code-line')).map(
        line => line.textContent,
      )
      text = lines.join('')
    }

    await navigator.clipboard.writeText(text)

    let count = parseInt(button.dataset.copyCount || '0', 10)
    count++
    button.dataset.copyCount = count

    const originalClass =
      button.getAttribute('data-original-class') || button.className

    button.className = originalClass.replaceAll('--c-white', '--c-green')
    button.setAttribute('data-original-class', originalClass)

    button.textContent = count === 1 ? '[copied!]' : `[copied ${count} times!]`

    setTimeout(() => {
      button.className = originalClass
    }, 1000)
  })
}

export function handleEmptyContent(contentId, containerId, emptyIndicatorId) {
  const content = document.getElementById(contentId)
  const container = document.getElementById(containerId)
  const emptyIndicator = document.getElementById(emptyIndicatorId)

  if (!content || !container || !emptyIndicator) return

  if (content.textContent.trim() === '') {
    container.classList.add('hidden')
    emptyIndicator.classList.remove('hidden')
  }
}

export function setupThemeToggle(buttonId) {
  const button = document.getElementById(buttonId)
  if (!button) return

  const updateTheme = () => {
    const isDark = document.body.classList.toggle('dark')
    localStorage.setItem('prefers-color-scheme', isDark ? 'dark' : 'light')
  }

  const storedTheme = localStorage.getItem('prefers-color-scheme')
  if (storedTheme) {
    document.body.classList.toggle('dark', storedTheme === 'dark')
  } else if (window.matchMedia) {
    const systemDark = window.matchMedia('(prefers-color-scheme: dark)').matches
    document.body.classList.toggle('dark', systemDark)
  }

  button.addEventListener('click', updateTheme)
}

export function findIsaFlag(text) {
  const flagPattern = /--isa\s+(\S+)/
  const foundFlag = text.match(flagPattern)
  return foundFlag ? foundFlag[1] : null
}

export function removeComments(code, commentStarter) {
  if (!commentStarter) return code

  let insideString = false
  let cleanedCode = ''

  for (
    let currentPosition = 0;
    currentPosition < code.length;
    currentPosition++
  ) {
    const currentCharacter = code[currentPosition]

    // Check for comment starter
    if (!insideString && code.startsWith(commentStarter, currentPosition)) {
      return cleanedCode
    }

    cleanedCode += currentCharacter

    // Check for new string starting
    if (!insideString && ["'", '"'].includes(currentCharacter)) {
      insideString = currentCharacter
      continue
    }

    // Check for string ending and quotes escape
    if (
      currentCharacter === insideString &&
      code[currentPosition - 1] !== '\\'
    ) {
      insideString = false
      continue
    }
  }

  return cleanedCode
}

function restoreComments(codeLines) {
  codeLines.forEach(line => {
    const originalText = line.getAttribute('data-original-text')
    if (originalText !== null) {
      line.textContent = originalText
      line.removeAttribute('data-original-text')
    }
  })
}

function hideComments(codeLines, commentStarter) {
  codeLines.forEach(line => {
    const originalText = line.textContent
    line.setAttribute('data-original-text', originalText)
    line.textContent = removeComments(originalText, commentStarter)
  })
}

export function setupHideCommentsButton(buttonId, containerId, isaType) {
  const toggleButton = document.getElementById(buttonId)
  const codeContainer = document.getElementById(containerId)

  if (!toggleButton || !codeContainer) return

  // Determine comment symbol based on ISA type
  const commentSymbol = isaType === 'f32a' ? '\\' : ';'

  toggleButton.addEventListener('click', () => {
    const codeLines = codeContainer.querySelectorAll('.code-line')
    const commentsAreHidden = toggleButton.dataset.hidden === 'true'

    if (commentsAreHidden) {
      restoreComments(codeLines)
      toggleButton.dataset.hidden = 'false'
      toggleButton.textContent = '[hide_comments]'
    } else {
      hideComments(codeLines, commentSymbol)
      toggleButton.dataset.hidden = 'true'
      toggleButton.textContent = '[show_comments]'
    }
  })
}
