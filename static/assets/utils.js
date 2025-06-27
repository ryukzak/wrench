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

export function getIsaFlag(text) {
  const regex = /--isa\s+(\S+)/
  const match = text.match(regex)
  return match ? match[1] : null
}
export function setupHideCommentsButton(buttonId, sourceElementId, isa) {
  const button = document.getElementById(buttonId)
  const sourceElement = document.getElementById(sourceElementId)

  if (!button || !sourceElement) return

  function removeComments(text, delimiter) {
    if (!delimiter) return text

    let inString = null // Tracks quote type: null, '"', or "'"
    let output = ''

    for (let i = 0; i < text.length; i++) {
      const char = text[i]

      // Handle string literals
      if (inString) {
        if (char === '\\' && i + 1 < text.length) {
          // Preserve escape sequences
          output += char + text[++i]
        } else if (char === inString) {
          // End of string
          inString = null
          output += char
        } else {
          output += char
        }
      }
      // Check for comment delimiter outside strings
      else if (text.startsWith(delimiter, i)) {
        return output // Return text before comment
      }
      // Check for string starters
      else if (char === '"' || char === "'") {
        inString = char
        output += char
      }
      // Regular character outside string
      else {
        output += char
      }
    }
    return output
  }

  button.addEventListener('click', () => {
    const comment_delimiter = isa === 'f32a' ? '\\' : ';'
    const codeContent = sourceElement.querySelector('.code-content')
    if (!codeContent) return

    const lines = codeContent.querySelectorAll('.code-line')
    const isCurrentlyHidden = button.getAttribute('data-hidden') === 'true'

    if (isCurrentlyHidden) {
      // Show comments: restore original content
      lines.forEach(line => {
        const original = line.getAttribute('data-original')
        if (original !== null) {
          line.textContent = original
          line.removeAttribute('data-original')
        }
      })
      button.setAttribute('data-hidden', 'false')
      button.textContent = '[hide_comments]'
    } else {
      // Hide comments using safe removal
      lines.forEach(line => {
        const currentText = line.textContent
        line.setAttribute('data-original', currentText)
        line.textContent = removeComments(currentText, comment_delimiter)
      })
      button.setAttribute('data-hidden', 'true')
      button.textContent = '[show_comments]'
    }
  })
}
