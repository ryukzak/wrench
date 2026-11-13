const HISTORY_KEY = 'wrench-scratch-history'
const DRAFT_KEY = 'wrench-scratch-draft'
const HISTORY_SIZE = 30
const IDLE_MS = 30000

const REGISTERS = {
  acc32: 'Acc:{Acc} V:{V} C:{C}',
  f32a: 'T:{T} S:{S} A:{A}',
  'risc-iv-32': 't0:{t0} t1:{t1} t2:{t2}',
  m68k: 'D0:{D0} D1:{D1} A0:{A0}',
  'vliw-iv': 't0:{t0} t1:{t1} t2:{t2}',
  wasm32: 'stack:{stack:dec}',
}

function starter(isa) {
  const halt = isa === 'vliw-iv' ? '/ / / halt' : 'halt'
  return {
    asm: `    .text\n\n_start:\n    ${halt}\n`,
    config: [
      'limit: 100',
      'memory_size: 0x100',
      'reports:',
      '  - name: Step-by-step log',
      '    slice: all',
      '    view: |',
      `      ${REGISTERS[isa]}`,
      '      {pc}:\t{instruction:next}\t{pc:label}',
      '',
    ].join('\n'),
  }
}

function load(key, fallback) {
  try {
    return JSON.parse(localStorage.getItem(key)) ?? fallback
  } catch {
    return fallback
  }
}

function save(key, value) {
  try {
    localStorage.setItem(key, JSON.stringify(value))
    return true
  } catch {
    return false
  }
}

function formatTime(time) {
  const d = new Date(time)
  const pad = n => String(n).padStart(2, '0')
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`
}

function track(event, properties) {
  window.posthog?.capture(event, properties)
}

async function pack(value) {
  const stream = new Blob([JSON.stringify(value)]).stream().pipeThrough(new CompressionStream('deflate-raw'))
  const bytes = new Uint8Array(await new Response(stream).arrayBuffer())
  let binary = ''
  for (const byte of bytes) binary += String.fromCharCode(byte)
  return btoa(binary).replaceAll('+', '-').replaceAll('/', '_').replace(/=+$/, '')
}

async function unpack(text) {
  const bytes = Uint8Array.from(atob(text.replaceAll('-', '+').replaceAll('_', '/')), c => c.charCodeAt(0))
  const stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream('deflate-raw'))
  return JSON.parse(await new Response(stream).text())
}

// How the code was written since the previous run: typed or pasted, how
// long, whether the student left the page. Sent with the run to PostHog.
function newEditorStats() {
  return {
    typed_chars: 0,
    pasted_chars: 0,
    paste_count: 0,
    max_paste_chars: 0,
    dropped_chars: 0,
    deleted_chars: 0,
    copy_count: 0,
    undo_count: 0,
    active_ms: 0,
  }
}

function newEditStats(origin) {
  return { origin, asm: newEditorStats(), config: newEditorStats(), away_count: 0, away_ms: 0, fmt_count: 0 }
}

const codeText = id =>
  `${Array.from(document.querySelectorAll(`#${id} .code-content .code-line`), line => line.textContent).join('\n')}\n`

// Report page: a link that opens the report's code and config in the
// scratchpad. The variant is not carried over.
export async function setupOpenInScratchLink(linkId, asmId, configId, isa) {
  const link = document.getElementById(linkId)
  if (!link || !isa) return
  link.href = `/scratch#${await pack({ isa, asm: codeText(asmId), config: codeText(configId) })}`
  link.classList.remove('hidden')
}

export function setupScratchpad() {
  const form = document.getElementById('scratch-form')
  const asm = document.getElementById('asm')
  const config = document.getElementById('config')
  const runButton = document.getElementById('run-button')
  const shareButton = document.getElementById('share-button')
  const status = document.getElementById('status')
  const log = document.getElementById('log')
  const dumpSection = document.getElementById('dump-section')
  const dump = document.getElementById('dump')
  const historyList = document.getElementById('history')

  const getIsa = () => form.querySelector('input[name="isa"]:checked').value
  const setIsa = isa => {
    const radio = form.querySelector(`input[name="isa"][value="${isa}"]`)
    if (radio) radio.checked = true
  }

  let editStats = newEditStats('starter')
  let lastIsa = getIsa()

  const updateCommand = () => {
    document.getElementById('command-isa').textContent = getIsa()
  }

  const fill = ({ isa, asm: asmText, config: configText }, origin) => {
    setIsa(isa)
    lastIsa = getIsa()
    updateCommand()
    asm.value = asmText
    config.value = configText
    editStats = newEditStats(origin)
    if (origin !== 'link') saveDraft()
  }

  const saveDraft = () => save(DRAFT_KEY, { isa: getIsa(), asm: asm.value, config: config.value })

  // The dump is a second simulator run, so it is fetched only while its
  // section is open.
  let shownRequest = null
  let dumpRequest = null

  const loadDump = async () => {
    if (!dumpSection.open || !shownRequest || dumpRequest === shownRequest) return
    const request = shownRequest
    dumpRequest = request
    dump.textContent = '/* loading... */'
    try {
      const response = await fetch('/scratch/dump', { method: 'POST', body: new URLSearchParams(request) })
      if (!response.ok) throw new Error(`${response.status} ${await response.text()}`)
      const result = await response.json()
      if (dumpRequest === request) dump.innerHTML = result.dump
    } catch (error) {
      if (dumpRequest === request) {
        dump.textContent = `ERROR: ${error.message}`
        dumpRequest = null
      }
    }
  }

  dumpSection.addEventListener('toggle', loadDump)

  const showResult = ({ isa, asm, config }, result) => {
    status.textContent = result.status
    status.style.color = result.success ? 'var(--c-green)' : 'var(--c-orange)'
    log.innerHTML = result.log
    dump.innerHTML = ''
    shownRequest = { isa, asm, config }
    dumpRequest = null
    loadDump()
  }

  const clearResult = () => {
    status.textContent = '/* press run */'
    status.style.color = ''
    log.innerHTML = ''
    dump.innerHTML = ''
    shownRequest = null
    dumpRequest = null
  }

  // History

  const renderHistory = () => {
    historyList.replaceChildren()
    load(HISTORY_KEY, []).forEach((entry, index) => {
      const item = document.createElement('li')
      item.className = 'flex flex-wrap items-baseline gap-x-2'

      const restore = document.createElement('button')
      restore.type = 'button'
      restore.className =
        'hover:bg-[var(--c-fuschia)] text-[var(--c-fuschia)] hover:text-[var(--c-black)] cursor-pointer whitespace-pre'
      restore.textContent = `[${formatTime(entry.time)} ${entry.isa}]`

      // The longest label, `[... risc-iv-32]`, is 32 characters.
      const label = document.createElement('span')
      label.className = 'inline-block w-[32ch]'
      label.append(restore)
      restore.addEventListener('click', () => {
        fill(entry, 'history')
        showResult(entry, entry.result)
        track('wrench:scratch_history_restore', { isa: entry.isa })
      })

      const verdict = document.createElement('span')
      verdict.className = `${entry.result.success ? 'text-[var(--c-green)]' : 'text-[var(--c-orange)]'} whitespace-pre`
      verdict.textContent = entry.result.success ? 'ok  ' : 'fail'

      const remove = document.createElement('button')
      remove.type = 'button'
      remove.className = 'hover:bg-[var(--c-grey)] text-[var(--c-grey)] hover:text-[var(--c-black)] cursor-pointer'
      remove.textContent = '[x]'
      remove.addEventListener('click', () => {
        const entries = load(HISTORY_KEY, [])
        entries.splice(index, 1)
        save(HISTORY_KEY, entries)
        renderHistory()
      })

      item.append(label, verdict, remove)
      historyList.append(item)
    })
  }

  const pushHistory = entry => {
    const entries = [entry, ...load(HISTORY_KEY, [])].slice(0, HISTORY_SIZE)
    while (entries.length && !save(HISTORY_KEY, entries)) entries.pop()
    renderHistory()
  }

  document.getElementById('history-clear-button').addEventListener('click', () => {
    save(HISTORY_KEY, [])
    renderHistory()
  })

  // Run

  const run = async () => {
    if (runButton.disabled) return
    runButton.disabled = true
    status.textContent = '/* running... */'
    status.style.color = ''
    const request = { isa: getIsa(), asm: asm.value, config: config.value }
    const stats = editStats
    editStats = newEditStats('edit')
    try {
      const response = await fetch('/scratch/run', {
        method: 'POST',
        body: new URLSearchParams({ ...request, editStats: JSON.stringify(stats) }),
      })
      if (!response.ok) throw new Error(`${response.status} ${await response.text()}`)
      const result = await response.json()
      showResult(request, result)
      pushHistory({ time: Date.now(), ...request, result })
    } catch (error) {
      status.textContent = `ERROR: ${error.message}`
      status.style.color = 'var(--c-orange)'
    } finally {
      runButton.disabled = false
    }
  }

  form.addEventListener('submit', event => {
    event.preventDefault()
    run()
  })
  form.addEventListener('keydown', event => {
    if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) {
      event.preventDefault()
      run()
    }
  })
  if (/Mac|iPhone|iPad/.test(navigator.platform)) {
    document.getElementById('run-hint').textContent = '/* cmd+enter */'
  }

  // Format

  const fmtButton = document.getElementById('fmt-button')
  fmtButton.addEventListener('click', async () => {
    fmtButton.disabled = true
    const before = asm.value
    try {
      const response = await fetch(`/fmt?${new URLSearchParams({ isa: getIsa() })}`, {
        method: 'POST',
        headers: { 'Content-Type': 'text/plain; charset=utf-8' },
        body: before,
      })
      const text = await response.text()
      if (!response.ok) throw new Error(text || response.status)
      if (asm.value !== before) throw new Error('the code changed while formatting, press [fmt] again')
      asm.value = text
      editStats.fmt_count++
      saveDraft()
      track('wrench:scratch_fmt', { isa: getIsa() })
    } catch (error) {
      status.textContent = `ERROR: ${error.message}`
      status.style.color = 'var(--c-orange)'
    } finally {
      fmtButton.disabled = false
    }
  })

  // Share

  shareButton.addEventListener('click', async () => {
    const isa = getIsa()
    try {
      const url = `${location.origin}/scratch#${await pack({ isa, asm: asm.value, config: config.value })}`
      await navigator.clipboard.writeText(url)
    } catch (error) {
      status.textContent = `ERROR: can't copy the link: ${error.message}`
      status.style.color = 'var(--c-orange)'
      return
    }
    shareButton.textContent = '[link copied!]'
    setTimeout(() => {
      shareButton.textContent = '[share]'
    }, 1000)
    track('wrench:scratch_share', { isa })
  })

  document.getElementById('reset-button').addEventListener('click', () => {
    fill({ isa: getIsa(), ...starter(getIsa()) }, 'starter')
    clearResult()
  })

  // A new ISA replaces the code only while it is still the starter.
  form.querySelectorAll('input[name="isa"]').forEach(radio => {
    radio.addEventListener('change', () => {
      const previous = starter(lastIsa)
      const untouched = [previous.asm, ''].includes(asm.value) && [previous.config, ''].includes(config.value)
      if (untouched) fill({ isa: getIsa(), ...starter(getIsa()) }, 'starter')
      lastIsa = getIsa()
      updateCommand()
      saveDraft()
    })
  })

  // Edit stats

  for (const editor of [asm, config]) {
    let lastInputAt = 0
    const stats = () => editStats[editor.id]

    editor.addEventListener('beforeinput', event => {
      const now = Date.now()
      if (now - lastInputAt < IDLE_MS) stats().active_ms += now - lastInputAt
      lastInputAt = now

      const selected = editor.selectionEnd - editor.selectionStart
      if (event.inputType.startsWith('delete')) {
        stats().deleted_chars += selected || 1
      } else if (event.inputType.startsWith('history')) {
        stats().undo_count++
      } else if (
        event.inputType.startsWith('insert') &&
        !['insertFromPaste', 'insertFromDrop'].includes(event.inputType)
      ) {
        stats().typed_chars += event.data?.length ?? 1
      }
    })
    editor.addEventListener('paste', event => {
      const length = event.clipboardData.getData('text').length
      stats().pasted_chars += length
      stats().paste_count++
      stats().max_paste_chars = Math.max(stats().max_paste_chars, length)
    })
    editor.addEventListener('drop', event => {
      stats().dropped_chars += event.dataTransfer.getData('text').length
    })
    editor.addEventListener('copy', () => stats().copy_count++)
    editor.addEventListener('cut', () => stats().copy_count++)
    editor.addEventListener('input', saveDraft)
  }

  let awaySince = null
  const leave = () => {
    if (awaySince === null) {
      awaySince = Date.now()
      editStats.away_count++
    }
  }
  const back = () => {
    if (awaySince !== null) {
      editStats.away_ms += Date.now() - awaySince
      awaySince = null
    }
  }
  window.addEventListener('blur', leave)
  window.addEventListener('focus', back)
  document.addEventListener('visibilitychange', () => (document.hidden ? leave() : back()))

  // Initial content: a shared link, then the draft, then the starter.

  const openLink = async () => {
    if (location.hash.length <= 1) return false
    try {
      fill(await unpack(location.hash.slice(1)), 'link')
      clearResult()
      track('wrench:scratch_open_link', { isa: getIsa() })
      return true
    } catch {
      status.textContent = 'ERROR: the shared link is broken'
      status.style.color = 'var(--c-orange)'
      return false
    } finally {
      history.replaceState(null, '', location.pathname)
    }
  }

  const init = async () => {
    if (await openLink()) return
    const draft = load(DRAFT_KEY, null)
    if (draft) fill(draft, 'draft')
    else fill({ isa: getIsa(), ...starter(getIsa()) }, 'starter')
  }

  window.addEventListener('hashchange', openLink)
  renderHistory()
  init()
}
