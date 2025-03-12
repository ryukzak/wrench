document.getElementById('submit-button')?.addEventListener('click', () => {
  let asmElement = document.getElementById('asm')
  if (asmElement !== null && asmElement instanceof HTMLTextAreaElement) {
    asmElement.value = asmElement.value.replace(new RegExp('\u00A0', 'g'), ' ')
  }

  let configElement = document.getElementById('config')
  if (configElement !== null && configElement instanceof HTMLTextAreaElement) {
    configElement.value = configElement.value.replace(
      new RegExp('\u00A0', 'g'),
      ' ',
    )
  }

  let form = document.getElementById('main-form')
  if (form !== null && form instanceof HTMLFormElement) {
    if (form.checkValidity()) {
      form.submit()
    } else {
      form.reportValidity()
    }
  }
})
