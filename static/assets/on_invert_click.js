function onInvertClick() {
  document.body.classList.toggle('dark')
  window.localStorage.setItem(
    'prefers-color-scheme',
    document.body.classList.contains('dark') ? 'dark' : 'light',
  )
}

if (
  window.matchMedia &&
  window.localStorage.getItem('prefers-color-scheme') === null
) {
  if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
    document.body.classList.add('dark')
  } else {
    document.body.classList.remove('dark')
  }
} else if (window.localStorage.getItem('prefers-color-scheme') === 'light') {
  document.body.classList.remove('dark')
}

const invertThemeButton = document
  .getElementById('invert-theme-button')
  ?.addEventListener('click', () => {
    onInvertClick()
  })
