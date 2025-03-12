function onInvertClick() {
  document.body.classList.toggle('inverted')
  window.localStorage.setItem(
    'prefers-color-scheme',
    document.body.classList.contains('inverted') ? 'light' : 'dark',
  )
}

if (
  window.matchMedia &&
  window.localStorage.getItem('prefers-color-scheme') === null
) {
  if (!window.matchMedia('(prefers-color-scheme: dark)').matches) {
    document.body.classList.add('inverted')
  }
} else if (window.localStorage.getItem('prefers-color-scheme') === 'light') {
  document.body.classList.add('inverted')
}

const invertThemeButton = document
  .getElementById('invert-theme-button')
  ?.addEventListener('click', () => {
    onInvertClick()
  })
