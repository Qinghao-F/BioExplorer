// frontend/src/main.jsx
import React from 'react'
import { createRoot } from 'react-dom/client'
import App from './App.jsx'
import './index.css'

function mount(id) {
  const el = document.getElementById(id)
  if (!el) return
  const root = createRoot(el)
  root.render(<App />)
}

// Mount in Shiny (div#react-root) and in dev (div#root)
mount('react-root')
mount('root')

// If Shiny reloads the page content via hot-reload, attempt remount
document.addEventListener('shiny:connected', () => mount('react-root'))
