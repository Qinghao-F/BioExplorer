// frontend/src/App.jsx
import React, { useEffect, useState } from 'react'

function Splash({ onDone }) {
  const [closing, setClosing] = useState(false)
  const finish = () => {
    if (!closing) {
      setClosing(true)
      setTimeout(() => onDone && onDone(), 500) // allow fade-out
    }
  }
  return (
    <div className={`gx-splash ${closing ? 'gx-splash-hide' : ''}`}>
      <div className="gx-splash-card">
        <div className="gx-splash-title">Welcome to GardenAPP!</div>
        <div className="gx-splash-sub">Please explore Melbourne City!</div>
        <div className="gx-loading-bar"><div className="gx-loading-progress" /></div>
        <div className="gx-splash-tip">Loading maps and interface…</div>
      </div>
    </div>
  )
}

export default function App() {
  const [note, setNote] = useState('')
  const [pageLoaded, setPageLoaded] = useState(false)
  const [shinyReady, setShinyReady] = useState(false)
  const [showSplash, setShowSplash] = useState(true)

  useEffect(() => {
    const onWindowLoad = () => setPageLoaded(true)
    window.addEventListener('load', onWindowLoad)

    const onShinyIdle = () => setShinyReady(true)
    const onShinyConnected = () => {
      // we wait for idle to ensure outputs are rendered
    }
    document.addEventListener('shiny:idle', onShinyIdle)
    document.addEventListener('shiny:connected', onShinyConnected)

    // Failsafe: if something blocks events, hide splash after 8s
    const fail = setTimeout(() => setShowSplash(false), 8000)

    return () => {
      window.removeEventListener('load', onWindowLoad)
      document.removeEventListener('shiny:idle', onShinyIdle)
      document.removeEventListener('shiny:connected', onShinyConnected)
      clearTimeout(fail)
    }
  }, [])

  useEffect(() => {
    if (pageLoaded && shinyReady) {
      // let the CSS animation play for a moment
      const t = setTimeout(() => setShowSplash(false), 400)
      return () => clearTimeout(t)
    }
  }, [pageLoaded, shinyReady])

  const refresh = () => {
    // Send a global signal into Shiny to reload CSVs
    window.Shiny && window.Shiny.setInputValue('react_refresh', Date.now(), { priority: 'event' })
  }

  return (
    <>
      {showSplash && <Splash onDone={() => setShowSplash(false)} />}
      <div className="gx-header">
        <div className="gx-brand">🌿 GardenApp</div>
        <div className="gx-actions">
          <input
            className="gx-input"
            placeholder="Optional note for refresh…"
            value={note}
            onChange={(e) => setNote(e.target.value)}
          />
          <button className="gx-btn" onClick={refresh} title="Reload CSVs from dataset/*.csv">
            Refresh Data
          </button>
        </div>
      </div>
    </>
  )
}
