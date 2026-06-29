// frontend/src/Index.jsx
import React, { useEffect, useState } from 'react'
import './index.css'

export default function Index({ onStart }) {
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const timer = setTimeout(() => setLoading(false), 2500)
    return () => clearTimeout(timer)
  }, [])

  return (
    <div className="bio-index">
      <div className="bio-bg" />
      <div className="bio-content">
        <h1 className="bio-title">BIOEXPLORER</h1>
        <p className="bio-sub">
          Discover the living diversity of urban nature.  
          Explore plants, butterflies, and gardens across Melbourne.
        </p>

        {loading ? (
          <div className="bio-loading">
            <div className="bio-loading-bar">
              <div className="bio-loading-progress" />
            </div>
            <span>Loading ecosystem modules…</span>
          </div>
        ) : (
          <button className="bio-btn" onClick={onStart}>
            Explore Now
          </button>
        )}
      </div>
    </div>
  )
}
