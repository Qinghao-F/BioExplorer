// pages/biodiversity/assets/bio.js
// Dynamically measure navbar actual height to eliminate 1px gap
(function () {
  function setNavH() {
    var nav = document.querySelector(".navbar");
    var h = nav ? nav.offsetHeight : 56;
    document.documentElement.style.setProperty("--nav-h", h + "px");
  }
  window.addEventListener("load", setNavH);
  window.addEventListener("resize", setNavH);
})();

// bio.js
$(document).ready(function () {
  const divider = document.querySelector(".bio-divider");
  const leftPanel = document.querySelector(".bio-side-panel");
  const mapContainer = document.querySelector(".bio-map-container");

  let isDragging = false;

  divider.addEventListener("mousedown", (e) => {
    isDragging = true;
    document.body.style.cursor = "col-resize";
    e.preventDefault();
  });

  document.addEventListener("mousemove", (e) => {
    if (!isDragging) return;
    const newWidth = (e.clientX / window.innerWidth) * 100;
    if (newWidth > 15 && newWidth < 70) {
      leftPanel.style.width = newWidth + "%";
      if (window.myLeafletMap) {
        window.myLeafletMap.invalidateSize();
      }
    }
  });

  document.addEventListener("mouseup", () => {
    isDragging = false;
    document.body.style.cursor = "default";
  });
});
