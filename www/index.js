Shiny.addCustomMessageHandler("toggleActive", function (x) {
  var el = document.getElementById(x.id);
  if (!el) return;
  if (x.active) el.classList.add("active");
  else el.classList.remove("active");
});
