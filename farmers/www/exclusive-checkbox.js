// Exclusive checkbox groups (supports Shiny namespaces like xyz-E_K[])
(function () {
  window.initExclusiveGroup = function (groupName, exclusiveValues) {
    if (!Array.isArray(exclusiveValues)) exclusiveValues = [exclusiveValues];

    // Match name="group" and name="group[]", even if namespaced (…-group / …-group[])
    const sel = `input[type="checkbox"][name$="${groupName}"], input[type="checkbox"][name$="${groupName}[]"]`;

    // Remove any previous handler for a clean re‑init
    if (window.__exclHandlers?.[groupName]) {
      document.removeEventListener("change", window.__exclHandlers[groupName], true);
    }
    window.__exclHandlers = window.__exclHandlers || {};

    let busy = false;
    const handler = function (e) {
      if (busy || !e.target.matches(sel)) return;
      busy = true;

      const boxes = Array.from(document.querySelectorAll(sel));
      const isExclusive = exclusiveValues.includes(e.target.value);

      if (e.target.checked) {
        if (isExclusive) {
          // exclusive checked → uncheck all others
          boxes.forEach(b => { if (b !== e.target && b.checked) b.checked = false; });
        } else {
          // non‑exclusive checked → uncheck exclusives
          boxes.forEach(b => { if (b !== e.target && exclusiveValues.includes(b.value) && b.checked) b.checked = false; });
        }
      }

      // Notify input bindings once (avoid recursive loops/flicker)
      e.target.dispatchEvent(new Event("change", { bubbles: true }));
      busy = false;
    };

    document.addEventListener("change", handler, true);
    window.__exclHandlers[groupName] = handler;
  };
})();
