// Exclusive checkbox groups – scoped to the input container id, no synthetic events
(function () {
  window.initExclusiveGroup = function (groupId, exclusiveValues) {
    const root = document.getElementById(groupId);
    if (!root) return;

    const excl = new Set([].concat(exclusiveValues || []).filter(Boolean));
    window.__exclHandlers = window.__exclHandlers || {};
    if (window.__exclHandlers[groupId]) {
      root.removeEventListener("change", window.__exclHandlers[groupId], true);
    }

    let busy = false;
    const handler = function (e) {
      if (busy || e.target.type !== "checkbox" || !root.contains(e.target)) return;
      busy = true;

      const boxes = Array.from(root.querySelectorAll('input[type="checkbox"]'));
      const isExclusive = excl.has(e.target.value);

      if (e.target.checked) {
        if (isExclusive) {
          boxes.forEach(b => { if (b !== e.target && b.checked) b.checked = false; });
        } else {
          boxes.forEach(b => { if (b !== e.target && excl.has(b.value) && b.checked) b.checked = false; });
        }s
      }

      // no synthetic dispatch; Shiny already got the user's original change event
      busy = false;
    };

    root.addEventListener("change", handler, true);
    window.__exclHandlers[groupId] = handler;
  };
})();