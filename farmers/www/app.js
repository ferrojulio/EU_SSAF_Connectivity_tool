// app.js

Shiny.addCustomMessageHandler('setAppPrefix', msg => {
  window.appPrefix = msg;
  console.log('✅ appPrefix set to:', window.appPrefix);
});

// ===== session storage helpers =====
function clearLocalStorage() {
  const currentAppPrefix = window.appPrefix || 'default';
  localStorage.removeItem(currentAppPrefix + '_sessionToken');
  localStorage.removeItem(currentAppPrefix + '_currentPage');
  location.reload();
}

function getFromLocalStorage(key) {
  return localStorage.getItem(key) || '';
}

// ===== legacy handlers =====
Shiny.addCustomMessageHandler('restartApp', msg => {
  clearLocalStorage();
});

Shiny.addCustomMessageHandler('saveToLocalStore', msg => {
  localStorage.setItem(msg.key, msg.value);
});

Shiny.addCustomMessageHandler('setSessionToken', msg => {
  if (!msg.key) return;
  let token = msg.value;
  if (typeof token !== 'string') {
    try { token = JSON.stringify(token); }
    catch(e) { token = String(token); }
    console.warn('Normalized token to string:', token);
  }
  localStorage.setItem(msg.key, token);
  console.log('✅ localStorage set:', msg.key, token);
  // ADD THIS LINE TO VERIFY IMMEDIATE READBACK
  console.log('🔍 localStorage readback verification:', msg.key, localStorage.getItem(msg.key));
});

Shiny.addCustomMessageHandler('clearLocalStorageKey', msg => {
  if (msg.key) {
    localStorage.removeItem(msg.key);
    console.log('✅ localStorage removed:', msg.key);
  }
});

// ===== unified page-change handler =====
Shiny.addCustomMessageHandler('pageChanged', msg => {
  // 1️⃣ Update the UI to show page "msg.page" (removed updateCurrentPage function call)

  // 2️⃣ Scroll smoothly to top
  window.scrollTo({ top: 0, behavior: 'smooth' });

  // 3️⃣ (Optional) show disconnect overlay via existing handler
  //    Shiny.addCustomMessageHandler('disconnectedAlert', …) will pick this up.

  // 4️⃣ Push into browser history
  if (window.history && window.history.pushState) {
    window.history.pushState({ page: msg.page }, '');
  }

  // 5️⃣ Persist for full-page reloads using appPrefix
  if (msg.appPrefix && msg.page !== undefined) {
    localStorage.setItem(msg.appPrefix + '_currentPage', msg.page);
  }
});

// ===== on Shiny connect: restore tokens + page index =====
$(document).on('shiny:connected', () => {
  // Get appPrefix from a hidden input or a global JS variable set by R
  // For now, we'll assume it's passed via a custom message or a global variable
  // This part will be handled by the R side sending the appPrefix on connect.
  // For initial load, we might need a default or a way to infer.

  // Let's assume the appPrefix is set by the R server via a custom message
  // and stored in a global variable, e.g., window.appPrefix
  const currentAppPrefix = window.appPrefix || 'default'; // Fallback to 'default'

  const sessionTokenKey = currentAppPrefix + '_sessionToken';
  const currentPageKey = currentAppPrefix + '_currentPage';

  const restoredSessionToken = getFromLocalStorage(sessionTokenKey);
  if (restoredSessionToken) {
    Shiny.setInputValue('restoredSessionToken', restoredSessionToken, {priority:'event'});
  }

  const rawPage = getFromLocalStorage(currentPageKey);
  const parsedPage = parseInt(rawPage, 10);
  Shiny.setInputValue(
    'restoredPage',
    isNaN(parsedPage) ? 0 : parsedPage,
    {priority:'event'}
  );
});

// ===== browser back/forward =====
window.onpopstate = event => {
  if (event.state && typeof event.state.page !== 'undefined') {
    Shiny.setInputValue('browserBackPage', event.state.page, {priority:'event'});
  }
};

function updateRankChoices(selectId, selectedValue) {
  if (selectedValue === "") {
    return;
  }

  // Restore the previously selected value to other dropdowns
  const previousValue = $(`#${selectId}`).data('previous-value');
  if (previousValue) {
    $('.soil-service-ranking-item select').not($(`#${selectId}`)).each(function() {
      $(this).find(`option[value='${previousValue}']`).prop('disabled', false);
    });
  }

  // Disable the newly selected value in other dropdowns
  $('.soil-service-ranking-item select').not($(`#${selectId}`)).each(function() {
    $(this).find(`option[value='${selectedValue}']`).prop('disabled', true);
  });

  // Store the current value for the next change
  $(`#${selectId}`).data('previous-value', selectedValue);
}