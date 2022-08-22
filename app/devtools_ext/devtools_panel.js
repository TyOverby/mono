console.log("devtools_panel");
 
var backgroundPageConnection = chrome.runtime.connect({ name: "panel" });
var tabId = chrome.devtools.inspectedWindow.tabId;

backgroundPageConnection.postMessage({ name: 'init', tabId });

backgroundPageConnection.onMessage.addListener(function (message) {
    console.log("devtools_panel.js", message);
});
