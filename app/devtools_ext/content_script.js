console.log("content script");

window.addEventListener('message', function(event) {
  console.log(event);

  // Only accept messages from the same frame
  if (event.source !== window) {
    return;
  }

  var message = event.data;

  // Only accept messages that we know are ours
  if (typeof message !== 'object' || message === null ||
      message.source !== 'my-devtools-extension') {
    return;
  }

  chrome.runtime.sendMessage(message);
});
