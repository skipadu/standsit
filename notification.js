function createNotification(title, body) {
  return new Notification(title, { body: body });
}

function showNotification(data) {
  if (!("Notification" in window)) {
    alert("This browser does not support desktop notification");
  }

  if (requestNotificationPermission()) {
    createNotification(data.title, data.body);
  }
}

function requestNotificationPermission() {
  if (Notification.permission !== "denied") {
    Notification.requestPermission().then(function (permission) {
      if (permission === "granted") {
        return true;
      }
      else {
        return false;
      }
    });
  }
  return true;
}
