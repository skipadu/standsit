function createNotification(title, body) {
  return new Notification(title, { body: body });
}

function showNotification(data) {
  if (!("Notification" in window)) {
    alert("This browser does not support desktop notification");
  }

  else if (Notification.permission === "granted") {
    createNotification(data.title, data.body);
  }

  else if (Notification.permission !== "denied") {
    Notification.requestPermission().then(function (permission) {
      if (permission === "granted") {
        createNotification(data.title, data.body);
      }
    });
  }
}
