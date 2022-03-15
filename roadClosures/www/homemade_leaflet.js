Shiny.addCustomMessageHandler('R_message', function(data) {
    if (data.type == "lines") {
      L.polyline(data, data.option ? data.option : null).addTo(myMap);
    }
    if (data.type == "points") {

    }
    if (data.type == "flyTo") {
      myMap.flyTo(data, data.zoom ? data.zoom : 13);
    }
    if (data.type == "function_call") {
      if (data.message == "flyToBrisbane") {
        goToBrisbane()
      }
      if (data.message == "saveView") {
        saveView()
      }
      if (data.message == "setView") {
        setView()
      }
    }
})

window.onload = function() {
  console.log("Onload event")
  const textbox = document.createElement("div")
  textbox.className = "coordinate-box"
  document.querySelector("body").appendChild(textbox)

  setTimeout(function() {
    myMap.addEventListener('mousemove', function(ev) {
      textbox.innerText = `Lng: ${ev.latlng.lng}, Lat: ${ev.latlng.lat}`;
    })
  }, 1000);
}

const goToBrisbane = function() {
  myMap.flyTo([-27.475703, 153.032212], 13)
}

const showPaths = function(lines) {
  for (let line of lines) {
    L.Polyline(line).addTo(myMap)
  }
}

GLOBAL = {center: null, zoom: 13};
const saveView = function() {
  GLOBAL.center = myMap.getCenter();
  GLOBAL.zoom = myMap.getZoom();
}

const setView = function() {
  if (GLOBAL.center) {
    setTimeout(
      () => {
        console.log("Set View!")
        myMap.setView(GLOBAL.center, GLOBAL.zoom, {animate: false})
      },
      0
    )
  }
}


