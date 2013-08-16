function goTo(url){
  var a = document.createElement("a");
  if (a.click){
    a.setAttribute("href", url);
    a.style.display = "none";
    document.body.appendChild(a);
    a.click();
  } else {
    window.location = url;
  }
}
function DoGet(aurl){
  var scripts = document.getElementsByTagName("script");
  for (i=0; i<scripts.length; i++) {
    var url = scripts[i].getAttribute("src");
    if(!url) continue;
    if(url.indexOf("appbase")>=0) {
      scripts[i].parentNode.removeChild(scripts[i]);
    }
  }
  // Anlegen und Einfügen des neuen Skripts
  var script = document.createElement("script");
  script.setAttribute("src", aurl);
  script.setAttribute("type", "text/javascript");
  document.body.appendChild(script);
}
