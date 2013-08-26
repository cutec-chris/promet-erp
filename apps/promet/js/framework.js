var Params = {
  Server: "localhost:8086",
  Theme:""
  }

  function loadPage(link){
    if (link == "index.html")
      {
        document.getElementsByTagName('nav')[0].style.display="block";
        document.getElementById('main').style.display="none";
        //$('#main > .toolbar > a').detach()
      }
    else
      {
        var request =  new XMLHttpRequest();
        request.onreadystatechange = function() {
          if (request.readyState == 4) {
            if ((request.status == 200)||(request.status == 0))
              {
                var mainDiv = document.getElementById('main');
                mainDiv.innerHTML = request.response;
                hideLoading();
                var ob = mainDiv.getElementsByTagName("script");
                for(var i=0; i<ob.length; i++){
                  if(ob[i].text!=null){
                    var ascript = ob[i].text;
                    var scripts = document.getElementsByTagName("script");
                    for (i=0; i<scripts.length; i++) {
                      var url = scripts[i].getAttribute("src");
                      if(!url) continue;
                      if(scripts[i].getAttribute("class")=="ownscript"){
                      scripts[i].parentNode.removeChild(scripts[i]);
                      }
                    }
                  // Anlegen und Einfügen des neuen Skripts
                  var script = document.createElement("script");
                  script.text=ascript;
                  script.setAttribute("type", "text/javascript");
                  script.setAttribute("class", "ownscript");
                  document.body.appendChild(script);
                  }
                }
              }
            else
              console.log('failed to fetch Page '+link+' '+request.status);
          }
          };
        request.open('get', link, true);
        request.send(null);
      }
  }
  function LinkClicked(e){
    var link = this.getAttribute("href");
    if (link != "#"){
      e.preventDefault();
      loadPage(link.substr(1,link.length)+'.html');
    }
    history.pushState(null, null, link);
  }
  function hideAddressBar(){
    if(document.documentElement.scrollHeight<window.innerHeight/window.devicePixelRatio)
      document.documentElement.style.height=(window.innerHeight/window.devicePixelRatio)+'px';
    if(navigator.userAgent.match(/Android/i))
      {
      setTimeout(function(){window.scrollTo(0,1)},0);
      }
    else
      setTimeout(function(){window.scrollTo(1,1)},0);
  }
  //hide loading bar
  function hideLoading(){
    var windowWidth = window.innerWidth;
    document.getElementById('main').style.display="block";
    if (windowWidth < 481)
      {
        document.getElementsByTagName('nav')[0].style.display="none";
        link=document.createElement('a');
        link.href="#index";
        link.className="back";
        link.text="zurück";
        link.addEventListener('click',LinkClicked);
        if (document.getElementsByClassName('toolbar')[1] != null)
          document.getElementsByClassName('toolbar')[1].appendChild(link);
      }
    hideAddressBar();
  };


document.onreadystatechange = function() {
  if (document.readyState === 'complete'){
  if (getCookie("prometServer")!=null)
    Params.Server = getCookie("prometServer");
  function appendSheet(href){
    link=document.createElement('link');
    link.href=href;
    link.rel="stylesheet";
    link.type="text/css";
    document.getElementsByTagName('head')[0].appendChild(link);
  }
  if (getCookie("prometTheme")!=null) {
    Params.Theme = getCookie("prometTheme");
    appendSheet("themes/"+Params.Theme+"/theme.css");
  }
  else {
  if (navigator.userAgent.match(/webOS/i) ||
      navigator.userAgent.match(/Windows Phone/i) ||
      navigator.userAgent.match(/BlackBerry/) ||
      navigator.userAgent.match(/ZuneWP7/i)
     ){ appendSheet("themes/jqt/theme.css");}
  else if (
      navigator.userAgent.match(/Android/i)
      ) {appendSheet("themes/android/theme.css");}
  else if (
      navigator.userAgent.match(/iPhone/i) ||
      navigator.userAgent.match(/iPod/i)
      ){appendSheet("themes/apple/theme.css");}
  else if (
      navigator.userAgent.match(/iPhone/i) ||
      navigator.userAgent.match(/iPod/i)
      ){ appendSheet("themes/apple/theme.css");}
  else if (
      navigator.userAgent.match(/iPad/i)
      ){ appendSheet("themes/apple/theme.css"); }
  else { appendSheet("themes/apple/theme.css"); }
  }
  window.addEventListener("resize",function(){hideAddressBar();});
  window.addEventListener("load",function(){hideAddressBar();});
  window.addEventListener("orientationchange",function(){hideAddressBar();});
  window.onpopstate = function(event) {
    var url = document.location;
    var sharp = String(url).indexOf("#")+1;
    loadPage(String(url).substr(sharp,String(url).length)+'.html');
  };
  var links = document.getElementsByTagName('a');
  for (var i=0; i < links.length; i++){
    links[i].addEventListener('click',LinkClicked);
  }
  function ConnectionAvalibe(){
    var FConnectionOK=false;
    DoGet("http://"+Params.Server+"/?action=connectionavail&random="+encodeURIComponent(Math.random()));
    ConnTestTimer = window.setTimeout("ConnectionTimeout()", 100);
  }
  if (navigator.onLine) {
      ConnectionAvalibe();
  }
}
};
function ConnectionTimeout(){
  window.clearTimeout(ConnTestTimer);
  OnDisconnected();
}
function ConnectionOK(){
  window.clearTimeout(ConnTestTimer);
  FConnectionOK = true;
  OnConnected();
  DoGet("http://"+Params.Server+"/?action=checklogin&random="+encodeURIComponent(Math.random()));
}
function DoLogout(){
  DoGet("http://"+Params.Server+"/?action=logout&random="+encodeURIComponent(Math.random()));
  OnDisconnected();
  OnConnected();
}

