document.onreadystatechange = function() {
  if (document.readyState === 'complete'){
  var Params = {
    Server: "localhost:8086"
    }
  function appendSheet(href){
    link=document.createElement('link');
    link.href=href;
    link.rel="stylesheet";
    link.type="text/css";
    document.getElementsByTagName('head')[0].appendChild(link);
  }
  if (getCookie("prometServer")!="")
    Params.Server = getCookie("prometServer");
  if (navigator.userAgent.match(/webOS/i) ||
      navigator.userAgent.match(/Windows Phone/i) ||
      navigator.userAgent.match(/BlackBerry/) ||
      navigator.userAgent.match(/ZuneWP7/i)
     )
    {
      appendSheet("themes/jqt/theme.css");
    }
  else if (
      navigator.userAgent.match(/Android/i)
      )
    {
      appendSheet("themes/android/theme.css");
    }
  else if (
      navigator.userAgent.match(/iPhone/i) ||
      navigator.userAgent.match(/iPod/i)
      )
    {
      appendSheet("themes/apple/theme.css");
    }
  else if (
      navigator.userAgent.match(/iPhone/i) ||
      navigator.userAgent.match(/iPod/i)
      )
    {
      appendSheet("themes/apple/theme.css");
    }
  else if (
      navigator.userAgent.match(/iPad/i)
      )
    {
      appendSheet("themes/apple/theme.css");
    }
  else
    {
      appendSheet("themes/windowsphone/theme.css");
    }
  function hideAddressBar(){
    if(document.documentElement.scrollHeight<window.outerHeight/window.devicePixelRatio)
      document.documentElement.style.height=(window.outerHeight/window.devicePixelRatio)+'px';
    if(navigator.userAgent.match(/Android/i))
      {
      setTimeout(window.scrollTo(0,1),0);
      }
    else
      setTimeout(window.scrollTo(1,1),0);
  }
  window.addEventListener("resize",function(){hideAddressBar();});
  window.addEventListener("load",function(){hideAddressBar();});
  window.addEventListener("orientationchange",function(){hideAddressBar();});
  window.onpopstate = function(event) {
    var url = document.location;
    var sharp = String(url).indexOf("#")+1;
    loadPage(String(url).substr(sharp,String(url).length)+'.html');
  };
  $('a').click(function(e){
    var link = $(this).attr("href");
    if (link != "#")
      {
        e.preventDefault();
        loadPage(link.substr(1,link.length)+'.html');
      }
    history.pushState(null, null, link);
    });
  function loadPage(link){
    if (link == "index.html")
      {
        $('nav').show();
        $('#main').hide();
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
                var ob = mainDiv.getElementsByTagName("script");
                for(var i=0; i<ob.length; i++){
                  if(ob[i].text!=null) eval(ob[i].text);
                }
                hideLoading();
              }
            else
              console.log('failed to fetch Page '+link+' '+request.status);
          }
          };
        request.open('get', link, true);
        request.send(null);
      }
  }
  //hide loading bar
  function hideLoading(){
    var windowWidth = window.screen.width < window.outerWidth ?
                      window.screen.width : window.outerWidth;
    $('#main').show();
    if (windowWidth < 481)
      {
        $('nav').hide();
        $('#main > .toolbar').append('<a href="#index" class="back">zur&uuml;ck</a>')
      }
    hideAddressBar();
  };
}
};
