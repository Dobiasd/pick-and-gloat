var elmContent = undefined;

function AcceptCookies() {
  if (lsTest() === true) {
    try {
      localStorage.setItem("cookieConsent", "true");
    } catch(e) {
    }
  }
  CheckCookieConsent();
}

function HideCookieRemark()
{
  document.getElementById('cookieRemark').style.height ='0px';
  document.getElementById('cookieRemark').style.padding ='0px';
  document.getElementById('cookieRemark').style.visibility = 'hidden';
  document.getElementById('ads').style.top = '3px';
}

function ShowCookieRemark()
{
  document.getElementById('cookieRemark').style.height ='18px';
  document.getElementById('cookieRemark').style.padding ='10px';
  document.getElementById('cookieRemark').style.visibility = 'visible';
  document.getElementById('ads').style.top = '41px';
}

function CheckCookieConsent() {
  if (lsTest() === true) {
    try {
      cookieConsent = localStorage.getItem("cookieConsent");
      if (cookieConsent == "true") {
        HideCookieRemark();
      }
      else {
        ShowCookieRemark();
      }
    } catch(e) {
      return "";
    }
  }
}

// http://stackoverflow.com/questions/16427636/check-if-localstorage-is-available
function lsTest(){
  var test = 'test';
  try {
    localStorage.setItem(test, test);
    localStorage.removeItem(test);
    return true;
  } catch(e) {
    return false;
  }
}

function SetAdsXPos(x) {
  var ads = document.getElementById('ads');
  if (!ads)
    return;
  ads.style.left = x.toString() + "px";
}

function GetWindowWidth() {
  return $(this).width();
}

function GetWindowHeight() {
  return $(this).height();
}

function CenterAds() {
  var remainingWidth = Math.max(0, GetWindowWidth() - 728)
  x = remainingWidth / 2;
  SetAdsXPos(x);
}

function OnResize() {
  elmContent.ports.windowWidth.send(GetWindowWidth());
  elmContent.ports.windowHeight.send(GetWindowHeight());
  CenterAds();
}

function Init() {
  CheckCookieConsent();
  CenterAds();
  $(window).on('resize', OnResize);
  var mainDiv = document.getElementById('main');
  elmContent = Elm.embed(Elm.Main, mainDiv,
    {windowWidth : GetWindowWidth(), windowHeight : GetWindowHeight()});
}