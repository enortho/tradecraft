import { Elm } from './src/Main.elm';

var app = Elm.Main.init({
  node : document.getElementById('app'),
  flags: Math.floor(Math.random()*0xFFFFFFFF)
});

var subscription = null;

const getRndInteger = (min, max)  =>
  Math.floor(Math.random() * (max - min) ) + min;

function randomInterval(fn) {
  subscription = setTimeout(() => {
    fn();
    randomInterval(fn);
  }, getRndInteger(1000, 10000))
}



app.ports.toggleDealGeneration.subscribe(() => {
  if (subscription != null) {
    // stop sending
    clearTimeout(subscription);
    subscription = null
  } else {
    randomInterval(() => app.ports.generateDeal.send())
  }
});

