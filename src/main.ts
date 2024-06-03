// @ts-ignore
import { Elm } from "./Main.elm";

var app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: Math.floor(Math.random()*0xFFFFFFFF)
});

var subscription: number | null = null;

const getRndInteger = (min: number, max: number)  =>
    Math.floor(Math.random() * (max - min) ) + min;

const wait = (ms: number)  =>
    Promise.resolve(r=>setTimeout(r,ms))

function randomInterval(fn: () => void) {
    subscription = setTimeout(() => {
        fn();
        randomInterval(fn);
    }, getRndInteger(1000, 10000)
    )
}



app.ports.toggleDealGeneration.subscribe(() => {
    if (subscription != null) {
        // stop sending
        clearTimeout(subscription);
        subscription = null
    } else {
        randomInterval(() => app.ports.generateDeal.send())
    }
})
