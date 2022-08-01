import "./paper.min.css";
import "./style.css";
import cards from "./cards.json";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root, flags: cards});

app.ports.scrollToElementById.subscribe((elementId) => {
    document.querySelector(`#${elementId}`)?.scrollIntoView();
});