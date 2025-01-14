import { fromEvent, merge } from "rxjs";
import { map, mergeScan, first } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById("markdown-input") as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;
const button = document.getElementById("save-button") as HTMLButtonElement;
const customTitleInput = document.getElementById("custom-title") as HTMLInputElement;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, markdown: value })),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value })),
);

// stream for button 
const saveButton$: Observable<Action> = fromEvent(button, "click").pipe(
    map(() => (s) => ({ ...s, save: true })), 
);

// track title input
const titleInputChange$: Observable<Action> = fromEvent(customTitleInput, "input").pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s: State) => ({ ...s, title: value })), // Update title
);

// send html and title to haskell backend
function saveHTML(content: string, title: string) {
    fetch("/api/convertMD", {
        method: "POST",
        headers: {
            "Content-Type": "application/json", 
        },
        body: `{"content":${content.replace(/\n/g, '\\n').replace(/"/g, '\\"')},"title":${title}`, // send info to backend
    }).then(response => response.json())
      .then(data => {
          console.log(data);
      })
      .catch(error => {
          console.error('Error:', error);
      });
}

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.markdown,
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            return {
                ...s,
                HTML: data.html,
            };
        }),
        first(),
    );
}

const initialState: State = {
    markdown: "",
    HTML: "",
    title: "",
    renderHTML: true,
    save: false,
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, checkboxStream$, saveButton$, titleInputChange$)
        .pipe(
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // send a response if save button is clicked
                if (newState.save) {
                    saveHTML(newState.HTML, newState.title);
                    return [ { ...newState, save: false } ];
                }
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                else {
                    return getHTML(newState).pipe(
                        map((s) => ({...s, save: false}))
                    )
                }
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById("html-output");
            // rendered output
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput.textContent = value.HTML;
                }
            }
            // html output
            const htmlOutput2 = document.getElementById("html-output-textarea");
            if (htmlOutput2) {
                htmlOutput2.innerHTML = "";
                htmlOutput2.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                        htmlOutput2.innerHTML = highlight + '\n' + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput2.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput2.textContent = value.HTML;
                }
            }
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
