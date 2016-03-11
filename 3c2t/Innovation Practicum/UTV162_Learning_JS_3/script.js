var selectedPanelNumber = -1;
var keyCodeArr = [37, 39, 27, 32];
var selectedClassName = "selected";
var imgHatClassName = "magnified";
var defaultText = "That's your choice!";
var textInfo;
var info;

function printHelp() {
    window.alert("Use arrows to choose the Houses, space - to see the patron ghost, escape - to remove selection.");
}

window.onload = function() {
    var hatRow = document.getElementById("hat-row");
    hatRow.addEventListener("click", onHatRowMouseClick);
    hatRow.addEventListener("mouseleave", onHatRowMouseLeave);
    info = ["Bloody Baron"]
    info.push("Grey Lady");
    info[3] = "Nearly Headless Nick";
    info[2] = "Fat Friar";
    textInfo = document.getElementById("your-choice-text").innerHTML = defaultText;
    document.addEventListener("keydown", onKeyPressed);
    printHelp();
}

function clearPanelSelection() {
    var panels = document.getElementsByClassName("panel");
    for (var i = 0; i < panels.length; i++) {
        panels[i].classList.remove(selectedClassName);
    }
}

function addPanelSelection(panelId) {
    panel = document.getElementById(panelId);
    panel.classList.add(selectedClassName);
}

function printInfo() {
    textInfo.innerHTML = info[selectedPanelNumber - 1];
}

function onKeyPressed(event) {
    if (keyCodeArr.indexOf(event.keyCode) == -1) {
        return;
    }
    switch (event.keyCode) {
        case 32: // space
            printInfo();
            break;
        case 27: // escape
            clearPanelSelection();
            selectedPanelNumber = -1;
            textInfo.innerHTML = defaultText;
            break;
        default:
            if (selectedPanelNumber == -1) {
                selectedPanelNumber = 1;
            } else {
                if (event.keyCode == 37) { // left arrow
                    selectedPanelNumber = (selectedPanelNumber >= 2) ? selectedPanelNumber - 1 : 4;
                }
                if (event.keyCode == 39) { // right arrow
                    selectedPanelNumber = selectedPanelNumber % 4 + 1;
                }
            }
            var selectedPanelId;
            clearPanelSelection();
            selectedPanelId = "dep" + selectedPanelNumber + "-panel";
            addPanelSelection(selectedPanelId);
            break;
    }
    
}

function onHatRowMouseClick(event) {
    if (event.target.id == "hat-name" || event.target.id == "img-hat") {
        document.getElementById("img-hat").classList.add("magnified");
    }
}

function onHatRowMouseLeave(event) {
    document.getElementById("img-hat").classList.remove("magnified");
}
