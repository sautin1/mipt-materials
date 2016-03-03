var peopleDict = {};

function countAge(year) {
    var curDate = new Date();
    return curDate.getFullYear() - year;
}

function countYear(age) {
    return countAge(age);
}

function isValidAge(age) {
    return age > 0 && age < 150;
}

function isDigit(str) {
    return str.search(/[0-9]/) >= 0;
}

function isAlpha(str) {
    var isRussianLetter = str.search(/[А-Яа-я]/);
    var isEnglishLetter = str.search(/[A-Za-z]/);
    return isRussianLetter >= 0 || isEnglishLetter >= 0;
}

function strip(line) {
    var beginIndex = 0;
    var endIndex = line.length;
    while (beginIndex < endIndex && !isAlpha(line.charAt(beginIndex)) && !isDigit(line.charAt(beginIndex))) {
        beginIndex++;
    }
    while (endIndex > beginIndex + 1 && !isAlpha(line.charAt(endIndex - 1)) && !isDigit(line.charAt(endIndex - 1))) {
        endIndex--;
    }
    return line.slice(beginIndex, endIndex);
}

function Person(name, surname, age, respected) {
    this.name = name;
    this.surname = surname;
    this.age = age;
    this.isRespected = respected;
}

Person.prototype.generateInfo = function () {
    var info = "";
    for (var x in this) {
        if (x === "generateInfo") {
            continue;
        }
        info += this[x] + ", ";
    };
    return strip(info);
}

function printHelp() {
    window.alert("Заполните все поля, чтобы добавить человека в БД. Введите только фамилию, чтобы найти человека в БД.");
}

function greet() {
    var nameElem      = document.getElementById("user-name");
    var surnameElem   = document.getElementById("user-surname");
    var yearElem      = document.getElementById("user-year");
    var respectedElem = document.getElementById("user-respected");
    var greetingElem  = document.getElementById("greeting");

    var name      = nameElem.value;
    var surname   = surnameElem.value;
    var year      = yearElem.value;
    var respected = respectedElem.checked;

    var age    = countAge(year);
    var person = null;
    var info   = "";
    if (!surname) {
        greetingElem.innerHTML = "Необходимо указать фамилию";
        return;
    }
    if (!name || !age) {
        if (surname in peopleDict) {
            person = peopleDict[surname];
            nameElem.value        = person.name;
            surnameElem.value     = person.surname;
            yearElem.value        = countYear(person.age);
            respectedElem.checked = person.isRespected;

            greetingElem.innerHTML = "Найден";
        } else {
            greetingElem.innerHTML = "Не найден";
        }
    } else {
        if (!isValidAge(age)) {
            greetingElem.innerHTML = "Неверный возраст";
            return;
        }   
        person = new Person(name, surname, age, respected);
        info = person.generateInfo();
        peopleDict[surname] = person;

        nameElem.value        = "";
        surnameElem.value     = "";
        yearElem.value        = "";
        respectedElem.checked = false;

        greetingElem.innerHTML = "Добавлен " + info;
    }
}
