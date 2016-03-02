function greet() {
	var name 	= document.getElementById("user-name").value;
	var surname = document.getElementById("user-surname").value;
	var year	= document.getElementById("user-year").value;
	var curDate	= new Date();
	var age = curDate.getFullYear() - year;
	var isShortName = (name.length + surname.length < 40);
	var isValidAge  = age > 0 && age < 150;
	var isRespected = document.getElementById("user-respected").checked;
	var message		= "";
	if (isShortName) {
		message += isRespected ? "Здравствуйте, " : "Привет, ";
		message += name + " " + surname + "!";
	} else {
		message += "Имя и фамилия слишком длинные :(";
	}
	message += "<br>";
	if (isValidAge) {
		message += (isRespected ? "Вам" : "Тебе") + " к концу года будет " + age + " лет.";
	} else {
		message += "Возраст некорректен!";
	}
	document.getElementById("greeting").innerHTML = message;
}
