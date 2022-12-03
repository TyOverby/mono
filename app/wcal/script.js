
let day_of_week_container = document.querySelector("#day-of-week-container");
let day_container = document.querySelector("#day-container");

let days = []
let days_of_the_week = 
  [
"MON",
"TUE",
"WED",
"THU",
"FRI",
"SAT",
"SUN",
  ]

let months = 
  [
["JAN", "sky", 1],
["FEB", "fuchsia", 2],
["MAR", "indigo", 3],
["APR", "red", 4],
["MAY", "emerald", 5],
["JUN", "cyan", 6],
["JUL", "red", 7],
["AUG", "sky", 8],
["SEP", "yellow", 9],
["OCT", "orange", 10],
["NOV", "stone", 11],
["DEC", "sky", 12],
  ]

function days_in_month(month) {
  return new Date(2023, month, 0).getDate();
}

for (let label of days_of_the_week) {
  let day = (document.createElement("div"));
  day.className = "day-label";
  day.appendChild(document.createTextNode(label));
  day_of_week_container.appendChild(day);
}

function make_event() {
  let event = document.createElement("div");
  event.className = "event";
  if (Math.random() < 0.3) {
    event.appendChild(document.createTextNode("this is the text for a pretty long event"));
  } else {
    event.appendChild(document.createTextNode("event " + (Math.random() + "").substring(2, 5)));
  }
  return event
}

function make_new_month(month) {
  let event = document.createElement("div");
  event.className = "new_month";
  event.appendChild(document.createTextNode(month));
  return event
}

for (let i = 0; i < 6; i ++ ) {
  let day = document.createElement("div");
    let color = months[0][1];
  day.className = "day blank";
  day.style.setProperty("--month-color-50", globalThis.colors[color]["50"])
  for (let i = 1; i <= 9; i++) {
    let ik = i * 100;
    day.style.setProperty("--month-color-" + ik, globalThis.colors[color]["" + ik])
  }
  day_container.appendChild(day);
}

let seen = {};

for (let [month, color, month_idx] of months) {
  for (let day_of_month = 1; day_of_month < days_in_month(month_idx) + 1; day_of_month++) {
    let day = document.createElement("div");
    let day_of_week = days_of_the_week[(new Date(2023, month_idx - 1, day_of_month).getDay() + 6) % 7];
    day.className = `day ${day_of_week}`;
    day_container.appendChild(day);
    //day.appendChild(new Text(day_of_week));

    if (!seen[day_of_week]) { 
        day.className += " first";
    }
    seen[day_of_week] = true;



    let day_label = document.createElement("div");
    day_label.className = "day-of-month";
    if (day_of_month == 1) {
      day_label.className += " new-month";

      let empty_span = document.createElement("span");
      empty_span.appendChild(document.createTextNode(""));
      day_label.appendChild(empty_span);

      let month_span = document.createElement("span");
      month_span.appendChild(document.createTextNode(month));
      day_label.appendChild(month_span);

    } 
    day_label.appendChild(document.createTextNode(day_of_month + ""));
    day.appendChild(day_label);

    day.style.setProperty("--month-color-50", globalThis.colors[color]["50"])
    for (let i = 1; i <= 9; i++) {
      let ik = i * 100;
      day.style.setProperty("--month-color-" + ik, globalThis.colors[color]["" + ik])
    }

    if (Math.random() < 0.0) {
      let event = make_event();
      day.appendChild(event);

      if (Math.random() < 0.5) {
        let event = make_event();
        day.appendChild(event);
      }
    }
  }
}


