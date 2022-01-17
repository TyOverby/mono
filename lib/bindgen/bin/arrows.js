function box(e) {
  let x = e.offsetLeft;
  let y = e.offsetTop;
  let width = e.offsetWidth;
  let height = e.offsetHeight;
  let midx = x + (width / 2);
  let midy = y + (height / 2);
  return { x, y, width, height, midx, midy }
}

function linep(from, to, width) {
  width = width || 1;
  return `<line 
            x1="${from.x}" 
            y1="${from.y}" 
            x2="${to.x}" 
            y2="${to.y}" 
            stroke-width="${width}px"
            stroke="rgb(50,50,50)"
            stroke-linecap="round" />`
}

function curvep(from, to, from_a, to_a) {
 return `<path
            stroke="rgba(50,50,50)"
            stroke-width="2px"
            fill="transparent"
            stroke-linecap="round"
            d="M ${from.x} ${from.y} C ${from_a.x} ${from_a.y}, ${to_a.x} ${to_a.y}, ${to.x} ${to.y}" />`
}

function line(from, to, width) {
  return linep({x:from.midx, y:from.midy}, {x:to.midx, y:to.midy}, width);
}

function length({x,y}) {
  return Math.sqrt(x*x + y*y);
}
function norm({x, y}) {
  let l = Math.sqrt(x*x + y*y);
  return { x : x / l, y : y / l };
}
function perp({x, y}) {
  return [{ x:y, y:-x}, {x:-y, y:x}];
}
function scale({x, y}, s) {
  return { x: x * s, y: y * s};
}
function midpoint({ x:x1, y:y1 }, { x:x2, y:y2 }) {
  return { x: (x1 + x2) / 2, y: (y1 + y2) / 2};
}
function sub({x:x1, y:y1}, {x:x2, y:y2}) {
  return { x: x1 - x2, y: y1 - y2};
}
function add({x:x1, y:y1}, {x:x2, y:y2}) {
  return { x: x1 + x2, y: y1 + y2};
}
function cross({x:x1, y:y1}, {x:x2, y:y2}) {
  return (x1*y2) - (y1*x2);
}

function angle({x,y}) {
  return Math.atan2(x, y) ||0;
}
function swap({x, y}) {
  return {x:y, y:-x};
}

start_squeeze = 0.9;
prog_squeeze = 0.9;

function curve_from_redirect(f, t) {
  let from = {x:f.midx, y:f.midy};
  let to = {x:t.midx, y:t.midy};
  let l = length(sub(from, to)) / 2;
  let from_a = add(from, swap({x:0, y:l}));
  let to_a = to;
  return curvep(from, to, from_a, to_a);
}

function curve_to_redirect(f, t) {
  let from = { x: f.midx, y: f.midy };
  let to = { x: t.midx, y: t.midy };
  let half = midpoint(to, midpoint(to, from));
  let remaining_distance = length(sub(half, to)); 
  let offset = remaining_distance * 0.375;
  let to_a = add(to, swap({x:0, y: -offset}));
  let from_a = sub(half, scale(norm(sub(from, to)), offset));
  return curvep(from, half, from, midpoint(to, from)) +
         curvep(half, to, from_a, to_a);
}

function curve_from_redirect_to_redirect(f, t) {
  let out = "";
  {
    let from = {x:f.midx, y:f.midy};
    let to = {x:t.midx, y:t.midy};
    let l = length(sub(from, to)) / 2;
    let from_a = add(from, swap({x:0, y:l}));
    let to_a = sub(to, swap({x:0, y:l})); 
    out += curvep(from, to, from_a, to_a);
  }
  return out;
}

function sqwoosh(from, to) {
  let out = '';

  let start = {x: from.midx, y:from.midy };
  let end = {x: to.midx, y:to.midy };
  let v = norm(sub(end, start));
  let [a, b] = perp(v);
  let radius = from.width / 2.0;
  start = add(start, scale(v, radius));
  end = add(start, scale(v, 15));
  for (var dist = 0; dist < radius; dist += 1) { 
    let rat = dist / radius;
    let p = add(start, scale(a, dist));
    let fac = (rat * rat * rat);
    p = add(p, scale(v, -fac * radius));
    let v2 = norm(sub(p, end));
    p = add(p, scale(v2, 2 * (1-rat)));
    let q = add(start, scale(b, dist));
    q = add(q, scale(v, -fac * radius));
    let v3 = norm(sub(q, end));
    q = add(q, scale(v3, 2 * (1-rat)));
    out += linep(p, end);
    out += linep(q, end);
    end = add(start, scale(sub(end, start), prog_squeeze));
  }

  return out;
}

function run(node) {
  let content = node.children[1];
  let canvas = node.children[0];
  let providers = Array.from(content.querySelectorAll("[data-src-name]"));
  let arrows = "";

  for (let provider of providers) {
    let provider_box = box(provider);
    let name = provider.getAttribute('data-src-name');
    let provider_kind = provider.getAttribute('data-kind');
    let subscribers = Array.from(content.querySelectorAll(`.dest-class-${name}`));
    for (let subscriber of subscribers) {
      let subscriber_box = box(subscriber);
      let subscriber_kind = subscriber.getAttribute('data-kind');
      if (provider_kind === 'redirect') {
        if (subscriber_kind === 'redirect') {
          arrows += curve_from_redirect_to_redirect(provider_box, subscriber_box);
        }
        else {
          arrows += curve_from_redirect(provider_box, subscriber_box);
        }
      } else {
        arrows += sqwoosh(provider_box, subscriber_box);
        if (subscriber_kind === 'redirect') {
          arrows += curve_to_redirect(provider_box, subscriber_box, 2);
        } else {
          arrows += line(provider_box, subscriber_box, 2);
        }
      }
    }
  }
  canvas.innerHTML = arrows;

  let scale = 2.0;
  let tx = 0.0;
  let ty = 0.0;

  let testcase = content.parentNode.parentNode;
  testcase.style.minWidth="";
  testcase.style.minHeight="";
  let w = testcase.clientWidth;
  let h = testcase.clientHeight;

  function update() {
    node.style.transformOrigin = `${tx + w/2}px ${ty + h/2}px`;
    node.style.transform = `scale(${scale}) translate(${tx}px,${ty}px)`;
    testcase.style.minWidth=`${w * scale}px`;
    testcase.style.minHeight=`${h * scale}px`;
  }
  update();

  content.addEventListener('wheel', function(event) {
    event.preventDefault();
    scale += event.deltaY * -0.01;

    scale = Math.min(Math.max(.125, scale), 4);
    update();
  }, { capture:true, passive:false });

  function onmove(event) {
    tx += event.movementX;
    ty += event.movementY;
    let w = content.clientWidth;
    let h = content.clientHeight;
    update();
    console.log({w,h});
  }

  function ondown() {
    content.addEventListener('mousemove',onmove, );
    document.addEventListener('mouseup', function() {
      content.removeEventListener('mousemove', onmove);
    });
  }

  content.addEventListener('mousedown', ondown);
}

function go() {
  requestAnimationFrame(function () {
    let maps = Array.from(document.querySelectorAll(".map"));
    for (let map of maps) {
      run(map);
    }
  });
}

go();
