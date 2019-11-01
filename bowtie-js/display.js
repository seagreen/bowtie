"use strict";

let topLevelResult = _result;

function main() {
  const canvas = document.getElementById("canvas");
  const ctx = canvas.getContext("2d");
  drawPicture(0, 0, ctx, topLevelResult[1]);

  window.onkeydown = event => {
    nextInput(canvas, ctx, ["KeyDown", event.key]);
  };

  window.onkeyup = event => {
    nextInput(canvas, ctx, ["KeyUp", event.key]);
  };

  window.setInterval(unit => nextInput(canvas, ctx, ["Tick"]), 1000 / 30);
}

function nextInput(canvas, ctx, input) {
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  topLevelResult = topLevelResult[2](input);
  drawPicture(0, 0, ctx, topLevelResult[1]);
}

function drawPicture(xCursor, yCursor, ctx, pic) {
  switch (pic[0]) {
    case "Line": {
      const p1 = pic[1];
      const p2 = pic[2];

      const startX = xCursor + p1[1];
      const startY = yCursor + p1[2];
      const endX = xCursor + p2[1];
      const endY = yCursor + p2[2];

      ctx.beginPath();
      ctx.moveTo(startX, startY);
      ctx.lineTo(endX, endY);
      ctx.closePath();
      ctx.stroke();
      break;
    }
    case "Text": {
      ctx.font = "30px serif";
      ctx.fillText(unicodeToString(pic[1]), xCursor, yCursor);
      break;
    }
    case "Translate": {
      drawPicture(xCursor + pic[1], yCursor + pic[2], ctx, pic[3]);
      break;
    }
    case "Pictures": {
      const pics = listToArray(pic[1]);
      for (let p of pics) {
        drawPicture(xCursor, yCursor, ctx, p);
      }
      break;
    }
    default: {
      throw new Error("Unknown picture type: " + pic.type);
    }
  }
}

function listToArray(xs) {
  switch (xs[0]) {
    case "Cons": {
      var shallowCopy = listToArray(xs[2]).slice();
      shallowCopy.unshift(xs[1]);
      return shallowCopy;
    }
    case "Nil": {
      return [];
    }
    default: {
      throw new Error("Unknown listToArray input: " + xs[0]);
    }
  }
}

function unicodeToString(u) {
  return arrayToString(listToArray(u[1]));
}

function arrayToString(xs) {
  return String.fromCodePoint(...xs);
}
