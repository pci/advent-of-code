const $ = document.querySelector.bind(document);

// A little helper wrapper
function State(state, ctx, score) {
  this.state = state;
  this.x = -1;
  this.y = 0;
  this.t = 0;
  this.score = score;
  this.ctx = ctx;
  this.size = 10;
  this.ended = false;
  this.input = 0;
}
State.prototype.step = function () {
  this.state = this.state.step(this.input);
  this.x = this.state.get_x();
  this.y = this.state.get_y();
  this.t = this.state.get_t();
  this.ended = this.state.is_ended();
};
State.prototype.stepGame = function () {
  // draw until we draw the ball:
  for (let i = 0; i < 2500; i++) {
    this.step();
    if (this.ended) {
      return;
    }
    if (this.x < 0) {
      // score
      this.score.innerText = this.t;
      continue;
    }
    const x = this.x * this.size;
    const y = this.y * this.size;
    const w = this.size;
    switch (this.t) {
      // empty
      case 0:
        this.ctx.clearRect(x, y, w, w);
        break;
      // wall
      case 1:
        this.ctx.fillStyle = "black";
        this.ctx.fillRect(x, y, w, w);
        break;
      // block
      case 2:
        this.ctx.fillStyle = "green";
        this.ctx.fillRect(x, y, w, w);
        break;
      // paddle
      case 3:
        this.ctx.fillStyle = "red";
        this.ctx.fillRect(x, y + w / 4, w, w / 2);
        break;
      // ball
      case 4:
        this.ctx.fillStyle = "blue";
        this.ctx.beginPath();
        this.ctx.ellipse(x + w / 2, y + w / 2, w / 2, w / 2, 0, 0, 2 * Math.PI);
        this.ctx.fill();
        // we return on a ball draw:
        return;
    }
  }
};
State.prototype.runGame = function () {
  const that = this;

  this.keydown = (e) => {
    switch (e.keyCode) {
      case 37:
        that.input -= 1;
        break;
      case 39:
        that.input += 1;
        break;
    }
  };
  this.keyup = (e) => {
    switch (e.keyCode) {
      case 37:
        that.input += 1;
        break;
      case 39:
        that.input -= 1;
        break;
    }
  };
  document.addEventListener("keydown", that.keydown);
  document.addEventListener("keyup", that.keyup);

  return new Promise((res, rej) => {
    // setup:
    that.stepGame();
    that.interval = setInterval(() => {
      if (that.ended) {
        clearInterval(that.interval);
        document.removeEventListener("keydown", that.keydown);
        document.removeEventListener("keyup", that.keyup);
        res();
        return;
      }
      that.stepGame.call(that);
    }, 250);
  });
};

(async () => {
  const score = $("#score");
  const canvas = $("#canvas");
  const ctx = canvas.getContext("2d");
  const start = $("#start");
  const rust = await import("../pkg/index.js").catch(console.error);

  // enable start button
  start.removeAttribute("disabled");

  let pong;
  start.addEventListener("click", () => {
    start.setAttribute("disabled", "true");
    pong = new State(rust.State.new_pong(), ctx, score);
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    pong
      .runGame()
      // let people play again:
      .then(() => start.removeAttribute("disabled"))
      .catch(console.log.bind(console));
  });
})();
