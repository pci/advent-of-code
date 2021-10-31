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
  this.currentInput = 0;
  this.input = 0;
  this.cheat = true;
  this.hardMode = true;
  this.paddlePosition = { x: 0, y: 0 };
  this.ballPosition = { x: 0, y: 0 };
  this.lastBallPosition = { x: -1, y: -1 };
}
State.prototype.step = function () {
  this.state.step(this.input);
  this.x = this.state.get_x();
  this.y = this.state.get_y();
  this.t = this.state.get_t();
  this.ended = this.state.is_ended();
};
State.prototype.debugRun = function () {
  // run until the game ends:
  while (true) {
    this.step();
    if (this.ended) {
      return;
    }
  }
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
      if (this.t !== 0) {
        // leave the final score up
        this.score.innerText = this.t;
      }
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
        this.paddlePosition = { x: this.x, y: this.y };
        break;
      // ball
      case 4:
        this.ctx.fillStyle = "blue";
        this.ctx.beginPath();
        this.ctx.ellipse(x + w / 2, y + w / 2, w / 2, w / 2, 0, 0, 2 * Math.PI);
        this.ctx.fill();
        if (this.lastBallPosition.y == -1) {
          // special bootstrap case
          this.lastBallPosition = { x: this.x - 1, y: this.y - 1 };
        } else {
          this.lastBallPosition = this.ballPosition;
        }
        this.ballPosition = { x: this.x, y: this.y };
        if (!this.cheat) {
          this.input = this.currentInput;
        } else {
          // super-advanced-AI™️
          if (this.ballPosition.y < this.lastBallPosition.y) {
            // going up, play chase
            if (this.ballPosition.x > this.paddlePosition.x) {
              this.input = 1;
            } else if (this.ballPosition.x < this.paddlePosition.x) {
              this.input = -1;
            } else {
              this.input = 0;
            }
          } else {
            // ball coming down, where is it going to land?
            const dy = this.paddlePosition.y - 1 - this.ballPosition.y;
            const dx =
              this.ballPosition.x +
              (this.ballPosition.x - this.lastBallPosition.x) * dy -
              this.paddlePosition.x;
            if (dx > 0) {
              this.input = 1;
            } else if (dx < 0) {
              this.input = -1;
            } else {
              this.input = 0;
            }
          }
        }
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
        that.currentInput = -1;
        that.input = -1;
        break;
      case 39:
        that.currentInput = 1;
        that.input = 1;
        break;
    }
  };
  this.keyup = (e) => {
    switch (e.keyCode) {
      case 37:
        that.currentInput = 0;
        break;
      case 39:
        that.currentInput = 0;
        break;
    }
  };
  document.addEventListener("keydown", that.keydown);
  document.addEventListener("keyup", that.keyup);

  return new Promise((res, rej) => {
    // setup:
    that.stepGame();
    that.interval = setInterval(
      () => {
        if (that.ended) {
          clearInterval(that.interval);
          document.removeEventListener("keydown", that.keydown);
          document.removeEventListener("keyup", that.keyup);
          res();
          return;
        }
        that.stepGame.call(that);
      },
      this.hardMode ? 20 : 250
    );
  });
};

(async () => {
  const score = $("#score");
  const canvas = $("#canvas");
  const cheatInput = $("#cheat");
  const hardModeInput = $("#hardmode");
  const ctx = canvas.getContext("2d");
  const start = $("#start");
  const debug = $("#debug");
  const rust = await import("../pkg/index.js").catch(console.error);

  // enable start button
  start.removeAttribute("disabled");

  let pong;
  start.addEventListener("click", () => {
    start.setAttribute("disabled", "true");
    pong = new State(rust.MutState.new_pong(), ctx, score);
    pong.cheat = cheatInput.checked;
    pong.hardMode = hardModeInput.checked;
    score.innerText = "0";

    ctx.clearRect(0, 0, canvas.width, canvas.height);
    pong
      .runGame()
      // let people play again:
      .then(() => start.removeAttribute("disabled"))
      .catch(console.log.bind(console));
  });

  if (debug)
    debug.addEventListener("click", () => {
      debug.setAttribute("disabled", "true");
      let initializationTime = 0;
      let runTime = 0;
      const globalStart = Date.now();
      const N = 10;
      let i = 0;
      const oneRun = () => {
        const start = performance.now();
        const pong = new State(rust.MutState.new_pong(), ctx, score);
        const initTime = performance.now() - start;
        pong.debugRun();
        runTime += performance.now() - initTime - start;
        initializationTime += initTime;
        score.innerText = `Completed ${i + 1}/10 in ${
          Date.now() - globalStart
        }ms total`;
        if (++i == N) {
          score.innerText = `${(10 * runTime) / N}ms`;
          debug.removeAttribute("disabled");
        } else {
          // give it a breather:
          setTimeout(oneRun, 100);
        }
      };
      oneRun();
    });
})();
