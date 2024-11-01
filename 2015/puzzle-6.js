const fs = require('node:fs/promises');

let arr = []

function count_1() {
  return arr.filter(x => x).length;
}

function count_2() {
  return arr.reduce((x, y) => x + y);
}

async function read_file() {
  const data = await fs.readFile("6-input", {encoding: 'utf8'});
  return data;
}

function parse_line(line) {
  let [type, ...rest] = line.split(" ")
  let obj = {type: "", bottom: [], top: []};
  if (type === "toggle") {
    obj.type = "toggle";
  } else {
    obj.type = rest[0];
    rest = rest.slice(1);
  }
  obj.bottom = rest[0].split(",").map(x => parseInt(x));
  obj.top = rest[2].split(",").map(x => parseInt(x));
  return obj
}

function execute_inst_1(inst) {
  for (let i = inst.bottom[0]; i <= inst.top[0]; ++i) {
    for (let j = inst.bottom[1]; j <= inst.top[1]; ++j) {
      if (inst.type === "on") {
        arr[(i * 1000) + j] = true;
      } else if (inst.type === "off") {
        arr[(i * 1000) + j] = false;
      } else {
        arr[(i * 1000) + j] = !arr[(i * 1000) + j];
      }
    }
  }
}

function execute_inst_2(inst) {
  for (let i = inst.bottom[0]; i <= inst.top[0]; ++i) {
    for (let j = inst.bottom[1]; j <= inst.top[1]; ++j) {
      if (inst.type === "on") {
        arr[(i * 1000) + j] += 1;
      } else if (inst.type === "off") {
        arr[(i * 1000) + j] = Math.max(0, arr[(i * 1000) + j] - 1);
      } else {
        arr[(i * 1000) + j] += 2;
      }
    }
  }
}

(async () => {
  let lines = (await read_file()).split(/\n/);
  const insts = lines.map(parse_line);

  for (let i = 0; i < 1000000; ++i) {
    arr.push(false);
  }
  for (let i = 0; i < insts.length; ++i) {
    execute_inst_1(insts[i]);
  }
  console.log("Round 1:", count_1());

  for (let i = 0; i < 1000000; ++i) {
    arr[i] = 0;
  }
  for (let i = 0; i < insts.length; ++i) {
    execute_inst_2(insts[i]);
  }
  console.log("Round 2:", count_2());
})()

// Local Variables:
// compile-command: "node puzzle-6.js"
// End:
