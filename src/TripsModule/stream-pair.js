module.exports = (function() {

// Because I can't figure out how to make stream.Duplex work, I'll implement
// just enough by hand that the KQMLStream wrapper will work.
function StreamPair(input, output) {
  this.input = input;
  this.output = output;
}
StreamPair.prototype = {
  constructor: StreamPair,
  setEncoding: function(encoding) {
    return this.input.setEncoding(encoding);
  },
  on: function(name, fn) {
    return this.input.on(name, fn);
  },
  write: function(data) {
    return this.output.write(data);
  }
}

return StreamPair;
})();
