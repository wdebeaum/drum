const path = require('path');
const child_process = require('child_process');
const ndarray = require('ndarray');
const KQML = require('KQML/kqml.js');
const errors = require('util/cwc/errors.js');
const nestedError = errors.nestedError;

/* Canvas of 8-bit-per-channel RGB values to scribble on before sending to
 * ImageMagick to convert to file in a real image format.
 * new RGBBytes(width, height) will create an uninitialized canvas.
 * new RGBBytes(width, height, { r: #, g: #, b: # }) will create a canvas
 * initialized to the given rgb color.
 * new RGBBytes(width, height, buffer) will create a canvas that uses the given
 * buffer (which must have length width*height*3).
 */
function RGBBytes(width, height, colorOrBuffer) {
  this.width = width;
  this.height = height;
  var numBytes = width * height * 3;
  if (colorOrBuffer instanceof Buffer) {
    if (colorOrBuffer.length != numBytes) {
      throw new Error("expected buffer of length " + numBytes + " bytes, but got one of length " + colorOrBuffer.length + " bytes");
    }
    this.data = colorOrBuffer;
  } else {
    this.data = Buffer.allocUnsafe(numBytes);
  }
  this.array = ndarray(this.data, [height, width, 3]);
  if (('object' == typeof colorOrBuffer) &&
      (!(colorOrBuffer instanceof Buffer)) &&
      ('r' in colorOrBuffer) &&
      ('g' in colorOrBuffer) &&
      ('b' in colorOrBuffer)) {
    this.fillRect(0,0,width,height,colorOrBuffer);
  }
}

[ // begin RGBBytes methods

  function get(x,y) {
    return {
      r: this.array.get(y, x, 0),
      g: this.array.get(y, x, 1),
      b: this.array.get(y, x, 2)
    };
  },

  function set(x,y,c) {
    this.array.set(y, x, 0, c.r);
    this.array.set(y, x, 1, c.g);
    this.array.set(y, x, 2, c.b);
  },

  function fillRect(x1,y1,x2,y2,c) {
    for (var y = y1; y < y2; y++) {
      for (var x = x1; x < x2; x++) {
	this.set(x,y,c);
      }
    }
  },

  function write(s, cb) {
    s.write(this.data, cb);
  },

  function end(s, cb) {
    s.end(this.data, cb);
  },

  function writeFile(filename, cb) {
    var formatName = path.extname(filename);
    var convert = child_process.spawn(
      'convert',
      [ '-size', `${this.width}x${this.height}`,
        '-depth', '8',
	'rgb:-', // read RGB data from stdin
	filename
      ], {
	stdio: ['pipe', 'ignore', 'inherit']
      }
    );
    convert.on('error', err => cb(nestedError('while spawning convert in order to write RGBBytes: ', err)));
    convert.stdin.on('error', err => cb(nestedError('while writing to convert in order to write RGBBytes: ', err)));
    convert.on('exit', (code, sig) => {
      if (code === null) {
	cb(nestedError(`while writing RGBBytes: convert terminated by signal ${sig}`));
      } else if (code == 0) { // success
	cb({ 0: 'file',
	  name: `"${KQML.escapeForQuotes(filename)}"`,
	  format: ['raster', `"${KQML.escapeForQuotes(formatName)}"`, this.width, this.height]
	});
      } else {
	cb(nestedError(`while writing RGBBytes: convert exited with nonzero code ${code}`));
      }
    });
    this.end(convert.stdin); // note: no callback, because it's handled on exit
  }

].map(fn=>{ RGBBytes.prototype[fn.name] = fn; });

/* Read the width and height of an image file, and pass them to cb. */
RGBBytes.readFileDims = function(filename, cb) {
  child_process.execFile('identify', [ '-format', '[%w,%h]', filename ],
			 (err, stdout, stderr) => {
    if (err) {
      cb(nestedError('while reading image dimensions: ', err));
      return;
    }
    var dims;
    try {
      dims = JSON.parse(stdout);
    } catch (err) {
      cb(nestedError('while parsing image dimensions: ', err));
      return;
    }
    cb(...dims);
  });
}

/* Read an image file into an RGBBytes object, given its dimensions, and pass
 * the object to cb.
 */
RGBBytes.readFileWithDims = function(filename, width, height, cb) {
  child_process.execFile('convert',
    // write 8-bit-per-channel RGB data to stdout
    [ filename, '-depth', '8', 'rgb:-' ],
    { encoding: 'buffer', // get bytes, not UTF-8 strings
      maxBuffer: width * height * 3 // avoid "maxBuffer length exceeded" errors
    },
    (err, stdout, stderr) => {
      if (err) {
	cb(nestedError('while reading image: ', err));
	return;
      }
      var rgb;
      try {
	rgb = new RGBBytes(width, height, stdout);
      } catch (err) {
	cb(nestedError('while creating RGBBytes canvas from image data: ', err));
      }
      cb(rgb);
    }
  );
}

/* Read an image file into an RGBBytes object and pass the object to cb. */
RGBBytes.readFile = function(filename, cb) {
  RGBBytes.readFileDims(filename, (width, height) => {
    RGBBytes.readFileWithDims(filename, width, height, cb);
  });
}

module.exports = RGBBytes;
