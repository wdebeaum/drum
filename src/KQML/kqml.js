const fs = require('fs');
const util = require('util');
const EventEmitter = require('events');
const PEG = require('pegjs');

// PEG.js 0.9.0->0.10.0 compatibility
if (!('buildParser' in PEG)) {
  PEG.buildParser = PEG.generate;
}

const kqmlParser = PEG.buildParser(fs.readFileSync(
  require.resolve('./kqml_parser.pegjs'),
  { encoding: 'UTF-8' }
));

function parse(string) {
  return kqmlParser.parse(string);
}

/** Is o a string containing a quoted (KQML) string? */
function isKQMLString(o) {
  return (typeof o == 'string' && /^"/.test(o));
}

/** Convert a quoted KQML string to a regular JavaScript string. */
function kqmlStringAsJS(kqml) {
  return kqml.
    replace(/^"|"$/g,'').
    replace(
      /\\(.)/g,
      function(m0, m1) {
	if (m1 == 'n') {
	  return "\n";
	} else if (m1 == 'r') {
	  return "\r";
	} else if (m1 == 't') {
	  return "\t";
	} else {
	  return m1;
	}
      }
    );
}

function escapeForQuotes(string) {
  return string.
    replace(/[\\]/g, "\\\\").
    replace(/[\n]/g, "\\n").
    replace(/[\r]/g, "\\r").
    replace(/[\t]/g, "\\t").
    replace(/"/g, "\\\"");
}

/** Is o a string containing a KQML keyword symbol? */
function isKeyword(o) {
  return (typeof o == 'string' && /^:/.test(o));
}

/** Turn a (keyword) symbol into something JavaScript likes as a property name.
 * This is based on WebTracker's function of the same name, but in addition
 * strips the leading ':' of the keyword, and downcases the whole thing at
 * first.
 */
function camelize(string) {
  return string.
    toLowerCase().
    replace(/^:/,'').
    replace(/::/g, '$').
    replace(
      /-([a-z])/g,
      function(m0, m1) {
	return m1.toUpperCase();
      }
    );
}

/** Turn a JavaScript property name into a keyword symbol. */
function decamelize(string) {
  return ':' +
    string.
    replace(/\$/g, '::').
    replace(/[A-Z]/g, "-$&").
    toLowerCase();
}

/** Turn a KQML array as returned by kqmlParser.parse into a JavaScript object,
 * with keyword arguments in their own (camelized) properties, and positional
 * arguments still accessible by their index.
 */
function keywordify(a) {
  var ret = {};
  var i;
  for (i = a.length - 2; i >= 0 && isKeyword(a[i]); i -= 2) {
    ret[camelize(a[i])] = a[i+1];
  }
  i++;
  for (; i >= 0; i--) {
    ret[i] = a[i];
  }
  return ret;
}

/** Undo keywordifiy */
function unkeywordify(o) {
  var ret = [];
  for (var i = 0; i in o; i++) {
    ret.push(o[i]);
  }
  for (var k in o) {
    if (!/^\d+$/.test(k)) {
      ret.push(decamelize(k), o[k]);
    }
  }
  return ret;
}

/** Get the positional arguments of a keywordified KQML expression (or an
 * arbitrary Object) as a real Array.
 */
function positionalArgs(kqml) {
  var ret = [];
  for (var i = 0; i in kqml; i++) {
    ret.push(kqml[i]);
  }
  return ret;
}

/** Get the keyword arguments of a keywordified KQML expression (or an
 * arbitrary object) as a new Object.
 */
function keywordArgs(kqml) {
  var ret = {};
  for (var k in kqml) {
    if (!/^\d+$/.test(k)) {
      ret[k] = kqml[k];
    }
  }
  return ret;
}

/** Does the subscription pattern match the message? If so, return an array of
 * what each * matched. If useNamedWildcards is true, return an object mapping
 * named wildcards (of the form '$name') to their matched values, instead of an
 * array. The $ will be removed in the keys of the returned object. Unnamed
 * wildcards ('*') will still match, but their values won't be captured.
 */
function patternMatches(pattern, message, useNamedWildcards) {
  var pt = typeof pattern;
  var mt = typeof message;
  if (pt == 'string' && pattern == '*') { // match anything
    if (useNamedWildcards) {
      return {};
    } else {
      return [message];
    }
  } else if (useNamedWildcards && pt == 'string' && /^\$\w+$/.test(pattern)) {
    var match = {};
    match[pattern.substr(1)] = message;
    return match;
  } else if (pt == 'string' && mt == 'string' && !isKQMLString(pattern)) {
    // symbol, downcase both sides before comparing
    return ((pattern.toLowerCase() == message.toLowerCase()) ? [] : false);
  } else if (pt != mt) {
    return false;
  } else if (pt == 'object' &&
             // just a plain Object or Array, and not e.g. a ChildProcess
             (Array.isArray(pattern) || pattern.constructor === Object)) {
    // keywordify both sides if necessary
    if (Array.isArray(pattern)) {
      var patternArray = pattern;
      pattern = keywordify(pattern);
    }
    if (Array.isArray(message)) {
      message = keywordify(message);
    }
    var i;
    var hasRest = false;
    var hasKeys = false;
    var match = (useNamedWildcards ? {} : []);
    for (i = 0; i in pattern; i++) { // iterate over positional args
      var submatch = undefined;
      if (pattern[i] == '.' && (i+1) in pattern && pattern[i+1] == '*') {
	hasRest = true;
	break;
      } else if (useNamedWildcards &&
                 pattern[i] == '.' && (i+1) in pattern && 
                 /^\$\w+$/.test(pattern[i+1])) {
	hasRest = pattern[i+1].substr(1);
	break;
      } else if (pattern[i] == '&key') {
	hasKeys = true;
	break;
      } else if (!(i in message &&
      		   (submatch = patternMatches(pattern[i], message[i],
		   			      useNamedWildcards)))) {
	return false;
      } else if (submatch) {
	if (useNamedWildcards) {
	  Object.assign(match, submatch);
	} else {
	  match = match.concat(submatch);
	}
      }
    }
    if (i in message && !hasRest) { // extra positional args in message
      return false;
    }
    if (hasRest) {
      // copy the rest of message into restMatch, and add that to match
      var restMatch = {};
      for (var j = 0; i in message; i++, j++) {
	restMatch[j] = message[i];
      }
      for (var k in message) {
	if (/^\d+$/.test(k)) {
	  continue;
	}
	restMatch[k] = message[k];
      }
      if (useNamedWildcards) {
	if (typeof hasRest == 'string') {
	  match[hasRest] = restMatch;
	}
      } else {
	match.push(restMatch);
      }
    }
    if (hasKeys) {
      var keys = [];
      if (patternArray) {
	for (i = patternArray.length - 1; i >= 0; i--) {
	  if (patternArray[i] == '&key') {
	    break;
	  } else {
	    i--;
	  }
	  // Note: assumes patternArray[i] is a keyword string
	  keys.push(camelize(patternArray[i]));
	}
      } else {
	for (var k in pattern) { // iterate over keyword args
	  if (!/^\d+$/.test(k)) {
	    keys.push(k);
	  }
	}
	keys.reverse();
      }
      for (i = keys.length - 1; i >= 0; i--) {
	var k = keys[i];
	var submatch = undefined;
	if (!(k in message &&
	      (submatch = patternMatches(pattern[k], message[k], useNamedWildcards)))) {
	  return false;
	} else if (submatch) {
	  if (useNamedWildcards) {
	    Object.assign(match, submatch);
	  } else {
	    match = match.concat(submatch);
	  }
	}
      }
    }
    return match;
  } else { // not an object or '*', just use ==
    return ((pattern == message) ? [] : false);
  }
}

/** Like patternMatches, but without wildcards or &key. Return true or false.
 * '.', '*', '&key', and '$foo' are treated as plain symbols. Symbols are still
 * case-insensitive, and keyword arguments still match in any order, but the
 * same set of keywords must be present on both sides.
 * Note that this is different from node.js' assert.deepEqual in that it
 * follows KQML conventions rather than JS ones, and it returns false instead
 * of throwing an exception.
 */
function deepEquals(a, b) {
  var at = typeof a;
  var bt = typeof b;
  if (at == 'string' && bt == 'string' && !isKQMLString(a)) {
    // symbol, downcase both sides before comparing
    return (a.toLowerCase() == b.toLowerCase());
  } else if (at != bt) {
    return false;
  } else if (at == 'object' &&
             // just a plain Object or Array, and not e.g. a ChildProcess
             (Array.isArray(a) || a.constructor === Object)) {
    // keywordify both sides if necessary
    if (Array.isArray(a)) {
      var aArray = a;
      a = keywordify(a);
    }
    if (Array.isArray(b)) {
      b = keywordify(b);
    }
    var k;
    for (k in a) {
      if (!(k in b && deepEquals(a[k], b[k]))) {
	return false;
      }
    }
    for (k in b) {
      if (!(k in a)) {
	return false;
      }
    }
    return true;
  } else { // none of the above, just use ==
    return (a == b);
  }
}

/** Turn a JavaScript object, like those returned by kqmlKeywordify, back into
 * a KQML-formatted string. Note that in order to get quoted strings, you must
 * use escapeForQuotes and add the actual quotes yourself, before calling this
 * function. Otherwise strings turn into symbols. This function does, however,
 * convert true to t, and false, undefined, null, [], and {} to nil. An object
 * with a toKQML() method will cause this function to call that method.
 */
function toKQML(o) {
  switch (typeof o) {
    case 'undefined':
      return 'nil';
    case 'boolean':
      return (o ? 't' : 'nil');
    case 'string':
      return o;
    case 'number':
      return o.toString();
    case 'object':
      if (o === null) {
	return 'nil';
      } else if ('toKQML' in o && typeof o.toKQML == 'function') {
	return o.toKQML();
      } else {
	var positional = [];
	var keywords = '';
	if (Array.isArray(o)) {
	  positional = o.map(toKQML);
	} else {
	  for (var p in o) {
	    var val = toKQML(o[p]);
	    if (/^\d+$/.test(p)) {
	      positional[parseInt(p)] = val;
	    } else {
	      keywords += ' ' + decamelize(p) + ' ' + val;
	    }
	  }
	}
	if (positional.length == 0 && keywords.length == 0) {
	  return 'nil'; // () isn't technically allowed in KQML
	} else {
	  return '(' + positional.join(' ') + keywords + ')';
	}
      }
    case 'function':
      if (o.name) {
	return "\"JS function " + o.name + "\"";
      } else {
	return "\"anonymous JS function\"";
      }
    default:
      throw "Unknown typeof value: " + (typeof o);
  }
}

function KQMLStream(stream) {
  EventEmitter.call(this);
  this.stream = stream;
  stream.setEncoding('UTF-8');
  this.dataSoFar = '';
  stream.on('close', (this.closeListener = this.emit.bind(this, 'close')));
  stream.on('data', (this.dataListener = this.onWrappedData.bind(this)));
}
util.inherits(KQMLStream, EventEmitter);

KQMLStream.prototype.onWrappedData = function(string) {
  this.dataSoFar += string;
  var errorOffset = 0;
  for(;;) {
    // get rid of what we parsed/tried to parse already
    this.dataSoFar = this.dataSoFar.slice(errorOffset);
    try { // try to parse everything we have so far
      var data = parse(this.dataSoFar);
      this.safeOnData(data);
      this.dataSoFar = '';
      break;
    } catch (e1) { // failing that...
      if (!e1.location) { throw e1; }
      errorOffset = e1.location.start.offset;
      //console.log('errorOffset=' + errorOffset);
      if (errorOffset >= this.dataSoFar.length) { // if at the end of dataSoFar
	break; // just end the loop and hope for more data next time
      }
      if (errorOffset == 0) {
	console.error("error at beginning of KQML input?!");
	break;
      }
      // otherwise try to parse up to the point the parse failed
      var prefix = this.dataSoFar.slice(0,errorOffset);
      try {
	var data = parse(prefix);
	this.safeOnData(data);
      } catch (e2) { // failing that, just log it and discard the failing bit
	console.error(e2);
      }
    }
  }
}

KQMLStream.prototype.write = function(data, cb) {
  this.stream.write(toKQML(data) + "\n", cb);
}

// like ondata, but handle errors and missing handler
KQMLStream.prototype.safeOnData = function(data) {
  if (this.listenerCount('data') > 0) {
    try {
      this.emit('data', data);
    } catch (e) {
      this.emit('error', new Error('error handling message ' + toKQML(data) + "\n" + e.stack));
    }
  } else {
    console.log(this);
    console.log(this.listenerCount('data'));
    console.error('message unhandled: ' + toKQML(data));
  }
}

KQMLStream.prototype.end = function() { this.stream.end(); }

KQMLStream.prototype.unwrap = function() {
  this.stream.removeListener('close', this.closeListener);
  this.stream.removeListener('data', this.dataListener);
  return this.stream;
}

/** Register a function to be called when a KQML expression is read from the
 * stream.
 */
/*function read(stream, ondata) {
  stream.on('data', function(data) {
    ondata(parse(data));
  });
}*/

/** Write an object (or a string) to a KQML stream. */
/*function write(stream, data) {
  stream.write(toKQML(data) + "\n");
}*/

function wrapStream(stream) {
  return new KQMLStream(stream);
}

module.exports = {
  parse: parse,
  isKQMLString: isKQMLString,
  kqmlStringAsJS: kqmlStringAsJS,
  escapeForQuotes: escapeForQuotes,
  isKeyword: isKeyword,
  camelize: camelize,
  decamelize: decamelize,
  keywordify: keywordify,
  unkeywordify: unkeywordify,
  positionalArgs: positionalArgs,
  keywordArgs: keywordArgs,
  patternMatches: patternMatches,
  deepEquals: deepEquals,
  toKQML: toKQML,
  KQMLStream: KQMLStream,
  wrapStream: wrapStream
};

