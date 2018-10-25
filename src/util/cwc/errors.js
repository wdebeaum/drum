'use strict';

const KQML = require('KQML/kqml.js');

/* Something threw a real JS Error object, or a web request or external command
 * failed unexpectedly. Something that isn't a malformed request or a search
 * failure.
 */
function programError(message) {
  return { 0: 'failure', type: 'cannot-perform', reason:
    { 0: 'program-error', message: '"' + KQML.escapeForQuotes(message) + '"' }
  };
}

/* An error/failure was thrown somwhere deeper in the program. */
function nestedError(prefix, err) {
  if ('message' in err) {
    console.warn(err.stack);
    return programError(prefix + err.message);
  } else {
    return err;
  }
}

/* Some part of the request is missing an argument we expected. */
function missingArgument(op, arg) {
  return { 0: 'failure', type: 'failed-to-interpret', reason:
    { 0: 'missing-argument',
      operator: op,
      argument: arg
    }
  };
}

/* Some part of the request has an argument we expected to be something else. */
function invalidArgument(operation, argument, expected) {
  var op = operation[0];
  var got = operation[argument];
  return { 0: 'failure', type: 'failed-to-interpret', reason:
    { 0: 'invalid-argument',
      operator: op,
      argument: argument,
      expected: '"' + KQML.escapeForQuotes(expected) + '"',
      got: got
    }
  };
}

/* Some part of the request has the wrong number of (positional) arguments. */
function invalidArgumentCount(operation, expected) {
  var op = operation[0];
  var got = operation.length-1;
  return { 0: 'failure', type: 'failed-to-interpret', reason:
    { 0: 'invalid-argument-count',
      operator: op,
      expected: '"' + KQML.escapeForQuotes(expected) + '"',
      got: got
    }
  };
}

/* The request uses arguments that might be OK on their own but don't work in
 * combination.
 */
function invalidArgumentCombo(comment) {
  return { 0: 'failure', type: 'failed-to-interpret', reason:
    { 0: 'invalid-argument-combo',
      comment: '"' + KQML.escapeForQuotes(comment) + '"'
    }
  };
}

/* Something in operator position in the request isn't an operator we
 * recognize.
 */
function unknownAction(what) {
  return { 0: 'failure', type: 'failed-to-interpret', reason:
    { 0: 'unknown-action', what: what }
  };
}

/* We can't find the thing you're talking about. */
function unknownObject(what) {
  // FIXME should this be cannot-perform or failed-to-interpret? Spaceman had the former, but ImageDisplay and Java stuff had the latter
  return { 0: 'failure', type: 'cannot-perform', reason:
    { 0: 'unknown-object', what: what }
  };
}

/* We found more than one match for the thing you're talking about. */
function ambiguous(what, resolutions) {
  return { 0: 'failure', type: 'cannot-perform',
    reason: { 0: 'ambiguous', what: what },
    possibleResolution: resolutions
  };
}

function insufficientResources(type) {
  return { 0: 'failure', type: 'cannot-perform', reason:
    { 0: 'insufficient-resources',
      type: type
    }
  };
}

module.exports = {
  programError: programError,
  nestedError: nestedError,
  missingArgument: missingArgument,
  invalidArgument: invalidArgument,
  invalidArgumentCount: invalidArgumentCount,
  invalidArgumentCombo: invalidArgumentCombo,
  unknownAction: unknownAction,
  unknownObject: unknownObject,
  ambiguous: ambiguous,
  insufficientResources: insufficientResources
};
