'use strict';

const util = require('util');

const KQML = require('KQML/kqml.js');
const TripsModule = require('TripsModule/trips-module.js');

function CWCModule(argv) {
  TripsModule.call(this, argv);
}
util.inherits(CWCModule, TripsModule);

[ // begin CWCModule methods

  function declareCapabilities() {
    // to be overridden by modules with capabilities
  },

  function declareCapabilitiesOnce() {
    if (this.alreadyDeclaredCapabilities) return;
    this.alreadyDeclaredCapabilities = true;
    this.declareCapabilities();
  },

  function addHandlers() {
    this.addHandler(KQML.parse('(tell &key :content (i-am-here &key :who cwmsagent))'),
		    this.declareCapabilitiesOnce);
    this.addHandler(KQML.parse('(request &key :content (restart))'),
		    this.restart);
  },

  function restart() {
    this.alreadyDeclaredCapabilities = false;
    this.sendMsg(KQML.parse('(request :content (are-you-there :who cwmsagent))'));
  },

  function init(oninit) {
    TripsModule.prototype.init.call(this, () => {
      this.addHandlers();
      this.restart();
      if (oninit !== undefined) {
	oninit();
      }
    });
  }

].map(fn=>{ CWCModule.prototype[fn.name] = fn; });

module.exports = CWCModule;
