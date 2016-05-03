// Gecko/WebKit specific (no IE)
var processor = new XSLTProcessor();
var xslrequest = new XMLHttpRequest();
xslrequest.open("GET", "wn-xml-to-xhtml.xsl", false);
xslrequest.send(null);
processor.importStylesheet(xslrequest.responseXML);

function togglePointers(link) {
  var pointers = link.parentNode.lastChild;
  if (pointers.style.display == '') {
    pointers.style.display = 'block';
  } else {
    pointers.style.display = '';
  }
}

function fillInSynsets(synsets) {
  for (var i = 0; i < synsets.length; i++) {
    if (synsets[i].nodeType == Node.ELEMENT_NODE &&
        synsets[i].childNodes.length == 0) {
      var id = synsets[i].id;
      var ss_type = id.substr(0,1);
      var synset_offset = id.substr(1,id.length-3);
      var lang = id.substr(id.length-2, 2);
      var xhr = new XMLHttpRequest();
      xhr.open("GET", "get-synset-xml.pl?ss_type=" + ss_type + "&synset_offset=" + synset_offset + "&lang=" + lang, false);
      xhr.send(null);
      // Gecko/WebKit specific (no IE)
      var newLI = processor.transformToFragment(xhr.responseXML, document);
      synsets[i].parentNode.replaceChild(newLI, synsets[i]);
    }
  }
}

function toggleSynsets(link) {
  var synsets = link.parentNode.lastChild;
  if (synsets.style.display == '') {
    synsets.style.display = 'block';
  } else {
    synsets.style.display = '';
  }
  fillInSynsets(synsets.childNodes);
}

function fillInEnglishSenses() {
  var englishSenses = document.getElementsByClassName("english_senses");
  for (var i = 0; i < englishSenses.length; i++) {
    var pointers = englishSenses[i].parentNode.lastChild.childNodes;
    for (var j = 0; j < pointers.length; j++) {
      if (pointers[j].firstChild.innerHTML == "English") {
	fillInSynsets(pointers[j].lastChild.childNodes);
	// NOTE: this is different from childNodes originally passed to
	// fillInSynsets, since that function replaces each one
	var synsets = pointers[j].lastChild.childNodes;
	englishSenses[i].innerHTML = '['
	var nonFirst = false;
	for (var k = 0; k < synsets.length; k++) {
	  if (synsets[k].nodeType == Node.ELEMENT_NODE) {
	    if (nonFirst) {
	      englishSenses[i].innerHTML += '; ';
	    } else {
	      nonFirst = true;
	    }
	    englishSenses[i].innerHTML +=
	      synsets[k].childNodes[4].innerHTML.replace(/ id=".*?"/g,'');
	  }
	}
	englishSenses[i].innerHTML += ']'
	break;
      }
    }
  }
}

var skss = document.styleSheets[1];
var senseKeyStyle = skss.cssRules[0].style;
var lemmaStyle = skss.cssRules[1].style;
function setSenseKeyVisibility() {
  var cb = document.getElementById("show_sense_keys");
  if (cb.checked) {
    senseKeyStyle.display = '';
    lemmaStyle.display = 'none';
  } else {
    senseKeyStyle.display = 'none';
    lemmaStyle.display = '';
  }
  // TODO change links so they preserve this setting?
}

function bodyOnLoad() {
  fillInEnglishSenses();
  setSenseKeyVisibility();
}

