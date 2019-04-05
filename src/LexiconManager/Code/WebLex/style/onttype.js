// get the stylesheet for ONT types
var processor = null
if (window.XSLTProcessor) {
  processor = new XSLTProcessor()
  var xslrequest = new XMLHttpRequest()
  xslrequest.open("GET", "../style/onttype.xsl", false)
  xslrequest.send(null)
  processor.importStylesheet(xslrequest.responseXML)
  processor.setParameter(null, "mode", "tree")
} else {
  alert("Your browser doesn't support XSLTProcessor (are you still using Internet Explorer?!)")
  throw "Unsupported browser"
}

// this loads the list item for onttype
function loadONTLI(onttype) {
  var xmlrequest = new XMLHttpRequest()
  xmlrequest.open("GET", "../data/ONT%3A%3A" + onttype + ".xml", false)
  xmlrequest.send(null)
  var oldONTLI = document.getElementById(onttype)
  var newONTLI = null
  if (window.XSLTProcessor) {
    newONTLI = processor.transformToFragment(xmlrequest.responseXML, document)
    oldONTLI.parentNode.replaceChild(newONTLI, oldONTLI)
  } else {
    throw "Unsupported browser"
  }
  setTargets()
}

// change whether a specified component is displayed
function genericToggle(toggleType, id) {
  var span = document.getElementById(id + "-" + toggleType)
  var link = document.getElementById(id + "-" + toggleType + "-link")
  if (span.style.display == "none") {
    link.innerHTML = link.innerHTML.replace("show", "hide")
    span.style.display = ''
  } else {
    link.innerHTML = link.innerHTML.replace("hide", "show")
    span.style.display = "none"
  }
}

function toggleWords(onttype) {
  genericToggle('words', onttype)
}

function toggleRoles(onttype) {
  genericToggle('roles', onttype)
}

function toggleDefinitions(onttype) {
  genericToggle('definitions', onttype)
}

function toggleComment(onttype) {
  genericToggle('comment', onttype)
}

function toggleFeatures(id) {
  genericToggle('sem', id)
}

// show the children of the given onttype, loading them if necessary
function showChildren(onttype) {
  var children = document.getElementById(onttype + "-children")
  if (children) {
    var link = document.getElementById(onttype + "-children-link")
    if (children.childNodes.length > 0)
    { // if we have children
      if (children.childNodes[0].childNodes.length == 0)
      { // if we haven't loaded the children yet, load them
	for (i in children.childNodes)
	{
	  if (children.childNodes[i].tagName == "LI")
	  {
	    loadONTLI(children.childNodes[i].getAttribute('id'))
	  }
	}
      }
      link.innerHTML = "V&nbsp;"
      children.style.display = ''
    }
  }
}

function hideChildren(onttype) {
  var children = document.getElementById(onttype + "-children")
  var link = document.getElementById(onttype + "-children-link")
  link.innerHTML = "&gt;&nbsp;"
  children.style.display = "none"
}

function toggleChildren(onttype) {
  var children = document.getElementById(onttype + "-children")
  if (children.style.display == "none") {
    showChildren(onttype)
  } else {
    hideChildren(onttype)
  }
}

function setTargets() {
  if (top.location.href != window.location.href) { // we're in frames
    var links = document.getElementsByTagName("a")
    for (var i = 0; i < links.length; i++) {
      if (/data\/W/.test(links[i].href)) {
        links[i].target = "lexicon"
      }
    }
  }
}

// set display='' for all ONT type LIs under the given one, but leave
// onttype-children ULs alone
function makeAllDescendantsShowable(onttype) {
  document.getElementById(onttype).style.display = ''
  var children = document.getElementById(onttype + "-children")
  if (children) {
    children = children.childNodes
    for (var i = 0; i < children.length; i++) {
      if (children[i].tagName == "LI") {
	makeAllDescendantsShowable(children[i].getAttribute("id"))
      }
    }
  }
}

function filterByRoles() {
  var rolesbox = document.getElementsByName("roles")[0]
  var roles = rolesbox.value.toLowerCase().replace(/[^,\w\s\-]/,'').replace(/^[,\s]+/,'').replace(/[,\s]+$/,'')
  if (roles.length > 0) {
    roles = roles.split(/[,\s]+/)
    // ask which types we need to have loaded/expanded
    var xmlrequest = new XMLHttpRequest()
    xmlrequest.open("GET", "get-ont-types-for-roles?roles=" + roles.join(','), false)
    xmlrequest.send(null)
    var responseElement = xmlrequest.responseXML.documentElement
    var ancestors = responseElement.getAttribute("ancestors").split(',')
    var ontTypesWithRoles = responseElement.getAttribute("ont-types-with-roles").split(',')
    // expand them (load them if necessary)
    for (var i = 0; i < ancestors.length; i++) {
      showChildren(ancestors[i])
      // hide those children that don't lead to types with the roles
      if (ontTypesWithRoles.indexOf(ancestors[i]) == -1) { // a proper ancestor
	var children = document.getElementById(ancestors[i] + "-children")
	children = children.childNodes
	for (var j = 0; j < children.length; j++) {
	  if (children[j].tagName == "LI") {
	    if (ancestors.indexOf(children[j].getAttribute("id")) == -1) {
	      children[j].style.display = "none"
	    } else {
	      children[j].style.display = ''
	    }
	  }
	}
	// hide roles
	var span = document.getElementById(ancestors[i] + "-roles")
	if (span) {
	  var link = document.getElementById(ancestors[i] + "-roles-link")
	  link.innerHTML = link.innerHTML.replace("hide", "show")
	  span.style.display = 'none'
	}
      } else { // one of the top types with the roles
	makeAllDescendantsShowable(ancestors[i])
	// show roles
	var span = document.getElementById(ancestors[i] + "-roles")
	var link = document.getElementById(ancestors[i] + "-roles-link")
	link.innerHTML = link.innerHTML.replace("show", "hide")
	span.style.display = ''
      }
    }
  } else { // no roles specified, display all children
    makeAllDescendantsShowable("root")
  }
  return false
}

