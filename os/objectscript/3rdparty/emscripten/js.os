var runJSStringResult = runJSStringResult
var index = 0

var function importJSValue(ret){
	ret === "ok::true" && return true
	ret === "ok::false" && return false
	ret === "ok::null" && return null
	var prefix = ret.sub(0, 9)
	prefix === "ok::str::" && return ret.sub(9)
	prefix === "ok::num::" && return toNumber(ret.sub(9))
	prefix === "ok::obj::" && return importJSObject(toNumber(ret.sub(9)), "object")
	prefix === "ok::fun::" && return importJSObject(toNumber(ret.sub(9)), "function")
	throw ret
}

var exportValues = {}
var exportValuesByIndex = {}

var function exportValue(value){
	var type = typeOf(value)
	if(type == "null" || type == "boolean" || type == "number" || type == "string"){
		return json.encode(value)
	}
	if("__javaScriptObjectIndex__" in value){
		return "ObjectScriptInterface.getExportedValue(${value.__javaScriptObjectIndex__})"
	}
	if(!(value in exportValues)){
		var index = #exportValues
		exportValues[value] = index
		exportValuesByIndex[index] = value
	}
	return "ObjectScriptInterface.importOSValue(${exportValues[value]})"
}

function importJSObject(i, type){
	var func = function(){
		print "[OS] begin call func from: ${arguments}"
		var args = []
		for(var _, value in arguments){
			args[]	= exportValue(value)
		}
		var next_i = index++
		var ret = runJSStringResult("ObjectScriptInterface.callExportedValue(${i}, ${next_i}"
			.. (#args > 0 ? ", ${args.join(',')})" : ")"))
		print "[OS] ObjectScriptInterface.callValue(${i}, ${next_i}, ${args.join(',')}): ${ret}"
		return importJSValue(ret)
	}
	func.__javaScriptObjectIndex__ = i
	
	function func.valueOf(){
		throw "importJSObject.valueOf: ${i}"
	}	
	
	function func.__get(name){
		var next_i = index++
		var ret = runJSStringResult("ObjectScriptInterface.getIndirectValue(${i}, ${next_i}, ${json.encode(name)})")
		print "ObjectScriptInterface.getIndirectValue(${i}, ${next_i}, ${json.encode(name)}): ${ret}"
		return importJSValue(ret)
	}
	
	function func.__set(name, value){
		throw "set ${name} = ${value}"
	}
	
	return func
}

function __get(name){
	var i = index++
	name = stringOf(name) || throw "string required for name"
	var ret = runJSStringResult("ObjectScriptInterface.getRootValue(${i}, ${json.encode(name)})")
	// print "ObjectScriptInterface.getRootValue(${i}, ${json.encode(name)}): ${ret}"
	return importJSValue(ret)
}

function __set(name, value){
	if(functionOf(value)){
		if("__javaScriptObjectIndex__" in value){
			var valueEncoded = "new JavaScriptValueWrapper(${value.__javaScriptObjectIndex__})"
		}else{
			// common function
			var valueEncoded = "ObjectScriptInterface.wrapOSFunction(${value.__id})"
		}
	}else{
		var valueEncoded = json.encode(value)
	}
	var ret = runJSStringResult("ObjectScriptInterface.setRootValue(${json.encode(name)}, ${valueEncoded})")
	// print "ObjectScriptInterface.setRootValue(${json.encode(name)}, ${json.encode(value)}): ${ret}"
	ret !== "" && throw ret
}
