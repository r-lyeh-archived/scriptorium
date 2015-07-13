function JavaScriptValueWrapper(i){
	this.i = i;
}

ObjectScriptInterface = {
	exportValues: {},
	
	exportJSValue: function(value, parent, i, name){
		if(value === true) return "ok::true";
		if(value === false) return "ok::false";
		if(value === undefined || value === null) return "ok::null";
		var type = typeof value;
		if(type == "number") return "ok::num::"+value;
		if(type == "string") return "ok::str::"+value;
		var mark = type == "function" ? "ok::fun::" : "ok::obj::";
		if("__javaScriptObjectIndex__" in value){
			i = value.__javaScriptObjectIndex__;
			// console.log("found export object", value);
			if(this.exportValues[i].value !== value){
				throw "program corrupted";
			}
			return mark+i;
		}
		value.__javaScriptObjectIndex__ = i;
		this.exportValues[i] = {parent:parent, name:name, value:value};
		return mark+i;
		// if($.isArray(value)){
	},
	
	getRootValue: function(i, name){
		var value = window[name];
		if(value === undefined && !(name in window)) return "unknown JS global property: "+name;
		return this.exportJSValue(value, null, i, name);
	},
	
	getIndirectValue: function(i, next_i, name){
		var obj = this.exportValues[i];
		var value = obj.value[name];
		// if(value === undefined) return "unknown JS property: "+this.exportValues[i].name+"."+name;
		return this.exportJSValue(value, obj.value, next_i, name);
	},
	
	getExportedValue: function(i){
		return this.exportValues[i].value;
	},
	
	callExportedValue: function(i, next_i){
		var obj = this.exportValues[i];
		console.log("[JS] callValue", obj, arguments);
		var func = obj.value;
		if(typeof func !== "function"){
			return "error to call not function: "+func;
		}
		var value = func.apply(obj.parent, Array.prototype.slice.call(arguments, 2));
		return this.exportJSValue(value, null, next_i, name);
	},
	
	importOSValue: function(i){
		throw "importOSValue: "+i;
	},
};

testNumVal = 123;
testStrVal = "my string";
testNullVal = null;
testUndefVal = undefined;
testTrueVal = true;
testFalseVal = false;
testObjVal = {user:"Ivan", age:21, item:{town:"Moscow", func: function(a, b, c){ 
	console.log("func called", a, b, c); 
	return "func-res:"+a+","+b+","+c; 
}}};


