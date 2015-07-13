require '../../examples-os/std'

function removeComments(s){
	s = s.replace(Regexp("/\/\*.*?\*\//"), "")
	s = s.replace(Regexp("/\/\/.*$/m"), "")
	return s
}

var gl_content = removeComments(File.readContents('C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\include\gl\GL.h'))
var glu_content = removeComments(File.readContents('C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\include\gl\GLU.h'))
var glut_content = removeComments(File.readContents('c:\Sources\OS\proj.win32\os\glut\glut.h')) // c:\Sources\libnoisesrc-1.0.0\glut\glut.h')

Buffer.__lshift = Buffer.append

var buf_numbers = Buffer() << "static OS::NumberDef oslib_gl_numbers[] = {\n"

var m = Regexp("/#define\s+(GL_[\w\d_]+)\s+([\w\d_]+)/g").exec(gl_content)
for(var i, count = 0, #m[0]; i < count; i++){
	buf_numbers << "\t{OS_TEXT(\"${m[1][i]}\"), ${m[1][i]}},\n"
}

buf_numbers << "\t// ------------------------\n"
var m = Regexp("/#define\s+(GLU_[\w\d_]+)\s+([\w\d_]+)/g").exec(glu_content)
for(var i, count = 0, #m[0]; i < count; i++){
	buf_numbers << "\t{OS_TEXT(\"${m[1][i]}\"), ${m[1][i]}},\n"
}

buf_numbers << "\t// ------------------------\n"
var m = Regexp("/#define\s+(GLUT_[\w\d_]+)\s+([\w\d_]+)/g").exec(glut_content)
for(var i, count = 0, #m[0]; i < count; i++){
	buf_numbers << "\t{OS_TEXT(\"${m[1][i]}\"), ${m[1][i]}},\n"
}
var m = Regexp("/#define\s+(GLUT_[\w\d_]+)\s+\(\(void\*\)(\d+)\)/g").exec(glut_content)
for(var i, count = 0, #m[0]; i < count; i++){
	buf_numbers << "\t{OS_TEXT(\"${m[1][i]}\"), ${m[2][i]}},\n"
}
buf_numbers << "\t{},\n};\n"

var gl_num_types = <<<END'
GLenum GLbitfield GLbyte GLshort GLint GLsizei GLubyte GLushort GLuint GLfloat GLclampf GLdouble GLclampd
int float double
END.split(Regexp("/\s+/")).flip()

gl_num_types['unsigned int'] = true

gl_const_v_types = {}
for(var _, v in <<<END'GLbyte GLshort GLint GLsizei GLubyte GLushort GLuint GLfloat GLclampf GLdouble GLclampd
int float double
END.split(Regexp("/\s+/"))){
	gl_const_v_types["const ${v} *"] = true
}

// print gl_v_types
// terminate()

// print "gl_num_types: ${gl_num_types}"

var buf = Buffer()
var buf_funcs = Buffer() << "static OS::FuncDef oslib_gl_funcs[] = {\n"

function funcDecl(ret, name, params){
	// if(name != 'glutCreateMenu') return;
	// print(arguments, params.split(Regexp("/\s*,\s*/")))
	
	// if(Regexp("/^(glut.+Func|gl.+\d[isfd]v)$/").test(name)){
	if(Regexp("/^(glut.+Func)$/").test(name)){
		print "[SKIP] ${name}: ${params}"
		return
	}
	var s = Buffer() << "static int OSLIB_${name}(OS * os, int params, int, int, void*)\n{\n"
	if(ret in gl_num_types){
		s << "\t${ret} r = ${name}("
		ret = 1
	}else{
		s << "\t${name}("
		ret = 0
	}
	if(params != "void" && params != "")
	for(var i, p in params.split(Regexp("/\s*,\s*/"))){
		p = p.trim()
		var m = Regexp("/^(.+?)\s*[\w\d_]+$/").exec(p)
		if(!m){ 
			print "[NOT SUPPORTED] ${name}: ${p}"
			return
		}
		var type = m[1]
		i > 0 && s << ",\n\t\t"
		if(type in gl_num_types){
			s << "(${type})os->toNumber(-params + ${i})"
		}else if(type == "GLboolean"){
			s << "(GLboolean)os->toBool(-params + ${i})"
		}else if(type == "const char *"){
			s << "os->toString(-params + ${i}).toChar()"
		}else if(type in gl_const_v_types){
			var t = Regexp("/^const (\w+) \*$/").exec(type)
			t = t[1]
			var n_m = Regexp("/(\d)u?[bisfd]v$/").exec(name)
			if(n_m[1]){
				s << "GLConstArrayWrapper<${t}>(os, -params + ${i}, ${n_m[1]}).toV()"
			}else if((n_m = Regexp("/(\d)[fd]$/").exec(name)) && n_m[1]){
				s << "GLConstArrayWrapper<${t}>(os, -params + ${i}, ${n_m[1]}).toV()"
			}else if(Regexp("/glLoadMatrixd|glLoadMatrixf/").test(name)){
				s << "GLConstArrayWrapper<${t}>(os, -params + ${i}, 16).toV()"
			}else if(Regexp("/[bisfd]v$/").test(name)){
				s << "GLConstArrayWrapper<${t}>(os, -params + ${i}).toV()"
			}else{
				// s << "GLConstArrayWrapper<${t}>(os, -params + ${i}).toV()"
				print "[NOT SUPPORTED] ${name}: ${type}"
				return
			}
		}else{
			print "[NOT SUPPORTED] ${name}: ${type}"
			return
		}
	}
	s << ");\n"
	if(ret > 0){
		s << "\tos->pushNumber((OS_NUMBER)r);\n"
	}
	s << "\treturn ${ret};\n"
	s << "}\n\n"
	buf << s
	buf_funcs << "\t{OS_TEXT(\"${name}\"), OSLIB_${name}},\n"
}

var m = Regexp("/WINGDIAPI\s+([\w]+)\s+APIENTRY\s+(gl[\w\d_]+)\s*\((.*?)\)\s*;/g").exec(gl_content)
for(var i, count = 0, #m[0]; i < count; i++){
	funcDecl(m[1][i], m[2][i], m[3][i])
}

var m = Regexp("/([\w]+)\s+APIENTRY\s+(glu[\w\d_]+)\s*\((.*?)\)\s*;/sg").exec(glu_content)
for(var i, count = 0, #m[0]; i < count; i++){
	funcDecl(m[1][i], m[2][i], m[3][i])
}

var m = Regexp("/extern\s+([\w]+)\s+APIENTRY\s+(glut[\w\d_]+)\s*\((.*?)\)\s*;/g").exec(glut_content)
for(var i, count = 0, #m[0]; i < count; i++){
	funcDecl(m[1][i], m[2][i], m[3][i])
}

buf_funcs << "\t{},\n};\n"

buf << buf_funcs << buf_numbers

File.writeContents("os-opengl.inc", buf)