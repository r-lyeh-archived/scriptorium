static int OSLIB_glAccum(OS * os, int params, int, int, void*)
{
	glAccum((GLenum)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glAlphaFunc(OS * os, int params, int, int, void*)
{
	glAlphaFunc((GLenum)os->toNumber(-params + 0),
		(GLclampf)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glArrayElement(OS * os, int params, int, int, void*)
{
	glArrayElement((GLint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glBegin(OS * os, int params, int, int, void*)
{
	glBegin((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glBindTexture(OS * os, int params, int, int, void*)
{
	glBindTexture((GLenum)os->toNumber(-params + 0),
		(GLuint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glBlendFunc(OS * os, int params, int, int, void*)
{
	glBlendFunc((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glCallList(OS * os, int params, int, int, void*)
{
	glCallList((GLuint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glClear(OS * os, int params, int, int, void*)
{
	glClear((GLbitfield)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glClearAccum(OS * os, int params, int, int, void*)
{
	glClearAccum((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLfloat)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glClearColor(OS * os, int params, int, int, void*)
{
	glClearColor((GLclampf)os->toNumber(-params + 0),
		(GLclampf)os->toNumber(-params + 1),
		(GLclampf)os->toNumber(-params + 2),
		(GLclampf)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glClearDepth(OS * os, int params, int, int, void*)
{
	glClearDepth((GLclampd)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glClearIndex(OS * os, int params, int, int, void*)
{
	glClearIndex((GLfloat)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glClearStencil(OS * os, int params, int, int, void*)
{
	glClearStencil((GLint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glColor3b(OS * os, int params, int, int, void*)
{
	glColor3b((GLbyte)os->toNumber(-params + 0),
		(GLbyte)os->toNumber(-params + 1),
		(GLbyte)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glColor3bv(OS * os, int params, int, int, void*)
{
	glColor3bv(GLConstArrayWrapper<GLbyte>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glColor3d(OS * os, int params, int, int, void*)
{
	glColor3d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glColor3dv(OS * os, int params, int, int, void*)
{
	glColor3dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glColor3f(OS * os, int params, int, int, void*)
{
	glColor3f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glColor3fv(OS * os, int params, int, int, void*)
{
	glColor3fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glColor3i(OS * os, int params, int, int, void*)
{
	glColor3i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glColor3iv(OS * os, int params, int, int, void*)
{
	glColor3iv(GLConstArrayWrapper<GLint>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glColor3s(OS * os, int params, int, int, void*)
{
	glColor3s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glColor3sv(OS * os, int params, int, int, void*)
{
	glColor3sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glColor3ub(OS * os, int params, int, int, void*)
{
	glColor3ub((GLubyte)os->toNumber(-params + 0),
		(GLubyte)os->toNumber(-params + 1),
		(GLubyte)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glColor3ubv(OS * os, int params, int, int, void*)
{
	glColor3ubv(GLConstArrayWrapper<GLubyte>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glColor3ui(OS * os, int params, int, int, void*)
{
	glColor3ui((GLuint)os->toNumber(-params + 0),
		(GLuint)os->toNumber(-params + 1),
		(GLuint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glColor3uiv(OS * os, int params, int, int, void*)
{
	glColor3uiv(GLConstArrayWrapper<GLuint>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glColor3us(OS * os, int params, int, int, void*)
{
	glColor3us((GLushort)os->toNumber(-params + 0),
		(GLushort)os->toNumber(-params + 1),
		(GLushort)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glColor3usv(OS * os, int params, int, int, void*)
{
	glColor3usv(GLConstArrayWrapper<GLushort>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glColor4b(OS * os, int params, int, int, void*)
{
	glColor4b((GLbyte)os->toNumber(-params + 0),
		(GLbyte)os->toNumber(-params + 1),
		(GLbyte)os->toNumber(-params + 2),
		(GLbyte)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glColor4bv(OS * os, int params, int, int, void*)
{
	glColor4bv(GLConstArrayWrapper<GLbyte>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glColor4d(OS * os, int params, int, int, void*)
{
	glColor4d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glColor4dv(OS * os, int params, int, int, void*)
{
	glColor4dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glColor4f(OS * os, int params, int, int, void*)
{
	glColor4f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLfloat)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glColor4fv(OS * os, int params, int, int, void*)
{
	glColor4fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glColor4i(OS * os, int params, int, int, void*)
{
	glColor4i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glColor4iv(OS * os, int params, int, int, void*)
{
	glColor4iv(GLConstArrayWrapper<GLint>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glColor4s(OS * os, int params, int, int, void*)
{
	glColor4s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2),
		(GLshort)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glColor4sv(OS * os, int params, int, int, void*)
{
	glColor4sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glColor4ub(OS * os, int params, int, int, void*)
{
	glColor4ub((GLubyte)os->toNumber(-params + 0),
		(GLubyte)os->toNumber(-params + 1),
		(GLubyte)os->toNumber(-params + 2),
		(GLubyte)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glColor4ubv(OS * os, int params, int, int, void*)
{
	glColor4ubv(GLConstArrayWrapper<GLubyte>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glColor4ui(OS * os, int params, int, int, void*)
{
	glColor4ui((GLuint)os->toNumber(-params + 0),
		(GLuint)os->toNumber(-params + 1),
		(GLuint)os->toNumber(-params + 2),
		(GLuint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glColor4uiv(OS * os, int params, int, int, void*)
{
	glColor4uiv(GLConstArrayWrapper<GLuint>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glColor4us(OS * os, int params, int, int, void*)
{
	glColor4us((GLushort)os->toNumber(-params + 0),
		(GLushort)os->toNumber(-params + 1),
		(GLushort)os->toNumber(-params + 2),
		(GLushort)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glColor4usv(OS * os, int params, int, int, void*)
{
	glColor4usv(GLConstArrayWrapper<GLushort>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glColorMask(OS * os, int params, int, int, void*)
{
	glColorMask((GLboolean)os->toBool(-params + 0),
		(GLboolean)os->toBool(-params + 1),
		(GLboolean)os->toBool(-params + 2),
		(GLboolean)os->toBool(-params + 3));
	return 0;
}

static int OSLIB_glColorMaterial(OS * os, int params, int, int, void*)
{
	glColorMaterial((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glCopyPixels(OS * os, int params, int, int, void*)
{
	glCopyPixels((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLsizei)os->toNumber(-params + 2),
		(GLsizei)os->toNumber(-params + 3),
		(GLenum)os->toNumber(-params + 4));
	return 0;
}

static int OSLIB_glCopyTexImage1D(OS * os, int params, int, int, void*)
{
	glCopyTexImage1D((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLenum)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4),
		(GLsizei)os->toNumber(-params + 5),
		(GLint)os->toNumber(-params + 6));
	return 0;
}

static int OSLIB_glCopyTexImage2D(OS * os, int params, int, int, void*)
{
	glCopyTexImage2D((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLenum)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4),
		(GLsizei)os->toNumber(-params + 5),
		(GLsizei)os->toNumber(-params + 6),
		(GLint)os->toNumber(-params + 7));
	return 0;
}

static int OSLIB_glCopyTexSubImage1D(OS * os, int params, int, int, void*)
{
	glCopyTexSubImage1D((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4),
		(GLsizei)os->toNumber(-params + 5));
	return 0;
}

static int OSLIB_glCopyTexSubImage2D(OS * os, int params, int, int, void*)
{
	glCopyTexSubImage2D((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4),
		(GLint)os->toNumber(-params + 5),
		(GLsizei)os->toNumber(-params + 6),
		(GLsizei)os->toNumber(-params + 7));
	return 0;
}

static int OSLIB_glCullFace(OS * os, int params, int, int, void*)
{
	glCullFace((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glDeleteLists(OS * os, int params, int, int, void*)
{
	glDeleteLists((GLuint)os->toNumber(-params + 0),
		(GLsizei)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glDepthFunc(OS * os, int params, int, int, void*)
{
	glDepthFunc((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glDepthMask(OS * os, int params, int, int, void*)
{
	glDepthMask((GLboolean)os->toBool(-params + 0));
	return 0;
}

static int OSLIB_glDepthRange(OS * os, int params, int, int, void*)
{
	glDepthRange((GLclampd)os->toNumber(-params + 0),
		(GLclampd)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glDisable(OS * os, int params, int, int, void*)
{
	glDisable((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glDisableClientState(OS * os, int params, int, int, void*)
{
	glDisableClientState((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glDrawArrays(OS * os, int params, int, int, void*)
{
	glDrawArrays((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLsizei)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glDrawBuffer(OS * os, int params, int, int, void*)
{
	glDrawBuffer((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glEdgeFlag(OS * os, int params, int, int, void*)
{
	glEdgeFlag((GLboolean)os->toBool(-params + 0));
	return 0;
}

static int OSLIB_glEnable(OS * os, int params, int, int, void*)
{
	glEnable((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glEnableClientState(OS * os, int params, int, int, void*)
{
	glEnableClientState((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glEnd(OS * os, int params, int, int, void*)
{
	glEnd();
	return 0;
}

static int OSLIB_glEndList(OS * os, int params, int, int, void*)
{
	glEndList();
	return 0;
}

static int OSLIB_glEvalCoord1d(OS * os, int params, int, int, void*)
{
	glEvalCoord1d((GLdouble)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glEvalCoord1dv(OS * os, int params, int, int, void*)
{
	glEvalCoord1dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 1).toV());
	return 0;
}

static int OSLIB_glEvalCoord1f(OS * os, int params, int, int, void*)
{
	glEvalCoord1f((GLfloat)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glEvalCoord1fv(OS * os, int params, int, int, void*)
{
	glEvalCoord1fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 1).toV());
	return 0;
}

static int OSLIB_glEvalCoord2d(OS * os, int params, int, int, void*)
{
	glEvalCoord2d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glEvalCoord2dv(OS * os, int params, int, int, void*)
{
	glEvalCoord2dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glEvalCoord2f(OS * os, int params, int, int, void*)
{
	glEvalCoord2f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glEvalCoord2fv(OS * os, int params, int, int, void*)
{
	glEvalCoord2fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glEvalMesh1(OS * os, int params, int, int, void*)
{
	glEvalMesh1((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glEvalMesh2(OS * os, int params, int, int, void*)
{
	glEvalMesh2((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4));
	return 0;
}

static int OSLIB_glEvalPoint1(OS * os, int params, int, int, void*)
{
	glEvalPoint1((GLint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glEvalPoint2(OS * os, int params, int, int, void*)
{
	glEvalPoint2((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glFinish(OS * os, int params, int, int, void*)
{
	glFinish();
	return 0;
}

static int OSLIB_glFlush(OS * os, int params, int, int, void*)
{
	glFlush();
	return 0;
}

static int OSLIB_glFogf(OS * os, int params, int, int, void*)
{
	glFogf((GLenum)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glFogfv(OS * os, int params, int, int, void*)
{
	glFogfv((GLenum)os->toNumber(-params + 0),
		GLConstArrayWrapper<GLfloat>(os, -params + 1).toV());
	return 0;
}

static int OSLIB_glFogi(OS * os, int params, int, int, void*)
{
	glFogi((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glFogiv(OS * os, int params, int, int, void*)
{
	glFogiv((GLenum)os->toNumber(-params + 0),
		GLConstArrayWrapper<GLint>(os, -params + 1).toV());
	return 0;
}

static int OSLIB_glFrontFace(OS * os, int params, int, int, void*)
{
	glFrontFace((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glFrustum(OS * os, int params, int, int, void*)
{
	glFrustum((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3),
		(GLdouble)os->toNumber(-params + 4),
		(GLdouble)os->toNumber(-params + 5));
	return 0;
}

static int OSLIB_glGenLists(OS * os, int params, int, int, void*)
{
	GLuint r = glGenLists((GLsizei)os->toNumber(-params + 0));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glGetError(OS * os, int params, int, int, void*)
{
	GLenum r = glGetError();
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glHint(OS * os, int params, int, int, void*)
{
	glHint((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glIndexMask(OS * os, int params, int, int, void*)
{
	glIndexMask((GLuint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glIndexd(OS * os, int params, int, int, void*)
{
	glIndexd((GLdouble)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glIndexdv(OS * os, int params, int, int, void*)
{
	glIndexdv(GLConstArrayWrapper<GLdouble>(os, -params + 0).toV());
	return 0;
}

static int OSLIB_glIndexf(OS * os, int params, int, int, void*)
{
	glIndexf((GLfloat)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glIndexfv(OS * os, int params, int, int, void*)
{
	glIndexfv(GLConstArrayWrapper<GLfloat>(os, -params + 0).toV());
	return 0;
}

static int OSLIB_glIndexi(OS * os, int params, int, int, void*)
{
	glIndexi((GLint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glIndexiv(OS * os, int params, int, int, void*)
{
	glIndexiv(GLConstArrayWrapper<GLint>(os, -params + 0).toV());
	return 0;
}

static int OSLIB_glIndexs(OS * os, int params, int, int, void*)
{
	glIndexs((GLshort)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glIndexsv(OS * os, int params, int, int, void*)
{
	glIndexsv(GLConstArrayWrapper<GLshort>(os, -params + 0).toV());
	return 0;
}

static int OSLIB_glIndexub(OS * os, int params, int, int, void*)
{
	glIndexub((GLubyte)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glIndexubv(OS * os, int params, int, int, void*)
{
	glIndexubv(GLConstArrayWrapper<GLubyte>(os, -params + 0).toV());
	return 0;
}

static int OSLIB_glInitNames(OS * os, int params, int, int, void*)
{
	glInitNames();
	return 0;
}

static int OSLIB_glIsEnabled(OS * os, int params, int, int, void*)
{
	glIsEnabled((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glIsList(OS * os, int params, int, int, void*)
{
	glIsList((GLuint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glIsTexture(OS * os, int params, int, int, void*)
{
	glIsTexture((GLuint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glLightModelf(OS * os, int params, int, int, void*)
{
	glLightModelf((GLenum)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glLightModelfv(OS * os, int params, int, int, void*)
{
	glLightModelfv((GLenum)os->toNumber(-params + 0),
		GLConstArrayWrapper<GLfloat>(os, -params + 1).toV());
	return 0;
}

static int OSLIB_glLightModeli(OS * os, int params, int, int, void*)
{
	glLightModeli((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glLightModeliv(OS * os, int params, int, int, void*)
{
	glLightModeliv((GLenum)os->toNumber(-params + 0),
		GLConstArrayWrapper<GLint>(os, -params + 1).toV());
	return 0;
}

static int OSLIB_glLightf(OS * os, int params, int, int, void*)
{
	glLightf((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glLightfv(OS * os, int params, int, int, void*)
{
	glLightfv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLfloat>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glLighti(OS * os, int params, int, int, void*)
{
	glLighti((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glLightiv(OS * os, int params, int, int, void*)
{
	glLightiv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLint>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glLineStipple(OS * os, int params, int, int, void*)
{
	glLineStipple((GLint)os->toNumber(-params + 0),
		(GLushort)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glLineWidth(OS * os, int params, int, int, void*)
{
	glLineWidth((GLfloat)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glListBase(OS * os, int params, int, int, void*)
{
	glListBase((GLuint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glLoadIdentity(OS * os, int params, int, int, void*)
{
	glLoadIdentity();
	return 0;
}

static int OSLIB_glLoadMatrixd(OS * os, int params, int, int, void*)
{
	glLoadMatrixd(GLConstArrayWrapper<GLdouble>(os, -params + 0, 16).toV());
	return 0;
}

static int OSLIB_glLoadMatrixf(OS * os, int params, int, int, void*)
{
	glLoadMatrixf(GLConstArrayWrapper<GLfloat>(os, -params + 0, 16).toV());
	return 0;
}

static int OSLIB_glLoadName(OS * os, int params, int, int, void*)
{
	glLoadName((GLuint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glLogicOp(OS * os, int params, int, int, void*)
{
	glLogicOp((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glMap1d(OS * os, int params, int, int, void*)
{
	glMap1d((GLenum)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4),
		GLConstArrayWrapper<GLdouble>(os, -params + 5, 1).toV());
	return 0;
}

static int OSLIB_glMap1f(OS * os, int params, int, int, void*)
{
	glMap1f((GLenum)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4),
		GLConstArrayWrapper<GLfloat>(os, -params + 5, 1).toV());
	return 0;
}

static int OSLIB_glMap2d(OS * os, int params, int, int, void*)
{
	glMap2d((GLenum)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4),
		(GLdouble)os->toNumber(-params + 5),
		(GLdouble)os->toNumber(-params + 6),
		(GLint)os->toNumber(-params + 7),
		(GLint)os->toNumber(-params + 8),
		GLConstArrayWrapper<GLdouble>(os, -params + 9, 2).toV());
	return 0;
}

static int OSLIB_glMap2f(OS * os, int params, int, int, void*)
{
	glMap2f((GLenum)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLint)os->toNumber(-params + 4),
		(GLfloat)os->toNumber(-params + 5),
		(GLfloat)os->toNumber(-params + 6),
		(GLint)os->toNumber(-params + 7),
		(GLint)os->toNumber(-params + 8),
		GLConstArrayWrapper<GLfloat>(os, -params + 9, 2).toV());
	return 0;
}

static int OSLIB_glMapGrid1d(OS * os, int params, int, int, void*)
{
	glMapGrid1d((GLint)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glMapGrid1f(OS * os, int params, int, int, void*)
{
	glMapGrid1f((GLint)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glMapGrid2d(OS * os, int params, int, int, void*)
{
	glMapGrid2d((GLint)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLdouble)os->toNumber(-params + 4),
		(GLdouble)os->toNumber(-params + 5));
	return 0;
}

static int OSLIB_glMapGrid2f(OS * os, int params, int, int, void*)
{
	glMapGrid2f((GLint)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3),
		(GLfloat)os->toNumber(-params + 4),
		(GLfloat)os->toNumber(-params + 5));
	return 0;
}

static int OSLIB_glMaterialf(OS * os, int params, int, int, void*)
{
	glMaterialf((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glMaterialfv(OS * os, int params, int, int, void*)
{
	glMaterialfv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLfloat>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glMateriali(OS * os, int params, int, int, void*)
{
	glMateriali((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glMaterialiv(OS * os, int params, int, int, void*)
{
	glMaterialiv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLint>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glMatrixMode(OS * os, int params, int, int, void*)
{
	glMatrixMode((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glNewList(OS * os, int params, int, int, void*)
{
	glNewList((GLuint)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glNormal3b(OS * os, int params, int, int, void*)
{
	glNormal3b((GLbyte)os->toNumber(-params + 0),
		(GLbyte)os->toNumber(-params + 1),
		(GLbyte)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glNormal3bv(OS * os, int params, int, int, void*)
{
	glNormal3bv(GLConstArrayWrapper<GLbyte>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glNormal3d(OS * os, int params, int, int, void*)
{
	glNormal3d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glNormal3dv(OS * os, int params, int, int, void*)
{
	glNormal3dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glNormal3f(OS * os, int params, int, int, void*)
{
	glNormal3f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glNormal3fv(OS * os, int params, int, int, void*)
{
	glNormal3fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glNormal3i(OS * os, int params, int, int, void*)
{
	glNormal3i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glNormal3iv(OS * os, int params, int, int, void*)
{
	glNormal3iv(GLConstArrayWrapper<GLint>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glNormal3s(OS * os, int params, int, int, void*)
{
	glNormal3s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glNormal3sv(OS * os, int params, int, int, void*)
{
	glNormal3sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glOrtho(OS * os, int params, int, int, void*)
{
	glOrtho((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3),
		(GLdouble)os->toNumber(-params + 4),
		(GLdouble)os->toNumber(-params + 5));
	return 0;
}

static int OSLIB_glPassThrough(OS * os, int params, int, int, void*)
{
	glPassThrough((GLfloat)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glPixelMapfv(OS * os, int params, int, int, void*)
{
	glPixelMapfv((GLenum)os->toNumber(-params + 0),
		(GLsizei)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLfloat>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glPixelMapuiv(OS * os, int params, int, int, void*)
{
	glPixelMapuiv((GLenum)os->toNumber(-params + 0),
		(GLsizei)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLuint>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glPixelMapusv(OS * os, int params, int, int, void*)
{
	glPixelMapusv((GLenum)os->toNumber(-params + 0),
		(GLsizei)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLushort>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glPixelStoref(OS * os, int params, int, int, void*)
{
	glPixelStoref((GLenum)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glPixelStorei(OS * os, int params, int, int, void*)
{
	glPixelStorei((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glPixelTransferf(OS * os, int params, int, int, void*)
{
	glPixelTransferf((GLenum)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glPixelTransferi(OS * os, int params, int, int, void*)
{
	glPixelTransferi((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glPixelZoom(OS * os, int params, int, int, void*)
{
	glPixelZoom((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glPointSize(OS * os, int params, int, int, void*)
{
	glPointSize((GLfloat)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glPolygonMode(OS * os, int params, int, int, void*)
{
	glPolygonMode((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glPolygonOffset(OS * os, int params, int, int, void*)
{
	glPolygonOffset((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glPopAttrib(OS * os, int params, int, int, void*)
{
	glPopAttrib();
	return 0;
}

static int OSLIB_glPopClientAttrib(OS * os, int params, int, int, void*)
{
	glPopClientAttrib();
	return 0;
}

static int OSLIB_glPopMatrix(OS * os, int params, int, int, void*)
{
	glPopMatrix();
	return 0;
}

static int OSLIB_glPopName(OS * os, int params, int, int, void*)
{
	glPopName();
	return 0;
}

static int OSLIB_glPushAttrib(OS * os, int params, int, int, void*)
{
	glPushAttrib((GLbitfield)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glPushClientAttrib(OS * os, int params, int, int, void*)
{
	glPushClientAttrib((GLbitfield)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glPushMatrix(OS * os, int params, int, int, void*)
{
	glPushMatrix();
	return 0;
}

static int OSLIB_glPushName(OS * os, int params, int, int, void*)
{
	glPushName((GLuint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glRasterPos2d(OS * os, int params, int, int, void*)
{
	glRasterPos2d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glRasterPos2dv(OS * os, int params, int, int, void*)
{
	glRasterPos2dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glRasterPos2f(OS * os, int params, int, int, void*)
{
	glRasterPos2f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glRasterPos2fv(OS * os, int params, int, int, void*)
{
	glRasterPos2fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glRasterPos2i(OS * os, int params, int, int, void*)
{
	glRasterPos2i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glRasterPos2iv(OS * os, int params, int, int, void*)
{
	glRasterPos2iv(GLConstArrayWrapper<GLint>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glRasterPos2s(OS * os, int params, int, int, void*)
{
	glRasterPos2s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glRasterPos2sv(OS * os, int params, int, int, void*)
{
	glRasterPos2sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glRasterPos3d(OS * os, int params, int, int, void*)
{
	glRasterPos3d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glRasterPos3dv(OS * os, int params, int, int, void*)
{
	glRasterPos3dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glRasterPos3f(OS * os, int params, int, int, void*)
{
	glRasterPos3f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glRasterPos3fv(OS * os, int params, int, int, void*)
{
	glRasterPos3fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glRasterPos3i(OS * os, int params, int, int, void*)
{
	glRasterPos3i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glRasterPos3iv(OS * os, int params, int, int, void*)
{
	glRasterPos3iv(GLConstArrayWrapper<GLint>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glRasterPos3s(OS * os, int params, int, int, void*)
{
	glRasterPos3s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glRasterPos3sv(OS * os, int params, int, int, void*)
{
	glRasterPos3sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glRasterPos4d(OS * os, int params, int, int, void*)
{
	glRasterPos4d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRasterPos4dv(OS * os, int params, int, int, void*)
{
	glRasterPos4dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glRasterPos4f(OS * os, int params, int, int, void*)
{
	glRasterPos4f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLfloat)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRasterPos4fv(OS * os, int params, int, int, void*)
{
	glRasterPos4fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glRasterPos4i(OS * os, int params, int, int, void*)
{
	glRasterPos4i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRasterPos4iv(OS * os, int params, int, int, void*)
{
	glRasterPos4iv(GLConstArrayWrapper<GLint>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glRasterPos4s(OS * os, int params, int, int, void*)
{
	glRasterPos4s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2),
		(GLshort)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRasterPos4sv(OS * os, int params, int, int, void*)
{
	glRasterPos4sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glReadBuffer(OS * os, int params, int, int, void*)
{
	glReadBuffer((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glRectd(OS * os, int params, int, int, void*)
{
	glRectd((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRectdv(OS * os, int params, int, int, void*)
{
	glRectdv(GLConstArrayWrapper<GLdouble>(os, -params + 0).toV(),
		GLConstArrayWrapper<GLdouble>(os, -params + 1).toV());
	return 0;
}

static int OSLIB_glRectf(OS * os, int params, int, int, void*)
{
	glRectf((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLfloat)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRectfv(OS * os, int params, int, int, void*)
{
	glRectfv(GLConstArrayWrapper<GLfloat>(os, -params + 0).toV(),
		GLConstArrayWrapper<GLfloat>(os, -params + 1).toV());
	return 0;
}

static int OSLIB_glRecti(OS * os, int params, int, int, void*)
{
	glRecti((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRectiv(OS * os, int params, int, int, void*)
{
	glRectiv(GLConstArrayWrapper<GLint>(os, -params + 0).toV(),
		GLConstArrayWrapper<GLint>(os, -params + 1).toV());
	return 0;
}

static int OSLIB_glRects(OS * os, int params, int, int, void*)
{
	glRects((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2),
		(GLshort)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRectsv(OS * os, int params, int, int, void*)
{
	glRectsv(GLConstArrayWrapper<GLshort>(os, -params + 0).toV(),
		GLConstArrayWrapper<GLshort>(os, -params + 1).toV());
	return 0;
}

static int OSLIB_glRenderMode(OS * os, int params, int, int, void*)
{
	GLint r = glRenderMode((GLenum)os->toNumber(-params + 0));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glRotated(OS * os, int params, int, int, void*)
{
	glRotated((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glRotatef(OS * os, int params, int, int, void*)
{
	glRotatef((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLfloat)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glScaled(OS * os, int params, int, int, void*)
{
	glScaled((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glScalef(OS * os, int params, int, int, void*)
{
	glScalef((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glScissor(OS * os, int params, int, int, void*)
{
	glScissor((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLsizei)os->toNumber(-params + 2),
		(GLsizei)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glShadeModel(OS * os, int params, int, int, void*)
{
	glShadeModel((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glStencilFunc(OS * os, int params, int, int, void*)
{
	glStencilFunc((GLenum)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLuint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glStencilMask(OS * os, int params, int, int, void*)
{
	glStencilMask((GLuint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glStencilOp(OS * os, int params, int, int, void*)
{
	glStencilOp((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLenum)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexCoord1d(OS * os, int params, int, int, void*)
{
	glTexCoord1d((GLdouble)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glTexCoord1dv(OS * os, int params, int, int, void*)
{
	glTexCoord1dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 1).toV());
	return 0;
}

static int OSLIB_glTexCoord1f(OS * os, int params, int, int, void*)
{
	glTexCoord1f((GLfloat)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glTexCoord1fv(OS * os, int params, int, int, void*)
{
	glTexCoord1fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 1).toV());
	return 0;
}

static int OSLIB_glTexCoord1i(OS * os, int params, int, int, void*)
{
	glTexCoord1i((GLint)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glTexCoord1iv(OS * os, int params, int, int, void*)
{
	glTexCoord1iv(GLConstArrayWrapper<GLint>(os, -params + 0, 1).toV());
	return 0;
}

static int OSLIB_glTexCoord1s(OS * os, int params, int, int, void*)
{
	glTexCoord1s((GLshort)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glTexCoord1sv(OS * os, int params, int, int, void*)
{
	glTexCoord1sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 1).toV());
	return 0;
}

static int OSLIB_glTexCoord2d(OS * os, int params, int, int, void*)
{
	glTexCoord2d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glTexCoord2dv(OS * os, int params, int, int, void*)
{
	glTexCoord2dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glTexCoord2f(OS * os, int params, int, int, void*)
{
	glTexCoord2f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glTexCoord2fv(OS * os, int params, int, int, void*)
{
	glTexCoord2fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glTexCoord2i(OS * os, int params, int, int, void*)
{
	glTexCoord2i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glTexCoord2iv(OS * os, int params, int, int, void*)
{
	glTexCoord2iv(GLConstArrayWrapper<GLint>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glTexCoord2s(OS * os, int params, int, int, void*)
{
	glTexCoord2s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glTexCoord2sv(OS * os, int params, int, int, void*)
{
	glTexCoord2sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glTexCoord3d(OS * os, int params, int, int, void*)
{
	glTexCoord3d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexCoord3dv(OS * os, int params, int, int, void*)
{
	glTexCoord3dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glTexCoord3f(OS * os, int params, int, int, void*)
{
	glTexCoord3f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexCoord3fv(OS * os, int params, int, int, void*)
{
	glTexCoord3fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glTexCoord3i(OS * os, int params, int, int, void*)
{
	glTexCoord3i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexCoord3iv(OS * os, int params, int, int, void*)
{
	glTexCoord3iv(GLConstArrayWrapper<GLint>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glTexCoord3s(OS * os, int params, int, int, void*)
{
	glTexCoord3s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexCoord3sv(OS * os, int params, int, int, void*)
{
	glTexCoord3sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glTexCoord4d(OS * os, int params, int, int, void*)
{
	glTexCoord4d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glTexCoord4dv(OS * os, int params, int, int, void*)
{
	glTexCoord4dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glTexCoord4f(OS * os, int params, int, int, void*)
{
	glTexCoord4f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLfloat)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glTexCoord4fv(OS * os, int params, int, int, void*)
{
	glTexCoord4fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glTexCoord4i(OS * os, int params, int, int, void*)
{
	glTexCoord4i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glTexCoord4iv(OS * os, int params, int, int, void*)
{
	glTexCoord4iv(GLConstArrayWrapper<GLint>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glTexCoord4s(OS * os, int params, int, int, void*)
{
	glTexCoord4s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2),
		(GLshort)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glTexCoord4sv(OS * os, int params, int, int, void*)
{
	glTexCoord4sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glTexEnvf(OS * os, int params, int, int, void*)
{
	glTexEnvf((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexEnvfv(OS * os, int params, int, int, void*)
{
	glTexEnvfv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLfloat>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glTexEnvi(OS * os, int params, int, int, void*)
{
	glTexEnvi((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexEnviv(OS * os, int params, int, int, void*)
{
	glTexEnviv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLint>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glTexGend(OS * os, int params, int, int, void*)
{
	glTexGend((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexGendv(OS * os, int params, int, int, void*)
{
	glTexGendv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLdouble>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glTexGenf(OS * os, int params, int, int, void*)
{
	glTexGenf((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexGenfv(OS * os, int params, int, int, void*)
{
	glTexGenfv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLfloat>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glTexGeni(OS * os, int params, int, int, void*)
{
	glTexGeni((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexGeniv(OS * os, int params, int, int, void*)
{
	glTexGeniv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLint>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glTexParameterf(OS * os, int params, int, int, void*)
{
	glTexParameterf((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexParameterfv(OS * os, int params, int, int, void*)
{
	glTexParameterfv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLfloat>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glTexParameteri(OS * os, int params, int, int, void*)
{
	glTexParameteri((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTexParameteriv(OS * os, int params, int, int, void*)
{
	glTexParameteriv((GLenum)os->toNumber(-params + 0),
		(GLenum)os->toNumber(-params + 1),
		GLConstArrayWrapper<GLint>(os, -params + 2).toV());
	return 0;
}

static int OSLIB_glTranslated(OS * os, int params, int, int, void*)
{
	glTranslated((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glTranslatef(OS * os, int params, int, int, void*)
{
	glTranslatef((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glVertex2d(OS * os, int params, int, int, void*)
{
	glVertex2d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glVertex2dv(OS * os, int params, int, int, void*)
{
	glVertex2dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glVertex2f(OS * os, int params, int, int, void*)
{
	glVertex2f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glVertex2fv(OS * os, int params, int, int, void*)
{
	glVertex2fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glVertex2i(OS * os, int params, int, int, void*)
{
	glVertex2i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glVertex2iv(OS * os, int params, int, int, void*)
{
	glVertex2iv(GLConstArrayWrapper<GLint>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glVertex2s(OS * os, int params, int, int, void*)
{
	glVertex2s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glVertex2sv(OS * os, int params, int, int, void*)
{
	glVertex2sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 2).toV());
	return 0;
}

static int OSLIB_glVertex3d(OS * os, int params, int, int, void*)
{
	glVertex3d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glVertex3dv(OS * os, int params, int, int, void*)
{
	glVertex3dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glVertex3f(OS * os, int params, int, int, void*)
{
	glVertex3f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glVertex3fv(OS * os, int params, int, int, void*)
{
	glVertex3fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glVertex3i(OS * os, int params, int, int, void*)
{
	glVertex3i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glVertex3iv(OS * os, int params, int, int, void*)
{
	glVertex3iv(GLConstArrayWrapper<GLint>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glVertex3s(OS * os, int params, int, int, void*)
{
	glVertex3s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glVertex3sv(OS * os, int params, int, int, void*)
{
	glVertex3sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 3).toV());
	return 0;
}

static int OSLIB_glVertex4d(OS * os, int params, int, int, void*)
{
	glVertex4d((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glVertex4dv(OS * os, int params, int, int, void*)
{
	glVertex4dv(GLConstArrayWrapper<GLdouble>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glVertex4f(OS * os, int params, int, int, void*)
{
	glVertex4f((GLfloat)os->toNumber(-params + 0),
		(GLfloat)os->toNumber(-params + 1),
		(GLfloat)os->toNumber(-params + 2),
		(GLfloat)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glVertex4fv(OS * os, int params, int, int, void*)
{
	glVertex4fv(GLConstArrayWrapper<GLfloat>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glVertex4i(OS * os, int params, int, int, void*)
{
	glVertex4i((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glVertex4iv(OS * os, int params, int, int, void*)
{
	glVertex4iv(GLConstArrayWrapper<GLint>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glVertex4s(OS * os, int params, int, int, void*)
{
	glVertex4s((GLshort)os->toNumber(-params + 0),
		(GLshort)os->toNumber(-params + 1),
		(GLshort)os->toNumber(-params + 2),
		(GLshort)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glVertex4sv(OS * os, int params, int, int, void*)
{
	glVertex4sv(GLConstArrayWrapper<GLshort>(os, -params + 0, 4).toV());
	return 0;
}

static int OSLIB_glViewport(OS * os, int params, int, int, void*)
{
	glViewport((GLint)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLsizei)os->toNumber(-params + 2),
		(GLsizei)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_gluErrorStringWIN(OS * os, int params, int, int, void*)
{
	gluErrorStringWIN((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_gluOrtho2D(OS * os, int params, int, int, void*)
{
	gluOrtho2D((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_gluPerspective(OS * os, int params, int, int, void*)
{
	gluPerspective((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_gluLookAt(OS * os, int params, int, int, void*)
{
	gluLookAt((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLdouble)os->toNumber(-params + 2),
		(GLdouble)os->toNumber(-params + 3),
		(GLdouble)os->toNumber(-params + 4),
		(GLdouble)os->toNumber(-params + 5),
		(GLdouble)os->toNumber(-params + 6),
		(GLdouble)os->toNumber(-params + 7),
		(GLdouble)os->toNumber(-params + 8));
	return 0;
}

static int OSLIB_glutInitDisplayMode(OS * os, int params, int, int, void*)
{
	glutInitDisplayMode((unsigned int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutInitDisplayString(OS * os, int params, int, int, void*)
{
	glutInitDisplayString(os->toString(-params + 0).toChar());
	return 0;
}

static int OSLIB_glutInitWindowPosition(OS * os, int params, int, int, void*)
{
	glutInitWindowPosition((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glutInitWindowSize(OS * os, int params, int, int, void*)
{
	glutInitWindowSize((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glutMainLoop(OS * os, int params, int, int, void*)
{
	glutMainLoop();
	return 0;
}

static int OSLIB_glutCreateWindow(OS * os, int params, int, int, void*)
{
	int r = glutCreateWindow(os->toString(-params + 0).toChar());
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutCreateSubWindow(OS * os, int params, int, int, void*)
{
	int r = glutCreateSubWindow((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1),
		(int)os->toNumber(-params + 2),
		(int)os->toNumber(-params + 3),
		(int)os->toNumber(-params + 4));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutDestroyWindow(OS * os, int params, int, int, void*)
{
	glutDestroyWindow((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutPostRedisplay(OS * os, int params, int, int, void*)
{
	glutPostRedisplay();
	return 0;
}

static int OSLIB_glutPostWindowRedisplay(OS * os, int params, int, int, void*)
{
	glutPostWindowRedisplay((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutSwapBuffers(OS * os, int params, int, int, void*)
{
	glutSwapBuffers();
	return 0;
}

static int OSLIB_glutGetWindow(OS * os, int params, int, int, void*)
{
	int r = glutGetWindow();
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutSetWindow(OS * os, int params, int, int, void*)
{
	glutSetWindow((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutSetWindowTitle(OS * os, int params, int, int, void*)
{
	glutSetWindowTitle(os->toString(-params + 0).toChar());
	return 0;
}

static int OSLIB_glutSetIconTitle(OS * os, int params, int, int, void*)
{
	glutSetIconTitle(os->toString(-params + 0).toChar());
	return 0;
}

static int OSLIB_glutPositionWindow(OS * os, int params, int, int, void*)
{
	glutPositionWindow((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glutReshapeWindow(OS * os, int params, int, int, void*)
{
	glutReshapeWindow((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glutPopWindow(OS * os, int params, int, int, void*)
{
	glutPopWindow();
	return 0;
}

static int OSLIB_glutPushWindow(OS * os, int params, int, int, void*)
{
	glutPushWindow();
	return 0;
}

static int OSLIB_glutIconifyWindow(OS * os, int params, int, int, void*)
{
	glutIconifyWindow();
	return 0;
}

static int OSLIB_glutShowWindow(OS * os, int params, int, int, void*)
{
	glutShowWindow();
	return 0;
}

static int OSLIB_glutHideWindow(OS * os, int params, int, int, void*)
{
	glutHideWindow();
	return 0;
}

static int OSLIB_glutFullScreen(OS * os, int params, int, int, void*)
{
	glutFullScreen();
	return 0;
}

static int OSLIB_glutSetCursor(OS * os, int params, int, int, void*)
{
	glutSetCursor((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutWarpPointer(OS * os, int params, int, int, void*)
{
	glutWarpPointer((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glutEstablishOverlay(OS * os, int params, int, int, void*)
{
	glutEstablishOverlay();
	return 0;
}

static int OSLIB_glutRemoveOverlay(OS * os, int params, int, int, void*)
{
	glutRemoveOverlay();
	return 0;
}

static int OSLIB_glutUseLayer(OS * os, int params, int, int, void*)
{
	glutUseLayer((GLenum)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutPostOverlayRedisplay(OS * os, int params, int, int, void*)
{
	glutPostOverlayRedisplay();
	return 0;
}

static int OSLIB_glutPostWindowOverlayRedisplay(OS * os, int params, int, int, void*)
{
	glutPostWindowOverlayRedisplay((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutShowOverlay(OS * os, int params, int, int, void*)
{
	glutShowOverlay();
	return 0;
}

static int OSLIB_glutHideOverlay(OS * os, int params, int, int, void*)
{
	glutHideOverlay();
	return 0;
}

static int OSLIB_glutDestroyMenu(OS * os, int params, int, int, void*)
{
	glutDestroyMenu((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutGetMenu(OS * os, int params, int, int, void*)
{
	int r = glutGetMenu();
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutSetMenu(OS * os, int params, int, int, void*)
{
	glutSetMenu((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutAddMenuEntry(OS * os, int params, int, int, void*)
{
	glutAddMenuEntry(os->toString(-params + 0).toChar(),
		(int)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glutAddSubMenu(OS * os, int params, int, int, void*)
{
	glutAddSubMenu(os->toString(-params + 0).toChar(),
		(int)os->toNumber(-params + 1));
	return 0;
}

static int OSLIB_glutChangeToMenuEntry(OS * os, int params, int, int, void*)
{
	glutChangeToMenuEntry((int)os->toNumber(-params + 0),
		os->toString(-params + 1).toChar(),
		(int)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glutChangeToSubMenu(OS * os, int params, int, int, void*)
{
	glutChangeToSubMenu((int)os->toNumber(-params + 0),
		os->toString(-params + 1).toChar(),
		(int)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glutRemoveMenuItem(OS * os, int params, int, int, void*)
{
	glutRemoveMenuItem((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutAttachMenu(OS * os, int params, int, int, void*)
{
	glutAttachMenu((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutDetachMenu(OS * os, int params, int, int, void*)
{
	glutDetachMenu((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutGetColor(OS * os, int params, int, int, void*)
{
	GLfloat r = glutGetColor((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutCopyColormap(OS * os, int params, int, int, void*)
{
	glutCopyColormap((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutGet(OS * os, int params, int, int, void*)
{
	int r = glutGet((GLenum)os->toNumber(-params + 0));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutDeviceGet(OS * os, int params, int, int, void*)
{
	int r = glutDeviceGet((GLenum)os->toNumber(-params + 0));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutExtensionSupported(OS * os, int params, int, int, void*)
{
	int r = glutExtensionSupported(os->toString(-params + 0).toChar());
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutGetModifiers(OS * os, int params, int, int, void*)
{
	int r = glutGetModifiers();
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutLayerGet(OS * os, int params, int, int, void*)
{
	int r = glutLayerGet((GLenum)os->toNumber(-params + 0));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutWireSphere(OS * os, int params, int, int, void*)
{
	glutWireSphere((GLdouble)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glutSolidSphere(OS * os, int params, int, int, void*)
{
	glutSolidSphere((GLdouble)os->toNumber(-params + 0),
		(GLint)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2));
	return 0;
}

static int OSLIB_glutWireCone(OS * os, int params, int, int, void*)
{
	glutWireCone((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glutSolidCone(OS * os, int params, int, int, void*)
{
	glutSolidCone((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glutWireCube(OS * os, int params, int, int, void*)
{
	glutWireCube((GLdouble)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutSolidCube(OS * os, int params, int, int, void*)
{
	glutSolidCube((GLdouble)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutWireTorus(OS * os, int params, int, int, void*)
{
	glutWireTorus((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glutSolidTorus(OS * os, int params, int, int, void*)
{
	glutSolidTorus((GLdouble)os->toNumber(-params + 0),
		(GLdouble)os->toNumber(-params + 1),
		(GLint)os->toNumber(-params + 2),
		(GLint)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glutWireDodecahedron(OS * os, int params, int, int, void*)
{
	glutWireDodecahedron();
	return 0;
}

static int OSLIB_glutSolidDodecahedron(OS * os, int params, int, int, void*)
{
	glutSolidDodecahedron();
	return 0;
}

static int OSLIB_glutWireTeapot(OS * os, int params, int, int, void*)
{
	glutWireTeapot((GLdouble)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutSolidTeapot(OS * os, int params, int, int, void*)
{
	glutSolidTeapot((GLdouble)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutWireOctahedron(OS * os, int params, int, int, void*)
{
	glutWireOctahedron();
	return 0;
}

static int OSLIB_glutSolidOctahedron(OS * os, int params, int, int, void*)
{
	glutSolidOctahedron();
	return 0;
}

static int OSLIB_glutWireTetrahedron(OS * os, int params, int, int, void*)
{
	glutWireTetrahedron();
	return 0;
}

static int OSLIB_glutSolidTetrahedron(OS * os, int params, int, int, void*)
{
	glutSolidTetrahedron();
	return 0;
}

static int OSLIB_glutWireIcosahedron(OS * os, int params, int, int, void*)
{
	glutWireIcosahedron();
	return 0;
}

static int OSLIB_glutSolidIcosahedron(OS * os, int params, int, int, void*)
{
	glutSolidIcosahedron();
	return 0;
}

static int OSLIB_glutVideoResizeGet(OS * os, int params, int, int, void*)
{
	int r = glutVideoResizeGet((GLenum)os->toNumber(-params + 0));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutSetupVideoResizing(OS * os, int params, int, int, void*)
{
	glutSetupVideoResizing();
	return 0;
}

static int OSLIB_glutStopVideoResizing(OS * os, int params, int, int, void*)
{
	glutStopVideoResizing();
	return 0;
}

static int OSLIB_glutVideoResize(OS * os, int params, int, int, void*)
{
	glutVideoResize((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1),
		(int)os->toNumber(-params + 2),
		(int)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glutVideoPan(OS * os, int params, int, int, void*)
{
	glutVideoPan((int)os->toNumber(-params + 0),
		(int)os->toNumber(-params + 1),
		(int)os->toNumber(-params + 2),
		(int)os->toNumber(-params + 3));
	return 0;
}

static int OSLIB_glutReportErrors(OS * os, int params, int, int, void*)
{
	glutReportErrors();
	return 0;
}

static int OSLIB_glutIgnoreKeyRepeat(OS * os, int params, int, int, void*)
{
	glutIgnoreKeyRepeat((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutSetKeyRepeat(OS * os, int params, int, int, void*)
{
	glutSetKeyRepeat((int)os->toNumber(-params + 0));
	return 0;
}

static int OSLIB_glutGameModeString(OS * os, int params, int, int, void*)
{
	glutGameModeString(os->toString(-params + 0).toChar());
	return 0;
}

static int OSLIB_glutEnterGameMode(OS * os, int params, int, int, void*)
{
	int r = glutEnterGameMode();
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static int OSLIB_glutLeaveGameMode(OS * os, int params, int, int, void*)
{
	glutLeaveGameMode();
	return 0;
}

static int OSLIB_glutGameModeGet(OS * os, int params, int, int, void*)
{
	int r = glutGameModeGet((GLenum)os->toNumber(-params + 0));
	os->pushNumber((OS_NUMBER)r);
	return 1;
}

static OS::FuncDef oslib_gl_funcs[] = {
	{OS_TEXT("glAccum"), OSLIB_glAccum},
	{OS_TEXT("glAlphaFunc"), OSLIB_glAlphaFunc},
	{OS_TEXT("glArrayElement"), OSLIB_glArrayElement},
	{OS_TEXT("glBegin"), OSLIB_glBegin},
	{OS_TEXT("glBindTexture"), OSLIB_glBindTexture},
	{OS_TEXT("glBlendFunc"), OSLIB_glBlendFunc},
	{OS_TEXT("glCallList"), OSLIB_glCallList},
	{OS_TEXT("glClear"), OSLIB_glClear},
	{OS_TEXT("glClearAccum"), OSLIB_glClearAccum},
	{OS_TEXT("glClearColor"), OSLIB_glClearColor},
	{OS_TEXT("glClearDepth"), OSLIB_glClearDepth},
	{OS_TEXT("glClearIndex"), OSLIB_glClearIndex},
	{OS_TEXT("glClearStencil"), OSLIB_glClearStencil},
	{OS_TEXT("glColor3b"), OSLIB_glColor3b},
	{OS_TEXT("glColor3bv"), OSLIB_glColor3bv},
	{OS_TEXT("glColor3d"), OSLIB_glColor3d},
	{OS_TEXT("glColor3dv"), OSLIB_glColor3dv},
	{OS_TEXT("glColor3f"), OSLIB_glColor3f},
	{OS_TEXT("glColor3fv"), OSLIB_glColor3fv},
	{OS_TEXT("glColor3i"), OSLIB_glColor3i},
	{OS_TEXT("glColor3iv"), OSLIB_glColor3iv},
	{OS_TEXT("glColor3s"), OSLIB_glColor3s},
	{OS_TEXT("glColor3sv"), OSLIB_glColor3sv},
	{OS_TEXT("glColor3ub"), OSLIB_glColor3ub},
	{OS_TEXT("glColor3ubv"), OSLIB_glColor3ubv},
	{OS_TEXT("glColor3ui"), OSLIB_glColor3ui},
	{OS_TEXT("glColor3uiv"), OSLIB_glColor3uiv},
	{OS_TEXT("glColor3us"), OSLIB_glColor3us},
	{OS_TEXT("glColor3usv"), OSLIB_glColor3usv},
	{OS_TEXT("glColor4b"), OSLIB_glColor4b},
	{OS_TEXT("glColor4bv"), OSLIB_glColor4bv},
	{OS_TEXT("glColor4d"), OSLIB_glColor4d},
	{OS_TEXT("glColor4dv"), OSLIB_glColor4dv},
	{OS_TEXT("glColor4f"), OSLIB_glColor4f},
	{OS_TEXT("glColor4fv"), OSLIB_glColor4fv},
	{OS_TEXT("glColor4i"), OSLIB_glColor4i},
	{OS_TEXT("glColor4iv"), OSLIB_glColor4iv},
	{OS_TEXT("glColor4s"), OSLIB_glColor4s},
	{OS_TEXT("glColor4sv"), OSLIB_glColor4sv},
	{OS_TEXT("glColor4ub"), OSLIB_glColor4ub},
	{OS_TEXT("glColor4ubv"), OSLIB_glColor4ubv},
	{OS_TEXT("glColor4ui"), OSLIB_glColor4ui},
	{OS_TEXT("glColor4uiv"), OSLIB_glColor4uiv},
	{OS_TEXT("glColor4us"), OSLIB_glColor4us},
	{OS_TEXT("glColor4usv"), OSLIB_glColor4usv},
	{OS_TEXT("glColorMask"), OSLIB_glColorMask},
	{OS_TEXT("glColorMaterial"), OSLIB_glColorMaterial},
	{OS_TEXT("glCopyPixels"), OSLIB_glCopyPixels},
	{OS_TEXT("glCopyTexImage1D"), OSLIB_glCopyTexImage1D},
	{OS_TEXT("glCopyTexImage2D"), OSLIB_glCopyTexImage2D},
	{OS_TEXT("glCopyTexSubImage1D"), OSLIB_glCopyTexSubImage1D},
	{OS_TEXT("glCopyTexSubImage2D"), OSLIB_glCopyTexSubImage2D},
	{OS_TEXT("glCullFace"), OSLIB_glCullFace},
	{OS_TEXT("glDeleteLists"), OSLIB_glDeleteLists},
	{OS_TEXT("glDepthFunc"), OSLIB_glDepthFunc},
	{OS_TEXT("glDepthMask"), OSLIB_glDepthMask},
	{OS_TEXT("glDepthRange"), OSLIB_glDepthRange},
	{OS_TEXT("glDisable"), OSLIB_glDisable},
	{OS_TEXT("glDisableClientState"), OSLIB_glDisableClientState},
	{OS_TEXT("glDrawArrays"), OSLIB_glDrawArrays},
	{OS_TEXT("glDrawBuffer"), OSLIB_glDrawBuffer},
	{OS_TEXT("glEdgeFlag"), OSLIB_glEdgeFlag},
	{OS_TEXT("glEnable"), OSLIB_glEnable},
	{OS_TEXT("glEnableClientState"), OSLIB_glEnableClientState},
	{OS_TEXT("glEnd"), OSLIB_glEnd},
	{OS_TEXT("glEndList"), OSLIB_glEndList},
	{OS_TEXT("glEvalCoord1d"), OSLIB_glEvalCoord1d},
	{OS_TEXT("glEvalCoord1dv"), OSLIB_glEvalCoord1dv},
	{OS_TEXT("glEvalCoord1f"), OSLIB_glEvalCoord1f},
	{OS_TEXT("glEvalCoord1fv"), OSLIB_glEvalCoord1fv},
	{OS_TEXT("glEvalCoord2d"), OSLIB_glEvalCoord2d},
	{OS_TEXT("glEvalCoord2dv"), OSLIB_glEvalCoord2dv},
	{OS_TEXT("glEvalCoord2f"), OSLIB_glEvalCoord2f},
	{OS_TEXT("glEvalCoord2fv"), OSLIB_glEvalCoord2fv},
	{OS_TEXT("glEvalMesh1"), OSLIB_glEvalMesh1},
	{OS_TEXT("glEvalMesh2"), OSLIB_glEvalMesh2},
	{OS_TEXT("glEvalPoint1"), OSLIB_glEvalPoint1},
	{OS_TEXT("glEvalPoint2"), OSLIB_glEvalPoint2},
	{OS_TEXT("glFinish"), OSLIB_glFinish},
	{OS_TEXT("glFlush"), OSLIB_glFlush},
	{OS_TEXT("glFogf"), OSLIB_glFogf},
	{OS_TEXT("glFogfv"), OSLIB_glFogfv},
	{OS_TEXT("glFogi"), OSLIB_glFogi},
	{OS_TEXT("glFogiv"), OSLIB_glFogiv},
	{OS_TEXT("glFrontFace"), OSLIB_glFrontFace},
	{OS_TEXT("glFrustum"), OSLIB_glFrustum},
	{OS_TEXT("glGenLists"), OSLIB_glGenLists},
	{OS_TEXT("glGetError"), OSLIB_glGetError},
	{OS_TEXT("glHint"), OSLIB_glHint},
	{OS_TEXT("glIndexMask"), OSLIB_glIndexMask},
	{OS_TEXT("glIndexd"), OSLIB_glIndexd},
	{OS_TEXT("glIndexdv"), OSLIB_glIndexdv},
	{OS_TEXT("glIndexf"), OSLIB_glIndexf},
	{OS_TEXT("glIndexfv"), OSLIB_glIndexfv},
	{OS_TEXT("glIndexi"), OSLIB_glIndexi},
	{OS_TEXT("glIndexiv"), OSLIB_glIndexiv},
	{OS_TEXT("glIndexs"), OSLIB_glIndexs},
	{OS_TEXT("glIndexsv"), OSLIB_glIndexsv},
	{OS_TEXT("glIndexub"), OSLIB_glIndexub},
	{OS_TEXT("glIndexubv"), OSLIB_glIndexubv},
	{OS_TEXT("glInitNames"), OSLIB_glInitNames},
	{OS_TEXT("glIsEnabled"), OSLIB_glIsEnabled},
	{OS_TEXT("glIsList"), OSLIB_glIsList},
	{OS_TEXT("glIsTexture"), OSLIB_glIsTexture},
	{OS_TEXT("glLightModelf"), OSLIB_glLightModelf},
	{OS_TEXT("glLightModelfv"), OSLIB_glLightModelfv},
	{OS_TEXT("glLightModeli"), OSLIB_glLightModeli},
	{OS_TEXT("glLightModeliv"), OSLIB_glLightModeliv},
	{OS_TEXT("glLightf"), OSLIB_glLightf},
	{OS_TEXT("glLightfv"), OSLIB_glLightfv},
	{OS_TEXT("glLighti"), OSLIB_glLighti},
	{OS_TEXT("glLightiv"), OSLIB_glLightiv},
	{OS_TEXT("glLineStipple"), OSLIB_glLineStipple},
	{OS_TEXT("glLineWidth"), OSLIB_glLineWidth},
	{OS_TEXT("glListBase"), OSLIB_glListBase},
	{OS_TEXT("glLoadIdentity"), OSLIB_glLoadIdentity},
	{OS_TEXT("glLoadMatrixd"), OSLIB_glLoadMatrixd},
	{OS_TEXT("glLoadMatrixf"), OSLIB_glLoadMatrixf},
	{OS_TEXT("glLoadName"), OSLIB_glLoadName},
	{OS_TEXT("glLogicOp"), OSLIB_glLogicOp},
	{OS_TEXT("glMap1d"), OSLIB_glMap1d},
	{OS_TEXT("glMap1f"), OSLIB_glMap1f},
	{OS_TEXT("glMap2d"), OSLIB_glMap2d},
	{OS_TEXT("glMap2f"), OSLIB_glMap2f},
	{OS_TEXT("glMapGrid1d"), OSLIB_glMapGrid1d},
	{OS_TEXT("glMapGrid1f"), OSLIB_glMapGrid1f},
	{OS_TEXT("glMapGrid2d"), OSLIB_glMapGrid2d},
	{OS_TEXT("glMapGrid2f"), OSLIB_glMapGrid2f},
	{OS_TEXT("glMaterialf"), OSLIB_glMaterialf},
	{OS_TEXT("glMaterialfv"), OSLIB_glMaterialfv},
	{OS_TEXT("glMateriali"), OSLIB_glMateriali},
	{OS_TEXT("glMaterialiv"), OSLIB_glMaterialiv},
	{OS_TEXT("glMatrixMode"), OSLIB_glMatrixMode},
	{OS_TEXT("glNewList"), OSLIB_glNewList},
	{OS_TEXT("glNormal3b"), OSLIB_glNormal3b},
	{OS_TEXT("glNormal3bv"), OSLIB_glNormal3bv},
	{OS_TEXT("glNormal3d"), OSLIB_glNormal3d},
	{OS_TEXT("glNormal3dv"), OSLIB_glNormal3dv},
	{OS_TEXT("glNormal3f"), OSLIB_glNormal3f},
	{OS_TEXT("glNormal3fv"), OSLIB_glNormal3fv},
	{OS_TEXT("glNormal3i"), OSLIB_glNormal3i},
	{OS_TEXT("glNormal3iv"), OSLIB_glNormal3iv},
	{OS_TEXT("glNormal3s"), OSLIB_glNormal3s},
	{OS_TEXT("glNormal3sv"), OSLIB_glNormal3sv},
	{OS_TEXT("glOrtho"), OSLIB_glOrtho},
	{OS_TEXT("glPassThrough"), OSLIB_glPassThrough},
	{OS_TEXT("glPixelMapfv"), OSLIB_glPixelMapfv},
	{OS_TEXT("glPixelMapuiv"), OSLIB_glPixelMapuiv},
	{OS_TEXT("glPixelMapusv"), OSLIB_glPixelMapusv},
	{OS_TEXT("glPixelStoref"), OSLIB_glPixelStoref},
	{OS_TEXT("glPixelStorei"), OSLIB_glPixelStorei},
	{OS_TEXT("glPixelTransferf"), OSLIB_glPixelTransferf},
	{OS_TEXT("glPixelTransferi"), OSLIB_glPixelTransferi},
	{OS_TEXT("glPixelZoom"), OSLIB_glPixelZoom},
	{OS_TEXT("glPointSize"), OSLIB_glPointSize},
	{OS_TEXT("glPolygonMode"), OSLIB_glPolygonMode},
	{OS_TEXT("glPolygonOffset"), OSLIB_glPolygonOffset},
	{OS_TEXT("glPopAttrib"), OSLIB_glPopAttrib},
	{OS_TEXT("glPopClientAttrib"), OSLIB_glPopClientAttrib},
	{OS_TEXT("glPopMatrix"), OSLIB_glPopMatrix},
	{OS_TEXT("glPopName"), OSLIB_glPopName},
	{OS_TEXT("glPushAttrib"), OSLIB_glPushAttrib},
	{OS_TEXT("glPushClientAttrib"), OSLIB_glPushClientAttrib},
	{OS_TEXT("glPushMatrix"), OSLIB_glPushMatrix},
	{OS_TEXT("glPushName"), OSLIB_glPushName},
	{OS_TEXT("glRasterPos2d"), OSLIB_glRasterPos2d},
	{OS_TEXT("glRasterPos2dv"), OSLIB_glRasterPos2dv},
	{OS_TEXT("glRasterPos2f"), OSLIB_glRasterPos2f},
	{OS_TEXT("glRasterPos2fv"), OSLIB_glRasterPos2fv},
	{OS_TEXT("glRasterPos2i"), OSLIB_glRasterPos2i},
	{OS_TEXT("glRasterPos2iv"), OSLIB_glRasterPos2iv},
	{OS_TEXT("glRasterPos2s"), OSLIB_glRasterPos2s},
	{OS_TEXT("glRasterPos2sv"), OSLIB_glRasterPos2sv},
	{OS_TEXT("glRasterPos3d"), OSLIB_glRasterPos3d},
	{OS_TEXT("glRasterPos3dv"), OSLIB_glRasterPos3dv},
	{OS_TEXT("glRasterPos3f"), OSLIB_glRasterPos3f},
	{OS_TEXT("glRasterPos3fv"), OSLIB_glRasterPos3fv},
	{OS_TEXT("glRasterPos3i"), OSLIB_glRasterPos3i},
	{OS_TEXT("glRasterPos3iv"), OSLIB_glRasterPos3iv},
	{OS_TEXT("glRasterPos3s"), OSLIB_glRasterPos3s},
	{OS_TEXT("glRasterPos3sv"), OSLIB_glRasterPos3sv},
	{OS_TEXT("glRasterPos4d"), OSLIB_glRasterPos4d},
	{OS_TEXT("glRasterPos4dv"), OSLIB_glRasterPos4dv},
	{OS_TEXT("glRasterPos4f"), OSLIB_glRasterPos4f},
	{OS_TEXT("glRasterPos4fv"), OSLIB_glRasterPos4fv},
	{OS_TEXT("glRasterPos4i"), OSLIB_glRasterPos4i},
	{OS_TEXT("glRasterPos4iv"), OSLIB_glRasterPos4iv},
	{OS_TEXT("glRasterPos4s"), OSLIB_glRasterPos4s},
	{OS_TEXT("glRasterPos4sv"), OSLIB_glRasterPos4sv},
	{OS_TEXT("glReadBuffer"), OSLIB_glReadBuffer},
	{OS_TEXT("glRectd"), OSLIB_glRectd},
	{OS_TEXT("glRectdv"), OSLIB_glRectdv},
	{OS_TEXT("glRectf"), OSLIB_glRectf},
	{OS_TEXT("glRectfv"), OSLIB_glRectfv},
	{OS_TEXT("glRecti"), OSLIB_glRecti},
	{OS_TEXT("glRectiv"), OSLIB_glRectiv},
	{OS_TEXT("glRects"), OSLIB_glRects},
	{OS_TEXT("glRectsv"), OSLIB_glRectsv},
	{OS_TEXT("glRenderMode"), OSLIB_glRenderMode},
	{OS_TEXT("glRotated"), OSLIB_glRotated},
	{OS_TEXT("glRotatef"), OSLIB_glRotatef},
	{OS_TEXT("glScaled"), OSLIB_glScaled},
	{OS_TEXT("glScalef"), OSLIB_glScalef},
	{OS_TEXT("glScissor"), OSLIB_glScissor},
	{OS_TEXT("glShadeModel"), OSLIB_glShadeModel},
	{OS_TEXT("glStencilFunc"), OSLIB_glStencilFunc},
	{OS_TEXT("glStencilMask"), OSLIB_glStencilMask},
	{OS_TEXT("glStencilOp"), OSLIB_glStencilOp},
	{OS_TEXT("glTexCoord1d"), OSLIB_glTexCoord1d},
	{OS_TEXT("glTexCoord1dv"), OSLIB_glTexCoord1dv},
	{OS_TEXT("glTexCoord1f"), OSLIB_glTexCoord1f},
	{OS_TEXT("glTexCoord1fv"), OSLIB_glTexCoord1fv},
	{OS_TEXT("glTexCoord1i"), OSLIB_glTexCoord1i},
	{OS_TEXT("glTexCoord1iv"), OSLIB_glTexCoord1iv},
	{OS_TEXT("glTexCoord1s"), OSLIB_glTexCoord1s},
	{OS_TEXT("glTexCoord1sv"), OSLIB_glTexCoord1sv},
	{OS_TEXT("glTexCoord2d"), OSLIB_glTexCoord2d},
	{OS_TEXT("glTexCoord2dv"), OSLIB_glTexCoord2dv},
	{OS_TEXT("glTexCoord2f"), OSLIB_glTexCoord2f},
	{OS_TEXT("glTexCoord2fv"), OSLIB_glTexCoord2fv},
	{OS_TEXT("glTexCoord2i"), OSLIB_glTexCoord2i},
	{OS_TEXT("glTexCoord2iv"), OSLIB_glTexCoord2iv},
	{OS_TEXT("glTexCoord2s"), OSLIB_glTexCoord2s},
	{OS_TEXT("glTexCoord2sv"), OSLIB_glTexCoord2sv},
	{OS_TEXT("glTexCoord3d"), OSLIB_glTexCoord3d},
	{OS_TEXT("glTexCoord3dv"), OSLIB_glTexCoord3dv},
	{OS_TEXT("glTexCoord3f"), OSLIB_glTexCoord3f},
	{OS_TEXT("glTexCoord3fv"), OSLIB_glTexCoord3fv},
	{OS_TEXT("glTexCoord3i"), OSLIB_glTexCoord3i},
	{OS_TEXT("glTexCoord3iv"), OSLIB_glTexCoord3iv},
	{OS_TEXT("glTexCoord3s"), OSLIB_glTexCoord3s},
	{OS_TEXT("glTexCoord3sv"), OSLIB_glTexCoord3sv},
	{OS_TEXT("glTexCoord4d"), OSLIB_glTexCoord4d},
	{OS_TEXT("glTexCoord4dv"), OSLIB_glTexCoord4dv},
	{OS_TEXT("glTexCoord4f"), OSLIB_glTexCoord4f},
	{OS_TEXT("glTexCoord4fv"), OSLIB_glTexCoord4fv},
	{OS_TEXT("glTexCoord4i"), OSLIB_glTexCoord4i},
	{OS_TEXT("glTexCoord4iv"), OSLIB_glTexCoord4iv},
	{OS_TEXT("glTexCoord4s"), OSLIB_glTexCoord4s},
	{OS_TEXT("glTexCoord4sv"), OSLIB_glTexCoord4sv},
	{OS_TEXT("glTexEnvf"), OSLIB_glTexEnvf},
	{OS_TEXT("glTexEnvfv"), OSLIB_glTexEnvfv},
	{OS_TEXT("glTexEnvi"), OSLIB_glTexEnvi},
	{OS_TEXT("glTexEnviv"), OSLIB_glTexEnviv},
	{OS_TEXT("glTexGend"), OSLIB_glTexGend},
	{OS_TEXT("glTexGendv"), OSLIB_glTexGendv},
	{OS_TEXT("glTexGenf"), OSLIB_glTexGenf},
	{OS_TEXT("glTexGenfv"), OSLIB_glTexGenfv},
	{OS_TEXT("glTexGeni"), OSLIB_glTexGeni},
	{OS_TEXT("glTexGeniv"), OSLIB_glTexGeniv},
	{OS_TEXT("glTexParameterf"), OSLIB_glTexParameterf},
	{OS_TEXT("glTexParameterfv"), OSLIB_glTexParameterfv},
	{OS_TEXT("glTexParameteri"), OSLIB_glTexParameteri},
	{OS_TEXT("glTexParameteriv"), OSLIB_glTexParameteriv},
	{OS_TEXT("glTranslated"), OSLIB_glTranslated},
	{OS_TEXT("glTranslatef"), OSLIB_glTranslatef},
	{OS_TEXT("glVertex2d"), OSLIB_glVertex2d},
	{OS_TEXT("glVertex2dv"), OSLIB_glVertex2dv},
	{OS_TEXT("glVertex2f"), OSLIB_glVertex2f},
	{OS_TEXT("glVertex2fv"), OSLIB_glVertex2fv},
	{OS_TEXT("glVertex2i"), OSLIB_glVertex2i},
	{OS_TEXT("glVertex2iv"), OSLIB_glVertex2iv},
	{OS_TEXT("glVertex2s"), OSLIB_glVertex2s},
	{OS_TEXT("glVertex2sv"), OSLIB_glVertex2sv},
	{OS_TEXT("glVertex3d"), OSLIB_glVertex3d},
	{OS_TEXT("glVertex3dv"), OSLIB_glVertex3dv},
	{OS_TEXT("glVertex3f"), OSLIB_glVertex3f},
	{OS_TEXT("glVertex3fv"), OSLIB_glVertex3fv},
	{OS_TEXT("glVertex3i"), OSLIB_glVertex3i},
	{OS_TEXT("glVertex3iv"), OSLIB_glVertex3iv},
	{OS_TEXT("glVertex3s"), OSLIB_glVertex3s},
	{OS_TEXT("glVertex3sv"), OSLIB_glVertex3sv},
	{OS_TEXT("glVertex4d"), OSLIB_glVertex4d},
	{OS_TEXT("glVertex4dv"), OSLIB_glVertex4dv},
	{OS_TEXT("glVertex4f"), OSLIB_glVertex4f},
	{OS_TEXT("glVertex4fv"), OSLIB_glVertex4fv},
	{OS_TEXT("glVertex4i"), OSLIB_glVertex4i},
	{OS_TEXT("glVertex4iv"), OSLIB_glVertex4iv},
	{OS_TEXT("glVertex4s"), OSLIB_glVertex4s},
	{OS_TEXT("glVertex4sv"), OSLIB_glVertex4sv},
	{OS_TEXT("glViewport"), OSLIB_glViewport},
	{OS_TEXT("gluErrorStringWIN"), OSLIB_gluErrorStringWIN},
	{OS_TEXT("gluOrtho2D"), OSLIB_gluOrtho2D},
	{OS_TEXT("gluPerspective"), OSLIB_gluPerspective},
	{OS_TEXT("gluLookAt"), OSLIB_gluLookAt},
	{OS_TEXT("glutInitDisplayMode"), OSLIB_glutInitDisplayMode},
	{OS_TEXT("glutInitDisplayString"), OSLIB_glutInitDisplayString},
	{OS_TEXT("glutInitWindowPosition"), OSLIB_glutInitWindowPosition},
	{OS_TEXT("glutInitWindowSize"), OSLIB_glutInitWindowSize},
	{OS_TEXT("glutMainLoop"), OSLIB_glutMainLoop},
	{OS_TEXT("glutCreateWindow"), OSLIB_glutCreateWindow},
	{OS_TEXT("glutCreateSubWindow"), OSLIB_glutCreateSubWindow},
	{OS_TEXT("glutDestroyWindow"), OSLIB_glutDestroyWindow},
	{OS_TEXT("glutPostRedisplay"), OSLIB_glutPostRedisplay},
	{OS_TEXT("glutPostWindowRedisplay"), OSLIB_glutPostWindowRedisplay},
	{OS_TEXT("glutSwapBuffers"), OSLIB_glutSwapBuffers},
	{OS_TEXT("glutGetWindow"), OSLIB_glutGetWindow},
	{OS_TEXT("glutSetWindow"), OSLIB_glutSetWindow},
	{OS_TEXT("glutSetWindowTitle"), OSLIB_glutSetWindowTitle},
	{OS_TEXT("glutSetIconTitle"), OSLIB_glutSetIconTitle},
	{OS_TEXT("glutPositionWindow"), OSLIB_glutPositionWindow},
	{OS_TEXT("glutReshapeWindow"), OSLIB_glutReshapeWindow},
	{OS_TEXT("glutPopWindow"), OSLIB_glutPopWindow},
	{OS_TEXT("glutPushWindow"), OSLIB_glutPushWindow},
	{OS_TEXT("glutIconifyWindow"), OSLIB_glutIconifyWindow},
	{OS_TEXT("glutShowWindow"), OSLIB_glutShowWindow},
	{OS_TEXT("glutHideWindow"), OSLIB_glutHideWindow},
	{OS_TEXT("glutFullScreen"), OSLIB_glutFullScreen},
	{OS_TEXT("glutSetCursor"), OSLIB_glutSetCursor},
	{OS_TEXT("glutWarpPointer"), OSLIB_glutWarpPointer},
	{OS_TEXT("glutEstablishOverlay"), OSLIB_glutEstablishOverlay},
	{OS_TEXT("glutRemoveOverlay"), OSLIB_glutRemoveOverlay},
	{OS_TEXT("glutUseLayer"), OSLIB_glutUseLayer},
	{OS_TEXT("glutPostOverlayRedisplay"), OSLIB_glutPostOverlayRedisplay},
	{OS_TEXT("glutPostWindowOverlayRedisplay"), OSLIB_glutPostWindowOverlayRedisplay},
	{OS_TEXT("glutShowOverlay"), OSLIB_glutShowOverlay},
	{OS_TEXT("glutHideOverlay"), OSLIB_glutHideOverlay},
	{OS_TEXT("glutDestroyMenu"), OSLIB_glutDestroyMenu},
	{OS_TEXT("glutGetMenu"), OSLIB_glutGetMenu},
	{OS_TEXT("glutSetMenu"), OSLIB_glutSetMenu},
	{OS_TEXT("glutAddMenuEntry"), OSLIB_glutAddMenuEntry},
	{OS_TEXT("glutAddSubMenu"), OSLIB_glutAddSubMenu},
	{OS_TEXT("glutChangeToMenuEntry"), OSLIB_glutChangeToMenuEntry},
	{OS_TEXT("glutChangeToSubMenu"), OSLIB_glutChangeToSubMenu},
	{OS_TEXT("glutRemoveMenuItem"), OSLIB_glutRemoveMenuItem},
	{OS_TEXT("glutAttachMenu"), OSLIB_glutAttachMenu},
	{OS_TEXT("glutDetachMenu"), OSLIB_glutDetachMenu},
	{OS_TEXT("glutGetColor"), OSLIB_glutGetColor},
	{OS_TEXT("glutCopyColormap"), OSLIB_glutCopyColormap},
	{OS_TEXT("glutGet"), OSLIB_glutGet},
	{OS_TEXT("glutDeviceGet"), OSLIB_glutDeviceGet},
	{OS_TEXT("glutExtensionSupported"), OSLIB_glutExtensionSupported},
	{OS_TEXT("glutGetModifiers"), OSLIB_glutGetModifiers},
	{OS_TEXT("glutLayerGet"), OSLIB_glutLayerGet},
	{OS_TEXT("glutWireSphere"), OSLIB_glutWireSphere},
	{OS_TEXT("glutSolidSphere"), OSLIB_glutSolidSphere},
	{OS_TEXT("glutWireCone"), OSLIB_glutWireCone},
	{OS_TEXT("glutSolidCone"), OSLIB_glutSolidCone},
	{OS_TEXT("glutWireCube"), OSLIB_glutWireCube},
	{OS_TEXT("glutSolidCube"), OSLIB_glutSolidCube},
	{OS_TEXT("glutWireTorus"), OSLIB_glutWireTorus},
	{OS_TEXT("glutSolidTorus"), OSLIB_glutSolidTorus},
	{OS_TEXT("glutWireDodecahedron"), OSLIB_glutWireDodecahedron},
	{OS_TEXT("glutSolidDodecahedron"), OSLIB_glutSolidDodecahedron},
	{OS_TEXT("glutWireTeapot"), OSLIB_glutWireTeapot},
	{OS_TEXT("glutSolidTeapot"), OSLIB_glutSolidTeapot},
	{OS_TEXT("glutWireOctahedron"), OSLIB_glutWireOctahedron},
	{OS_TEXT("glutSolidOctahedron"), OSLIB_glutSolidOctahedron},
	{OS_TEXT("glutWireTetrahedron"), OSLIB_glutWireTetrahedron},
	{OS_TEXT("glutSolidTetrahedron"), OSLIB_glutSolidTetrahedron},
	{OS_TEXT("glutWireIcosahedron"), OSLIB_glutWireIcosahedron},
	{OS_TEXT("glutSolidIcosahedron"), OSLIB_glutSolidIcosahedron},
	{OS_TEXT("glutVideoResizeGet"), OSLIB_glutVideoResizeGet},
	{OS_TEXT("glutSetupVideoResizing"), OSLIB_glutSetupVideoResizing},
	{OS_TEXT("glutStopVideoResizing"), OSLIB_glutStopVideoResizing},
	{OS_TEXT("glutVideoResize"), OSLIB_glutVideoResize},
	{OS_TEXT("glutVideoPan"), OSLIB_glutVideoPan},
	{OS_TEXT("glutReportErrors"), OSLIB_glutReportErrors},
	{OS_TEXT("glutIgnoreKeyRepeat"), OSLIB_glutIgnoreKeyRepeat},
	{OS_TEXT("glutSetKeyRepeat"), OSLIB_glutSetKeyRepeat},
	{OS_TEXT("glutGameModeString"), OSLIB_glutGameModeString},
	{OS_TEXT("glutEnterGameMode"), OSLIB_glutEnterGameMode},
	{OS_TEXT("glutLeaveGameMode"), OSLIB_glutLeaveGameMode},
	{OS_TEXT("glutGameModeGet"), OSLIB_glutGameModeGet},
	{},
};
static OS::NumberDef oslib_gl_numbers[] = {
	{OS_TEXT("GL_VERSION_1_1"), GL_VERSION_1_1},
	{OS_TEXT("GL_ACCUM"), GL_ACCUM},
	{OS_TEXT("GL_LOAD"), GL_LOAD},
	{OS_TEXT("GL_RETURN"), GL_RETURN},
	{OS_TEXT("GL_MULT"), GL_MULT},
	{OS_TEXT("GL_ADD"), GL_ADD},
	{OS_TEXT("GL_NEVER"), GL_NEVER},
	{OS_TEXT("GL_LESS"), GL_LESS},
	{OS_TEXT("GL_EQUAL"), GL_EQUAL},
	{OS_TEXT("GL_LEQUAL"), GL_LEQUAL},
	{OS_TEXT("GL_GREATER"), GL_GREATER},
	{OS_TEXT("GL_NOTEQUAL"), GL_NOTEQUAL},
	{OS_TEXT("GL_GEQUAL"), GL_GEQUAL},
	{OS_TEXT("GL_ALWAYS"), GL_ALWAYS},
	{OS_TEXT("GL_CURRENT_BIT"), GL_CURRENT_BIT},
	{OS_TEXT("GL_POINT_BIT"), GL_POINT_BIT},
	{OS_TEXT("GL_LINE_BIT"), GL_LINE_BIT},
	{OS_TEXT("GL_POLYGON_BIT"), GL_POLYGON_BIT},
	{OS_TEXT("GL_POLYGON_STIPPLE_BIT"), GL_POLYGON_STIPPLE_BIT},
	{OS_TEXT("GL_PIXEL_MODE_BIT"), GL_PIXEL_MODE_BIT},
	{OS_TEXT("GL_LIGHTING_BIT"), GL_LIGHTING_BIT},
	{OS_TEXT("GL_FOG_BIT"), GL_FOG_BIT},
	{OS_TEXT("GL_DEPTH_BUFFER_BIT"), GL_DEPTH_BUFFER_BIT},
	{OS_TEXT("GL_ACCUM_BUFFER_BIT"), GL_ACCUM_BUFFER_BIT},
	{OS_TEXT("GL_STENCIL_BUFFER_BIT"), GL_STENCIL_BUFFER_BIT},
	{OS_TEXT("GL_VIEWPORT_BIT"), GL_VIEWPORT_BIT},
	{OS_TEXT("GL_TRANSFORM_BIT"), GL_TRANSFORM_BIT},
	{OS_TEXT("GL_ENABLE_BIT"), GL_ENABLE_BIT},
	{OS_TEXT("GL_COLOR_BUFFER_BIT"), GL_COLOR_BUFFER_BIT},
	{OS_TEXT("GL_HINT_BIT"), GL_HINT_BIT},
	{OS_TEXT("GL_EVAL_BIT"), GL_EVAL_BIT},
	{OS_TEXT("GL_LIST_BIT"), GL_LIST_BIT},
	{OS_TEXT("GL_TEXTURE_BIT"), GL_TEXTURE_BIT},
	{OS_TEXT("GL_SCISSOR_BIT"), GL_SCISSOR_BIT},
	{OS_TEXT("GL_ALL_ATTRIB_BITS"), GL_ALL_ATTRIB_BITS},
	{OS_TEXT("GL_POINTS"), GL_POINTS},
	{OS_TEXT("GL_LINES"), GL_LINES},
	{OS_TEXT("GL_LINE_LOOP"), GL_LINE_LOOP},
	{OS_TEXT("GL_LINE_STRIP"), GL_LINE_STRIP},
	{OS_TEXT("GL_TRIANGLES"), GL_TRIANGLES},
	{OS_TEXT("GL_TRIANGLE_STRIP"), GL_TRIANGLE_STRIP},
	{OS_TEXT("GL_TRIANGLE_FAN"), GL_TRIANGLE_FAN},
	{OS_TEXT("GL_QUADS"), GL_QUADS},
	{OS_TEXT("GL_QUAD_STRIP"), GL_QUAD_STRIP},
	{OS_TEXT("GL_POLYGON"), GL_POLYGON},
	{OS_TEXT("GL_ZERO"), GL_ZERO},
	{OS_TEXT("GL_ONE"), GL_ONE},
	{OS_TEXT("GL_SRC_COLOR"), GL_SRC_COLOR},
	{OS_TEXT("GL_ONE_MINUS_SRC_COLOR"), GL_ONE_MINUS_SRC_COLOR},
	{OS_TEXT("GL_SRC_ALPHA"), GL_SRC_ALPHA},
	{OS_TEXT("GL_ONE_MINUS_SRC_ALPHA"), GL_ONE_MINUS_SRC_ALPHA},
	{OS_TEXT("GL_DST_ALPHA"), GL_DST_ALPHA},
	{OS_TEXT("GL_ONE_MINUS_DST_ALPHA"), GL_ONE_MINUS_DST_ALPHA},
	{OS_TEXT("GL_DST_COLOR"), GL_DST_COLOR},
	{OS_TEXT("GL_ONE_MINUS_DST_COLOR"), GL_ONE_MINUS_DST_COLOR},
	{OS_TEXT("GL_SRC_ALPHA_SATURATE"), GL_SRC_ALPHA_SATURATE},
	{OS_TEXT("GL_TRUE"), GL_TRUE},
	{OS_TEXT("GL_FALSE"), GL_FALSE},
	{OS_TEXT("GL_CLIP_PLANE0"), GL_CLIP_PLANE0},
	{OS_TEXT("GL_CLIP_PLANE1"), GL_CLIP_PLANE1},
	{OS_TEXT("GL_CLIP_PLANE2"), GL_CLIP_PLANE2},
	{OS_TEXT("GL_CLIP_PLANE3"), GL_CLIP_PLANE3},
	{OS_TEXT("GL_CLIP_PLANE4"), GL_CLIP_PLANE4},
	{OS_TEXT("GL_CLIP_PLANE5"), GL_CLIP_PLANE5},
	{OS_TEXT("GL_BYTE"), GL_BYTE},
	{OS_TEXT("GL_UNSIGNED_BYTE"), GL_UNSIGNED_BYTE},
	{OS_TEXT("GL_SHORT"), GL_SHORT},
	{OS_TEXT("GL_UNSIGNED_SHORT"), GL_UNSIGNED_SHORT},
	{OS_TEXT("GL_INT"), GL_INT},
	{OS_TEXT("GL_UNSIGNED_INT"), GL_UNSIGNED_INT},
	{OS_TEXT("GL_FLOAT"), GL_FLOAT},
	{OS_TEXT("GL_2_BYTES"), GL_2_BYTES},
	{OS_TEXT("GL_3_BYTES"), GL_3_BYTES},
	{OS_TEXT("GL_4_BYTES"), GL_4_BYTES},
	{OS_TEXT("GL_DOUBLE"), GL_DOUBLE},
	{OS_TEXT("GL_NONE"), GL_NONE},
	{OS_TEXT("GL_FRONT_LEFT"), GL_FRONT_LEFT},
	{OS_TEXT("GL_FRONT_RIGHT"), GL_FRONT_RIGHT},
	{OS_TEXT("GL_BACK_LEFT"), GL_BACK_LEFT},
	{OS_TEXT("GL_BACK_RIGHT"), GL_BACK_RIGHT},
	{OS_TEXT("GL_FRONT"), GL_FRONT},
	{OS_TEXT("GL_BACK"), GL_BACK},
	{OS_TEXT("GL_LEFT"), GL_LEFT},
	{OS_TEXT("GL_RIGHT"), GL_RIGHT},
	{OS_TEXT("GL_FRONT_AND_BACK"), GL_FRONT_AND_BACK},
	{OS_TEXT("GL_AUX0"), GL_AUX0},
	{OS_TEXT("GL_AUX1"), GL_AUX1},
	{OS_TEXT("GL_AUX2"), GL_AUX2},
	{OS_TEXT("GL_AUX3"), GL_AUX3},
	{OS_TEXT("GL_NO_ERROR"), GL_NO_ERROR},
	{OS_TEXT("GL_INVALID_ENUM"), GL_INVALID_ENUM},
	{OS_TEXT("GL_INVALID_VALUE"), GL_INVALID_VALUE},
	{OS_TEXT("GL_INVALID_OPERATION"), GL_INVALID_OPERATION},
	{OS_TEXT("GL_STACK_OVERFLOW"), GL_STACK_OVERFLOW},
	{OS_TEXT("GL_STACK_UNDERFLOW"), GL_STACK_UNDERFLOW},
	{OS_TEXT("GL_OUT_OF_MEMORY"), GL_OUT_OF_MEMORY},
	{OS_TEXT("GL_2D"), GL_2D},
	{OS_TEXT("GL_3D"), GL_3D},
	{OS_TEXT("GL_3D_COLOR"), GL_3D_COLOR},
	{OS_TEXT("GL_3D_COLOR_TEXTURE"), GL_3D_COLOR_TEXTURE},
	{OS_TEXT("GL_4D_COLOR_TEXTURE"), GL_4D_COLOR_TEXTURE},
	{OS_TEXT("GL_PASS_THROUGH_TOKEN"), GL_PASS_THROUGH_TOKEN},
	{OS_TEXT("GL_POINT_TOKEN"), GL_POINT_TOKEN},
	{OS_TEXT("GL_LINE_TOKEN"), GL_LINE_TOKEN},
	{OS_TEXT("GL_POLYGON_TOKEN"), GL_POLYGON_TOKEN},
	{OS_TEXT("GL_BITMAP_TOKEN"), GL_BITMAP_TOKEN},
	{OS_TEXT("GL_DRAW_PIXEL_TOKEN"), GL_DRAW_PIXEL_TOKEN},
	{OS_TEXT("GL_COPY_PIXEL_TOKEN"), GL_COPY_PIXEL_TOKEN},
	{OS_TEXT("GL_LINE_RESET_TOKEN"), GL_LINE_RESET_TOKEN},
	{OS_TEXT("GL_EXP"), GL_EXP},
	{OS_TEXT("GL_EXP2"), GL_EXP2},
	{OS_TEXT("GL_CW"), GL_CW},
	{OS_TEXT("GL_CCW"), GL_CCW},
	{OS_TEXT("GL_COEFF"), GL_COEFF},
	{OS_TEXT("GL_ORDER"), GL_ORDER},
	{OS_TEXT("GL_DOMAIN"), GL_DOMAIN},
	{OS_TEXT("GL_CURRENT_COLOR"), GL_CURRENT_COLOR},
	{OS_TEXT("GL_CURRENT_INDEX"), GL_CURRENT_INDEX},
	{OS_TEXT("GL_CURRENT_NORMAL"), GL_CURRENT_NORMAL},
	{OS_TEXT("GL_CURRENT_TEXTURE_COORDS"), GL_CURRENT_TEXTURE_COORDS},
	{OS_TEXT("GL_CURRENT_RASTER_COLOR"), GL_CURRENT_RASTER_COLOR},
	{OS_TEXT("GL_CURRENT_RASTER_INDEX"), GL_CURRENT_RASTER_INDEX},
	{OS_TEXT("GL_CURRENT_RASTER_TEXTURE_COORDS"), GL_CURRENT_RASTER_TEXTURE_COORDS},
	{OS_TEXT("GL_CURRENT_RASTER_POSITION"), GL_CURRENT_RASTER_POSITION},
	{OS_TEXT("GL_CURRENT_RASTER_POSITION_VALID"), GL_CURRENT_RASTER_POSITION_VALID},
	{OS_TEXT("GL_CURRENT_RASTER_DISTANCE"), GL_CURRENT_RASTER_DISTANCE},
	{OS_TEXT("GL_POINT_SMOOTH"), GL_POINT_SMOOTH},
	{OS_TEXT("GL_POINT_SIZE"), GL_POINT_SIZE},
	{OS_TEXT("GL_POINT_SIZE_RANGE"), GL_POINT_SIZE_RANGE},
	{OS_TEXT("GL_POINT_SIZE_GRANULARITY"), GL_POINT_SIZE_GRANULARITY},
	{OS_TEXT("GL_LINE_SMOOTH"), GL_LINE_SMOOTH},
	{OS_TEXT("GL_LINE_WIDTH"), GL_LINE_WIDTH},
	{OS_TEXT("GL_LINE_WIDTH_RANGE"), GL_LINE_WIDTH_RANGE},
	{OS_TEXT("GL_LINE_WIDTH_GRANULARITY"), GL_LINE_WIDTH_GRANULARITY},
	{OS_TEXT("GL_LINE_STIPPLE"), GL_LINE_STIPPLE},
	{OS_TEXT("GL_LINE_STIPPLE_PATTERN"), GL_LINE_STIPPLE_PATTERN},
	{OS_TEXT("GL_LINE_STIPPLE_REPEAT"), GL_LINE_STIPPLE_REPEAT},
	{OS_TEXT("GL_LIST_MODE"), GL_LIST_MODE},
	{OS_TEXT("GL_MAX_LIST_NESTING"), GL_MAX_LIST_NESTING},
	{OS_TEXT("GL_LIST_BASE"), GL_LIST_BASE},
	{OS_TEXT("GL_LIST_INDEX"), GL_LIST_INDEX},
	{OS_TEXT("GL_POLYGON_MODE"), GL_POLYGON_MODE},
	{OS_TEXT("GL_POLYGON_SMOOTH"), GL_POLYGON_SMOOTH},
	{OS_TEXT("GL_POLYGON_STIPPLE"), GL_POLYGON_STIPPLE},
	{OS_TEXT("GL_EDGE_FLAG"), GL_EDGE_FLAG},
	{OS_TEXT("GL_CULL_FACE"), GL_CULL_FACE},
	{OS_TEXT("GL_CULL_FACE_MODE"), GL_CULL_FACE_MODE},
	{OS_TEXT("GL_FRONT_FACE"), GL_FRONT_FACE},
	{OS_TEXT("GL_LIGHTING"), GL_LIGHTING},
	{OS_TEXT("GL_LIGHT_MODEL_LOCAL_VIEWER"), GL_LIGHT_MODEL_LOCAL_VIEWER},
	{OS_TEXT("GL_LIGHT_MODEL_TWO_SIDE"), GL_LIGHT_MODEL_TWO_SIDE},
	{OS_TEXT("GL_LIGHT_MODEL_AMBIENT"), GL_LIGHT_MODEL_AMBIENT},
	{OS_TEXT("GL_SHADE_MODEL"), GL_SHADE_MODEL},
	{OS_TEXT("GL_COLOR_MATERIAL_FACE"), GL_COLOR_MATERIAL_FACE},
	{OS_TEXT("GL_COLOR_MATERIAL_PARAMETER"), GL_COLOR_MATERIAL_PARAMETER},
	{OS_TEXT("GL_COLOR_MATERIAL"), GL_COLOR_MATERIAL},
	{OS_TEXT("GL_FOG"), GL_FOG},
	{OS_TEXT("GL_FOG_INDEX"), GL_FOG_INDEX},
	{OS_TEXT("GL_FOG_DENSITY"), GL_FOG_DENSITY},
	{OS_TEXT("GL_FOG_START"), GL_FOG_START},
	{OS_TEXT("GL_FOG_END"), GL_FOG_END},
	{OS_TEXT("GL_FOG_MODE"), GL_FOG_MODE},
	{OS_TEXT("GL_FOG_COLOR"), GL_FOG_COLOR},
	{OS_TEXT("GL_DEPTH_RANGE"), GL_DEPTH_RANGE},
	{OS_TEXT("GL_DEPTH_TEST"), GL_DEPTH_TEST},
	{OS_TEXT("GL_DEPTH_WRITEMASK"), GL_DEPTH_WRITEMASK},
	{OS_TEXT("GL_DEPTH_CLEAR_VALUE"), GL_DEPTH_CLEAR_VALUE},
	{OS_TEXT("GL_DEPTH_FUNC"), GL_DEPTH_FUNC},
	{OS_TEXT("GL_ACCUM_CLEAR_VALUE"), GL_ACCUM_CLEAR_VALUE},
	{OS_TEXT("GL_STENCIL_TEST"), GL_STENCIL_TEST},
	{OS_TEXT("GL_STENCIL_CLEAR_VALUE"), GL_STENCIL_CLEAR_VALUE},
	{OS_TEXT("GL_STENCIL_FUNC"), GL_STENCIL_FUNC},
	{OS_TEXT("GL_STENCIL_VALUE_MASK"), GL_STENCIL_VALUE_MASK},
	{OS_TEXT("GL_STENCIL_FAIL"), GL_STENCIL_FAIL},
	{OS_TEXT("GL_STENCIL_PASS_DEPTH_FAIL"), GL_STENCIL_PASS_DEPTH_FAIL},
	{OS_TEXT("GL_STENCIL_PASS_DEPTH_PASS"), GL_STENCIL_PASS_DEPTH_PASS},
	{OS_TEXT("GL_STENCIL_REF"), GL_STENCIL_REF},
	{OS_TEXT("GL_STENCIL_WRITEMASK"), GL_STENCIL_WRITEMASK},
	{OS_TEXT("GL_MATRIX_MODE"), GL_MATRIX_MODE},
	{OS_TEXT("GL_NORMALIZE"), GL_NORMALIZE},
	{OS_TEXT("GL_VIEWPORT"), GL_VIEWPORT},
	{OS_TEXT("GL_MODELVIEW_STACK_DEPTH"), GL_MODELVIEW_STACK_DEPTH},
	{OS_TEXT("GL_PROJECTION_STACK_DEPTH"), GL_PROJECTION_STACK_DEPTH},
	{OS_TEXT("GL_TEXTURE_STACK_DEPTH"), GL_TEXTURE_STACK_DEPTH},
	{OS_TEXT("GL_MODELVIEW_MATRIX"), GL_MODELVIEW_MATRIX},
	{OS_TEXT("GL_PROJECTION_MATRIX"), GL_PROJECTION_MATRIX},
	{OS_TEXT("GL_TEXTURE_MATRIX"), GL_TEXTURE_MATRIX},
	{OS_TEXT("GL_ATTRIB_STACK_DEPTH"), GL_ATTRIB_STACK_DEPTH},
	{OS_TEXT("GL_CLIENT_ATTRIB_STACK_DEPTH"), GL_CLIENT_ATTRIB_STACK_DEPTH},
	{OS_TEXT("GL_ALPHA_TEST"), GL_ALPHA_TEST},
	{OS_TEXT("GL_ALPHA_TEST_FUNC"), GL_ALPHA_TEST_FUNC},
	{OS_TEXT("GL_ALPHA_TEST_REF"), GL_ALPHA_TEST_REF},
	{OS_TEXT("GL_DITHER"), GL_DITHER},
	{OS_TEXT("GL_BLEND_DST"), GL_BLEND_DST},
	{OS_TEXT("GL_BLEND_SRC"), GL_BLEND_SRC},
	{OS_TEXT("GL_BLEND"), GL_BLEND},
	{OS_TEXT("GL_LOGIC_OP_MODE"), GL_LOGIC_OP_MODE},
	{OS_TEXT("GL_INDEX_LOGIC_OP"), GL_INDEX_LOGIC_OP},
	{OS_TEXT("GL_COLOR_LOGIC_OP"), GL_COLOR_LOGIC_OP},
	{OS_TEXT("GL_AUX_BUFFERS"), GL_AUX_BUFFERS},
	{OS_TEXT("GL_DRAW_BUFFER"), GL_DRAW_BUFFER},
	{OS_TEXT("GL_READ_BUFFER"), GL_READ_BUFFER},
	{OS_TEXT("GL_SCISSOR_BOX"), GL_SCISSOR_BOX},
	{OS_TEXT("GL_SCISSOR_TEST"), GL_SCISSOR_TEST},
	{OS_TEXT("GL_INDEX_CLEAR_VALUE"), GL_INDEX_CLEAR_VALUE},
	{OS_TEXT("GL_INDEX_WRITEMASK"), GL_INDEX_WRITEMASK},
	{OS_TEXT("GL_COLOR_CLEAR_VALUE"), GL_COLOR_CLEAR_VALUE},
	{OS_TEXT("GL_COLOR_WRITEMASK"), GL_COLOR_WRITEMASK},
	{OS_TEXT("GL_INDEX_MODE"), GL_INDEX_MODE},
	{OS_TEXT("GL_RGBA_MODE"), GL_RGBA_MODE},
	{OS_TEXT("GL_DOUBLEBUFFER"), GL_DOUBLEBUFFER},
	{OS_TEXT("GL_STEREO"), GL_STEREO},
	{OS_TEXT("GL_RENDER_MODE"), GL_RENDER_MODE},
	{OS_TEXT("GL_PERSPECTIVE_CORRECTION_HINT"), GL_PERSPECTIVE_CORRECTION_HINT},
	{OS_TEXT("GL_POINT_SMOOTH_HINT"), GL_POINT_SMOOTH_HINT},
	{OS_TEXT("GL_LINE_SMOOTH_HINT"), GL_LINE_SMOOTH_HINT},
	{OS_TEXT("GL_POLYGON_SMOOTH_HINT"), GL_POLYGON_SMOOTH_HINT},
	{OS_TEXT("GL_FOG_HINT"), GL_FOG_HINT},
	{OS_TEXT("GL_TEXTURE_GEN_S"), GL_TEXTURE_GEN_S},
	{OS_TEXT("GL_TEXTURE_GEN_T"), GL_TEXTURE_GEN_T},
	{OS_TEXT("GL_TEXTURE_GEN_R"), GL_TEXTURE_GEN_R},
	{OS_TEXT("GL_TEXTURE_GEN_Q"), GL_TEXTURE_GEN_Q},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_I"), GL_PIXEL_MAP_I_TO_I},
	{OS_TEXT("GL_PIXEL_MAP_S_TO_S"), GL_PIXEL_MAP_S_TO_S},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_R"), GL_PIXEL_MAP_I_TO_R},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_G"), GL_PIXEL_MAP_I_TO_G},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_B"), GL_PIXEL_MAP_I_TO_B},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_A"), GL_PIXEL_MAP_I_TO_A},
	{OS_TEXT("GL_PIXEL_MAP_R_TO_R"), GL_PIXEL_MAP_R_TO_R},
	{OS_TEXT("GL_PIXEL_MAP_G_TO_G"), GL_PIXEL_MAP_G_TO_G},
	{OS_TEXT("GL_PIXEL_MAP_B_TO_B"), GL_PIXEL_MAP_B_TO_B},
	{OS_TEXT("GL_PIXEL_MAP_A_TO_A"), GL_PIXEL_MAP_A_TO_A},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_I_SIZE"), GL_PIXEL_MAP_I_TO_I_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_S_TO_S_SIZE"), GL_PIXEL_MAP_S_TO_S_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_R_SIZE"), GL_PIXEL_MAP_I_TO_R_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_G_SIZE"), GL_PIXEL_MAP_I_TO_G_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_B_SIZE"), GL_PIXEL_MAP_I_TO_B_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_I_TO_A_SIZE"), GL_PIXEL_MAP_I_TO_A_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_R_TO_R_SIZE"), GL_PIXEL_MAP_R_TO_R_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_G_TO_G_SIZE"), GL_PIXEL_MAP_G_TO_G_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_B_TO_B_SIZE"), GL_PIXEL_MAP_B_TO_B_SIZE},
	{OS_TEXT("GL_PIXEL_MAP_A_TO_A_SIZE"), GL_PIXEL_MAP_A_TO_A_SIZE},
	{OS_TEXT("GL_UNPACK_SWAP_BYTES"), GL_UNPACK_SWAP_BYTES},
	{OS_TEXT("GL_UNPACK_LSB_FIRST"), GL_UNPACK_LSB_FIRST},
	{OS_TEXT("GL_UNPACK_ROW_LENGTH"), GL_UNPACK_ROW_LENGTH},
	{OS_TEXT("GL_UNPACK_SKIP_ROWS"), GL_UNPACK_SKIP_ROWS},
	{OS_TEXT("GL_UNPACK_SKIP_PIXELS"), GL_UNPACK_SKIP_PIXELS},
	{OS_TEXT("GL_UNPACK_ALIGNMENT"), GL_UNPACK_ALIGNMENT},
	{OS_TEXT("GL_PACK_SWAP_BYTES"), GL_PACK_SWAP_BYTES},
	{OS_TEXT("GL_PACK_LSB_FIRST"), GL_PACK_LSB_FIRST},
	{OS_TEXT("GL_PACK_ROW_LENGTH"), GL_PACK_ROW_LENGTH},
	{OS_TEXT("GL_PACK_SKIP_ROWS"), GL_PACK_SKIP_ROWS},
	{OS_TEXT("GL_PACK_SKIP_PIXELS"), GL_PACK_SKIP_PIXELS},
	{OS_TEXT("GL_PACK_ALIGNMENT"), GL_PACK_ALIGNMENT},
	{OS_TEXT("GL_MAP_COLOR"), GL_MAP_COLOR},
	{OS_TEXT("GL_MAP_STENCIL"), GL_MAP_STENCIL},
	{OS_TEXT("GL_INDEX_SHIFT"), GL_INDEX_SHIFT},
	{OS_TEXT("GL_INDEX_OFFSET"), GL_INDEX_OFFSET},
	{OS_TEXT("GL_RED_SCALE"), GL_RED_SCALE},
	{OS_TEXT("GL_RED_BIAS"), GL_RED_BIAS},
	{OS_TEXT("GL_ZOOM_X"), GL_ZOOM_X},
	{OS_TEXT("GL_ZOOM_Y"), GL_ZOOM_Y},
	{OS_TEXT("GL_GREEN_SCALE"), GL_GREEN_SCALE},
	{OS_TEXT("GL_GREEN_BIAS"), GL_GREEN_BIAS},
	{OS_TEXT("GL_BLUE_SCALE"), GL_BLUE_SCALE},
	{OS_TEXT("GL_BLUE_BIAS"), GL_BLUE_BIAS},
	{OS_TEXT("GL_ALPHA_SCALE"), GL_ALPHA_SCALE},
	{OS_TEXT("GL_ALPHA_BIAS"), GL_ALPHA_BIAS},
	{OS_TEXT("GL_DEPTH_SCALE"), GL_DEPTH_SCALE},
	{OS_TEXT("GL_DEPTH_BIAS"), GL_DEPTH_BIAS},
	{OS_TEXT("GL_MAX_EVAL_ORDER"), GL_MAX_EVAL_ORDER},
	{OS_TEXT("GL_MAX_LIGHTS"), GL_MAX_LIGHTS},
	{OS_TEXT("GL_MAX_CLIP_PLANES"), GL_MAX_CLIP_PLANES},
	{OS_TEXT("GL_MAX_TEXTURE_SIZE"), GL_MAX_TEXTURE_SIZE},
	{OS_TEXT("GL_MAX_PIXEL_MAP_TABLE"), GL_MAX_PIXEL_MAP_TABLE},
	{OS_TEXT("GL_MAX_ATTRIB_STACK_DEPTH"), GL_MAX_ATTRIB_STACK_DEPTH},
	{OS_TEXT("GL_MAX_MODELVIEW_STACK_DEPTH"), GL_MAX_MODELVIEW_STACK_DEPTH},
	{OS_TEXT("GL_MAX_NAME_STACK_DEPTH"), GL_MAX_NAME_STACK_DEPTH},
	{OS_TEXT("GL_MAX_PROJECTION_STACK_DEPTH"), GL_MAX_PROJECTION_STACK_DEPTH},
	{OS_TEXT("GL_MAX_TEXTURE_STACK_DEPTH"), GL_MAX_TEXTURE_STACK_DEPTH},
	{OS_TEXT("GL_MAX_VIEWPORT_DIMS"), GL_MAX_VIEWPORT_DIMS},
	{OS_TEXT("GL_MAX_CLIENT_ATTRIB_STACK_DEPTH"), GL_MAX_CLIENT_ATTRIB_STACK_DEPTH},
	{OS_TEXT("GL_SUBPIXEL_BITS"), GL_SUBPIXEL_BITS},
	{OS_TEXT("GL_INDEX_BITS"), GL_INDEX_BITS},
	{OS_TEXT("GL_RED_BITS"), GL_RED_BITS},
	{OS_TEXT("GL_GREEN_BITS"), GL_GREEN_BITS},
	{OS_TEXT("GL_BLUE_BITS"), GL_BLUE_BITS},
	{OS_TEXT("GL_ALPHA_BITS"), GL_ALPHA_BITS},
	{OS_TEXT("GL_DEPTH_BITS"), GL_DEPTH_BITS},
	{OS_TEXT("GL_STENCIL_BITS"), GL_STENCIL_BITS},
	{OS_TEXT("GL_ACCUM_RED_BITS"), GL_ACCUM_RED_BITS},
	{OS_TEXT("GL_ACCUM_GREEN_BITS"), GL_ACCUM_GREEN_BITS},
	{OS_TEXT("GL_ACCUM_BLUE_BITS"), GL_ACCUM_BLUE_BITS},
	{OS_TEXT("GL_ACCUM_ALPHA_BITS"), GL_ACCUM_ALPHA_BITS},
	{OS_TEXT("GL_NAME_STACK_DEPTH"), GL_NAME_STACK_DEPTH},
	{OS_TEXT("GL_AUTO_NORMAL"), GL_AUTO_NORMAL},
	{OS_TEXT("GL_MAP1_COLOR_4"), GL_MAP1_COLOR_4},
	{OS_TEXT("GL_MAP1_INDEX"), GL_MAP1_INDEX},
	{OS_TEXT("GL_MAP1_NORMAL"), GL_MAP1_NORMAL},
	{OS_TEXT("GL_MAP1_TEXTURE_COORD_1"), GL_MAP1_TEXTURE_COORD_1},
	{OS_TEXT("GL_MAP1_TEXTURE_COORD_2"), GL_MAP1_TEXTURE_COORD_2},
	{OS_TEXT("GL_MAP1_TEXTURE_COORD_3"), GL_MAP1_TEXTURE_COORD_3},
	{OS_TEXT("GL_MAP1_TEXTURE_COORD_4"), GL_MAP1_TEXTURE_COORD_4},
	{OS_TEXT("GL_MAP1_VERTEX_3"), GL_MAP1_VERTEX_3},
	{OS_TEXT("GL_MAP1_VERTEX_4"), GL_MAP1_VERTEX_4},
	{OS_TEXT("GL_MAP2_COLOR_4"), GL_MAP2_COLOR_4},
	{OS_TEXT("GL_MAP2_INDEX"), GL_MAP2_INDEX},
	{OS_TEXT("GL_MAP2_NORMAL"), GL_MAP2_NORMAL},
	{OS_TEXT("GL_MAP2_TEXTURE_COORD_1"), GL_MAP2_TEXTURE_COORD_1},
	{OS_TEXT("GL_MAP2_TEXTURE_COORD_2"), GL_MAP2_TEXTURE_COORD_2},
	{OS_TEXT("GL_MAP2_TEXTURE_COORD_3"), GL_MAP2_TEXTURE_COORD_3},
	{OS_TEXT("GL_MAP2_TEXTURE_COORD_4"), GL_MAP2_TEXTURE_COORD_4},
	{OS_TEXT("GL_MAP2_VERTEX_3"), GL_MAP2_VERTEX_3},
	{OS_TEXT("GL_MAP2_VERTEX_4"), GL_MAP2_VERTEX_4},
	{OS_TEXT("GL_MAP1_GRID_DOMAIN"), GL_MAP1_GRID_DOMAIN},
	{OS_TEXT("GL_MAP1_GRID_SEGMENTS"), GL_MAP1_GRID_SEGMENTS},
	{OS_TEXT("GL_MAP2_GRID_DOMAIN"), GL_MAP2_GRID_DOMAIN},
	{OS_TEXT("GL_MAP2_GRID_SEGMENTS"), GL_MAP2_GRID_SEGMENTS},
	{OS_TEXT("GL_TEXTURE_1D"), GL_TEXTURE_1D},
	{OS_TEXT("GL_TEXTURE_2D"), GL_TEXTURE_2D},
	{OS_TEXT("GL_FEEDBACK_BUFFER_POINTER"), GL_FEEDBACK_BUFFER_POINTER},
	{OS_TEXT("GL_FEEDBACK_BUFFER_SIZE"), GL_FEEDBACK_BUFFER_SIZE},
	{OS_TEXT("GL_FEEDBACK_BUFFER_TYPE"), GL_FEEDBACK_BUFFER_TYPE},
	{OS_TEXT("GL_SELECTION_BUFFER_POINTER"), GL_SELECTION_BUFFER_POINTER},
	{OS_TEXT("GL_SELECTION_BUFFER_SIZE"), GL_SELECTION_BUFFER_SIZE},
	{OS_TEXT("GL_TEXTURE_WIDTH"), GL_TEXTURE_WIDTH},
	{OS_TEXT("GL_TEXTURE_HEIGHT"), GL_TEXTURE_HEIGHT},
	{OS_TEXT("GL_TEXTURE_INTERNAL_FORMAT"), GL_TEXTURE_INTERNAL_FORMAT},
	{OS_TEXT("GL_TEXTURE_BORDER_COLOR"), GL_TEXTURE_BORDER_COLOR},
	{OS_TEXT("GL_TEXTURE_BORDER"), GL_TEXTURE_BORDER},
	{OS_TEXT("GL_DONT_CARE"), GL_DONT_CARE},
	{OS_TEXT("GL_FASTEST"), GL_FASTEST},
	{OS_TEXT("GL_NICEST"), GL_NICEST},
	{OS_TEXT("GL_LIGHT0"), GL_LIGHT0},
	{OS_TEXT("GL_LIGHT1"), GL_LIGHT1},
	{OS_TEXT("GL_LIGHT2"), GL_LIGHT2},
	{OS_TEXT("GL_LIGHT3"), GL_LIGHT3},
	{OS_TEXT("GL_LIGHT4"), GL_LIGHT4},
	{OS_TEXT("GL_LIGHT5"), GL_LIGHT5},
	{OS_TEXT("GL_LIGHT6"), GL_LIGHT6},
	{OS_TEXT("GL_LIGHT7"), GL_LIGHT7},
	{OS_TEXT("GL_AMBIENT"), GL_AMBIENT},
	{OS_TEXT("GL_DIFFUSE"), GL_DIFFUSE},
	{OS_TEXT("GL_SPECULAR"), GL_SPECULAR},
	{OS_TEXT("GL_POSITION"), GL_POSITION},
	{OS_TEXT("GL_SPOT_DIRECTION"), GL_SPOT_DIRECTION},
	{OS_TEXT("GL_SPOT_EXPONENT"), GL_SPOT_EXPONENT},
	{OS_TEXT("GL_SPOT_CUTOFF"), GL_SPOT_CUTOFF},
	{OS_TEXT("GL_CONSTANT_ATTENUATION"), GL_CONSTANT_ATTENUATION},
	{OS_TEXT("GL_LINEAR_ATTENUATION"), GL_LINEAR_ATTENUATION},
	{OS_TEXT("GL_QUADRATIC_ATTENUATION"), GL_QUADRATIC_ATTENUATION},
	{OS_TEXT("GL_COMPILE"), GL_COMPILE},
	{OS_TEXT("GL_COMPILE_AND_EXECUTE"), GL_COMPILE_AND_EXECUTE},
	{OS_TEXT("GL_CLEAR"), GL_CLEAR},
	{OS_TEXT("GL_AND"), GL_AND},
	{OS_TEXT("GL_AND_REVERSE"), GL_AND_REVERSE},
	{OS_TEXT("GL_COPY"), GL_COPY},
	{OS_TEXT("GL_AND_INVERTED"), GL_AND_INVERTED},
	{OS_TEXT("GL_NOOP"), GL_NOOP},
	{OS_TEXT("GL_XOR"), GL_XOR},
	{OS_TEXT("GL_OR"), GL_OR},
	{OS_TEXT("GL_NOR"), GL_NOR},
	{OS_TEXT("GL_EQUIV"), GL_EQUIV},
	{OS_TEXT("GL_INVERT"), GL_INVERT},
	{OS_TEXT("GL_OR_REVERSE"), GL_OR_REVERSE},
	{OS_TEXT("GL_COPY_INVERTED"), GL_COPY_INVERTED},
	{OS_TEXT("GL_OR_INVERTED"), GL_OR_INVERTED},
	{OS_TEXT("GL_NAND"), GL_NAND},
	{OS_TEXT("GL_SET"), GL_SET},
	{OS_TEXT("GL_EMISSION"), GL_EMISSION},
	{OS_TEXT("GL_SHININESS"), GL_SHININESS},
	{OS_TEXT("GL_AMBIENT_AND_DIFFUSE"), GL_AMBIENT_AND_DIFFUSE},
	{OS_TEXT("GL_COLOR_INDEXES"), GL_COLOR_INDEXES},
	{OS_TEXT("GL_MODELVIEW"), GL_MODELVIEW},
	{OS_TEXT("GL_PROJECTION"), GL_PROJECTION},
	{OS_TEXT("GL_TEXTURE"), GL_TEXTURE},
	{OS_TEXT("GL_COLOR"), GL_COLOR},
	{OS_TEXT("GL_DEPTH"), GL_DEPTH},
	{OS_TEXT("GL_STENCIL"), GL_STENCIL},
	{OS_TEXT("GL_COLOR_INDEX"), GL_COLOR_INDEX},
	{OS_TEXT("GL_STENCIL_INDEX"), GL_STENCIL_INDEX},
	{OS_TEXT("GL_DEPTH_COMPONENT"), GL_DEPTH_COMPONENT},
	{OS_TEXT("GL_RED"), GL_RED},
	{OS_TEXT("GL_GREEN"), GL_GREEN},
	{OS_TEXT("GL_BLUE"), GL_BLUE},
	{OS_TEXT("GL_ALPHA"), GL_ALPHA},
	{OS_TEXT("GL_RGB"), GL_RGB},
	{OS_TEXT("GL_RGBA"), GL_RGBA},
	{OS_TEXT("GL_LUMINANCE"), GL_LUMINANCE},
	{OS_TEXT("GL_LUMINANCE_ALPHA"), GL_LUMINANCE_ALPHA},
	{OS_TEXT("GL_BITMAP"), GL_BITMAP},
	{OS_TEXT("GL_POINT"), GL_POINT},
	{OS_TEXT("GL_LINE"), GL_LINE},
	{OS_TEXT("GL_FILL"), GL_FILL},
	{OS_TEXT("GL_RENDER"), GL_RENDER},
	{OS_TEXT("GL_FEEDBACK"), GL_FEEDBACK},
	{OS_TEXT("GL_SELECT"), GL_SELECT},
	{OS_TEXT("GL_FLAT"), GL_FLAT},
	{OS_TEXT("GL_SMOOTH"), GL_SMOOTH},
	{OS_TEXT("GL_KEEP"), GL_KEEP},
	{OS_TEXT("GL_REPLACE"), GL_REPLACE},
	{OS_TEXT("GL_INCR"), GL_INCR},
	{OS_TEXT("GL_DECR"), GL_DECR},
	{OS_TEXT("GL_VENDOR"), GL_VENDOR},
	{OS_TEXT("GL_RENDERER"), GL_RENDERER},
	{OS_TEXT("GL_VERSION"), GL_VERSION},
	{OS_TEXT("GL_EXTENSIONS"), GL_EXTENSIONS},
	{OS_TEXT("GL_S"), GL_S},
	{OS_TEXT("GL_T"), GL_T},
	{OS_TEXT("GL_R"), GL_R},
	{OS_TEXT("GL_Q"), GL_Q},
	{OS_TEXT("GL_MODULATE"), GL_MODULATE},
	{OS_TEXT("GL_DECAL"), GL_DECAL},
	{OS_TEXT("GL_TEXTURE_ENV_MODE"), GL_TEXTURE_ENV_MODE},
	{OS_TEXT("GL_TEXTURE_ENV_COLOR"), GL_TEXTURE_ENV_COLOR},
	{OS_TEXT("GL_TEXTURE_ENV"), GL_TEXTURE_ENV},
	{OS_TEXT("GL_EYE_LINEAR"), GL_EYE_LINEAR},
	{OS_TEXT("GL_OBJECT_LINEAR"), GL_OBJECT_LINEAR},
	{OS_TEXT("GL_SPHERE_MAP"), GL_SPHERE_MAP},
	{OS_TEXT("GL_TEXTURE_GEN_MODE"), GL_TEXTURE_GEN_MODE},
	{OS_TEXT("GL_OBJECT_PLANE"), GL_OBJECT_PLANE},
	{OS_TEXT("GL_EYE_PLANE"), GL_EYE_PLANE},
	{OS_TEXT("GL_NEAREST"), GL_NEAREST},
	{OS_TEXT("GL_LINEAR"), GL_LINEAR},
	{OS_TEXT("GL_NEAREST_MIPMAP_NEAREST"), GL_NEAREST_MIPMAP_NEAREST},
	{OS_TEXT("GL_LINEAR_MIPMAP_NEAREST"), GL_LINEAR_MIPMAP_NEAREST},
	{OS_TEXT("GL_NEAREST_MIPMAP_LINEAR"), GL_NEAREST_MIPMAP_LINEAR},
	{OS_TEXT("GL_LINEAR_MIPMAP_LINEAR"), GL_LINEAR_MIPMAP_LINEAR},
	{OS_TEXT("GL_TEXTURE_MAG_FILTER"), GL_TEXTURE_MAG_FILTER},
	{OS_TEXT("GL_TEXTURE_MIN_FILTER"), GL_TEXTURE_MIN_FILTER},
	{OS_TEXT("GL_TEXTURE_WRAP_S"), GL_TEXTURE_WRAP_S},
	{OS_TEXT("GL_TEXTURE_WRAP_T"), GL_TEXTURE_WRAP_T},
	{OS_TEXT("GL_CLAMP"), GL_CLAMP},
	{OS_TEXT("GL_REPEAT"), GL_REPEAT},
	{OS_TEXT("GL_CLIENT_PIXEL_STORE_BIT"), GL_CLIENT_PIXEL_STORE_BIT},
	{OS_TEXT("GL_CLIENT_VERTEX_ARRAY_BIT"), GL_CLIENT_VERTEX_ARRAY_BIT},
	{OS_TEXT("GL_CLIENT_ALL_ATTRIB_BITS"), GL_CLIENT_ALL_ATTRIB_BITS},
	{OS_TEXT("GL_POLYGON_OFFSET_FACTOR"), GL_POLYGON_OFFSET_FACTOR},
	{OS_TEXT("GL_POLYGON_OFFSET_UNITS"), GL_POLYGON_OFFSET_UNITS},
	{OS_TEXT("GL_POLYGON_OFFSET_POINT"), GL_POLYGON_OFFSET_POINT},
	{OS_TEXT("GL_POLYGON_OFFSET_LINE"), GL_POLYGON_OFFSET_LINE},
	{OS_TEXT("GL_POLYGON_OFFSET_FILL"), GL_POLYGON_OFFSET_FILL},
	{OS_TEXT("GL_ALPHA4"), GL_ALPHA4},
	{OS_TEXT("GL_ALPHA8"), GL_ALPHA8},
	{OS_TEXT("GL_ALPHA12"), GL_ALPHA12},
	{OS_TEXT("GL_ALPHA16"), GL_ALPHA16},
	{OS_TEXT("GL_LUMINANCE4"), GL_LUMINANCE4},
	{OS_TEXT("GL_LUMINANCE8"), GL_LUMINANCE8},
	{OS_TEXT("GL_LUMINANCE12"), GL_LUMINANCE12},
	{OS_TEXT("GL_LUMINANCE16"), GL_LUMINANCE16},
	{OS_TEXT("GL_LUMINANCE4_ALPHA4"), GL_LUMINANCE4_ALPHA4},
	{OS_TEXT("GL_LUMINANCE6_ALPHA2"), GL_LUMINANCE6_ALPHA2},
	{OS_TEXT("GL_LUMINANCE8_ALPHA8"), GL_LUMINANCE8_ALPHA8},
	{OS_TEXT("GL_LUMINANCE12_ALPHA4"), GL_LUMINANCE12_ALPHA4},
	{OS_TEXT("GL_LUMINANCE12_ALPHA12"), GL_LUMINANCE12_ALPHA12},
	{OS_TEXT("GL_LUMINANCE16_ALPHA16"), GL_LUMINANCE16_ALPHA16},
	{OS_TEXT("GL_INTENSITY"), GL_INTENSITY},
	{OS_TEXT("GL_INTENSITY4"), GL_INTENSITY4},
	{OS_TEXT("GL_INTENSITY8"), GL_INTENSITY8},
	{OS_TEXT("GL_INTENSITY12"), GL_INTENSITY12},
	{OS_TEXT("GL_INTENSITY16"), GL_INTENSITY16},
	{OS_TEXT("GL_R3_G3_B2"), GL_R3_G3_B2},
	{OS_TEXT("GL_RGB4"), GL_RGB4},
	{OS_TEXT("GL_RGB5"), GL_RGB5},
	{OS_TEXT("GL_RGB8"), GL_RGB8},
	{OS_TEXT("GL_RGB10"), GL_RGB10},
	{OS_TEXT("GL_RGB12"), GL_RGB12},
	{OS_TEXT("GL_RGB16"), GL_RGB16},
	{OS_TEXT("GL_RGBA2"), GL_RGBA2},
	{OS_TEXT("GL_RGBA4"), GL_RGBA4},
	{OS_TEXT("GL_RGB5_A1"), GL_RGB5_A1},
	{OS_TEXT("GL_RGBA8"), GL_RGBA8},
	{OS_TEXT("GL_RGB10_A2"), GL_RGB10_A2},
	{OS_TEXT("GL_RGBA12"), GL_RGBA12},
	{OS_TEXT("GL_RGBA16"), GL_RGBA16},
	{OS_TEXT("GL_TEXTURE_RED_SIZE"), GL_TEXTURE_RED_SIZE},
	{OS_TEXT("GL_TEXTURE_GREEN_SIZE"), GL_TEXTURE_GREEN_SIZE},
	{OS_TEXT("GL_TEXTURE_BLUE_SIZE"), GL_TEXTURE_BLUE_SIZE},
	{OS_TEXT("GL_TEXTURE_ALPHA_SIZE"), GL_TEXTURE_ALPHA_SIZE},
	{OS_TEXT("GL_TEXTURE_LUMINANCE_SIZE"), GL_TEXTURE_LUMINANCE_SIZE},
	{OS_TEXT("GL_TEXTURE_INTENSITY_SIZE"), GL_TEXTURE_INTENSITY_SIZE},
	{OS_TEXT("GL_PROXY_TEXTURE_1D"), GL_PROXY_TEXTURE_1D},
	{OS_TEXT("GL_PROXY_TEXTURE_2D"), GL_PROXY_TEXTURE_2D},
	{OS_TEXT("GL_TEXTURE_PRIORITY"), GL_TEXTURE_PRIORITY},
	{OS_TEXT("GL_TEXTURE_RESIDENT"), GL_TEXTURE_RESIDENT},
	{OS_TEXT("GL_TEXTURE_BINDING_1D"), GL_TEXTURE_BINDING_1D},
	{OS_TEXT("GL_TEXTURE_BINDING_2D"), GL_TEXTURE_BINDING_2D},
	{OS_TEXT("GL_VERTEX_ARRAY"), GL_VERTEX_ARRAY},
	{OS_TEXT("GL_NORMAL_ARRAY"), GL_NORMAL_ARRAY},
	{OS_TEXT("GL_COLOR_ARRAY"), GL_COLOR_ARRAY},
	{OS_TEXT("GL_INDEX_ARRAY"), GL_INDEX_ARRAY},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY"), GL_TEXTURE_COORD_ARRAY},
	{OS_TEXT("GL_EDGE_FLAG_ARRAY"), GL_EDGE_FLAG_ARRAY},
	{OS_TEXT("GL_VERTEX_ARRAY_SIZE"), GL_VERTEX_ARRAY_SIZE},
	{OS_TEXT("GL_VERTEX_ARRAY_TYPE"), GL_VERTEX_ARRAY_TYPE},
	{OS_TEXT("GL_VERTEX_ARRAY_STRIDE"), GL_VERTEX_ARRAY_STRIDE},
	{OS_TEXT("GL_NORMAL_ARRAY_TYPE"), GL_NORMAL_ARRAY_TYPE},
	{OS_TEXT("GL_NORMAL_ARRAY_STRIDE"), GL_NORMAL_ARRAY_STRIDE},
	{OS_TEXT("GL_COLOR_ARRAY_SIZE"), GL_COLOR_ARRAY_SIZE},
	{OS_TEXT("GL_COLOR_ARRAY_TYPE"), GL_COLOR_ARRAY_TYPE},
	{OS_TEXT("GL_COLOR_ARRAY_STRIDE"), GL_COLOR_ARRAY_STRIDE},
	{OS_TEXT("GL_INDEX_ARRAY_TYPE"), GL_INDEX_ARRAY_TYPE},
	{OS_TEXT("GL_INDEX_ARRAY_STRIDE"), GL_INDEX_ARRAY_STRIDE},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_SIZE"), GL_TEXTURE_COORD_ARRAY_SIZE},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_TYPE"), GL_TEXTURE_COORD_ARRAY_TYPE},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_STRIDE"), GL_TEXTURE_COORD_ARRAY_STRIDE},
	{OS_TEXT("GL_EDGE_FLAG_ARRAY_STRIDE"), GL_EDGE_FLAG_ARRAY_STRIDE},
	{OS_TEXT("GL_VERTEX_ARRAY_POINTER"), GL_VERTEX_ARRAY_POINTER},
	{OS_TEXT("GL_NORMAL_ARRAY_POINTER"), GL_NORMAL_ARRAY_POINTER},
	{OS_TEXT("GL_COLOR_ARRAY_POINTER"), GL_COLOR_ARRAY_POINTER},
	{OS_TEXT("GL_INDEX_ARRAY_POINTER"), GL_INDEX_ARRAY_POINTER},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_POINTER"), GL_TEXTURE_COORD_ARRAY_POINTER},
	{OS_TEXT("GL_EDGE_FLAG_ARRAY_POINTER"), GL_EDGE_FLAG_ARRAY_POINTER},
	{OS_TEXT("GL_V2F"), GL_V2F},
	{OS_TEXT("GL_V3F"), GL_V3F},
	{OS_TEXT("GL_C4UB_V2F"), GL_C4UB_V2F},
	{OS_TEXT("GL_C4UB_V3F"), GL_C4UB_V3F},
	{OS_TEXT("GL_C3F_V3F"), GL_C3F_V3F},
	{OS_TEXT("GL_N3F_V3F"), GL_N3F_V3F},
	{OS_TEXT("GL_C4F_N3F_V3F"), GL_C4F_N3F_V3F},
	{OS_TEXT("GL_T2F_V3F"), GL_T2F_V3F},
	{OS_TEXT("GL_T4F_V4F"), GL_T4F_V4F},
	{OS_TEXT("GL_T2F_C4UB_V3F"), GL_T2F_C4UB_V3F},
	{OS_TEXT("GL_T2F_C3F_V3F"), GL_T2F_C3F_V3F},
	{OS_TEXT("GL_T2F_N3F_V3F"), GL_T2F_N3F_V3F},
	{OS_TEXT("GL_T2F_C4F_N3F_V3F"), GL_T2F_C4F_N3F_V3F},
	{OS_TEXT("GL_T4F_C4F_N3F_V4F"), GL_T4F_C4F_N3F_V4F},
	{OS_TEXT("GL_EXT_vertex_array"), GL_EXT_vertex_array},
	{OS_TEXT("GL_EXT_bgra"), GL_EXT_bgra},
	{OS_TEXT("GL_EXT_paletted_texture"), GL_EXT_paletted_texture},
	{OS_TEXT("GL_WIN_swap_hint"), GL_WIN_swap_hint},
	{OS_TEXT("GL_WIN_draw_range_elements"), GL_WIN_draw_range_elements},
	{OS_TEXT("GL_VERTEX_ARRAY_EXT"), GL_VERTEX_ARRAY_EXT},
	{OS_TEXT("GL_NORMAL_ARRAY_EXT"), GL_NORMAL_ARRAY_EXT},
	{OS_TEXT("GL_COLOR_ARRAY_EXT"), GL_COLOR_ARRAY_EXT},
	{OS_TEXT("GL_INDEX_ARRAY_EXT"), GL_INDEX_ARRAY_EXT},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_EXT"), GL_TEXTURE_COORD_ARRAY_EXT},
	{OS_TEXT("GL_EDGE_FLAG_ARRAY_EXT"), GL_EDGE_FLAG_ARRAY_EXT},
	{OS_TEXT("GL_VERTEX_ARRAY_SIZE_EXT"), GL_VERTEX_ARRAY_SIZE_EXT},
	{OS_TEXT("GL_VERTEX_ARRAY_TYPE_EXT"), GL_VERTEX_ARRAY_TYPE_EXT},
	{OS_TEXT("GL_VERTEX_ARRAY_STRIDE_EXT"), GL_VERTEX_ARRAY_STRIDE_EXT},
	{OS_TEXT("GL_VERTEX_ARRAY_COUNT_EXT"), GL_VERTEX_ARRAY_COUNT_EXT},
	{OS_TEXT("GL_NORMAL_ARRAY_TYPE_EXT"), GL_NORMAL_ARRAY_TYPE_EXT},
	{OS_TEXT("GL_NORMAL_ARRAY_STRIDE_EXT"), GL_NORMAL_ARRAY_STRIDE_EXT},
	{OS_TEXT("GL_NORMAL_ARRAY_COUNT_EXT"), GL_NORMAL_ARRAY_COUNT_EXT},
	{OS_TEXT("GL_COLOR_ARRAY_SIZE_EXT"), GL_COLOR_ARRAY_SIZE_EXT},
	{OS_TEXT("GL_COLOR_ARRAY_TYPE_EXT"), GL_COLOR_ARRAY_TYPE_EXT},
	{OS_TEXT("GL_COLOR_ARRAY_STRIDE_EXT"), GL_COLOR_ARRAY_STRIDE_EXT},
	{OS_TEXT("GL_COLOR_ARRAY_COUNT_EXT"), GL_COLOR_ARRAY_COUNT_EXT},
	{OS_TEXT("GL_INDEX_ARRAY_TYPE_EXT"), GL_INDEX_ARRAY_TYPE_EXT},
	{OS_TEXT("GL_INDEX_ARRAY_STRIDE_EXT"), GL_INDEX_ARRAY_STRIDE_EXT},
	{OS_TEXT("GL_INDEX_ARRAY_COUNT_EXT"), GL_INDEX_ARRAY_COUNT_EXT},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_SIZE_EXT"), GL_TEXTURE_COORD_ARRAY_SIZE_EXT},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_TYPE_EXT"), GL_TEXTURE_COORD_ARRAY_TYPE_EXT},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_STRIDE_EXT"), GL_TEXTURE_COORD_ARRAY_STRIDE_EXT},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_COUNT_EXT"), GL_TEXTURE_COORD_ARRAY_COUNT_EXT},
	{OS_TEXT("GL_EDGE_FLAG_ARRAY_STRIDE_EXT"), GL_EDGE_FLAG_ARRAY_STRIDE_EXT},
	{OS_TEXT("GL_EDGE_FLAG_ARRAY_COUNT_EXT"), GL_EDGE_FLAG_ARRAY_COUNT_EXT},
	{OS_TEXT("GL_VERTEX_ARRAY_POINTER_EXT"), GL_VERTEX_ARRAY_POINTER_EXT},
	{OS_TEXT("GL_NORMAL_ARRAY_POINTER_EXT"), GL_NORMAL_ARRAY_POINTER_EXT},
	{OS_TEXT("GL_COLOR_ARRAY_POINTER_EXT"), GL_COLOR_ARRAY_POINTER_EXT},
	{OS_TEXT("GL_INDEX_ARRAY_POINTER_EXT"), GL_INDEX_ARRAY_POINTER_EXT},
	{OS_TEXT("GL_TEXTURE_COORD_ARRAY_POINTER_EXT"), GL_TEXTURE_COORD_ARRAY_POINTER_EXT},
	{OS_TEXT("GL_EDGE_FLAG_ARRAY_POINTER_EXT"), GL_EDGE_FLAG_ARRAY_POINTER_EXT},
	{OS_TEXT("GL_DOUBLE_EXT"), GL_DOUBLE_EXT},
	{OS_TEXT("GL_BGR_EXT"), GL_BGR_EXT},
	{OS_TEXT("GL_BGRA_EXT"), GL_BGRA_EXT},
	{OS_TEXT("GL_COLOR_TABLE_FORMAT_EXT"), GL_COLOR_TABLE_FORMAT_EXT},
	{OS_TEXT("GL_COLOR_TABLE_WIDTH_EXT"), GL_COLOR_TABLE_WIDTH_EXT},
	{OS_TEXT("GL_COLOR_TABLE_RED_SIZE_EXT"), GL_COLOR_TABLE_RED_SIZE_EXT},
	{OS_TEXT("GL_COLOR_TABLE_GREEN_SIZE_EXT"), GL_COLOR_TABLE_GREEN_SIZE_EXT},
	{OS_TEXT("GL_COLOR_TABLE_BLUE_SIZE_EXT"), GL_COLOR_TABLE_BLUE_SIZE_EXT},
	{OS_TEXT("GL_COLOR_TABLE_ALPHA_SIZE_EXT"), GL_COLOR_TABLE_ALPHA_SIZE_EXT},
	{OS_TEXT("GL_COLOR_TABLE_LUMINANCE_SIZE_EXT"), GL_COLOR_TABLE_LUMINANCE_SIZE_EXT},
	{OS_TEXT("GL_COLOR_TABLE_INTENSITY_SIZE_EXT"), GL_COLOR_TABLE_INTENSITY_SIZE_EXT},
	{OS_TEXT("GL_COLOR_INDEX1_EXT"), GL_COLOR_INDEX1_EXT},
	{OS_TEXT("GL_COLOR_INDEX2_EXT"), GL_COLOR_INDEX2_EXT},
	{OS_TEXT("GL_COLOR_INDEX4_EXT"), GL_COLOR_INDEX4_EXT},
	{OS_TEXT("GL_COLOR_INDEX8_EXT"), GL_COLOR_INDEX8_EXT},
	{OS_TEXT("GL_COLOR_INDEX12_EXT"), GL_COLOR_INDEX12_EXT},
	{OS_TEXT("GL_COLOR_INDEX16_EXT"), GL_COLOR_INDEX16_EXT},
	{OS_TEXT("GL_MAX_ELEMENTS_VERTICES_WIN"), GL_MAX_ELEMENTS_VERTICES_WIN},
	{OS_TEXT("GL_MAX_ELEMENTS_INDICES_WIN"), GL_MAX_ELEMENTS_INDICES_WIN},
	{OS_TEXT("GL_PHONG_WIN"), GL_PHONG_WIN},
	{OS_TEXT("GL_PHONG_HINT_WIN"), GL_PHONG_HINT_WIN},
	{OS_TEXT("GL_FOG_SPECULAR_TEXTURE_WIN"), GL_FOG_SPECULAR_TEXTURE_WIN},
	{OS_TEXT("GL_LOGIC_OP"), GL_LOGIC_OP},
	{OS_TEXT("GL_TEXTURE_COMPONENTS"), GL_TEXTURE_COMPONENTS},
	// ------------------------
	{OS_TEXT("GLU_VERSION_1_1"), GLU_VERSION_1_1},
	{OS_TEXT("GLU_VERSION_1_2"), GLU_VERSION_1_2},
	{OS_TEXT("GLU_INVALID_ENUM"), GLU_INVALID_ENUM},
	{OS_TEXT("GLU_INVALID_VALUE"), GLU_INVALID_VALUE},
	{OS_TEXT("GLU_OUT_OF_MEMORY"), GLU_OUT_OF_MEMORY},
	{OS_TEXT("GLU_INCOMPATIBLE_GL_VERSION"), GLU_INCOMPATIBLE_GL_VERSION},
	{OS_TEXT("GLU_VERSION"), GLU_VERSION},
	{OS_TEXT("GLU_EXTENSIONS"), GLU_EXTENSIONS},
	{OS_TEXT("GLU_TRUE"), GLU_TRUE},
	{OS_TEXT("GLU_FALSE"), GLU_FALSE},
	{OS_TEXT("GLU_SMOOTH"), GLU_SMOOTH},
	{OS_TEXT("GLU_FLAT"), GLU_FLAT},
	{OS_TEXT("GLU_NONE"), GLU_NONE},
	{OS_TEXT("GLU_POINT"), GLU_POINT},
	{OS_TEXT("GLU_LINE"), GLU_LINE},
	{OS_TEXT("GLU_FILL"), GLU_FILL},
	{OS_TEXT("GLU_SILHOUETTE"), GLU_SILHOUETTE},
	{OS_TEXT("GLU_OUTSIDE"), GLU_OUTSIDE},
	{OS_TEXT("GLU_INSIDE"), GLU_INSIDE},
	{OS_TEXT("GLU_TESS_MAX_COORD"), GLU_TESS_MAX_COORD},
	{OS_TEXT("GLU_TESS_WINDING_RULE"), GLU_TESS_WINDING_RULE},
	{OS_TEXT("GLU_TESS_BOUNDARY_ONLY"), GLU_TESS_BOUNDARY_ONLY},
	{OS_TEXT("GLU_TESS_TOLERANCE"), GLU_TESS_TOLERANCE},
	{OS_TEXT("GLU_TESS_WINDING_ODD"), GLU_TESS_WINDING_ODD},
	{OS_TEXT("GLU_TESS_WINDING_NONZERO"), GLU_TESS_WINDING_NONZERO},
	{OS_TEXT("GLU_TESS_WINDING_POSITIVE"), GLU_TESS_WINDING_POSITIVE},
	{OS_TEXT("GLU_TESS_WINDING_NEGATIVE"), GLU_TESS_WINDING_NEGATIVE},
	{OS_TEXT("GLU_TESS_WINDING_ABS_GEQ_TWO"), GLU_TESS_WINDING_ABS_GEQ_TWO},
	{OS_TEXT("GLU_TESS_BEGIN"), GLU_TESS_BEGIN},
	{OS_TEXT("GLU_TESS_VERTEX"), GLU_TESS_VERTEX},
	{OS_TEXT("GLU_TESS_END"), GLU_TESS_END},
	{OS_TEXT("GLU_TESS_ERROR"), GLU_TESS_ERROR},
	{OS_TEXT("GLU_TESS_EDGE_FLAG"), GLU_TESS_EDGE_FLAG},
	{OS_TEXT("GLU_TESS_COMBINE"), GLU_TESS_COMBINE},
	{OS_TEXT("GLU_TESS_BEGIN_DATA"), GLU_TESS_BEGIN_DATA},
	{OS_TEXT("GLU_TESS_VERTEX_DATA"), GLU_TESS_VERTEX_DATA},
	{OS_TEXT("GLU_TESS_END_DATA"), GLU_TESS_END_DATA},
	{OS_TEXT("GLU_TESS_ERROR_DATA"), GLU_TESS_ERROR_DATA},
	{OS_TEXT("GLU_TESS_EDGE_FLAG_DATA"), GLU_TESS_EDGE_FLAG_DATA},
	{OS_TEXT("GLU_TESS_COMBINE_DATA"), GLU_TESS_COMBINE_DATA},
	{OS_TEXT("GLU_TESS_ERROR1"), GLU_TESS_ERROR1},
	{OS_TEXT("GLU_TESS_ERROR2"), GLU_TESS_ERROR2},
	{OS_TEXT("GLU_TESS_ERROR3"), GLU_TESS_ERROR3},
	{OS_TEXT("GLU_TESS_ERROR4"), GLU_TESS_ERROR4},
	{OS_TEXT("GLU_TESS_ERROR5"), GLU_TESS_ERROR5},
	{OS_TEXT("GLU_TESS_ERROR6"), GLU_TESS_ERROR6},
	{OS_TEXT("GLU_TESS_ERROR7"), GLU_TESS_ERROR7},
	{OS_TEXT("GLU_TESS_ERROR8"), GLU_TESS_ERROR8},
	{OS_TEXT("GLU_TESS_MISSING_BEGIN_POLYGON"), GLU_TESS_MISSING_BEGIN_POLYGON},
	{OS_TEXT("GLU_TESS_MISSING_BEGIN_CONTOUR"), GLU_TESS_MISSING_BEGIN_CONTOUR},
	{OS_TEXT("GLU_TESS_MISSING_END_POLYGON"), GLU_TESS_MISSING_END_POLYGON},
	{OS_TEXT("GLU_TESS_MISSING_END_CONTOUR"), GLU_TESS_MISSING_END_CONTOUR},
	{OS_TEXT("GLU_TESS_COORD_TOO_LARGE"), GLU_TESS_COORD_TOO_LARGE},
	{OS_TEXT("GLU_TESS_NEED_COMBINE_CALLBACK"), GLU_TESS_NEED_COMBINE_CALLBACK},
	{OS_TEXT("GLU_AUTO_LOAD_MATRIX"), GLU_AUTO_LOAD_MATRIX},
	{OS_TEXT("GLU_CULLING"), GLU_CULLING},
	{OS_TEXT("GLU_SAMPLING_TOLERANCE"), GLU_SAMPLING_TOLERANCE},
	{OS_TEXT("GLU_DISPLAY_MODE"), GLU_DISPLAY_MODE},
	{OS_TEXT("GLU_PARAMETRIC_TOLERANCE"), GLU_PARAMETRIC_TOLERANCE},
	{OS_TEXT("GLU_SAMPLING_METHOD"), GLU_SAMPLING_METHOD},
	{OS_TEXT("GLU_U_STEP"), GLU_U_STEP},
	{OS_TEXT("GLU_V_STEP"), GLU_V_STEP},
	{OS_TEXT("GLU_PATH_LENGTH"), GLU_PATH_LENGTH},
	{OS_TEXT("GLU_PARAMETRIC_ERROR"), GLU_PARAMETRIC_ERROR},
	{OS_TEXT("GLU_DOMAIN_DISTANCE"), GLU_DOMAIN_DISTANCE},
	{OS_TEXT("GLU_MAP1_TRIM_2"), GLU_MAP1_TRIM_2},
	{OS_TEXT("GLU_MAP1_TRIM_3"), GLU_MAP1_TRIM_3},
	{OS_TEXT("GLU_OUTLINE_POLYGON"), GLU_OUTLINE_POLYGON},
	{OS_TEXT("GLU_OUTLINE_PATCH"), GLU_OUTLINE_PATCH},
	{OS_TEXT("GLU_NURBS_ERROR1"), GLU_NURBS_ERROR1},
	{OS_TEXT("GLU_NURBS_ERROR2"), GLU_NURBS_ERROR2},
	{OS_TEXT("GLU_NURBS_ERROR3"), GLU_NURBS_ERROR3},
	{OS_TEXT("GLU_NURBS_ERROR4"), GLU_NURBS_ERROR4},
	{OS_TEXT("GLU_NURBS_ERROR5"), GLU_NURBS_ERROR5},
	{OS_TEXT("GLU_NURBS_ERROR6"), GLU_NURBS_ERROR6},
	{OS_TEXT("GLU_NURBS_ERROR7"), GLU_NURBS_ERROR7},
	{OS_TEXT("GLU_NURBS_ERROR8"), GLU_NURBS_ERROR8},
	{OS_TEXT("GLU_NURBS_ERROR9"), GLU_NURBS_ERROR9},
	{OS_TEXT("GLU_NURBS_ERROR10"), GLU_NURBS_ERROR10},
	{OS_TEXT("GLU_NURBS_ERROR11"), GLU_NURBS_ERROR11},
	{OS_TEXT("GLU_NURBS_ERROR12"), GLU_NURBS_ERROR12},
	{OS_TEXT("GLU_NURBS_ERROR13"), GLU_NURBS_ERROR13},
	{OS_TEXT("GLU_NURBS_ERROR14"), GLU_NURBS_ERROR14},
	{OS_TEXT("GLU_NURBS_ERROR15"), GLU_NURBS_ERROR15},
	{OS_TEXT("GLU_NURBS_ERROR16"), GLU_NURBS_ERROR16},
	{OS_TEXT("GLU_NURBS_ERROR17"), GLU_NURBS_ERROR17},
	{OS_TEXT("GLU_NURBS_ERROR18"), GLU_NURBS_ERROR18},
	{OS_TEXT("GLU_NURBS_ERROR19"), GLU_NURBS_ERROR19},
	{OS_TEXT("GLU_NURBS_ERROR20"), GLU_NURBS_ERROR20},
	{OS_TEXT("GLU_NURBS_ERROR21"), GLU_NURBS_ERROR21},
	{OS_TEXT("GLU_NURBS_ERROR22"), GLU_NURBS_ERROR22},
	{OS_TEXT("GLU_NURBS_ERROR23"), GLU_NURBS_ERROR23},
	{OS_TEXT("GLU_NURBS_ERROR24"), GLU_NURBS_ERROR24},
	{OS_TEXT("GLU_NURBS_ERROR25"), GLU_NURBS_ERROR25},
	{OS_TEXT("GLU_NURBS_ERROR26"), GLU_NURBS_ERROR26},
	{OS_TEXT("GLU_NURBS_ERROR27"), GLU_NURBS_ERROR27},
	{OS_TEXT("GLU_NURBS_ERROR28"), GLU_NURBS_ERROR28},
	{OS_TEXT("GLU_NURBS_ERROR29"), GLU_NURBS_ERROR29},
	{OS_TEXT("GLU_NURBS_ERROR30"), GLU_NURBS_ERROR30},
	{OS_TEXT("GLU_NURBS_ERROR31"), GLU_NURBS_ERROR31},
	{OS_TEXT("GLU_NURBS_ERROR32"), GLU_NURBS_ERROR32},
	{OS_TEXT("GLU_NURBS_ERROR33"), GLU_NURBS_ERROR33},
	{OS_TEXT("GLU_NURBS_ERROR34"), GLU_NURBS_ERROR34},
	{OS_TEXT("GLU_NURBS_ERROR35"), GLU_NURBS_ERROR35},
	{OS_TEXT("GLU_NURBS_ERROR36"), GLU_NURBS_ERROR36},
	{OS_TEXT("GLU_NURBS_ERROR37"), GLU_NURBS_ERROR37},
	{OS_TEXT("GLU_CW"), GLU_CW},
	{OS_TEXT("GLU_CCW"), GLU_CCW},
	{OS_TEXT("GLU_INTERIOR"), GLU_INTERIOR},
	{OS_TEXT("GLU_EXTERIOR"), GLU_EXTERIOR},
	{OS_TEXT("GLU_UNKNOWN"), GLU_UNKNOWN},
	{OS_TEXT("GLU_BEGIN"), GLU_BEGIN},
	{OS_TEXT("GLU_VERTEX"), GLU_VERTEX},
	{OS_TEXT("GLU_END"), GLU_END},
	{OS_TEXT("GLU_ERROR"), GLU_ERROR},
	{OS_TEXT("GLU_EDGE_FLAG"), GLU_EDGE_FLAG},
	// ------------------------
	{OS_TEXT("GLUT_API_VERSION"), GLUT_API_VERSION},
	{OS_TEXT("GLUT_XLIB_IMPLEMENTATION"), GLUT_XLIB_IMPLEMENTATION},
	{OS_TEXT("GLUT_RGB"), GLUT_RGB},
	{OS_TEXT("GLUT_RGBA"), GLUT_RGBA},
	{OS_TEXT("GLUT_INDEX"), GLUT_INDEX},
	{OS_TEXT("GLUT_SINGLE"), GLUT_SINGLE},
	{OS_TEXT("GLUT_DOUBLE"), GLUT_DOUBLE},
	{OS_TEXT("GLUT_ACCUM"), GLUT_ACCUM},
	{OS_TEXT("GLUT_ALPHA"), GLUT_ALPHA},
	{OS_TEXT("GLUT_DEPTH"), GLUT_DEPTH},
	{OS_TEXT("GLUT_STENCIL"), GLUT_STENCIL},
	{OS_TEXT("GLUT_MULTISAMPLE"), GLUT_MULTISAMPLE},
	{OS_TEXT("GLUT_STEREO"), GLUT_STEREO},
	{OS_TEXT("GLUT_LUMINANCE"), GLUT_LUMINANCE},
	{OS_TEXT("GLUT_LEFT_BUTTON"), GLUT_LEFT_BUTTON},
	{OS_TEXT("GLUT_MIDDLE_BUTTON"), GLUT_MIDDLE_BUTTON},
	{OS_TEXT("GLUT_RIGHT_BUTTON"), GLUT_RIGHT_BUTTON},
	{OS_TEXT("GLUT_DOWN"), GLUT_DOWN},
	{OS_TEXT("GLUT_UP"), GLUT_UP},
	{OS_TEXT("GLUT_KEY_F1"), GLUT_KEY_F1},
	{OS_TEXT("GLUT_KEY_F2"), GLUT_KEY_F2},
	{OS_TEXT("GLUT_KEY_F3"), GLUT_KEY_F3},
	{OS_TEXT("GLUT_KEY_F4"), GLUT_KEY_F4},
	{OS_TEXT("GLUT_KEY_F5"), GLUT_KEY_F5},
	{OS_TEXT("GLUT_KEY_F6"), GLUT_KEY_F6},
	{OS_TEXT("GLUT_KEY_F7"), GLUT_KEY_F7},
	{OS_TEXT("GLUT_KEY_F8"), GLUT_KEY_F8},
	{OS_TEXT("GLUT_KEY_F9"), GLUT_KEY_F9},
	{OS_TEXT("GLUT_KEY_F10"), GLUT_KEY_F10},
	{OS_TEXT("GLUT_KEY_F11"), GLUT_KEY_F11},
	{OS_TEXT("GLUT_KEY_F12"), GLUT_KEY_F12},
	{OS_TEXT("GLUT_KEY_LEFT"), GLUT_KEY_LEFT},
	{OS_TEXT("GLUT_KEY_UP"), GLUT_KEY_UP},
	{OS_TEXT("GLUT_KEY_RIGHT"), GLUT_KEY_RIGHT},
	{OS_TEXT("GLUT_KEY_DOWN"), GLUT_KEY_DOWN},
	{OS_TEXT("GLUT_KEY_PAGE_UP"), GLUT_KEY_PAGE_UP},
	{OS_TEXT("GLUT_KEY_PAGE_DOWN"), GLUT_KEY_PAGE_DOWN},
	{OS_TEXT("GLUT_KEY_HOME"), GLUT_KEY_HOME},
	{OS_TEXT("GLUT_KEY_END"), GLUT_KEY_END},
	{OS_TEXT("GLUT_KEY_INSERT"), GLUT_KEY_INSERT},
	{OS_TEXT("GLUT_LEFT"), GLUT_LEFT},
	{OS_TEXT("GLUT_ENTERED"), GLUT_ENTERED},
	{OS_TEXT("GLUT_MENU_NOT_IN_USE"), GLUT_MENU_NOT_IN_USE},
	{OS_TEXT("GLUT_MENU_IN_USE"), GLUT_MENU_IN_USE},
	{OS_TEXT("GLUT_NOT_VISIBLE"), GLUT_NOT_VISIBLE},
	{OS_TEXT("GLUT_VISIBLE"), GLUT_VISIBLE},
	{OS_TEXT("GLUT_HIDDEN"), GLUT_HIDDEN},
	{OS_TEXT("GLUT_FULLY_RETAINED"), GLUT_FULLY_RETAINED},
	{OS_TEXT("GLUT_PARTIALLY_RETAINED"), GLUT_PARTIALLY_RETAINED},
	{OS_TEXT("GLUT_FULLY_COVERED"), GLUT_FULLY_COVERED},
	{OS_TEXT("GLUT_RED"), GLUT_RED},
	{OS_TEXT("GLUT_GREEN"), GLUT_GREEN},
	{OS_TEXT("GLUT_BLUE"), GLUT_BLUE},
	{OS_TEXT("GLUT_NORMAL"), GLUT_NORMAL},
	{OS_TEXT("GLUT_OVERLAY"), GLUT_OVERLAY},
	{OS_TEXT("GLUT_WINDOW_X"), GLUT_WINDOW_X},
	{OS_TEXT("GLUT_WINDOW_Y"), GLUT_WINDOW_Y},
	{OS_TEXT("GLUT_WINDOW_WIDTH"), GLUT_WINDOW_WIDTH},
	{OS_TEXT("GLUT_WINDOW_HEIGHT"), GLUT_WINDOW_HEIGHT},
	{OS_TEXT("GLUT_WINDOW_BUFFER_SIZE"), GLUT_WINDOW_BUFFER_SIZE},
	{OS_TEXT("GLUT_WINDOW_STENCIL_SIZE"), GLUT_WINDOW_STENCIL_SIZE},
	{OS_TEXT("GLUT_WINDOW_DEPTH_SIZE"), GLUT_WINDOW_DEPTH_SIZE},
	{OS_TEXT("GLUT_WINDOW_RED_SIZE"), GLUT_WINDOW_RED_SIZE},
	{OS_TEXT("GLUT_WINDOW_GREEN_SIZE"), GLUT_WINDOW_GREEN_SIZE},
	{OS_TEXT("GLUT_WINDOW_BLUE_SIZE"), GLUT_WINDOW_BLUE_SIZE},
	{OS_TEXT("GLUT_WINDOW_ALPHA_SIZE"), GLUT_WINDOW_ALPHA_SIZE},
	{OS_TEXT("GLUT_WINDOW_ACCUM_RED_SIZE"), GLUT_WINDOW_ACCUM_RED_SIZE},
	{OS_TEXT("GLUT_WINDOW_ACCUM_GREEN_SIZE"), GLUT_WINDOW_ACCUM_GREEN_SIZE},
	{OS_TEXT("GLUT_WINDOW_ACCUM_BLUE_SIZE"), GLUT_WINDOW_ACCUM_BLUE_SIZE},
	{OS_TEXT("GLUT_WINDOW_ACCUM_ALPHA_SIZE"), GLUT_WINDOW_ACCUM_ALPHA_SIZE},
	{OS_TEXT("GLUT_WINDOW_DOUBLEBUFFER"), GLUT_WINDOW_DOUBLEBUFFER},
	{OS_TEXT("GLUT_WINDOW_RGBA"), GLUT_WINDOW_RGBA},
	{OS_TEXT("GLUT_WINDOW_PARENT"), GLUT_WINDOW_PARENT},
	{OS_TEXT("GLUT_WINDOW_NUM_CHILDREN"), GLUT_WINDOW_NUM_CHILDREN},
	{OS_TEXT("GLUT_WINDOW_COLORMAP_SIZE"), GLUT_WINDOW_COLORMAP_SIZE},
	{OS_TEXT("GLUT_WINDOW_NUM_SAMPLES"), GLUT_WINDOW_NUM_SAMPLES},
	{OS_TEXT("GLUT_WINDOW_STEREO"), GLUT_WINDOW_STEREO},
	{OS_TEXT("GLUT_WINDOW_CURSOR"), GLUT_WINDOW_CURSOR},
	{OS_TEXT("GLUT_SCREEN_WIDTH"), GLUT_SCREEN_WIDTH},
	{OS_TEXT("GLUT_SCREEN_HEIGHT"), GLUT_SCREEN_HEIGHT},
	{OS_TEXT("GLUT_SCREEN_WIDTH_MM"), GLUT_SCREEN_WIDTH_MM},
	{OS_TEXT("GLUT_SCREEN_HEIGHT_MM"), GLUT_SCREEN_HEIGHT_MM},
	{OS_TEXT("GLUT_MENU_NUM_ITEMS"), GLUT_MENU_NUM_ITEMS},
	{OS_TEXT("GLUT_DISPLAY_MODE_POSSIBLE"), GLUT_DISPLAY_MODE_POSSIBLE},
	{OS_TEXT("GLUT_INIT_WINDOW_X"), GLUT_INIT_WINDOW_X},
	{OS_TEXT("GLUT_INIT_WINDOW_Y"), GLUT_INIT_WINDOW_Y},
	{OS_TEXT("GLUT_INIT_WINDOW_WIDTH"), GLUT_INIT_WINDOW_WIDTH},
	{OS_TEXT("GLUT_INIT_WINDOW_HEIGHT"), GLUT_INIT_WINDOW_HEIGHT},
	{OS_TEXT("GLUT_INIT_DISPLAY_MODE"), GLUT_INIT_DISPLAY_MODE},
	{OS_TEXT("GLUT_ELAPSED_TIME"), GLUT_ELAPSED_TIME},
	{OS_TEXT("GLUT_WINDOW_FORMAT_ID"), GLUT_WINDOW_FORMAT_ID},
	{OS_TEXT("GLUT_HAS_KEYBOARD"), GLUT_HAS_KEYBOARD},
	{OS_TEXT("GLUT_HAS_MOUSE"), GLUT_HAS_MOUSE},
	{OS_TEXT("GLUT_HAS_SPACEBALL"), GLUT_HAS_SPACEBALL},
	{OS_TEXT("GLUT_HAS_DIAL_AND_BUTTON_BOX"), GLUT_HAS_DIAL_AND_BUTTON_BOX},
	{OS_TEXT("GLUT_HAS_TABLET"), GLUT_HAS_TABLET},
	{OS_TEXT("GLUT_NUM_MOUSE_BUTTONS"), GLUT_NUM_MOUSE_BUTTONS},
	{OS_TEXT("GLUT_NUM_SPACEBALL_BUTTONS"), GLUT_NUM_SPACEBALL_BUTTONS},
	{OS_TEXT("GLUT_NUM_BUTTON_BOX_BUTTONS"), GLUT_NUM_BUTTON_BOX_BUTTONS},
	{OS_TEXT("GLUT_NUM_DIALS"), GLUT_NUM_DIALS},
	{OS_TEXT("GLUT_NUM_TABLET_BUTTONS"), GLUT_NUM_TABLET_BUTTONS},
	{OS_TEXT("GLUT_DEVICE_IGNORE_KEY_REPEAT"), GLUT_DEVICE_IGNORE_KEY_REPEAT},
	{OS_TEXT("GLUT_DEVICE_KEY_REPEAT"), GLUT_DEVICE_KEY_REPEAT},
	{OS_TEXT("GLUT_HAS_JOYSTICK"), GLUT_HAS_JOYSTICK},
	{OS_TEXT("GLUT_OWNS_JOYSTICK"), GLUT_OWNS_JOYSTICK},
	{OS_TEXT("GLUT_JOYSTICK_BUTTONS"), GLUT_JOYSTICK_BUTTONS},
	{OS_TEXT("GLUT_JOYSTICK_AXES"), GLUT_JOYSTICK_AXES},
	{OS_TEXT("GLUT_JOYSTICK_POLL_RATE"), GLUT_JOYSTICK_POLL_RATE},
	{OS_TEXT("GLUT_OVERLAY_POSSIBLE"), GLUT_OVERLAY_POSSIBLE},
	{OS_TEXT("GLUT_LAYER_IN_USE"), GLUT_LAYER_IN_USE},
	{OS_TEXT("GLUT_HAS_OVERLAY"), GLUT_HAS_OVERLAY},
	{OS_TEXT("GLUT_TRANSPARENT_INDEX"), GLUT_TRANSPARENT_INDEX},
	{OS_TEXT("GLUT_NORMAL_DAMAGED"), GLUT_NORMAL_DAMAGED},
	{OS_TEXT("GLUT_OVERLAY_DAMAGED"), GLUT_OVERLAY_DAMAGED},
	{OS_TEXT("GLUT_VIDEO_RESIZE_POSSIBLE"), GLUT_VIDEO_RESIZE_POSSIBLE},
	{OS_TEXT("GLUT_VIDEO_RESIZE_IN_USE"), GLUT_VIDEO_RESIZE_IN_USE},
	{OS_TEXT("GLUT_VIDEO_RESIZE_X_DELTA"), GLUT_VIDEO_RESIZE_X_DELTA},
	{OS_TEXT("GLUT_VIDEO_RESIZE_Y_DELTA"), GLUT_VIDEO_RESIZE_Y_DELTA},
	{OS_TEXT("GLUT_VIDEO_RESIZE_WIDTH_DELTA"), GLUT_VIDEO_RESIZE_WIDTH_DELTA},
	{OS_TEXT("GLUT_VIDEO_RESIZE_HEIGHT_DELTA"), GLUT_VIDEO_RESIZE_HEIGHT_DELTA},
	{OS_TEXT("GLUT_VIDEO_RESIZE_X"), GLUT_VIDEO_RESIZE_X},
	{OS_TEXT("GLUT_VIDEO_RESIZE_Y"), GLUT_VIDEO_RESIZE_Y},
	{OS_TEXT("GLUT_VIDEO_RESIZE_WIDTH"), GLUT_VIDEO_RESIZE_WIDTH},
	{OS_TEXT("GLUT_VIDEO_RESIZE_HEIGHT"), GLUT_VIDEO_RESIZE_HEIGHT},
	{OS_TEXT("GLUT_NORMAL"), GLUT_NORMAL},
	{OS_TEXT("GLUT_OVERLAY"), GLUT_OVERLAY},
	{OS_TEXT("GLUT_ACTIVE_SHIFT"), GLUT_ACTIVE_SHIFT},
	{OS_TEXT("GLUT_ACTIVE_CTRL"), GLUT_ACTIVE_CTRL},
	{OS_TEXT("GLUT_ACTIVE_ALT"), GLUT_ACTIVE_ALT},
	{OS_TEXT("GLUT_CURSOR_RIGHT_ARROW"), GLUT_CURSOR_RIGHT_ARROW},
	{OS_TEXT("GLUT_CURSOR_LEFT_ARROW"), GLUT_CURSOR_LEFT_ARROW},
	{OS_TEXT("GLUT_CURSOR_INFO"), GLUT_CURSOR_INFO},
	{OS_TEXT("GLUT_CURSOR_DESTROY"), GLUT_CURSOR_DESTROY},
	{OS_TEXT("GLUT_CURSOR_HELP"), GLUT_CURSOR_HELP},
	{OS_TEXT("GLUT_CURSOR_CYCLE"), GLUT_CURSOR_CYCLE},
	{OS_TEXT("GLUT_CURSOR_SPRAY"), GLUT_CURSOR_SPRAY},
	{OS_TEXT("GLUT_CURSOR_WAIT"), GLUT_CURSOR_WAIT},
	{OS_TEXT("GLUT_CURSOR_TEXT"), GLUT_CURSOR_TEXT},
	{OS_TEXT("GLUT_CURSOR_CROSSHAIR"), GLUT_CURSOR_CROSSHAIR},
	{OS_TEXT("GLUT_CURSOR_UP_DOWN"), GLUT_CURSOR_UP_DOWN},
	{OS_TEXT("GLUT_CURSOR_LEFT_RIGHT"), GLUT_CURSOR_LEFT_RIGHT},
	{OS_TEXT("GLUT_CURSOR_TOP_SIDE"), GLUT_CURSOR_TOP_SIDE},
	{OS_TEXT("GLUT_CURSOR_BOTTOM_SIDE"), GLUT_CURSOR_BOTTOM_SIDE},
	{OS_TEXT("GLUT_CURSOR_LEFT_SIDE"), GLUT_CURSOR_LEFT_SIDE},
	{OS_TEXT("GLUT_CURSOR_RIGHT_SIDE"), GLUT_CURSOR_RIGHT_SIDE},
	{OS_TEXT("GLUT_CURSOR_TOP_LEFT_CORNER"), GLUT_CURSOR_TOP_LEFT_CORNER},
	{OS_TEXT("GLUT_CURSOR_TOP_RIGHT_CORNER"), GLUT_CURSOR_TOP_RIGHT_CORNER},
	{OS_TEXT("GLUT_CURSOR_BOTTOM_RIGHT_CORNER"), GLUT_CURSOR_BOTTOM_RIGHT_CORNER},
	{OS_TEXT("GLUT_CURSOR_BOTTOM_LEFT_CORNER"), GLUT_CURSOR_BOTTOM_LEFT_CORNER},
	{OS_TEXT("GLUT_CURSOR_INHERIT"), GLUT_CURSOR_INHERIT},
	{OS_TEXT("GLUT_CURSOR_NONE"), GLUT_CURSOR_NONE},
	{OS_TEXT("GLUT_CURSOR_FULL_CROSSHAIR"), GLUT_CURSOR_FULL_CROSSHAIR},
	{OS_TEXT("GLUT_KEY_REPEAT_OFF"), GLUT_KEY_REPEAT_OFF},
	{OS_TEXT("GLUT_KEY_REPEAT_ON"), GLUT_KEY_REPEAT_ON},
	{OS_TEXT("GLUT_KEY_REPEAT_DEFAULT"), GLUT_KEY_REPEAT_DEFAULT},
	{OS_TEXT("GLUT_JOYSTICK_BUTTON_A"), GLUT_JOYSTICK_BUTTON_A},
	{OS_TEXT("GLUT_JOYSTICK_BUTTON_B"), GLUT_JOYSTICK_BUTTON_B},
	{OS_TEXT("GLUT_JOYSTICK_BUTTON_C"), GLUT_JOYSTICK_BUTTON_C},
	{OS_TEXT("GLUT_JOYSTICK_BUTTON_D"), GLUT_JOYSTICK_BUTTON_D},
	{OS_TEXT("GLUT_GAME_MODE_ACTIVE"), GLUT_GAME_MODE_ACTIVE},
	{OS_TEXT("GLUT_GAME_MODE_POSSIBLE"), GLUT_GAME_MODE_POSSIBLE},
	{OS_TEXT("GLUT_GAME_MODE_WIDTH"), GLUT_GAME_MODE_WIDTH},
	{OS_TEXT("GLUT_GAME_MODE_HEIGHT"), GLUT_GAME_MODE_HEIGHT},
	{OS_TEXT("GLUT_GAME_MODE_PIXEL_DEPTH"), GLUT_GAME_MODE_PIXEL_DEPTH},
	{OS_TEXT("GLUT_GAME_MODE_REFRESH_RATE"), GLUT_GAME_MODE_REFRESH_RATE},
	{OS_TEXT("GLUT_GAME_MODE_DISPLAY_CHANGED"), GLUT_GAME_MODE_DISPLAY_CHANGED},
	{OS_TEXT("GLUT_STROKE_ROMAN"), 0},
	{OS_TEXT("GLUT_STROKE_MONO_ROMAN"), 1},
	{OS_TEXT("GLUT_BITMAP_9_BY_15"), 2},
	{OS_TEXT("GLUT_BITMAP_8_BY_13"), 3},
	{OS_TEXT("GLUT_BITMAP_TIMES_ROMAN_10"), 4},
	{OS_TEXT("GLUT_BITMAP_TIMES_ROMAN_24"), 5},
	{OS_TEXT("GLUT_BITMAP_HELVETICA_10"), 6},
	{OS_TEXT("GLUT_BITMAP_HELVETICA_12"), 7},
	{OS_TEXT("GLUT_BITMAP_HELVETICA_18"), 8},
	{},
};
