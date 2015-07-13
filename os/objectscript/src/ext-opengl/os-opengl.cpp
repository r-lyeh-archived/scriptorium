#include "../objectscript.h"
#include "../os-binder.h"
#include "os-opengl.h"

#include <windows.h>
#include <gl\GL.h>
#include <glut.h>

namespace ObjectScript {

template <class T> 
struct GLConstArrayWrapper
{
	OS * os;
	T * src;
	int src_size;
	GLConstArrayWrapper(OS * p_os, int offs, int req_values = 0): os(p_os)
	{
		bool valid_type = os->isArray(offs) || os->isObject(offs); 
		int param_size = valid_type ? os->getLen(offs) : 0;
		
		src_size = req_values > 0 ? req_values : param_size; 
		if(src_size < 1){
			OS_ASSERT(false);
			src = NULL;
			return;
		}
		src = (T*)os->malloc(sizeof(*src) * src_size OS_DBG_FILEPOS);

		int i = 0;
		os->pushStackValue(offs);
		for(; i < param_size && os->nextIteratorStep(); i++){
			src[i] = (T)os->toNumber(-1);
			os->pop(2);
		}
		os->pop();
		
		for(; i < src_size; i++){
			src[i] = (T)0;
		}
	}

	~GLConstArrayWrapper()
	{
		os->free(src);
	}

	const T * toV() const { return src; }
};

#include "os-opengl.inc"

struct OpenglCache
{
	static OpenglCache * instance;
	
	OS * os;
	int glutDisplayFunc;
	int glutIdleFunc;
	int glutKeyboardFunc;
	int glutReshapeFunc;
	int glutMouseFunc;
	int glutMotionFunc;
	int glutPassiveMotionFunc;
	int glutEntryFunc;
	int glutVisibilityFunc;
	int glutMenuStateFunc;
	int glutCreateMenu;
#if (GLUT_API_VERSION >= 2)
	int glutSpecialFunc;
	int glutSpaceballMotionFunc;
	int glutSpaceballRotateFunc;
	int glutSpaceballButtonFunc;
	int glutButtonBoxFunc;
	int glutDialsFunc;
	int glutTabletMotionFunc;
	int glutTabletButtonFunc;
#if (GLUT_API_VERSION >= 3)
	int glutMenuStatusFunc;
	int glutOverlayDisplayFunc;
#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
	int glutWindowStatusFunc;
#endif
#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
	int glutKeyboardUpFunc;
	int glutSpecialUpFunc;
	int glutJoystickFunc;
#endif
#endif
#endif

	struct GlutFunc
	{
		int id;
		int func;

		GlutFunc(OS * os, int p_id, int f)
		{
			id = p_id;
			os->retainValueById(func = f);
		}

		int retain(OS * os, int p_id, int f)
		{
			id = p_id;
			os->releaseValueById(func);
			os->retainValueById(func = f);
			return id;
		}

		void release(OS * os)
		{
			os->releaseValueById(func);
			func = 0;
			id = 0;
		}

		bool isFree()
		{
			return id == 0;
		}
	};

	OS::Vector<GlutFunc> timers;
	int timerCounter;

	void freeAllTimers()
	{
		for(int i = 0; i < timers.count; i++){
			timers[i].release(os);
		}
		os->vectorClear(timers);
	}

	int registerTimer(int f)
	{
		for(int i = 0; i < timers.count; i++){
			if(timers[i].isFree()){
				return timers[i].retain(os, ++timerCounter, f);
			}
		}
		os->vectorAddItem(timers, GlutFunc(os, ++timerCounter, f) OS_DBG_FILEPOS);
		return timerCounter;
	}

	GlutFunc * getTimerById(int id)
	{
		for(int i = 0; i < timers.count; i++){
			GlutFunc * timer = &timers[i];
			if(timer->id == id){
				return timer;
			}
		}
		return NULL;
	}

	/*
	OS::Vector<GlutFunc> menus;

	void freeAllMenus()
	{
		for(int i = 0; i < menus.count; i++){
			menus[i].release(os);
		}
		os->vectorClear(menus);
	}

	int registerMenu(int id, int f)
	{
		int i;
		for(i = 0; i < menus.count; i++){
			if(menus[i].id == id){
				return menus[i].retain(os, id, f);
			}
		}
		for(i = 0; i < menus.count; i++){
			if(menus[i].isFree()){
				return menus[i].retain(os, id, f);
			}
		}
		os->vectorAddItem(menus, GlutFunc(os, id, f) OS_DBG_FILEPOS);
		return id;
	}

	GlutFunc * getMenuById(int id)
	{
		for(int i = 0; i < menus.count; i++){
			GlutFunc * menu = &menus[i];
			if(menu->id == id){
				return menu;
			}
		}
		return NULL;
	}
	*/

	static void triggerError(OS * os, const OS_CHAR * msg)
	{
		os->setException(msg);
	}

	OpenglCache(OS * p_os)
	{
		OS_ASSERT(!instance);
		instance = this;
		os = p_os;
		timerCounter = 0;
		glutDisplayFunc = 0;
		glutIdleFunc = 0;
		glutKeyboardFunc = 0;
		glutReshapeFunc = 0;
		glutMouseFunc = 0;
		glutMotionFunc = 0;
		glutPassiveMotionFunc = 0;
		glutEntryFunc = 0;
		glutVisibilityFunc = 0;
		glutMenuStateFunc = 0;
		glutCreateMenu = 0;
	#if (GLUT_API_VERSION >= 2)
		glutSpecialFunc = 0;
		glutSpaceballMotionFunc = 0;
		glutSpaceballRotateFunc = 0;
		glutSpaceballButtonFunc = 0;
		glutButtonBoxFunc = 0;
		glutDialsFunc = 0;
		glutTabletMotionFunc = 0;
		glutTabletButtonFunc = 0;
	#if (GLUT_API_VERSION >= 3)
		glutMenuStatusFunc = 0;
		glutOverlayDisplayFunc = 0;
	#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
		glutWindowStatusFunc = 0;
	#endif
	#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
		glutKeyboardUpFunc = 0;
		glutSpecialUpFunc = 0;
		glutJoystickFunc = 0;
	#endif
	#endif
	#endif
	}

	~OpenglCache()
	{
		OS_ASSERT(instance == this);
		freeAllTimers();
		// freeAllMenus();
		os->releaseValueById(glutDisplayFunc);
		os->releaseValueById(glutIdleFunc);
		os->releaseValueById(glutKeyboardFunc);
		os->releaseValueById(glutReshapeFunc);
		os->releaseValueById(glutMouseFunc);
		os->releaseValueById(glutMotionFunc);
		os->releaseValueById(glutPassiveMotionFunc);
		os->releaseValueById(glutEntryFunc);
		os->releaseValueById(glutVisibilityFunc);
		os->releaseValueById(glutMenuStateFunc);
		os->releaseValueById(glutCreateMenu);
	#if (GLUT_API_VERSION >= 2)
		os->releaseValueById(glutSpecialFunc);
		os->releaseValueById(glutSpaceballMotionFunc);
		os->releaseValueById(glutSpaceballRotateFunc);
		os->releaseValueById(glutSpaceballButtonFunc);
		os->releaseValueById(glutButtonBoxFunc);
		os->releaseValueById(glutDialsFunc);
		os->releaseValueById(glutTabletMotionFunc);
		os->releaseValueById(glutTabletButtonFunc);
	#if (GLUT_API_VERSION >= 3)
		os->releaseValueById(glutMenuStatusFunc);
		os->releaseValueById(glutOverlayDisplayFunc);
	#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
		os->releaseValueById(glutWindowStatusFunc);
	#endif
	#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
		os->releaseValueById(glutKeyboardUpFunc);
		os->releaseValueById(glutSpecialUpFunc);
		os->releaseValueById(glutJoystickFunc);
	#endif
	#endif
	#endif
		instance = NULL;
	}

	static int __construct(OS * os, int params, int, int, void*)
	{
		triggerError(os, OS_TEXT("you should not create new instance of OpenglCache"));
		return 0;
	}

	static int __glutBitmapCharacter(OS * os, int params, int, int, void*)
	{
		if(params >= 2){
			if(os->getType(-params + 0) == OS_VALUE_TYPE_NUMBER){
				int std_font = os->toInt(-params + 0);
				glutBitmapCharacter((void*)std_font, os->toInt(-params + 1));
				return 0;
			}
			OS::String str = os->toString(-params + 0);
			glutBitmapCharacter((void*)str.toChar(), os->toInt(-params + 1));
		}
		return 0;
	}

	void saveFunc(int& saved_func_id, int id)
	{
		os->releaseValueById(saved_func_id);
		os->retainValueById(saved_func_id = id);
	}

#define OS_GLUT_DECL_FUNC(name) \
	static void wrapper_##name() \
	{ \
		OS_ASSERT(instance); \
		if(instance){ \
			OS * os = instance->os; \
			os->pushValueById(instance->name); \
			os->pushNull(); \
			os->call(0, 0); \
		} \
	} \
	\
	static int __##name(OS * os, int params, int, int, void*) \
	{ \
		OS_ASSERT(instance); \
		if(instance && params > 0){ \
			if(os->isNull(-params)){ \
				instance->saveFunc(instance->name, 0); \
				::name(NULL); \
				return 0; \
			} \
			if(!os->isFunction(-params)){ \
			os->setException(OS_TEXT(#name " requires function argument")); \
				return 0; \
			} \
			instance->saveFunc(instance->name, os->getValueId(-params)); \
			::name(wrapper_##name); \
		} \
		return 0; \
	}

#define OS_GLUT_DECL_FUNC_N(name, p) \
	static void wrapper_##name p \
	{ \
		OS_ASSERT(instance); \
		if(instance){ \
			OS * os = instance->os; \
			os->pushValueById(instance->name); \
			os->pushNull(); \
			os->pushNumber(p1); \
			os->call(1, 0); \
		} \
	} \
	\
	static int __##name(OS * os, int params, int, int, void*) \
	{ \
		OS_ASSERT(instance); \
		if(instance && params > 0){ \
			if(os->isNull(-params)){ \
				instance->saveFunc(instance->name, 0); \
				::name(NULL); \
				return 0; \
			} \
			if(!os->isFunction(-params)){ \
			os->setException(OS_TEXT(#name " requires function argument")); \
				return 0; \
			} \
			instance->saveFunc(instance->name, os->getValueId(-params)); \
			::name(wrapper_##name); \
		} \
		return 0; \
	}

#define OS_GLUT_DECL_FUNC_NN(name, p) \
	static void wrapper_##name p \
	{ \
		OS_ASSERT(instance); \
		if(instance){ \
			OS * os = instance->os; \
			os->pushValueById(instance->name); \
			os->pushNull(); \
			os->pushNumber(p1); \
			os->pushNumber(p2); \
			os->call(2, 0); \
		} \
	} \
	\
	static int __##name(OS * os, int params, int, int, void*) \
	{ \
		OS_ASSERT(instance); \
		if(instance && params > 0){ \
			if(os->isNull(-params)){ \
				instance->saveFunc(instance->name, 0); \
				::name(NULL); \
				return 0; \
			} \
			if(!os->isFunction(-params)){ \
			os->setException(OS_TEXT(#name " requires function argument")); \
				return 0; \
			} \
			instance->saveFunc(instance->name, os->getValueId(-params)); \
			::name(wrapper_##name); \
		} \
		return 0; \
	}

#define OS_GLUT_DECL_FUNC_NNN(name, p) \
	static void wrapper_##name p \
	{ \
		OS_ASSERT(instance); \
		if(instance){ \
			OS * os = instance->os; \
			os->pushValueById(instance->name); \
			os->pushNull(); \
			os->pushNumber(p1); \
			os->pushNumber(p2); \
			os->pushNumber(p3); \
			os->call(3, 0); \
		} \
	} \
	\
	static int __##name(OS * os, int params, int, int, void*) \
	{ \
		OS_ASSERT(instance); \
		if(instance && params > 0){ \
			if(os->isNull(-params)){ \
				instance->saveFunc(instance->name, 0); \
				::name(NULL); \
				return 0; \
			} \
			if(!os->isFunction(-params)){ \
			os->setException(OS_TEXT(#name " requires function argument")); \
				return 0; \
			} \
			instance->saveFunc(instance->name, os->getValueId(-params)); \
			::name(wrapper_##name); \
		} \
		return 0; \
	}

#define OS_GLUT_DECL_FUNC_NNNN(name, p) \
	static void wrapper_##name p \
	{ \
		OS_ASSERT(instance); \
		if(instance){ \
			OS * os = instance->os; \
			os->pushValueById(instance->name); \
			os->pushNull(); \
			os->pushNumber(p1); \
			os->pushNumber(p2); \
			os->pushNumber(p3); \
			os->pushNumber(p4); \
			os->call(4, 0); \
		} \
	} \
	\
	static int __##name(OS * os, int params, int, int, void*) \
	{ \
		OS_ASSERT(instance); \
		if(instance && params > 0){ \
			if(os->isNull(-params)){ \
				instance->saveFunc(instance->name, 0); \
				::name(NULL); \
				return 0; \
			} \
			if(!os->isFunction(-params)){ \
			os->setException(OS_TEXT(#name " requires function argument")); \
				return 0; \
			} \
			instance->saveFunc(instance->name, os->getValueId(-params)); \
			::name(wrapper_##name); \
		} \
		return 0; \
	}

	static void wrapper_glutTimerFunc(int p1)
	{
		OS_ASSERT(instance);
		if(instance){
			OS * os = instance->os;
			GlutFunc * timer = instance->getTimerById(p1);
			if(timer){
				os->pushValueById(timer->func);
				os->pushNull();
				timer->release(os);
				os->call(0, 0);
			}
		}
	}
	
	static int __glutTimerFunc(OS * os, int params, int, int, void*)
	{
		OS_ASSERT(instance);
		if(instance && params >= 2){
			if(os->isNull(-params + 1)){
				return 0;
			}
			if(!os->isFunction(-params + 1)){
				os->setException(OS_TEXT("glutTimerFunc requires function argument"));
				return 0;
			}
			int id = instance->registerTimer(os->getValueId(-params + 1));
			int ms = os->toInt(-params + 0);
			if(ms < 1) ms = 1;
			::glutTimerFunc((unsigned int)ms, wrapper_glutTimerFunc, id);
		}
		return 0;
	}

	/*
	static void wrapper_glutCreateMenu(int p1)
	{
		OS_ASSERT(instance);
		if(instance){
			OS * os = instance->os;
			GlutFunc * menu = instance->getMenuById(p1);
			if(timer){
				os->pushValueById(timer->func);
				os->pushNull();
				timer->release(os);
				os->call(0, 0);
			}
		}
	}
	
	static int __glutCreateMenu(OS * os, int params, int, int, void*)
	{
		OS_ASSERT(instance);
		if(instance && params >= 1){
			if(os->isNull(-params + 0)){
				return 0;
			}
			if(!os->isFunction(-params + 0)){
				os->setException(OS_TEXT("glutCreateMenu requires function argument"));
				return 0;
			}
			int id = ::glutCreateMenu(wrapper_glutCreateMenu);
			instance->registerMenu(id, os->getValueId(-params + 0));
		}
		return 0;
	}
	*/

	OS_GLUT_DECL_FUNC(glutDisplayFunc)
	OS_GLUT_DECL_FUNC_NN(glutReshapeFunc, (int p1, int p2))
	OS_GLUT_DECL_FUNC(glutIdleFunc)
	OS_GLUT_DECL_FUNC_NNN(glutKeyboardFunc, (unsigned char p1, int p2, int p3))
	OS_GLUT_DECL_FUNC_NNNN(glutMouseFunc, (int p1, int p2, int p3, int p4))
	OS_GLUT_DECL_FUNC_NN(glutMotionFunc, (int p1, int p2))
	OS_GLUT_DECL_FUNC_NN(glutPassiveMotionFunc, (int p1, int p2))
	OS_GLUT_DECL_FUNC_N(glutEntryFunc, (int p1))
	OS_GLUT_DECL_FUNC_N(glutVisibilityFunc, (int p1))
	OS_GLUT_DECL_FUNC_N(glutMenuStateFunc, (int p1))
	OS_GLUT_DECL_FUNC_N(glutCreateMenu, (int p1))

	#if (GLUT_API_VERSION >= 2)
	OS_GLUT_DECL_FUNC_NNN(glutSpecialFunc, (int p1, int p2, int p3));
	OS_GLUT_DECL_FUNC_NNN(glutSpaceballMotionFunc, (int p1, int p2, int p3));
	OS_GLUT_DECL_FUNC_NNN(glutSpaceballRotateFunc, (int p1, int p2, int p3));
	OS_GLUT_DECL_FUNC_NN(glutSpaceballButtonFunc, (int p1, int p2));
	OS_GLUT_DECL_FUNC_NN(glutButtonBoxFunc, (int p1, int p2));
	OS_GLUT_DECL_FUNC_NN(glutDialsFunc, (int p1, int p2));
	OS_GLUT_DECL_FUNC_NN(glutTabletMotionFunc, (int p1, int p2));
	OS_GLUT_DECL_FUNC_NNNN(glutTabletButtonFunc, (int p1, int p2, int p3, int p4));
	#if (GLUT_API_VERSION >= 3)
	OS_GLUT_DECL_FUNC_NNN(glutMenuStatusFunc, (int p1, int p2, int p3));
	OS_GLUT_DECL_FUNC(glutOverlayDisplayFunc);
	#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
	OS_GLUT_DECL_FUNC_N(glutWindowStatusFunc, (int p1));
	#endif
	#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
	OS_GLUT_DECL_FUNC_NNN(glutKeyboardUpFunc, (unsigned char p1, int p2, int p3));
	OS_GLUT_DECL_FUNC_NNN(glutSpecialUpFunc, (int p1, int p2, int p3));

	static void wrapper_glutJoystickFunc(unsigned int p1, int p2, int p3, int p4)
	{
		OS_ASSERT(instance);
		if(instance){
			OS * os = instance->os;
			os->pushValueById(instance->glutJoystickFunc);
			os->pushNull();
			os->pushNumber(p1);
			os->pushNumber(p2);
			os->pushNumber(p3);
			os->pushNumber(p4);
			os->call(4, 0);
		}
	}
	
	static int __glutJoystickFunc(OS * os, int params, int, int, void*)
	{
		OS_ASSERT(instance);
		if(instance && params >= 1){
			if(os->isNull(-params + 0)){
				return 0;
			}
			if(!os->isFunction(-params + 0)){
				os->setException(OS_TEXT("glutJoystickFunc requires function argument"));
				return 0;
			}
			instance->saveFunc(instance->glutJoystickFunc, os->getValueId(-params));
			::glutJoystickFunc(wrapper_glutJoystickFunc, os->toInt(-params + 1));
		}
		return 0;
	}
	#endif
	#endif
	#endif

};

OpenglCache * OpenglCache::instance = NULL;

template <> struct CtypeName<OpenglCache>{ static const OS_CHAR * getName(){ return OS_TEXT("OpenglCache"); } };
template <> struct CtypeValue<OpenglCache*>: public CtypeUserClass<OpenglCache*>{};
template <> struct UserDataDestructor<OpenglCache>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<OpenglCache*>((OpenglCache*)data));
		OpenglCache * buf = (OpenglCache*)data;
		buf->~OpenglCache();
		os->free(buf);
	}
};

void initOpenglExtension(OS* os)
{
	os->pushStackValue(OS_REGISTER_USERPOOL);
	os->pushString(CtypeName<OpenglCache>::getName());
	os->pushUserPointer(CtypeId<OpenglCache>::getId(), new (os->malloc(sizeof(OpenglCache) OS_DBG_FILEPOS)) OpenglCache(os),  UserDataDestructor<OpenglCache>::dtor);
	OS::FuncDef hidden_funcs[] = {
		{OS_TEXT("__construct"), OpenglCache::__construct},
		{}
	};
	os->setFuncs(hidden_funcs);
	os->setProperty();

	os->pushGlobals();
	os->setFuncs(oslib_gl_funcs);
	os->setNumbers(oslib_gl_numbers);

	OS::FuncDef funcs[] = {
		{OS_TEXT("glutBitmapCharacter"), OpenglCache::__glutBitmapCharacter},
		{OS_TEXT("glutDisplayFunc"), OpenglCache::__glutDisplayFunc},
		{OS_TEXT("glutReshapeFunc"), OpenglCache::__glutReshapeFunc},
		{OS_TEXT("glutIdleFunc"), OpenglCache::__glutIdleFunc},
		{OS_TEXT("glutKeyboardFunc"), OpenglCache::__glutKeyboardFunc},
		{OS_TEXT("glutMouseFunc"), OpenglCache::__glutMouseFunc},
		{OS_TEXT("glutMotionFunc"), OpenglCache::__glutMotionFunc},
		{OS_TEXT("glutPassiveMotionFunc"), OpenglCache::__glutPassiveMotionFunc},
		{OS_TEXT("glutEntryFunc"), OpenglCache::__glutEntryFunc},
		{OS_TEXT("glutVisibilityFunc"), OpenglCache::__glutVisibilityFunc},
		{OS_TEXT("glutMenuStateFunc"), OpenglCache::__glutMenuStateFunc},
		{OS_TEXT("glutTimerFunc"), OpenglCache::__glutTimerFunc},
		{OS_TEXT("glutCreateMenu"), OpenglCache::__glutCreateMenu},
	#if (GLUT_API_VERSION >= 2)
		{OS_TEXT("glutSpecialFunc"), OpenglCache::__glutSpecialFunc},
		{OS_TEXT("glutSpaceballMotionFunc"), OpenglCache::__glutSpaceballMotionFunc},
		{OS_TEXT("glutSpaceballRotateFunc"), OpenglCache::__glutSpaceballRotateFunc},
		{OS_TEXT("glutSpaceballButtonFunc"), OpenglCache::__glutSpaceballButtonFunc},
		{OS_TEXT("glutButtonBoxFunc"), OpenglCache::__glutButtonBoxFunc},
		{OS_TEXT("glutDialsFunc"), OpenglCache::__glutDialsFunc},
		{OS_TEXT("glutTabletMotionFunc"), OpenglCache::__glutTabletMotionFunc},
		{OS_TEXT("glutTabletButtonFunc"), OpenglCache::__glutTabletButtonFunc},
	#if (GLUT_API_VERSION >= 3)
		{OS_TEXT("glutMenuStatusFunc"), OpenglCache::__glutMenuStatusFunc},
		{OS_TEXT("glutOverlayDisplayFunc"), OpenglCache::__glutOverlayDisplayFunc},
	#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
		{OS_TEXT("glutWindowStatusFunc"), OpenglCache::__glutWindowStatusFunc},
	#endif
	#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
		{OS_TEXT("glutKeyboardUpFunc"), OpenglCache::__glutKeyboardUpFunc},
		{OS_TEXT("glutSpecialUpFunc"), OpenglCache::__glutSpecialUpFunc},
		{OS_TEXT("glutJoystickFunc"), OpenglCache::__glutJoystickFunc},
	#endif
	#endif
	#endif
		{}
	};
	os->setFuncs(funcs);

	os->pop();
}

} // namespace ObjectScript
