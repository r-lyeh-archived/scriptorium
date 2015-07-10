public:
static variable merge(variable &arg, const variable &def)
{
	variable m(RARRAY);
	size_t i = 0;
	for (; i < arg.length(); ++i)
		m.push(arg[i]);
	for (; i < def.length(); ++i)
		m.push(def[i]);
	return m.pointer();
}
private:
#define FV(n,t,g) class vBCFunctionV##n:public vBase{void(*x)t;rsv d;public:vBCFunctionV##n(void(*f)t,const variable &de):d(de){x=f;}\
	PSL_MEMORY_MANAGER(vBCFunctionV##n)\
	Type type()const{return BCFUNCTION;}vBase *clone(){return new vBCFunctionV##n(x, d);}\
	bool toBool()const{return true;}size_t length()const{return 1;}\
	void prepare(Environment &env, Variable *v){variable a_ = env.pop();variable a = merge(a_, (variable)d);x g;env.push(a);}\
	rsv call(Environment &env, variable &a_, Variable *v){variable a = merge(a_, (variable)d);x g;return a;}};
FV(0,(),())
template<class A0>FV(1,(A0),(a))
template<class A0,class A1>FV(2,(A0,A1),(a[0],a[1]))
template<class A0,class A1,class A2>FV(3,(A0,A1,A2),(a[0],a[1],a[2]))
template<class A0,class A1,class A2,class A3>FV(4,(A0,A1,A2,A3),(a[0],a[1],a[2],a[3]))
template<class A0,class A1,class A2,class A3,class A4>FV(5,(A0,A1,A2,A3,A4),(a[0],a[1],a[2],a[3],a[4]))
template<class A0,class A1,class A2,class A3,class A4,class A5>FV(6,(A0,A1,A2,A3,A4,A5),(a[0],a[1],a[2],a[3],a[4],a[5]))
#undef FV
#define FR(n,t,g) class vBCFunctionR##n:public vBase{R(*x)t;rsv d;public:vBCFunctionR##n(R(*f)t,const variable &de):d(de){x=f;}\
	PSL_MEMORY_MANAGER(vBCFunctionR##n)\
	Type type()const{return BCFUNCTION;}vBase *clone(){return new vBCFunctionR##n(x, d);}\
	bool toBool()const{return true;}size_t length()const{return 1;}\
	void prepare(Environment &env, Variable *v){variable a_ = env.pop();variable a = merge(a_, (variable)d);variable r = x g;env.push(r);}\
	rsv call(Environment &env, variable &a_, Variable *v){variable a = merge(a_, (variable)d);variable r = x g;return r;}};
template<class R>FR(0,(),())
template<class R, class A0>FR(1,(A0),(a))
template<class R, class A0,class A1>FR(2,(A0,A1),(a[0],a[1]))
template<class R, class A0,class A1,class A2>FR(3,(A0,A1,A2),(a[0],a[1],a[2]))
template<class R, class A0,class A1,class A2,class A3>FR(4,(A0,A1,A2,A3),(a[0],a[1],a[2],a[3]))
template<class R, class A0,class A1,class A2,class A3,class A4>FR(5,(A0,A1,A2,A3,A4),(a[0],a[1],a[2],a[3],a[4]))
template<class R, class A0,class A1,class A2,class A3,class A4,class A5>FR(6,(A0,A1,A2,A3,A4,A5),(a[0],a[1],a[2],a[3],a[4],a[5]))
#undef FR
static vBase *BCFunction(void(*f)(), const variable &d){return new vBCFunctionV0(f, d);}
template<class A0>
static vBase *BCFunction(void(*f)(A0), const variable &d){return new vBCFunctionV1<A0>(f, d);}
template<class A0, class A1>
static vBase *BCFunction(void(*f)(A0,A1), const variable &d){return new vBCFunctionV2<A0,A1>(f, d);}
template<class A0, class A1, class A2>
static vBase *BCFunction(void(*f)(A0,A1,A2), const variable &d){return new vBCFunctionV3<A0,A1,A2>(f, d);}
template<class A0, class A1, class A2, class A3>
static vBase *BCFunction(void(*f)(A0,A1,A2,A3), const variable &d){return new vBCFunctionV4<A0,A1,A2,A3>(f, d);}
template<class A0, class A1, class A2, class A3, class A4>
static vBase *BCFunction(void(*f)(A0,A1,A2,A3,A4), const variable &d){return new vBCFunctionV5<A0,A1,A2,A3,A4>(f, d);}
template<class A0, class A1, class A2, class A3, class A4, class A5>
static vBase *BCFunction(void(*f)(A0,A1,A2,A3,A4,A5), const variable &d){return new vBCFunctionV6<A0,A1,A2,A3,A4,A5>(f, d);}
template<class R>
static vBase *BCFunction(R(*f)(), const variable &d){return new vBCFunctionR0<R>(f, d);}
template<class R, class A0>
static vBase *BCFunction(R(*f)(A0), const variable &d){return new vBCFunctionR1<R,A0>(f, d);}
template<class R, class A0, class A1>
static vBase *BCFunction(R(*f)(A0,A1), const variable &d){return new vBCFunctionR2<R,A0,A1>(f, d);}
template<class R, class A0, class A1, class A2>
static vBase *BCFunction(R(*f)(A0,A1,A2), const variable &d){return new vBCFunctionR3<R,A0,A1,A2>(f, d);}
template<class R, class A0, class A1, class A2, class A3>
static vBase *BCFunction(R(*f)(A0,A1,A2,A3), const variable &d){return new vBCFunctionR4<R,A0,A1,A2,A3>(f, d);}
template<class R, class A0, class A1, class A2, class A3, class A4>
static vBase *BCFunction(R(*f)(A0,A1,A2,A3,A4), const variable &d){return new vBCFunctionR5<R,A0,A1,A2,A3,A4>(f, d);}
template<class R, class A0, class A1, class A2, class A3, class A4, class A5>
static vBase *BCFunction(R(*f)(A0,A1,A2,A3,A4,A5), const variable &d){return new vBCFunctionR6<R,A0,A1,A2,A3,A4,A5>(f, d);}

#define MV(n,t,g) class vCCMethodV##n : public vBase{void(C::*x)t;Variable *this_v;rsv d;public:\
	PSL_MEMORY_MANAGER(vCCMethodV##n)\
	vCCMethodV##n(void(C::*m)t,const variable &de):d(de){x=m;this_v=NULL;}vBase *clone(){return new vCCMethodV##n(x, d);}\
	Type type()const{return CCMETHOD;}void push(Variable *v){this_v=v;}\
	vBase *substitution(Variable *v){if (isMethod(v)){vBase *c = v->bclone();c->push(this_v);delete this;return c;}return this;}\
	bool toBool()const{return true;}size_t length()const{return 1;}\
	Variable *index(size_t i){return i?this_v:NULL;}\
	void prepare(Environment &env, Variable *v){variable a_ = env.pop();variable a = merge(a_, (variable)d);C*i=static_cast<C*>(this_v->index(0)->toPointer());(i->*x)g;env.push(a);}\
	rsv call(Environment &env, variable &a_, Variable *v){variable a = merge(a_, (variable)d);C*i=static_cast<C*>(this_v->index(0)->toPointer());(i->*x)g;return a;}};
template<class C>MV(0,(),())
template<class C>MV(C0,()const,())
template<class C, class A0>MV(1,(A0),(a))
template<class C, class A0>MV(C1,(A0)const,(a))
template<class C, class A0, class A1>MV(2,(A0,A1),(a[0],a[1]))
template<class C, class A0, class A1>MV(C2,(A0,A1)const,(a[0],a[1]))
template<class C, class A0, class A1, class A2>MV(3,(A0,A1,A2),(a[0],a[1],a[2]))
template<class C, class A0, class A1, class A2>MV(C3,(A0,A1,A2)const,(a[0],a[1],a[2]))
template<class C, class A0, class A1, class A2, class A3>MV(4,(A0,A1,A2,A3),(a[0],a[1],a[2],a[3]))
template<class C, class A0, class A1, class A2, class A3>MV(C4,(A0,A1,A2,A3)const,(a[0],a[1],a[2],a[3]))
template<class C, class A0, class A1, class A2, class A3, class A4>MV(5,(A0,A1,A2,A3,A4),(a[0],a[1],a[2],a[3],a[4]))
template<class C, class A0, class A1, class A2, class A3, class A4>MV(C5,(A0,A1,A2,A3,A4)const,(a[0],a[1],a[2],a[3],a[4]))
template<class C, class A0, class A1, class A2, class A3, class A4, class A5>MV(6,(A0,A1,A2,A3,A4,A5),(a[0],a[1],a[2],a[3],a[4],a[5]))
template<class C, class A0, class A1, class A2, class A3, class A4, class A5>MV(C6,(A0,A1,A2,A3,A4,A5)const,(a[0],a[1],a[2],a[3],a[4],a[5]))
#undef MV
#define MR(n,t,g) class vCCMethodR##n : public vBase{R(C::*x)t;Variable *this_v;rsv d;public:\
	PSL_MEMORY_MANAGER(vCCMethodR##n)\
	vCCMethodR##n(R(C::*m)t, const variable &de):d(de){x=m;this_v=NULL;}vBase *clone(){return new vCCMethodR##n(x, d);}\
	Type type()const{return CCMETHOD;}void push(Variable *v){this_v=v;}\
	bool toBool()const{return true;}size_t length()const{return 1;}\
	Variable *index(size_t i){return i?this_v:NULL;}\
	void prepare(Environment &env, Variable *v){variable a_ = env.pop();variable a = merge(a_, (variable)d);C*i=static_cast<C*>(this_v->index(0)->toPointer());variable r = (i->*x)g;env.push(r);}\
	rsv call(Environment &env, variable &a_, Variable *v){variable a = merge(a_, (variable)d);C*i=static_cast<C*>(this_v->index(0)->toPointer());variable r = (i->*x)g;return r;}};
template<class C, class R>MR(0,(),())
template<class C, class R>MR(C0,()const,())
template<class C, class R, class A0>MR(1,(A0),(a))
template<class C, class R, class A0>MR(C1,(A0)const,(a))
template<class C, class R, class A0, class A1>MR(2,(A0,A1),(a[0],a[1]))
template<class C, class R, class A0, class A1>MR(C2,(A0,A1)const,(a[0],a[1]))
template<class C, class R, class A0, class A1, class A2>MR(3,(A0,A1,A2),(a[0],a[1],a[2]))
template<class C, class R, class A0, class A1, class A2>MR(C3,(A0,A1,A2)const,(a[0],a[1],a[2]))
template<class C, class R, class A0, class A1, class A2, class A3>MR(4,(A0,A1,A2,A3),(a[0],a[1],a[2],a[3]))
template<class C, class R, class A0, class A1, class A2, class A3>MR(C4,(A0,A1,A2,A3)const,(a[0],a[1],a[2],a[3]))
template<class C, class R, class A0, class A1, class A2, class A3, class A4>MR(5,(A0,A1,A2,A3,A4),(a[0],a[1],a[2],a[3],a[4]))
template<class C, class R, class A0, class A1, class A2, class A3, class A4>MR(C5,(A0,A1,A2,A3,A4)const,(a[0],a[1],a[2],a[3],a[4]))
template<class C, class R, class A0, class A1, class A2, class A3, class A4, class A5>MR(6,(A0,A1,A2,A3,A4,A5),(a[0],a[1],a[2],a[3],a[4],a[5]))
template<class C, class R, class A0, class A1, class A2, class A3, class A4, class A5>MR(C6,(A0,A1,A2,A3,A4,A5)const,(a[0],a[1],a[2],a[3],a[4],a[5]))
#undef MR
template<class C>
static vBase *CCMethod(void(C::*m)(), const variable &d){return new vCCMethodV0<C>(m, d);}
template<class C>
static vBase *CCMethod(void(C::*m)()const, const variable &d){return new vCCMethodVC0<C>(m, d);}
template<class C, class A0>
static vBase *CCMethod(void(C::*m)(A0), const variable &d){return new vCCMethodV1<C,A0>(m, d);}
template<class C, class A0>
static vBase *CCMethod(void(C::*m)(A0)const, const variable &d){return new vCCMethodVC1<C,A0>(m, d);}
template<class C, class A0, class A1>
static vBase *CCMethod(void(C::*m)(A0,A1), const variable &d){return new vCCMethodV2<C,A0,A1>(m, d);}
template<class C, class A0, class A1>
static vBase *CCMethod(void(C::*m)(A0,A1)const, const variable &d){return new vCCMethodVC2<C,A0,A1>(m, d);}
template<class C, class A0, class A1, class A2>
static vBase *CCMethod(void(C::*m)(A0,A1,A2), const variable &d){return new vCCMethodV3<C,A0,A1,A2>(m, d);}
template<class C, class A0, class A1, class A2>
static vBase *CCMethod(void(C::*m)(A0,A1,A2)const, const variable &d){return new vCCMethodVC3<C,A0,A1,A2>(m, d);}
template<class C, class A0, class A1, class A2, class A3>
static vBase *CCMethod(void(C::*m)(A0,A1,A2,A3), const variable &d){return new vCCMethodV4<C,A0,A1,A2,A3>(m, d);}
template<class C, class A0, class A1, class A2, class A3>
static vBase *CCMethod(void(C::*m)(A0,A1,A2,A3)const, const variable &d){return new vCCMethodVC4<C,A0,A1,A2,A3>(m, d);}
template<class C, class A0, class A1, class A2, class A3, class A4>
static vBase *CCMethod(void(C::*m)(A0,A1,A2,A3,A4), const variable &d){return new vCCMethodV5<C,A0,A1,A2,A3,A4>(m, d);}
template<class C, class A0, class A1, class A2, class A3, class A4>
static vBase *CCMethod(void(C::*m)(A0,A1,A2,A3,A4)const, const variable &d){return new vCCMethodVC5<C,A0,A1,A2,A3,A4>(m, d);}
template<class C, class A0, class A1, class A2, class A3, class A4, class A5>
static vBase *CCMethod(void(C::*m)(A0,A1,A2,A3,A4,A5), const variable &d){return new vCCMethodV6<C,A0,A1,A2,A3,A4,A5>(m, d);}
template<class C, class A0, class A1, class A2, class A3, class A4, class A5>
static vBase *CCMethod(void(C::*m)(A0,A1,A2,A3,A4,A5)const, const variable &d){return new vCCMethodVC6<C,A0,A1,A2,A3,A4,A5>(m, d);}
template<class C, class R>
static vBase *CCMethod(R(C::*m)(), const variable &d){return new vCCMethodR0<C,R>(m, d);}
template<class C, class R>
static vBase *CCMethod(R(C::*m)()const, const variable &d){return new vCCMethodRC0<C,R>(m, d);}
template<class C, class R, class A0>
static vBase *CCMethod(R(C::*m)(A0), const variable &d){return new vCCMethodR1<C,R,A0>(m, d);}
template<class C, class R, class A0>
static vBase *CCMethod(R(C::*m)(A0)const, const variable &d){return new vCCMethodRC1<C,R,A0>(m, d);}
template<class C, class R, class A0, class A1>
static vBase *CCMethod(R(C::*m)(A0,A1), const variable &d){return new vCCMethodR2<C,R,A0,A1>(m, d);}
template<class C, class R, class A0, class A1>
static vBase *CCMethod(R(C::*m)(A0,A1)const, const variable &d){return new vCCMethodRC2<C,R,A0,A1>(m, d);}
template<class C, class R, class A0, class A1, class A2>
static vBase *CCMethod(R(C::*m)(A0,A1,A2), const variable &d){return new vCCMethodR3<C,R,A0,A1,A2>(m, d);}
template<class C, class R, class A0, class A1, class A2>
static vBase *CCMethod(R(C::*m)(A0,A1,A2)const, const variable &d){return new vCCMethodRC3<C,R,A0,A1,A2>(m, d);}
template<class C, class R, class A0, class A1, class A2, class A3>
static vBase *CCMethod(R(C::*m)(A0,A1,A2,A3), const variable &d){return new vCCMethodR4<C,R,A0,A1,A2,A3>(m, d);}
template<class C, class R, class A0, class A1, class A2, class A3>
static vBase *CCMethod(R(C::*m)(A0,A1,A2,A3)const, const variable &d){return new vCCMethodRC4<C,R,A0,A1,A2,A3>(m, d);}
template<class C, class R, class A0, class A1, class A2, class A3, class A4>
static vBase *CCMethod(R(C::*m)(A0,A1,A2,A3,A4), const variable &d){return new vCCMethodR5<C,R,A0,A1,A2,A3,A4>(m, d);}
template<class C, class R, class A0, class A1, class A2, class A3, class A4>
static vBase *CCMethod(R(C::*m)(A0,A1,A2,A3,A4)const, const variable &d){return new vCCMethodRC5<C,R,A0,A1,A2,A3,A4>(m, d);}
template<class C, class R, class A0, class A1, class A2, class A3, class A4, class A5>
static vBase *CCMethod(R(C::*m)(A0,A1,A2,A3,A4,A5), const variable &d){return new vCCMethodR6<C,R,A0,A1,A2,A3,A4,A5>(m, d);}
template<class C, class R, class A0, class A1, class A2, class A3, class A4, class A5>
static vBase *CCMethod(R(C::*m)(A0,A1,A2,A3,A4,A5)const, const variable &d){return new vCCMethodRC6<C,R,A0,A1,A2,A3,A4,A5>(m, d);}
