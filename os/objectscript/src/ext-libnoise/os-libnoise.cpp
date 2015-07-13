#include "os-libnoise.h"
#include "../objectscript.h"
#include "../os-binder.h"

#include "noise/noise.h"
#include "noise/latlon.h"
#include "noiseutils/noiseutils.h"

#define OS_AUTO_TEXT(exp) OS_TEXT(#exp)

using namespace noise;

namespace ObjectScript {

template <>
struct CtypeValue<noise::NoiseQuality>
{
	typedef noise::NoiseQuality type;

	static bool isValid(type){ return true; }

	static type def(ObjectScript::OS*){ return type(); }
	static type getArg(ObjectScript::OS * os, int offs)
	{
		return (type)(int)os->toNumber(offs);
	}

	static void push(ObjectScript::OS * os, const type& val)
	{
		os->pushNumber((OS_NUMBER)val);
	}
};
template <> struct CtypeName<noise::NoiseQuality> { static const OS_CHAR * getName(){ return "NoiseQuality"; } };
// OS_DECL_CTYPE_NUMBER(noise::NoiseQuality);

template <>
struct CtypeValue<noise::utils::Color>
{
	typedef noise::utils::Color type;

	static bool isValid(type){ return true; }

	static type def(ObjectScript::OS*){ return type(0, 0, 0, 255); }
	static type getArg(ObjectScript::OS * os, int offs)
	{
		noise::uint8 c[4];
		int i;
		double val;
		static const char * const components[] = {"r", "g", "b", "a"};
		switch(os->getType(offs)){
		case OS_VALUE_TYPE_ARRAY:
			for(i = 0; i < 4; i++){
				os->pushStackValue(offs);
				os->pushNumber(i);
				os->getProperty();
				val = os->popFloat();
				if(val < 0) val = 0; else if(val > 255) val = 255;
				c[i] = (noise::uint8)(val);
			}
			return type(c[0], c[1], c[2], c[3]);

		case OS_VALUE_TYPE_OBJECT:
			for(i = 0; i < 4; i++){
				os->pushStackValue(offs);
				os->getProperty(components[i]);
				val = os->popFloat();
				if(val < 0) val = 0; else if(val > 255) val = 255;
				c[i] = (noise::uint8)(val);
			}
			return type(c[0], c[1], c[2], c[3]);

		case OS_VALUE_TYPE_NUMBER:
			val = os->toDouble(offs);
			if(val < 0) val = 0; else if(val > 255) val = 255;
			c[0] = (noise::uint8)(val);
			return type(c[0], c[0], c[0], 255);
		}
		return def(os);
	}

	static void push(ObjectScript::OS * os, const type& val)
	{
		os->newArray();

		os->pushStackValue();
		os->pushNumber(val.red);
		os->addProperty();		

		os->pushStackValue();
		os->pushNumber(val.green);
		os->addProperty();		

		os->pushStackValue();
		os->pushNumber(val.blue);
		os->addProperty();		

		os->pushStackValue();
		os->pushNumber(val.alpha);
		os->addProperty();		
	}
};
template <> struct CtypeName<noise::utils::Color> { static const OS_CHAR * getName(){ return "NoiseColor"; } };

class OSNoiseModule: public module::Module
{
	typedef module::Module super;

public:

	module::Module * getSourceModule(int i)
	{ 
		try{
			return (module::Module*)&super::GetSourceModule(i);
		}catch(...){}
		return NULL;
	}
	void setSourceModule(OS * os, int i, module::Module * sourceModule)
	{
		if(!sourceModule){
			os->setException(OS_TEXT("NoiseAbstractModule.setSourceModule requires NoiseModule argument"));
			return;
		}
		module::Module * oldSourceModule = getSourceModule(i);
		if(sourceModule != oldSourceModule){
			os->releaseValueById(os->findUserPointerValueId((void*)oldSourceModule));
			super::SetSourceModule(i, *sourceModule);
			os->retainValueById(os->findUserPointerValueId((void*)getSourceModule(i)));
		}
	}
}; // OSNoiseModule

template <> struct CtypeName<module::Module>{ static const OS_CHAR * getName(){ return OS_TEXT("NoiseAbstractModule"); } };
template <> struct CtypeValue<module::Module*>: public CtypeUserClass<module::Module*>{};
template <> struct UserDataDestructor<module::Module>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<module::Module*>((module::Module*)data));
		module::Module * self = (module::Module*)data;
		int count = self->GetSourceModuleCount();
		for(int i = 0; i < count; i++){
			try{ // TODO: GetSourceModule can throw exception
				os->releaseValueById(os->findUserPointerValueId((void*)&self->GetSourceModule(i)));
			}catch(...){
			}
		}
		self->~Module();
		os->free(self);
	}
};

template <class T>
static int initNoiseObjectAttrs(OS * os, T * obj, int params)
{
	int offs = os->getAbsoluteOffs(-params+0);
	pushCtypeValue(os, obj);
	if(params > 0 && os->isObject(offs)){
		os->getProperty(-1, OS_TEXT("attrs"));
		OS_ASSERT(os->isFunction());
		os->pushStackValue(-2);
		os->pushStackValue(offs);
		os->call(1, 0);
	}
	return 1;
}

static void initNoiseModule(OS * os)
{
	struct Lib {
		static int getSourceModule(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(module::Module*);
			pushCtypeValue(os, ((OSNoiseModule*)self)->getSourceModule(os->toInt(-params+0)));
			return 1;
		}
		static int setSourceModule(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(module::Module*);
			((OSNoiseModule*)self)->setSourceModule(os, 
				os->toInt(-params+0), 
				CtypeValue<module::Module*>::getArg(os, -params+1));
			return 0;
		}
	};
	OS::FuncDef funcs[] = {
		{OS_TEXT("getSourceModule"), &Lib::getSourceModule},
		{OS_TEXT("setSourceModule"), &Lib::setSourceModule},
		def(OS_TEXT("sourceModuleCount"), &module::Module::GetSourceModuleCount),
		def(OS_TEXT("getValue"), &module::Module::GetValue),
		{}
	};
	registerUserClass<module::Module>(os, funcs);

	OS::NumberDef numbers[] = {
		{OS_TEXT("NOISE_QUALITY_FAST"), QUALITY_FAST},
		{OS_TEXT("NOISE_QUALITY_STD"), QUALITY_STD},
		{OS_TEXT("NOISE_QUALITY_BEST"), QUALITY_BEST},
		{}
	};
	os->pushGlobals();
	os->setNumbers(numbers);
	os->pop();
}

#define DECL_NOISE_MODULE(name) \
template <> struct CtypeName<module::name>{ static const OS_CHAR * getName(){ return OS_AUTO_TEXT(Noise) OS_AUTO_TEXT(name); } }; \
template <> struct CtypeValue<module::name*>: public CtypeUserClass<module::name*>{}; \
template <> struct UserDataDestructor<module::name>: public UserDataDestructor<module::Module> {}; \
static int __constructNoise ## name(OS * os, int params, int, int, void*) \
{ \
	module::name * module = new (os->malloc(sizeof(module::name) OS_DBG_FILEPOS)) module::name(); \
	return initNoiseObjectAttrs(os, module, params); \
}

// === module::Perlin

DECL_NOISE_MODULE(Perlin);

static void initNoisePerlin(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoisePerlin},
		def(OS_TEXT("__get@frequency"), &module::Perlin::GetFrequency),
		def(OS_TEXT("__set@frequency"), &module::Perlin::SetFrequency),
		def(OS_TEXT("__get@lacunarity"), &module::Perlin::GetLacunarity),
		def(OS_TEXT("__set@lacunarity"), &module::Perlin::SetLacunarity),
		def(OS_TEXT("__get@noiseQuality"), &module::Perlin::GetNoiseQuality),
		def(OS_TEXT("__set@noiseQuality"), &module::Perlin::SetNoiseQuality),
		def(OS_TEXT("__get@octaveCount"), &module::Perlin::GetOctaveCount),
		def(OS_TEXT("__set@octaveCount"), &module::Perlin::SetOctaveCount),
		def(OS_TEXT("__get@persistence"), &module::Perlin::GetPersistence),
		def(OS_TEXT("__set@persistence"), &module::Perlin::SetPersistence),
		def(OS_TEXT("__get@seed"), &module::Perlin::GetSeed),
		def(OS_TEXT("__set@seed"), &module::Perlin::SetSeed),
		{}
	};
	registerUserClass<module::Perlin, module::Module>(os, funcs);
}

// === module::Billow

DECL_NOISE_MODULE(Billow);

static void initNoiseBillow(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseBillow},
		def(OS_TEXT("__get@frequency"), &module::Billow::GetFrequency),
		def(OS_TEXT("__set@frequency"), &module::Billow::SetFrequency),
		def(OS_TEXT("__get@lacunarity"), &module::Billow::GetLacunarity),
		def(OS_TEXT("__set@lacunarity"), &module::Billow::SetLacunarity),
		def(OS_TEXT("__get@noiseQuality"), &module::Billow::GetNoiseQuality),
		def(OS_TEXT("__set@noiseQuality"), &module::Billow::SetNoiseQuality),
		def(OS_TEXT("__get@octaveCount"), &module::Billow::GetOctaveCount),
		def(OS_TEXT("__set@octaveCount"), &module::Billow::SetOctaveCount),
		def(OS_TEXT("__get@persistence"), &module::Billow::GetPersistence),
		def(OS_TEXT("__set@persistence"), &module::Billow::SetPersistence),
		def(OS_TEXT("__get@seed"), &module::Billow::GetSeed),
		def(OS_TEXT("__set@seed"), &module::Billow::SetSeed),
		{}
	};
	registerUserClass<module::Billow, module::Module>(os, funcs);
}

// === module::RidgedMulti

DECL_NOISE_MODULE(RidgedMulti);

static void initNoiseRidgedMulti(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseRidgedMulti},
		def(OS_TEXT("__get@frequency"), &module::RidgedMulti::GetFrequency),
		def(OS_TEXT("__set@frequency"), &module::RidgedMulti::SetFrequency),
		def(OS_TEXT("__get@lacunarity"), &module::RidgedMulti::GetLacunarity),
		def(OS_TEXT("__set@lacunarity"), &module::RidgedMulti::SetLacunarity),
		def(OS_TEXT("__get@noiseQuality"), &module::RidgedMulti::GetNoiseQuality),
		def(OS_TEXT("__set@noiseQuality"), &module::RidgedMulti::SetNoiseQuality),
		def(OS_TEXT("__get@octaveCount"), &module::RidgedMulti::GetOctaveCount),
		def(OS_TEXT("__set@octaveCount"), &module::RidgedMulti::SetOctaveCount),
		def(OS_TEXT("__get@seed"), &module::RidgedMulti::GetSeed),
		def(OS_TEXT("__set@seed"), &module::RidgedMulti::SetSeed),
		{}
	};
	registerUserClass<module::RidgedMulti, module::Module>(os, funcs);
}

// === module::Cylinders

DECL_NOISE_MODULE(Cylinders);

static void initNoiseCylinders(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseCylinders},
		def(OS_TEXT("__get@frequency"), &module::Cylinders::GetFrequency),
		def(OS_TEXT("__set@frequency"), &module::Cylinders::SetFrequency),
		{}
	};
	registerUserClass<module::Cylinders, module::Module>(os, funcs);
}

// === module::Spheres

DECL_NOISE_MODULE(Spheres);

static void initNoiseSpheres(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseSpheres},
		def(OS_TEXT("__get@frequency"), &module::Spheres::GetFrequency),
		def(OS_TEXT("__set@frequency"), &module::Spheres::SetFrequency),
		{}
	};
	registerUserClass<module::Spheres, module::Module>(os, funcs);
}

// === module::ScalePoint

DECL_NOISE_MODULE(ScalePoint);

static void initNoiseScalePoint(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseScalePoint},
		def(OS_TEXT("__get@xScale"), &module::ScalePoint::GetXScale),
		def(OS_TEXT("__set@xScale"), &module::ScalePoint::SetXScale),
		def(OS_TEXT("__get@yScale"), &module::ScalePoint::GetYScale),
		def(OS_TEXT("__set@yScale"), &module::ScalePoint::SetYScale),
		def(OS_TEXT("__get@zScale"), &module::ScalePoint::GetZScale),
		def(OS_TEXT("__set@zScale"), &module::ScalePoint::SetZScale),
		// def(OS_TEXT("setScale"), &module::ScalePoint::SetScale),
		{}
	};
	registerUserClass<module::ScalePoint, module::Module>(os, funcs);
}

// === module::ScaleBias

DECL_NOISE_MODULE(ScaleBias);

static void initNoiseScaleBias(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseScaleBias},
		def(OS_TEXT("__get@scale"), &module::ScaleBias::GetScale),
		def(OS_TEXT("__set@scale"), &module::ScaleBias::SetScale),
		def(OS_TEXT("__get@bias"), &module::ScaleBias::GetBias),
		def(OS_TEXT("__set@bias"), &module::ScaleBias::SetBias),
		{}
	};
	registerUserClass<module::ScaleBias, module::Module>(os, funcs);
}

// === module::Displace

DECL_NOISE_MODULE(Displace);

static void initNoiseDisplace(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseDisplace},
		{}
	};
	registerUserClass<module::Displace, module::Module>(os, funcs);
}

// === module::Add

DECL_NOISE_MODULE(Add);

static void initNoiseAdd(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseAdd},
		{}
	};
	registerUserClass<module::Add, module::Module>(os, funcs);
}

// === module::Max

DECL_NOISE_MODULE(Max);

static void initNoiseMax(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseMax},
		{}
	};
	registerUserClass<module::Max, module::Module>(os, funcs);
}

// === module::Min

DECL_NOISE_MODULE(Min);

static void initNoiseMin(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseMin},
		{}
	};
	registerUserClass<module::Min, module::Module>(os, funcs);
}

// === module::Multiply

DECL_NOISE_MODULE(Multiply);

static void initNoiseMultiply(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseMultiply},
		{}
	};
	registerUserClass<module::Multiply, module::Module>(os, funcs);
}

// === module::Power

DECL_NOISE_MODULE(Power);

static void initNoisePower(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoisePower},
		{}
	};
	registerUserClass<module::Power, module::Module>(os, funcs);
}

// === module::Invert

DECL_NOISE_MODULE(Invert);

static void initNoiseInvert(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseInvert},
		{}
	};
	registerUserClass<module::Invert, module::Module>(os, funcs);
}

// === module::Abs

DECL_NOISE_MODULE(Abs);

static void initNoiseAbs(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseAbs},
		{}
	};
	registerUserClass<module::Abs, module::Module>(os, funcs);
}

// === module::Blend

DECL_NOISE_MODULE(Blend);

static void initNoiseBlend(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseBlend},
		{}
	};
	registerUserClass<module::Blend, module::Module>(os, funcs);
}

// === module::Cache

DECL_NOISE_MODULE(Cache);

static void initNoiseCache(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseCache},
		{}
	};
	registerUserClass<module::Cache, module::Module>(os, funcs);
}

// === module::Checkerboard

DECL_NOISE_MODULE(Checkerboard);

static void initNoiseCheckerboard(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseCheckerboard},
		{}
	};
	registerUserClass<module::Checkerboard, module::Module>(os, funcs);
}

// === module::Turbulence

DECL_NOISE_MODULE(Turbulence);

static void initNoiseTurbulence(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseTurbulence},
		def(OS_TEXT("__get@frequency"), &module::Turbulence::GetFrequency),
		def(OS_TEXT("__set@frequency"), &module::Turbulence::SetFrequency),
		def(OS_TEXT("__get@power"), &module::Turbulence::GetPower),
		def(OS_TEXT("__set@power"), &module::Turbulence::SetPower),
		def(OS_TEXT("__get@roughness"), &module::Turbulence::GetRoughnessCount),
		def(OS_TEXT("__set@roughness"), &module::Turbulence::SetRoughness),
		def(OS_TEXT("__get@seed"), &module::Turbulence::GetSeed),
		def(OS_TEXT("__set@seed"), &module::Turbulence::SetSeed),
		{}
	};
	registerUserClass<module::Turbulence, module::Module>(os, funcs);
}

// === module::Voronoi

DECL_NOISE_MODULE(Voronoi);

static void initNoiseVoronoi(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseVoronoi},
		def(OS_TEXT("__get@frequency"), &module::Voronoi::GetFrequency),
		def(OS_TEXT("__set@frequency"), &module::Voronoi::SetFrequency),
		def(OS_TEXT("__get@seed"), &module::Voronoi::GetSeed),
		def(OS_TEXT("__set@seed"), &module::Voronoi::SetSeed),
		def(OS_TEXT("__get@distance"), &module::Voronoi::IsDistanceEnabled),
		def(OS_TEXT("__set@distance"), &module::Voronoi::EnableDistance),
		def(OS_TEXT("__get@displacement"), &module::Voronoi::GetDisplacement),
		def(OS_TEXT("__set@displacement"), &module::Voronoi::SetDisplacement),
		{}
	};
	registerUserClass<module::Voronoi, module::Module>(os, funcs);
}

// === module::TranslatePoint

DECL_NOISE_MODULE(TranslatePoint);

static void initNoiseTranslatePoint(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseTranslatePoint},
		def(OS_TEXT("__get@xTranslation"), &module::TranslatePoint::GetXTranslation),
		def(OS_TEXT("__set@xTranslation"), &module::TranslatePoint::SetXTranslation),
		def(OS_TEXT("__get@yTranslation"), &module::TranslatePoint::GetYTranslation),
		def(OS_TEXT("__set@yTranslation"), &module::TranslatePoint::SetYTranslation),
		def(OS_TEXT("__get@zTranslation"), &module::TranslatePoint::GetZTranslation),
		def(OS_TEXT("__set@zTranslation"), &module::TranslatePoint::SetZTranslation),
		{}
	};
	registerUserClass<module::TranslatePoint, module::Module>(os, funcs);
}

// === module::RotatePoint

DECL_NOISE_MODULE(RotatePoint);

static void initNoiseRotatePoint(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseRotatePoint},
		def(OS_TEXT("__get@xAngle"), &module::RotatePoint::GetXAngle),
		def(OS_TEXT("__set@xAngle"), &module::RotatePoint::SetXAngle),
		def(OS_TEXT("__get@yAngle"), &module::RotatePoint::GetYAngle),
		def(OS_TEXT("__set@yAngle"), &module::RotatePoint::SetYAngle),
		def(OS_TEXT("__get@zAngle"), &module::RotatePoint::GetZAngle),
		def(OS_TEXT("__set@zAngle"), &module::RotatePoint::SetZAngle),
		def(OS_TEXT("setAngles"), &module::RotatePoint::SetAngles),
		{}
	};
	registerUserClass<module::RotatePoint, module::Module>(os, funcs);
}

// === module::Select

DECL_NOISE_MODULE(Select);

static void initNoiseSelect(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseSelect},
		def(OS_TEXT("__get@edgeFalloff"), &module::Select::GetEdgeFalloff),
		def(OS_TEXT("__set@edgeFalloff"), &module::Select::SetEdgeFalloff),
		def(OS_TEXT("__get@lowerBound"), &module::Select::GetLowerBound),
		def(OS_TEXT("__get@upperBound"), &module::Select::GetUpperBound),
		def(OS_TEXT("setBounds"), &module::Select::SetBounds),
		{}
	};
	registerUserClass<module::Select, module::Module>(os, funcs);
}

// === module::Clamp

DECL_NOISE_MODULE(Clamp);

static void initNoiseClamp(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseClamp},
		def(OS_TEXT("__get@lowerBound"), &module::Clamp::GetLowerBound),
		def(OS_TEXT("__get@upperBound"), &module::Clamp::GetUpperBound),
		def(OS_TEXT("setBounds"), &module::Clamp::SetBounds),
		{}
	};
	registerUserClass<module::Clamp, module::Module>(os, funcs);
}

// === module::Const

DECL_NOISE_MODULE(Const);

static void initNoiseConst(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseConst},
		def(OS_TEXT("__get@constValue"), &module::Const::GetConstValue),
		def(OS_TEXT("__set@constValue"), &module::Const::SetConstValue),
		{}
	};
	registerUserClass<module::Const, module::Module>(os, funcs);
}

// === module::Exponent

DECL_NOISE_MODULE(Exponent);

static void initNoiseExponent(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseExponent},
		def(OS_TEXT("__get@exponent"), &module::Exponent::GetExponent),
		def(OS_TEXT("__set@exponent"), &module::Exponent::SetExponent),
		{}
	};
	registerUserClass<module::Exponent, module::Module>(os, funcs);
}

// === module::Curve

DECL_NOISE_MODULE(Curve);

static void initNoiseCurve(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseCurve},
		def(OS_TEXT("__get@controlPointCount"), &module::Curve::GetControlPointCount),
		def(OS_TEXT("addControlPoint"), &module::Curve::AddControlPoint),
		def(OS_TEXT("clearAllControlPoints"), &module::Curve::ClearAllControlPoints),
		{}
	};
	registerUserClass<module::Curve, module::Module>(os, funcs);
}

// === module::Terrace

DECL_NOISE_MODULE(Terrace);

static void initNoiseTerrace(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseTerrace},
		def(OS_TEXT("__get@controlPointCount"), &module::Terrace::GetControlPointCount),
		def(OS_TEXT("addControlPoint"), &module::Terrace::AddControlPoint),
		def(OS_TEXT("clearAllControlPoints"), &module::Terrace::ClearAllControlPoints),
		def(OS_TEXT("__get@invertTerraces"), &module::Terrace::IsTerracesInverted),
		def(OS_TEXT("__set@invertTerraces"), &module::Terrace::InvertTerraces),
		{}
	};
	registerUserClass<module::Terrace, module::Module>(os, funcs);
}

// =================================================================
// =================================================================
// =================================================================

typedef noise::utils::NoiseMapBuilder NoiseMapBuilder;
typedef noise::utils::NoiseMap NoiseMap;
typedef noise::utils::Image NoiseImage;

class OSNoiseMapBuilder: public NoiseMapBuilder
{
	typedef NoiseMapBuilder super;

public:

	NoiseMap * getDestNoiseMap(){ return &super::GetDestNoiseMap();	}
	void setDestNoiseMap(OS * os, NoiseMap * destNoiseMap)
	{
		if(!destNoiseMap){
			os->setException(OS_TEXT("NoiseMapBuilder.setDestNoiseMap requires NoiseModule argument"));
			return;
		}
		os->releaseValueById(os->findUserPointerValueId((void*)getDestNoiseMap()));
		super::SetDestNoiseMap(*destNoiseMap);
		os->retainValueById(os->findUserPointerValueId((void*)getDestNoiseMap()));
	}

	const module::Module * getSourceModule(){ return &super::GetSourceModule();	}
	void setSourceModule(OS * os, const module::Module * sourceModule)
	{
		if(!sourceModule){
			os->setException(OS_TEXT("NoiseMapBuilder.setSourceModule requires NoiseModule argument"));
			return;
		}
		const module::Module * oldSourceModule = getSourceModule();
		if(oldSourceModule != sourceModule){
			os->releaseValueById(os->findUserPointerValueId((void*)oldSourceModule));
			super::SetSourceModule(*sourceModule);
			os->retainValueById(os->findUserPointerValueId((void*)getSourceModule()));
		}
	}

}; // OSNoiseMapBuilder

struct NoiseMapBuilderCallbackData
{
	OS * os;
	int funcId;
};

template <> struct CtypeName<NoiseMapBuilder>{ static const OS_CHAR * getName(){ return OS_TEXT("NoiseMapBuilder"); } };
template <> struct CtypeValue<NoiseMapBuilder*>: public CtypeUserClass<NoiseMapBuilder*>{};
template <> struct UserDataDestructor<NoiseMapBuilder>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<NoiseMapBuilder*>((NoiseMapBuilder*)data));
		OSNoiseMapBuilder * self = (OSNoiseMapBuilder*)data;
		os->releaseValueById(os->findUserPointerValueId((void*)self->getDestNoiseMap()));
		os->releaseValueById(os->findUserPointerValueId((void*)self->getSourceModule()));
		
		NoiseMapBuilderCallbackData * callbackData = (NoiseMapBuilderCallbackData*)self->GetCallbackParam();
		if(callbackData){
			os->releaseValueById(callbackData->funcId);
			os->free(callbackData);
		}

		self->~OSNoiseMapBuilder();
		os->free(self);
	}
};

template <> struct CtypeName<NoiseMap>{ static const OS_CHAR * getName(){ return OS_TEXT("NoiseMap"); } };
template <> struct CtypeValue<NoiseMap*>: public CtypeUserClass<NoiseMap*>{};
template <> struct UserDataDestructor<NoiseMap>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<NoiseMap*>((NoiseMap*)data));
		NoiseMap * self = (NoiseMap*)data;
		self->~NoiseMap();
		os->free(self);
	}
};

template <> struct CtypeName<NoiseImage>{ static const OS_CHAR * getName(){ return OS_TEXT("NoiseImage"); } };
template <> struct CtypeValue<NoiseImage*>: public CtypeUserClass<NoiseImage*>{};
template <> struct UserDataDestructor<NoiseImage>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<NoiseImage*>((NoiseImage*)data));
		NoiseImage * self = (NoiseImage*)data;
		self->~NoiseImage();
		os->free(self);
	}
};

static int __constructNoiseMapBuilder(OS * os, int params, int, int, void*)
{
	return 0;
}

static void initNoiseMapBuilder(OS * os)
{
	struct Lib {
		static int getSourceModule(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMapBuilder*);
			pushCtypeValue(os, ((OSNoiseMapBuilder*)self)->getSourceModule());
			return 1;
		}
		static int setSourceModule(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMapBuilder*);
			((OSNoiseMapBuilder*)self)->setSourceModule(os, 
				CtypeValue<module::Module*>::getArg(os, -params+0));
			return 0;
		}

		static int getDestNoiseMap(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMapBuilder*);
			pushCtypeValue(os, ((OSNoiseMapBuilder*)self)->getDestNoiseMap());
			return 1;
		}
		static int setDestNoiseMap(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMapBuilder*);
			((OSNoiseMapBuilder*)self)->setDestNoiseMap(os, 
				CtypeValue<NoiseMap*>::getArg(os, -params+0));
			return 0;
		}

		static void noiseMapCallback(int row, NoiseMapBuilder * self, void * userParam)
		{
			OS_ASSERT(self && userParam);
			NoiseMapBuilderCallbackData * callbackData = (NoiseMapBuilderCallbackData*)userParam;
			OS_ASSERT(callbackData && callbackData->os && callbackData->funcId);
			if(callbackData){
				OS * os = callbackData->os;
				os->pushValueById(callbackData->funcId);
				os->pushNull();
				os->pushNumber(row);
				os->call(1, 0);
			}
		}

		static int getCallback(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMapBuilder*);
			NoiseMapBuilderCallbackData * callbackData = (NoiseMapBuilderCallbackData*)self->GetCallbackParam();
			if(callbackData){
				os->pushValueById(callbackData->funcId);
				return 1;
			}
			return 0;
		}

		static int setCallback(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMapBuilder*);
			NoiseMapBuilderCallbackData * callbackData;
			if(params < 1 || os->isNull(-params+0)){
				callbackData = (NoiseMapBuilderCallbackData*)self->GetCallbackParam();
				if(callbackData){
					os->releaseValueById(callbackData->funcId);
					os->free(callbackData);
				}
				self->SetCallback(NULL, NULL);
				return 0;
			}
			if(!os->isFunction(-params+0)){
				os->setException("NoiseMapBuilder.setCallback requires function argument");
				return 0;
			}
			callbackData = (NoiseMapBuilderCallbackData*)self->GetCallbackParam();
			if(!callbackData){
				callbackData = (NoiseMapBuilderCallbackData*)os->malloc(sizeof(NoiseMapBuilderCallbackData) OS_DBG_FILEPOS);
				callbackData->funcId = 0;
			}
			int funcId = os->getValueId(-params+0);
			if(callbackData->funcId != funcId){
				os->releaseValueById(callbackData->funcId);
				os->retainValueById(callbackData->funcId = funcId);
			}
			callbackData->os = os;
			self->SetCallback(noiseMapCallback, callbackData);
			return 0;
		}
	};
	
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseMapBuilder},
		def(OS_TEXT("build"), &NoiseMapBuilder::Build),
		def(OS_TEXT("__get@destHeight"), &NoiseMapBuilder::GetDestHeight),
		def(OS_TEXT("__get@destWidth"), &NoiseMapBuilder::GetDestWidth),
		def(OS_TEXT("setDestSize"), &NoiseMapBuilder::SetDestSize),
		{OS_TEXT("__get@sourceModule"), &Lib::getSourceModule},
		{OS_TEXT("__set@sourceModule"), &Lib::setSourceModule},
		{OS_TEXT("__get@destNoiseMap"), &Lib::getDestNoiseMap},
		{OS_TEXT("__set@destNoiseMap"), &Lib::setDestNoiseMap},
		{OS_TEXT("__get@callback"), &Lib::getCallback},
		{OS_TEXT("__set@callback"), &Lib::setCallback},
		{}
	};
	registerUserClass<NoiseMapBuilder>(os, funcs);
}

#define DECL_NOISE_MAP_BUILDER(name) \
template <> struct CtypeName<noise::utils::name>{ static const OS_CHAR * getName(){ return OS_AUTO_TEXT(name); } }; \
template <> struct CtypeValue<noise::utils::name*>: public CtypeUserClass<noise::utils::name*>{}; \
template <> struct UserDataDestructor<noise::utils::name>: public UserDataDestructor<NoiseMapBuilder> {}; \
static int __construct ## name(OS * os, int params, int, int, void*) \
{ \
	noise::utils::name * obj = new (os->malloc(sizeof(noise::utils::name) OS_DBG_FILEPOS)) noise::utils::name(); \
	return initNoiseObjectAttrs(os, obj, params); \
}

// === NoiseMapBuilderCylinder

DECL_NOISE_MAP_BUILDER(NoiseMapBuilderCylinder);

static void initNoiseMapBuilderCylinder(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseMapBuilderCylinder},
		def(OS_TEXT("__get@lowerAngleBound"), &noise::utils::NoiseMapBuilderCylinder::GetLowerAngleBound),
		def(OS_TEXT("__get@lowerHeightBound"), &noise::utils::NoiseMapBuilderCylinder::GetLowerHeightBound),
		def(OS_TEXT("__get@upperAngleBound"), &noise::utils::NoiseMapBuilderCylinder::GetUpperAngleBound),
		def(OS_TEXT("__get@upperHeightBound"), &noise::utils::NoiseMapBuilderCylinder::GetUpperHeightBound),
		def(OS_TEXT("setBounds"), &noise::utils::NoiseMapBuilderCylinder::SetBounds),
		{}
	};
	registerUserClass<noise::utils::NoiseMapBuilderCylinder, NoiseMapBuilder>(os, funcs);
}

// === NoiseMapBuilderPlane

DECL_NOISE_MAP_BUILDER(NoiseMapBuilderPlane);

static void initNoiseMapBuilderPlane(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseMapBuilderPlane},
		def(OS_TEXT("__get@seamless"), &noise::utils::NoiseMapBuilderPlane::IsSeamlessEnabled),
		def(OS_TEXT("__set@seamless"), &noise::utils::NoiseMapBuilderPlane::EnableSeamless),
		def(OS_TEXT("__get@lowerXBound"), &noise::utils::NoiseMapBuilderPlane::GetLowerXBound),
		def(OS_TEXT("__get@lowerZBound"), &noise::utils::NoiseMapBuilderPlane::GetLowerZBound),
		def(OS_TEXT("__get@upperXBound"), &noise::utils::NoiseMapBuilderPlane::GetUpperXBound),
		def(OS_TEXT("__get@upperZBound"), &noise::utils::NoiseMapBuilderPlane::GetUpperZBound),
		def(OS_TEXT("setBounds"), &noise::utils::NoiseMapBuilderPlane::SetBounds),
		{}
	};
	registerUserClass<noise::utils::NoiseMapBuilderPlane, NoiseMapBuilder>(os, funcs);
}

// === NoiseMapBuilderSphere

DECL_NOISE_MAP_BUILDER(NoiseMapBuilderSphere);

static void initNoiseMapBuilderSphere(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &__constructNoiseMapBuilderSphere},
		def(OS_TEXT("__get@eastLonBound"), &noise::utils::NoiseMapBuilderSphere::GetEastLonBound),
		def(OS_TEXT("__get@northLatBound"), &noise::utils::NoiseMapBuilderSphere::GetNorthLatBound),
		def(OS_TEXT("__get@southLatBound"), &noise::utils::NoiseMapBuilderSphere::GetSouthLatBound),
		def(OS_TEXT("__get@westLonBound"), &noise::utils::NoiseMapBuilderSphere::GetWestLonBound),
		def(OS_TEXT("setBounds"), &noise::utils::NoiseMapBuilderSphere::SetBounds),
		{}
	};
	registerUserClass<noise::utils::NoiseMapBuilderSphere, NoiseMapBuilder>(os, funcs);
}

// === NoiseMap

static void initNoiseMap(OS * os)
{
	struct Lib {
		static int __constructNoiseMap(OS * os, int params, int, int, void*)
		{
			if(params >= 2){
				pushCtypeValue(os, new (os->malloc(sizeof(NoiseMap) OS_DBG_FILEPOS)) NoiseMap(
					os->toInt(-params+0), os->toInt(-params+1)));
				return 1;
			}
			pushCtypeValue(os, new (os->malloc(sizeof(NoiseMap) OS_DBG_FILEPOS)) NoiseMap());
			return 1;
		}

		static int writeTER(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMap*);
			if(params < 1){
				os->setException("NoiseMap.writeTER requires string argument");
				return 0;
			}
			OS::String filename = os->toString(-params+0);
			utils::WriterTER textureWriter;
			textureWriter.SetSourceNoiseMap(*self);
			textureWriter.SetDestFilename(filename.toChar());
			if(params >= 2){
				float metersPerPoint = os->toFloat(-params+1);
				OS_ASSERT(metersPerPoint > 0);
				textureWriter.SetMetersPerPoint(metersPerPoint);
			}
			textureWriter.WriteDestFile();
			return 0;
		}

		static void initMap(int * map, int size, bool is_little_endian)
		{
			if(is_little_endian){
				for(int i = 0; i < size; i++){
					map[i] = i;
				}
			}else{
				for(int i = 0; i < size; i++){
					map[i] = size-1-i;
				}
			}
		}

		static void packLong(long val, int size, int *map, char *output)
		{
			char *v = (char*)&val;
			for (int i = 0; i < size; i++) {
				*output++ = v[map[i]];
			}
		}

		static void writeRAW(OS * os, NoiseMap * self, const OS_CHAR * filename, const OS_CHAR * fmt, const OS_CHAR * mode = "wb")
		{
			int x, y, size;
			int map[sizeof(int)];
			
			bool is_little_endian = (*(OS_INT32*)"\x01\x02\x03\x04") == 0x04030201;
			int height = self->GetHeight();
			int width = self->GetWidth();

			char * buf = NULL;
			int buf_size = 0;

			OS::FileHandle * file = os->openFile(filename, mode);
			if(!file){
				os->setException(OS::String::format(os, "NoiseMap.writeRAW: error open file '%s'", filename));
				return;
			}

			switch(*fmt){
			/*
			case 'h': 
			case 'H': 
				// TODO: 4 bit per arg
				// no break
			*/

			default:
				os->setException(OS::String::format(os, "NoiseMap.writeRAW: unsupported format '%c'", *fmt));
				break;

			/*
			case 'a': 
			case 'A':
			case 'c': 
			case 'C':
			// case 'x':
				// TODO: 8 bit per arg
				size = 1;
				break;
			*/

			case 's': 
			case 'S': 
			case 'n': 
			case 'v':
				// TODO: 16 bit per arg
				size = 2;
				if(*fmt == 'n'){
					initMap(map, size, false);
				}else if(*fmt == 'v'){
					initMap(map, size, true);
				}else{
					initMap(map, size, is_little_endian);
				}
				buf_size = size * width;
				buf = (char*)os->malloc(buf_size OS_DBG_FILEPOS);
				OS_ASSERT(buf);
				for(y = 0; y < height; y++){
					char * dest = buf;
					float * cur = self->GetSlabPtr(y);
					for(x = 0; x < width; x++){
						packLong((long)*cur, size, map, dest);
						dest += size;
					}
					os->writeFile(buf, buf_size, file);
				}
				break;

			case 'i': 
			case 'I':
				size = sizeof(int);
				initMap(map, size, is_little_endian);
				buf_size = size * width;
				buf = (char*)os->malloc(buf_size OS_DBG_FILEPOS);
				OS_ASSERT(buf);
				for(y = 0; y < height; y++){
					char * dest = buf;
					float * cur = self->GetSlabPtr(y);
					for(x = 0; x < width; x++){
						packLong((long)*cur, size, map, dest);
						dest += size;
					}
					os->writeFile(buf, buf_size, file);
				}
				break;

			case 'l': 
			case 'L': 
			case 'N': 
			case 'V':
				// TODO: 32 bit per arg
				size = 4;
				if(*fmt == 'N'){
					initMap(map, size, false);
				}else if(*fmt == 'V'){
					initMap(map, size, true);
				}else{
					initMap(map, size, is_little_endian);
				}
				buf_size = size * width;
				buf = (char*)os->malloc(buf_size OS_DBG_FILEPOS);
				OS_ASSERT(buf);
				for(y = 0; y < height; y++){
					char * dest = buf;
					float * cur = self->GetSlabPtr(y);
					for(x = 0; x < width; x++){
						packLong((long)*cur, size, map, dest);
						dest += size;
					}
					os->writeFile(buf, buf_size, file);
				}
				break;

			case 'f':
				size = sizeof(float);
				buf_size = size * width;
				// buf = (char*)os->malloc(buf_size OS_DBG_FILEPOS);
				// OS_ASSERT(buf);
				for(int y = 0; y < height; y++){
					float * cur = self->GetSlabPtr(y);
#if 1
					os->writeFile(cur, buf_size, file);
#else
					float * dest = (float*)buf;
					for(int x = 0; x < width; x++){
						*dest++ = *cur++;
					}
					os->writeFile(buf, buf_size, file);
#endif
				}
				break;

			case 'd':
				size = sizeof(double);
				buf_size = size * width;
				buf = (char*)os->malloc(buf_size OS_DBG_FILEPOS);
				OS_ASSERT(buf);
				for(int y = 0; y < height; y++){
					float * cur = self->GetSlabPtr(y);
					double * dest = (double*)buf;
					for(int x = 0; x < width; x++){
						*dest++ = *cur++;
					}
					os->writeFile(buf, buf_size, file);
				}
				break;
			}
			os->closeFile(file);
			os->free(buf);
		}

		static int writeRAW(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMap*);
			if(params < 1){
				os->setException("NoiseMap.writeTER requires string argument");
				return 0;
			}
			OS::String filename = os->toString(-params+0);
			writeRAW(os, self, filename,
				params > 1 ? os->toString(-params+1) : "f",
				params > 2 ? os->toString(-params+2) : "wb");
			return 0;
		}

		static int adjustLowerBound(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMap*);
			float value = os->toFloat(-params + 0);
			int height = self->GetHeight();
			int width = self->GetWidth();
			for (int y = 0; y < height; y++) {
				float* pCur = self->GetSlabPtr (y);
				for (int x = 0; x < width; x++) {
				  if (*pCur < value) {
					*pCur = value;
				  }
				  ++pCur;
				}
			}
			return 0;
		}

		static int adjustUpperBound(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseMap*);
			float value = os->toFloat(-params + 0);
			int height = self->GetHeight();
			int width = self->GetWidth();
			for (int y = 0; y < height; y++) {
				float* pCur = self->GetSlabPtr (y);
				for (int x = 0; x < width; x++) {
				  if (*pCur > value) {
					*pCur = value;
				  }
				  ++pCur;
				}
			}
			return 0;
		}
	};
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &Lib::__constructNoiseMap},
		{OS_TEXT("writeTER"), &Lib::writeTER},
		{OS_TEXT("writeRAW"), &Lib::writeRAW},
		{OS_TEXT("adjustLowerBound"), &Lib::adjustLowerBound},
		{OS_TEXT("adjustUpperBound"), &Lib::adjustUpperBound},
		def(OS_TEXT("clear"), &NoiseMap::Clear),
		def(OS_TEXT("__get@borderValue"), &NoiseMap::GetBorderValue),
		def(OS_TEXT("__set@borderValue"), &NoiseMap::SetBorderValue),
		def(OS_TEXT("__get@height"), &NoiseMap::GetHeight),
		def(OS_TEXT("__get@width"), &NoiseMap::GetWidth),
		def(OS_TEXT("__get@stride"), &NoiseMap::GetStride),
		def(OS_TEXT("getValue"), &NoiseMap::GetValue),
		def(OS_TEXT("setValue"), &NoiseMap::SetValue),
		def(OS_TEXT("setSize"), &NoiseMap::SetSize),
		{}
	};
	registerUserClass<NoiseMap>(os, funcs);
}

// =================================================================
// =================================================================
// =================================================================

static void initNoiseImage(OS * os)
{
	struct Lib {
		static int __constructNoiseImage(OS * os, int params, int, int, void*)
		{
			if(params >= 2){
				pushCtypeValue(os, new (os->malloc(sizeof(NoiseImage) OS_DBG_FILEPOS)) NoiseImage(
					os->toInt(-params+0), os->toInt(-params+1)));
				return 1;
			}
			pushCtypeValue(os, new (os->malloc(sizeof(NoiseImage) OS_DBG_FILEPOS)) NoiseImage());
			return 1;
		}

		static int writeBMP(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(NoiseImage*);
			if(params < 1){
				os->setException("NoiseImage.writeBMP requires string argument");
				return 0;
			}
			OS::String filename = os->toString(-params+0);
			utils::WriterBMP textureWriter;
			textureWriter.SetSourceImage(*self);
			textureWriter.SetDestFilename(filename.toChar());
			textureWriter.WriteDestFile();
			return 0;
		}
	};
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &Lib::__constructNoiseImage},
		{OS_TEXT("writeBMP"), &Lib::writeBMP},
		def(OS_TEXT("clear"), &NoiseImage::Clear),
		def(OS_TEXT("__get@borderValue"), &NoiseImage::GetBorderValue),
		def(OS_TEXT("__set@borderValue"), &NoiseImage::SetBorderValue),
		def(OS_TEXT("__get@height"), &NoiseImage::GetHeight),
		def(OS_TEXT("__get@width"), &NoiseImage::GetWidth),
		def(OS_TEXT("__get@stride"), &NoiseImage::GetStride),
		def(OS_TEXT("getValue"), &NoiseImage::GetValue),
		def(OS_TEXT("setValue"), &NoiseImage::SetValue),
		def(OS_TEXT("setSize"), &NoiseImage::SetSize),
		{}
	};
	registerUserClass<NoiseImage>(os, funcs);
}

// =================================================================
// =================================================================
// =================================================================

typedef noise::utils::RendererImage RendererImage;

template <> struct CtypeName<RendererImage>{ static const OS_CHAR * getName(){ return OS_TEXT("NoiseRenderer"); } };
template <> struct CtypeValue<RendererImage*>: public CtypeUserClass<RendererImage*>{};
template <> struct UserDataDestructor<RendererImage>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<RendererImage*>((RendererImage*)data));
		RendererImage * self = (RendererImage*)data;
		self->~RendererImage();
		os->free(self);
	}
};

static void initNoiseRenderer(OS * os)
{
	struct Lib {
		static int __constructNoiseRenderer(OS * os, int params, int, int, void*)
		{
			RendererImage * r = new (os->malloc(sizeof(RendererImage) OS_DBG_FILEPOS)) RendererImage();
			return initNoiseObjectAttrs(os, r, params);
		}

		static int getBackgroundImage(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererImage*);
			pushCtypeValue(os, dynamic_cast<NoiseImage*>((NoiseImage*)&self->GetBackgroundImage()));
			return 1;
		}
		static int setBackgroundImage(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererImage*);
			NoiseImage * image = CtypeValue<NoiseImage*>::getArg(os, -params+0);
			if(!image){
				os->setException(OS_TEXT("NoiseRenderer.setDestImage requires NoiseImage argument"));
				return 0;
			}
			NoiseImage * old = dynamic_cast<NoiseImage*>((NoiseImage*)&self->GetBackgroundImage());
			if(image != old){
				os->releaseValueById(os->findUserPointerValueId((void*)old));
				self->SetBackgroundImage(*image);
				os->retainValueById(os->findUserPointerValueId((void*)&self->GetBackgroundImage()));
			}
			return 0;
		}

		static int getDestImage(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererImage*);
			pushCtypeValue(os, dynamic_cast<NoiseImage*>((NoiseImage*)&self->GetDestImage()));
			return 1;
		}
		static int setDestImage(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererImage*);
			NoiseImage * image = CtypeValue<NoiseImage*>::getArg(os, -params+0);
			if(!image){
				os->setException(OS_TEXT("NoiseRenderer.setDestImage requires NoiseImage argument"));
				return 0;
			}
			NoiseImage * old = dynamic_cast<NoiseImage*>((NoiseImage*)&self->GetDestImage());
			if(image != old){
				os->releaseValueById(os->findUserPointerValueId((void*)old));
				self->SetDestImage(*image);
				os->retainValueById(os->findUserPointerValueId((void*)&self->GetDestImage()));
			}
			return 0;
		}
		
		static int getSourceNoiseMap(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererImage*);
			pushCtypeValue(os, dynamic_cast<NoiseMap*>((NoiseMap*)&self->GetSourceNoiseMap()));
			return 1;
		}
		static int setSourceNoiseMap(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererImage*);
			NoiseMap * noiseMap = CtypeValue<NoiseMap*>::getArg(os, -params+0);
			if(!noiseMap){
				os->setException(OS_TEXT("NoiseRenderer.setSourceNoiseMap requires NoiseMap argument"));
				return 0;
			}
			NoiseMap * old = dynamic_cast<NoiseMap*>((NoiseMap*)&self->GetSourceNoiseMap());
			if(noiseMap != old){
				os->releaseValueById(os->findUserPointerValueId((void*)old));
				self->SetSourceNoiseMap(*noiseMap);
				os->retainValueById(os->findUserPointerValueId((void*)&self->GetSourceNoiseMap()));
			}
			return 0;
		}

	};
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &Lib::__constructNoiseRenderer},
		def(OS_TEXT("addGradientPoint"), &RendererImage::AddGradientPoint),
		def(OS_TEXT("buildGrayscaleGradient"), &RendererImage::BuildGrayscaleGradient),
		def(OS_TEXT("buildTerrainGradient"), &RendererImage::BuildTerrainGradient),
		def(OS_TEXT("clearGradient"), &RendererImage::ClearGradient),
		def(OS_TEXT("__get@light"), &RendererImage::IsLightEnabled),
		def(OS_TEXT("__set@light"), &RendererImage::EnableLight),
		def(OS_TEXT("__get@wrap"), &RendererImage::IsWrapEnabled),
		def(OS_TEXT("__set@wrap"), &RendererImage::EnableWrap),
		def(OS_TEXT("__get@lightAzimuth"), &RendererImage::GetLightAzimuth),
		def(OS_TEXT("__set@lightAzimuth"), &RendererImage::SetLightAzimuth),
		def(OS_TEXT("__get@lightBrightness"), &RendererImage::GetLightBrightness),
		def(OS_TEXT("__set@lightBrightness"), &RendererImage::SetLightBrightness),
		def(OS_TEXT("__get@lightColor"), &RendererImage::GetLightColor),
		def(OS_TEXT("__set@lightColor"), &RendererImage::SetLightColor),
		def(OS_TEXT("__get@lightContrast"), &RendererImage::GetLightContrast),
		def(OS_TEXT("__set@lightContrast"), &RendererImage::SetLightContrast),
		def(OS_TEXT("__get@lightElev"), &RendererImage::GetLightElev),
		def(OS_TEXT("__set@lightElev"), &RendererImage::SetLightElev),
		def(OS_TEXT("__get@lightIntensity"), &RendererImage::GetLightIntensity),
		def(OS_TEXT("__set@lightIntensity"), &RendererImage::SetLightIntensity),
		def(OS_TEXT("render"), &RendererImage::Render),
		{OS_TEXT("__get@backgroundImage"), &Lib::getBackgroundImage},
		{OS_TEXT("__set@backgroundImage"), &Lib::setBackgroundImage},
		{OS_TEXT("__get@destImage"), &Lib::getDestImage},
		{OS_TEXT("__set@destImage"), &Lib::setDestImage},
		{OS_TEXT("__get@sourceNoiseMap"), &Lib::getSourceNoiseMap},
		{OS_TEXT("__set@sourceNoiseMap"), &Lib::setSourceNoiseMap},
		{}
	};
	registerUserClass<RendererImage>(os, funcs);
}

// =================================================================
// =================================================================
// =================================================================

typedef noise::utils::RendererNormalMap RendererNormalMap;

template <> struct CtypeName<RendererNormalMap>{ static const OS_CHAR * getName(){ return OS_TEXT("NoiseRendererNormalMap"); } };
template <> struct CtypeValue<RendererNormalMap*>: public CtypeUserClass<RendererNormalMap*>{};
template <> struct UserDataDestructor<RendererNormalMap>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<RendererNormalMap*>((RendererNormalMap*)data));
		RendererNormalMap * self = (RendererNormalMap*)data;
		self->~RendererNormalMap();
		os->free(self);
	}
};

static void initRendererNormalMap(OS * os)
{
	struct Lib {
		static int __constructRendererNormalMap(OS * os, int params, int, int, void*)
		{
			RendererNormalMap * r = new (os->malloc(sizeof(RendererNormalMap) OS_DBG_FILEPOS)) RendererNormalMap();
			return initNoiseObjectAttrs(os, r, params);
		}

		static int getDestImage(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererNormalMap*);
			pushCtypeValue(os, dynamic_cast<NoiseImage*>((NoiseImage*)&self->GetDestImage()));
			return 1;
		}
		static int setDestImage(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererNormalMap*);
			NoiseImage * image = CtypeValue<NoiseImage*>::getArg(os, -params+0);
			if(!image){
				os->setException(OS_TEXT("NoiseRendererNormalMap.setDestImage requires NoiseImage argument"));
				return 0;
			}
			NoiseImage * old = dynamic_cast<NoiseImage*>((NoiseImage*)&self->GetDestImage());
			if(image != old){
				os->releaseValueById(os->findUserPointerValueId((void*)old));
				self->SetDestImage(*image);
				os->retainValueById(os->findUserPointerValueId((void*)&self->GetDestImage()));
			}
			return 0;
		}
		
		static int getSourceNoiseMap(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererNormalMap*);
			pushCtypeValue(os, dynamic_cast<NoiseMap*>((NoiseMap*)&self->GetSourceNoiseMap()));
			return 1;
		}
		static int setSourceNoiseMap(OS * os, int params, int, int, void*)
		{
			OS_GET_SELF(RendererNormalMap*);
			NoiseMap * noiseMap = CtypeValue<NoiseMap*>::getArg(os, -params+0);
			if(!noiseMap){
				os->setException(OS_TEXT("NoiseRenderer.setSourceNoiseMap requires NoiseMap argument"));
				return 0;
			}
			NoiseMap * old = dynamic_cast<NoiseMap*>((NoiseMap*)&self->GetSourceNoiseMap());
			if(noiseMap != old){
				os->releaseValueById(os->findUserPointerValueId((void*)old));
				self->SetSourceNoiseMap(*noiseMap);
				os->retainValueById(os->findUserPointerValueId((void*)&self->GetSourceNoiseMap()));
			}
			return 0;
		}

	};
	OS::FuncDef funcs[] = {
		{OS_TEXT("__construct"), &Lib::__constructRendererNormalMap},
		def(OS_TEXT("__get@bumpHeight"), &RendererNormalMap::GetBumpHeight),
		def(OS_TEXT("__set@bumpHeight"), &RendererNormalMap::SetBumpHeight),
		def(OS_TEXT("__get@wrap"), &RendererNormalMap::IsWrapEnabled),
		def(OS_TEXT("__set@wrap"), &RendererNormalMap::EnableWrap),
		def(OS_TEXT("render"), &RendererNormalMap::Render),
		{OS_TEXT("__get@destImage"), &Lib::getDestImage},
		{OS_TEXT("__set@destImage"), &Lib::setDestImage},
		{OS_TEXT("__get@sourceNoiseMap"), &Lib::getSourceNoiseMap},
		{OS_TEXT("__set@sourceNoiseMap"), &Lib::setSourceNoiseMap},
		{}
	};
	registerUserClass<RendererNormalMap>(os, funcs);
}

void initNoiseUtils(OS * os)
{
	struct Lib {
		static int latLonToXYZ(OS * os, int params, int, int, void*)
		{
			double x, y, z;
			noise::LatLonToXYZ(os->toDouble(-params+0), os->toDouble(-params+1), x, y, z);
			os->pushNumber(x);
			os->pushNumber(y);
			os->pushNumber(z);
			return 3;
		}

		static int gradientCoherentNoise3D(OS * os, int params, int, int, void*)
		{
			double x = params > 0 ? os->toDouble(-params+0) : 0;
			double y = params > 1 ? os->toDouble(-params+1) : 0;
			double z = params > 2 ? os->toDouble(-params+2) : 0;
			int seed = params > 3 ? os->toInt(-params+3) : 0;
			NoiseQuality noiseQuality = params > 4 ? (NoiseQuality)os->toInt(-params+3) : QUALITY_STD;
			double value = noise::GradientCoherentNoise3D(x, y, z, seed, noiseQuality);
			os->pushNumber(value);
			return 1;
		}

		static int valueCoherentNoise3D(OS * os, int params, int, int, void*)
		{
			double x = params > 0 ? os->toDouble(-params+0) : 0;
			double y = params > 1 ? os->toDouble(-params+1) : 0;
			double z = params > 2 ? os->toDouble(-params+2) : 0;
			int seed = params > 3 ? os->toInt(-params+3) : 0;
			NoiseQuality noiseQuality = params > 4 ? (NoiseQuality)os->toInt(-params+3) : QUALITY_STD;
			double value = noise::ValueCoherentNoise3D(x, y, z, seed, noiseQuality);
			os->pushNumber(value);
			return 1;
		}
	};
	OS::FuncDef funcs[] = {
		{OS_TEXT("latLonToXYZ"), &Lib::latLonToXYZ},
		{OS_TEXT("gradientCoherentNoise3D"), &Lib::gradientCoherentNoise3D},
		{OS_TEXT("valueCoherentNoise3D"), &Lib::valueCoherentNoise3D},
		def(OS_TEXT("gradientNoise3D"), &noise::GradientNoise3D),
		def(OS_TEXT("intValueNoise3D"), &noise::IntValueNoise3D),
		def(OS_TEXT("valueNoise3D"), &noise::ValueNoise3D),
		{}
	};
	os->getModule(OS_TEXT("noise"));
	os->setFuncs(funcs);
	os->pop();
}

void initLibNoiseExtension(OS* os)
{
	initNoiseModule(os);
	initNoisePerlin(os);
	initNoiseBillow(os);
	initNoiseRidgedMulti(os);
	initNoiseCylinders(os);
	initNoiseSpheres(os);
	initNoiseScalePoint(os);
	initNoiseScaleBias(os);
	initNoiseDisplace(os);
	initNoiseAdd(os);
	initNoiseMax(os);
	initNoiseMin(os);
	initNoiseMultiply(os);
	initNoisePower(os);
	initNoiseInvert(os);
	initNoiseAbs(os);
	initNoiseBlend(os);
	initNoiseCache(os);
	initNoiseCheckerboard(os);
	initNoiseTurbulence(os);
	initNoiseVoronoi(os);
	initNoiseTranslatePoint(os);
	initNoiseRotatePoint(os);
	initNoiseSelect(os);
	initNoiseClamp(os);
	initNoiseConst(os);
	initNoiseExponent(os);
	initNoiseCurve(os);
	initNoiseTerrace(os);

	initNoiseMap(os);
	initNoiseMapBuilder(os);
	initNoiseMapBuilderCylinder(os);
	initNoiseMapBuilderPlane(os);
	initNoiseMapBuilderSphere(os);

	initNoiseImage(os);
	initNoiseRenderer(os);
	initRendererNormalMap(os);

	initNoiseUtils(os);

	os->eval(OS_AUTO_TEXT(
		NoiseAbstractModule.attrs = NoiseMapBuilder.attrs = 
		NoiseRenderer.attrs = NoiseRendererNormalMap.attrs = function(attrs){
			for(var k, v in attrs){
				this[k] = v
			}
			return this
		}
		function NoiseAbstractModule.__get@sourceModule(){ return @getSourceModule(0) }
		function NoiseAbstractModule.__set@sourceModule(s){ @setSourceModule(0, s) }
		for(var i = 0; i < 10; i++){
			{|i|
				NoiseAbstractModule["__get@sourceModule"..i] = {||@getSourceModule(i)}
				NoiseAbstractModule["__set@sourceModule"..i] = {|s|@setSourceModule(i, s)}
			}(i)
		}

		function NoiseRenderer.gradient(gradient){
			@clearGradient()
			for(var k, v in gradient){
				@addGradientPoint(k, v)
			}
			return this
		}

		NoiseSelect.__get@controlModule = NoiseSelect.__get@sourceModule2
		NoiseSelect.__set@controlModule = NoiseSelect.__set@sourceModule2

		NoiseBlend.__get@controlModule = NoiseBlend.__get@sourceModule2
		NoiseBlend.__set@controlModule = NoiseBlend.__set@sourceModule2

		NoiseDisplace.__get@xDisplaceModule = NoiseDisplace.__get@sourceModule1
		NoiseDisplace.__set@xDisplaceModule = NoiseDisplace.__set@sourceModule1

		NoiseDisplace.__get@yDisplaceModule = NoiseDisplace.__get@sourceModule2
		NoiseDisplace.__set@yDisplaceModule = NoiseDisplace.__set@sourceModule2
		
		NoiseDisplace.__get@zDisplaceModule = NoiseDisplace.__get@sourceModule3
		NoiseDisplace.__set@ZDisplaceModule = NoiseDisplace.__set@sourceModule3
	));

}

} // namespace ObjectScript
