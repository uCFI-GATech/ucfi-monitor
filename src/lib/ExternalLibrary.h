#ifndef __KB3_EXTERNALLIBRARY_
#define __KB3_EXTERNALLIBRARY_

class ExternalLibrary {
	public:
		static const char* noopFuncs[];
		static const char* mallocFuncs[];
		static const char* reallocFuncs[];
		static const char* retArg0Funcs[];
		static const char* retArg1Funcs[];
		static const char* retArg2Funcs[];
		static const char* memcpyFuncs[];
		static const char* convertFuncs[];

		static bool lookupName(const char* table[], const char* str);
};

enum {
	EXT_NOOP = 0,
	EXT_MALLOC,
	EXT_RETARG0,
	EXT_RETARG1,
	EXT_RETARG2,
	EXT_MEMCPY,
	EXT_CONVERT,
	EXT_VASTART,
	EXT_UNKNOWN,
} ExtFuncType;

#endif
