#include "common.h"
#include "ExternalLibrary.h"

const char* ExternalLibrary::noopFuncs[] = {
	"log", "log10", "exp", "exp2", "exp10", "strcmp", "strncmp", "strlen",
	"atoi", "atof",	"atol", "atoll", "remove", "unlink", "rename", "memcmp", "free",
	"execl", "execlp", "execle", "execv", "execvp", "chmod",
	"puts", "write", "open", "create", "truncate", "chdir", "mkdir", "rmdir",
	"read", "pipe",	"wait", "time",	"stat", "fstat", "lstat", "strtod",	"strtof",
	"strtold", "fdopen", "fflush", "feof", "fileno", "clearerr", "rewind",
	"ftell", "ferror", "fgetc",	"fgetc", "_IO_getc", "fwrite", "fread",	"fgets",
	"ungetc", "fputc", "fputs", "putc",	"ftell", "rewind", "_IO_putc", "fseek",
	"fgetpos", "fsetpos", "printf", "fprintf", "sprintf", "vprintf", "vfprintf",
	"vsprintf", "scanf", "fscanf", "sscanf", "__assert_fail", "modf", "putchar",
	"isalnum", "isalpha", "isascii", "isatty", "isblank", "iscntrl", "isdigit",
	"isgraph", "islower", "isprint", "ispunct", "isspace", "isupper", "iswalnum",
	"iswalpha", "iswctype", "iswdigit", "iswlower", "iswspace", "iswprint",
	"iswupper", "sin", "cos", "sinf", "cosf", "asin", "acos", "tan", "atan",
	"fabs", "pow", "floor", "ceil", "sqrt", "sqrtf", "hypot", 
	"random", "tolower","toupper", "towlower", "towupper", "system", "clock",
	"exit", "abort", "gettimeofday", "settimeofday", "sleep", "ctime",
	"strspn", "strcspn", "localtime", "strftime",
	"qsort", "popen", "pclose",
	"rand", "rand_r", "srand", "seed48", "drand48", "lrand48", "srand48",
	"__isoc99_sscanf", "__isoc99_fscanf", "fclose", "close", "perror", 
//	"strerror", // this function returns an extenal static pointer
	"__errno_location", "abs", "difftime", "setbuf",
	"_ZdlPv", "_ZdaPv",	// delete and delete[]
	"fesetround", "fegetround", "fetestexcept", "feraiseexcept", "feclearexcept",
	"llvm.bswap.i16", "llvm.bswap.i32", "llvm.ctlz.i64",
	"llvm.lifetime.start", "llvm.lifetime.end", "llvm.stackrestore",
	"memset", "llvm.memset.i32", "llvm.memset.p0i8.i32", "llvm.memset.i64",
	"llvm.memset.p0i8.i64", "llvm.va_end",
	// The following functions might not be NOOP. They need to be removed from this list in the future
	"setrlimit", "getrlimit", "_obstack_begin", "getpagesize", "pthread_mutex_init", "pthread_mutex_lock",
	"pthread_mutex_unlock", "_obstack_newchunk", "llvm.expect.i64",
	// Multiprocess
	//"fork",
	nullptr
};

const char* ExternalLibrary::mallocFuncs[] = {
	"malloc", "valloc", "calloc",
	"_Znwj", "_ZnwjRKSt9nothrow_t", "_Znwm", "_ZnwmRKSt9nothrow_t", 
	"_Znaj", "_ZnajRKSt9nothrow_t", "_Znam", "_ZnamRKSt9nothrow_t", 
	"strdup", "strndup",
	"getenv",
	"memalign", "posix_memalign", "__ctype_b_loc",
	"strerror", // this function returns an extenal static pointer
	"fopen",
	nullptr
};

const char* ExternalLibrary::reallocFuncs[] = {
	"realloc", "strtok", "strtok_r", "getcwd",
	nullptr
};

const char* ExternalLibrary::retArg0Funcs[] = {
	"fgets", "gets", "stpcpy",  "strcat", "strchr", "strcpy",
	"strerror_r", "strncat", "strncpy", "strpbrk", "strptime", "strrchr", "strstr",
	nullptr
};

const char* ExternalLibrary::retArg1Funcs[] = {
	// Actually the return value of signal() will NOT alias its second argument, 
	// but if you call it twice the return values may alias.
	// We're making conservative assumption here
	"signal",
	nullptr
};

const char* ExternalLibrary::retArg2Funcs[] = {
	"freopen",
	nullptr
};

const char* ExternalLibrary::memcpyFuncs[] = {
	"llvm.memcpy.i32", "llvm.memcpy.p0i8.p0i8.i32", "llvm.memcpy.i64",
	"llvm.memcpy.p0i8.p0i8.i64", "llvm.memmove.i32", "llvm.memmove.p0i8.p0i8.i32",
	"llvm.memmove.i64", "llvm.memmove.p0i8.p0i8.i64",
	"memcpy", "memmove", "bcopy",
	nullptr
};

const char* ExternalLibrary::convertFuncs[] = {
	"strtod", "strtof", "strtol", "strtold", "strtoll", "strtoul",
	nullptr
};

bool ExternalLibrary::lookupName(const char* table[], const char* str)
	{
	for (unsigned i = 0; table[i] != nullptr; ++i)
	{
		if (strcmp(table[i], str) == 0)
			return true;
	}
	return false;
}
