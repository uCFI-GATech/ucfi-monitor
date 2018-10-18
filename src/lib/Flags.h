#ifndef __FLAGS__
#define __FLAGS__

/* =========================================================
 * flags here
 * =========================================================
 */

//#define DUMP_RESULT	// dump CFI success/empty/fail to log.1
//#define DUMP_PARSER	// dump the PT parse to log.2
//#define DUMP_READER	// dump the PT read in doAnalysis() to log.1
//#define DUMP_NODE		// dump the node content to errs()
//#define DUMP_CALL_STACK	// dump the push/pop
//#define DUMP_CHECK
#define DUMP_STATISTICS	// do statistics

#define MAX_BB_NUM	1000000
#define HEX(x) format("0x%lx", x)

/* =========================================================
 * operations here
 *
 * do not change code below this line, unless you know
 * what you are doing
 * =========================================================
 */

#ifdef DUMP_STATISTICS
#define DS(x)	x
#else
#define DS(x)
#endif

#ifdef DUMP_RESULT
#define DRE(x)	x
#else
#define DRE(x)
#endif

#ifdef DUMP_CHECK
#define DC(x)	x
#else
#define DC(x)
#endif

#define DUMP_in_func(x,y) \
	do { \
		const Function *topFunc = PTA::CallStack.back(); \
		if (topFunc->getName() == (y)) \
			(x); \
	} while (0)

#define DUMP_in_ID(x) \
	do { \
		unsigned long IDArray[] = { \
			11460, /* %sec.147.i is phinode from %sec.2.i, or %sec.246.i */\
			14056, /* %sec.2.i = load (gepi from %sec.247.i)*/ \
		}; \
		int index = 0; \
		for (index = 0; index < sizeof(IDArray) / sizeof(IDArray[0]); index ++) \
			if (BB_packet == IDArray[index]) {\
				(x); \
				break; \
			} \
	} while (0)


static  void dump_func(Node * aNode) {
	if (!aNode) {
		return; 
	}

	aNode->dump();
	return;
}

static void dump_func2(Node * aNode) {
	int64_t offset = 144 + aNode->startOffset; 
	aNode = aNode->mem[0][offset];

	offset = 16 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 16 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 16 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 16 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 16 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 16 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 16 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 16 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 240 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 8 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 864 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	offset = 720 + aNode->startOffset;
	aNode = aNode->mem[0][offset];

	aNode->dump();
}


static void dump_func3(Node * aNode) {
	int64_t offset = 0 + aNode->startOffset; 
	aNode = aNode->mem[0][offset];

	offset = 0 + aNode->startOffset; 
	aNode = aNode->mem[0][offset];

	offset = 64 + aNode->startOffset; 
	aNode = aNode->mem[0][offset];

	offset = 80 + aNode->startOffset; 
	aNode = aNode->mem[0][offset];

	aNode->dump();
}

static unsigned long IDArray_bak[] = { \
			4810, /* call to copy_object */ \
			13263, /* call instruction to copy_section*/ \
			13299, /* call instruction to _bfd_elf_compute_section_file_positions*/ \
			13439, /* gepi %124 from %abfd */ \
			14771, /* load %sec.246.i from %124*/ \
			14832, /* lead to empty */ \
			1872, /* before first call to bfd_section_frm_shdr*/\
			1973, /* 2nd call to bfd_section_from_shdr */ \
			2095, /* 3rd */ \
			2222, /* 4th */ \
			2304, /* 5th */ \
			2386, /* 6th */ \
			2473, /* 7th */ \
			2561, /* 8th */ \
			2648, /* 9th */ \
			2663, /* 10th - with 5 */\
			2747, /* 11th - with 10*/\
};

#if 0
#define DUMP_in_counter(x) \
	do { \
		unsigned long IDArray[] = { \
			27748, /* first call to bfd_check_format_matches*/ \
		}; \
		int index = 0; \
		for (index = 0; index < sizeof(IDArray) / sizeof(IDArray[0]); index ++) {\
			Node * aNode = (Node *)0x24c3090; \
			if (global_counter == 27746) {\
				(x); \
				break; \
			} else if (global_counter == IDArray[index]) {\
				(x);\
				dump_func3(aNode); \
				break;\
			} \
		}\
	} while (0)
#else
#define DUMP_in_counter(x) \
	do { \
		unsigned long IDArray[] = { \
			31675, \
		}; \
		int index = 0; \
		for (index = 0; index < sizeof(IDArray) / sizeof(IDArray[0]); index ++) {\
			if (global_counter == IDArray[index]) {\
				(x);\
				break;\
			} \
		}\
	} while (0)
#endif

#define DUMP_if_counter(x) \
	do {\
		if ((global_counter >= 28093 && global_counter <= 28098)) {\
			(x); \
		} \
	}while(0)


#ifdef DUMP_NODE
#if 1
#define DN(x) DUMP_in_counter(x)
//#define DN(x) DUMP_if_counter(x)
//#define DN(x) DUMP_in_func(x, "bfd_fopen");DUMP_in_counter(x)
//#define DN(x) DUMP_in_ID(x) 
#if 0
#define DN(x) \
	do {\
		if (global_counter >= 1660 && global_counter <= 1700 )\
		{\
			x; \
			((Node *)0x922c4a0)->dump(); \
		} else if (global_counter == 1566) { \
			x;\
		}; \
	} while (0)
#endif
//#define DN(x)
#else
#define DN(x)	x
#endif
#else
#define DN(x)
#endif

#ifdef DUMP_PARSER
#define DP(x)	x
#else
#define DP(x)
#endif

#ifdef DUMP_READER
#define DR(x)	x
#else
#define DR(x)
#endif

#ifdef DUMP_CALL_STACK
#define DCS(x) x
#else
#define DCS(x)
#endif

#endif
