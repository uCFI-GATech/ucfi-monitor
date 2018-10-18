set (KB3SourceCodes
		Monitor.h
		Monitor.cpp
		ExternalLibrary.h
		ExternalLibrary.cpp
		NodeFactory.h
		NodeFactory.cpp
		ControlFlowMonitor.cpp
		ControlFlowMonitor.h
		PTA.h
		PTA.cpp
		PTAAction.h
		PTAAction.cpp
		KB3Instruction.h
		KB3Instruction.cpp
		Node.cpp
		Node.h
		pt.h
		pt.cpp
)

# Build libraries.
#add_library (KB3Obj OBJECT ${KB3SourceCodes})
#add_library (KB3 SHARED $<TARGET_OBJECTS:KB3Obj>)
#add_library (KB3Static STATIC $<TARGET_OBJECTS:KB3Obj>)

# Build executable, HtleakMain.
set (EXECUTABLE_OUTPUT_PATH ${KB3_BINARY_DIR})
link_directories (${KB3_BINARY_DIR}/lib)
add_executable(KB3Main ${KB3SourceCodes})
target_link_libraries(KB3Main 
  LLVMAsmParser 
  LLVMSupport 
  LLVMCore 
  LLVMAnalysis
  LLVMIRReader
  LLVMBitWriter
  )
