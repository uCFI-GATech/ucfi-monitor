#include "PTA.h"
#include "ControlFlowMonitor.h"
#include "Monitor.h"
#include "KB3Instruction.h"

using namespace PTCFI;

void error(const char *msg) {
	perror(msg);
	exit(1);
}

int main(int argc, char** argv) {
	cl::ParseCommandLineOptions(argc, argv, "Parsing arguments\n");
	errs() << BitcodeFile << '\n';
	ErrorOr<std::unique_ptr<MemoryBuffer>> mb = MemoryBuffer::getFile(BitcodeFile);
	if (std::error_code ec = mb.getError()) {
		errs() << ec.message();
		return false;
	}
	LLVMContext context;
	ErrorOr<Module*> m = parseBitcodeFile(mb->get()->getMemBufferRef(), context);
	if (std::error_code ec = m.getError()) {
		errs() << ec.message();
		return false;
	}
	errs() << "===== IR file loaded =======\n";

	Module* module = m.get();
	PurifyModule2(module);
	errs() << "===== Module purified =====\n";

	// init PTA
	PTA* pta = new PTA(module);
	errs() << "===== Globals handled ======\n";

	ProcessModule(module, pta);
	errs() << "==== Module processed ======\n";

	// handle the bb info file
	HandleBBInfoFile(BBInfoFile, module);
	errs() << "===== BB file hanlded =====\n";

	initializePinTrace(PinTraceFile);
	
	pta->initializeMainArg(*module);

	errs() << "===== Ready to go =====\n";

	doAnalysis(pta);

	return 0;
}
