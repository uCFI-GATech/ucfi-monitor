/*
 * pt.h is a single-file library for processing Intel PT trace.
 * Specifically, it can
 *   (1) decode Intel PT packets: pt_get_packet(buf, size, &len)
 *       buf: a pointer to the trace buffer
 *       size: the size of the trace buffer
 *       &len: the size of the decoded trace packet
 *       @ret: the type of the decoded trace packet
 *   (2) recover control flows: pt_recover(buf, size, arg)
 *       buf: a pointer to the trace buffer
 *       size: the size of the trace buffer
 *       arg: a general arg to connect the caller specified
 *            context to the callbacks (see below)
 *       @ret: void
 *
 * It is designed to be source-based for maximum performance.
 * To recover the control flow, it depends on the pre-defined
 * interfaces for finding, understanding and following basic
 * blocks:
 *   - bool pt_block_is_call(block)
 *   - bool pt_block_is_direct(block)
 *   - bool pt_block_is_ret(block)
 *   - bool pt_block_is_cond(block)
 *   - bool pt_block_is_syscall(block)
 *   - pt_block *pt_get_block(addr)
 *   - bool pt_in_block(addr, block)
 *   - unsigned long pt_get_fallthrough_addr(block)
 *   - pt_block *pt_get_fallthrough_block(block)
 *   - unsigned long pt_get_target_addr(block)
 *   - pt_block *pt_get_target_block(block)
 *
 * It also provides a default implementation of the above
 * interfaces using mirror pages when macro PT_USE_MIRROR
 * is defined when including this file.
 * The default version relies on pre-defined interfaces to
 * identifiy code page and mirror page given an address in
 * the Intel PT trace buffer:
 *   - unsigned long PT_IP_TO_CODE(unsigned long)
 *   - unsigned long PT_IP_TO_BLOCK(unsigned long)
 *   - unsigned long PT_CODE_TO_BLOCK(unsigned long)
 *   - unsigned long PT_CODE_TO_IP(unsigned long)
 *   - unsigned long PT_BLOCK_TO_CODE(unsigned long)
 *   - unsigned long PT_BLOCK_TO_IP(unsigned long)
 *
 * In addition, the default implementation uses distorm3
 * library for the purpose of on-demand disassembling, and
 * encodes the logic into pt_disasm_block function.
 * (Feel free to replace it as needed :P)
 *
 * It provides callbacks while recovering control flows:
 *   - void pt_on_block(addr, arg)
 *   - void pt_on_call(addr, arg)
 *   - void pt_on_ret(addr, arg)
 *   - void pt_on_xabort(arg)
 *   - void pt_on_xbegin(arg)
 *   - void pt_on_xcommit(arg)
 *   - void pt_on_mode(payload, arg)
 */

#ifndef _PT_H
#define _PT_H

#ifdef PT_USE_MIRROR

#include <mnemonics.h>
#include <stdlib.h>
#include <string.h>

#endif /* PT_USE_MIRROR */

enum pt_packet_kind {
	PT_PACKET_ERROR = -1,
	PT_PACKET_NONE,
	PT_PACKET_TNTSHORT,
	PT_PACKET_TNTLONG,
	PT_PACKET_TIP,
	PT_PACKET_TIPPGE,
	PT_PACKET_TIPPGD,
	PT_PACKET_FUP,
	PT_PACKET_PIP,
	PT_PACKET_MODE,
	PT_PACKET_TRACESTOP,
	PT_PACKET_CBR,
	PT_PACKET_TSC,
	PT_PACKET_MTC,
	PT_PACKET_TMA,
	PT_PACKET_CYC,
	PT_PACKET_VMCS,
	PT_PACKET_OVF,
	PT_PACKET_PSB,
	PT_PACKET_PSBEND,
	PT_PACKET_MNT,
	PT_PACKET_PAD,
	PT_PACKET_UCFI_IGNORED,
};

enum pt_packet_kind pt_get_packet(unsigned char *buffer, unsigned long size, unsigned long *len);

unsigned long pt_get_and_update_ip(unsigned char *packet, unsigned int len, unsigned long *last_ip);
#endif
