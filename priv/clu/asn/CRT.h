/*
 * Generated by asn1c-0.9.28 (http://lionet.info/asn1c)
 * From ASN.1 module "ROSTER"
 * 	found in "/Users/proger/cg/chat/src/ROSTER.asn1"
 */

#ifndef	_CRT_H_
#define	_CRT_H_


#include <asn_application.h>

/* Including external dependencies */
#include <OCTET_STRING.h>
#include <constr_SEQUENCE.h>

#ifdef __cplusplus
extern "C" {
#endif

/* CRT */
typedef struct CRT {
	OCTET_STRING_t	 der;
	
	/* Context for parsing across buffer boundaries */
	asn_struct_ctx_t _asn_ctx;
} CRT_t;

/* Implementation */
extern asn_TYPE_descriptor_t asn_DEF_CRT;

#ifdef __cplusplus
}
#endif

#endif	/* _CRT_H_ */
#include <asn_internal.h>
