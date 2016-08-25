/*
* Copyright(c) 2015 Chew Esmero
* All rights reserved.
*/

#include "ccfltr.h"

void PrintGuidValues(PCHAR pszFn, PWCHAR pszLabel, LPGUID lpGuid)
{
	L("[%s] %S: %08x-%04x-%04x-%02x%02x-%02x%02x%02x%02x%02x%02x\n",
		pszFn,
		pszLabel,
		lpGuid->Data1,
		lpGuid->Data2,
		lpGuid->Data3,
		lpGuid->Data4[0],
		lpGuid->Data4[1],
		lpGuid->Data4[2],
		lpGuid->Data4[3],
		lpGuid->Data4[4],
		lpGuid->Data4[5],
		lpGuid->Data4[6],
		lpGuid->Data4[7]);
}
