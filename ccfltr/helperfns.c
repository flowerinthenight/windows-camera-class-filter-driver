/*
 *		Copyright (C) 2015 Chew Esmero
 *
 *		This program is free software: you can redistribute it and/or modify
 *		it under the terms of the GNU General Public License as published by
 *		the Free Software Foundation, either version 3 of the License, or
 *		(at your option) any later version.
 *
 *		This program is distributed in the hope that it will be useful,
 *		but WITHOUT ANY WARRANTY; without even the implied warranty of
 *		MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *		GNU General Public License for more details.
 *
 *		You should have received a copy of the GNU General Public License
 *		along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
