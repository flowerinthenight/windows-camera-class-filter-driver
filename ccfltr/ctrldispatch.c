/*
* Copyright(c) 2015 Chew Esmero
* All rights reserved.
*/

#include "ccfltr.h"

NTSTATUS ControlDispatchIoTestDirect(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp)
{
    NTSTATUS ntStatus = STATUS_SUCCESS;
    PCONTROL_DEVICE_EXTENSION pControlExtension;
    PIO_STACK_LOCATION pIrpStack;
    PVOID pStreamBuffer;
    ULONG ulBufferSize;
    LPBYTE lpOutBuffer = NULL;
    
    PAGED_CODE();
    
    pControlExtension = (PCONTROL_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

    pIrpStack = IoGetCurrentIrpStackLocation(Irp);

    L("[%s] ControlObject: IOCTL_LENCFLTR_TEST (Out_Direct)\n", FN);

    /*
     * In this type of transfer, the I/O manager allocates a system buffer large enough to accommodate the User input
     * buffer, sets the buffer address in Irp->AssociatedIrp.SystemBuffer and copies the content of user input buffer
     * into the SystemBuffer. For the output buffer, the I/O manager probes to see whether the virtual address is
     * writable in the caller's access mode, locks the pages in memory and passes the pointer to MDL describing the
     * buffer in Irp->MdlAddress.
     */
#if 0
    inBuf = Irp->AssociatedIrp.SystemBuffer;

    SIOCTL_KDPRINT(("\tData from User : "));
    PrintChars(inBuf, inBufLength);
#endif

#if 1
    /*
     * To access the output buffer, just get the system address for the buffer. For this method, this buffer is intended
     * for transfering data from the driver to the application.
     */
    lpOutBuffer = (LPBYTE)MmGetSystemAddressForMdlSafe(Irp->MdlAddress, NormalPagePriority);

    if (!lpOutBuffer) {
        L("[%s] ControlObject: STATUS_INSUFFICIENT_RESOURCES\n", FN);
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    if (lpOutBuffer && pControlExtension->FilterExtension->StreamBuffer) {
        pStreamBuffer = pControlExtension->FilterExtension->StreamBuffer;
        ulBufferSize = pControlExtension->FilterExtension->VideoInfoHeader.DataFormat.SampleSize;
        
        RtlCopyMemory(lpOutBuffer, pStreamBuffer, ulBufferSize);
    }

    Irp->IoStatus.Information = pIrpStack->Parameters.DeviceIoControl.OutputBufferLength;

	/* Changes made to the SystemBuffer are not copied to the user input buffer by the I/O manager. */
#endif

    return ntStatus;
}

NTSTATUS ControlDispatchIoBufferRefresh(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp)
{
    NTSTATUS ntStatus = STATUS_SUCCESS;
    NTSTATUS ntStatusSection = STATUS_SUCCESS;
    PCONTROL_DEVICE_EXTENSION pControlExtension;
    PDEVICE_EXTENSION pFilterExtension;
    UNICODE_STRING uniSm;
    OBJECT_ATTRIBUTES objSm;
    SHAREDBUFFER_HEADER buffHeader;
    HANDLE hSection;
    LARGE_INTEGER liOffset;
    PVOID pBaseAddr = NULL;
    LPBYTE pKsSideBuffer = NULL;
    LPBYTE pUserBuffer = NULL;
    SIZE_T cbSize = 0;
    ULONG ulSampleSize = 0;

    UNREFERENCED_PARAMETER(Irp);
    
    PAGED_CODE();

    pControlExtension = (PCONTROL_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

    liOffset.u.HighPart = 0;
    liOffset.u.LowPart = 0;

    RtlInitUnicodeString(&uniSm, L"\\BaseNamedObjects\\{9E2288A3-7955-428F-9064-9C05C0EDC608}");
    InitializeObjectAttributes(&objSm, &uniSm, OBJ_FORCE_ACCESS_CHECK | OBJ_KERNEL_HANDLE, NULL, NULL);

    ntStatus = ZwOpenSection(&hSection, SECTION_ALL_ACCESS, &objSm);

    if (NT_SUCCESS(ntStatus)) {
        ntStatus = ZwMapViewOfSection(hSection,
                                      NtCurrentProcess(),
                                      &pBaseAddr,
                                      0,
                                      0,
                                      &liOffset,
                                      &cbSize,
                                      ViewUnmap,
                                      0,
                                      PAGE_READWRITE);

        ntStatusSection = ntStatus;

        try {
            /* Check first for user buffer validity. */
            ProbeForWrite(pBaseAddr, cbSize, sizeof(BYTE));

            if (NT_SUCCESS(ntStatus)) {
                pFilterExtension = pControlExtension->FilterExtension;
                pKsSideBuffer = (LPBYTE)pFilterExtension->StreamBuffer;

                if (InterlockedCompareExchange(&pFilterExtension->IsVideoInfoHeader2, MAXLONG, MAXLONG) > 0) {
                    ulSampleSize = pFilterExtension->VideoInfoHeader2.DataFormat.SampleSize;
                    buffHeader.dwFrameType = pFilterExtension->VideoInfoHeader2.DataFormat.SubFormat.Data1;
                    buffHeader.cbFrameSize = ulSampleSize;
                    buffHeader.dwWidth = pFilterExtension->VideoInfoHeader2.VideoInfoHeader2.bmiHeader.biWidth;
                    buffHeader.dwHeight = pFilterExtension->VideoInfoHeader2.VideoInfoHeader2.bmiHeader.biHeight;
                } else {
                    ulSampleSize = pFilterExtension->VideoInfoHeader.DataFormat.SampleSize;
                    buffHeader.dwFrameType = pFilterExtension->VideoInfoHeader.DataFormat.SubFormat.Data1;
                    buffHeader.cbFrameSize = ulSampleSize;
                    buffHeader.dwWidth = pFilterExtension->VideoInfoHeader.VideoInfoHeader.bmiHeader.biWidth;
                    buffHeader.dwHeight = pFilterExtension->VideoInfoHeader.VideoInfoHeader.bmiHeader.biHeight;
                }

                pUserBuffer = (LPBYTE)pBaseAddr;

                if (cbSize >= (ulSampleSize + sizeof(SHAREDBUFFER_HEADER))) {
                    /* Also check every time user buffer is accessed. */
                    try {
                        RtlCopyMemory(pUserBuffer, &buffHeader, sizeof(SHAREDBUFFER_HEADER));
                        pUserBuffer += sizeof(SHAREDBUFFER_HEADER);
                        RtlCopyMemory(pUserBuffer, pKsSideBuffer, ulSampleSize);
                    } except (EXCEPTION_EXECUTE_HANDLER) {
                        ntStatus = GetExceptionCode();
                        L("[%s] ControlObject: Exception in 0x%p = 0x%x\n", FN, pUserBuffer, ntStatus);
                    }
                }

                ZwUnmapViewOfSection(NtCurrentProcess(), pBaseAddr);
            } else {
                L("[%s] ControlObject: ZwMapViewOfSection error 0x%x\n", FN, ntStatus);
            }
        } except (EXCEPTION_EXECUTE_HANDLER) {
            ntStatus = GetExceptionCode();
            L("[%s] ControlObject: Exception in 0x%p = 0x%x\n", FN, pBaseAddr, ntStatus);
            
            if (NT_SUCCESS(ntStatusSection)) {
                /* Still unmap the view even if we get an exception. */
                ZwUnmapViewOfSection(NtCurrentProcess(), pBaseAddr);
            }
        }

        ZwClose(hSection);
    } else {
        L("[%s] ControlObject: ZwOpenSection error 0x%x\n", FN, ntStatus);
    }
    
    return ntStatus;
}

NTSTATUS ControlDispatchIoStreamStart(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp)
{
    NTSTATUS ntStatus = STATUS_SUCCESS;
    PIO_STACK_LOCATION pIrpStack;
    PCONTROL_DEVICE_EXTENSION pControlExtension;
    PDEVICE_EXTENSION pFilterExtension;
    PDWORD pdwProcessId;
    DWORD cbInSize;
    HANDLE hStreamerPid;
    LONG lStreamPid;
    
    PAGED_CODE();

    pIrpStack = IoGetCurrentIrpStackLocation(Irp);

    pControlExtension = (PCONTROL_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

    hStreamerPid = PsGetCurrentProcessId();

    L("[%s] IOCTL_LENCFLTR_INTRNAL_STRM_START: ProcID: %d\n", FN, hStreamerPid);

    cbInSize = pIrpStack->Parameters.DeviceIoControl.InputBufferLength;

    if (cbInSize == sizeof(DWORD)) {
        pdwProcessId = (PDWORD)Irp->AssociatedIrp.SystemBuffer;
        L("[%s] IOCTL_LENCFLTR_INTRNAL_STRM_START: ProcID (in): %d\n", FN, *pdwProcessId);

        if (hStreamerPid == (HANDLE)*pdwProcessId) {
            pFilterExtension = pControlExtension->FilterExtension;
#pragma warning(disable:4311)
            lStreamPid = (LONG)hStreamerPid;
#pragma warning(default:4311)
            InterlockedAnd(&pFilterExtension->StreamerPid, 0);
            InterlockedCompareExchange(&pFilterExtension->StreamerPid, lStreamPid, 0);
        }

        ntStatus = STATUS_SUCCESS;
    } else {
        ntStatus = STATUS_INVALID_PARAMETER;
    }

    return ntStatus;
}

NTSTATUS ControlDispatchIoStreamStop(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp)
{
    NTSTATUS ntStatus = STATUS_SUCCESS;
    PIO_STACK_LOCATION pIrpStack;
    PCONTROL_DEVICE_EXTENSION pControlExtension;
    PDEVICE_EXTENSION pFilterExtension;
    PDWORD pdwProcessId;
    DWORD cbInSize;
    HANDLE hStreamerPid;
    LONG lStreamPid;

    PAGED_CODE();

    pIrpStack = IoGetCurrentIrpStackLocation(Irp);

    pControlExtension = (PCONTROL_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

    hStreamerPid = PsGetCurrentProcessId();

    L("[%s] IOCTL_LENCFLTR_INTRNAL_STRM_STOP: ProcID: %d\n", FN, hStreamerPid);

    cbInSize = pIrpStack->Parameters.DeviceIoControl.InputBufferLength;

    if (cbInSize == sizeof(DWORD)) {
        pdwProcessId = (PDWORD)Irp->AssociatedIrp.SystemBuffer;
        L("[%s] IOCTL_LENCFLTR_INTRNAL_STRM_STOP: ProcID (in): %d\n", FN, *pdwProcessId);

        /* Reset to zero if this is our streamer. */
        if (hStreamerPid == (HANDLE)*pdwProcessId) {
            pFilterExtension = pControlExtension->FilterExtension;
#pragma warning(disable:4311)
            lStreamPid = (LONG)hStreamerPid;
#pragma warning(default:4311)
            InterlockedCompareExchange(&pFilterExtension->StreamerPid, 0, lStreamPid);
        }

        ntStatus = STATUS_SUCCESS;
    } else {
        ntStatus = STATUS_INVALID_PARAMETER;
    }

    return ntStatus;
}
