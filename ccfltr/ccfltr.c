/*
* Copyright(c) 2015 Chew Esmero
* All rights reserved.
*/

#include "ccfltr.h"

#ifdef ALLOC_PRAGMA
#pragma alloc_text(INIT, DriverEntry)                       /* driver entry point function */
#pragma alloc_text(PAGE, FilterAddDevice)                   /* main device object creation function */
#pragma alloc_text(PAGE, FilterDispatchPnp)                 /* pnp related IO dispatcher function */
#pragma alloc_text(PAGE, FilterUnload)                      /* unload function */
#endif

#ifdef ALLOC_PRAGMA
#pragma alloc_text(PAGE, FilterCreateControlObject)         /* sideband creation function */
#pragma alloc_text(PAGE, FilterDeleteControlObject)         /* sideband removal function */
#pragma alloc_text(PAGE, FilterDispatchIo)                  /* intermediate dispatch funtion */
#endif

#define TEST_MJPG_REPLACE 0

/*
 * Definitions of related GUIDs.
 */
GUID GUID_Specifier_VideoInfo = { STATIC_KSDATAFORMAT_SPECIFIER_VIDEOINFO };
GUID GUID_Specifier_VideoInfo2 = { STATIC_KSDATAFORMAT_SPECIFIER_VIDEOINFO2 };
GUID GUID_PROPSETID_Connection = { STATIC_KSPROPSETID_Connection };
GUID GUID_FormatSubtype_YUY2 = { STATIC_KSFORMAT_SUBTYPE_YUY2 };
GUID GUID_FormatSubtype_NV12 = { STATIC_KSFORMAT_SUBTYPE_NV12 };
GUID GUID_FormatSubtype_MJPG = { STATIC_KSFORMAT_SUBTYPE_MJPG };
GUID GUID_PROPSETID_VidcapCameraControl = { STATIC_PROPSETID_VIDCAP_CAMERACONTROL };

/*
 * Routine Description:
 *
 * Driver's main entry point function.
 */
NTSTATUS DriverEntry(__in PDRIVER_OBJECT DriverObject, __in PUNICODE_STRING RegistryPath)
{
    NTSTATUS ntStatus = STATUS_SUCCESS;
    PDRIVER_DISPATCH *pfnDriverDispatch;
    ULONG ulIndex;

    UNREFERENCED_PARAMETER (RegistryPath);

    L("[%s] Built %s %s\n", FN, __DATE__, __TIME__);

    /* Create dispatch points. */
    for (ulIndex = (ULONG)0, pfnDriverDispatch = DriverObject->MajorFunction;
            ulIndex <= IRP_MJ_MAXIMUM_FUNCTION;
            ulIndex++, pfnDriverDispatch++) {
        *pfnDriverDispatch = FilterPass;
    }

    DriverObject->MajorFunction[IRP_MJ_PNP] = FilterDispatchPnp;
    DriverObject->MajorFunction[IRP_MJ_POWER] = FilterDispatchPower;
    DriverObject->DriverExtension->AddDevice = FilterAddDevice;
    DriverObject->DriverUnload = FilterUnload;

    /* Set the following dispatch points as we will be doing something useful to these requests instead of just passing them down. */
    DriverObject->MajorFunction[IRP_MJ_CREATE] = 
        DriverObject->MajorFunction[IRP_MJ_CLOSE] = 
        DriverObject->MajorFunction[IRP_MJ_CLEANUP] = 
        DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = 
        FilterDispatchIo;

    return ntStatus;
}

/*
 * Routine Description:
 *
 * The Plug & Play subsystem is handing us a brand new PDO, for which we (by means of INF registration) have been asked to provide
 * a driver.
 *
 * We need to determine if we need to be in the driver stack for the device. Create a function device object to attach to
 * the stack, initialize that device object and return ntStatus success.
 * 
 * Remember: We can NOT actually send ANY non pnp IRPS to the given driver stack, UNTIL we have received an IRP_MN_START_DEVICE.
 */
NTSTATUS FilterAddDevice(__in PDRIVER_OBJECT DriverObject, __in PDEVICE_OBJECT PhysicalDeviceObject)
{
    NTSTATUS ntStatus = STATUS_SUCCESS;
    PDEVICE_OBJECT pDeviceObject = NULL;
    PDEVICE_EXTENSION pDeviceExtension = NULL;
    ULONG ulDeviceType = (ULONG)FILE_DEVICE_UNKNOWN;
    BOOL bPdoSupported = FALSE;
    ULONG cbSize;
    HANDLE hThreadTemp;
    WCHAR szTmpInfo[512];
    ULONG cbTmpInfoSize = 0;

    PAGED_CODE();

    /* IoIsWdmVersionAvailable(1, 0x20) returns TRUE on os after Windows 2000. */
    if (RtlIsNtDdiVersionAvailable(NTDDI_WINXP)) {
        /*
         * Win2K system bugchecks if the filter attached to a storage device doesn't specify the same DeviceType as the device it's
         * attaching to. This bugcheck happens in the filesystem when you disable the devicestack whose top level deviceobject
         * doesn't have a VPB. To workaround we will get the toplevel object's DeviceType and specify that in IoCreateDevice.
         */
        pDeviceObject = IoGetAttachedDeviceReference(PhysicalDeviceObject);
        ulDeviceType = pDeviceObject->DeviceType;
        ObDereferenceObject(pDeviceObject);
    }

    /* Create a filter device object. */
    ntStatus = IoCreateDevice(DriverObject,
                              sizeof(DEVICE_EXTENSION),
                              NULL,
                              ulDeviceType,
                              FILE_DEVICE_SECURE_OPEN,
                              FALSE,
                              &pDeviceObject);

    if (!NT_SUCCESS(ntStatus)) {
        /*
         * Returning failure here prevents the entire stack from functioning, but most likely the rest of the stack will not be
         * able to create device objects either, so it is still OK.
         */
        return ntStatus;
    }

    L("[%s] AddDevice PDO (0x%p) FDO (0x%p)\n", FN, PhysicalDeviceObject, pDeviceObject);

    pDeviceExtension = (PDEVICE_EXTENSION)pDeviceObject->DeviceExtension;
    pDeviceExtension->Common.Type = DEVICE_TYPE_FIDO;
    pDeviceExtension->NextLowerDriver = IoAttachDeviceToDeviceStack(pDeviceObject, PhysicalDeviceObject);

    L("[%s] AddDevice DeviceExtension: 0x%p\n", FN, pDeviceExtension);

    /* Failure for attachment is an indication of a broken plug & play system. */
    if (pDeviceExtension->NextLowerDriver == NULL) {
        IoDeleteDevice(pDeviceObject);
        return STATUS_UNSUCCESSFUL;
    }

    pDeviceObject->Flags |= pDeviceExtension->NextLowerDriver->Flags & (DO_BUFFERED_IO | DO_DIRECT_IO | DO_POWER_PAGABLE);
    pDeviceObject->DeviceType = pDeviceExtension->NextLowerDriver->DeviceType;
    pDeviceObject->Characteristics = pDeviceExtension->NextLowerDriver->Characteristics;

    /*
     * Let us use remove lock to keep count of IRPs so that we don't detach and delete our deviceobject until all pending I/Os
     * in our devstack are completed. Remlock is required to protect us from various race conditions where our driver can get
     * unloaded while we are still running dispatch or completion code.
     */
    IoInitializeRemoveLock(&pDeviceExtension->RemoveLock, POOL_TAG, 0, 0);

    pDeviceExtension->PhysicalDeviceObject = PhysicalDeviceObject;
    pDeviceExtension->Self = pDeviceObject;
    pDeviceExtension->RemoveLockCount = 0;
    pDeviceExtension->GlobalStreamOpen = 0;
    pDeviceExtension->PinNameOpen = 0;
    pDeviceExtension->InstanceCount = (ULONG)0;
    pDeviceExtension->DataUsed = 0;

    InterlockedAnd(&pDeviceExtension->IsVideoInfoHeader2, 0);
    InterlockedAnd(&pDeviceExtension->KsStateRun, 0);
    InterlockedAnd(&pDeviceExtension->StreamerPid, 0);

    KeInitializeEvent(&pDeviceExtension->ControlLock, SynchronizationEvent, TRUE);

    /* Get our PDO friendly name and check whether we support it at this time or not. */
    ntStatus = IoGetDeviceProperty(PhysicalDeviceObject,
                                   DevicePropertyFriendlyName,
                                   512 * sizeof(WCHAR),
                                   szTmpInfo,
                                   (PULONG)&cbTmpInfoSize);

    if (NT_SUCCESS(ntStatus)) {
        if (_wcsicmp(szTmpInfo, L"Integrated Camera") == 0) {
            bPdoSupported = TRUE;
        }
    } else {
        ntStatus = IoGetDeviceProperty(PhysicalDeviceObject,
                                       DevicePropertyDeviceDescription,
                                       512 * sizeof(WCHAR),
                                       szTmpInfo,
                                       (PULONG)&cbTmpInfoSize);

        if (NT_SUCCESS(ntStatus)) {
            if (_wcsicmp(szTmpInfo, L"Intel(R) Imaging Signal Processor 2400") == 0) {
                bPdoSupported = TRUE;
            }
        }
    }

    if (bPdoSupported) {
        L("[%s] 0x%p PDO supported.\n", FN, PhysicalDeviceObject);
        InterlockedIncrement(&pDeviceExtension->IsPdoSupported);

        cbSize = (1920 * 1080 * 2) + sizeof(SHAREDBUFFER_HEADER);
        pDeviceExtension->StreamBuffer = (LPBYTE)ExAllocatePoolWithTag(NonPagedPool, cbSize, 'UVCF');
        L("[%s] Intermediate streambuffer allocated (%d).\n", FN, cbSize);

        /* Initialize system thread control events. */
        KeInitializeEvent(&pDeviceExtension->EventCtrl, NotificationEvent, FALSE);
        KeInitializeEvent(&pDeviceExtension->EventTerm, NotificationEvent, FALSE);

        /* Start our 'restreamer' system thread. */
        ntStatus = PsCreateSystemThread(&hThreadTemp,
                                        THREAD_ALL_ACCESS,
                                        NULL,
                                        NULL,
                                        NULL,
                                        (PKSTART_ROUTINE)StreamMonitorSystemThread,
                                        pDeviceExtension);

        if (!NT_SUCCESS(ntStatus)) {
            L("[%s] PsCreateSystemThread error 0x%x\n", FN, ntStatus);
        } else {
            ObReferenceObjectByHandle(hThreadTemp,
                                      THREAD_ALL_ACCESS,
                                      NULL,
                                      KernelMode,
                                      (PVOID*)&pDeviceExtension->ThreadRestream,
                                      NULL);
            
            ZwClose(hThreadTemp);
            L("[%s] System thread 0x%p created.\n", FN, pDeviceExtension->ThreadRestream);
        }
    } else {
        L("[%s] 0x%p PDO not supported at this time.\n", FN, PhysicalDeviceObject);
    }
    
    /* Set the initial state of the Filter DO */
    INITIALIZE_PNP_STATE(pDeviceExtension);

    L("[%s] 0x%p to 0x%p->0x%p\n", FN, pDeviceObject, pDeviceExtension->NextLowerDriver, PhysicalDeviceObject);
    pDeviceObject->Flags &= ~DO_DEVICE_INITIALIZING;
    return STATUS_SUCCESS;
}

/* {8348FB22-127C-4dc6-9295-65AE0A4109BB} */
DEFINE_GUID(METHODSETID_SETIMAGE, 0x8348fb22, 0x127c, 0x4dc6, 0x92, 0x95, 0x65, 0xae, 0xa, 0x41, 0x9, 0xbb);

/*
DEFINE_KSMETHOD_SET_TABLE(SETIMAGE_MethodSet)
{
    DEFINE_KSMETHOD_SET
	(
		&METHODSETID_SETIMAGE,
		1,
		SETIMAGE_MethodItem,
		0,
		0
    )
};
*/

/*
NTSTATUS KsSetImage(IN PIRP pIrp, IN PKSMETHOD pKSMethod, IN PVOID pData)
{
    NTSTATUS            ntStatus = STATUS_SUCCESS;
    ULONG               ulInputBufferSize = 0;
    ULONG               ulOutputBufferSize = 0;
    PIO_STACK_LOCATION  pIrpStack  = NULL;

    PAGED_CODE();

    ASSERT(pIrp);
    ASSERT(pKSMethod);

    pIrpStack = IoGetCurrentIrpStackLocation(pIrp);
    
	if (pIrpStack)
	{
        ulOutputBufferSize = pIrpStack->Parameters.DeviceIoControl.OutputBufferLength;
        ulInputBufferSize = pIrpStack->Parameters.DeviceIoControl.InputBufferLength;
        // DbgPrint("Input/Output size %d %d\n",ulInputBufferSize,ulOutputBufferSize);
		// __dump("[%s] Input/Output size %d %d\n", Fn, ulInputBufferSize, ulOutputBufferSize);
    }
	
    if (ulOutputBufferSize < KsDataFormatVideoInfoHeader.DataFormat.SampleSize)
    {
        DbgPrint("Output size %d too small\n",ulOutputBufferSize);
        return STATUS_UNSUCCESSFUL;
    }

    if (ulInputBufferSize < sizeof(ULONG))
    {
        DbgPrint("Input size %d too small\n",ulInputBufferSize);
        pIrp->IoStatus.Information = sizeof(ULONG);
        return STATUS_BUFFER_OVERFLOW;
    }
	
    RtlCopyMemory(GlobalBuffer, pData, KsDataFormatVideoInfoHeader.DataFormat.SampleSize);
    pIrp->IoStatus.Information = ulOutputBufferSize;

    return ntStatus;
}
*/

/*
NTSTATUS KsGetImage(IN PIRP pIrp, IN PKSMETHOD pKSMethod, IN PVOID pData)
{
    NTSTATUS            ntStatus = STATUS_SUCCESS;
    ULONG               ulInputBufferSize = 0;
    ULONG               ulOutputBufferSize = 0;
    PIO_STACK_LOCATION  pIrpStack  = NULL;

    PAGED_CODE();

    ASSERT(pIrp);
    ASSERT(pKSMethod);

    pIrpStack = IoGetCurrentIrpStackLocation(pIrp);
    
	if (pIrpStack)
	{
        ulOutputBufferSize = pIrpStack->Parameters.DeviceIoControl.OutputBufferLength;
        ulInputBufferSize = pIrpStack->Parameters.DeviceIoControl.InputBufferLength;
        // DbgPrint("Input/Output size %d %d\n",ulInputBufferSize,ulOutputBufferSize);
		// __dump("[%s] Input/Output size %d %d\n", Fn, ulInputBufferSize, ulOutputBufferSize);
    }
	
    if (ulOutputBufferSize < KsDataFormatVideoInfoHeader.DataFormat.SampleSize)
    {
        DbgPrint("Output size %d too small\n",ulOutputBufferSize);
        return STATUS_UNSUCCESSFUL;
    }

    if (ulInputBufferSize < sizeof(ULONG))
    {
        DbgPrint("Input size %d too small\n",ulInputBufferSize);
        pIrp->IoStatus.Information = sizeof(ULONG);
        return STATUS_BUFFER_OVERFLOW;
    }
	
    RtlCopyMemory(pData, GlobalBuffer, KsDataFormatVideoInfoHeader.DataFormat.SampleSize);
    pIrp->IoStatus.Information = ulOutputBufferSize;

    return ntStatus;
}
*/

/* Kernel stream monitor system thread. Updates registry if stream is active. */
VOID StreamMonitorSystemThread(PDEVICE_EXTENSION DeviceExtension)
{
    NTSTATUS ntStatus = STATUS_SUCCESS;
    PVOID pEventArray[EVENT_COUNT];
    BOOL bContinue = TRUE;
    ULONG ulValue = (ULONG)0;

    L("[%s] --> Start -->\n", FN);

    pEventArray[EVENT_CTRL] = &DeviceExtension->EventCtrl;
    pEventArray[EVENT_EXIT] = &DeviceExtension->EventTerm;

    while (bContinue == TRUE) {
        ntStatus = KeWaitForMultipleObjects(EVENT_COUNT,
                                            pEventArray,
                                            WaitAny,
                                            Executive,
                                            KernelMode,
                                            FALSE,
                                            NULL,
                                            NULL);
        switch (ntStatus) {
            case STATUS_WAIT_0:
                ulValue = (ULONG)InterlockedCompareExchange(&DeviceExtension->KsStateRun, MAXLONG, MAXLONG);

                L("[%s] KsStateRun = %d\n", FN, ulValue);

                ntStatus = RtlWriteRegistryValue(RTL_REGISTRY_ABSOLUTE,
                                                 KEY_KSSTATE_RUN,
                                                 L"State",
                                                 REG_DWORD,
                                                 &ulValue,
                                                 sizeof(ULONG));
                
                KeClearEvent(&DeviceExtension->EventCtrl);
                break;
            
            case STATUS_WAIT_1:
                L("[%s] Requested to terminate!\n", FN);
                bContinue = FALSE;
                break;
            
            default:
                L("[%s] KeWaitForMultipleObjects error 0x%x\n", FN, ntStatus);
                break;
        }
    }
    
    L("[%s] <-- End <--\n", FN);
    
    PsTerminateSystemThread(STATUS_SUCCESS);
}

DWORD m = 0;

/* Our completion routine for video streaming. */
NTSTATUS ModifyStreamRead(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp, PVOID Context)
{
    PDEVICE_EXTENSION pDeviceExtension;
    PIO_STACK_LOCATION pIrpStack;
    PKSSTREAM_HEADER pStreamHdr = NULL;
    PKS_FRAME_INFO pFrameInfo = NULL;
    PUCHAR pMdlBufferOut;
    LPBYTE lpStreamBuffer;
    ULONG ulWidth;
    ULONG ulHeight;
    ULONG ulBufferSize;
    PGUID pSubtypeGuid = NULL;
    BOOL bCriteria1 = FALSE;
    BOOL bCriteria2 = FALSE;
    BOOL bCriteria3 = FALSE;
    // ULONG itr = (ULONG)0;
    ULONG line = (ULONG)1;

    UNREFERENCED_PARAMETER(DeviceObject);
    
    if (Irp->PendingReturned) {
        IoMarkIrpPending(Irp);
    }

    m += 1;
    
    pDeviceExtension = (PDEVICE_EXTENSION)Context;
    pIrpStack = IoGetCurrentIrpStackLocation(Irp);
    
    if (InterlockedCompareExchange(&pDeviceExtension->IsVideoInfoHeader2, MAXLONG, MAXLONG) > 0) {
        ulWidth = (ULONG)pDeviceExtension->VideoInfoHeader2.VideoInfoHeader2.bmiHeader.biWidth;
        ulHeight = (ULONG)pDeviceExtension->VideoInfoHeader2.VideoInfoHeader2.bmiHeader.biHeight;
        ulBufferSize = (ULONG)pDeviceExtension->VideoInfoHeader2.DataFormat.SampleSize;
        pSubtypeGuid = &pDeviceExtension->VideoInfoHeader2.DataFormat.SubFormat;
    } else {
        ulWidth = (ULONG)pDeviceExtension->VideoInfoHeader.VideoInfoHeader.bmiHeader.biWidth;
        ulHeight = (ULONG)pDeviceExtension->VideoInfoHeader.VideoInfoHeader.bmiHeader.biHeight;
        ulBufferSize = (ULONG)pDeviceExtension->VideoInfoHeader.DataFormat.SampleSize;
        pSubtypeGuid = &pDeviceExtension->VideoInfoHeader.DataFormat.SubFormat;
    }

    lpStreamBuffer = (LPBYTE)pDeviceExtension->StreamBuffer;
    
    bCriteria1 = Irp->MdlAddress && IsEqualGUID(pSubtypeGuid, &GUID_FormatSubtype_YUY2);
    bCriteria2 = Irp->MdlAddress && IsEqualGUID(pSubtypeGuid, &GUID_FormatSubtype_NV12);
    bCriteria3 = Irp->MdlAddress && IsEqualGUID(pSubtypeGuid, &GUID_FormatSubtype_MJPG);

    if (bCriteria1 || bCriteria2 || bCriteria3) {
        pStreamHdr = (PKSSTREAM_HEADER)Irp->AssociatedIrp.SystemBuffer;
        pFrameInfo = (PKS_FRAME_INFO)((PBYTE)pStreamHdr + sizeof(KSSTREAM_HEADER));
        
        pMdlBufferOut = (PUCHAR)MmGetSystemAddressForMdlSafe(Irp->MdlAddress, NormalPagePriority);
        
        if (pStreamHdr && pFrameInfo && pMdlBufferOut) {
            if (m >= 30) {
                L("[%s] ___________________________\n", FN);
                L("[%s] KeGetCurrentIrql --> %d\n", FN, KeGetCurrentIrql());
                L("[%s] sizeof(KSSTREAM_HEADER): %d\n", FN, sizeof(KSSTREAM_HEADER));
                L("[%s] sizeof(KS_FRAME_INFO): %d\n", FN, sizeof(KS_FRAME_INFO));
                L("[%s] RequestorMode: %d\n", FN, Irp->RequestorMode);
                L("[%s] ulWidth: %d\n", FN, ulWidth);
                L("[%s] ulHeight: %d\n", FN, ulHeight);
                L("[%s] ulBufferSize: %d\n", FN, ulBufferSize);
                L("[%s] pMdlBufferOut: 0x%p\n", FN, pMdlBufferOut);
                L("[%s] pStreamHdr: 0x%p\n", FN, pStreamHdr);
                L("[%s]    Size: %d\n", FN, pStreamHdr->Size);
                L("[%s]    FrameExtent: %d\n", FN, pStreamHdr->FrameExtent);
                L("[%s]    DataUsed: %d\n", FN, pStreamHdr->DataUsed);
                L("[%s]    Data: 0x%p\n", FN, pStreamHdr->Data);
                L("[%s] pFrameInfo: 0x%p\n", FN, pFrameInfo);
                L("[%s]    Size: %d\n", FN, pFrameInfo->ExtendedHeaderSize);
                L("[%s]    Pitch: %d\n", FN, pFrameInfo->lSurfacePitch);
                L("[%s]    PicNum: %lld\n", FN, pFrameInfo->PictureNumber);
                L("[%s]    Drop: %lld\n", FN, pFrameInfo->DropCount);
                L("[%s] IoStatus.Information: %d\n", FN, Irp->IoStatus.Information);
                L("[%s] IoStatus.Status: 0x%x\n", FN, Irp->IoStatus.Status);
            }

#if TEST_MJPG_REPLACE == 1
            if (bCriteria3) {
                if (!pDeviceExtension->DataUsed) {
                    pDeviceExtension->DataUsed = pStreamHdr->DataUsed;
                    RtlCopyMemory(lpStreamBuffer, pMdlBufferOut, pStreamHdr->DataUsed);
                    L("[%s] Local buffer replaced %d\n", FN, pDeviceExtension->DataUsed);
                } else {
                    /*
                    if (pStreamHdr->DataUsed <= pDeviceExtension->DataUsed && Irp->IoStatus.Status == STATUS_SUCCESS) {
                        RtlCopyMemory(pMdlBufferOut, lpStreamBuffer, pDeviceExtension->DataUsed);
                        pStreamHdr->DataUsed = pDeviceExtension->DataUsed;
                    }
                    */

                    if (Irp->IoStatus.Status == STATUS_SUCCESS) {
                        RtlCopyMemory(pMdlBufferOut, lpStreamBuffer, pDeviceExtension->DataUsed);
                        pStreamHdr->DataUsed = pDeviceExtension->DataUsed;
                    }
                }
            }
#else
            try {
                if (Irp->RequestorMode == UserMode) {
                    ProbeForRead(pStreamHdr->Data, pStreamHdr->DataUsed, sizeof(BYTE));
                }
                
                if (Irp->IoStatus.Status == STATUS_SUCCESS) {
                    RtlCopyMemory(lpStreamBuffer, pMdlBufferOut, pStreamHdr->DataUsed);
                    pDeviceExtension->DataUsed = pStreamHdr->DataUsed;
                    
                    line = (ULONG)1;

					/*
                    for (itr = (ULONG)0; itr < pStreamHdr->DataUsed; itr++) {
                        line++;
                        
                        KdPrintEx((DPFLTR_IHVDRIVER_ID, DPFLTR_ERROR_LEVEL, "%02x ", pMdlBufferOut[itr]));
                        
                        if ((line % 32) == 0) {
                            KdPrintEx((DPFLTR_IHVDRIVER_ID, DPFLTR_ERROR_LEVEL, "\n"));
                            line = (ULONG)1;
                        }
                    }
                    
                    KdPrintEx((DPFLTR_IHVDRIVER_ID, DPFLTR_ERROR_LEVEL, "\n"));
                    */
                }

                if (pStreamHdr->DataUsed != ulBufferSize) {
                    if (InterlockedCompareExchange(&pDeviceExtension->IsVideoInfoHeader2, MAXLONG, MAXLONG) > 0) {
                        pDeviceExtension->VideoInfoHeader2.DataFormat.SampleSize = pStreamHdr->DataUsed;
                    } else {
                        pDeviceExtension->VideoInfoHeader.DataFormat.SampleSize = pStreamHdr->DataUsed;
                    }
                }
            } except (EXCEPTION_EXECUTE_HANDLER) {
                L("[%s] Exception in 0x%p read (0x%x)\n", FN, pMdlBufferOut, GetExceptionCode());
            }
#endif
        }
    }

    if (m >= 30) m = 0;

    IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
    InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
    
    return STATUS_CONTINUE_COMPLETION; 
}

/* Generic wait completion routine. */
NTSTATUS WaitComplete(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp, IN PVOID Context)
{
    UNREFERENCED_PARAMETER(DeviceObject);
    
    if (Irp->PendingReturned == TRUE) {
        KeSetEvent((PKEVENT)Context, IO_NO_INCREMENT, FALSE);
    }
    
    return STATUS_MORE_PROCESSING_REQUIRED;
} 

NTSTATUS CustomKsPropertyHandler(IN PIRP Irp, IN PKSIDENTIFIER Request, IN OUT PVOID Data)
{
    PIO_STACK_LOCATION pIrpStackLocation;
    PDEVICE_EXTENSION pFilterExtension;
    
    pIrpStackLocation = IoGetCurrentIrpStackLocation(Irp);
    pFilterExtension = (PDEVICE_EXTENSION)pIrpStackLocation->DeviceObject->DeviceExtension;
    
    /*
    if (TRUE == IsEqualGUID(&Request->Set, &GUID_PROPSETID_VidcapCameraControl)) {
        if (pRequest->Id==KSPROPERTY_CAMERACONTROL_PRIVACY) {
            KSPROPERTY_CAMERACONTROL_S *CamControl=Irp->UserBuffer;
            
            if (pRequest->Flags==KSPROPERTY_TYPE_SET) {
                DebugPrint(("KSPROPERTY_CAMERACONTROL_PRIVACY Set Value= %x\n",CamControl->Value));
                // RtlCopyMemory(CamControl,pRequest,sizeof(KSPROPERTY));
                PrivacyModeFlag=CamControl->Value;
                Irp->IoStatus.Status=STATUS_SUCCESS;
            }
			
            if (pRequest->Flags==KSPROPERTY_TYPE_GET) {
                DebugPrint(("KSPROPERTY_CAMERACONTROL_PRIVACY Get Value= %x\n",CamControl->Value));
                //RtlCopyMemory(CamControl,pRequest,sizeof(KSPROPERTY));
                CamControl->Value=PrivacyModeFlag;
                DebugPrint(("KSPROPERTY_CAMERACONTROL_PRIVACY Get Value changed= %x\n",CamControl->Value));
                Irp->IoStatus.Status=STATUS_SUCCESS;
            }

            if (pRequest->Flags==KSPROPERTY_TYPE_SETSUPPORT) {
                DebugPrint(("KSPROPERTY_TYPE_SETSUPPORT\n"));
            }
            
            if (pRequest->Flags==KSPROPERTY_TYPE_BASICSUPPORT) {
                DebugPrint(("KSPROPERTY_TYPE_BASICSUPPORT\n"));
            }
        }
    }
    */

    if (IsEqualGUID(&Request->Set, &GUID_PROPSETID_Connection) == TRUE) {
        if (Request->Id == KSPROPERTY_CONNECTION_STATE) {
            PULONG pData = (PULONG)Data;
            KSSTATE ksState = (KSSTATE)*pData;
            
            if (Request->Flags == KSPROPERTY_TYPE_SET) {
                switch (ksState) {
                    case KSSTATE_STOP:
                        InterlockedAnd(&pFilterExtension->KsStateRun, 0);
                        KeSetEvent(&pFilterExtension->EventCtrl, 0, FALSE);
                        L("[%s] SET: KSSTATE_STOP\n", FN);
                        break;
                    
                    case KSSTATE_ACQUIRE:
                        InterlockedAnd(&pFilterExtension->KsStateRun, 0);
                        KeSetEvent(&pFilterExtension->EventCtrl, 0, FALSE);
                        L("[%s] SET: KSSTATE_ACQUIRE\n", FN);
                        break;
                    
                    case KSSTATE_PAUSE:
                        InterlockedAnd(&pFilterExtension->KsStateRun, 0);
                        KeSetEvent(&pFilterExtension->EventCtrl, 0, FALSE);
                        L("[%s] SET: KSSTATE_PAUSE\n", FN);
                        break;
                    
                    case KSSTATE_RUN:
                        InterlockedIncrement(&pFilterExtension->KsStateRun);
                        KeSetEvent(&pFilterExtension->EventCtrl, 0, FALSE);
                        L("[%s] SET: KSSTATE_RUN\n", FN);
                        break;
                    
                    default:
                        break;
                }

                Irp->IoStatus.Status = STATUS_SUCCESS;
            }
        }
    }
    
    return STATUS_SUCCESS;
}

NTSTATUS CustomKsMethodHandler(IN PIRP Irp, IN PKSIDENTIFIER Request, IN OUT PVOID Data)
{
    UNREFERENCED_PARAMETER(Irp);
    UNREFERENCED_PARAMETER(Request);
    UNREFERENCED_PARAMETER(Data);
    
    /*
    L("[%s] Called!\n", FN);
    
    if (TRUE == IsEqualGUID( &pRequest->Set,  &GUID_PROPSETID_VidcapCameraControl)) {
        if (pRequest->Id==KSPROPERTY_CAMERACONTROL_PRIVACY) {
            KSPROPERTY_CAMERACONTROL_S *CamControl=Irp->UserBuffer;
            
            if (pRequest->Flags==KSPROPERTY_TYPE_SET) {
                DebugPrint(("KSPROPERTY_CAMERACONTROL_PRIVACY Set Value= %x\n",CamControl->Value));
                //RtlCopyMemory(CamControl,pRequest,sizeof(KSPROPERTY));
                PrivacyModeFlag=CamControl->Value;
                Irp->IoStatus.Status=STATUS_SUCCESS;
            }
            
            if (pRequest->Flags==KSPROPERTY_TYPE_GET) {
                DebugPrint(("KSPROPERTY_CAMERACONTROL_PRIVACY Get Value= %x\n",CamControl->Value));
                //RtlCopyMemory(CamControl,pRequest,sizeof(KSPROPERTY));
                CamControl->Value=PrivacyModeFlag;
                DebugPrint(("KSPROPERTY_CAMERACONTROL_PRIVACY Get Value changed= %x\n",CamControl->Value));
                Irp->IoStatus.Status=STATUS_SUCCESS;
            }

            if (pRequest->Flags==KSPROPERTY_TYPE_SETSUPPORT) {
                DebugPrint(("KSPROPERTY_TYPE_SETSUPPORT\n"));
            }
            
            if (pRequest->Flags==KSPROPERTY_TYPE_BASICSUPPORT) {
                DebugPrint(("KSPROPERTY_TYPE_BASICSUPPORT\n"));
            }
        }
    }
    */

    return STATUS_SUCCESS;
}

/*
 * Routine Description:
 *
 * The default dispatch routine. If this driver does not recognize the IRP, then it should send it down, unmodified. If the device
 * holds iris, this IRP must be queued in the device extension, no completion routine is required.
 *
 * For demonstrative purposes only, we will pass all the (non-PnP) Irps down on the stack (as we are a filter driver). A real
 * driver might choose to service some of these Irps.
 *
 * As we have NO idea which function we are happily passing on, we can make no assumptions about whether or not it will be called
 * at raised IRQL. For this reason, this function must be in put into non-paged pool (aka the default location).
 */
NTSTATUS FilterPass(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
    PDEVICE_EXTENSION pDeviceExtension = NULL;
    PIO_STACK_LOCATION pIrpStack = NULL;
    NTSTATUS ntStatus = STATUS_SUCCESS;
    PKSPIN_CONNECT pConnectDetails = NULL;
    PKSDATAFORMAT pDataFormat = NULL;
    PKS_DATAFORMAT_VIDEOINFOHEADER pVideoInfoHeader;
    PKS_DATAFORMAT_VIDEOINFOHEADER2 pVideoInfoHeader2;
    LPWSTR pszPinName = GUIDSTR_KSPIN_NAME;
    ULONG ulStreamStop = (ULONG)0;
    LPWSTR pszOffset = NULL;
    KEVENT knEvent;
    
    KeInitializeEvent(&knEvent, NotificationEvent, FALSE);
    pDeviceExtension = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
    ntStatus = IoAcquireRemoveLock(&pDeviceExtension->RemoveLock, Irp);

    if (!NT_SUCCESS(ntStatus)) {
        Irp->IoStatus.Status = ntStatus;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return ntStatus;
    }

    InterlockedIncrement(&pDeviceExtension->RemoveLockCount);

    /* First and foremost, continue only if we support this PDO. Otherwise, just pass through. */
    if (InterlockedCompareExchange(&pDeviceExtension->IsPdoSupported, MAXLONG, MAXLONG) < 1) {
        IoSkipCurrentIrpStackLocation(Irp);
        ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
        
        IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
        InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
        
        return ntStatus;
    }
    
    pIrpStack = IoGetCurrentIrpStackLocation(Irp);

    /*************************************************************************************************************/

    if (pIrpStack->MajorFunction == IRP_MJ_CREATE) {
#pragma warning(disable:4311)
        LONG lCallerPid = (LONG)PsGetCurrentProcessId();
#pragma warning(default:4311)
        DWORD dwSubType = 0;
        LONG lActiveX = 0;
        LONG lActiveY = 0;
        
        L("[%s] IRP_MJ_CREATE: ProcessId: %d\n", FN, lCallerPid);
        
        if (pIrpStack->FileObject->FileName.Length) {
            L("[%s] IRP_MJ_CREATE: FileLen: %d\n", FN, pIrpStack->FileObject->FileName.Length);
            
            // if (pIrpStack->FileObject->FileName.Length > 100) KdBreakPoint();

            /* Check for '\Global...' parameter for our flag. Sequence: 5c 67 6c 6f 62 61 6c. */

            /* The '{146F1A...' portion of the PIN name. Sequence: 7b 31 34 36 46 31 41. */
            
            if (InterlockedCompareExchange(&pDeviceExtension->StreamerPid, MAXLONG, MAXLONG) == lCallerPid) {
                L("[%s] IRP_MJ_CREATE: This is the internal streamer.\n", FN);
            } else {
                LARGE_INTEGER liTimeout;
                
                L("[%s] IRP_MJ_CREATE: This is another client. Stop internal streamer.\n", FN);
                
                (void)RtlWriteRegistryValue(RTL_REGISTRY_ABSOLUTE,
                                            REG_INTRNL_STRM,
                                            L"State",
                                            REG_DWORD,
                                            &ulStreamStop,
                                            sizeof(ULONG));
                
                /* Pause a bit (1s). Not a good idea generally, but.... */
                liTimeout.QuadPart = -1 * 1000000;
                KeWaitForSingleObject(&knEvent, Executive, KernelMode, FALSE, &liTimeout);
            }
        }
        
        /* Pass IRP to next lower driver. */
        IoCopyCurrentIrpStackLocationToNext(Irp);
        IoSetCompletionRoutine(Irp, WaitComplete, &knEvent, TRUE, TRUE, TRUE);
        ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
        
        /* Wait for IRP return then regain control. */
        if (ntStatus == STATUS_PENDING) {
            KeWaitForSingleObject(&knEvent, Executive, KernelMode, FALSE, NULL);
        }
        
        if (!pIrpStack->FileObject->FileName.Buffer) {
            L("[%s] IRP_MJ_CREATE: Error STATUS_INVALID_PARAMETER (NullBuff).\n", FN);
            
            IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
            InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
            return STATUS_INVALID_PARAMETER;
        }

        /* Only check KS_DATAFORMAT_VIDEOINFOHEADER size since KS_DATAFORMAT_VIDEOINFOHEADER2 is bigger than v1. */
        if (pIrpStack->FileObject->FileName.Length < wcslen(pszPinName) * sizeof(WCHAR) +
                sizeof(KSPIN_CONNECT) + sizeof(KS_DATAFORMAT_VIDEOINFOHEADER)) {
            L("[%s] IRP_MJ_CREATE: Error STATUS_INVALID_PARAMETER (NameLen).\n", FN);
            
            IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
            InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
			return STATUS_INVALID_PARAMETER;
        }
        
        pszOffset = wcsstr(pIrpStack->FileObject->FileName.Buffer, pszPinName);
        
        if (!pszOffset) {
            /* Request is not targeted to a pin. */
            L("[%s] IRP_MJ_CREATE: Error STATUS_INVALID_PARAMETER (!Offset).\n", FN);
            
            IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
            InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
            return STATUS_INVALID_PARAMETER;
        }
        
        pConnectDetails = (PKSPIN_CONNECT)(pszOffset + wcslen(pszPinName));
        pDataFormat = (PKSDATAFORMAT)((PBYTE)pConnectDetails + sizeof(KSPIN_CONNECT));
        
        if (IsEqualGUID(&pDataFormat->Specifier, &GUID_Specifier_VideoInfo) == TRUE) {
            size_t cbTmpSize = (wcslen(pszPinName) * sizeof(WCHAR))
                + sizeof(KSPIN_CONNECT)
                + sizeof(KS_DATAFORMAT_VIDEOINFOHEADER);
            
            L("[%s] IRP_MJ_CREATE: Use KS_DATAFORMAT_VIDEOINFOHEADER (Size = %d).\n", FN, cbTmpSize);
            
            pVideoInfoHeader = (PKS_DATAFORMAT_VIDEOINFOHEADER)((PBYTE)pConnectDetails + sizeof(KSPIN_CONNECT));
            
            if (pVideoInfoHeader) {
                CHAR c1, c2, c3, c4;
                DWORD dwComp = pVideoInfoHeader->VideoInfoHeader.bmiHeader.biCompression;
                FOURCC_DMP(dwComp, &c1, &c2, &c3, &c4);
                
                PrintGuidValues(FN, L"IRP_MJ_CREATE: MajorFormat", &pVideoInfoHeader->DataFormat.MajorFormat);
                PrintGuidValues(FN, L"IRP_MJ_CREATE: SubFormat", &pVideoInfoHeader->DataFormat.SubFormat);
                PrintGuidValues(FN, L"IRP_MJ_CREATE: Specifier", &pVideoInfoHeader->DataFormat.Specifier);
                L("[%s] IRP_MJ_CREATE: SampleSize: %d\n", FN, pVideoInfoHeader->DataFormat.SampleSize);
                L("[%s] IRP_MJ_CREATE: BitCount: %d\n", FN, pVideoInfoHeader->VideoInfoHeader.bmiHeader.biBitCount);
                L("[%s] IRP_MJ_CREATE: Compr: 0x%x [%c%c%c%c]\n", FN, dwComp, c4, c3, c2, c1);
                L("[%s] IRP_MJ_CREATE: Width: %d\n", FN, pVideoInfoHeader->VideoInfoHeader.bmiHeader.biWidth);
                L("[%s] IRP_MJ_CREATE: Height: %d\n", FN, pVideoInfoHeader->VideoInfoHeader.bmiHeader.biHeight);
                L("[%s] IRP_MJ_CREATE: Size: %d\n", FN, pVideoInfoHeader->VideoInfoHeader.bmiHeader.biSize);
                L("[%s] IRP_MJ_CREATE: SizeImg: %d\n", FN, pVideoInfoHeader->VideoInfoHeader.bmiHeader.biSizeImage);
                
                dwSubType = dwComp;
                lActiveX = pVideoInfoHeader->VideoInfoHeader.bmiHeader.biWidth;
                lActiveY = pVideoInfoHeader->VideoInfoHeader.bmiHeader.biHeight;
            }
            
            InterlockedAnd(&pDeviceExtension->IsVideoInfoHeader2, 0);
            RtlCopyMemory(&pDeviceExtension->VideoInfoHeader, pVideoInfoHeader, sizeof(KS_DATAFORMAT_VIDEOINFOHEADER));
        } else if (IsEqualGUID(&pDataFormat->Specifier, &GUID_Specifier_VideoInfo2) == TRUE) {
            size_t cbTmpSize = (wcslen(pszPinName) * sizeof(WCHAR))
                + sizeof(KSPIN_CONNECT)
                + sizeof(KS_DATAFORMAT_VIDEOINFOHEADER2);
            
            L("[%s] IRP_MJ_CREATE: Use KS_DATAFORMAT_VIDEOINFOHEADER2 (Size = %d).\n", FN, cbTmpSize);
            
            pVideoInfoHeader2 = (PKS_DATAFORMAT_VIDEOINFOHEADER2)((PBYTE)pConnectDetails + sizeof(KSPIN_CONNECT));
            
            if (pVideoInfoHeader2) {
                CHAR c1, c2, c3, c4;
                DWORD dwComp = pVideoInfoHeader2->VideoInfoHeader2.bmiHeader.biCompression;
                FOURCC_DMP(dwComp, &c1, &c2, &c3, &c4);
                
                PrintGuidValues(FN, L"IRP_MJ_CREATE: MajorFormat", &pVideoInfoHeader2->DataFormat.MajorFormat);
                PrintGuidValues(FN, L"IRP_MJ_CREATE: SubFormat", &pVideoInfoHeader2->DataFormat.SubFormat);
                PrintGuidValues(FN, L"IRP_MJ_CREATE: Specifier", &pVideoInfoHeader2->DataFormat.Specifier);
                L("[%s] IRP_MJ_CREATE: FormatFlags: 0x%x\n", FN, pVideoInfoHeader2->DataFormat.Flags);
                L("[%s] IRP_MJ_CREATE: SampleSize: %d\n", FN, pVideoInfoHeader2->DataFormat.SampleSize);
                L("[%s] IRP_MJ_CREATE: BitCount: %d\n", FN, pVideoInfoHeader2->VideoInfoHeader2.bmiHeader.biBitCount);
                L("[%s] IRP_MJ_CREATE: Compr: 0x%x [%c%c%c%c]\n", FN, dwComp, c4, c3, c2, c1);
                L("[%s] IRP_MJ_CREATE: Width: %d\n", FN, pVideoInfoHeader2->VideoInfoHeader2.bmiHeader.biWidth);
                L("[%s] IRP_MJ_CREATE: Height: %d\n", FN, pVideoInfoHeader2->VideoInfoHeader2.bmiHeader.biHeight);
                L("[%s] IRP_MJ_CREATE: Size: %d\n", FN, pVideoInfoHeader2->VideoInfoHeader2.bmiHeader.biSize);
                L("[%s] IRP_MJ_CREATE: SizeImg: %d\n", FN, pVideoInfoHeader2->VideoInfoHeader2.bmiHeader.biSizeImage);
                
                dwSubType = dwComp;
                lActiveX = pVideoInfoHeader2->VideoInfoHeader2.bmiHeader.biWidth;
                lActiveY = pVideoInfoHeader2->VideoInfoHeader2.bmiHeader.biHeight;
            }
            
            InterlockedIncrement(&pDeviceExtension->IsVideoInfoHeader2);
            RtlCopyMemory(&pDeviceExtension->VideoInfoHeader2, pVideoInfoHeader2, sizeof(KS_DATAFORMAT_VIDEOINFOHEADER2));
        } else {
            L("[%s] IRP_MJ_CREATE: I don't know this specifier.\n", FN);
            InterlockedAnd(&pDeviceExtension->IsVideoInfoHeader2, 0);
            RtlZeroMemory(&pDeviceExtension->VideoInfoHeader, sizeof(KS_DATAFORMAT_VIDEOINFOHEADER));
            RtlZeroMemory(&pDeviceExtension->VideoInfoHeader2, sizeof(KS_DATAFORMAT_VIDEOINFOHEADER2));
        }

        (void)RtlWriteRegistryValue(RTL_REGISTRY_ABSOLUTE, REG_FLTR, L"ActiveSubType", REG_DWORD, &dwSubType, sizeof(ULONG));
        (void)RtlWriteRegistryValue(RTL_REGISTRY_ABSOLUTE, REG_FLTR, L"ActiveResX", REG_DWORD, &lActiveX, sizeof(ULONG));
        (void)RtlWriteRegistryValue(RTL_REGISTRY_ABSOLUTE, REG_FLTR, L"ActiveResY", REG_DWORD, &lActiveY, sizeof(ULONG));
        
        ntStatus = Irp->IoStatus.Status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        
        IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
        InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
        
        return ntStatus;
    }

    /*************************************************************************************************************/

    if ((pIrpStack->MajorFunction == IRP_MJ_DEVICE_CONTROL) &&
            (pIrpStack->Parameters.DeviceIoControl.IoControlCode == IOCTL_KS_READ_STREAM)) {
        IoMarkIrpPending(Irp);
        IoCopyCurrentIrpStackLocationToNext(Irp);
        IoSetCompletionRoutine(Irp, ModifyStreamRead, (PVOID)pDeviceExtension, TRUE, TRUE, TRUE);
        ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
        
        return STATUS_PENDING;
    }

    /*************************************************************************************************************/

    /*
    if ((pIrpStack->MajorFunction == IRP_MJ_DEVICE_CONTROL) &&
            (pIrpStack->Parameters.DeviceIoControl.IoControlCode == IOCTL_KS_WRITE_STREAM)) {
        // L("[%s] IRP_MJ_DEVICE_CONTROL | IOCTL_KS_READ_STREAM\n", FN);
        
        L("[%s] B4_MdlAddr: 0x%p\n", FN, Irp->MdlAddress);
        L("[%s] B4_UsrBuff: 0x%p\n", FN, Irp->UserBuffer);
        L("[%s] B4_Method : %d\n", FN, METHOD_FROM_CTL_CODE(IOCTL_KS_WRITE_STREAM));
        
        IoMarkIrpPending(Irp);
        IoCopyCurrentIrpStackLocationToNext(Irp);
        IoSetCompletionRoutine(Irp, ModifyStreamWrite, (PVOID)pDeviceExtension, TRUE, TRUE, TRUE);
        ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
        
        return STATUS_PENDING;
    }
    */

    /*************************************************************************************************************/

	if ((pIrpStack->MajorFunction == IRP_MJ_DEVICE_CONTROL) &&
            (pIrpStack->Parameters.DeviceIoControl.IoControlCode == IOCTL_KS_PROPERTY)) {
        // L("[%s] IRP_MJ_DEVICE_CONTROL | IOCTL_KS_PROPERTY\n", FN);
        
        IoCopyCurrentIrpStackLocationToNext(Irp);
        IoSetCompletionRoutine(Irp, WaitComplete, &knEvent, TRUE, TRUE, TRUE );
        ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
        
        if (ntStatus == STATUS_PENDING) {
            KeWaitForSingleObject(&knEvent, Executive, KernelMode, FALSE, NULL);
        }
        
        KsDispatchSpecificProperty(Irp, (PFNKSHANDLER)CustomKsPropertyHandler);
        ntStatus = Irp->IoStatus.Status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
        
        InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
        
        return ntStatus;
    }

    /*************************************************************************************************************/

    if ((pIrpStack->MajorFunction == IRP_MJ_DEVICE_CONTROL) &&
            (pIrpStack->Parameters.DeviceIoControl.IoControlCode == IOCTL_KS_METHOD)) {
        // L("[%s] IRP_MJ_DEVICE_CONTROL | IOCTL_KS_METHOD\n", FN);

        IoCopyCurrentIrpStackLocationToNext(Irp);
        IoSetCompletionRoutine(Irp, WaitComplete, &knEvent, TRUE, TRUE, TRUE );
        ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
        
        if (ntStatus == STATUS_PENDING) {
            KeWaitForSingleObject(&knEvent, Executive, KernelMode, FALSE, NULL);
        }
        
        KsDispatchSpecificMethod(Irp,(PFNKSHANDLER)CustomKsMethodHandler);
        ntStatus = Irp->IoStatus.Status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
        
        InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
        
        return ntStatus;
    }

    /*************************************************************************************************************/

    if (pIrpStack->MajorFunction == IRP_MJ_CLEANUP) {
        L("[%s] IRP_MJ_CLEANUP: ProcessID: %d\n", FN, PsGetCurrentProcessId());

        IoCopyCurrentIrpStackLocationToNext(Irp);
        IoSetCompletionRoutine(Irp, WaitComplete, &knEvent, TRUE, TRUE, TRUE );
        ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
        
        if (ntStatus == STATUS_PENDING) {
            KeWaitForSingleObject(&knEvent, Executive, KernelMode, FALSE, NULL);
        }

        ntStatus = Irp->IoStatus.Status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
        
        InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
        
        return ntStatus;
    }

    /*************************************************************************************************************/

    if (pIrpStack->MajorFunction == IRP_MJ_CLOSE) {
        L("[%s] IRP_MJ_CLOSE:\n", FN);

        IoCopyCurrentIrpStackLocationToNext(Irp);
        IoSetCompletionRoutine(Irp, WaitComplete, &knEvent, TRUE, TRUE, TRUE );
        ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
		
        if (ntStatus == STATUS_PENDING) {
            KeWaitForSingleObject(&knEvent, Executive, KernelMode, FALSE, NULL);
        }

        ntStatus = Irp->IoStatus.Status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
        
        InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
        
        return ntStatus;
    }

    /*************************************************************************************************************/

    IoSkipCurrentIrpStackLocation(Irp);
    ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
    IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
    
    InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
    
    return ntStatus;
}

/*
 * Routine Description:
 *
 * The plug and play dispatch routines.
 *
 * Most of these the driver will completely ignore. In all cases it must pass on the IRP to the lower driver.
 */
NTSTATUS FilterDispatchPnp(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
    PDEVICE_EXTENSION pDeviceExtension;
    PIO_STACK_LOCATION pIrpStack;
    NTSTATUS ntStatus;
    KEVENT knEvent;
    
    PAGED_CODE();
    
    pDeviceExtension = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
    pIrpStack = IoGetCurrentIrpStackLocation(Irp);
    
    L("[%s] FilterDO %s IRP:0x%p\n", FN, PnPMinorFunctionString(pIrpStack->MinorFunction), Irp);

    ntStatus = IoAcquireRemoveLock(&pDeviceExtension->RemoveLock, Irp);
	
    if (!NT_SUCCESS (ntStatus)) {
        Irp->IoStatus.Status = ntStatus;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return ntStatus;
    }

    InterlockedIncrement(&pDeviceExtension->RemoveLockCount);

    switch (pIrpStack->MinorFunction) {
        case IRP_MN_START_DEVICE:
        {
            /*
             * The device is starting. We cannot touch the device (send it any non pnp irps) until a start device has been passed
             * down to the lower drivers.
             */
            KeInitializeEvent(&knEvent, NotificationEvent, FALSE);
            IoCopyCurrentIrpStackLocationToNext(Irp);
            IoSetCompletionRoutine(Irp, FilterStartCompletionRoutine, &knEvent, TRUE, TRUE, TRUE);
            ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
            
            /*
             * Wait for lower drivers to be done with the Irp. Important thing to note here is when you allocate memory for an event
             * in the stack you must do a KernelMode wait instead of UserMode to prevent the stack from getting paged out.
             */
            if (ntStatus == STATUS_PENDING) {
                KeWaitForSingleObject(&knEvent, Executive, KernelMode, FALSE, NULL);
                ntStatus = Irp->IoStatus.Status;
            }
            
            if (NT_SUCCESS(ntStatus)) {
                /* As we are successfully now back, we will first set our state to Started. */
                SET_NEW_PNP_STATE(pDeviceExtension, Started);
                
                /*
                 * On the way up inherit FILE_REMOVABLE_MEDIA during Start. This characteristic is available only after the driver
                 * stack is started!.
                 */
                if (pDeviceExtension->NextLowerDriver->Characteristics & FILE_REMOVABLE_MEDIA) {
                    DeviceObject->Characteristics |= FILE_REMOVABLE_MEDIA;
                }

                /* If PreviousPnPState is stopped then we are being stopped temporarily and restarted for resource rebalance. */
                if (Stopped != pDeviceExtension->PreviousPnPState) {
                    /* Device is started for the first time. */
                    FilterCreateControlObject(DeviceObject);
                }
            }
            
            Irp->IoStatus.Status = ntStatus;
            IoCompleteRequest (Irp, IO_NO_INCREMENT);
            
            IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
            InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
            
            return ntStatus;
        }

        case IRP_MN_REMOVE_DEVICE:
        {
            if (InterlockedCompareExchange(&pDeviceExtension->IsPdoSupported, MAXLONG, MAXLONG) > 0) {
                /* Kill our system thread first. */
                KeSetEvent(&pDeviceExtension->EventTerm, 0, FALSE);
                KeWaitForSingleObject(pDeviceExtension->ThreadRestream, Executive, KernelMode, FALSE, NULL);
                ObDereferenceObject(pDeviceExtension->ThreadRestream);

                L("[%s] Intermediate streamBuffer released.\n", FN);
                ExFreePoolWithTag((PVOID)pDeviceExtension->StreamBuffer, 'UVCF'); 
            }

            /* Wait for all outstanding requests to complete */
            IoReleaseRemoveLockAndWait(&pDeviceExtension->RemoveLock, Irp);

            IoSkipCurrentIrpStackLocation(Irp);
            ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);

            SET_NEW_PNP_STATE(pDeviceExtension, Deleted);

            FilterDeleteControlObject(DeviceObject);
            IoDetachDevice(pDeviceExtension->NextLowerDriver);
            IoDeleteDevice(DeviceObject);
            
            return ntStatus;
        }

        case IRP_MN_QUERY_STOP_DEVICE:
        {
            SET_NEW_PNP_STATE(pDeviceExtension, StopPending);
            ntStatus = STATUS_SUCCESS;
            break;
        }

        case IRP_MN_CANCEL_STOP_DEVICE:
        {
            /*
             * Check to see whether you have received cancel-stop without first receiving a query-stop. This could happen if
             * someone above us fails a query-stop and passes down the subsequent cancel-stop.
             */
            if (StopPending == pDeviceExtension->DevicePnPState) {
                /* We did receive a query-stop, so restore. */
                RESTORE_PREVIOUS_PNP_STATE(pDeviceExtension);
            }
            
            /* We must not fail this IRP. */
            ntStatus = STATUS_SUCCESS;
            break;
        }

        case IRP_MN_STOP_DEVICE:
        {
            SET_NEW_PNP_STATE(pDeviceExtension, Stopped);
            ntStatus = STATUS_SUCCESS;
            break;
        }

        case IRP_MN_QUERY_REMOVE_DEVICE:
        {
            SET_NEW_PNP_STATE(pDeviceExtension, RemovePending);
            ntStatus = STATUS_SUCCESS;
            break;
        }

        case IRP_MN_SURPRISE_REMOVAL:
        {
            SET_NEW_PNP_STATE(pDeviceExtension, SurpriseRemovePending);
            ntStatus = STATUS_SUCCESS;
            break;
        }
        
        case IRP_MN_CANCEL_REMOVE_DEVICE:
        {
            /*
             * Check to see whether you have received cancel-remove without first receiving a query-remove. This could happen if
             * someone above us fails a query-remove and passes down the subsequent cancel-remove.
             */
            if (RemovePending == pDeviceExtension->DevicePnPState) {
                /* We did receive a query-remove, so restore. */
                RESTORE_PREVIOUS_PNP_STATE(pDeviceExtension);
            }
            
            /* We must not fail this IRP. */
            ntStatus = STATUS_SUCCESS;
            break;
        }
        
        case IRP_MN_DEVICE_USAGE_NOTIFICATION:
        {
            /* On the way down, pagable might become set. Mimic the driver above us. If no one is above us, just set pagable. */
#pragma prefast(suppress:__WARNING_INACCESSIBLE_MEMBER)
            if ((DeviceObject->AttachedDevice == NULL) || (DeviceObject->AttachedDevice->Flags & DO_POWER_PAGABLE)) {
                DeviceObject->Flags |= DO_POWER_PAGABLE;
            }
            
            IoCopyCurrentIrpStackLocationToNext(Irp);
            IoSetCompletionRoutine(Irp, FilterDeviceUsageNotificationCompletionRoutine, NULL, TRUE, TRUE, TRUE);

            return IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
        }

        default:
        {
            /* If you don't handle any IRP you must leave the ntStatus as is. */
            ntStatus = Irp->IoStatus.Status;
            break;
        }
    }

	/* Pass the IRP down and forget it. */
    Irp->IoStatus.Status = ntStatus;
    IoSkipCurrentIrpStackLocation (Irp);
    ntStatus = IoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
    
    IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
    InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
	
    return ntStatus;
}

/*
 * Routine Description:
 *
 * A completion routine for use when calling the lower device objects to which our filter deviceobject is attached.
 */
NTSTATUS FilterStartCompletionRoutine(PDEVICE_OBJECT DeviceObject, PIRP Irp, PVOID Context)
{
    PKEVENT knEvent = (PKEVENT)Context;

    UNREFERENCED_PARAMETER(DeviceObject);

    /*
     * If the lower driver didn't return STATUS_PENDING, we don't need to set the event because we won't be waiting on it. 
     * This optimization avoids grabbing the dispatcher lock, and improves perf.
     */
    if (Irp->PendingReturned == TRUE) {
        KeSetEvent(knEvent, IO_NO_INCREMENT, FALSE);
    }
    
    /* The dispatch routine will have to call IoCompleteRequest */
    return STATUS_MORE_PROCESSING_REQUIRED;
}

/*
 * Routine Description:
 *
 * A completion routine for use when calling the lower device objects to which our filter deviceobject is attached.
 */
NTSTATUS FilterDeviceUsageNotificationCompletionRoutine(PDEVICE_OBJECT DeviceObject, PIRP Irp, PVOID Context)
{
    PDEVICE_EXTENSION pDeviceExtension;
    
    UNREFERENCED_PARAMETER(Context);

    pDeviceExtension = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    if (Irp->PendingReturned) {
        IoMarkIrpPending(Irp);
    }

    /* On the way up, pagable might become clear. Mimic the driver below us. */
    if (!(pDeviceExtension->NextLowerDriver->Flags & DO_POWER_PAGABLE)) {
        DeviceObject->Flags &= ~DO_POWER_PAGABLE;
    }
    
    IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
    InterlockedDecrement(&pDeviceExtension->RemoveLockCount);
    
    return STATUS_CONTINUE_COMPLETION;
}

/*
 * Routine Description:
 *
 * This routine is the dispatch routine for power irps.
 */
NTSTATUS FilterDispatchPower(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
    PDEVICE_EXTENSION pDeviceExtension;
    NTSTATUS ntStatus;
    
    pDeviceExtension = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
    ntStatus = IoAcquireRemoveLock(&pDeviceExtension->RemoveLock, Irp);

    InterlockedIncrement(&pDeviceExtension->RemoveLockCount);
	
    if (!NT_SUCCESS(ntStatus)) {
        /* Maybe device is being removed. */
        Irp->IoStatus.Status = ntStatus;
        PoStartNextPowerIrp(Irp);
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return ntStatus;
    }

    PoStartNextPowerIrp(Irp);
    IoSkipCurrentIrpStackLocation(Irp);
    ntStatus = PoCallDriver(pDeviceExtension->NextLowerDriver, Irp);
    
    IoReleaseRemoveLock(&pDeviceExtension->RemoveLock, Irp);
    InterlockedDecrement(&pDeviceExtension->RemoveLockCount);

    return ntStatus;
}

/*
 * Routine Description:
 *
 * Free all the allocated resources in DriverEntry, etc.
 */
VOID FilterUnload(__in PDRIVER_OBJECT DriverObject)
{
    PAGED_CODE();

    /* The device object(s) should be NULL now (since we unload, all the devices objects associated with this driver must be deleted. */
    if (DriverObject->DeviceObject != NULL) {
        ASSERTMSG("DeviceObject is not deleted ", FALSE);
    }

    /* We should not be unloaded until all the devices we control have been removed from our queue. */
    L("[%s] Called\n", FN);
    
    return;
}

NTSTATUS FilterCreateControlObject(__in PDEVICE_OBJECT DeviceObject)
{
    UNICODE_STRING ntDeviceName;
    UNICODE_STRING symbolicLinkName;
    PDEVICE_OBJECT pCtrlDeviceObject;
    PCONTROL_DEVICE_EXTENSION pControlExtension;
    PDEVICE_EXTENSION pFilterExtension;
    NTSTATUS ntStatus = STATUS_UNSUCCESSFUL;
    UNICODE_STRING sddlString;

    PAGED_CODE();
    
    pFilterExtension = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

	/* IoCreateDeviceSecure & IoCreateSymbolicLink must be called at PASSIVE_LEVEL. Hence we use an event and not a fast mutex. */
    KeEnterCriticalRegion();
    KeWaitForSingleObject(&pFilterExtension->ControlLock, Executive, KernelMode, FALSE, NULL);

    /* If this is a first instance of the device, then create a control object and register dispatch points to handle ioctls. */
    if (++pFilterExtension->InstanceCount == 1) {
        /* Initialize the unicode strings */
        RtlInitUnicodeString(&ntDeviceName, NTDEVICE_NAME_STRING);
        RtlInitUnicodeString(&symbolicLinkName, SYMBOLIC_NAME_STRING);

        /* Initialize a security descriptor string. Refer to SDDL docs in the SDK for more info. */
        RtlInitUnicodeString( &sddlString, L"D:P(A;;GA;;;SY)(A;;GA;;;BA)");

        /*
         * Create a named deviceobject so that applications or drivers can directly talk to us without going throuhg the entire
         * stack. This call could fail if there are not enough resources or another deviceobject of same name exists (name
         * collision). Let us use the new IoCreateDeviceSecure and specify a security descriptor (SD) that allows only System
         * and Admin groups to access the control device. Let us also specify a unique guid to allow administrators to change
         * the SD if he desires to do so without changing the driver. The SD will be stored in:
         *
         *   HKLM\SYSTEM\CCSet\Control\Class\<GUID>\Properties\Security
         *
         * An admin can override the SD specified in the below call by modifying the registry.
         */
        ntStatus = IoCreateDeviceSecure(DeviceObject->DriverObject,
                                        sizeof(CONTROL_DEVICE_EXTENSION),
                                        &ntDeviceName,
                                        FILE_DEVICE_UNKNOWN,
                                        FILE_DEVICE_SECURE_OPEN,
                                        FALSE,
                                        &sddlString,
                                        (LPCGUID)&GUID_SD_FILTER_CONTROL_OBJECT,
                                        &pCtrlDeviceObject);
        
        if (NT_SUCCESS(ntStatus)) {
            L("[%s] ControlDeviceObject created 0x%p\n", FN, pCtrlDeviceObject);

            pCtrlDeviceObject->Flags |= DO_BUFFERED_IO;
            
            ntStatus = IoCreateSymbolicLink(&symbolicLinkName, &ntDeviceName);

            if (!NT_SUCCESS(ntStatus)) {
                IoDeleteDevice(pCtrlDeviceObject);
                goto End;
            }

            pControlExtension = (PCONTROL_DEVICE_EXTENSION)pCtrlDeviceObject->DeviceExtension;
            pControlExtension->Common.Type = DEVICE_TYPE_CDO;
            pControlExtension->ControlData = NULL;
            pControlExtension->Deleted = (ULONG)FALSE;
            pControlExtension->Self = pCtrlDeviceObject;
            pControlExtension->FilterExtension = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
            pFilterExtension->ControlDeviceObject = pCtrlDeviceObject;
            
            L("[%s] FilterExtension (From Control): 0x%p\n", FN, pControlExtension->FilterExtension);
            
            pCtrlDeviceObject->Flags &= ~DO_DEVICE_INITIALIZING;
        } else {
            L("[%s] IoCreateDeviceSecure failed: 0x%x\n", FN, ntStatus);
        }
    }

    L("[%s] Control object instance count: %d\n", FN, pFilterExtension->InstanceCount);
    
End:
    KeSetEvent(&pFilterExtension->ControlLock, IO_NO_INCREMENT, FALSE);
    KeLeaveCriticalRegion();
    
    return ntStatus;
}

VOID FilterDeleteControlObject(__in PDEVICE_OBJECT DeviceObject)
{
	UNICODE_STRING symbolicLinkName;
	PDEVICE_OBJECT pCtrlDeviceObject;
	PDEVICE_EXTENSION pFilterExtenstion;
	PCONTROL_DEVICE_EXTENSION pCtrlExtension;

	PAGED_CODE();    

	pFilterExtenstion = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

	KeEnterCriticalRegion();
	KeWaitForSingleObject(&pFilterExtenstion->ControlLock, Executive, KernelMode, FALSE, NULL);

	pCtrlDeviceObject = pFilterExtenstion->ControlDeviceObject;

	/*
	 * If this is the last instance of the device then delete the control object and symbolic link to enable the pnp manager
	 * to unload the driver.
	 */
	if (!(--pFilterExtenstion->InstanceCount) && pCtrlDeviceObject)
	{
		L("[%s] Delete ControlObject 0x%p\n", FN, pCtrlDeviceObject);

		RtlInitUnicodeString(&symbolicLinkName, SYMBOLIC_NAME_STRING);
		pCtrlExtension = (PCONTROL_DEVICE_EXTENSION)pCtrlDeviceObject->DeviceExtension;
		pCtrlExtension->Deleted = (ULONG)TRUE;
		IoDeleteSymbolicLink(&symbolicLinkName);
		IoDeleteDevice(pCtrlDeviceObject);
	}

	KeSetEvent(&pFilterExtenstion->ControlLock, IO_NO_INCREMENT, FALSE);
	KeLeaveCriticalRegion();
}

/*
 * Routine Description:
 *
 * This routine is the dispatch routine for non passthru irps. We will check the input device object to see if the request is
 * meant for the control device object. If it is, we will handle and complete the IRP, if not, we will pass it down to the
 * lower driver.
 */
NTSTATUS FilterDispatchIo(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
	PIO_STACK_LOCATION pIrpStack;
	NTSTATUS ntStatus;
	PCONTROL_DEVICE_EXTENSION pControlExtension;
	PCOMMON_DEVICE_DATA pCommonData;

	PAGED_CODE();

	pCommonData = (PCOMMON_DEVICE_DATA)DeviceObject->DeviceExtension;

	/*
	 * Please note that this is a common dispatch point for controlobject and filter deviceobject attached to the pnp stack. 
	 */
	if (pCommonData->Type == DEVICE_TYPE_FIDO)
	{
		/*
		 * We will just pass the request down as we are not interested in handling requests that come on the PnP stack.
		 */
		return FilterPass(DeviceObject, Irp);    
	}

	ASSERT(pCommonData->Type == DEVICE_TYPE_CDO);

	pControlExtension = (PCONTROL_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

	/*
	 * Else this is targeted at our control deviceobject so let's handle it. Here we will handle the IOCTl requests that come
	 * from the app. We don't have to worry about acquiring remlocks for I/Os that come on our control object because the I/O
	 * manager takes reference on our deviceobject when it initiates a request to our device and that keeps our driver from
	 * unloading when we have pending I/Os. But we still have to watch out for a scenario where another driver can send
	 * requests to our deviceobject directly without opening a handle.
	 */
	if (!pControlExtension->Deleted)
	{
		ntStatus = STATUS_SUCCESS;
		Irp->IoStatus.Information = 0;
		pIrpStack = IoGetCurrentIrpStackLocation(Irp);

		switch (pIrpStack->MajorFunction)
		{
			case IRP_MJ_CREATE:
				// L("[%s] ControlObject: IRP_MJ_CREATE\n", FN);
				break;

			case IRP_MJ_CLOSE:
				// L("[%s] ControlObject: IRP_MJ_CLOSE\n", FN);
				break;

			case IRP_MJ_CLEANUP:
				// L("[%s] ControlObject: IRP_MJ_CLEANUP\n", FN);
				break;

			case  IRP_MJ_DEVICE_CONTROL:
			{
				switch (pIrpStack->Parameters.DeviceIoControl.IoControlCode)
				{
					case IOCTL_LENCFLTR_TEST:
						ntStatus = ControlDispatchIoTestDirect(DeviceObject, Irp);
						break;

					case IOCTL_LENCFLTR_TEST_SM:
						ntStatus = STATUS_SUCCESS;
						break;

					case IOCTL_LENCFLTR_TEST_X:
						ntStatus = STATUS_SUCCESS;
						break;

					case IOCTL_LENCFLTR_BUFFER_REFRESH:
						ntStatus = ControlDispatchIoBufferRefresh(DeviceObject, Irp);
						break;

					case IOCTL_LENCFLTR_INTRNAL_STRM_START:
						ntStatus = ControlDispatchIoStreamStart(DeviceObject, Irp);
						break;

					case IOCTL_LENCFLTR_INTRNAL_STRM_STOP:
						ntStatus = ControlDispatchIoStreamStop(DeviceObject, Irp);
						break;
					
					default:
						ntStatus = STATUS_INVALID_PARAMETER;
						break;
				}
			}

			default: break;
		}
	}
	else
	{
		ASSERTMSG(FALSE, "Requests being sent to a dead device\n");
		ntStatus = STATUS_DEVICE_REMOVED;
	}
	
	Irp->IoStatus.Status = ntStatus;
	IoCompleteRequest (Irp, IO_NO_INCREMENT);
	return ntStatus;
}

#if DBG
PCHAR PnPMinorFunctionString(UCHAR MinorFunction)
{
	switch (MinorFunction)
	{
		case IRP_MN_START_DEVICE:					return "IRP_MN_START_DEVICE";
		case IRP_MN_QUERY_REMOVE_DEVICE:			return "IRP_MN_QUERY_REMOVE_DEVICE";
		case IRP_MN_REMOVE_DEVICE:					return "IRP_MN_REMOVE_DEVICE";
		case IRP_MN_CANCEL_REMOVE_DEVICE:			return "IRP_MN_CANCEL_REMOVE_DEVICE";
		case IRP_MN_STOP_DEVICE:					return "IRP_MN_STOP_DEVICE";
		case IRP_MN_QUERY_STOP_DEVICE:				return "IRP_MN_QUERY_STOP_DEVICE";
		case IRP_MN_CANCEL_STOP_DEVICE:				return "IRP_MN_CANCEL_STOP_DEVICE";
		case IRP_MN_QUERY_DEVICE_RELATIONS:			return "IRP_MN_QUERY_DEVICE_RELATIONS";
		case IRP_MN_QUERY_INTERFACE:				return "IRP_MN_QUERY_INTERFACE";
		case IRP_MN_QUERY_CAPABILITIES:				return "IRP_MN_QUERY_CAPABILITIES";
		case IRP_MN_QUERY_RESOURCES:				return "IRP_MN_QUERY_RESOURCES";
		case IRP_MN_QUERY_RESOURCE_REQUIREMENTS:	return "IRP_MN_QUERY_RESOURCE_REQUIREMENTS";
		case IRP_MN_QUERY_DEVICE_TEXT:				return "IRP_MN_QUERY_DEVICE_TEXT";
		case IRP_MN_FILTER_RESOURCE_REQUIREMENTS:	return "IRP_MN_FILTER_RESOURCE_REQUIREMENTS";
		case IRP_MN_READ_CONFIG:					return "IRP_MN_READ_CONFIG";
		case IRP_MN_WRITE_CONFIG:					return "IRP_MN_WRITE_CONFIG";
		case IRP_MN_EJECT:							return "IRP_MN_EJECT";
		case IRP_MN_SET_LOCK:						return "IRP_MN_SET_LOCK";
		case IRP_MN_QUERY_ID:						return "IRP_MN_QUERY_ID";
		case IRP_MN_QUERY_PNP_DEVICE_STATE:			return "IRP_MN_QUERY_PNP_DEVICE_STATE";
		case IRP_MN_QUERY_BUS_INFORMATION:			return "IRP_MN_QUERY_BUS_INFORMATION";
		case IRP_MN_DEVICE_USAGE_NOTIFICATION:		return "IRP_MN_DEVICE_USAGE_NOTIFICATION";
		case IRP_MN_SURPRISE_REMOVAL:				return "IRP_MN_SURPRISE_REMOVAL";
		default:									return "UNKNOWN_PNP_IRP";
	}
}
#endif
