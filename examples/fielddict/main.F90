program main

   use mod_config
   use mod_types
   use ESMF
   use NUOPC

   implicit none

   integer :: rc
   integer :: iEntry
   integer :: nATMImport, nATMExport
   integer :: nOCNImport, nOCNExport
   integer :: nICEImport, nICEExport
   type(NUOPC_FreeFormat) :: fdff
   type(ESMF_GridComp) :: esmComp
   type(ESMF_State) :: atmImportState, atmExportState
   type(ESMF_State) :: ocnImportState, ocnExportState
   type(ESMF_State) :: iceImportState, iceExportState
   type(ESMF_VM) :: vm
   character(ESMF_MAXSTR) :: entryNameWRF

   !-----------------------------------------------------------------------
!     Initialize ESMF framework
!-----------------------------------------------------------------------
!
   call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
                        defaultCalkind=ESMF_CALKIND_GREGORIAN, &
                        vm=vm, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Create component
!-----------------------------------------------------------------------
!
   esmComp = ESMF_GridCompCreate(name="NEW_GRID", rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call read_config(vm, rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   atmImportState = ESMF_StateCreate(name="ATM_import", &
                                     stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   atmExportState = ESMF_StateCreate(name="ATM_export", &
                                     stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ocnImportState = ESMF_StateCreate(name="OCN_import", &
                                     stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ocnExportState = ESMF_StateCreate(name="OCN_export", &
                                     stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   iceImportState = ESMF_StateCreate(name="ICE_import", &
                                     stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   iceExportState = ESMF_StateCreate(name="ICE_export", &
                                     stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   nATMImport = 0
   nATMExport = 0
   nOCNImport = 0
   nOCNExport = 0
   nICEImport = 0
   nICEExport = 0

   do iEntry = 1, nATMExportList
      entryNameWRF = trim(atm_exportWRFNameList(iEntry))
      call NUOPC_Advertise(atmExportState, &
                           StandardName=entryNameWRF, name=entryNameWRF, rc=rc)
      nATMExport = nATMExport + 1

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                             line=__LINE__, file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
   end do

   do iEntry = 1, nATMImportList
      entryNameWRF = trim(atm_importWRFNameList(iEntry))
      call NUOPC_Advertise(atmImportState, &
                           StandardName=entryNameWRF, name=entryNameWRF, rc=rc)
      nATMImport = nATMImport + 1

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                             line=__LINE__, file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
   end do

   do iEntry = 1, nOCNExportList
      entryNameWRF = trim(ocn_exportWRFNameList(iEntry))
      call NUOPC_Advertise(ocnExportState, &
                           StandardName=entryNameWRF, name=entryNameWRF, rc=rc)
      nOCNExport = nOCNExport + 1

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                             line=__LINE__, file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
   end do

   do iEntry = 1, nOCNImportList
      entryNameWRF = trim(ocn_importWRFNameList(iEntry))
      call NUOPC_Advertise(ocnImportState, &
                           StandardName=entryNameWRF, name=entryNameWRF, rc=rc)
      nOCNImport = nOCNImport + 1

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                             line=__LINE__, file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
   end do

   do iEntry = 1, nICEExportList
      entryNameWRF = trim(ice_exportWRFNameList(iEntry))
      call NUOPC_Advertise(iceExportState, &
                           StandardName=entryNameWRF, name=entryNameWRF, rc=rc)
      nICEExport = nICEExport + 1

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                             line=__LINE__, file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
   end do

   do iEntry = 1, nICEImportList
      entryNameWRF = trim(ice_importWRFNameList(iEntry))
      call NUOPC_Advertise(iceImportState, &
                           StandardName=entryNameWRF, name=entryNameWRF, rc=rc)
      nICEImport = nICEImport + 1

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                             line=__LINE__, file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
   end do

   print *, 'ATM advertised import fields: ', nATMImport
   print *, 'ATM advertised export fields: ', nATMExport
   print *, 'OCN advertised import fields: ', nOCNImport
   print *, 'OCN advertised export fields: ', nOCNExport
   print *, 'ICE advertised import fields: ', nICEImport
   print *, 'ICE advertised export fields: ', nICEExport

   call NUOPC_FieldDictionaryEgest(fdff, iofmt=ESMF_IOFMT_YAML, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call NUOPC_FreeFormatLog(fdff, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call NUOPC_FreeFormatDestroy(fdff, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

end program main
