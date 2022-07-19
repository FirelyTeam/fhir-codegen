// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// More detailed code concerning the type of the audit event - defined by DICOM with some FHIR specific additions.
  /// </summary>
  public static class AuditEventSubTypeCodes
  {
    /// <summary>
    /// Audit event: Application Entity has started
    /// </summary>
    public static readonly Coding ApplicationStart_dicom_dcim = new Coding
    {
      Code = "110120",
      Display = "Application Start",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Application Entity has stopped
    /// </summary>
    public static readonly Coding ApplicationStop_dicom_dcim = new Coding
    {
      Code = "110121",
      Display = "Application Stop",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: User login has been attempted
    /// </summary>
    public static readonly Coding Login_dicom_dcim = new Coding
    {
      Code = "110122",
      Display = "Login",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: User logout has been attempted
    /// </summary>
    public static readonly Coding Logout_dicom_dcim = new Coding
    {
      Code = "110123",
      Display = "Logout",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Node has been attached
    /// </summary>
    public static readonly Coding Attach_dicom_dcim = new Coding
    {
      Code = "110124",
      Display = "Attach",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Node has been detached
    /// </summary>
    public static readonly Coding Detach_dicom_dcim = new Coding
    {
      Code = "110125",
      Display = "Detach",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Node Authentication has been attempted
    /// </summary>
    public static readonly Coding NodeAuthentication_dicom_dcim = new Coding
    {
      Code = "110126",
      Display = "Node Authentication",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Emergency Override has started
    /// </summary>
    public static readonly Coding EmergencyOverrideStarted_dicom_dcim = new Coding
    {
      Code = "110127",
      Display = "Emergency Override Started",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Network configuration has been changed
    /// </summary>
    public static readonly Coding NetworkConfiguration_dicom_dcim = new Coding
    {
      Code = "110128",
      Display = "Network Configuration",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Security configuration has been changed
    /// </summary>
    public static readonly Coding SecurityConfiguration_dicom_dcim = new Coding
    {
      Code = "110129",
      Display = "Security Configuration",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Hardware configuration has been changed
    /// </summary>
    public static readonly Coding HardwareConfiguration_dicom_dcim = new Coding
    {
      Code = "110130",
      Display = "Hardware Configuration",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Software configuration has been changed
    /// </summary>
    public static readonly Coding SoftwareConfiguration_dicom_dcim = new Coding
    {
      Code = "110131",
      Display = "Software Configuration",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: A use of a restricted function has been attempted
    /// </summary>
    public static readonly Coding UseOfRestrictedFunction_dicom_dcim = new Coding
    {
      Code = "110132",
      Display = "Use of Restricted Function",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Audit recording has been stopped
    /// </summary>
    public static readonly Coding AuditRecordingStopped_dicom_dcim = new Coding
    {
      Code = "110133",
      Display = "Audit Recording Stopped",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Audit recording has been started
    /// </summary>
    public static readonly Coding AuditRecordingStarted_dicom_dcim = new Coding
    {
      Code = "110134",
      Display = "Audit Recording Started",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Security attributes of an object have been changed
    /// </summary>
    public static readonly Coding ObjectSecurityAttributesChanged_dicom_dcim = new Coding
    {
      Code = "110135",
      Display = "Object Security Attributes Changed",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Security roles have been changed
    /// </summary>
    public static readonly Coding SecurityRolesChanged_dicom_dcim = new Coding
    {
      Code = "110136",
      Display = "Security Roles Changed",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Security attributes of a user have been changed
    /// </summary>
    public static readonly Coding UserSecurityAttributesChanged_dicom_dcim = new Coding
    {
      Code = "110137",
      Display = "User security Attributes Changed",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Emergency Override has Stopped
    /// </summary>
    public static readonly Coding EmergencyOverrideStopped_dicom_dcim = new Coding
    {
      Code = "110138",
      Display = "Emergency Override Stopped",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Remote Service Operation has Begun
    /// </summary>
    public static readonly Coding RemoteServiceOperationStarted_dicom_dcim = new Coding
    {
      Code = "110139",
      Display = "Remote Service Operation Started",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Remote Service Operation has Stopped
    /// </summary>
    public static readonly Coding RemoteServiceOperationStopped_dicom_dcim = new Coding
    {
      Code = "110140",
      Display = "Remote Service Operation Stopped",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Local Service Operation has Begun
    /// </summary>
    public static readonly Coding LocalServiceOperationStarted_dicom_dcim = new Coding
    {
      Code = "110141",
      Display = "Local Service Operation Started",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Local Service Operation Stopped
    /// </summary>
    public static readonly Coding LocalServiceOperationStopped_dicom_dcim = new Coding
    {
      Code = "110142",
      Display = "Local Service Operation Stopped",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// perform a set of a separate interactions in a single http operation
    /// </summary>
    public static readonly Coding Batch_restful_interaction = new Coding
    {
      Code = "batch",
      Display = "batch",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Get a Capability Statement for the system.
    /// </summary>
    public static readonly Coding Capabilities_restful_interaction = new Coding
    {
      Code = "capabilities",
      Display = "capabilities",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Create a new resource with a server assigned id.
    /// </summary>
    public static readonly Coding Create_restful_interaction = new Coding
    {
      Code = "create",
      Display = "create",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Delete a resource.
    /// </summary>
    public static readonly Coding Delete_restful_interaction = new Coding
    {
      Code = "delete",
      Display = "delete",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Retrieve the change history for a particular resource, type of resource, or the entire system.
    /// </summary>
    public static readonly Coding History_restful_interaction = new Coding
    {
      Code = "history",
      Display = "history",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Retrieve the change history for a particular resource.
    /// </summary>
    public static readonly Coding HistoryInstance_restful_interaction = new Coding
    {
      Code = "history-instance",
      Display = "history-instance",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Retrieve the change history for all resources on a system.
    /// </summary>
    public static readonly Coding HistorySystem_restful_interaction = new Coding
    {
      Code = "history-system",
      Display = "history-system",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Retrieve the change history for all resources of a particular type.
    /// </summary>
    public static readonly Coding HistoryType_restful_interaction = new Coding
    {
      Code = "history-type",
      Display = "history-type",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Perform an operation as defined by an OperationDefinition.
    /// </summary>
    public static readonly Coding Operation_restful_interaction = new Coding
    {
      Code = "operation",
      Display = "operation",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Update an existing resource by posting a set of changes to it.
    /// </summary>
    public static readonly Coding Patch_restful_interaction = new Coding
    {
      Code = "patch",
      Display = "patch",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Read the current state of the resource.
    /// </summary>
    public static readonly Coding Read_restful_interaction = new Coding
    {
      Code = "read",
      Display = "read",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Search a resource type or all resources based on some filter criteria.
    /// </summary>
    public static readonly Coding Search_restful_interaction = new Coding
    {
      Code = "search",
      Display = "search",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Search all resources based on some filter criteria.
    /// </summary>
    public static readonly Coding SearchSystem_restful_interaction = new Coding
    {
      Code = "search-system",
      Display = "search-system",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Search all resources of the specified type based on some filter criteria.
    /// </summary>
    public static readonly Coding SearchType_restful_interaction = new Coding
    {
      Code = "search-type",
      Display = "search-type",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Update, create or delete a set of resources as a single transaction.
    /// </summary>
    public static readonly Coding Transaction_restful_interaction = new Coding
    {
      Code = "transaction",
      Display = "transaction",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Update an existing resource by its id (or create it if it is new).
    /// </summary>
    public static readonly Coding Update_restful_interaction = new Coding
    {
      Code = "update",
      Display = "update",
      System = "http://hl7.org/fhir/restful-interaction"
    };
    /// <summary>
    /// Read the state of a specific version of the resource.
    /// </summary>
    public static readonly Coding Vread_restful_interaction = new Coding
    {
      Code = "vread",
      Display = "vread",
      System = "http://hl7.org/fhir/restful-interaction"
    };

    /// <summary>
    /// Literal for code: ApplicationStart_dicom_dcim
    /// </summary>
    public const string LiteralApplicationStart_dicom_dcim = "110120";

    /// <summary>
    /// Literal for code: DicomDcimApplicationStart_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimApplicationStart_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110120";

    /// <summary>
    /// Literal for code: ApplicationStop_dicom_dcim
    /// </summary>
    public const string LiteralApplicationStop_dicom_dcim = "110121";

    /// <summary>
    /// Literal for code: DicomDcimApplicationStop_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimApplicationStop_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110121";

    /// <summary>
    /// Literal for code: Login_dicom_dcim
    /// </summary>
    public const string LiteralLogin_dicom_dcim = "110122";

    /// <summary>
    /// Literal for code: DicomDcimLogin_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimLogin_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110122";

    /// <summary>
    /// Literal for code: Logout_dicom_dcim
    /// </summary>
    public const string LiteralLogout_dicom_dcim = "110123";

    /// <summary>
    /// Literal for code: DicomDcimLogout_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimLogout_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110123";

    /// <summary>
    /// Literal for code: Attach_dicom_dcim
    /// </summary>
    public const string LiteralAttach_dicom_dcim = "110124";

    /// <summary>
    /// Literal for code: DicomDcimAttach_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimAttach_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110124";

    /// <summary>
    /// Literal for code: Detach_dicom_dcim
    /// </summary>
    public const string LiteralDetach_dicom_dcim = "110125";

    /// <summary>
    /// Literal for code: DicomDcimDetach_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimDetach_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110125";

    /// <summary>
    /// Literal for code: NodeAuthentication_dicom_dcim
    /// </summary>
    public const string LiteralNodeAuthentication_dicom_dcim = "110126";

    /// <summary>
    /// Literal for code: DicomDcimNodeAuthentication_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimNodeAuthentication_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110126";

    /// <summary>
    /// Literal for code: EmergencyOverrideStarted_dicom_dcim
    /// </summary>
    public const string LiteralEmergencyOverrideStarted_dicom_dcim = "110127";

    /// <summary>
    /// Literal for code: DicomDcimEmergencyOverrideStarted_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimEmergencyOverrideStarted_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110127";

    /// <summary>
    /// Literal for code: NetworkConfiguration_dicom_dcim
    /// </summary>
    public const string LiteralNetworkConfiguration_dicom_dcim = "110128";

    /// <summary>
    /// Literal for code: DicomDcimNetworkConfiguration_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimNetworkConfiguration_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110128";

    /// <summary>
    /// Literal for code: SecurityConfiguration_dicom_dcim
    /// </summary>
    public const string LiteralSecurityConfiguration_dicom_dcim = "110129";

    /// <summary>
    /// Literal for code: DicomDcimSecurityConfiguration_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimSecurityConfiguration_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110129";

    /// <summary>
    /// Literal for code: HardwareConfiguration_dicom_dcim
    /// </summary>
    public const string LiteralHardwareConfiguration_dicom_dcim = "110130";

    /// <summary>
    /// Literal for code: DicomDcimHardwareConfiguration_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimHardwareConfiguration_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110130";

    /// <summary>
    /// Literal for code: SoftwareConfiguration_dicom_dcim
    /// </summary>
    public const string LiteralSoftwareConfiguration_dicom_dcim = "110131";

    /// <summary>
    /// Literal for code: DicomDcimSoftwareConfiguration_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimSoftwareConfiguration_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110131";

    /// <summary>
    /// Literal for code: UseOfRestrictedFunction_dicom_dcim
    /// </summary>
    public const string LiteralUseOfRestrictedFunction_dicom_dcim = "110132";

    /// <summary>
    /// Literal for code: DicomDcimUseOfRestrictedFunction_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimUseOfRestrictedFunction_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110132";

    /// <summary>
    /// Literal for code: AuditRecordingStopped_dicom_dcim
    /// </summary>
    public const string LiteralAuditRecordingStopped_dicom_dcim = "110133";

    /// <summary>
    /// Literal for code: DicomDcimAuditRecordingStopped_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimAuditRecordingStopped_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110133";

    /// <summary>
    /// Literal for code: AuditRecordingStarted_dicom_dcim
    /// </summary>
    public const string LiteralAuditRecordingStarted_dicom_dcim = "110134";

    /// <summary>
    /// Literal for code: DicomDcimAuditRecordingStarted_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimAuditRecordingStarted_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110134";

    /// <summary>
    /// Literal for code: ObjectSecurityAttributesChanged_dicom_dcim
    /// </summary>
    public const string LiteralObjectSecurityAttributesChanged_dicom_dcim = "110135";

    /// <summary>
    /// Literal for code: DicomDcimObjectSecurityAttributesChanged_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimObjectSecurityAttributesChanged_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110135";

    /// <summary>
    /// Literal for code: SecurityRolesChanged_dicom_dcim
    /// </summary>
    public const string LiteralSecurityRolesChanged_dicom_dcim = "110136";

    /// <summary>
    /// Literal for code: DicomDcimSecurityRolesChanged_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimSecurityRolesChanged_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110136";

    /// <summary>
    /// Literal for code: UserSecurityAttributesChanged_dicom_dcim
    /// </summary>
    public const string LiteralUserSecurityAttributesChanged_dicom_dcim = "110137";

    /// <summary>
    /// Literal for code: DicomDcimUserSecurityAttributesChanged_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimUserSecurityAttributesChanged_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110137";

    /// <summary>
    /// Literal for code: EmergencyOverrideStopped_dicom_dcim
    /// </summary>
    public const string LiteralEmergencyOverrideStopped_dicom_dcim = "110138";

    /// <summary>
    /// Literal for code: DicomDcimEmergencyOverrideStopped_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimEmergencyOverrideStopped_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110138";

    /// <summary>
    /// Literal for code: RemoteServiceOperationStarted_dicom_dcim
    /// </summary>
    public const string LiteralRemoteServiceOperationStarted_dicom_dcim = "110139";

    /// <summary>
    /// Literal for code: DicomDcimRemoteServiceOperationStarted_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimRemoteServiceOperationStarted_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110139";

    /// <summary>
    /// Literal for code: RemoteServiceOperationStopped_dicom_dcim
    /// </summary>
    public const string LiteralRemoteServiceOperationStopped_dicom_dcim = "110140";

    /// <summary>
    /// Literal for code: DicomDcimRemoteServiceOperationStopped_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimRemoteServiceOperationStopped_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110140";

    /// <summary>
    /// Literal for code: LocalServiceOperationStarted_dicom_dcim
    /// </summary>
    public const string LiteralLocalServiceOperationStarted_dicom_dcim = "110141";

    /// <summary>
    /// Literal for code: DicomDcimLocalServiceOperationStarted_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimLocalServiceOperationStarted_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110141";

    /// <summary>
    /// Literal for code: LocalServiceOperationStopped_dicom_dcim
    /// </summary>
    public const string LiteralLocalServiceOperationStopped_dicom_dcim = "110142";

    /// <summary>
    /// Literal for code: DicomDcimLocalServiceOperationStopped_dicom_dcim
    /// </summary>
    public const string LiteralDicomDcimLocalServiceOperationStopped_dicom_dcim = "http://dicom.nema.org/resources/ontology/DCM#110142";

    /// <summary>
    /// Literal for code: Batch_restful_interaction
    /// </summary>
    public const string LiteralBatch_restful_interaction = "batch";

    /// <summary>
    /// Literal for code: RestfulInteractionBatch_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionBatch_restful_interaction = "http://hl7.org/fhir/restful-interaction#batch";

    /// <summary>
    /// Literal for code: Capabilities_restful_interaction
    /// </summary>
    public const string LiteralCapabilities_restful_interaction = "capabilities";

    /// <summary>
    /// Literal for code: RestfulInteractionCapabilities_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionCapabilities_restful_interaction = "http://hl7.org/fhir/restful-interaction#capabilities";

    /// <summary>
    /// Literal for code: Create_restful_interaction
    /// </summary>
    public const string LiteralCreate_restful_interaction = "create";

    /// <summary>
    /// Literal for code: RestfulInteractionCreate_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionCreate_restful_interaction = "http://hl7.org/fhir/restful-interaction#create";

    /// <summary>
    /// Literal for code: Delete_restful_interaction
    /// </summary>
    public const string LiteralDelete_restful_interaction = "delete";

    /// <summary>
    /// Literal for code: RestfulInteractionDelete_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionDelete_restful_interaction = "http://hl7.org/fhir/restful-interaction#delete";

    /// <summary>
    /// Literal for code: History_restful_interaction
    /// </summary>
    public const string LiteralHistory_restful_interaction = "history";

    /// <summary>
    /// Literal for code: RestfulInteractionHistory_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionHistory_restful_interaction = "http://hl7.org/fhir/restful-interaction#history";

    /// <summary>
    /// Literal for code: HistoryInstance_restful_interaction
    /// </summary>
    public const string LiteralHistoryInstance_restful_interaction = "history-instance";

    /// <summary>
    /// Literal for code: RestfulInteractionHistoryInstance_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionHistoryInstance_restful_interaction = "http://hl7.org/fhir/restful-interaction#history-instance";

    /// <summary>
    /// Literal for code: HistorySystem_restful_interaction
    /// </summary>
    public const string LiteralHistorySystem_restful_interaction = "history-system";

    /// <summary>
    /// Literal for code: RestfulInteractionHistorySystem_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionHistorySystem_restful_interaction = "http://hl7.org/fhir/restful-interaction#history-system";

    /// <summary>
    /// Literal for code: HistoryType_restful_interaction
    /// </summary>
    public const string LiteralHistoryType_restful_interaction = "history-type";

    /// <summary>
    /// Literal for code: RestfulInteractionHistoryType_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionHistoryType_restful_interaction = "http://hl7.org/fhir/restful-interaction#history-type";

    /// <summary>
    /// Literal for code: Operation_restful_interaction
    /// </summary>
    public const string LiteralOperation_restful_interaction = "operation";

    /// <summary>
    /// Literal for code: RestfulInteractionOperation_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionOperation_restful_interaction = "http://hl7.org/fhir/restful-interaction#operation";

    /// <summary>
    /// Literal for code: Patch_restful_interaction
    /// </summary>
    public const string LiteralPatch_restful_interaction = "patch";

    /// <summary>
    /// Literal for code: RestfulInteractionPatch_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionPatch_restful_interaction = "http://hl7.org/fhir/restful-interaction#patch";

    /// <summary>
    /// Literal for code: Read_restful_interaction
    /// </summary>
    public const string LiteralRead_restful_interaction = "read";

    /// <summary>
    /// Literal for code: RestfulInteractionRead_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionRead_restful_interaction = "http://hl7.org/fhir/restful-interaction#read";

    /// <summary>
    /// Literal for code: Search_restful_interaction
    /// </summary>
    public const string LiteralSearch_restful_interaction = "search";

    /// <summary>
    /// Literal for code: RestfulInteractionSearch_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionSearch_restful_interaction = "http://hl7.org/fhir/restful-interaction#search";

    /// <summary>
    /// Literal for code: SearchSystem_restful_interaction
    /// </summary>
    public const string LiteralSearchSystem_restful_interaction = "search-system";

    /// <summary>
    /// Literal for code: RestfulInteractionSearchSystem_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionSearchSystem_restful_interaction = "http://hl7.org/fhir/restful-interaction#search-system";

    /// <summary>
    /// Literal for code: SearchType_restful_interaction
    /// </summary>
    public const string LiteralSearchType_restful_interaction = "search-type";

    /// <summary>
    /// Literal for code: RestfulInteractionSearchType_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionSearchType_restful_interaction = "http://hl7.org/fhir/restful-interaction#search-type";

    /// <summary>
    /// Literal for code: Transaction_restful_interaction
    /// </summary>
    public const string LiteralTransaction_restful_interaction = "transaction";

    /// <summary>
    /// Literal for code: RestfulInteractionTransaction_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionTransaction_restful_interaction = "http://hl7.org/fhir/restful-interaction#transaction";

    /// <summary>
    /// Literal for code: Update_restful_interaction
    /// </summary>
    public const string LiteralUpdate_restful_interaction = "update";

    /// <summary>
    /// Literal for code: RestfulInteractionUpdate_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionUpdate_restful_interaction = "http://hl7.org/fhir/restful-interaction#update";

    /// <summary>
    /// Literal for code: Vread_restful_interaction
    /// </summary>
    public const string LiteralVread_restful_interaction = "vread";

    /// <summary>
    /// Literal for code: RestfulInteractionVread_restful_interaction
    /// </summary>
    public const string LiteralRestfulInteractionVread_restful_interaction = "http://hl7.org/fhir/restful-interaction#vread";

    /// <summary>
    /// Dictionary for looking up AuditEventSubType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "110120", ApplicationStart_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110120", ApplicationStart_dicom_dcim }, 
      { "110121", ApplicationStop_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110121", ApplicationStop_dicom_dcim }, 
      { "110122", Login_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110122", Login_dicom_dcim }, 
      { "110123", Logout_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110123", Logout_dicom_dcim }, 
      { "110124", Attach_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110124", Attach_dicom_dcim }, 
      { "110125", Detach_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110125", Detach_dicom_dcim }, 
      { "110126", NodeAuthentication_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110126", NodeAuthentication_dicom_dcim }, 
      { "110127", EmergencyOverrideStarted_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110127", EmergencyOverrideStarted_dicom_dcim }, 
      { "110128", NetworkConfiguration_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110128", NetworkConfiguration_dicom_dcim }, 
      { "110129", SecurityConfiguration_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110129", SecurityConfiguration_dicom_dcim }, 
      { "110130", HardwareConfiguration_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110130", HardwareConfiguration_dicom_dcim }, 
      { "110131", SoftwareConfiguration_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110131", SoftwareConfiguration_dicom_dcim }, 
      { "110132", UseOfRestrictedFunction_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110132", UseOfRestrictedFunction_dicom_dcim }, 
      { "110133", AuditRecordingStopped_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110133", AuditRecordingStopped_dicom_dcim }, 
      { "110134", AuditRecordingStarted_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110134", AuditRecordingStarted_dicom_dcim }, 
      { "110135", ObjectSecurityAttributesChanged_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110135", ObjectSecurityAttributesChanged_dicom_dcim }, 
      { "110136", SecurityRolesChanged_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110136", SecurityRolesChanged_dicom_dcim }, 
      { "110137", UserSecurityAttributesChanged_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110137", UserSecurityAttributesChanged_dicom_dcim }, 
      { "110138", EmergencyOverrideStopped_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110138", EmergencyOverrideStopped_dicom_dcim }, 
      { "110139", RemoteServiceOperationStarted_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110139", RemoteServiceOperationStarted_dicom_dcim }, 
      { "110140", RemoteServiceOperationStopped_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110140", RemoteServiceOperationStopped_dicom_dcim }, 
      { "110141", LocalServiceOperationStarted_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110141", LocalServiceOperationStarted_dicom_dcim }, 
      { "110142", LocalServiceOperationStopped_dicom_dcim }, 
      { "http://dicom.nema.org/resources/ontology/DCM#110142", LocalServiceOperationStopped_dicom_dcim }, 
      { "batch", Batch_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#batch", Batch_restful_interaction }, 
      { "capabilities", Capabilities_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#capabilities", Capabilities_restful_interaction }, 
      { "create", Create_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#create", Create_restful_interaction }, 
      { "delete", Delete_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#delete", Delete_restful_interaction }, 
      { "history", History_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#history", History_restful_interaction }, 
      { "history-instance", HistoryInstance_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#history-instance", HistoryInstance_restful_interaction }, 
      { "history-system", HistorySystem_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#history-system", HistorySystem_restful_interaction }, 
      { "history-type", HistoryType_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#history-type", HistoryType_restful_interaction }, 
      { "operation", Operation_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#operation", Operation_restful_interaction }, 
      { "patch", Patch_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#patch", Patch_restful_interaction }, 
      { "read", Read_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#read", Read_restful_interaction }, 
      { "search", Search_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#search", Search_restful_interaction }, 
      { "search-system", SearchSystem_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#search-system", SearchSystem_restful_interaction }, 
      { "search-type", SearchType_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#search-type", SearchType_restful_interaction }, 
      { "transaction", Transaction_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#transaction", Transaction_restful_interaction }, 
      { "update", Update_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#update", Update_restful_interaction }, 
      { "vread", Vread_restful_interaction }, 
      { "http://hl7.org/fhir/restful-interaction#vread", Vread_restful_interaction }, 
    };
  };
}
