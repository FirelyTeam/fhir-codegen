// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// A list of all the event resource types defined in this version of the FHIR specification.
  /// </summary>
  public static class EventResourceTypesCodes
  {
    /// <summary>
    /// Item containing charge code(s) associated with the provision of healthcare provider products.
    /// </summary>
    public static readonly Coding ChargeItem = new Coding
    {
      Code = "ChargeItem",
      Display = "ChargeItem",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Remittance resource.
    /// </summary>
    public static readonly Coding ClaimResponse = new Coding
    {
      Code = "ClaimResponse",
      Display = "ClaimResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A clinical assessment performed when planning treatments and management strategies for a patient.
    /// </summary>
    public static readonly Coding ClinicalImpression = new Coding
    {
      Code = "ClinicalImpression",
      Display = "ClinicalImpression",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A record of information transmitted from a sender to a receiver.
    /// </summary>
    public static readonly Coding Communication = new Coding
    {
      Code = "Communication",
      Display = "Communication",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A set of resources composed into a single coherent clinical statement with clinical attestation.
    /// </summary>
    public static readonly Coding Composition = new Coding
    {
      Code = "Composition",
      Display = "Composition",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Detailed information about conditions, problems or diagnoses.
    /// </summary>
    public static readonly Coding Condition = new Coding
    {
      Code = "Condition",
      Display = "Condition",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A healthcare consumer's policy choices to permits or denies recipients or roles to perform actions for specific purposes and periods of time.
    /// </summary>
    public static readonly Coding Consent = new Coding
    {
      Code = "Consent",
      Display = "Consent",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Insurance or medical plan or a payment agreement.
    /// </summary>
    public static readonly Coding Coverage = new Coding
    {
      Code = "Coverage",
      Display = "Coverage",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Record of use of a device.
    /// </summary>
    public static readonly Coding DeviceUseStatement = new Coding
    {
      Code = "DeviceUseStatement",
      Display = "DeviceUseStatement",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A Diagnostic report - a combination of request information, atomic results, images, interpretation, as well as formatted reports.
    /// </summary>
    public static readonly Coding DiagnosticReport = new Coding
    {
      Code = "DiagnosticReport",
      Display = "DiagnosticReport",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A list that defines a set of documents.
    /// </summary>
    public static readonly Coding DocumentManifest = new Coding
    {
      Code = "DocumentManifest",
      Display = "DocumentManifest",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A reference to a document.
    /// </summary>
    public static readonly Coding DocumentReference = new Coding
    {
      Code = "DocumentReference",
      Display = "DocumentReference",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// An interaction during which services are provided to the patient.
    /// </summary>
    public static readonly Coding Encounter = new Coding
    {
      Code = "Encounter",
      Display = "Encounter",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// EnrollmentResponse resource.
    /// </summary>
    public static readonly Coding EnrollmentResponse = new Coding
    {
      Code = "EnrollmentResponse",
      Display = "EnrollmentResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// An association of a Patient with an Organization and  Healthcare Provider(s) for a period of time that the Organization assumes some level of responsibility.
    /// </summary>
    public static readonly Coding EpisodeOfCare = new Coding
    {
      Code = "EpisodeOfCare",
      Display = "EpisodeOfCare",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Explanation of Benefit resource.
    /// </summary>
    public static readonly Coding ExplanationOfBenefit = new Coding
    {
      Code = "ExplanationOfBenefit",
      Display = "ExplanationOfBenefit",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Information about patient's relatives, relevant for patient.
    /// </summary>
    public static readonly Coding FamilyMemberHistory = new Coding
    {
      Code = "FamilyMemberHistory",
      Display = "FamilyMemberHistory",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// The formal response to a guidance request.
    /// </summary>
    public static readonly Coding GuidanceResponse = new Coding
    {
      Code = "GuidanceResponse",
      Display = "GuidanceResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A set of images produced in single study (one or more series of references images).
    /// </summary>
    public static readonly Coding ImagingStudy = new Coding
    {
      Code = "ImagingStudy",
      Display = "ImagingStudy",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Immunization event information.
    /// </summary>
    public static readonly Coding Immunization = new Coding
    {
      Code = "Immunization",
      Display = "Immunization",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Results of a measure evaluation.
    /// </summary>
    public static readonly Coding MeasureReport = new Coding
    {
      Code = "MeasureReport",
      Display = "MeasureReport",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A photo, video, or audio recording acquired or used in healthcare. The actual content may be inline or provided by direct reference.
    /// </summary>
    public static readonly Coding Media = new Coding
    {
      Code = "Media",
      Display = "Media",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Administration of medication to a patient.
    /// </summary>
    public static readonly Coding MedicationAdministration = new Coding
    {
      Code = "MedicationAdministration",
      Display = "MedicationAdministration",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Dispensing a medication to a named patient.
    /// </summary>
    public static readonly Coding MedicationDispense = new Coding
    {
      Code = "MedicationDispense",
      Display = "MedicationDispense",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Record of medication being taken by a patient.
    /// </summary>
    public static readonly Coding MedicationStatement = new Coding
    {
      Code = "MedicationStatement",
      Display = "MedicationStatement",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Measurements and simple assertions.
    /// </summary>
    public static readonly Coding Observation = new Coding
    {
      Code = "Observation",
      Display = "Observation",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// PaymentNotice request.
    /// </summary>
    public static readonly Coding PaymentNotice = new Coding
    {
      Code = "PaymentNotice",
      Display = "PaymentNotice",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// PaymentReconciliation resource.
    /// </summary>
    public static readonly Coding PaymentReconciliation = new Coding
    {
      Code = "PaymentReconciliation",
      Display = "PaymentReconciliation",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// An action that is being or was performed on a patient.
    /// </summary>
    public static readonly Coding Procedure = new Coding
    {
      Code = "Procedure",
      Display = "Procedure",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// ProcessResponse resource.
    /// </summary>
    public static readonly Coding ProcessResponse = new Coding
    {
      Code = "ProcessResponse",
      Display = "ProcessResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A structured set of questions and their answers.
    /// </summary>
    public static readonly Coding QuestionnaireResponse = new Coding
    {
      Code = "QuestionnaireResponse",
      Display = "QuestionnaireResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Potential outcomes for a subject with likelihood.
    /// </summary>
    public static readonly Coding RiskAssessment = new Coding
    {
      Code = "RiskAssessment",
      Display = "RiskAssessment",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Delivery of bulk Supplies.
    /// </summary>
    public static readonly Coding SupplyDelivery = new Coding
    {
      Code = "SupplyDelivery",
      Display = "SupplyDelivery",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A task to be performed.
    /// </summary>
    public static readonly Coding Task = new Coding
    {
      Code = "Task",
      Display = "Task",
      System = "http://hl7.org/fhir/event-resource-types"
    };

    /// <summary>
    /// Literal for code: ChargeItem
    /// </summary>
    public const string LiteralChargeItem = "ChargeItem";

    /// <summary>
    /// Literal for code: EventResourceTypesChargeItem
    /// </summary>
    public const string LiteralEventResourceTypesChargeItem = "http://hl7.org/fhir/event-resource-types#ChargeItem";

    /// <summary>
    /// Literal for code: ClaimResponse
    /// </summary>
    public const string LiteralClaimResponse = "ClaimResponse";

    /// <summary>
    /// Literal for code: EventResourceTypesClaimResponse
    /// </summary>
    public const string LiteralEventResourceTypesClaimResponse = "http://hl7.org/fhir/event-resource-types#ClaimResponse";

    /// <summary>
    /// Literal for code: ClinicalImpression
    /// </summary>
    public const string LiteralClinicalImpression = "ClinicalImpression";

    /// <summary>
    /// Literal for code: EventResourceTypesClinicalImpression
    /// </summary>
    public const string LiteralEventResourceTypesClinicalImpression = "http://hl7.org/fhir/event-resource-types#ClinicalImpression";

    /// <summary>
    /// Literal for code: Communication
    /// </summary>
    public const string LiteralCommunication = "Communication";

    /// <summary>
    /// Literal for code: EventResourceTypesCommunication
    /// </summary>
    public const string LiteralEventResourceTypesCommunication = "http://hl7.org/fhir/event-resource-types#Communication";

    /// <summary>
    /// Literal for code: Composition
    /// </summary>
    public const string LiteralComposition = "Composition";

    /// <summary>
    /// Literal for code: EventResourceTypesComposition
    /// </summary>
    public const string LiteralEventResourceTypesComposition = "http://hl7.org/fhir/event-resource-types#Composition";

    /// <summary>
    /// Literal for code: Condition
    /// </summary>
    public const string LiteralCondition = "Condition";

    /// <summary>
    /// Literal for code: EventResourceTypesCondition
    /// </summary>
    public const string LiteralEventResourceTypesCondition = "http://hl7.org/fhir/event-resource-types#Condition";

    /// <summary>
    /// Literal for code: Consent
    /// </summary>
    public const string LiteralConsent = "Consent";

    /// <summary>
    /// Literal for code: EventResourceTypesConsent
    /// </summary>
    public const string LiteralEventResourceTypesConsent = "http://hl7.org/fhir/event-resource-types#Consent";

    /// <summary>
    /// Literal for code: Coverage
    /// </summary>
    public const string LiteralCoverage = "Coverage";

    /// <summary>
    /// Literal for code: EventResourceTypesCoverage
    /// </summary>
    public const string LiteralEventResourceTypesCoverage = "http://hl7.org/fhir/event-resource-types#Coverage";

    /// <summary>
    /// Literal for code: DeviceUseStatement
    /// </summary>
    public const string LiteralDeviceUseStatement = "DeviceUseStatement";

    /// <summary>
    /// Literal for code: EventResourceTypesDeviceUseStatement
    /// </summary>
    public const string LiteralEventResourceTypesDeviceUseStatement = "http://hl7.org/fhir/event-resource-types#DeviceUseStatement";

    /// <summary>
    /// Literal for code: DiagnosticReport
    /// </summary>
    public const string LiteralDiagnosticReport = "DiagnosticReport";

    /// <summary>
    /// Literal for code: EventResourceTypesDiagnosticReport
    /// </summary>
    public const string LiteralEventResourceTypesDiagnosticReport = "http://hl7.org/fhir/event-resource-types#DiagnosticReport";

    /// <summary>
    /// Literal for code: DocumentManifest
    /// </summary>
    public const string LiteralDocumentManifest = "DocumentManifest";

    /// <summary>
    /// Literal for code: EventResourceTypesDocumentManifest
    /// </summary>
    public const string LiteralEventResourceTypesDocumentManifest = "http://hl7.org/fhir/event-resource-types#DocumentManifest";

    /// <summary>
    /// Literal for code: DocumentReference
    /// </summary>
    public const string LiteralDocumentReference = "DocumentReference";

    /// <summary>
    /// Literal for code: EventResourceTypesDocumentReference
    /// </summary>
    public const string LiteralEventResourceTypesDocumentReference = "http://hl7.org/fhir/event-resource-types#DocumentReference";

    /// <summary>
    /// Literal for code: Encounter
    /// </summary>
    public const string LiteralEncounter = "Encounter";

    /// <summary>
    /// Literal for code: EventResourceTypesEncounter
    /// </summary>
    public const string LiteralEventResourceTypesEncounter = "http://hl7.org/fhir/event-resource-types#Encounter";

    /// <summary>
    /// Literal for code: EnrollmentResponse
    /// </summary>
    public const string LiteralEnrollmentResponse = "EnrollmentResponse";

    /// <summary>
    /// Literal for code: EventResourceTypesEnrollmentResponse
    /// </summary>
    public const string LiteralEventResourceTypesEnrollmentResponse = "http://hl7.org/fhir/event-resource-types#EnrollmentResponse";

    /// <summary>
    /// Literal for code: EpisodeOfCare
    /// </summary>
    public const string LiteralEpisodeOfCare = "EpisodeOfCare";

    /// <summary>
    /// Literal for code: EventResourceTypesEpisodeOfCare
    /// </summary>
    public const string LiteralEventResourceTypesEpisodeOfCare = "http://hl7.org/fhir/event-resource-types#EpisodeOfCare";

    /// <summary>
    /// Literal for code: ExplanationOfBenefit
    /// </summary>
    public const string LiteralExplanationOfBenefit = "ExplanationOfBenefit";

    /// <summary>
    /// Literal for code: EventResourceTypesExplanationOfBenefit
    /// </summary>
    public const string LiteralEventResourceTypesExplanationOfBenefit = "http://hl7.org/fhir/event-resource-types#ExplanationOfBenefit";

    /// <summary>
    /// Literal for code: FamilyMemberHistory
    /// </summary>
    public const string LiteralFamilyMemberHistory = "FamilyMemberHistory";

    /// <summary>
    /// Literal for code: EventResourceTypesFamilyMemberHistory
    /// </summary>
    public const string LiteralEventResourceTypesFamilyMemberHistory = "http://hl7.org/fhir/event-resource-types#FamilyMemberHistory";

    /// <summary>
    /// Literal for code: GuidanceResponse
    /// </summary>
    public const string LiteralGuidanceResponse = "GuidanceResponse";

    /// <summary>
    /// Literal for code: EventResourceTypesGuidanceResponse
    /// </summary>
    public const string LiteralEventResourceTypesGuidanceResponse = "http://hl7.org/fhir/event-resource-types#GuidanceResponse";

    /// <summary>
    /// Literal for code: ImagingStudy
    /// </summary>
    public const string LiteralImagingStudy = "ImagingStudy";

    /// <summary>
    /// Literal for code: EventResourceTypesImagingStudy
    /// </summary>
    public const string LiteralEventResourceTypesImagingStudy = "http://hl7.org/fhir/event-resource-types#ImagingStudy";

    /// <summary>
    /// Literal for code: Immunization
    /// </summary>
    public const string LiteralImmunization = "Immunization";

    /// <summary>
    /// Literal for code: EventResourceTypesImmunization
    /// </summary>
    public const string LiteralEventResourceTypesImmunization = "http://hl7.org/fhir/event-resource-types#Immunization";

    /// <summary>
    /// Literal for code: MeasureReport
    /// </summary>
    public const string LiteralMeasureReport = "MeasureReport";

    /// <summary>
    /// Literal for code: EventResourceTypesMeasureReport
    /// </summary>
    public const string LiteralEventResourceTypesMeasureReport = "http://hl7.org/fhir/event-resource-types#MeasureReport";

    /// <summary>
    /// Literal for code: Media
    /// </summary>
    public const string LiteralMedia = "Media";

    /// <summary>
    /// Literal for code: EventResourceTypesMedia
    /// </summary>
    public const string LiteralEventResourceTypesMedia = "http://hl7.org/fhir/event-resource-types#Media";

    /// <summary>
    /// Literal for code: MedicationAdministration
    /// </summary>
    public const string LiteralMedicationAdministration = "MedicationAdministration";

    /// <summary>
    /// Literal for code: EventResourceTypesMedicationAdministration
    /// </summary>
    public const string LiteralEventResourceTypesMedicationAdministration = "http://hl7.org/fhir/event-resource-types#MedicationAdministration";

    /// <summary>
    /// Literal for code: MedicationDispense
    /// </summary>
    public const string LiteralMedicationDispense = "MedicationDispense";

    /// <summary>
    /// Literal for code: EventResourceTypesMedicationDispense
    /// </summary>
    public const string LiteralEventResourceTypesMedicationDispense = "http://hl7.org/fhir/event-resource-types#MedicationDispense";

    /// <summary>
    /// Literal for code: MedicationStatement
    /// </summary>
    public const string LiteralMedicationStatement = "MedicationStatement";

    /// <summary>
    /// Literal for code: EventResourceTypesMedicationStatement
    /// </summary>
    public const string LiteralEventResourceTypesMedicationStatement = "http://hl7.org/fhir/event-resource-types#MedicationStatement";

    /// <summary>
    /// Literal for code: Observation
    /// </summary>
    public const string LiteralObservation = "Observation";

    /// <summary>
    /// Literal for code: EventResourceTypesObservation
    /// </summary>
    public const string LiteralEventResourceTypesObservation = "http://hl7.org/fhir/event-resource-types#Observation";

    /// <summary>
    /// Literal for code: PaymentNotice
    /// </summary>
    public const string LiteralPaymentNotice = "PaymentNotice";

    /// <summary>
    /// Literal for code: EventResourceTypesPaymentNotice
    /// </summary>
    public const string LiteralEventResourceTypesPaymentNotice = "http://hl7.org/fhir/event-resource-types#PaymentNotice";

    /// <summary>
    /// Literal for code: PaymentReconciliation
    /// </summary>
    public const string LiteralPaymentReconciliation = "PaymentReconciliation";

    /// <summary>
    /// Literal for code: EventResourceTypesPaymentReconciliation
    /// </summary>
    public const string LiteralEventResourceTypesPaymentReconciliation = "http://hl7.org/fhir/event-resource-types#PaymentReconciliation";

    /// <summary>
    /// Literal for code: Procedure
    /// </summary>
    public const string LiteralProcedure = "Procedure";

    /// <summary>
    /// Literal for code: EventResourceTypesProcedure
    /// </summary>
    public const string LiteralEventResourceTypesProcedure = "http://hl7.org/fhir/event-resource-types#Procedure";

    /// <summary>
    /// Literal for code: ProcessResponse
    /// </summary>
    public const string LiteralProcessResponse = "ProcessResponse";

    /// <summary>
    /// Literal for code: EventResourceTypesProcessResponse
    /// </summary>
    public const string LiteralEventResourceTypesProcessResponse = "http://hl7.org/fhir/event-resource-types#ProcessResponse";

    /// <summary>
    /// Literal for code: QuestionnaireResponse
    /// </summary>
    public const string LiteralQuestionnaireResponse = "QuestionnaireResponse";

    /// <summary>
    /// Literal for code: EventResourceTypesQuestionnaireResponse
    /// </summary>
    public const string LiteralEventResourceTypesQuestionnaireResponse = "http://hl7.org/fhir/event-resource-types#QuestionnaireResponse";

    /// <summary>
    /// Literal for code: RiskAssessment
    /// </summary>
    public const string LiteralRiskAssessment = "RiskAssessment";

    /// <summary>
    /// Literal for code: EventResourceTypesRiskAssessment
    /// </summary>
    public const string LiteralEventResourceTypesRiskAssessment = "http://hl7.org/fhir/event-resource-types#RiskAssessment";

    /// <summary>
    /// Literal for code: SupplyDelivery
    /// </summary>
    public const string LiteralSupplyDelivery = "SupplyDelivery";

    /// <summary>
    /// Literal for code: EventResourceTypesSupplyDelivery
    /// </summary>
    public const string LiteralEventResourceTypesSupplyDelivery = "http://hl7.org/fhir/event-resource-types#SupplyDelivery";

    /// <summary>
    /// Literal for code: Task
    /// </summary>
    public const string LiteralTask = "Task";

    /// <summary>
    /// Literal for code: EventResourceTypesTask
    /// </summary>
    public const string LiteralEventResourceTypesTask = "http://hl7.org/fhir/event-resource-types#Task";

    /// <summary>
    /// Dictionary for looking up EventResourceTypes Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "ChargeItem", ChargeItem }, 
      { "http://hl7.org/fhir/event-resource-types#ChargeItem", ChargeItem }, 
      { "ClaimResponse", ClaimResponse }, 
      { "http://hl7.org/fhir/event-resource-types#ClaimResponse", ClaimResponse }, 
      { "ClinicalImpression", ClinicalImpression }, 
      { "http://hl7.org/fhir/event-resource-types#ClinicalImpression", ClinicalImpression }, 
      { "Communication", Communication }, 
      { "http://hl7.org/fhir/event-resource-types#Communication", Communication }, 
      { "Composition", Composition }, 
      { "http://hl7.org/fhir/event-resource-types#Composition", Composition }, 
      { "Condition", Condition }, 
      { "http://hl7.org/fhir/event-resource-types#Condition", Condition }, 
      { "Consent", Consent }, 
      { "http://hl7.org/fhir/event-resource-types#Consent", Consent }, 
      { "Coverage", Coverage }, 
      { "http://hl7.org/fhir/event-resource-types#Coverage", Coverage }, 
      { "DeviceUseStatement", DeviceUseStatement }, 
      { "http://hl7.org/fhir/event-resource-types#DeviceUseStatement", DeviceUseStatement }, 
      { "DiagnosticReport", DiagnosticReport }, 
      { "http://hl7.org/fhir/event-resource-types#DiagnosticReport", DiagnosticReport }, 
      { "DocumentManifest", DocumentManifest }, 
      { "http://hl7.org/fhir/event-resource-types#DocumentManifest", DocumentManifest }, 
      { "DocumentReference", DocumentReference }, 
      { "http://hl7.org/fhir/event-resource-types#DocumentReference", DocumentReference }, 
      { "Encounter", Encounter }, 
      { "http://hl7.org/fhir/event-resource-types#Encounter", Encounter }, 
      { "EnrollmentResponse", EnrollmentResponse }, 
      { "http://hl7.org/fhir/event-resource-types#EnrollmentResponse", EnrollmentResponse }, 
      { "EpisodeOfCare", EpisodeOfCare }, 
      { "http://hl7.org/fhir/event-resource-types#EpisodeOfCare", EpisodeOfCare }, 
      { "ExplanationOfBenefit", ExplanationOfBenefit }, 
      { "http://hl7.org/fhir/event-resource-types#ExplanationOfBenefit", ExplanationOfBenefit }, 
      { "FamilyMemberHistory", FamilyMemberHistory }, 
      { "http://hl7.org/fhir/event-resource-types#FamilyMemberHistory", FamilyMemberHistory }, 
      { "GuidanceResponse", GuidanceResponse }, 
      { "http://hl7.org/fhir/event-resource-types#GuidanceResponse", GuidanceResponse }, 
      { "ImagingStudy", ImagingStudy }, 
      { "http://hl7.org/fhir/event-resource-types#ImagingStudy", ImagingStudy }, 
      { "Immunization", Immunization }, 
      { "http://hl7.org/fhir/event-resource-types#Immunization", Immunization }, 
      { "MeasureReport", MeasureReport }, 
      { "http://hl7.org/fhir/event-resource-types#MeasureReport", MeasureReport }, 
      { "Media", Media }, 
      { "http://hl7.org/fhir/event-resource-types#Media", Media }, 
      { "MedicationAdministration", MedicationAdministration }, 
      { "http://hl7.org/fhir/event-resource-types#MedicationAdministration", MedicationAdministration }, 
      { "MedicationDispense", MedicationDispense }, 
      { "http://hl7.org/fhir/event-resource-types#MedicationDispense", MedicationDispense }, 
      { "MedicationStatement", MedicationStatement }, 
      { "http://hl7.org/fhir/event-resource-types#MedicationStatement", MedicationStatement }, 
      { "Observation", Observation }, 
      { "http://hl7.org/fhir/event-resource-types#Observation", Observation }, 
      { "PaymentNotice", PaymentNotice }, 
      { "http://hl7.org/fhir/event-resource-types#PaymentNotice", PaymentNotice }, 
      { "PaymentReconciliation", PaymentReconciliation }, 
      { "http://hl7.org/fhir/event-resource-types#PaymentReconciliation", PaymentReconciliation }, 
      { "Procedure", Procedure }, 
      { "http://hl7.org/fhir/event-resource-types#Procedure", Procedure }, 
      { "ProcessResponse", ProcessResponse }, 
      { "http://hl7.org/fhir/event-resource-types#ProcessResponse", ProcessResponse }, 
      { "QuestionnaireResponse", QuestionnaireResponse }, 
      { "http://hl7.org/fhir/event-resource-types#QuestionnaireResponse", QuestionnaireResponse }, 
      { "RiskAssessment", RiskAssessment }, 
      { "http://hl7.org/fhir/event-resource-types#RiskAssessment", RiskAssessment }, 
      { "SupplyDelivery", SupplyDelivery }, 
      { "http://hl7.org/fhir/event-resource-types#SupplyDelivery", SupplyDelivery }, 
      { "Task", Task }, 
      { "http://hl7.org/fhir/event-resource-types#Task", Task }, 
    };
  };
}
