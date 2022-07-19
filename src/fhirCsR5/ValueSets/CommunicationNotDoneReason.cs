// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Codes for the reason why a communication did not happen.
  /// </summary>
  public static class CommunicationNotDoneReasonCodes
  {
    /// <summary>
    /// The communication was not done due to a family objection.
    /// </summary>
    public static readonly Coding FamilyObjection = new Coding
    {
      Code = "family-objection",
      Display = "Family Objection",
      System = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason"
    };
    /// <summary>
    /// The communication was not done due to an invalid phone number.
    /// </summary>
    public static readonly Coding InvalidPhoneNumber = new Coding
    {
      Code = "invalid-phone-number",
      Display = "Invalid Phone Number",
      System = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason"
    };
    /// <summary>
    /// The communication was not done due to a patient objection.
    /// </summary>
    public static readonly Coding PatientObjection = new Coding
    {
      Code = "patient-objection",
      Display = "Patient Objection",
      System = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason"
    };
    /// <summary>
    /// The communication was not done due to the recipient being unavailable.
    /// </summary>
    public static readonly Coding RecipientUnavailable = new Coding
    {
      Code = "recipient-unavailable",
      Display = "Recipient Unavailable",
      System = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason"
    };
    /// <summary>
    /// The communication was not done due to a system error.
    /// </summary>
    public static readonly Coding SystemError = new Coding
    {
      Code = "system-error",
      Display = "System Error",
      System = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason"
    };
    /// <summary>
    /// The communication was not done due to an unknown reason.
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason"
    };

    /// <summary>
    /// Literal for code: FamilyObjection
    /// </summary>
    public const string LiteralFamilyObjection = "family-objection";

    /// <summary>
    /// Literal for code: CommunicationNotDoneReasonFamilyObjection
    /// </summary>
    public const string LiteralCommunicationNotDoneReasonFamilyObjection = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#family-objection";

    /// <summary>
    /// Literal for code: InvalidPhoneNumber
    /// </summary>
    public const string LiteralInvalidPhoneNumber = "invalid-phone-number";

    /// <summary>
    /// Literal for code: CommunicationNotDoneReasonInvalidPhoneNumber
    /// </summary>
    public const string LiteralCommunicationNotDoneReasonInvalidPhoneNumber = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#invalid-phone-number";

    /// <summary>
    /// Literal for code: PatientObjection
    /// </summary>
    public const string LiteralPatientObjection = "patient-objection";

    /// <summary>
    /// Literal for code: CommunicationNotDoneReasonPatientObjection
    /// </summary>
    public const string LiteralCommunicationNotDoneReasonPatientObjection = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#patient-objection";

    /// <summary>
    /// Literal for code: RecipientUnavailable
    /// </summary>
    public const string LiteralRecipientUnavailable = "recipient-unavailable";

    /// <summary>
    /// Literal for code: CommunicationNotDoneReasonRecipientUnavailable
    /// </summary>
    public const string LiteralCommunicationNotDoneReasonRecipientUnavailable = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#recipient-unavailable";

    /// <summary>
    /// Literal for code: SystemError
    /// </summary>
    public const string LiteralSystemError = "system-error";

    /// <summary>
    /// Literal for code: CommunicationNotDoneReasonSystemError
    /// </summary>
    public const string LiteralCommunicationNotDoneReasonSystemError = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#system-error";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";

    /// <summary>
    /// Literal for code: CommunicationNotDoneReasonUnknown
    /// </summary>
    public const string LiteralCommunicationNotDoneReasonUnknown = "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#unknown";

    /// <summary>
    /// Dictionary for looking up CommunicationNotDoneReason Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "family-objection", FamilyObjection }, 
      { "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#family-objection", FamilyObjection }, 
      { "invalid-phone-number", InvalidPhoneNumber }, 
      { "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#invalid-phone-number", InvalidPhoneNumber }, 
      { "patient-objection", PatientObjection }, 
      { "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#patient-objection", PatientObjection }, 
      { "recipient-unavailable", RecipientUnavailable }, 
      { "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#recipient-unavailable", RecipientUnavailable }, 
      { "system-error", SystemError }, 
      { "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#system-error", SystemError }, 
      { "unknown", Unknown }, 
      { "http://terminology.hl7.org/CodeSystem/communication-not-done-reason#unknown", Unknown }, 
    };
  };
}
