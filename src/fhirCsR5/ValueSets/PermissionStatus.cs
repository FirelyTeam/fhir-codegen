// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Codes identifying the lifecycle stage of a product.
  /// </summary>
  public static class PermissionStatusCodes
  {
    /// <summary>
    /// Permission is given.
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/permission-status"
    };
    /// <summary>
    /// Permission is being defined.
    /// </summary>
    public static readonly Coding Draft = new Coding
    {
      Code = "draft",
      Display = "Draft",
      System = "http://hl7.org/fhir/permission-status"
    };
    /// <summary>
    /// Permission was entered in error and is not active.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/permission-status"
    };
    /// <summary>
    /// Permission not granted.
    /// </summary>
    public static readonly Coding Rejected = new Coding
    {
      Code = "rejected",
      Display = "Rejected",
      System = "http://hl7.org/fhir/permission-status"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: PermissionStatusActive
    /// </summary>
    public const string LiteralPermissionStatusActive = "http://hl7.org/fhir/permission-status#active";

    /// <summary>
    /// Literal for code: Draft
    /// </summary>
    public const string LiteralDraft = "draft";

    /// <summary>
    /// Literal for code: PermissionStatusDraft
    /// </summary>
    public const string LiteralPermissionStatusDraft = "http://hl7.org/fhir/permission-status#draft";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: PermissionStatusEnteredInError
    /// </summary>
    public const string LiteralPermissionStatusEnteredInError = "http://hl7.org/fhir/permission-status#entered-in-error";

    /// <summary>
    /// Literal for code: Rejected
    /// </summary>
    public const string LiteralRejected = "rejected";

    /// <summary>
    /// Literal for code: PermissionStatusRejected
    /// </summary>
    public const string LiteralPermissionStatusRejected = "http://hl7.org/fhir/permission-status#rejected";

    /// <summary>
    /// Dictionary for looking up PermissionStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "active", Active }, 
      { "http://hl7.org/fhir/permission-status#active", Active }, 
      { "draft", Draft }, 
      { "http://hl7.org/fhir/permission-status#draft", Draft }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/permission-status#entered-in-error", EnteredInError }, 
      { "rejected", Rejected }, 
      { "http://hl7.org/fhir/permission-status#rejected", Rejected }, 
    };
  };
}