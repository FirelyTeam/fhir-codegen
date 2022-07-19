// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// The type of action to be performed
  /// </summary>
  public static class ActionTypeCodes
  {
    /// <summary>
    /// The action is to create a new resource
    /// </summary>
    public static readonly Coding Create = new Coding
    {
      Code = "create",
      Display = "Create",
      System = "http://hl7.org/fhir/action-type"
    };
    /// <summary>
    /// The action is to fire a specific event
    /// </summary>
    public static readonly Coding FireEvent = new Coding
    {
      Code = "fire-event",
      Display = "Fire Event",
      System = "http://hl7.org/fhir/action-type"
    };
    /// <summary>
    /// The action is to remove an existing resource
    /// </summary>
    public static readonly Coding Remove = new Coding
    {
      Code = "remove",
      Display = "Remove",
      System = "http://hl7.org/fhir/action-type"
    };
    /// <summary>
    /// The action is to update an existing resource
    /// </summary>
    public static readonly Coding Update = new Coding
    {
      Code = "update",
      Display = "Update",
      System = "http://hl7.org/fhir/action-type"
    };

    /// <summary>
    /// Literal for code: Create
    /// </summary>
    public const string LiteralCreate = "create";

    /// <summary>
    /// Literal for code: ActionTypeCreate
    /// </summary>
    public const string LiteralActionTypeCreate = "http://hl7.org/fhir/action-type#create";

    /// <summary>
    /// Literal for code: FireEvent
    /// </summary>
    public const string LiteralFireEvent = "fire-event";

    /// <summary>
    /// Literal for code: ActionTypeFireEvent
    /// </summary>
    public const string LiteralActionTypeFireEvent = "http://hl7.org/fhir/action-type#fire-event";

    /// <summary>
    /// Literal for code: Remove
    /// </summary>
    public const string LiteralRemove = "remove";

    /// <summary>
    /// Literal for code: ActionTypeRemove
    /// </summary>
    public const string LiteralActionTypeRemove = "http://hl7.org/fhir/action-type#remove";

    /// <summary>
    /// Literal for code: Update
    /// </summary>
    public const string LiteralUpdate = "update";

    /// <summary>
    /// Literal for code: ActionTypeUpdate
    /// </summary>
    public const string LiteralActionTypeUpdate = "http://hl7.org/fhir/action-type#update";

    /// <summary>
    /// Dictionary for looking up ActionType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "create", Create }, 
      { "http://hl7.org/fhir/action-type#create", Create }, 
      { "fire-event", FireEvent }, 
      { "http://hl7.org/fhir/action-type#fire-event", FireEvent }, 
      { "remove", Remove }, 
      { "http://hl7.org/fhir/action-type#remove", Remove }, 
      { "update", Update }, 
      { "http://hl7.org/fhir/action-type#update", Update }, 
    };
  };
}
