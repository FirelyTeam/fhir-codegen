// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Codes indicating the degree of authority/intentionality associated with a request.
  /// </summary>
  public static class RequestIntentCodes
  {
    /// <summary>
    /// The request represents a legally binding instruction authored by a Patient or RelatedPerson.
    /// </summary>
    public static readonly Coding Directive = new Coding
    {
      Code = "directive",
      Display = "Directive",
      System = "http://hl7.org/fhir/request-intent"
    };
    /// <summary>
    /// The request represents the view of an authorization instantiated by a fulfilling system representing the details of the fulfiller's intention to act upon a submitted order.
    /// </summary>
    public static readonly Coding FillerOrder = new Coding
    {
      Code = "filler-order",
      Display = "Filler Order",
      System = "http://hl7.org/fhir/request-intent"
    };
    /// <summary>
    /// An order created in fulfillment of a broader order that represents the authorization for a single activity occurrence.  E.g. The administration of a single dose of a drug.
    /// </summary>
    public static readonly Coding InstanceOrder = new Coding
    {
      Code = "instance-order",
      Display = "Instance Order",
      System = "http://hl7.org/fhir/request-intent"
    };
    /// <summary>
    /// The request represents a component or option for a RequestGroup that establishes timing, conditionality and/or other constraints among a set of requests.  Refer to [[[RequestGroup]]] for additional information on how this status is used.
    /// </summary>
    public static readonly Coding Option = new Coding
    {
      Code = "option",
      Display = "Option",
      System = "http://hl7.org/fhir/request-intent"
    };
    /// <summary>
    /// The request represents a request/demand and authorization for action by a Practitioner.
    /// </summary>
    public static readonly Coding Order = new Coding
    {
      Code = "order",
      Display = "Order",
      System = "http://hl7.org/fhir/request-intent"
    };
    /// <summary>
    /// The request represents an original authorization for action.
    /// </summary>
    public static readonly Coding OriginalOrder = new Coding
    {
      Code = "original-order",
      Display = "Original Order",
      System = "http://hl7.org/fhir/request-intent"
    };
    /// <summary>
    /// The request represents an intention to ensure something occurs without providing an authorization for others to act.
    /// </summary>
    public static readonly Coding Plan = new Coding
    {
      Code = "plan",
      Display = "Plan",
      System = "http://hl7.org/fhir/request-intent"
    };
    /// <summary>
    /// The request is a suggestion made by someone/something that does not have an intention to ensure it occurs and without providing an authorization to act.
    /// </summary>
    public static readonly Coding Proposal = new Coding
    {
      Code = "proposal",
      Display = "Proposal",
      System = "http://hl7.org/fhir/request-intent"
    };
    /// <summary>
    /// The request represents an automatically generated supplemental authorization for action based on a parent authorization together with initial results of the action taken against that parent authorization.
    /// </summary>
    public static readonly Coding ReflexOrder = new Coding
    {
      Code = "reflex-order",
      Display = "Reflex Order",
      System = "http://hl7.org/fhir/request-intent"
    };

    /// <summary>
    /// Literal for code: Directive
    /// </summary>
    public const string LiteralDirective = "directive";

    /// <summary>
    /// Literal for code: RequestIntentDirective
    /// </summary>
    public const string LiteralRequestIntentDirective = "http://hl7.org/fhir/request-intent#directive";

    /// <summary>
    /// Literal for code: FillerOrder
    /// </summary>
    public const string LiteralFillerOrder = "filler-order";

    /// <summary>
    /// Literal for code: RequestIntentFillerOrder
    /// </summary>
    public const string LiteralRequestIntentFillerOrder = "http://hl7.org/fhir/request-intent#filler-order";

    /// <summary>
    /// Literal for code: InstanceOrder
    /// </summary>
    public const string LiteralInstanceOrder = "instance-order";

    /// <summary>
    /// Literal for code: RequestIntentInstanceOrder
    /// </summary>
    public const string LiteralRequestIntentInstanceOrder = "http://hl7.org/fhir/request-intent#instance-order";

    /// <summary>
    /// Literal for code: Option
    /// </summary>
    public const string LiteralOption = "option";

    /// <summary>
    /// Literal for code: RequestIntentOption
    /// </summary>
    public const string LiteralRequestIntentOption = "http://hl7.org/fhir/request-intent#option";

    /// <summary>
    /// Literal for code: Order
    /// </summary>
    public const string LiteralOrder = "order";

    /// <summary>
    /// Literal for code: RequestIntentOrder
    /// </summary>
    public const string LiteralRequestIntentOrder = "http://hl7.org/fhir/request-intent#order";

    /// <summary>
    /// Literal for code: OriginalOrder
    /// </summary>
    public const string LiteralOriginalOrder = "original-order";

    /// <summary>
    /// Literal for code: RequestIntentOriginalOrder
    /// </summary>
    public const string LiteralRequestIntentOriginalOrder = "http://hl7.org/fhir/request-intent#original-order";

    /// <summary>
    /// Literal for code: Plan
    /// </summary>
    public const string LiteralPlan = "plan";

    /// <summary>
    /// Literal for code: RequestIntentPlan
    /// </summary>
    public const string LiteralRequestIntentPlan = "http://hl7.org/fhir/request-intent#plan";

    /// <summary>
    /// Literal for code: Proposal
    /// </summary>
    public const string LiteralProposal = "proposal";

    /// <summary>
    /// Literal for code: RequestIntentProposal
    /// </summary>
    public const string LiteralRequestIntentProposal = "http://hl7.org/fhir/request-intent#proposal";

    /// <summary>
    /// Literal for code: ReflexOrder
    /// </summary>
    public const string LiteralReflexOrder = "reflex-order";

    /// <summary>
    /// Literal for code: RequestIntentReflexOrder
    /// </summary>
    public const string LiteralRequestIntentReflexOrder = "http://hl7.org/fhir/request-intent#reflex-order";

    /// <summary>
    /// Dictionary for looking up RequestIntent Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "directive", Directive }, 
      { "http://hl7.org/fhir/request-intent#directive", Directive }, 
      { "filler-order", FillerOrder }, 
      { "http://hl7.org/fhir/request-intent#filler-order", FillerOrder }, 
      { "instance-order", InstanceOrder }, 
      { "http://hl7.org/fhir/request-intent#instance-order", InstanceOrder }, 
      { "option", Option }, 
      { "http://hl7.org/fhir/request-intent#option", Option }, 
      { "order", Order }, 
      { "http://hl7.org/fhir/request-intent#order", Order }, 
      { "original-order", OriginalOrder }, 
      { "http://hl7.org/fhir/request-intent#original-order", OriginalOrder }, 
      { "plan", Plan }, 
      { "http://hl7.org/fhir/request-intent#plan", Plan }, 
      { "proposal", Proposal }, 
      { "http://hl7.org/fhir/request-intent#proposal", Proposal }, 
      { "reflex-order", ReflexOrder }, 
      { "http://hl7.org/fhir/request-intent#reflex-order", ReflexOrder }, 
    };
  };
}