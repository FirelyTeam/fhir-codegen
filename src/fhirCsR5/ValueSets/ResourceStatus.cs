// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The master set of status codes used throughout FHIR. All status codes are mapped to one of these codes.
  /// </summary>
  public static class ResourceStatusCodes
  {
    /// <summary>
    /// The process described/requested in the resource did not complete - usually due to some workflow error, and no further action is planned
    /// </summary>
    public static readonly Coding Abandoned = new Coding
    {
      Code = "abandoned",
      Display = "abandoned",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The fulfiller has decided to perform the action, and plans are in train to do this in the future
    /// </summary>
    public static readonly Coding Accepted = new Coding
    {
      Code = "accepted",
      Display = "accepted",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The resource describes information that is currently valid or a process that is presently occuring
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "active",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// Ahead of the planned timelines
    /// </summary>
    public static readonly Coding AheadOfTarget = new Coding
    {
      Code = "ahead-of-target",
      Display = "ahead-of-target",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The pre-conditions for the action are all fulfilled, and it is imminent
    /// </summary>
    public static readonly Coding Arrived = new Coding
    {
      Code = "arrived",
      Display = "arrived",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// Behind the planned timelines
    /// </summary>
    public static readonly Coding BehindTarget = new Coding
    {
      Code = "behind-target",
      Display = "behind-target",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// not available at this time/location
    /// </summary>
    public static readonly Coding BusyUnavailable = new Coding
    {
      Code = "busy-unavailable",
      Display = "busy-unavailable",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The process described/requested in the resource has been completed, and no further action is planned
    /// </summary>
    public static readonly Coding Complete = new Coding
    {
      Code = "complete",
      Display = "complete",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The information in this resource is approved
    /// </summary>
    public static readonly Coding Confirmed = new Coding
    {
      Code = "confirmed",
      Display = "confirmed",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The fulfiller chose not to perform the action
    /// </summary>
    public static readonly Coding Declined = new Coding
    {
      Code = "declined",
      Display = "declined",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// Potentially true?
    /// </summary>
    public static readonly Coding Differential = new Coding
    {
      Code = "differential",
      Display = "differential",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The information in the resource is still being prepared and edited
    /// </summary>
    public static readonly Coding Draft = new Coding
    {
      Code = "draft",
      Display = "draft",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The resource was created in error, and should not be treated as valid (note: in many cases, for various data integrity related reasons, the information cannot be removed from the record)
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "error",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The process described/requested in the resource could not be completed, and no further action is planned
    /// </summary>
    public static readonly Coding Failed = new Coding
    {
      Code = "failed",
      Display = "failed",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// Free for scheduling
    /// </summary>
    public static readonly Coding Free = new Coding
    {
      Code = "free",
      Display = "free",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The hardware is disconnected
    /// </summary>
    public static readonly Coding HwDiscon = new Coding
    {
      Code = "hw-discon",
      Display = "hw-discon",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The resource describes information that is no longer valid or a process that is stopped occurring
    /// </summary>
    public static readonly Coding Inactive = new Coding
    {
      Code = "inactive",
      Display = "inactive",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// Not ready to act
    /// </summary>
    public static readonly Coding NotReady = new Coding
    {
      Code = "not-ready",
      Display = "not-ready",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// Ready to act
    /// </summary>
    public static readonly Coding OnTarget = new Coding
    {
      Code = "on-target",
      Display = "on-target",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// This information is still being assembled
    /// </summary>
    public static readonly Coding Partial = new Coding
    {
      Code = "partial",
      Display = "partial",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The resource describes a course of action that is planned and agreed/approved, but at the time of recording was still future
    /// </summary>
    public static readonly Coding Planned = new Coding
    {
      Code = "planned",
      Display = "planned",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The resource describes an action or plan that is proposed, and not yet approved by the participants
    /// </summary>
    public static readonly Coding Proposed = new Coding
    {
      Code = "proposed",
      Display = "proposed",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The fulfiller has received the request, but not yet agreed to carry out the action
    /// </summary>
    public static readonly Coding Received = new Coding
    {
      Code = "received",
      Display = "received",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// This information has been ruled out by testing and evaluation
    /// </summary>
    public static readonly Coding Refuted = new Coding
    {
      Code = "refuted",
      Display = "refuted",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The information in this resource has been replaced by information in another resource
    /// </summary>
    public static readonly Coding Replaced = new Coding
    {
      Code = "replaced",
      Display = "replaced",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// A fulfiller has been asked to perform this action, but it has not yet occurred
    /// </summary>
    public static readonly Coding Requested = new Coding
    {
      Code = "requested",
      Display = "requested",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The issue identified by this resource is no longer of concern
    /// </summary>
    public static readonly Coding Resolved = new Coding
    {
      Code = "resolved",
      Display = "resolved",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The process described/requested in this resource has been halted for some reason
    /// </summary>
    public static readonly Coding Suspended = new Coding
    {
      Code = "suspended",
      Display = "suspended",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The device transducer is disconnected
    /// </summary>
    public static readonly Coding TransducDiscon = new Coding
    {
      Code = "transduc-discon",
      Display = "transduc-discon",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// The information in this resource is not yet approved
    /// </summary>
    public static readonly Coding Unconfirmed = new Coding
    {
      Code = "unconfirmed",
      Display = "unconfirmed",
      System = "http://hl7.org/fhir/resource-status"
    };
    /// <summary>
    /// Authoring system does not know the status
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "unknown",
      System = "http://hl7.org/fhir/resource-status"
    };

    /// <summary>
    /// Literal for code: Abandoned
    /// </summary>
    public const string LiteralAbandoned = "abandoned";

    /// <summary>
    /// Literal for code: ResourceStatusAbandoned
    /// </summary>
    public const string LiteralResourceStatusAbandoned = "http://hl7.org/fhir/resource-status#abandoned";

    /// <summary>
    /// Literal for code: Accepted
    /// </summary>
    public const string LiteralAccepted = "accepted";

    /// <summary>
    /// Literal for code: ResourceStatusAccepted
    /// </summary>
    public const string LiteralResourceStatusAccepted = "http://hl7.org/fhir/resource-status#accepted";

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: ResourceStatusActive
    /// </summary>
    public const string LiteralResourceStatusActive = "http://hl7.org/fhir/resource-status#active";

    /// <summary>
    /// Literal for code: AheadOfTarget
    /// </summary>
    public const string LiteralAheadOfTarget = "ahead-of-target";

    /// <summary>
    /// Literal for code: ResourceStatusAheadOfTarget
    /// </summary>
    public const string LiteralResourceStatusAheadOfTarget = "http://hl7.org/fhir/resource-status#ahead-of-target";

    /// <summary>
    /// Literal for code: Arrived
    /// </summary>
    public const string LiteralArrived = "arrived";

    /// <summary>
    /// Literal for code: ResourceStatusArrived
    /// </summary>
    public const string LiteralResourceStatusArrived = "http://hl7.org/fhir/resource-status#arrived";

    /// <summary>
    /// Literal for code: BehindTarget
    /// </summary>
    public const string LiteralBehindTarget = "behind-target";

    /// <summary>
    /// Literal for code: ResourceStatusBehindTarget
    /// </summary>
    public const string LiteralResourceStatusBehindTarget = "http://hl7.org/fhir/resource-status#behind-target";

    /// <summary>
    /// Literal for code: BusyUnavailable
    /// </summary>
    public const string LiteralBusyUnavailable = "busy-unavailable";

    /// <summary>
    /// Literal for code: ResourceStatusBusyUnavailable
    /// </summary>
    public const string LiteralResourceStatusBusyUnavailable = "http://hl7.org/fhir/resource-status#busy-unavailable";

    /// <summary>
    /// Literal for code: Complete
    /// </summary>
    public const string LiteralComplete = "complete";

    /// <summary>
    /// Literal for code: ResourceStatusComplete
    /// </summary>
    public const string LiteralResourceStatusComplete = "http://hl7.org/fhir/resource-status#complete";

    /// <summary>
    /// Literal for code: Confirmed
    /// </summary>
    public const string LiteralConfirmed = "confirmed";

    /// <summary>
    /// Literal for code: ResourceStatusConfirmed
    /// </summary>
    public const string LiteralResourceStatusConfirmed = "http://hl7.org/fhir/resource-status#confirmed";

    /// <summary>
    /// Literal for code: Declined
    /// </summary>
    public const string LiteralDeclined = "declined";

    /// <summary>
    /// Literal for code: ResourceStatusDeclined
    /// </summary>
    public const string LiteralResourceStatusDeclined = "http://hl7.org/fhir/resource-status#declined";

    /// <summary>
    /// Literal for code: Differential
    /// </summary>
    public const string LiteralDifferential = "differential";

    /// <summary>
    /// Literal for code: ResourceStatusDifferential
    /// </summary>
    public const string LiteralResourceStatusDifferential = "http://hl7.org/fhir/resource-status#differential";

    /// <summary>
    /// Literal for code: Draft
    /// </summary>
    public const string LiteralDraft = "draft";

    /// <summary>
    /// Literal for code: ResourceStatusDraft
    /// </summary>
    public const string LiteralResourceStatusDraft = "http://hl7.org/fhir/resource-status#draft";

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: ResourceStatusError
    /// </summary>
    public const string LiteralResourceStatusError = "http://hl7.org/fhir/resource-status#error";

    /// <summary>
    /// Literal for code: Failed
    /// </summary>
    public const string LiteralFailed = "failed";

    /// <summary>
    /// Literal for code: ResourceStatusFailed
    /// </summary>
    public const string LiteralResourceStatusFailed = "http://hl7.org/fhir/resource-status#failed";

    /// <summary>
    /// Literal for code: Free
    /// </summary>
    public const string LiteralFree = "free";

    /// <summary>
    /// Literal for code: ResourceStatusFree
    /// </summary>
    public const string LiteralResourceStatusFree = "http://hl7.org/fhir/resource-status#free";

    /// <summary>
    /// Literal for code: HwDiscon
    /// </summary>
    public const string LiteralHwDiscon = "hw-discon";

    /// <summary>
    /// Literal for code: ResourceStatusHwDiscon
    /// </summary>
    public const string LiteralResourceStatusHwDiscon = "http://hl7.org/fhir/resource-status#hw-discon";

    /// <summary>
    /// Literal for code: Inactive
    /// </summary>
    public const string LiteralInactive = "inactive";

    /// <summary>
    /// Literal for code: ResourceStatusInactive
    /// </summary>
    public const string LiteralResourceStatusInactive = "http://hl7.org/fhir/resource-status#inactive";

    /// <summary>
    /// Literal for code: NotReady
    /// </summary>
    public const string LiteralNotReady = "not-ready";

    /// <summary>
    /// Literal for code: ResourceStatusNotReady
    /// </summary>
    public const string LiteralResourceStatusNotReady = "http://hl7.org/fhir/resource-status#not-ready";

    /// <summary>
    /// Literal for code: OnTarget
    /// </summary>
    public const string LiteralOnTarget = "on-target";

    /// <summary>
    /// Literal for code: ResourceStatusOnTarget
    /// </summary>
    public const string LiteralResourceStatusOnTarget = "http://hl7.org/fhir/resource-status#on-target";

    /// <summary>
    /// Literal for code: Partial
    /// </summary>
    public const string LiteralPartial = "partial";

    /// <summary>
    /// Literal for code: ResourceStatusPartial
    /// </summary>
    public const string LiteralResourceStatusPartial = "http://hl7.org/fhir/resource-status#partial";

    /// <summary>
    /// Literal for code: Planned
    /// </summary>
    public const string LiteralPlanned = "planned";

    /// <summary>
    /// Literal for code: ResourceStatusPlanned
    /// </summary>
    public const string LiteralResourceStatusPlanned = "http://hl7.org/fhir/resource-status#planned";

    /// <summary>
    /// Literal for code: Proposed
    /// </summary>
    public const string LiteralProposed = "proposed";

    /// <summary>
    /// Literal for code: ResourceStatusProposed
    /// </summary>
    public const string LiteralResourceStatusProposed = "http://hl7.org/fhir/resource-status#proposed";

    /// <summary>
    /// Literal for code: Received
    /// </summary>
    public const string LiteralReceived = "received";

    /// <summary>
    /// Literal for code: ResourceStatusReceived
    /// </summary>
    public const string LiteralResourceStatusReceived = "http://hl7.org/fhir/resource-status#received";

    /// <summary>
    /// Literal for code: Refuted
    /// </summary>
    public const string LiteralRefuted = "refuted";

    /// <summary>
    /// Literal for code: ResourceStatusRefuted
    /// </summary>
    public const string LiteralResourceStatusRefuted = "http://hl7.org/fhir/resource-status#refuted";

    /// <summary>
    /// Literal for code: Replaced
    /// </summary>
    public const string LiteralReplaced = "replaced";

    /// <summary>
    /// Literal for code: ResourceStatusReplaced
    /// </summary>
    public const string LiteralResourceStatusReplaced = "http://hl7.org/fhir/resource-status#replaced";

    /// <summary>
    /// Literal for code: Requested
    /// </summary>
    public const string LiteralRequested = "requested";

    /// <summary>
    /// Literal for code: ResourceStatusRequested
    /// </summary>
    public const string LiteralResourceStatusRequested = "http://hl7.org/fhir/resource-status#requested";

    /// <summary>
    /// Literal for code: Resolved
    /// </summary>
    public const string LiteralResolved = "resolved";

    /// <summary>
    /// Literal for code: ResourceStatusResolved
    /// </summary>
    public const string LiteralResourceStatusResolved = "http://hl7.org/fhir/resource-status#resolved";

    /// <summary>
    /// Literal for code: Suspended
    /// </summary>
    public const string LiteralSuspended = "suspended";

    /// <summary>
    /// Literal for code: ResourceStatusSuspended
    /// </summary>
    public const string LiteralResourceStatusSuspended = "http://hl7.org/fhir/resource-status#suspended";

    /// <summary>
    /// Literal for code: TransducDiscon
    /// </summary>
    public const string LiteralTransducDiscon = "transduc-discon";

    /// <summary>
    /// Literal for code: ResourceStatusTransducDiscon
    /// </summary>
    public const string LiteralResourceStatusTransducDiscon = "http://hl7.org/fhir/resource-status#transduc-discon";

    /// <summary>
    /// Literal for code: Unconfirmed
    /// </summary>
    public const string LiteralUnconfirmed = "unconfirmed";

    /// <summary>
    /// Literal for code: ResourceStatusUnconfirmed
    /// </summary>
    public const string LiteralResourceStatusUnconfirmed = "http://hl7.org/fhir/resource-status#unconfirmed";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";

    /// <summary>
    /// Literal for code: ResourceStatusUnknown
    /// </summary>
    public const string LiteralResourceStatusUnknown = "http://hl7.org/fhir/resource-status#unknown";

    /// <summary>
    /// Dictionary for looking up ResourceStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "abandoned", Abandoned }, 
      { "http://hl7.org/fhir/resource-status#abandoned", Abandoned }, 
      { "accepted", Accepted }, 
      { "http://hl7.org/fhir/resource-status#accepted", Accepted }, 
      { "active", Active }, 
      { "http://hl7.org/fhir/resource-status#active", Active }, 
      { "ahead-of-target", AheadOfTarget }, 
      { "http://hl7.org/fhir/resource-status#ahead-of-target", AheadOfTarget }, 
      { "arrived", Arrived }, 
      { "http://hl7.org/fhir/resource-status#arrived", Arrived }, 
      { "behind-target", BehindTarget }, 
      { "http://hl7.org/fhir/resource-status#behind-target", BehindTarget }, 
      { "busy-unavailable", BusyUnavailable }, 
      { "http://hl7.org/fhir/resource-status#busy-unavailable", BusyUnavailable }, 
      { "complete", Complete }, 
      { "http://hl7.org/fhir/resource-status#complete", Complete }, 
      { "confirmed", Confirmed }, 
      { "http://hl7.org/fhir/resource-status#confirmed", Confirmed }, 
      { "declined", Declined }, 
      { "http://hl7.org/fhir/resource-status#declined", Declined }, 
      { "differential", Differential }, 
      { "http://hl7.org/fhir/resource-status#differential", Differential }, 
      { "draft", Draft }, 
      { "http://hl7.org/fhir/resource-status#draft", Draft }, 
      { "error", Error }, 
      { "http://hl7.org/fhir/resource-status#error", Error }, 
      { "failed", Failed }, 
      { "http://hl7.org/fhir/resource-status#failed", Failed }, 
      { "free", Free }, 
      { "http://hl7.org/fhir/resource-status#free", Free }, 
      { "hw-discon", HwDiscon }, 
      { "http://hl7.org/fhir/resource-status#hw-discon", HwDiscon }, 
      { "inactive", Inactive }, 
      { "http://hl7.org/fhir/resource-status#inactive", Inactive }, 
      { "not-ready", NotReady }, 
      { "http://hl7.org/fhir/resource-status#not-ready", NotReady }, 
      { "on-target", OnTarget }, 
      { "http://hl7.org/fhir/resource-status#on-target", OnTarget }, 
      { "partial", Partial }, 
      { "http://hl7.org/fhir/resource-status#partial", Partial }, 
      { "planned", Planned }, 
      { "http://hl7.org/fhir/resource-status#planned", Planned }, 
      { "proposed", Proposed }, 
      { "http://hl7.org/fhir/resource-status#proposed", Proposed }, 
      { "received", Received }, 
      { "http://hl7.org/fhir/resource-status#received", Received }, 
      { "refuted", Refuted }, 
      { "http://hl7.org/fhir/resource-status#refuted", Refuted }, 
      { "replaced", Replaced }, 
      { "http://hl7.org/fhir/resource-status#replaced", Replaced }, 
      { "requested", Requested }, 
      { "http://hl7.org/fhir/resource-status#requested", Requested }, 
      { "resolved", Resolved }, 
      { "http://hl7.org/fhir/resource-status#resolved", Resolved }, 
      { "suspended", Suspended }, 
      { "http://hl7.org/fhir/resource-status#suspended", Suspended }, 
      { "transduc-discon", TransducDiscon }, 
      { "http://hl7.org/fhir/resource-status#transduc-discon", TransducDiscon }, 
      { "unconfirmed", Unconfirmed }, 
      { "http://hl7.org/fhir/resource-status#unconfirmed", Unconfirmed }, 
      { "unknown", Unknown }, 
      { "http://hl7.org/fhir/resource-status#unknown", Unknown }, 
    };
  };
}
