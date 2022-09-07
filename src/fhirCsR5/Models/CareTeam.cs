// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Xml;
using fhirCsR5.Serialization;

namespace fhirCsR5.Models
{
  /// <summary>
  /// Identifies all people and organizations who are expected to be involved in the care team.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<CareTeamParticipant>))]
  public class CareTeamParticipant : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// This is populated while creating / managing the CareTeam to ensure there is coverage when servicing CarePlan activities from the Schedule.
    /// </summary>
    public Period CoveragePeriod { get; set; }
    /// <summary>
    /// This is populated while creating / managing the CareTeam to ensure there is coverage when servicing CarePlan activities from the Schedule.
    /// </summary>
    public Timing CoverageTiming { get; set; }
    /// <summary>
    /// Patient only needs to be listed if they have a role other than "subject of care".
    /// Member is optional because some participants may be known only by their role, particularly in draft plans.
    /// </summary>
    public Reference Member { get; set; }
    /// <summary>
    /// The organization of the practitioner.
    /// </summary>
    public Reference OnBehalfOf { get; set; }
    /// <summary>
    /// Roles may sometimes be inferred by type of Practitioner.  These are relationships that hold only within the context of the care team.  General relationships should be handled as properties of the Patient resource directly.  If a participant has multiple roles within the team, then there should be multiple participants.
    /// </summary>
    public CodeableConcept Role { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Role != null)
      {
        writer.WritePropertyName("role");
        Role.SerializeJson(writer, options);
      }

      if (Member != null)
      {
        writer.WritePropertyName("member");
        Member.SerializeJson(writer, options);
      }

      if (OnBehalfOf != null)
      {
        writer.WritePropertyName("onBehalfOf");
        OnBehalfOf.SerializeJson(writer, options);
      }

      if (CoveragePeriod != null)
      {
        writer.WritePropertyName("coveragePeriod");
        CoveragePeriod.SerializeJson(writer, options);
      }

      if (CoverageTiming != null)
      {
        writer.WritePropertyName("coverageTiming");
        CoverageTiming.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "coveragePeriod":
          CoveragePeriod = new fhirCsR5.Models.Period();
          CoveragePeriod.DeserializeJson(ref reader, options);
          break;

        case "coverageTiming":
          CoverageTiming = new fhirCsR5.Models.Timing();
          CoverageTiming.DeserializeJson(ref reader, options);
          break;

        case "member":
          Member = new fhirCsR5.Models.Reference();
          Member.DeserializeJson(ref reader, options);
          break;

        case "onBehalfOf":
          OnBehalfOf = new fhirCsR5.Models.Reference();
          OnBehalfOf.DeserializeJson(ref reader, options);
          break;

        case "role":
          Role = new fhirCsR5.Models.CodeableConcept();
          Role.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// The Care Team includes all the people and organizations who plan to participate in the coordination and delivery of care.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<CareTeam>))]
  public class CareTeam : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "CareTeam";
    /// <summary>
    /// There may be multiple axis of categorization and one team may serve multiple purposes.
    /// </summary>
    public List<CodeableConcept> Category { get; set; }
    /// <summary>
    /// This is a business identifier, not a resource identifier (see [discussion](resource.html#identifiers)).  It is best practice for the identifier to only appear on a single resource instance, however business practices may occasionally dictate that multiple resource instances with the same identifier can exist - possibly even with different resource types.  For example, multiple Patient and a Person resource instance might share the same social insurance number.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The organization responsible for the care team.
    /// </summary>
    public List<Reference> ManagingOrganization { get; set; }
    /// <summary>
    /// The meaning/purpose of the team is conveyed in CareTeam.category.  This element may also convey semantics of the team (e.g. "Red trauma team"), but its primary purpose is to distinguish between identical teams in a human-friendly way.  ("Team 18735" isn't as friendly.).
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// Comments made about the CareTeam.
    /// </summary>
    public List<Annotation> Note { get; set; }
    /// <summary>
    /// Identifies all people and organizations who are expected to be involved in the care team.
    /// </summary>
    public List<CareTeamParticipant> Participant { get; set; }
    /// <summary>
    /// Indicates when the team did (or is intended to) come into effect and end.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// Describes why the care team exists.
    /// </summary>
    public List<CodeableReference> Reason { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains the code entered-in-error that marks the care team as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// Identifies the patient or group whose intended care is handled by the team.
    /// </summary>
    public Reference Subject { get; set; }
    /// <summary>
    /// The ContactPoint.use code of home is not appropriate to use. These contacts are not the contact details of individual care team members.
    /// </summary>
    public List<ContactPoint> Telecom { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      if (!string.IsNullOrEmpty(ResourceType))
      {
        writer.WriteString("resourceType", (string)ResourceType!);
      }


      ((fhirCsR5.Models.DomainResource)this).SerializeJson(writer, options, false);

      if ((Identifier != null) && (Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();

        foreach (Identifier valIdentifier in Identifier)
        {
          valIdentifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Status))
      {
        writer.WriteString("status", (string)Status!);
      }

      if (_Status != null)
      {
        writer.WritePropertyName("_status");
        _Status.SerializeJson(writer, options);
      }

      if ((Category != null) && (Category.Count != 0))
      {
        writer.WritePropertyName("category");
        writer.WriteStartArray();

        foreach (CodeableConcept valCategory in Category)
        {
          valCategory.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
      }

      if (Subject != null)
      {
        writer.WritePropertyName("subject");
        Subject.SerializeJson(writer, options);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if ((Participant != null) && (Participant.Count != 0))
      {
        writer.WritePropertyName("participant");
        writer.WriteStartArray();

        foreach (CareTeamParticipant valParticipant in Participant)
        {
          valParticipant.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Reason != null) && (Reason.Count != 0))
      {
        writer.WritePropertyName("reason");
        writer.WriteStartArray();

        foreach (CodeableReference valReason in Reason)
        {
          valReason.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((ManagingOrganization != null) && (ManagingOrganization.Count != 0))
      {
        writer.WritePropertyName("managingOrganization");
        writer.WriteStartArray();

        foreach (Reference valManagingOrganization in ManagingOrganization)
        {
          valManagingOrganization.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Telecom != null) && (Telecom.Count != 0))
      {
        writer.WritePropertyName("telecom");
        writer.WriteStartArray();

        foreach (ContactPoint valTelecom in Telecom)
        {
          valTelecom.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Note != null) && (Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();

        foreach (Annotation valNote in Note)
        {
          valNote.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "category":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Category = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableConcept objCategory = new fhirCsR5.Models.CodeableConcept();
            objCategory.DeserializeJson(ref reader, options);
            Category.Add(objCategory);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Category.Count == 0)
          {
            Category = null;
          }

          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Identifier objIdentifier = new fhirCsR5.Models.Identifier();
            objIdentifier.DeserializeJson(ref reader, options);
            Identifier.Add(objIdentifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Identifier.Count == 0)
          {
            Identifier = null;
          }

          break;

        case "managingOrganization":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ManagingOrganization = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Reference objManagingOrganization = new fhirCsR5.Models.Reference();
            objManagingOrganization.DeserializeJson(ref reader, options);
            ManagingOrganization.Add(objManagingOrganization);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ManagingOrganization.Count == 0)
          {
            ManagingOrganization = null;
          }

          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR5.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Annotation objNote = new fhirCsR5.Models.Annotation();
            objNote.DeserializeJson(ref reader, options);
            Note.Add(objNote);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Note.Count == 0)
          {
            Note = null;
          }

          break;

        case "participant":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Participant = new List<CareTeamParticipant>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CareTeamParticipant objParticipant = new fhirCsR5.Models.CareTeamParticipant();
            objParticipant.DeserializeJson(ref reader, options);
            Participant.Add(objParticipant);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Participant.Count == 0)
          {
            Participant = null;
          }

          break;

        case "period":
          Period = new fhirCsR5.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "reason":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Reason = new List<CodeableReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableReference objReason = new fhirCsR5.Models.CodeableReference();
            objReason.DeserializeJson(ref reader, options);
            Reason.Add(objReason);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Reason.Count == 0)
          {
            Reason = null;
          }

          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR5.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subject":
          Subject = new fhirCsR5.Models.Reference();
          Subject.DeserializeJson(ref reader, options);
          break;

        case "telecom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Telecom = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.ContactPoint objTelecom = new fhirCsR5.Models.ContactPoint();
            objTelecom.DeserializeJson(ref reader, options);
            Telecom.Add(objTelecom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Telecom.Count == 0)
          {
            Telecom = null;
          }

          break;

        default:
          ((fhirCsR5.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the CareTeam.status field
  /// </summary>
  public static class CareTeamStatusCodes {
    public const string PROPOSED = "proposed";
    public const string ACTIVE = "active";
    public const string SUSPENDED = "suspended";
    public const string INACTIVE = "inactive";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public static HashSet<string> Values = new HashSet<string>() {
      "proposed",
      "active",
      "suspended",
      "inactive",
      "entered-in-error",
    };
  }
}
