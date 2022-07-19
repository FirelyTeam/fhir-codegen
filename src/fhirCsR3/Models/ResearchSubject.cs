// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR3.Serialization;

namespace fhirCsR3.Models
{
  /// <summary>
  /// A process where a researcher or organization plans and then executes a series of steps intended to increase the field of healthcare-related knowledge.  This includes studies of safety, efficacy, comparative effectiveness and other information about medications, devices, therapies and other interventional and investigative techniques.  A ResearchStudy involves the gathering of information about human or animal subjects.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ResearchSubject>))]
  public class ResearchSubject : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "ResearchSubject";
    /// <summary>
    /// The name of the arm in the study the subject actually followed as part of this study.
    /// </summary>
    public string ActualArm { get; set; }
    /// <summary>
    /// Extension container element for ActualArm
    /// </summary>
    public Element _ActualArm { get; set; }
    /// <summary>
    /// The name of the arm in the study the subject is expected to follow as part of this study.
    /// </summary>
    public string AssignedArm { get; set; }
    /// <summary>
    /// Extension container element for AssignedArm
    /// </summary>
    public Element _AssignedArm { get; set; }
    /// <summary>
    /// A record of the patient's informed agreement to participate in the study.
    /// </summary>
    public Reference Consent { get; set; }
    /// <summary>
    /// Identifiers assigned to this research study by the sponsor or other systems.
    /// </summary>
    public Identifier Identifier { get; set; }
    /// <summary>
    /// The record of the person or animal who is involved in the study.
    /// </summary>
    public Reference Individual { get; set; }
    /// <summary>
    /// The dates the subject began and ended their participation in the study.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains codes that mark the resource as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// Reference to the study the subject is participating in.
    /// </summary>
    public Reference Study { get; set; }
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


      ((fhirCsR3.Models.DomainResource)this).SerializeJson(writer, options, false);

      if (Identifier != null)
      {
        writer.WritePropertyName("identifier");
        Identifier.SerializeJson(writer, options);
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

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if (Study != null)
      {
        writer.WritePropertyName("study");
        Study.SerializeJson(writer, options);
      }

      if (Individual != null)
      {
        writer.WritePropertyName("individual");
        Individual.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(AssignedArm))
      {
        writer.WriteString("assignedArm", (string)AssignedArm!);
      }

      if (_AssignedArm != null)
      {
        writer.WritePropertyName("_assignedArm");
        _AssignedArm.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ActualArm))
      {
        writer.WriteString("actualArm", (string)ActualArm!);
      }

      if (_ActualArm != null)
      {
        writer.WritePropertyName("_actualArm");
        _ActualArm.SerializeJson(writer, options);
      }

      if (Consent != null)
      {
        writer.WritePropertyName("consent");
        Consent.SerializeJson(writer, options);
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
        case "actualArm":
          ActualArm = reader.GetString();
          break;

        case "_actualArm":
          _ActualArm = new fhirCsR3.Models.Element();
          _ActualArm.DeserializeJson(ref reader, options);
          break;

        case "assignedArm":
          AssignedArm = reader.GetString();
          break;

        case "_assignedArm":
          _AssignedArm = new fhirCsR3.Models.Element();
          _AssignedArm.DeserializeJson(ref reader, options);
          break;

        case "consent":
          Consent = new fhirCsR3.Models.Reference();
          Consent.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          Identifier = new fhirCsR3.Models.Identifier();
          Identifier.DeserializeJson(ref reader, options);
          break;

        case "individual":
          Individual = new fhirCsR3.Models.Reference();
          Individual.DeserializeJson(ref reader, options);
          break;

        case "period":
          Period = new fhirCsR3.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR3.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "study":
          Study = new fhirCsR3.Models.Reference();
          Study.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the ResearchSubject.status field
  /// </summary>
  public static class ResearchSubjectStatusCodes {
    public const string CANDIDATE = "candidate";
    public const string ENROLLED = "enrolled";
    public const string ACTIVE = "active";
    public const string SUSPENDED = "suspended";
    public const string WITHDRAWN = "withdrawn";
    public const string COMPLETED = "completed";
    public static HashSet<string> Values = new HashSet<string>() {
      "candidate",
      "enrolled",
      "active",
      "suspended",
      "withdrawn",
      "completed",
    };
  }
}
