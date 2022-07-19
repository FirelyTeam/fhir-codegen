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
  /// Indicates an action that has been taken or is committed to to reduce or eliminate the likelihood of the risk identified by the detected issue from manifesting.  Can also reflect an observation of known mitigating factors that may reduce/eliminate the need for any action.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<DetectedIssueMitigation>))]
  public class DetectedIssueMitigation : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The "text" component can be used for detail or when no appropriate code exists.
    /// </summary>
    public CodeableConcept Action { get; set; }
    /// <summary>
    /// Identifies the practitioner who determined the mitigation and takes responsibility for the mitigation step occurring.
    /// </summary>
    public Reference Author { get; set; }
    /// <summary>
    /// This may not be the same as when the mitigating step was actually taken.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Action != null)
      {
        writer.WritePropertyName("action");
        Action.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Date))
      {
        writer.WriteString("date", (string)Date!);
      }

      if (_Date != null)
      {
        writer.WritePropertyName("_date");
        _Date.SerializeJson(writer, options);
      }

      if (Author != null)
      {
        writer.WritePropertyName("author");
        Author.SerializeJson(writer, options);
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
        case "action":
          Action = new fhirCsR3.Models.CodeableConcept();
          Action.DeserializeJson(ref reader, options);
          break;

        case "author":
          Author = new fhirCsR3.Models.Reference();
          Author.DeserializeJson(ref reader, options);
          break;

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR3.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Indicates an actual or potential clinical issue with or between one or more active or proposed clinical actions for a patient; e.g. Drug-drug interaction, Ineffective treatment frequency, Procedure-condition conflict, etc.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<DetectedIssue>))]
  public class DetectedIssue : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "DetectedIssue";
    /// <summary>
    /// Individual or device responsible for the issue being raised.  For example, a decision support application or a pharmacist conducting a medication review.
    /// </summary>
    public Reference Author { get; set; }
    /// <summary>
    /// Identifies the general type of issue identified.
    /// </summary>
    public CodeableConcept Category { get; set; }
    /// <summary>
    /// The date or date-time when the detected issue was initially identified.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// Should focus on information not covered elsewhere as discrete data - no need to duplicate the narrative.
    /// </summary>
    public string Detail { get; set; }
    /// <summary>
    /// Extension container element for Detail
    /// </summary>
    public Element _Detail { get; set; }
    /// <summary>
    /// Business identifier associated with the detected issue record.
    /// </summary>
    public Identifier Identifier { get; set; }
    /// <summary>
    /// There's an implicit constraint on the number of implicated resources based on DetectedIssue.type; e.g. For drug-drug, there would be more than one.  For timing, there would typically only be one.
    /// </summary>
    public List<Reference> Implicated { get; set; }
    /// <summary>
    /// Indicates an action that has been taken or is committed to to reduce or eliminate the likelihood of the risk identified by the detected issue from manifesting.  Can also reflect an observation of known mitigating factors that may reduce/eliminate the need for any action.
    /// </summary>
    public List<DetectedIssueMitigation> Mitigation { get; set; }
    /// <summary>
    /// Indicates the patient whose record the detected issue is associated with.
    /// </summary>
    public Reference Patient { get; set; }
    /// <summary>
    /// The literature, knowledge-base or similar reference that describes the propensity for the detected issue identified.
    /// </summary>
    public string Reference { get; set; }
    /// <summary>
    /// Extension container element for Reference
    /// </summary>
    public Element _Reference { get; set; }
    /// <summary>
    /// Indicates the degree of importance associated with the identified issue based on the potential impact on the patient.
    /// </summary>
    public string Severity { get; set; }
    /// <summary>
    /// Extension container element for Severity
    /// </summary>
    public Element _Severity { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains the codes cancelled and entered-in-error that mark the issue as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
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

      if (Category != null)
      {
        writer.WritePropertyName("category");
        Category.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Severity))
      {
        writer.WriteString("severity", (string)Severity!);
      }

      if (_Severity != null)
      {
        writer.WritePropertyName("_severity");
        _Severity.SerializeJson(writer, options);
      }

      if (Patient != null)
      {
        writer.WritePropertyName("patient");
        Patient.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Date))
      {
        writer.WriteString("date", (string)Date!);
      }

      if (_Date != null)
      {
        writer.WritePropertyName("_date");
        _Date.SerializeJson(writer, options);
      }

      if (Author != null)
      {
        writer.WritePropertyName("author");
        Author.SerializeJson(writer, options);
      }

      if ((Implicated != null) && (Implicated.Count != 0))
      {
        writer.WritePropertyName("implicated");
        writer.WriteStartArray();

        foreach (Reference valImplicated in Implicated)
        {
          valImplicated.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Detail))
      {
        writer.WriteString("detail", (string)Detail!);
      }

      if (_Detail != null)
      {
        writer.WritePropertyName("_detail");
        _Detail.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Reference))
      {
        writer.WriteString("reference", (string)Reference!);
      }

      if (_Reference != null)
      {
        writer.WritePropertyName("_reference");
        _Reference.SerializeJson(writer, options);
      }

      if ((Mitigation != null) && (Mitigation.Count != 0))
      {
        writer.WritePropertyName("mitigation");
        writer.WriteStartArray();

        foreach (DetectedIssueMitigation valMitigation in Mitigation)
        {
          valMitigation.SerializeJson(writer, options, true);
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
        case "author":
          Author = new fhirCsR3.Models.Reference();
          Author.DeserializeJson(ref reader, options);
          break;

        case "category":
          Category = new fhirCsR3.Models.CodeableConcept();
          Category.DeserializeJson(ref reader, options);
          break;

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR3.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "detail":
          Detail = reader.GetString();
          break;

        case "_detail":
          _Detail = new fhirCsR3.Models.Element();
          _Detail.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          Identifier = new fhirCsR3.Models.Identifier();
          Identifier.DeserializeJson(ref reader, options);
          break;

        case "implicated":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Implicated = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objImplicated = new fhirCsR3.Models.Reference();
            objImplicated.DeserializeJson(ref reader, options);
            Implicated.Add(objImplicated);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Implicated.Count == 0)
          {
            Implicated = null;
          }

          break;

        case "mitigation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Mitigation = new List<DetectedIssueMitigation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.DetectedIssueMitigation objMitigation = new fhirCsR3.Models.DetectedIssueMitigation();
            objMitigation.DeserializeJson(ref reader, options);
            Mitigation.Add(objMitigation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Mitigation.Count == 0)
          {
            Mitigation = null;
          }

          break;

        case "patient":
          Patient = new fhirCsR3.Models.Reference();
          Patient.DeserializeJson(ref reader, options);
          break;

        case "reference":
          Reference = reader.GetString();
          break;

        case "_reference":
          _Reference = new fhirCsR3.Models.Element();
          _Reference.DeserializeJson(ref reader, options);
          break;

        case "severity":
          Severity = reader.GetString();
          break;

        case "_severity":
          _Severity = new fhirCsR3.Models.Element();
          _Severity.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR3.Models.Element();
          _Status.DeserializeJson(ref reader, options);
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
  /// Code Values for the DetectedIssue.severity field
  /// </summary>
  public static class DetectedIssueSeverityCodes {
    public const string HIGH = "high";
    public const string MODERATE = "moderate";
    public const string LOW = "low";
    public static HashSet<string> Values = new HashSet<string>() {
      "high",
      "moderate",
      "low",
    };
  }
  /// <summary>
  /// Code Values for the DetectedIssue.status field
  /// </summary>
  public static class DetectedIssueStatusCodes {
    public const string REGISTERED = "registered";
    public const string PRELIMINARY = "preliminary";
    public const string FINAL = "final";
    public const string AMENDED = "amended";
    public const string CORRECTED = "corrected";
    public const string CANCELLED = "cancelled";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public const string UNKNOWN = "unknown";
    public static HashSet<string> Values = new HashSet<string>() {
      "registered",
      "preliminary",
      "final",
      "amended",
      "corrected",
      "cancelled",
      "entered-in-error",
      "unknown",
    };
  }
}
