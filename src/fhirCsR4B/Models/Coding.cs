// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR4B.Serialization;

namespace fhirCsR4B.Models
{
  /// <summary>
  /// A reference to a code defined by a terminology system.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<Coding>))]
  public class Coding : Element,  IFhirJsonSerializable {
    /// <summary>
    /// A symbol in syntax defined by the system. The symbol may be a predefined code or an expression in a syntax defined by the coding system (e.g. post-coordination).
    /// </summary>
    public string Code { get; set; }
    /// <summary>
    /// Extension container element for Code
    /// </summary>
    public Element _Code { get; set; }
    /// <summary>
    /// A representation of the meaning of the code in the system, following the rules of the system.
    /// </summary>
    public string Display { get; set; }
    /// <summary>
    /// Extension container element for Display
    /// </summary>
    public Element _Display { get; set; }
    /// <summary>
    /// The URI may be an OID (urn:oid:...) or a UUID (urn:uuid:...).  OIDs and UUIDs SHALL be references to the HL7 OID registry. Otherwise, the URI should come from HL7's list of FHIR defined special URIs or it should reference to some definition that establishes the system clearly and unambiguously.
    /// </summary>
    public string System { get; set; }
    /// <summary>
    /// Extension container element for System
    /// </summary>
    public Element _System { get; set; }
    /// <summary>
    /// Amongst a set of alternatives, a directly chosen code is the most appropriate starting point for new translations. There is some ambiguity about what exactly 'directly chosen' implies, and trading partner agreement may be needed to clarify the use of this element and its consequences more completely.
    /// </summary>
    public bool? UserSelected { get; set; }
    /// <summary>
    /// Extension container element for UserSelected
    /// </summary>
    public Element _UserSelected { get; set; }
    /// <summary>
    /// Where the terminology does not clearly define what string should be used to identify code system versions, the recommendation is to use the date (expressed in FHIR date format) on which that version was officially published as the version date.
    /// </summary>
    public string Version { get; set; }
    /// <summary>
    /// Extension container element for Version
    /// </summary>
    public Element _Version { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4B.Models.Element)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(System))
      {
        writer.WriteString("system", (string)System!);
      }

      if (_System != null)
      {
        writer.WritePropertyName("_system");
        _System.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Version))
      {
        writer.WriteString("version", (string)Version!);
      }

      if (_Version != null)
      {
        writer.WritePropertyName("_version");
        _Version.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Code))
      {
        writer.WriteString("code", (string)Code!);
      }

      if (_Code != null)
      {
        writer.WritePropertyName("_code");
        _Code.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Display))
      {
        writer.WriteString("display", (string)Display!);
      }

      if (_Display != null)
      {
        writer.WritePropertyName("_display");
        _Display.SerializeJson(writer, options);
      }

      if (UserSelected != null)
      {
        writer.WriteBoolean("userSelected", (bool)UserSelected!);
      }

      if (_UserSelected != null)
      {
        writer.WritePropertyName("_userSelected");
        _UserSelected.SerializeJson(writer, options);
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
        case "code":
          Code = reader.GetString();
          break;

        case "_code":
          _Code = new fhirCsR4B.Models.Element();
          _Code.DeserializeJson(ref reader, options);
          break;

        case "display":
          Display = reader.GetString();
          break;

        case "_display":
          _Display = new fhirCsR4B.Models.Element();
          _Display.DeserializeJson(ref reader, options);
          break;

        case "system":
          System = reader.GetString();
          break;

        case "_system":
          _System = new fhirCsR4B.Models.Element();
          _System.DeserializeJson(ref reader, options);
          break;

        case "userSelected":
          UserSelected = reader.GetBoolean();
          break;

        case "_userSelected":
          _UserSelected = new fhirCsR4B.Models.Element();
          _UserSelected.DeserializeJson(ref reader, options);
          break;

        case "version":
          Version = reader.GetString();
          break;

        case "_version":
          _Version = new fhirCsR4B.Models.Element();
          _Version.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4B.Models.Element)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
}
