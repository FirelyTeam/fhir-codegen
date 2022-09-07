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
  /// A reference to a resource (by instance), or instead, a reference to a concept defined in a terminology or ontology (by class).
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<CodeableReference>))]
  public class CodeableReference : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// A reference to a concept - e.g. the information is identified by its general class to the degree of precision found in the terminology.
    /// </summary>
    public CodeableConcept Concept { get; set; }
    /// <summary>
    /// A reference to a resource the provides exact details about the information being referenced.
    /// </summary>
    public Reference Reference { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.DataType)this).SerializeJson(writer, options, false);

      if (Concept != null)
      {
        writer.WritePropertyName("concept");
        Concept.SerializeJson(writer, options);
      }

      if (Reference != null)
      {
        writer.WritePropertyName("reference");
        Reference.SerializeJson(writer, options);
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
        case "concept":
          Concept = new fhirCsR5.Models.CodeableConcept();
          Concept.DeserializeJson(ref reader, options);
          break;

        case "reference":
          Reference = new fhirCsR5.Models.Reference();
          Reference.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.DataType)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
