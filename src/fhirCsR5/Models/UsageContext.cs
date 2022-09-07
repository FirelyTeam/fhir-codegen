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
  /// Specifies clinical/business/etc. metadata that can be used to retrieve, index and/or categorize an artifact. This metadata can either be specific to the applicable population (e.g., age category, DRG) or the specific context of care (e.g., venue, care setting, provider of care).
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<UsageContext>))]
  public class UsageContext : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// A code that identifies the type of context being specified by this usage context.
    /// </summary>
    public Coding Code { get; set; }
    /// <summary>
    /// A value that defines the context specified in this context of use. The interpretation of the value is defined by the code.
    /// </summary>
    public CodeableConcept ValueCodeableConcept { get; set; }
    /// <summary>
    /// A value that defines the context specified in this context of use. The interpretation of the value is defined by the code.
    /// </summary>
    public Quantity ValueQuantity { get; set; }
    /// <summary>
    /// A value that defines the context specified in this context of use. The interpretation of the value is defined by the code.
    /// </summary>
    public Range ValueRange { get; set; }
    /// <summary>
    /// A value that defines the context specified in this context of use. The interpretation of the value is defined by the code.
    /// </summary>
    public Reference ValueReference { get; set; }
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

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
      }

      if (ValueCodeableConcept != null)
      {
        writer.WritePropertyName("valueCodeableConcept");
        ValueCodeableConcept.SerializeJson(writer, options);
      }

      if (ValueQuantity != null)
      {
        writer.WritePropertyName("valueQuantity");
        ValueQuantity.SerializeJson(writer, options);
      }

      if (ValueRange != null)
      {
        writer.WritePropertyName("valueRange");
        ValueRange.SerializeJson(writer, options);
      }

      if (ValueReference != null)
      {
        writer.WritePropertyName("valueReference");
        ValueReference.SerializeJson(writer, options);
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
          Code = new fhirCsR5.Models.Coding();
          Code.DeserializeJson(ref reader, options);
          break;

        case "valueCodeableConcept":
          ValueCodeableConcept = new fhirCsR5.Models.CodeableConcept();
          ValueCodeableConcept.DeserializeJson(ref reader, options);
          break;

        case "valueQuantity":
          ValueQuantity = new fhirCsR5.Models.Quantity();
          ValueQuantity.DeserializeJson(ref reader, options);
          break;

        case "valueRange":
          ValueRange = new fhirCsR5.Models.Range();
          ValueRange.DeserializeJson(ref reader, options);
          break;

        case "valueReference":
          ValueReference = new fhirCsR5.Models.Reference();
          ValueReference.DeserializeJson(ref reader, options);
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
