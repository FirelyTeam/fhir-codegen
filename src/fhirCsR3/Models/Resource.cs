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
  /// This is the base resource type for everything.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamResourceConverter))]
  public class Resource : IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public virtual string ResourceType => string.Empty;
    /// <summary>
    /// The only time that a resource does not have an id is when it is being submitted to the server using a create operation.
    /// </summary>
    public string Id { get; set; }
    /// <summary>
    /// Extension container element for Id
    /// </summary>
    public Element _Id { get; set; }
    /// <summary>
    /// Asserting this rule set restricts the content to be only understood by a limited set of trading partners. This inherently limits the usefulness of the data in the long term. However, the existing health eco-system is highly fractured, and not yet ready to define, collect, and exchange data in a generally computable sense. Wherever possible, implementers and/or specification writers should avoid using this element. 
    /// This element is labelled as a modifier because the implicit rules may provide additional knowledge about the resource that modifies it's meaning or interpretation.
    /// </summary>
    public string ImplicitRules { get; set; }
    /// <summary>
    /// Extension container element for ImplicitRules
    /// </summary>
    public Element _ImplicitRules { get; set; }
    /// <summary>
    /// Language is provided to support indexing and accessibility (typically, services such as text to speech use the language tag). The html language tag in the narrative applies  to the narrative. The language tag on the resource may be used to specify the language of other presentations generated from the data in the resource  Not all the content has to be in the base language. The Resource.language should not be assumed to apply to the narrative automatically. If a language is specified, it should it also be specified on the div element in the html (see rules in HTML5 for information about the relationship between xml:lang and the html lang attribute).
    /// </summary>
    public string Language { get; set; }
    /// <summary>
    /// Extension container element for Language
    /// </summary>
    public Element _Language { get; set; }
    /// <summary>
    /// The metadata about the resource. This is content that is maintained by the infrastructure. Changes to the content may not always be associated with version changes to the resource.
    /// </summary>
    public Meta Meta { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      if (!string.IsNullOrEmpty(Id))
      {
        writer.WriteString("id", (string)Id!);
      }

      if (_Id != null)
      {
        writer.WritePropertyName("_id");
        _Id.SerializeJson(writer, options);
      }

      if (Meta != null)
      {
        writer.WritePropertyName("meta");
        Meta.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ImplicitRules))
      {
        writer.WriteString("implicitRules", (string)ImplicitRules!);
      }

      if (_ImplicitRules != null)
      {
        writer.WritePropertyName("_implicitRules");
        _ImplicitRules.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Language))
      {
        writer.WriteString("language", (string)Language!);
      }

      if (_Language != null)
      {
        writer.WritePropertyName("_language");
        _Language.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "id":
          Id = reader.GetString();
          break;

        case "_id":
          _Id = new fhirCsR3.Models.Element();
          _Id.DeserializeJson(ref reader, options);
          break;

        case "implicitRules":
          ImplicitRules = reader.GetString();
          break;

        case "_implicitRules":
          _ImplicitRules = new fhirCsR3.Models.Element();
          _ImplicitRules.DeserializeJson(ref reader, options);
          break;

        case "language":
          Language = reader.GetString();
          break;

        case "_language":
          _Language = new fhirCsR3.Models.Element();
          _Language.DeserializeJson(ref reader, options);
          break;

        case "meta":
          Meta = new fhirCsR3.Models.Meta();
          Meta.DeserializeJson(ref reader, options);
          break;

      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
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
