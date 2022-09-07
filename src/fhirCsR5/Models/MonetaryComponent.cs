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
  /// Availability data for an {item}.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<MonetaryComponent>))]
  public class MonetaryComponent : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// Explicit value amount to be used.
    /// </summary>
    public Money Amount { get; set; }
    /// <summary>
    /// Codes may be used to differentiate between kinds of taxes, surcharges, discounts etc.
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// Factor used for calculating this component.
    /// </summary>
    public decimal? Factor { get; set; }
    /// <summary>
    /// Extension container element for Factor
    /// </summary>
    public Element _Factor { get; set; }
    /// <summary>
    /// base | surcharge | deduction | discount | tax | informational.
    /// </summary>
    public string Type { get; set; }
    /// <summary>
    /// Extension container element for Type
    /// </summary>
    public Element _Type { get; set; }
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

      if (!string.IsNullOrEmpty(Type))
      {
        writer.WriteString("type", (string)Type!);
      }

      if (_Type != null)
      {
        writer.WritePropertyName("_type");
        _Type.SerializeJson(writer, options);
      }

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
      }

      if (Factor != null)
      {
        writer.WriteNumber("factor", (decimal)Factor!);
      }

      if (_Factor != null)
      {
        writer.WritePropertyName("_factor");
        _Factor.SerializeJson(writer, options);
      }

      if (Amount != null)
      {
        writer.WritePropertyName("amount");
        Amount.SerializeJson(writer, options);
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
        case "amount":
          Amount = new fhirCsR5.Models.Money();
          Amount.DeserializeJson(ref reader, options);
          break;

        case "code":
          Code = new fhirCsR5.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "factor":
          Factor = reader.GetDecimal();
          break;

        case "_factor":
          _Factor = new fhirCsR5.Models.Element();
          _Factor.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = reader.GetString();
          break;

        case "_type":
          _Type = new fhirCsR5.Models.Element();
          _Type.DeserializeJson(ref reader, options);
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
  /// <summary>
  /// Code Values for the MonetaryComponent.type field
  /// </summary>
  public static class MonetaryComponentTypeCodes {
    public const string VAL_BASE = "base";
    public const string SURCHARGE = "surcharge";
    public const string DEDUCTION = "deduction";
    public const string DISCOUNT = "discount";
    public const string TAX = "tax";
    public const string INFORMATIONAL = "informational";
    public static HashSet<string> Values = new HashSet<string>() {
      "base",
      "surcharge",
      "deduction",
      "discount",
      "tax",
      "informational",
    };
  }
}
