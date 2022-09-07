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
  /// A measured amount (or an amount that can potentially be measured). Note that measured amounts include amounts that are not precisely quantified, including amounts involving arbitrary units and floating currencies.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<Quantity>))]
  public class Quantity : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// The preferred system is UCUM, but SNOMED CT can also be used (for customary units) or ISO 4217 for currency.  The context of use may additionally require a code from a particular system.
    /// </summary>
    public string Code { get; set; }
    /// <summary>
    /// Extension container element for Code
    /// </summary>
    public Element _Code { get; set; }
    /// <summary>
    /// How the value should be understood and represented - whether the actual value is greater or less than the stated value due to measurement issues; e.g. if the comparator is "&lt;" , then the real value is &lt; stated value.
    /// </summary>
    public string Comparator { get; set; }
    /// <summary>
    /// Extension container element for Comparator
    /// </summary>
    public Element _Comparator { get; set; }
    /// <summary>
    /// The identification of the system that provides the coded form of the unit.
    /// </summary>
    public string System { get; set; }
    /// <summary>
    /// Extension container element for System
    /// </summary>
    public Element _System { get; set; }
    /// <summary>
    /// A human-readable form of the unit.
    /// </summary>
    public string Unit { get; set; }
    /// <summary>
    /// Extension container element for Unit
    /// </summary>
    public Element _Unit { get; set; }
    /// <summary>
    /// The implicit precision in the value should always be honored. Monetary values have their own rules for handling precision (refer to standard accounting text books).
    /// </summary>
    public decimal? Value { get; set; }
    /// <summary>
    /// Extension container element for Value
    /// </summary>
    public Element _Value { get; set; }
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

      if (Value != null)
      {
        writer.WriteNumber("value", (decimal)Value!);
      }

      if (_Value != null)
      {
        writer.WritePropertyName("_value");
        _Value.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Comparator))
      {
        writer.WriteString("comparator", (string)Comparator!);
      }

      if (_Comparator != null)
      {
        writer.WritePropertyName("_comparator");
        _Comparator.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Unit))
      {
        writer.WriteString("unit", (string)Unit!);
      }

      if (_Unit != null)
      {
        writer.WritePropertyName("_unit");
        _Unit.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(System))
      {
        writer.WriteString("system", (string)System!);
      }

      if (_System != null)
      {
        writer.WritePropertyName("_system");
        _System.SerializeJson(writer, options);
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
          _Code = new fhirCsR5.Models.Element();
          _Code.DeserializeJson(ref reader, options);
          break;

        case "comparator":
          Comparator = reader.GetString();
          break;

        case "_comparator":
          _Comparator = new fhirCsR5.Models.Element();
          _Comparator.DeserializeJson(ref reader, options);
          break;

        case "system":
          System = reader.GetString();
          break;

        case "_system":
          _System = new fhirCsR5.Models.Element();
          _System.DeserializeJson(ref reader, options);
          break;

        case "unit":
          Unit = reader.GetString();
          break;

        case "_unit":
          _Unit = new fhirCsR5.Models.Element();
          _Unit.DeserializeJson(ref reader, options);
          break;

        case "value":
          Value = reader.GetDecimal();
          break;

        case "_value":
          _Value = new fhirCsR5.Models.Element();
          _Value.DeserializeJson(ref reader, options);
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
  /// Code Values for the Quantity.comparator field
  /// </summary>
  public static class QuantityComparatorCodes {
    public const string LESS_THAN = "<";
    public const string LESS_OR_EQUAL = "<=";
    public const string GREATER_OR_EQUAL = ">=";
    public const string GREATER_THAN = ">";
    public const string AD = "ad";
    public static HashSet<string> Values = new HashSet<string>() {
      "<",
      "<=",
      ">=",
      ">",
      "ad",
    };
  }
}
