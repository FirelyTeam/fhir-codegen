// <auto-generated/>
// Contents of: hl7.fhir.r4.core version: 4.0.1

using System;
using System.Buffers;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using Hl7.Fhir.Model;
using Hl7.Fhir.Model.JsonExtensions;
using Hl7.Fhir.Serialization;

/*
  Copyright (c) 2011+, HL7, Inc.
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification, 
  are permitted provided that the following conditions are met:
  
   * Redistributions of source code must retain the above copyright notice, this 
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice, 
     this list of conditions and the following disclaimer in the documentation 
     and/or other materials provided with the distribution.
   * Neither the name of HL7 nor the names of its contributors may be used to 
     endorse or promote products derived from this software without specific 
     prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
  POSSIBILITY OF SUCH DAMAGE.
  
*/

namespace Hl7.Fhir.Model.JsonExtensions
{
  /// <summary>
  /// JSON Serialization Extensions for DeviceMetric
  /// </summary>
  public static class DeviceMetricJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR DeviceMetric into JSON
    /// </summary>
    public static void SerializeJson(this DeviceMetric current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","DeviceMetric");
      // Complex: DeviceMetric, Export: DeviceMetric, Base: DomainResource (DomainResource)
      ((Hl7.Fhir.Model.DomainResource)current).SerializeJson(writer, options, false);

      if ((current.Identifier != null) && (current.Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();
        foreach (Identifier val in current.Identifier)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      writer.WritePropertyName("type");
      current.Type.SerializeJson(writer, options);

      if (current.Unit != null)
      {
        writer.WritePropertyName("unit");
        current.Unit.SerializeJson(writer, options);
      }

      if (current.Source != null)
      {
        writer.WritePropertyName("source");
        current.Source.SerializeJson(writer, options);
      }

      if (current.Parent != null)
      {
        writer.WritePropertyName("parent");
        current.Parent.SerializeJson(writer, options);
      }

      if (current.OperationalStatusElement != null)
      {
        if (current.OperationalStatusElement.Value != null)
        {
          writer.WriteString("operationalStatus",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.OperationalStatusElement.Value));
        }
        if (current.OperationalStatusElement.HasExtensions() || (!string.IsNullOrEmpty(current.OperationalStatusElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_operationalStatus",false,current.OperationalStatusElement.Extension,current.OperationalStatusElement.ElementId);
        }
      }

      if (current.ColorElement != null)
      {
        if (current.ColorElement.Value != null)
        {
          writer.WriteString("color",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.ColorElement.Value));
        }
        if (current.ColorElement.HasExtensions() || (!string.IsNullOrEmpty(current.ColorElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_color",false,current.ColorElement.Extension,current.ColorElement.ElementId);
        }
      }

      writer.WriteString("category",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.CategoryElement.Value));

      if (current.MeasurementPeriod != null)
      {
        writer.WritePropertyName("measurementPeriod");
        current.MeasurementPeriod.SerializeJson(writer, options);
      }

      if ((current.Calibration != null) && (current.Calibration.Count != 0))
      {
        writer.WritePropertyName("calibration");
        writer.WriteStartArray();
        foreach (DeviceMetric.CalibrationComponent val in current.Calibration)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DeviceMetric
    /// </summary>
    public static void DeserializeJson(this DeviceMetric current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          if (Hl7.Fhir.Serialization.FhirSerializerOptions.Debug) { Console.WriteLine($"DeviceMetric >>> DeviceMetric.{propertyName}, depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}"); }
          reader.Read();
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException($"DeviceMetric: invalid state! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DeviceMetric
    /// </summary>
    public static void DeserializeJsonProperty(this DeviceMetric current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"DeviceMetric error reading 'identifier' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Identifier v_Identifier = new Hl7.Fhir.Model.Identifier();
            v_Identifier.DeserializeJson(ref reader, options);
            current.Identifier.Add(v_Identifier);

            if (!reader.Read())
            {
              throw new JsonException($"DeviceMetric error reading 'identifier' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Identifier.Count == 0)
          {
            current.Identifier = null;
          }
          break;

        case "type":
          current.Type = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Type).DeserializeJson(ref reader, options);
          break;

        case "unit":
          current.Unit = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Unit).DeserializeJson(ref reader, options);
          break;

        case "source":
          current.Source = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Source).DeserializeJson(ref reader, options);
          break;

        case "parent":
          current.Parent = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Parent).DeserializeJson(ref reader, options);
          break;

        case "operationalStatus":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.OperationalStatusElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricOperationalStatus>();
            reader.Skip();
          }
          else
          {
            current.OperationalStatusElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricOperationalStatus>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.DeviceMetric.DeviceMetricOperationalStatus>(reader.GetString()));
          }
          break;

        case "_operationalStatus":
          if (current.OperationalStatusElement == null) { current.OperationalStatusElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricOperationalStatus>(); }
          ((Hl7.Fhir.Model.Element)current.OperationalStatusElement).DeserializeJson(ref reader, options);
          break;

        case "color":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.ColorElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricColor>();
            reader.Skip();
          }
          else
          {
            current.ColorElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricColor>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.DeviceMetric.DeviceMetricColor>(reader.GetString()));
          }
          break;

        case "_color":
          if (current.ColorElement == null) { current.ColorElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricColor>(); }
          ((Hl7.Fhir.Model.Element)current.ColorElement).DeserializeJson(ref reader, options);
          break;

        case "category":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.CategoryElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCategory>();
            reader.Skip();
          }
          else
          {
            current.CategoryElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCategory>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCategory>(reader.GetString()));
          }
          break;

        case "_category":
          if (current.CategoryElement == null) { current.CategoryElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCategory>(); }
          ((Hl7.Fhir.Model.Element)current.CategoryElement).DeserializeJson(ref reader, options);
          break;

        case "measurementPeriod":
          current.MeasurementPeriod = new Hl7.Fhir.Model.Timing();
          ((Hl7.Fhir.Model.Timing)current.MeasurementPeriod).DeserializeJson(ref reader, options);
          break;

        case "calibration":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"DeviceMetric error reading 'calibration' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Calibration = new List<DeviceMetric.CalibrationComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.DeviceMetric.CalibrationComponent v_Calibration = new Hl7.Fhir.Model.DeviceMetric.CalibrationComponent();
            v_Calibration.DeserializeJson(ref reader, options);
            current.Calibration.Add(v_Calibration);

            if (!reader.Read())
            {
              throw new JsonException($"DeviceMetric error reading 'calibration' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Calibration.Count == 0)
          {
            current.Calibration = null;
          }
          break;

        // Complex: DeviceMetric, Export: DeviceMetric, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR DeviceMetric#Calibration into JSON
    /// </summary>
    public static void SerializeJson(this DeviceMetric.CalibrationComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: DeviceMetric#Calibration, Export: CalibrationComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.TypeElement != null)
      {
        if (current.TypeElement.Value != null)
        {
          writer.WriteString("type",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.TypeElement.Value));
        }
        if (current.TypeElement.HasExtensions() || (!string.IsNullOrEmpty(current.TypeElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_type",false,current.TypeElement.Extension,current.TypeElement.ElementId);
        }
      }

      if (current.StateElement != null)
      {
        if (current.StateElement.Value != null)
        {
          writer.WriteString("state",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StateElement.Value));
        }
        if (current.StateElement.HasExtensions() || (!string.IsNullOrEmpty(current.StateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_state",false,current.StateElement.Extension,current.StateElement.ElementId);
        }
      }

      if (current.TimeElement != null)
      {
        if (current.TimeElement.Value != null)
        {
          writer.WriteString("time",((DateTimeOffset)current.TimeElement.Value).ToString("yyyy-MM-dd'T'HH:mm:ss.FFFFFFFK",System.Globalization.CultureInfo.InvariantCulture));
        }
        if (current.TimeElement.HasExtensions() || (!string.IsNullOrEmpty(current.TimeElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_time",false,current.TimeElement.Extension,current.TimeElement.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DeviceMetric#Calibration
    /// </summary>
    public static void DeserializeJson(this DeviceMetric.CalibrationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          if (Hl7.Fhir.Serialization.FhirSerializerOptions.Debug) { Console.WriteLine($"DeviceMetric.CalibrationComponent >>> DeviceMetric#Calibration.{propertyName}, depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}"); }
          reader.Read();
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException($"DeviceMetric.CalibrationComponent: invalid state! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DeviceMetric#Calibration
    /// </summary>
    public static void DeserializeJsonProperty(this DeviceMetric.CalibrationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "type":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.TypeElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCalibrationType>();
            reader.Skip();
          }
          else
          {
            current.TypeElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCalibrationType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCalibrationType>(reader.GetString()));
          }
          break;

        case "_type":
          if (current.TypeElement == null) { current.TypeElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCalibrationType>(); }
          ((Hl7.Fhir.Model.Element)current.TypeElement).DeserializeJson(ref reader, options);
          break;

        case "state":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.StateElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCalibrationState>();
            reader.Skip();
          }
          else
          {
            current.StateElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCalibrationState>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCalibrationState>(reader.GetString()));
          }
          break;

        case "_state":
          if (current.StateElement == null) { current.StateElement = new Code<Hl7.Fhir.Model.DeviceMetric.DeviceMetricCalibrationState>(); }
          ((Hl7.Fhir.Model.Element)current.StateElement).DeserializeJson(ref reader, options);
          break;

        case "time":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.TimeElement = new Instant();
            reader.Skip();
          }
          else
          {
            current.TimeElement = new Instant(DateTimeOffset.Parse(reader.GetString()));
          }
          break;

        case "_time":
          if (current.TimeElement == null) { current.TimeElement = new Instant(); }
          ((Hl7.Fhir.Model.Element)current.TimeElement).DeserializeJson(ref reader, options);
          break;

        // Complex: calibration, Export: CalibrationComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class DeviceMetricJsonConverter : JsonConverter<DeviceMetric>
    {
      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, DeviceMetric value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override DeviceMetric Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        DeviceMetric target = new DeviceMetric();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
