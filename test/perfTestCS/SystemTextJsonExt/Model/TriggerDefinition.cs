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
  /// JSON Serialization Extensions for TriggerDefinition
  /// </summary>
  public static class TriggerDefinitionJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR TriggerDefinition into JSON
    /// </summary>
    public static void SerializeJson(this TriggerDefinition current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("type",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.TypeElement.Value));

      if ((current.NameElement != null) && (current.NameElement.Value != null))
      {
        writer.WriteString("name",current.NameElement.Value);
      }

      if (current.Timing != null)
      {
        switch (current.Timing)
        {
          case Timing v_Timing:
            writer.WritePropertyName("timingTiming");
            v_Timing.SerializeJson(writer, options);
            break;
          case ResourceReference v_ResourceReference:
            writer.WritePropertyName("timingReference");
            v_ResourceReference.SerializeJson(writer, options);
            break;
          case Date v_Date:
            writer.WriteString("timingDate",v_Date.Value);
            break;
          case FhirDateTime v_FhirDateTime:
            writer.WriteString("timingDateTime",v_FhirDateTime.Value);
            break;
        }
      }
      if ((current.Data != null) && (current.Data.Count != 0))
      {
        writer.WritePropertyName("data");
        writer.WriteStartArray();
        foreach (DataRequirement val in current.Data)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Condition != null)
      {
        writer.WritePropertyName("condition");
        current.Condition.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR TriggerDefinition
    /// </summary>
    public static void DeserializeJson(this TriggerDefinition current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR TriggerDefinition
    /// </summary>
    public static void DeserializeJsonProperty(this TriggerDefinition current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "type":
          current.TypeElement =new Code<Hl7.Fhir.Model.TriggerDefinition.TriggerType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.TriggerDefinition.TriggerType>(reader.GetString()));
          break;

        case "name":
          current.NameElement = new FhirString(reader.GetString());
          break;

        case "timingTiming":
          current.Timing = new Hl7.Fhir.Model.Timing();
          current.Timing.DeserializeJson(ref reader, options);
          break;

        case "timingReference":
          current.Timing = new Hl7.Fhir.Model.ResourceReference();
          current.Timing.DeserializeJson(ref reader, options);
          break;

        case "timingDate":
          current.Timing = new Date(reader.GetString());
          break;

        case "timingDateTime":
          current.Timing = new FhirDateTime(reader.GetString());
          break;

        case "data":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Data = new List<DataRequirement>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.DataRequirement v_Data = new Hl7.Fhir.Model.DataRequirement();
            v_Data.DeserializeJson(ref reader, options);
            current.Data.Add(v_Data);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Data.Count == 0)
          {
            current.Data = null;
          }
          break;

        case "condition":
          current.Condition = new Hl7.Fhir.Model.Expression();
          current.Condition.DeserializeJson(ref reader, options);
          break;

      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class TriggerDefinitionJsonConverter : JsonConverter<TriggerDefinition>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(TriggerDefinition).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, TriggerDefinition value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override TriggerDefinition Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        TriggerDefinition target = new TriggerDefinition();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
