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
  /// JSON Serialization Extensions for RelatedArtifact
  /// </summary>
  public static class RelatedArtifactJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR RelatedArtifact into JSON
    /// </summary>
    public static void SerializeJson(this RelatedArtifact current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("type",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.TypeElement.Value));

      if ((current.LabelElement != null) && (current.LabelElement.Value != null))
      {
        writer.WriteString("label",current.LabelElement.Value);
      }

      if ((current.DisplayElement != null) && (current.DisplayElement.Value != null))
      {
        writer.WriteString("display",current.DisplayElement.Value);
      }

      if ((current.Citation != null) && (current.Citation.Value != null))
      {
        writer.WriteString("citation",current.Citation.Value);
      }

      if ((current.UrlElement != null) && (current.UrlElement.Value != null))
      {
        writer.WriteString("url",current.UrlElement.Value);
      }

      if (current.Document != null)
      {
        writer.WritePropertyName("document");
        current.Document.SerializeJson(writer, options);
      }

      if ((current.ResourceElement != null) && (current.ResourceElement.Value != null))
      {
        writer.WriteString("resource",current.ResourceElement.Value);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR RelatedArtifact
    /// </summary>
    public static void DeserializeJson(this RelatedArtifact current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR RelatedArtifact
    /// </summary>
    public static void DeserializeJsonProperty(this RelatedArtifact current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "type":
          current.TypeElement =new Code<Hl7.Fhir.Model.RelatedArtifact.RelatedArtifactType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.RelatedArtifact.RelatedArtifactType>(reader.GetString()));
          break;

        case "label":
          current.LabelElement = new FhirString(reader.GetString());
          break;

        case "display":
          current.DisplayElement = new FhirString(reader.GetString());
          break;

        case "citation":
          current.Citation = new Markdown(reader.GetString());
          break;

        case "url":
          current.UrlElement = new FhirUrl(reader.GetString());
          break;

        case "document":
          current.Document = new Hl7.Fhir.Model.Attachment();
          current.Document.DeserializeJson(ref reader, options);
          break;

        case "resource":
          current.ResourceElement = new Canonical(reader.GetString());
          break;

        case "_resource":
          ((Hl7.Fhir.Model.Element)current.ResourceElement).DeserializeJson(ref reader, options);
          break;

      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class RelatedArtifactJsonConverter : JsonConverter<RelatedArtifact>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(RelatedArtifact).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, RelatedArtifact value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override RelatedArtifact Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        RelatedArtifact target = new RelatedArtifact();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
