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
  /// JSON Serialization Extensions for DetectedIssue
  /// </summary>
  public static class DetectedIssueJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR DetectedIssue into JSON
    /// </summary>
    public static void SerializeJson(this DetectedIssue current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","DetectedIssue");
      // Complex: DetectedIssue, Export: DetectedIssue, Base: DomainResource (DomainResource)
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

      writer.WriteString("status",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StatusElement.Value));

      if (current.Code != null)
      {
        writer.WritePropertyName("code");
        current.Code.SerializeJson(writer, options);
      }

      if (current.SeverityElement != null)
      {
        if (current.SeverityElement.Value != null)
        {
          writer.WriteString("severity",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.SeverityElement.Value));
        }
        if (current.SeverityElement.HasExtensions() || (!string.IsNullOrEmpty(current.SeverityElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_severity",false,current.SeverityElement.Extension,current.SeverityElement.ElementId);
        }
      }

      if (current.Patient != null)
      {
        writer.WritePropertyName("patient");
        current.Patient.SerializeJson(writer, options);
      }

      if (current.Identified != null)
      {
        switch (current.Identified)
        {
          case FhirDateTime v_FhirDateTime:
            if (v_FhirDateTime != null)
            {
              if (!string.IsNullOrEmpty(v_FhirDateTime.Value))
              {
                writer.WriteString("identifiedDateTime",v_FhirDateTime.Value);
              }
              if (v_FhirDateTime.HasExtensions() || (!string.IsNullOrEmpty(v_FhirDateTime.ElementId)))
              {
                JsonStreamUtilities.SerializeExtensionList(writer,options,"_identifiedDateTime",false,v_FhirDateTime.Extension,v_FhirDateTime.ElementId);
              }
            }
            break;
          case Period v_Period:
            writer.WritePropertyName("identifiedPeriod");
            v_Period.SerializeJson(writer, options);
            break;
        }
      }
      if (current.Author != null)
      {
        writer.WritePropertyName("author");
        current.Author.SerializeJson(writer, options);
      }

      if ((current.Implicated != null) && (current.Implicated.Count != 0))
      {
        writer.WritePropertyName("implicated");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Implicated)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Evidence != null) && (current.Evidence.Count != 0))
      {
        writer.WritePropertyName("evidence");
        writer.WriteStartArray();
        foreach (DetectedIssue.EvidenceComponent val in current.Evidence)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.DetailElement != null)
      {
        if (!string.IsNullOrEmpty(current.DetailElement.Value))
        {
          writer.WriteString("detail",current.DetailElement.Value);
        }
        if (current.DetailElement.HasExtensions() || (!string.IsNullOrEmpty(current.DetailElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_detail",false,current.DetailElement.Extension,current.DetailElement.ElementId);
        }
      }

      if (current.ReferenceElement != null)
      {
        if (!string.IsNullOrEmpty(current.ReferenceElement.Value))
        {
          writer.WriteString("reference",current.ReferenceElement.Value);
        }
        if (current.ReferenceElement.HasExtensions() || (!string.IsNullOrEmpty(current.ReferenceElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_reference",false,current.ReferenceElement.Extension,current.ReferenceElement.ElementId);
        }
      }

      if ((current.Mitigation != null) && (current.Mitigation.Count != 0))
      {
        writer.WritePropertyName("mitigation");
        writer.WriteStartArray();
        foreach (DetectedIssue.MitigationComponent val in current.Mitigation)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DetectedIssue
    /// </summary>
    public static void DeserializeJson(this DetectedIssue current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          if (Hl7.Fhir.Serialization.FhirSerializerOptions.Debug) { Console.WriteLine($"DetectedIssue >>> DetectedIssue.{propertyName}, depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}"); }
          reader.Read();
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException($"DetectedIssue: invalid state! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DetectedIssue
    /// </summary>
    public static void DeserializeJsonProperty(this DetectedIssue current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"DetectedIssue error reading 'identifier' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Identifier v_Identifier = new Hl7.Fhir.Model.Identifier();
            v_Identifier.DeserializeJson(ref reader, options);
            current.Identifier.Add(v_Identifier);

            if (!reader.Read())
            {
              throw new JsonException($"DetectedIssue error reading 'identifier' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Identifier.Count == 0)
          {
            current.Identifier = null;
          }
          break;

        case "status":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.StatusElement = new Code<Hl7.Fhir.Model.ObservationStatus>();
            reader.Skip();
          }
          else
          {
            current.StatusElement = new Code<Hl7.Fhir.Model.ObservationStatus>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.ObservationStatus>(reader.GetString()));
          }
          break;

        case "_status":
          if (current.StatusElement == null) { current.StatusElement = new Code<Hl7.Fhir.Model.ObservationStatus>(); }
          ((Hl7.Fhir.Model.Element)current.StatusElement).DeserializeJson(ref reader, options);
          break;

        case "code":
          current.Code = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Code).DeserializeJson(ref reader, options);
          break;

        case "severity":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.SeverityElement = new Code<Hl7.Fhir.Model.DetectedIssue.DetectedIssueSeverity>();
            reader.Skip();
          }
          else
          {
            current.SeverityElement = new Code<Hl7.Fhir.Model.DetectedIssue.DetectedIssueSeverity>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.DetectedIssue.DetectedIssueSeverity>(reader.GetString()));
          }
          break;

        case "_severity":
          if (current.SeverityElement == null) { current.SeverityElement = new Code<Hl7.Fhir.Model.DetectedIssue.DetectedIssueSeverity>(); }
          ((Hl7.Fhir.Model.Element)current.SeverityElement).DeserializeJson(ref reader, options);
          break;

        case "patient":
          current.Patient = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Patient).DeserializeJson(ref reader, options);
          break;

        case "identifiedDateTime":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.Identified = new FhirDateTime();
            reader.Skip();
          }
          else
          {
            current.Identified = new FhirDateTime(reader.GetString());
          }
          break;

        case "_identifiedDateTime":
          if (current.Identified == null) { current.Identified = new FhirDateTime(); }
          ((Hl7.Fhir.Model.Element)current.Identified).DeserializeJson(ref reader, options);
          break;

        case "identifiedPeriod":
          current.Identified = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Identified).DeserializeJson(ref reader, options);
          break;

        case "author":
          current.Author = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Author).DeserializeJson(ref reader, options);
          break;

        case "implicated":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"DetectedIssue error reading 'implicated' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Implicated = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Implicated = new Hl7.Fhir.Model.ResourceReference();
            v_Implicated.DeserializeJson(ref reader, options);
            current.Implicated.Add(v_Implicated);

            if (!reader.Read())
            {
              throw new JsonException($"DetectedIssue error reading 'implicated' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Implicated.Count == 0)
          {
            current.Implicated = null;
          }
          break;

        case "evidence":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"DetectedIssue error reading 'evidence' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Evidence = new List<DetectedIssue.EvidenceComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.DetectedIssue.EvidenceComponent v_Evidence = new Hl7.Fhir.Model.DetectedIssue.EvidenceComponent();
            v_Evidence.DeserializeJson(ref reader, options);
            current.Evidence.Add(v_Evidence);

            if (!reader.Read())
            {
              throw new JsonException($"DetectedIssue error reading 'evidence' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Evidence.Count == 0)
          {
            current.Evidence = null;
          }
          break;

        case "detail":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.DetailElement = new FhirString();
            reader.Skip();
          }
          else
          {
            current.DetailElement = new FhirString(reader.GetString());
          }
          break;

        case "_detail":
          if (current.DetailElement == null) { current.DetailElement = new FhirString(); }
          ((Hl7.Fhir.Model.Element)current.DetailElement).DeserializeJson(ref reader, options);
          break;

        case "reference":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.ReferenceElement = new FhirUri();
            reader.Skip();
          }
          else
          {
            current.ReferenceElement = new FhirUri(reader.GetString());
          }
          break;

        case "_reference":
          if (current.ReferenceElement == null) { current.ReferenceElement = new FhirUri(); }
          ((Hl7.Fhir.Model.Element)current.ReferenceElement).DeserializeJson(ref reader, options);
          break;

        case "mitigation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"DetectedIssue error reading 'mitigation' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Mitigation = new List<DetectedIssue.MitigationComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.DetectedIssue.MitigationComponent v_Mitigation = new Hl7.Fhir.Model.DetectedIssue.MitigationComponent();
            v_Mitigation.DeserializeJson(ref reader, options);
            current.Mitigation.Add(v_Mitigation);

            if (!reader.Read())
            {
              throw new JsonException($"DetectedIssue error reading 'mitigation' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Mitigation.Count == 0)
          {
            current.Mitigation = null;
          }
          break;

        // Complex: DetectedIssue, Export: DetectedIssue, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR DetectedIssue#Evidence into JSON
    /// </summary>
    public static void SerializeJson(this DetectedIssue.EvidenceComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: DetectedIssue#Evidence, Export: EvidenceComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if ((current.Code != null) && (current.Code.Count != 0))
      {
        writer.WritePropertyName("code");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Code)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Detail != null) && (current.Detail.Count != 0))
      {
        writer.WritePropertyName("detail");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Detail)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DetectedIssue#Evidence
    /// </summary>
    public static void DeserializeJson(this DetectedIssue.EvidenceComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          if (Hl7.Fhir.Serialization.FhirSerializerOptions.Debug) { Console.WriteLine($"DetectedIssue.EvidenceComponent >>> DetectedIssue#Evidence.{propertyName}, depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}"); }
          reader.Read();
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException($"DetectedIssue.EvidenceComponent: invalid state! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DetectedIssue#Evidence
    /// </summary>
    public static void DeserializeJsonProperty(this DetectedIssue.EvidenceComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "code":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"EvidenceComponent error reading 'code' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Code = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Code = new Hl7.Fhir.Model.CodeableConcept();
            v_Code.DeserializeJson(ref reader, options);
            current.Code.Add(v_Code);

            if (!reader.Read())
            {
              throw new JsonException($"EvidenceComponent error reading 'code' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Code.Count == 0)
          {
            current.Code = null;
          }
          break;

        case "detail":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"EvidenceComponent error reading 'detail' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Detail = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Detail = new Hl7.Fhir.Model.ResourceReference();
            v_Detail.DeserializeJson(ref reader, options);
            current.Detail.Add(v_Detail);

            if (!reader.Read())
            {
              throw new JsonException($"EvidenceComponent error reading 'detail' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Detail.Count == 0)
          {
            current.Detail = null;
          }
          break;

        // Complex: evidence, Export: EvidenceComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR DetectedIssue#Mitigation into JSON
    /// </summary>
    public static void SerializeJson(this DetectedIssue.MitigationComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: DetectedIssue#Mitigation, Export: MitigationComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WritePropertyName("action");
      current.Action.SerializeJson(writer, options);

      if (current.DateElement != null)
      {
        if (!string.IsNullOrEmpty(current.DateElement.Value))
        {
          writer.WriteString("date",current.DateElement.Value);
        }
        if (current.DateElement.HasExtensions() || (!string.IsNullOrEmpty(current.DateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_date",false,current.DateElement.Extension,current.DateElement.ElementId);
        }
      }

      if (current.Author != null)
      {
        writer.WritePropertyName("author");
        current.Author.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DetectedIssue#Mitigation
    /// </summary>
    public static void DeserializeJson(this DetectedIssue.MitigationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          if (Hl7.Fhir.Serialization.FhirSerializerOptions.Debug) { Console.WriteLine($"DetectedIssue.MitigationComponent >>> DetectedIssue#Mitigation.{propertyName}, depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}"); }
          reader.Read();
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException($"DetectedIssue.MitigationComponent: invalid state! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
    }

    /// <summary>
    /// Deserialize JSON into a FHIR DetectedIssue#Mitigation
    /// </summary>
    public static void DeserializeJsonProperty(this DetectedIssue.MitigationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "action":
          current.Action = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Action).DeserializeJson(ref reader, options);
          break;

        case "date":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.DateElement = new FhirDateTime();
            reader.Skip();
          }
          else
          {
            current.DateElement = new FhirDateTime(reader.GetString());
          }
          break;

        case "_date":
          if (current.DateElement == null) { current.DateElement = new FhirDateTime(); }
          ((Hl7.Fhir.Model.Element)current.DateElement).DeserializeJson(ref reader, options);
          break;

        case "author":
          current.Author = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Author).DeserializeJson(ref reader, options);
          break;

        // Complex: mitigation, Export: MitigationComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class DetectedIssueJsonConverter : JsonConverter<DetectedIssue>
    {
      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, DetectedIssue value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override DetectedIssue Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        DetectedIssue target = new DetectedIssue();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
