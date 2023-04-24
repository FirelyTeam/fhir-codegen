﻿// <copyright file="FhirPrimitive.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>

using System;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Health.Fhir.CodeGenCommon.Models;

/// <summary>A class representing a FHIR primitive (r2:simple) type.</summary>
public class FhirPrimitive : FhirModelBase, ICloneable
{
    /// <summary>Initializes a new instance of the <see cref="FhirPrimitive"/> class.</summary>
    /// <param name="id">              The identifier.</param>
    /// <param name="name">            Name of this FHIR primitive.</param>
    /// <param name="baseTypeName">    The base type name for this primitive type.</param>
    /// <param name="version">         Version of this definition.</param>
    /// <param name="url">             URL of the resource.</param>
    /// <param name="publicationStatus">The publication status.</param>
    /// <param name="standardStatus">  The standard status.</param>
    /// <param name="fmmLevel">        The FHIR Maturity Model level.</param>
    /// <param name="isExperimental">  If this primitive type is marked experimental.</param>
    /// <param name="shortDescription">Information describing the short.</param>
    /// <param name="purpose">         The purpose of this definition.</param>
    /// <param name="comment">         The comment.</param>
    /// <param name="validationRegEx"> The validation RegEx.</param>
    public FhirPrimitive(
        string id,
        string name,
        string baseTypeName,
        string baseTypeCanonical,
        string version,
        Uri url,
        string publicationStatus,
        string standardStatus,
        int? fmmLevel,
        bool isExperimental,
        string shortDescription,
        string purpose,
        string comment,
        string validationRegEx,
        string narrative,
        string narrativeStatus,
        string fhirVersion)
        : base(
            FhirArtifactClassEnum.PrimitiveType,
            id,
            name,
            name,
            baseTypeName,
            baseTypeCanonical,
            version,
            url,
            publicationStatus,
            standardStatus,
            fmmLevel,
            isExperimental,
            shortDescription,
            purpose,
            comment,
            validationRegEx,
            narrative,
            narrativeStatus,
            fhirVersion)
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="FhirPrimitive"/> class.
    /// </summary>
    /// <param name="source">Source for the.</param>
    public FhirPrimitive(FhirPrimitive source)
        : this(
              source.Id,
              source.Name,
              source.BaseTypeName,
              source.BaseTypeCanonical,
              source.Version,
              source.URL,
              source.PublicationStatus,
              source.StandardStatus,
              source.FhirMaturityLevel,
              source.IsExperimental,
              source.ShortDescription,
              source.Purpose,
              source.Comment,
              source.ValidationRegEx,
              source.NarrativeText,
              source.NarrativeStatus,
              source.FhirVersion)
    {
    }

    /// <summary>Deep copy.</summary>
    /// <returns>A FhirPrimitive.</returns>
    public object Clone()
    {
        // generate the base object
        FhirPrimitive primitive = new FhirPrimitive(this);

        return primitive;
    }
}