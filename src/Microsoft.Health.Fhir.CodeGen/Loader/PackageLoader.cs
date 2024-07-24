﻿// <copyright file="PackageLoader.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>

using Hl7.Fhir.Language.Debugging;
using System.Diagnostics;
using System.Net;
using Hl7.Fhir.Model;
using Hl7.Fhir.Utility;
using Microsoft.Health.Fhir.CodeGen.Models;
using Microsoft.Health.Fhir.CodeGenCommon.Packaging;
using Microsoft.Health.Fhir.PackageManager;
using Hl7.Fhir.Serialization;
using System.Text.Json;
using Microsoft.Health.Fhir.CodeGen.FhirExtensions;
using Microsoft.Health.Fhir.CodeGenCommon.Models;

namespace Microsoft.Health.Fhir.CodeGen.Loader;

/// <summary>A package loader.</summary>
public class PackageLoader : IDisposable
{
    /// <summary>(Immutable) The cache.</summary>
    private readonly IFhirPackageClient _cache;

    private bool _disposedValue = false;

    /// <summary>(Immutable) The sorted order for definition types we want to load.</summary>
    private static readonly string[] _sortedLoadOrder =
    [
        "CodeSystem",
        "ValueSet",
        "StructureDefinition",
        "SearchParameter",
        "OperationDefinition",
        "CapabilityStatement",
        "Conformance",
        "ImplementationGuide",
        "CompartmentDefinition",
    ];

    /// <summary>True to automatically load expansions.</summary>
    private bool _autoLoadExpansions;

    /// <summary>True to load dependencies when loading packages.</summary>
    private bool _loadDependencies;

    /// <summary>The JSON model.</summary>
    private LoaderOptions.JsonDeserializationModel _jsonModel;

    /// <summary>Options for controlling the JSON.</summary>
    private JsonSerializerOptions _jsonOptions;

    /// <summary>The lenient JSON parser.</summary>
    private FhirJsonPocoDeserializer _jsonParser;

#if !DISABLE_XML
    /// <summary>The lenient XML parser.</summary>
    private FhirXmlPocoDeserializer _xmlParser;
#endif

    private Microsoft.Health.Fhir.CrossVersion.Converter_43_50? _converter_43_50 = null;
    private Microsoft.Health.Fhir.CrossVersion.Converter_30_50? _converter_30_50 = null;
    private Microsoft.Health.Fhir.CrossVersion.Converter_20_50? _converter_20_50 = null;

    private object _convertLockObject = new();

    /// <summary>Initializes a new instance of the <see cref="PackageLoader"/> class.</summary>
    /// <param name="cache">The cache.</param>
    /// <param name="opts"> Options for controlling the operation.</param>
    public PackageLoader(IFhirPackageClient cache, LoaderOptions opts)
    {
        _cache = cache;

        _jsonOptions = opts.FhirJsonOptions;
        _jsonParser = new(opts.FhirJsonSettings);

#if !DISABLE_XML
        _xmlParser = new(opts.FhirXmlSettings);
#endif
        _jsonModel = opts.JsonModel;

        _autoLoadExpansions = opts.AutoLoadExpansions;
        _loadDependencies = opts.ResolvePackageDependencies;
    }

    /// <summary>Adds all interaction parameters to a core definition collection.</summary>
    /// <param name="dc">The device-context.</param>
    private void AddAllInteractionParameters(DefinitionCollection dc)
    {
        dc.AddHttpQueryParameter(new()
        {
            Name = "_format",
            Url = "http://hl7.org/fhir/http.html#format",
            Description = "Parameter to specify alternative response formats by their MIME-types.",
            ParamType = SearchParamType.String,
        });

        dc.AddHttpQueryParameter(new()
        {
            Name = "_summary",
            Url = "http://hl7.org/fhir/search.html#summary",
            Description = "Request to return a portion of matching resources.",
            ParamType = SearchParamType.Token,
            AllowedValues = ["true", "false", "count", "data", "text",],
        });

        dc.AddHttpQueryParameter(new()
        {
            Name = "_elements",
            Url = "http://hl7.org/fhir/search.html#elements",
            Description = "Request to return specific elements from resources.",
            ParamType = SearchParamType.Token,
        });

        // add parameters from R4 and later
        if (dc.FhirSequence >= FhirReleases.FhirSequenceCodes.R4)
        {
            dc.AddHttpQueryParameter(new()
            {
                Name = "_pretty",
                Url = "http://hl7.org/fhir/http.html#pretty",
                Description = "Ask for a pretty printed response for human convenience.",
                ParamType = SearchParamType.String,
                AllowedValues = [ "true", "false", ],
            });
        }
    }

    /// <summary>Adds a search result parameters to a core definition collection.</summary>
    /// <param name="dc">The device-context.</param>
    private void AddSearchResultParameters(DefinitionCollection dc)
    {
        dc.AddSearchResultParameter(new()
        {
            Name = "_sort",
            Url = "http://hl7.org/fhir/search.html#sort",
            Description = "Used to indicate which order to return the results, which can have a value of one of the search parameters.",
            ParamType = SearchParamType.String,
        });

        dc.AddSearchResultParameter(new()
        {
            Name = "_count",
            Url = "http://hl7.org/fhir/search.html#count",
            Description = "A hint to the server regarding how many resources should be returned in a single page. Servers SHALL NOT return more resources than requested but are allowed to return less than the client requested.",
            ParamType = SearchParamType.Number,
        });

        dc.AddSearchResultParameter(new()
        {
            Name = "_include",
            Url = "http://hl7.org/fhir/search.html#include",
            Description = "Request to return resources related to the search results, by moving forward across references.",
            ParamType = SearchParamType.String,
        });

        dc.AddSearchResultParameter(new()
        {
            Name = "_revinclude",
            Url = "http://hl7.org/fhir/search.html#revinclude",
            Description = "Request to return resources related to the search results, by moving backwards across references.",
            ParamType = SearchParamType.String,
        });

        dc.AddSearchResultParameter(new()
        {
            Name = "_contained",
            Url = "http://hl7.org/fhir/search.html#contained",
            Description = "Request modification to handling of contained resource searching.",
            ParamType = SearchParamType.Token,
            AllowedValues = [ "true", "false", "both", ],
        });

        dc.AddSearchResultParameter(new()
        {
            Name = "_containedType",
            Url = "http://hl7.org/fhir/search.html#containedType",
            Description = "When contained resources are being returned, whether the server should return either the container or the contained resource.",
            ParamType = SearchParamType.Token,
            AllowedValues = [ "container", "contained", ],
        });

        // add parameters from R4 and later
        if (dc.FhirSequence >= FhirReleases.FhirSequenceCodes.R4)
        {
            dc.AddSearchResultParameter(new()
            {
                Name = "_total",
                Url = "http://hl7.org/fhir/search.html#total",
                Description = "Optimization hint for servers indicating reliance on the Bundle.total element.",
                ParamType = SearchParamType.Token,
                AllowedValues = [ "none", "estimate", "accurate" ],
            });
        }
    }

    /// <summary>Adds a missing core search parameters to a core definition collection.</summary>
    /// <param name="dc">The device-context.</param>
    private void AddMissingCoreSearchParameters(DefinitionCollection dc, string packageId, string packageVersion)
    {
        dc.AddSearchParameter(new()
        {
            Id = "Resource-content",
            Name = "_content",
            Code = "_content",
            Url = "http://hl7.org/fhir/SearchParameter/Resource-content",
            Version = dc.FhirSequence.ToLongVersion(),
            Title = "Resource content filter",
            Status = PublicationStatus.Active,
            Description = "Search on the entire content of the resource.",
            Base = [ VersionIndependentResourceTypesAll.Resource ],
            Type = SearchParamType.Special,
        }, packageId, packageVersion, true);

        dc.AddSearchParameter(new()
        {
            Id = "Resource-filter",
            Name = "_filter",
            Code = "_filter",
            Url = "http://hl7.org/fhir/SearchParameter/Resource-filter",
            Version = dc.FhirSequence.ToLongVersion(),
            Title = "Advanced search filter",
            Status = PublicationStatus.Active,
            Description = "Filter search parameter which supports a more sophisticated grammar for searching.",
            Base = [ VersionIndependentResourceTypesAll.Resource ],
            Type = SearchParamType.Special,
        }, packageId, packageVersion, true);

        dc.AddSearchParameter(new()
        {
            Id = "Resource-text",
            Name = "_text",
            Code = "_text",
            Url = "http://hl7.org/fhir/SearchParameter/Resource-text",
            Version = dc.FhirSequence.ToLongVersion(),
            Title = "Resource text filter",
            Status = PublicationStatus.Active,
            Description = "Search the narrative content of a resource.",
            Base = [ VersionIndependentResourceTypesAll.Resource ],
            Type = SearchParamType.String,
        }, packageId, packageVersion, true);

        dc.AddSearchParameter(new()
        {
            Id = "Resource-list",
            Name = "_list",
            Code = "_list",
            Url = "http://hl7.org/fhir/SearchParameter/Resource-list",
            Version = dc.FhirSequence.ToLongVersion(),
            Title = "List reference filter",
            Status = PublicationStatus.Active,
            Description = "Filter based on resources referenced by a List resource.",
            Base = [ VersionIndependentResourceTypesAll.Resource ],
            Type = SearchParamType.Reference,
            Target = [ VersionIndependentResourceTypesAll.List ],
        }, packageId, packageVersion, true);

        if (dc.FhirSequence >= FhirReleases.FhirSequenceCodes.R4)
        {
            dc.AddSearchParameter(new()
            {
                Id = "Resource-has",
                Name = "_has",
                Code = "_has",
                Url = "http://hl7.org/fhir/SearchParameter/Resource-has",
                Version = dc.FhirSequence.ToLongVersion(),
                Title = "Limited support for reverse chaining",
                Status = PublicationStatus.Active,
                Description = "For selecting resources based on the properties of resources that refer to them.",
                Base = [ VersionIndependentResourceTypesAll.Resource ],
                Type = SearchParamType.Special,
            }, packageId, packageVersion, true);

            dc.AddSearchParameter(new()
            {
                Id = "Resource-type",
                Name = "_type",
                Code = "_type",
                Url = "http://hl7.org/fhir/SearchParameter/Resource-type",
                Version = dc.FhirSequence.ToLongVersion(),
                Title = "Resource type filter",
                Status = PublicationStatus.Active,
                Description = "For filtering resources based on their type in searches across resource types.",
                Base = [ VersionIndependentResourceTypesAll.Resource ],
                Type = SearchParamType.Token,
            }, packageId, packageVersion, true);
        }
    }

    /// <summary>Loads a package.</summary>
    /// <exception cref="Exception">Thrown when an exception error condition occurs.</exception>
    /// <param name="name">    The name.</param>
    /// <param name="packages">The cached package.</param>
    /// <returns>An asynchronous result that yields the package.</returns>
    public async Task<DefinitionCollection?> LoadPackages(
        string name,
        IEnumerable<PackageCacheEntry> packages,
        DefinitionCollection? inProgress = null)
    {
        DefinitionCollection definitions = inProgress ?? new()
        {
            Name = name,
        };

        foreach (PackageCacheEntry cachedPackage in packages)
        {
            // check if we are flagged to load expansions and this is a core package
            if (_autoLoadExpansions && FhirPackageUtils.PackageIsFhirCore(cachedPackage.Name))
            {
                string expansionPackageName = cachedPackage.Name.Replace(".core", ".expansions");
                string expansionDirective = expansionPackageName + "#" + cachedPackage.Version;

                Console.WriteLine($"Auto-loading core expansions: {cachedPackage.ResolvedDirective}...");

                PackageCacheEntry? expansion = _cache.FindOrDownloadPackageByDirective(expansionDirective).Result;

                if (expansion == null)
                {
                    throw new Exception($"Could not find or download expansion package: {expansionDirective}");
                }

                await LoadPackages(name, [expansion], definitions);
            }

            // skip if we have already loaded this package
            if (definitions.Manifests.ContainsKey(cachedPackage.ResolvedDirective))
            {
                Console.WriteLine($"Skipping already loaded dependency: {cachedPackage.ResolvedDirective}");
                continue;
            }

            Console.WriteLine($"Processing {cachedPackage.ResolvedDirective}...");

            CachePackageManifest? manifest = _cache.GetManifest(cachedPackage) ?? throw new Exception("Failed to load package manifest");

            definitions.Manifests.Add(cachedPackage.ResolvedDirective, manifest);

            if (string.IsNullOrEmpty(definitions.MainPackageId) || name.Equals(manifest.Name, StringComparison.OrdinalIgnoreCase))
            {
                definitions.MainPackageId = manifest.Name;
                definitions.MainPackageVersion = manifest.Version;
                definitions.MainPackageCanonical = manifest.CanonicalUrl;
            }

            // update the collection FHIR version based on the first package we come across with one
            if (string.IsNullOrEmpty(definitions.FhirVersionLiteral) && manifest.AllFhirVersions.Any())
            {
                definitions.FhirVersionLiteral = manifest.AllFhirVersions.First();

                definitions.FhirSequence = FhirReleases.FhirVersionToSequence(manifest.AllFhirVersions.First());

                definitions.FhirVersion = definitions.FhirSequence switch
                {
                    FhirReleases.FhirSequenceCodes.Unknown => null,
                    FhirReleases.FhirSequenceCodes.DSTU2 => FHIRVersion.N1_0,
                    FhirReleases.FhirSequenceCodes.STU3 => FHIRVersion.N3_0,
                    FhirReleases.FhirSequenceCodes.R4 => FHIRVersion.N4_0,
                    FhirReleases.FhirSequenceCodes.R4B => FHIRVersion.N4_3,
                    FhirReleases.FhirSequenceCodes.R5 => FHIRVersion.N5_0,
                    _ => null,
                };
            }

            FhirReleases.FhirSequenceCodes packageFhirVersion = manifest.AllFhirVersions.Any()
                ? FhirReleases.FhirVersionToSequence(manifest.AllFhirVersions.First())
                : definitions.FhirSequence;

            // create the converter we need
            switch (definitions.FhirSequence)
            {
                case FhirReleases.FhirSequenceCodes.DSTU2:
                    {
                        if (_converter_20_50 == null)
                        {
                            lock (_convertLockObject)
                            {
                                _converter_20_50 ??= new();
                            }
                        }
                    }
                    break;

                case FhirReleases.FhirSequenceCodes.STU3:
                    {
                        if (_converter_30_50 == null)
                        {
                            lock (_convertLockObject)
                            {
                                _converter_30_50 ??= new();
                            }
                        }
                    }
                    break;

                case FhirReleases.FhirSequenceCodes.R4:
                case FhirReleases.FhirSequenceCodes.R4B:
                    {
                        if (_converter_43_50 == null)
                        {
                            lock (_convertLockObject)
                            {
                                _converter_43_50 ??= new();
                            }
                        }
                    }
                    break;

                default:
                case FhirReleases.FhirSequenceCodes.R5:
                    {
                    }
                    break;
            }

            // if we are resolving dependencies, do those now
            if (_loadDependencies && (cachedPackage.ResolvedDependencies.Length != 0))
            {
                await LoadPackages(name, cachedPackage.ResolvedDependencies, definitions);
                Console.WriteLine($"Dependencies resolved - loading package {cachedPackage.ResolvedDirective}...");
            }
            else
            {
                Console.WriteLine($"Loading {cachedPackage.ResolvedDirective}...");
            }

            // grab the contents of our package
            PackageContents? packageContents = _cache.GetIndexedContents(cachedPackage) ?? throw new Exception("Failed to load package contents");

            if (string.IsNullOrEmpty(cachedPackage.Directory))
            {
                throw new Exception("Package directory is empty");
            }

            if (packageContents.Files.Length == 0)
            {
                throw new Exception("Package contents are empty");
            }

            definitions.ContentListings.Add(cachedPackage.ResolvedDirective, packageContents);

            // create an dictionary of indexes we are going to load - note that we are essentially traversing twice, but that is better than projecting each time
            List<int>[] sortedFileIndexes = new List<int>[_sortedLoadOrder.Length];

            for (int i = 0; i < sortedFileIndexes.Length; i++)
            {
                sortedFileIndexes[i] = [];
            }

            // traverse our files
            int fileIndex = 0;
            foreach (PackageContents.PackageFile pf in packageContents.Files)
            {
                int loadIndex = Array.IndexOf(_sortedLoadOrder, pf.ResourceType);
                if (loadIndex < 0)
                {
                    fileIndex++;
                    continue;
                }

                // add this index to the list of indexes for this type
                sortedFileIndexes[loadIndex].Add(fileIndex++);
            }

            // iterate over our sorted list of indexes
            for (int i = 0; i < _sortedLoadOrder.Length; i++)
            {
                string rt = _sortedLoadOrder[i];

                Type? netType = Hl7.Fhir.Model.ModelInfo.GetTypeForFhirType(rt);

                if (netType == null)
                {
                    if (rt == "Conformance")
                    {
                        netType = typeof(CapabilityStatement);
                    }
                    else
                    {
                        throw new Exception($"Failed to find .NET type for FHIR type {rt}");
                    }
                }

                foreach (int fi in sortedFileIndexes[i])
                {
                    PackageContents.PackageFile pFile = packageContents.Files[fi];

                    // load the file
                    string path = Path.Combine(cachedPackage.Directory, "package", pFile.FileName);

                    if (!File.Exists(path))
                    {
                        throw new Exception($"Listed file {cachedPackage.ResolvedDirective}:{pFile.FileName} does not exist");
                    }

                    string fileExtension = Path.GetExtension(pFile.FileName);

                    object? r;

                    switch (definitions.FhirSequence)
                    {
                        case FhirReleases.FhirSequenceCodes.DSTU2:
                            {
                                r = ParseContents20(fileExtension, path: path);
                            }
                            break;

                        case FhirReleases.FhirSequenceCodes.STU3:
                            {
                                r = ParseContents30(fileExtension, path: path);
                            }
                            break;

                        case FhirReleases.FhirSequenceCodes.R4:
                        case FhirReleases.FhirSequenceCodes.R4B:
                            {
                                r = ParseContents43(fileExtension, path: path);
                            }
                            break;

                        default:
                        case FhirReleases.FhirSequenceCodes.R5:
                            {
                                if (_jsonModel == LoaderOptions.JsonDeserializationModel.SystemTextJson)
                                {
                                    r = ParseContentsSystemTextStream(fileExtension, netType, path: path);
                                }
                                else
                                {
                                    r = ParseContentsPoco(fileExtension, path);
                                }
                            }
                            break;
                    }

                    if (r == null)
                    {
                        throw new Exception($"Failed to parse {rt} {cachedPackage.ResolvedDirective}:{pFile.FileName}");
                    }

                    definitions.AddResource(r, packageFhirVersion, manifest.Name, manifest.Version, manifest.CanonicalUrl);
                }
            }

            // check to see if this package is a 'core' FHIR package to add missing contents
            if (manifest.Type.Equals("core", StringComparison.OrdinalIgnoreCase) ||
                manifest.Type.Equals("fhir.core", StringComparison.OrdinalIgnoreCase))
            {
                AddMissingCoreSearchParameters(definitions, manifest.Name, manifest.Version);
                AddAllInteractionParameters(definitions);
                AddSearchResultParameters(definitions);
            }
        }

        // generate any missing Snapshots - note this has to be done after all resources are loaded so dependencies can be resolved
        _ = await definitions.TryGenerateMissingSnapshots();

        return definitions;
    }

    /// <summary>Parse contents.</summary>
    /// <exception cref="Exception">Thrown when an exception error condition occurs.</exception>
    /// <param name="format"> Describes the format to use.</param>
    /// <param name="path">   (Optional) Full pathname of the file.</param>
    /// <param name="content">(Optional) The content.</param>
    /// <returns>A TResource?</returns>
    public object? ParseContentsPoco(string format, string path = "", string content = "")
    {
        switch (format.ToLowerInvariant())
        {
            case ".json":
            case "json":
            case "fhir+json":
            case "application/json":
            case "application/fhir+json":
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllTextAsync(path).Result;
                    }

                    // always use lenient parsing
                    Resource parsed = _jsonParser.DeserializeResource(content);
                    return parsed;
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing JSON: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing JSON: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }

            case ".xml":
            case "xml":
            case "fhir+xml":
            case "application/xml":
            case "application/fhir+xml":
#if DISABLE_XML
                throw new Exception("XML is currently disabled");
#else
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllTextAsync(path).Result;
                    }

                    // always use lenient parsing
                    Resource parsed = _xmlParser.DeserializeResource(content);
                    return parsed;
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing XML: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing XML: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }
#endif
            default:
                {
                    Console.WriteLine($"Unsupported parse format: {format}");
                    return null;
                }
        }
    }

    /// <summary>Parse contents of in the FHIR R4B format.</summary>
    /// <typeparam name="TResource">Type of the resource.</typeparam>
    /// <param name="format">Describes the format to use.</param>
    /// <param name="path">  Full pathname of the file.</param>
    /// <returns>An asynchronous result that yields a TResource?</returns>
    public object? ParseContents43(string format, string path = "", string content = "")
    {
        switch (format.ToLowerInvariant())
        {
            case ".json":
            case "json":
            case "fhir+json":
            case "application/json":
            case "application/fhir+json":
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllText(path);
                    }

                    Hl7.Fhir.ElementModel.ISourceNode sn = FhirJsonNode.Parse(content);
                    return _converter_43_50!.Convert(sn);
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing R4B JSON: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing R4B JSON: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }

            case ".xml":
            case "xml":
            case "fhir+xml":
            case "application/xml":
            case "application/fhir+xml":
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllText(path);
                    }

                    Hl7.Fhir.ElementModel.ISourceNode sn = FhirXmlNode.Parse(content);
                    return _converter_43_50!.Convert(sn);
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing R4B XML: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing R4B XML: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }

            default:
                {
                    Console.WriteLine($"Unsupported parse format: {format}");
                    return null;
                }
        }
    }

    public object? ParseContents30(string format, string path = "", string content = "")
    {
        switch (format.ToLowerInvariant())
        {
            case ".json":
            case "json":
            case "fhir+json":
            case "application/json":
            case "application/fhir+json":
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllText(path);
                    }

                    Hl7.Fhir.ElementModel.ISourceNode sn = FhirJsonNode.Parse(content);
                    return _converter_30_50!.Convert(sn);
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing STU3 JSON: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing STU3 JSON: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }

            case ".xml":
            case "xml":
            case "fhir+xml":
            case "application/xml":
            case "application/fhir+xml":
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllTextAsync(path).Result;
                    }

                    Hl7.Fhir.ElementModel.ISourceNode sn = FhirXmlNode.Parse(content);
                    return _converter_30_50!.Convert(sn);
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing STU3 XML: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing STU3 XML: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }

            default:
                {
                    Console.WriteLine($"Unsupported parse format: {format}");
                    return null;
                }
        }
    }

    public object? ParseContents20(string format, string path = "", string content = "")
    {
        switch (format.ToLowerInvariant())
        {
            case ".json":
            case "json":
            case "fhir+json":
            case "application/json":
            case "application/fhir+json":
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllTextAsync(path).Result;
                    }

                    Hl7.Fhir.ElementModel.ISourceNode sn = FhirJsonNode.Parse(content);
                    return _converter_20_50!.Convert(sn);
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing DSTU2 JSON: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing DSTU2 JSON: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }

            case ".xml":
            case "xml":
            case "fhir+xml":
            case "application/xml":
            case "application/fhir+xml":
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllTextAsync(path).Result;
                    }

                    Hl7.Fhir.ElementModel.ISourceNode sn = FhirXmlNode.Parse(content);
                    return _converter_20_50!.Convert(sn);
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing DSTU2 XML: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing DSTU2 XML: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }

            default:
                {
                    Console.WriteLine($"Unsupported parse format: {format}");
                    return null;
                }
        }
    }

    /// <summary>Parse the contents of a resource in the specified format.</summary>
    /// <exception cref="Exception">Thrown when an exception error condition occurs.</exception>
    /// <param name="format">      The format of the content.</param>
    /// <param name="resourceType">Type of the resource.</param>
    /// <param name="path">        (Optional) Full pathname of the file.</param>
    /// <param name="content">     (Optional) The content.</param>
    /// <returns>
    /// The parsed resource of type <typeparamref name="TResource"/> or null if parsing fails.
    /// </returns>
    public object? ParseContentsSystemTextStream(string format, Type resourceType, string path = "", string content = "")
    {
        switch (format.ToLowerInvariant())
        {
            case ".json":
            case "json":
            case "fhir+json":
            case "application/json":
            case "application/fhir+json":
                try
                {
                    if (!string.IsNullOrEmpty(content))
                    {
                        return JsonSerializer.Deserialize(content, resourceType, _jsonOptions);
                    }

                    using (FileStream fs = new(path, FileMode.Open, FileAccess.Read, FileShare.Read))
                    {
                        return JsonSerializer.Deserialize(fs, resourceType, _jsonOptions);
                    }
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing JSON: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing JSON: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }

            case ".xml":
            case "xml":
            case "fhir+xml":
            case "application/xml":
            case "application/fhir+xml":
#if DISABLE_XML
                throw new Exception("XML is currently disabled");
#else
                try
                {
                    if (string.IsNullOrEmpty(content))
                    {
                        content = File.ReadAllText(path);
                    }

                    // always use lenient parsing
                    Resource parsed = _xmlParser.DeserializeResource(content);
                    return parsed;
                }
                catch (Exception ex)
                {
                    if (ex.InnerException == null)
                    {
                        Console.WriteLine($"Error parsing XML: {ex.Message}");
                    }
                    else
                    {
                        Console.WriteLine($"Error parsing XML: {ex.Message} ({ex.InnerException.Message})");
                    }
                    return null;
                }
#endif
            default:
                {
                    Console.WriteLine($"Unsupported parse format: {format}");
                    return null;
                }
        }
    }

    /// <summary>Error handler as mitigation for https://github.com/FirelyTeam/firely-net-sdk/issues/2701. </summary>
    /// <param name="reader">           [in,out] The reader.</param>
    /// <param name="targetType">       Type of the target.</param>
    /// <param name="originalValue">    The original value.</param>
    /// <param name="originalException">Details of the exception.</param>
    /// <returns>A Tuple.</returns>
    public static (object?, FhirJsonException?) LocalPrimitiveParseHandler(
        ref Utf8JsonReader reader,
        Type targetType,
        object? originalValue,
        FhirJsonException originalException)
    {
        if (targetType == typeof(long))
        {
            if (originalValue is long ol)
            {
                return (ol, null);
            }

            if (originalValue is string s)
            {
                if (long.TryParse(s, out long l))
                {
                    return (l, null);
                }
            }
        }

        return (originalValue, originalException);
    }


    /// <summary>
    /// Releases the unmanaged resources used by the <see cref="FhirCache"/>
    /// and optionally releases the managed resources.
    /// </summary>
    /// <param name="disposing">True to release both managed and unmanaged resources; false to
    ///  release only unmanaged resources.</param>
    protected virtual void Dispose(bool disposing)
    {
        if (!_disposedValue)
        {
            if (disposing)
            {
                // TODO: dispose managed state (managed objects)
                if (_converter_43_50 != null)
                {
                    lock (_convertLockObject)
                    {
                        _converter_43_50 = null;
                    }
                }

                if (_converter_30_50 != null)
                {
                    lock (_convertLockObject)
                    {
                        _converter_30_50 = null;
                    }
                }

                if (_converter_20_50 != null)
                {
                    lock (_convertLockObject)
                    {
                        _converter_20_50 = null;
                    }
                }
            }

            // TODO: free unmanaged resources (unmanaged objects) and override finalizer
            // TODO: set large fields to null
            _disposedValue = true;
        }
    }

    /// <summary>
    /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged
    /// resources.
    /// </summary>
    void IDisposable.Dispose()
    {
        // Do not change this code. Put cleanup code in 'Dispose(bool disposing)' method
        Dispose(disposing: true);
        GC.SuppressFinalize(this);
    }
}