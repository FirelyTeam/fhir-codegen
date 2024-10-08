﻿// <copyright file="LinqExtensions.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>


using System.Collections;
using System.Runtime.CompilerServices;

namespace Microsoft.Health.Fhir.CodeGenCommon.Extensions;

/// <summary>A linq extensions.</summary>
public static class LinqExtensions
{
    /// <summary>
    /// An IEnumerable&lt;T&gt; extension method that applies an operation to all items in this
    /// collection.
    /// </summary>
    /// <typeparam name="T">Generic type parameter.</typeparam>
    /// <param name="ie">    The IE to act on.</param>
    /// <param name="action">The action.</param>
    /// <returns>True if it succeeds, false if it fails.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool ForEach<T>(this IEnumerable<T> ie, Func<T, int, bool> action)
    {
        int i = 0;
        foreach (T e in ie)
        {
            if (!action(e, i++))
            {
                return false;
            }
        }
        
        return true;
    }

    /// <summary>
    /// An Array extension method that applies an operation to all items in this collection.
    /// </summary>
    /// <typeparam name="T">The type of the elements in the array.</typeparam>
    /// <param name="ie">The array to act on.</param>
    /// <param name="action">The action to apply to each element.</param>
    /// <returns>True if the action succeeds for all elements, false otherwise.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool ForEach<T>(this Array ie, Func<T, bool, bool> action)
    {
        int lastIndex = ie.Length - 1;
        int i = 0;
        foreach (T e in ie)
        {
            if (!action(e, (i++ == lastIndex)))
            {
                return false;
            }
        }

        return true;
    }

    /// <summary>
    /// An IEnumerable&lt;T&gt; extension method that applies an operation to all items in this
    /// collection.
    /// </summary>
    /// <typeparam name="T">Generic type parameter.</typeparam>
    /// <param name="ie">    The IE to act on.</param>
    /// <param name="action">The action.</param>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void ForEach<T>(this IEnumerable<T> ie, Action<T> action)
    {
        foreach (T e in ie)
        {
            action(e);
        }
    }

    /// <summary>
    /// A Dictionary&lt;KT,VT&gt; extension method that deep copies the dictionary.
    /// </summary>
    /// <typeparam name="KT">Key Type.</typeparam>
    /// <typeparam name="VT">Value Type.</typeparam>
    /// <param name="source">The source dictionary to copy.</param>
    /// <returns>A Dictionary&lt;KT,VT&gt;</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Dictionary<KT,VT> DeepCopy<KT, VT>(this Dictionary<KT, VT> source)
        where KT : notnull
        where VT : ICloneable
    {
        Dictionary<KT, VT> dest = [];

        foreach (KeyValuePair<KT, VT> kvp in source)
        {
            dest.Add(kvp.Key, (VT)kvp.Value.Clone());
        }

        return dest;
    }

    /// <summary>
    /// A Dictionary&lt;KT,VT&gt; extension method that deep copies the dictionary.
    /// </summary>
    /// <typeparam name="KT">Key Type.</typeparam>
    /// <typeparam name="VT">Value Type.</typeparam>
    /// <param name="source">The source dictionary to copy.</param>
    /// <returns>A Dictionary&lt;KT,VT&gt;</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Dictionary<KT, List<VT>> DeepCopy<KT, VT>(this Dictionary<KT, List<VT>> source)
        where KT : notnull
        where VT : ICloneable
    {
        Dictionary<KT, List<VT>> dest = [];

        foreach (KeyValuePair<KT, List<VT>> kvp in source)
        {
            List<VT> list = [];

            foreach (VT value in kvp.Value)
            {
                list.Add((VT)value.Clone());
            }

            dest.Add(kvp.Key, list);
        }

        return dest;
    }

    /// <summary>
    /// A Dictionary&lt;KT,VT&gt; extension method that deep copies the dictionary.
    /// </summary>
    /// <param name="source">The source dictionary to copy.</param>
    /// <returns>A Dictionary&lt;KT,VT&gt;</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Dictionary<string, List<object>> DeepCopy(this Dictionary<string, List<object>> source)
    {
        Dictionary<string, List<object>> dest = [];

        foreach (KeyValuePair<string, List<object>> kvp in source)
        {
            List<object> list = [];

            foreach (object value in kvp.Value)
            {
                switch (value)
                {
                    case ICloneable cloneable: list.Add(cloneable.Clone()); break;
                    default: list.Add(value); break;
                }
            }

            dest.Add(kvp.Key, list);
        }

        return dest;
    }


    /// <summary>
    /// A Dictionary&lt;KT,VT&gt; extension method that shallow copies the given source.
    /// </summary>
    /// <typeparam name="KT">Key Type.</typeparam>
    /// <typeparam name="VT">Value Type.</typeparam>
    /// <param name="source">The source dictionary to copy.</param>
    /// <returns>A Dictionary&lt;KT,VT&gt;</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Dictionary<KT, VT> ShallowCopy<KT, VT>(this Dictionary<KT, VT> source)
        where KT : notnull
    {
        Dictionary<KT, VT> dest = [];

        foreach (KeyValuePair<KT, VT> kvp in source)
        {
            dest.Add(kvp.Key, kvp.Value);
        }

        return dest;
    }

    /// <summary>
    /// A Dictionary&lt;KT,VT&gt; extension method that shallow copies the given source.
    /// </summary>
    /// <typeparam name="KT">Type of the kt.</typeparam>
    /// <typeparam name="VT">Type of the vt.</typeparam>
    /// <param name="source">The source dictionary to copy.</param>
    /// <returns>A Dictionary&lt;KT,VT&gt;</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Dictionary<KT, List<VT>> ShallowCopy<KT, VT>(this Dictionary<KT, List<VT>> source)
        where KT : notnull
    {
        Dictionary<KT, List<VT>> dest = [];

        foreach (KeyValuePair<KT, List<VT>> kvp in source)
        {
            dest.Add(kvp.Key, kvp.Value);
        }

        return dest;
    }

    /// <summary>A HashSet&lt;string&gt; extension method that copies to.</summary>
    /// <param name="source">The source dictionary to copy.</param>
    /// <param name="dest">  Destination for the.</param>
    /// <returns>A HashSet&lt;string&gt;</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CopyTo(this HashSet<string> source, HashSet<string> dest)
    {
        foreach (string val in source)
        {
            dest.Add(val);
        }
    }

    /// <summary>
    /// A HashSet extension method that deep copies the HashSet.
    /// </summary>
    /// <param name="source">The source dictionary to copy.</param>
    /// <returns>A Dictionary&lt;KT,VT&gt;</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static HashSet<T> DeepCopy<T>(this HashSet<T> source)
    {
        HashSet<T> dest = [];

        dest.UnionWith(source);

        return dest;
    }

    /// <summary>A string extension method that converts a val to an enum.</summary>
    /// <typeparam name="T">Generic type parameter.</typeparam>
    /// <param name="val">The val to act on.</param>
    /// <returns>Val as a T?</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T? ToEnum<T>(this string val)
        where T : struct, System.Enum
    {
        if (val.TryFhirEnum(out T v))
        {
            return v;
        }

        return null;
    }

    /// <summary>A string extension method that converts a val to an enum.</summary>
    /// <typeparam name="T">Generic type parameter.</typeparam>
    /// <param name="source">    The source dictionary to copy.</param>
    /// <param name="defaultVal">(Optional) The default value.</param>
    /// <returns>Val as a T?</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IEnumerable<T> ToEnum<T>(
        this IEnumerable<string> source,
        T? defaultVal = null)
        where T : struct, System.Enum
    {
        List<T> list = [];

        foreach (string val in source)
        {
            if (val.TryFhirEnum(out T v))
            {
                list.Add(v);
            }
            else if (defaultVal != null)
            {
                list.Add((T)defaultVal);
            }
        }

        return list.Count != 0 ? list.AsEnumerable() : [];
    }
}

