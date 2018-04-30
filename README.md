# scala-express
Scala and scalajs library for BIM IFC text file format based on STEP file described by an EXPRESS schema

## Goals

1. Having a type-safe library for reading and writing IFC text file, heavy usage of type classes with help 
of Shapeless and Refined to ensure quality of encoding/decoding

2. Generate Scala classes to manipulate STEP file from an EXPRESS schema

## Step of "codec"
Similar to encoding/decoding solution from circe (just replace Json by Step)

1. Parsing string/file/stream to Step class instances
2. Try to convert each Step class instances to the good IfcEntity classes (with Strict, StrictOrConvert or Flexible validation)
3. Use all Ifc instances as you want
4. Convert each Ifc instances to Step instances but ensures that respects rules from IFC EXPRESS schema
5. Build STEP text file from Step class instances

## Strict, StrictOrConvert or Flexible validation
Example : 

```TYPE IfcGloballyUniqueId = STRING (22) FIXED```
And char should be in this list : ```0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_$```

```representationContexts : OPTIONAL SET [1:?] OF IfcRepresentationContext```

```directionRatios : LIST [2:3] OF IfcReal```

With Strict :

- IfcGloballyUniqueId must be exactly 22 chars from the list or it will fail
- representationContexts must be undefined or a non-empty list, or it will fail
- directionRatios must be a list of 2 or 3 IfcReal, or it will fail

With StrictOrConvert : 

- IfcGloballyUniqueId will be updated to be exactly 22 chars from the list if needed
- representationContexts will be updated to undefined if an empty list was set
- directionRatios will be completed with IfcReal.Default and exceeding items will be dropped

With Flexible :

- IfcGloballyUniqueId can contain anything and a warning is emitted
- representationContexts could have an empty list and just a warning is emitted
- directionRatios could have any size

Output mode can be StrictOrConvert while input mode was Flexible, the file will be normalized after that (if possible)
