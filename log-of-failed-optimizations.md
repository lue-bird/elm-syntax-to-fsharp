- patternBindingsAsFsharpVariables: Directly collect into a set instead of through an intermediate list (was slower). Also then tried inserting into a single set passed as an argument
  to avoid allocating many small sets (also slower)
