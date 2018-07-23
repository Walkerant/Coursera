Contributed by Charilaos Skiadas

# Notes on the material
* Three key ways of creating "compound" types, make sure to understand them
* Tuples are just records
* Datatype bindings are used to create one-of types: extremely powerful and flexible (for example can build trees)
* A datatype binding creates functions (or constants) for each of the constructors.
* Note the keyword \verb|datatype|datatype is not used when writing values of that type.
* Type synonyms offer alternative names for complicated types (e.g., type date = int*int*int)
* Pattern matching rocks! Enough said.
* Using \verb|val p = e|valp=e where \verb|p|p is a pattern (like \verb|(x,y,z)|(x,y,z) or \verb|{ x= x, y = y }|{x=x,y=y}) can be a very effective way to get values out of a tuple or record.
* Lists and options are really just datatypes (worth understanding how that works). Practice by creating your own "list" type.
* Nested patterns can be very expressive.
* Pay attention to the use of the wildcard \verb|_|_ for parts of a pattern we don't care to name.
* Raising/handling exceptions is similar to datatypes and pattern-matching.
* Appending to the end of a list is relatively expensive.
