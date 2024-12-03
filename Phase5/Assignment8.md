# Assignment 8 - Phase 5

Gruppe 24, DAT 3:

Carl-Emil Andersen 202107334

Oliver Thomas 202107067

Rune Schuster 202001199

## Expanding ast
```ocaml
type field = Field of 
         { name  : fieldname 
         ; typ   : typ
         ; loc   : Loc.location
         }

type rcrd = Recor of {recordname : recordname; fields : field list}

type funcOrRcrd =
| Rcr of rcrd
| Fun of func

type program = funcOrRcrd list
```
The program should interpreted as a list of functions and programs. Hence we expanded the ast to represent this. 
The reasoning behind the rcrd type is that a Record has a name and fields.
The rest was pretty much just taken from the assignment description.
## Expanding the tast
The tast has roughly the same stuff as the ast except we added a non optional type and removed the location.
## Expanding the errors module
We expanded the errors module a lot to accomodate for the new erros. A list of errors added can be seen below:
```ocaml
| DuplicateRecordDecleration of {rname : string; loc : Loc.location}
| StdLibraryRecprd of {rname : string; loc : Loc.location}
| ReferringToNonExistingRcrd of {rname : string; fieldname : string; loc : Loc.location}
| DuplicateField of {fieldname : string; loc : Loc.location}
| RecordMisMatch of {expected : TAst.typ; loc : Loc.location}
| ExprNotARecord of {loc : Loc.location}
| FieldDoesNotExist of {fname : string; loc : Loc.location}
| ExpectedRcrd of {rname : string; loc : Loc.location}
| ArrayExprNotInt of {loc : Loc.location}
| ExprNotAnArray of {loc : Loc.location}
```
This errors are not the entire list of errors that should be present in this assignment in our opinion, however due to time constraint we have chosen to focus on these. This means that all above errors are implemented and correctly thrown if the dolphin code breaks it.
## Expanding the control flow graph
While expanding the cfg in codegen we made a list of less smart decisions. This have led to our environment being quite overloaded.
However while overloaded our environment still provivdes us with the information nessecary to solve the problems. 
Mainly an issue we have with the cfg is in regards to environments, we have multiple tables that maps from a record to a list of types it contains.
## Expanding the codegen
Main challenges in this part has been the thinking. 

The above statement should be intepreted as a little time spent analyzing which meant that time spent coding was significantly longer than nessecary. 
This is also what caused the unessecary cfg problems regarding it being overloaded. 
An interesting thing we did with records was deciding that each record should be have its fields sorted lexicographically.
This decision was made such that we always could calculate what index the field would have depending on its field name, and the list of fields.
With regards to array, the main issue was pointer work when arrays where combined with records. These problems, which are now fixed - hopefully, was again caused by an insignificant analysis of the problem at hand.

We did add a couple of comments to the llvm code to signal where we were located, which made debugging larger programs easier.

The way names are generated for strings might cause errors down the road. We assume that if i user plays around with the strings a bit they can get the compiler to do some unintended behavior. This is our intuition at the moment, we will have to paly around with strings a bit more if time permits it later down the road. For now we have a couple of tests using strings which work as intended.
