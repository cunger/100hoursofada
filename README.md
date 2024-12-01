# 100 hours of Ada

This is a playground for learning Ada, with the following toy projects:

* `aoc2023` was my first Advent of Code, and I used it mainly to practice Ada.
* `aoc2021` was done later, mainly to get familiar with SPARK.
* `aoc2015` was also done later, and is just a quick and dirty warmup.
* `minesweeper` is implementing the board generation and part of the game logic of Minesweeper. Mainly for learning AUnit and trying pre- and post-conditions.
* `playground` collects bits and pieces to try out.
* `protohackers` are basic solutions to the [Protohackers challenges](https://protohackers.com)
* `semaphores` is a basic implementation of a semaphore and a timed semaphore. Mainly to get familiar with tasks and timing.
* `sniff` is a simple, crude port scanner. Mainly for getting a feel for network programming.
* `watchdog` is a basic implementation of a [watchdog timer](https://en.wikipedia.org/wiki/Watchdog_timer). Mainly to get familiar with tasks and timing.

The general project structure was extracted into a template: https://github.com/cunger/alr-template

I'm jumping between projects and topics, because these 100 hours are meant for exploration and experimenting.

# Wisdom 🧙‍♂️

> "Slow is smooth, smooth is fast." Don't rush to failure.

Ada Distilled:

> Ada as an engineering tool, requires the software developers to adopt an engineering attitude to using it. It is not enough to simply be a good computer
programmer when human safety is at risk. Software at that level of risk must be engineered. 

> Ada is designed to maximize the error checking a compiler can do early in the development process. It allows you to expresses your intent so it's not only clear for human readers but also explicit for the compiler, so your adherence to it can be checked automatically at compile time. 

> The default for every Ada construct is _safe_.

Neil Storey:

> As safety cannot be demonstrated by testing alone, a system‘s acceptance must be based on confidence gained in other ways. Key factors in any safety case are the development and production processes used and the quality methods used to oversee them.

# Notes

* [Expressions and statements](#expressions-and-statements)
* [Functions and procedures](#functions-and-procedures)
* [Packages](#-packages)
* [Types](#types)
   * [Elementary types](#elementary-types)
   * [Composite types](#composite-types)
   * [Derived types and subtypes](#derived-types-and-subtypes)
   * [Range constraints](#range-constraints)
* [Scope and visibility](#scope-and-visibility)
* [Constraints and error handling](#constraints-and-error-handling)
   * [Pre- and post-conditions](#pre--and-post-conditions)
   * [Exceptions](#-exceptions)
* [Arrays](#arrays)
* [Vectors](#vectors)
* [Testing](#testing)
* [Concurrency](#concurrency)
* [References](#-references)
* [Explore](#explore)

## Lift off! 🚀

```ada
-- countdown.adb (file name needs to correspond to name of the main procedure)

with Ada.Text_IO; use Ada.Text_IO;

--
-- Prints a simple countdown from 10 to lift off.
--
procedure Countdown is
begin
   for I in reverse 1 .. 10 loop
      Put_Line (Integer'Image (I) & "...");
   end loop;

   Put_Line ("Lift off!");
end Countdown;
```

* Expressions (e.g. `I = 0`) and statements (e.g. `I := 0;`) are distinct. That is, expressions are not valid statements.
* Identifiers are case-insensitive. 
* `function Fill(Board : Board)` does not work.
* Loop variables are constant within the loop body, i.e. cannot be re-assigned.
* Execution of a branch in a `case` statement does not fall through to the next branch.
* `and` and `or` are not short-circuiting (both operands are evaluated in no fixed order), but their variants `and then` and `or else` are (for example `I /= 0 and then N/I > 1`) 

## Expressions and statements

Blocks are of the general form:
```ada
declare
   -- local declarations (visible within this unit but nowhere else)
   -- can include functions, procedures, and variables
   -- note that declared variables are not initialized by default
begin
   -- sequence of statements or nested blocks
exception
   -- handling exceptions
end;
```

The statement `null;` does nothing.

## Functions and procedures

Specification:
```ada
procedure Not_Doing_Anything;

procedure Blah (Input : Input_Type; Other : Other_Type);

function Blubb (Input : Input_Type) return Output_Type;
```

Implementation:
```ada
procedure Not_Doing_Anything is null;

procedure Blah (Input : Input_Type; Other : Other_Type) is
   -- local declarations (visible within this unit but nowhere else)
begin
   -- sequence of statements or blocks
exception
   when <choice> =>
      -- sequence of statements
end Unit_Name;

function Blubb (Input : Input_Type)
  return Output_Type is
   -- local declarations
begin
   -- sequence of statements
exception
   when <choice> =>
      -- sequence of statements
end Blubb;
```

Parameters:
* Function and procedure parameters are by default imutable: Their implicit mode is `in` (read-only access). If you want to modify a parameter, you have to explicitely specify mode `in out` or `out`.
* `out` parameters should be treated like uninitialized variables, because they could be. They can be useful for assigning values to multiple return parameters (instead of returning a record type).
* Whether parameters are passed by value or by reference is decided by the compiler. For example, since `in` parameters cannot be modified, they can be passed as value or reference, depending on size and other conditions. You don't have to worry about it as a programmer.

**Named association:** Parameters in a subprogram can be named. For example, a subprogram that is defined like this:
```ada
procedure Point_To(Azimuth, Elevation : Float);
```
Can be called like this:
```ada
-- positional:
Point_To (42.8, 16.2);
-- named:
Point_To (Azimuth => 42.8, Elevation => 16.2);
```
Same holds for constructing arrays:
```ada
type Color_Channel is (Y, Cb, Cr);
type Color_Value is range 0 .. 255;
type Color is array (Color_Channel) of Color_Value;

-- positional:
Color1 : constant Color := (0, 0, 0);
-- named:
Color2 : constant Color := (Y => 0, Cb => 0, Cr => 0);
```

## 📦 Packages

Packages are library units and can be imported like this:
```ada
with Library_Unit; -- context clause
```

They are split into two parts:
* `.ads` (_Ada specification_) files contain all declarations (private or visible to the outside) 
* `.adb` (_Ada body_) files contain the implementation

Where the package body is required only to complete incomplete items in the specification (e.g. subprogram declarations without implementation). If there are no incomplete declarations in the specification, then there must be no iplementation part.

Specification:
```ada
package Package_Name is
   -- public declarations
private
   -- private declarations
end Package_Name;
```

Implementation:
```ada
with Other_Library;

package body Package_Name is
   -- implementation
end Package_Name;
```

Packages can have child packages. For example, `Ada.Text_IO` is a child package of `Ada`. 

Everything from the parent's specification is visible in the child. That is, everything declared in the `.ads` file; not stuff in the declaration part of the package body. For example, constants you want to be visible need to be part of the specification:
```ada
package Reactor is

   Model : constant String := "Konvoi";
   -- visible in all child packages

end Reactor

package body Reactor is

   Model : constant String := "EPR";
   -- only visible within the body
   -- NOT visible in child packages
   -- NOT shadowing: compilation fails because of conflict with declaration

end Reactor
```

Built-in packages: 
* `Ada` is the standard language library, including child libraries like `Ada.Calendar`, `Ada.Numerics`, `Ada.Real_Time`, `Ada.Strings`, `Ada.Streams`, and so on. (For their specifications, see [github.com/reznikmm/adalib](https://github.com/reznikmm/adalib).)
* `Interfaces` for interfacing with other systems, e.g. `Interfaces.C` and `Interfaces.Fortran` 
* `System` for system-specific stuff (such as `MAX_INT`, or `TICK` for the clock period in seconds)

Packages, procedures, and functions and be renamed in the declaration:
```ada
with Ada.Text_IO;

procedure Main is
   
   package IO renames Ada.Text_IO;
   procedure Print (Line : String) renames IO.Put_Line;

begin
   Print ("Fnord!");
end Main;
```

## Types

### Elementary types

**Access types** (a.k.a. pointers)

**Scalar types**

Defined for all scalar types: `'Min`, `'Max`, `in`, `not in`

* Discrete types

  Defined for all discrete types: `'First`, `'Last`, `'Range`

  * enumeration
  ```ada
  type State is (Idle, Waiting, Processing, Stuck);
  type Boolean is (False, True);
  ```
  * integer (signed and modular)

* Real types
  * floating
  * fixed (decimal and ordinal)

Type conversion:
```ada
Float(2)
Integer(2.0)
Positive(10)
...
```

### Composite types

* arrays
* records
* interfaces
* protected objects
* tasks

Extending a record type:
```ada
-- A record type must be tagged in order to be extensible.
type Message is tagged
   record
      Content : Unbounded_String;
      Length : Natural;
   end record;

-- Extending a record type adds fields.
type Sent_Message is new Message with
   record
      Recipient : Unbounded_String;
      Timestamp : Date;
   end record;
```

### Derived types and subtypes

```ada
-- derived types 
type Meters is new Float;
type Miles  is new Float;
-- creates new types, so they do not count as Float
-- (i.e. also don't inherit operations like +, *)
-- and cannot be mixed but always need to be converted
type Temperature is new Integer range 0 .. 10_000;
-- ranges become part of the type checking at compile time

-- subtypes 
subtype Natural  is Integer range 0 .. Integer'Last;
subtype Positive is Integer range 1 .. Integer'Last;
-- do count as Integer
-- ranges are checked at run time

-- subtype without constraints as alias
subtype Amount is Integer;

I : Integer range 1 .. 100; -- static subtype
J : Integer range 1 .. N;   -- dynamic subtype
```


Restrict types (See range constraints below):
```ada
type Index is new Integer range 0 .. 100;
-- compiler has to choose pre-defined Integer type

type Index is range -10 .. 10;
-- compiler can choose the best representation of the values

type Index is new Float range -1.0 .. 1.0;
-- also works with other number types

type Hour is mod 24;
-- TODO fixed-point and decimal types
```

Parametrized types:
```ada
PixelMatrix is array (Integer range <>, Integer range <>) of Color;

type Bitmap (Width, Height : Positive) is record
   Pixels : PixelMatrix (1 .. width, 1 .. height);
end record;

-- declare like: B : Bitmap (32, 32); 
-- use parameters like: B.Width, B.Height
```

### Range constraints

Bounded types are checked for overflows, both at compile time and at run time.
```ada
procedure Main is

   I : Integer;
   J : Integer;

begin

   I := Integer'Last + 1;
   -- Compiler:
   -- error: static expression fails Constraint_Check
   -- error: value not in range of type "Standard.Integer"

   I := Integer'Last;
   J := I + 1;
   -- Compiler:
   -- warning: value not in range of type "Standard.Integer"
   -- warning: Constraint_Error will be raised at run time
   -- Run time:
   -- CONSTRAINT_ERROR : overflow check failed

   J := (Integer'Last + Integer'Last) / 10
   -- no overflow
end Main;
```

```ada
procedure Main is

   type Temperatur_K is new Integer range 0 .. Integer'Last;
   Tk : Temperatur_K;

begin
   Tk := -10;
   -- Compiler:
   -- warning: value not in range of type "Temperatur_K"
   -- warning: Constraint_Error will be raised at run time
   -- Run time: 
   -- CONSTRAINT_ERROR : range check failed
end Main;
```

> An Ada software engineer will avoid using pre-defined types such as Integer. Instead, that engineer will create an integer data type with all of its own unique and reliable properties. 

## Scope and visibility

Keeping a type private:
```ada
procedure Demo is
   type Blah is private;
   -- needs to be declared before it can be used in other declarations
private
   type Blah is <definition>;
begin
   null;
end Demo;
```

## Constraints and error handling

### Pre- and post-conditions

```ada
procedure Blah
with Pre  => ...,
     Post => ...
is begin
   ...
end Blah; 
```

Note that conditions and invariants belong to the decalaration, not the implementation.
So if you have a specification file, they have to go there:
```ada
procedure Blah (X : Integer; Y : in out Integer)
with Pre  => (X > 0) and (Y > 0),
     Post => Y /= Y'Old;

function Blubb (Flag : Boolean)
with Pre  => not Flag,
     Post => ...;
```

See also https://learn.adacore.com/courses/intro-to-ada/chapters/contracts.html

### 💥 Exceptions

Exceptions are objects, not types.

Declaring, raising and handling an exception in a nutshell:
```ada
with Ada.Text_IO;
with Ada.Exception;

declare

   All_Is_Not_Fine : exception;

begin

   raise All_Is_Not_Fine;

   -- or with a message
   raise All_Is_Not_Fine with "Ooops";

exception
   when E : All_Is_Not_Fine =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   when others =>
      raise; -- if you want to re-raise the exception
end;
```
An exception part can be added to all blocks (also to subprograms, for example).

Predfined exceptions are `Constraint_Error` (for overflows an bound errors), `Storage_Error` (for memory issues), `Tasking_Error` (for task-related problems), and `Program_Error` (more arcane). Leave them to Ada and always define the exceptions you want to raise.

Every run-time error results in an exception and can be handled.

## Arrays

* Arrays can have any enumerable, bounded type as an index type. (It can be unbound when declaring the array but needs to be constrained before the array is used.)
  ```ada
  type Valve_State is (Open, Closed, Stuck_Inbetween);
  type Valve_States is array (1..10) of Valve_State;
  ```
* Arrays can be concatenated with `&`. 
* `String` is defined as an array of characters. (So strings are bounded; for unbounded strings see [`Unbounded_String`](https://learn.adacore.com/courses/intro-to-ada/chapters/standard_library_strings.html#intro-ada-unbounded-strings).)
* Arrays can be multidimensional, for example:
  ```ada
  type Matrix is array (
     Positive range <>, -- first dimension, unconstrained
     Positive range <>  -- second dimension, unconstrained
  ) of Boolean;

  type Matrices is array (Positive range <>) of Matrix (1 .. 10, 1 .. 20);
  -- note that the dimensions need to be constrained when the type is used
  ```
  This is different from arrays of arrays (which you can also have, of course).

Now you can make your code very obvious and easy to check:
```ada
procedure Iterate is
   type Index is range 1 .. 10;
begin
   for I in Index'First .. Index'Last 
      loop
         -- access element as List (I)
      end loop;
end Iterate;
```

## Vectors

A [vector](https://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-18-2.html) is like an array that expands with each element that is added.

Specifying a vector type:
```ada
with Ada.Containers.Vectors;

package Boolean_Vectors is new Ada.Containers.Vectors (
   Index_Type   => Positive,
   Element_Type => Boolean
);

subtype Boolean_Vector is Boolean_Vectors.Vector;
```

Elements `E` can be added to a vector `V` simply by appending `V.Append (E)` (or prepending `V.Prepend (E)`).

Traversing a vector using a cursor:
```ada
V : Vector;
C : Cursor;

C := First (V); -- or: Last (V)
C := Next (C);  -- or: Previous (C)

-- Check whether the vector contains an element at the cursor position,
-- and access that element.
if Has_Element (C) then
   E := Element (C);
end if;
```

## Concurrency

> A concurrent program is [...] one that has more than one thread of control. 
Execution of this program will, if processors are available, run each of those threads of control in parallel.
Otherwise, the threads will be interleaved. [pseudo-parallelism]

```ada
procedure Main
   task A;
   task B;

   task body A is null;
   task body B is null;
begin
   -- A and B are executed concurrently.
   -- They are activated immediately, before any other statements in the begin body.

   null;

   -- The procedure will stop when all statements have been executed
   -- and A and B have finished.
end Main;
```

**Synchronous communication: Rendevouz via entry points**

```ada
task A is
   entry Start;
end A;

task body A is
begin
   -- do initialization

   -- rendevouz point
   -- this means here we wait until Start is called
   accept Start do
      -- start
   end Start;

   -- do something once Start was called.
end A;
```

**Asynchronous communcation: Protected objects**

...


## Testing

* [AUnit](https://www.adacore.com/documentation/aunit-cookbook)
* [Ahven](http://ahven.stronglytyped.org)

# 📚 References

* [Reference Manual](https://www.adaic.org/resources/add_content/standards/05rm/html/RM-TOC.html)

* https://learn.adacore.com/courses/intro-to-ada/
* David Given: [A random walk through Ada](https://cowlark.com/2014-04-27-ada/index.html)
* Richard Riehle: Ada Distilled
* https://en.wikibooks.org/wiki/Ada_Programming

# Explore

* https://github.com/ohenley/awesome-ada 
* Crates: https://alire.ada.dev/crates.html

Read:
* https://github.com/mosteo/aaa/blob/master/tests/tests.gpr
* https://github.com/mosteo/rxada
* https://github.com/AdaCore/gnatcoll-core

# Backlog

* mod types (`type Hash is mod Hash_Value`)
* type conversions
* Custom fixed point types: 
  ```
  type Type_Name is delta <delta-value> digits <digits-value> (range <start>..<end>);
  ```
* attributes
* null (as value, e.g. for an access type, and as procedure body)
* toolchains