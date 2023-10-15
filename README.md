# 100 hours of Ada

This is a playground for learning Ada. There is no specific structure or goal, just bits and pieces to note and try out on the way.

Build with:
```
$ alr build
```

Run with:
```
$ alr run
```

## 001 Lift off! 🚀

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
* Loop variables are constant within the loop body, i.e. cannot be re-assigned.
* Execution of a branch in a `case` statement does not fall through to the next branch.

## 002 

Ada compilation units are split into two parts:
* `.ads` (_Ada specification_) files contain all declarations (private or visible to the outside) 
* `.adb` (_Ada body_) files contain the implementation

Program unit:
```ada
kind-of-unit Unit_Name
   -- local declarations (visible within this unit but nowhere else)
begin
   -- sequence ofstatements
exception
   -- sequence of statements
end Unit_Name;
```

Parameters:
* Function and procedure parameters are by default imutable: Their implicit mode is `in` (read-only access). If you want to modify a parameter, you have to explicitely specify mode `in out` or `out`.
* `out` parameters should be treated like uninitialized variables. They can be useful for assigning values to multiple return parameters (instead of returning a record type).

## 003 📦

General structure:
```ada
package Package_Name is
   -- public declarations
private
   -- private declarations
end Package_Name;

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

## 004 Overflows

Integer ranges are checked for overflows, both at compile time and at run time.
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

Not only for standard integers, but also for custom-defined integer types (which allow you to specify the admissible range).
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

## 005 

> A key difference between Ada and most other software development languages is that Ada is designed as an engineering tool as well as a programming tool.

> Ada as an engineering tool, requires the software developers to adopt an engineering attitude to using it. It is not enough to simply be a good computer
programmer when human safety is at risk. Software at that level of risk must be engineered. 

> Ada is designed to maximize the error checking a compiler can do early in the development process

> The default for every Ada construct is _safe_.

## 006

* elementary types
* composite types (arrays, records)
* derived types and subtypes
```ada
-- derived types (do not count as Float, cannot be mixed but always need to be converted)
type Meters is new Float;
type Miles  is new Float;

-- subtypes (do count as Integer)
subtype Natural  is Integer range 0 .. Integer'Last;
subtype Positive is Integer range 1 .. Integer'Last;

-- subtype without constraints as alias
subtype Amount is Integer;
```

**Named association:** Parameters in a subprogram can be named. For example, a subprogram that is defined like this:
```ada
procedure Point_To(Azimuth, Elevation : Float);
```
Can be called like this:
```ada
-- positional:
Point_To(42.8, 16.2);
-- named:
Point_To(Azimuth => 42.8, Elevation => 16.2);
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

## 007

* Arrays have an index type, which can be any enumerable type. It needs to be constrained before the array can be used.
  ```ada
  type Valve_State is (Open, Closed, Stuck_Inbetween);
  type Valve_States is array (1..10) of Valve_State;
  ```
* Arrays can be concatenated with `&`. 
* Arrays an be multidimensional, for example:
  ```ada
  type Matrix is array (
     Positive range <>, -- first dimension
     Positive range <>  -- second dimension
  ) of Boolean;

  type Matrices is array (Positive range <>) of Matrix(1 .. 10, 1 .. 20);
  ```

Now you can make your intent very obvious:
```ada
procedure List_Demo is
   type Index is range 1 .. 10;
begin
   for I in Index'First .. Index'Last 
      loop
         -- access element as List (I)
      end loop;
end List_Demo;
```

## 📚 Next

* https://learn.adacore.com/courses/intro-to-ada/chapters/records.html
* https://learn.adacore.com/courses/Ada_For_The_CPP_Java_Developer/index.html

## Backlog

* What’s the difference between `type Blah new Integer range …` and `subtype Integer range …`?
* type conversions
* Custom fixed point types: 
  ```
  type Type_Name is delta <delta-value> digits <digits-value> (range <start>..<end>);
  ```
* attributes
* pass-by-value vs pass-by-reference, access parameters
* error handling
* AUnit and other approaches to testing
* nulls and the lack thereof
* Procedure with an empty body
```ada
procedure NotImplemented is null; 
```
* private types 