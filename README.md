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
* Loop variables cannot be re-assigned.
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

## 📚 Next

* https://learn.adacore.com/courses/intro-to-ada/chapters/records.html
* https://learn.adacore.com/courses/Ada_For_The_CPP_Java_Developer/index.html

## Backlog

* attributes
* pass-by-value vs pass-by-reference, access parameters
* error handling
* AUnit and other approaches to testing
* nulls and the lack thereof
* Procedure with an empty body
```ada
procedure NotImplemented is null; 
```
