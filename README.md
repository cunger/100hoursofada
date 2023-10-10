# 100 hours of Ada

This is a playground for learning Ada; it's not doing anything meaningful yet.

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

* Function and procedure parameters are by default imutable: Their implicit mode is `in` (read-only access). If you want to modify a parameter, you have to explicitely specify mode `in out` or `out`.
* `out` parameters should be treated like uninitialized variables. They can be useful for assigning values to multiple return parameters (instead of returning a record type).
* `.ads` files contain the specification, `.adb` files contain the implementation.

📚 Later reading: https://learn.adacore.com/courses/GNAT_Toolchain_Intro/index.html

## Backlog

* pass-by-copy
* error handling
* testing
