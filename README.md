# 100 hours of Ada

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

* Function and procedure parameters are by default imutable, because their implicit (if not specified) is `in` (only read access). If you want to modify a parameter, you have to specify mode `in out` or `out`.
* `out` parameters should be treated like uninitialized variables. They might be useful for assigning values to multiple return parameters (instead of returning a record type).
* `.ads` files contain the specification, `.adb` files contain the implementation.

📚 Later reading: https://learn.adacore.com/courses/GNAT_Toolchain_Intro/index.html


## Backlog

* pass-by-copy
* testing
* error handling
