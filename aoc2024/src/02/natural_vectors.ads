with Ada.Containers.Vectors;

package Natural_Vectors is new Ada.Containers.Vectors (
   Index_Type   => Positive,
   Element_Type => Natural
);