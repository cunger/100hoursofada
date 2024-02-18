package TCP.Client is

   function Send (Host_Name : in String; Port : in Positive; Message : in String)
      return String;

end TCP.Client;