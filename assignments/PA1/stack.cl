(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class StackCommand {

   cmd: String;
   nxt: StackCommand;

   init_args(c: String, n: StackCommand): StackCommand {
      {
         cmd <- c;
         nxt <- n;
         self;
      }
   };

   init(c: String, n: StackCommand): StackCommand {
      if c = "d" then
         (new StackCommandDisplay).init_args(c, n)
      else if c = "e" then
         (new StackCommandEva).init_args(c, n)
      else if c = "s" then
         (new StackCommandSwap).init_args(c, n)
      else if c = "+" then
         (new StackCommandAdd).init_args(c, n)
      else
         (new StackCommandNum).init_args(c, n)
      fi fi fi fi
   };

   push(s: StackCommand): StackCommand {
      {
         s.setnxt(self);
         s;
      }
   };

   getcmd(): String { cmd };
   getnxt(): StackCommand { nxt };
   setnxt(n: StackCommand): Object { nxt <- n };

   action(): StackCommand {
      case self of
         d: StackCommandDisplay => d.action();
         e: StackCommandEva => e.action();
         o: StackCommand => self;
      esac
   };

};

class StackCommandDisplay inherits StackCommand {
   display(s: StackCommand): Object {
      if (isvoid s) then
         0
      else
         {
            (new IO).out_string(s.getcmd());
            if (isvoid s.getnxt()) then
               0
            else
               {
                  (new IO).out_string("\n");
                  self.display(s.getnxt());
               }
            fi;
         }
      fi
   };

   action(): StackCommand {
      {
         self.display(nxt);
         nxt;
      }
   };   
};

class StackCommandEva inherits StackCommand {
   action(): StackCommand { 
      case nxt of
         a: StackCommandAdd => a.eva();
         s: StackCommandSwap => s.eva();
         o: StackCommand => nxt;
      esac      
   };
};

class StackCommandAdd inherits StackCommand {
   eva(): StackCommand {
      if (isvoid nxt) then
         nxt
      else
         case nxt of
            n: StackCommandNum => n.add();
            o: StackCommand => nxt;
         esac
      fi
   };
};

class StackCommandSwap inherits StackCommand {
   eva(): StackCommand {
      if (isvoid nxt) then
         nxt
      else if (isvoid nxt.getnxt()) then
         nxt
      else
         {
            let tmp: StackCommand <- nxt.getnxt() in
               {
                  nxt.setnxt(tmp.getnxt());
                  tmp.setnxt(nxt);
                  self.setnxt(tmp);
               };
            nxt;
         }
      fi fi
   };
};

class StackCommandNum inherits StackCommand {
   add(): StackCommand {
      if (isvoid nxt) then
         self
      else
         case nxt of
            n: StackCommandNum => {
               let x1: Int <- (new A2I).a2i(cmd),
                   x2: Int <- (new A2I).a2i(nxt.getcmd()) in
                  {
                     cmd <- (new A2I).i2a(x1 + x2);
                     nxt <- nxt.getnxt();
                  };
               self;
            };
            o: StackCommand => self;
         esac
      fi 
   };
};

class Main inherits IO {

   s: StackCommand <- (new StackCommand);
   cmd: String <- "";

   main() : Object {
      while ({
               out_string(">");
               cmd <- in_string();
               if cmd = "x" then false else true fi;
            }) loop
         {
            let head: StackCommand <- (new StackCommand).init(cmd, s)
            in
               if (isvoid s) then
                  s <- head.action()
               else
                  s <- s.push(head).action()
               fi;
         }
      pool
   };

};
