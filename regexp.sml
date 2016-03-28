structure Regexp: sig
              datatype t = Empty
                         | Sym of string
                         | Or of t * t
                         | And of t * t
                         | Rep of t

              val match: t -> string -> bool
          end
= struct
    fun isEmpty s = s = ""
    fun splitGen s = let
        val i = ref 0
        fun next () = let
            val i' = !i
            val ret = if i' <= String.size s
                      then SOME((String.substring(s, 0, i'), String.substring(s, i', String.size s - i')))
                      else NONE
            val () = i := i' + 1
        in
            ret
        end
    in
        next
    end


    fun withSprits s f = let
        val split = splitGen s
        fun loop () = let
            val sp = split()
        in
            case sp of
                SOME(ret) => if f ret
                             then true
                             else loop ()
              | NONE => false
        end
    in
        loop ()
    end

    fun splitNGen s n =
      if n <= 0 orelse (String.size s) < n
      then fn () => NONE
      (* else if  *)
      (* then let val v = ref (SOME []) in *)
      (*          fn () => (let val ret = !v in v := NONE; ret end) end *)
      else if n = 1
      then let val v = ref (SOME [s]) in
               fn () => (let val ret = !v in v := NONE; ret end) end
      else let
          val size = String.size(s)
          val i = ref 1
          fun genGen () = splitNGen (String.substring(s, !i, (String.size s) - !i)) (n - 1)
          val gen = ref (genGen ())
          fun next () = let
              val tl = (!gen) ()
          in
              case tl of
                  SOME(tl') => SOME(String.substring(s, 0, !i) ::tl')
                | NONE => if (!i) = size
                          then NONE
                          else (
                              i := (!i) + 1;
                              gen := genGen();
                              next()
                          )
          end
      in
          next
      end

    fun partsGen s = let
        val size = String.size s
        val i = ref 1
        fun genGen () = splitNGen s (!i)
        val gen = ref (genGen ())
        fun next () = case (!gen) () of
                          SOME v => SOME v
                        | NONE => (
                            i := (!i) + 1;
                            if !i <= size
                            then (gen := genGen();
                                  next())
                            else NONE)
    in
        next
    end

    fun withParts s f = let
        val parts = partsGen s
        fun loop () = case parts() of
                          SOME part => if f part
                                       then true
                                       else loop ()
                        | NONE => false
    in
        loop ()
    end

    val gen = partsGen "abcdeg";

    datatype  t
      = Empty
      | Sym of string
      | Or of t * t
      | And of t * t
      | Rep of t

    fun match Empty u = isEmpty u
      | match (Sym a) u = a = u
      | match (Or(p, q)) u = match p u orelse match q u
      | match (And(p, q)) u = withSprits u (fn (u1, u2) => match p u1 andalso match q u2)
      | match (Rep(r)) u = withParts u (fn input => List.all (match r) input)
end
