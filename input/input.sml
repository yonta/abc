structure Input
          : sig
            type ('result, 'input) reader = ('result, 'input) StringCvt.reader
            type ('result, 'input) scanner = (char, 'input) reader
                                             -> ('result, 'input) reader
            val test : (unit -> unit) -> string -> unit
            val makeStringReader :
                ((char, substring) reader -> ('a, substring) reader)
                -> ('a, string) reader
            val && : ('result1, 'input) reader * ('result2, 'input) reader
                     -> (('result1 * 'result2), 'input) reader
            val || : ('result1, 'input) reader * ('result1, 'input) reader
                     -> ('result1, 'input) reader
            val &&> : ('result1, 'input) reader * ('input -> 'input)
                      -> ('result1, 'input) reader
            val >&& : ('input -> 'input) * ('result, 'input) reader
                      -> ('result, 'input) reader
            val ||> : ('result, 'input) reader * ('input -> 'input)
                      -> 'result -> ('result, 'input) reader
            val ** : ('result, 'input) reader -> ('result list, 'input) reader
            val listReader : ('result, 'input) reader list
                             -> ('result list, 'input) reader
            val skipOne : (char -> bool)
                          -> (char, 'input) reader -> 'input -> 'input
            val scan : (char -> bool)
                         -> (char, 'input) reader -> (string, 'input) reader
            val skip : (char -> bool)
                       -> (char, 'input) reader -> 'input -> 'input
            val skipWS : (char, 'input) reader -> 'input -> 'input
            val &&& : ('result1, 'input) scanner * ('result2, 'input) scanner
                      -> ('result1 * 'result2, 'input) scanner
            val >&&& : ((char, 'input) reader -> 'input -> 'input)
                       * ('result, 'input) scanner
                       -> ('result, 'input) scanner
            val showRestSubstring : ('result * substring) option
                                    -> ('result * string) option
            val scanForString : ('result, substring) scanner
                                -> ('result, string) reader
          end
=
struct
  infix && || >&& &&> ||> &&& >&&&

  type ('result, 'input) reader = ('result, 'input) StringCvt.reader
  type ('result, 'input) scanner = (char, 'input) reader
                                   -> ('result, 'input) reader

  fun test f filename =
      let
        (* val () = print "========== start ==========\n" *)
        val originalIns = TextIO.getInstream TextIO.stdIn
        val fileIns = TextIO.getInstream (TextIO.openIn (filename))
        val () = TextIO.setInstream (TextIO.stdIn, fileIns)
        val () = f () handle Fail str => print ("FAIL:" ^ str)
                           | exn =>
                             (TextIO.closeIn TextIO.stdIn;
                              TextIO.setInstream (TextIO.stdIn, originalIns);
                              raise exn)
        val () = TextIO.closeIn TextIO.stdIn
        val () = TextIO.setInstream (TextIO.stdIn, originalIns)
        val () = print "\n"
                       (* val () = print "\n========== end ==========\n" *)
      in
        ()
      end

  fun makeStringReader scan input =
      let
        val substr = Substring.full input
        val reader = scan Substring.getc substr
      in
        Option.map (fn (a, b) => (a, Substring.string b)) reader
      end

  fun op && (reader1, reader2) input =
      case reader1 input of
          NONE => NONE
        | SOME (result1, rest) =>
          (case reader2 rest of
               NONE => NONE
             | SOME (result2, rest) => SOME ((result1, result2), rest))

  fun op || (reader1, reader2) input =
      case reader1 input of
          NONE => (case reader2 input of
                       NONE => NONE
                     | r2 as SOME _ => r2)
        | r1 as SOME _ => r1

  fun op &&> (reader, f) input =
      case reader input of
          NONE => NONE
        | SOME (result, rest) => SOME (result, f rest)

  fun op >&& (f, reader) input = reader (f input)

  fun op ||> (reader, f) base input =
      case reader input of
          NONE => SOME (base, f input)
        | r as SOME _ => r

  fun ** reader input =
      case reader input of
          NONE => NONE
        | SOME (result, rest) =>
          case ** reader rest of
              NONE => SOME ([result], rest)
            | SOME (results, rest2) => SOME (result :: results, rest2)

  fun listReader nil input = SOME (nil, input)
    | listReader (reader :: readers) input =
      case reader input of
          NONE => NONE
        | SOME (result, rest) =>
          case listReader readers rest of
              NONE => NONE
            | SOME (results, rest2) => SOME (result :: results, rest2)

  fun skipOne charis reader input =
      case reader input of
          NONE => input
        | SOME (c, rest) => if charis c then rest else input

  local
    fun makeResult chars input =
        if null chars then NONE else SOME (String.implode (rev chars), input)
    fun scanImpl acc charis getc input =
        case getc input of
            NONE => makeResult acc input
          | SOME (c, newInput) =>
            if charis c then scanImpl (c :: acc) charis getc newInput
            else makeResult acc input
  in
  fun scan charis getc input = scanImpl nil charis getc input
  end

  fun skip charis getc input =
      case scan charis getc input of
          NONE => input
        | SOME (_, rest) => rest

  fun skipWS getc input = StringCvt.skipWS getc input

  fun op &&& (scan1, scan2) getc input = (scan1 getc && scan2 getc) input
  fun op >&&& (f, scan2) getc input = (f getc >&& scan2 getc) input

  fun showRestSubstring NONE = NONE
    | showRestSubstring (SOME (result, substr)) =
      SOME (result, Substring.string substr)

  fun scanForString scan input =
      showRestSubstring (scan Substring.getc (Substring.full input))

end

(* example *)
infix && || &&> >&& ||>
local
  open Input
in
val _ =
    let
      val getc = Substring.getc
      val intReader = Int.scan StringCvt.DEC getc
      (* automaticly skip white space *)
      val readers = [intReader, intReader, intReader,
                     intReader, intReader, intReader]
    in
      listReader readers
    end
end
