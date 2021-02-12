structure SegmentTree : sig
            datatype 'elem t =
                     Node of 'elem * 'elem t * 'elem t
                   | Leaf of 'elem
                   | NoneLeaf
            val make : 'elem list -> ('elem -> 'elem -> 'elem) -> 'elem t
          end
=
struct
  datatype 'a t =
           Node of 'a * 'a t * 'a t
         | Leaf of 'a
         | NoneLeaf

  fun log2 x =
      let
        val x = Real.fromInt x
        val a = Math.ln x
        val b = Math.ln 2.0
      in
        a / b
      end

  fun value (Node (x, _, _)) = x
    | value (Leaf x) = x
    | value NoneLeaf = raise Fail "value of NoneLeaf"

  fun treeSize x = Real.ceil (log2 x)

  fun update (Node (x, left, right)) parent =
      let
        val newLeft = update left parent
        val newRight = update right parent
      in
        case (newLeft, newRight) of
            (NoneLeaf, NoneLeaf) => NoneLeaf
          | (NoneLeaf, _) => Node (value newRight, NoneLeaf, newRight)
          | (_, NoneLeaf) => Node (value newLeft, newLeft, NoneLeaf)
          | _ =>
            let val newValue = parent (value newLeft) (value newRight)
            in Node (newValue, newLeft, newRight) end
      end
    | update leaf _ = leaf

  fun makeTree 0 (x :: xs) _ = (Leaf x, xs)
    | makeTree 0 nil _ = (NoneLeaf, nil)
    | makeTree n xs parent =
      let
        val (t1, rest1) = makeTree (n - 1) xs parent
        val (t2, rest) = makeTree (n - 1) rest1 parent
        val tree =
            case (t1, t2) of
                (NoneLeaf, NoneLeaf) => NoneLeaf
              | (NoneLeaf, _) => Node (value t2, NoneLeaf, t2)
              | (_, NoneLeaf) => Node (value t1, t1, NoneLeaf)
              | _ => let val x = parent (value t1) (value t2)
                     in Node (x, t1, t2) end
      in
        (tree, rest)
      end

  fun make elements parent =
      let
        val height = treeSize (length elements)
        val (tree, _) = makeTree height elements parent
      in
        tree
      end

  fun curry f x y = f (x, y)
end
