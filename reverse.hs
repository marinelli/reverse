{-# LANGUAGE
    UnicodeSyntax
#-}

module Main where

import Prelude hiding (iterate, reverse)
import Data.IORef



data LinkedList a
  = None
  | Node { value ∷ IORef a
         , next  ∷ IORef (LinkedList a)
         }


linkedlist ∷ a → IO (LinkedList a)
linkedlist x =
  do
    v ← newIORef x
    n ← newIORef None
    pure $ Node { value = v , next = n }


get_value ∷ LinkedList a → IO a
get_value n =
  case n of
    None → error "Empty"
    node → (readIORef $ value node) >>= \ x → pure x


set_value ∷ a → LinkedList a → IO ()
set_value x n =
  case n of
    None → pure ()
    node → writeIORef (value node) x


get_next ∷ LinkedList a → IO (LinkedList a)
get_next n =
  case n of
    None → error "Empty"
    node → (readIORef $ next node) >>= \x → pure x


set_next ∷ LinkedList a → LinkedList a → IO ()
set_next n1 n2 =
  case n1 of
    None → pure ()
    node → writeIORef (next node) n2


reverse ∷ LinkedList a → IO (LinkedList a)
reverse l =

    let reverse' ∷ LinkedList a → LinkedList a → IO (LinkedList a)
        reverse' n1 n2 =
          case n2 of
            None → pure n1
            _    → do
              n3 ← get_next n2
              set_next n2 n1
              reverse' n2 n3
    in

    reverse' None l


iterate ∷ (a → IO ()) -> LinkedList a -> IO ()
iterate f l =
  case l of
    None → pure ()
    _    →
      do
        v ← get_value l
        f v
        n ← get_next l
        iterate f n


print_list ∷ Show a ⇒ LinkedList a → IO ()
print_list l =
  iterate (\ x → putStr $ show x ++ " ") l



main ∷ IO ()
main =
  do
    l1 ← linkedlist 1
    l2 ← linkedlist 2
    l3 ← linkedlist 3
    l4 ← linkedlist 4
    l5 ← linkedlist 5

    let lists = [ ("l1", l1) , ("l2", l2) ,
                  ("l3", l3) , ("l4", l4) ,
                  ("l5", l5)
                ]

    putStr "\n"

    putStr ">>> Before linking the lists\n"
    mapM_ (\ (s, l) →
             do
               putStr $ s ++ " : "
               print_list l
               putStr "\n"
          )
          lists

    set_next l1 l2
    set_next l2 l3
    set_next l3 l4
    set_next l4 l5

    putStr "\n"

    putStr ">>> After linking and before reversing the linked list\n"
    mapM_ (\ (s, l) →
             do
               putStr $ s ++ " : "
               print_list l
               putStr "\n"
          )
          lists

    putStr "\n"

    putStr " The evaluation of `reverse l1´ computes the reversed\n\
           \ list of l1 and the final result is equal to l5,\n\
           \ the new head of the linked list.\n\n"

    _ ← reverse l1

    putStr ">>> After reversing the linked list\n"
    mapM_ (\ (s, l) →
             do
               putStr $ s ++ " : "
               print_list l
               putStr "\n"
          )
          lists

    putStr "\n"


