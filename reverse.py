#!/usr/bin/env python3


class LinkedList:

    def __init__ (self, x) :
        self.value = x
        self.next  = None

    def get_value (self) :
        return self.value

    def set_value (self, x) :
        self.value = x

    def get_next (self) :
        return self.next

    def set_next (self, node) :
        self.next = node



def reverse (l) :

    def reverse_aux (n1, n2) :
        if n2 == None :
            return n1
        else :
            n3 = n2.get_next ()
            n2.set_next (n1)
            return reverse_aux (n2, n3)

    return reverse_aux (None, l)


def iterate (f, l) :

    if l == None :
        return
    else :
        f (l.get_value ())
        return iterate (f, l.get_next ())


def print_list (l) :

    iterate ((lambda s : print (str (s) + " ", end = '')), l)
    print ()


def main () :

    l1 = LinkedList (1)
    l2 = LinkedList (2)
    l3 = LinkedList (3)
    l4 = LinkedList (4)
    l5 = LinkedList (5)

    lists = [ ("l1", l1) , ("l2", l2)
            , ("l3", l3) , ("l4", l4)
            , ("l5", l5)
            ]

    print ()

    print (">>> Before linking the lists")
    list (map (lambda p : print (p [0] + " : ", end = '') or print_list (p [1]), lists))

    l1.set_next (l2)
    l2.set_next (l3)
    l3.set_next (l4)
    l4.set_next (l5)

    print ()

    print (">>> After linking and before reversing the linked list")
    list (map (lambda p : print (p [0] + " : ", end = '') or print_list (p [1]), lists))

    print ()

    print ("\
 The execution of `reverse l1´ computes the reversed\n\
 list of l1 and the returned result is equal to l5,\n\
 the new head of the linked list.\n\
")

    reverse (l1)

    print (">>> After reversing the linked list")
    list (map (lambda p : print (p [0] + " : ", end = '') or print_list (p [1]), lists))

    print ()



if __name__ == "__main__" :
    main ()


