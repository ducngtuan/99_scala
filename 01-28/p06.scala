/** Check if a list is a palindrome */

def isPalindrome[A](list: List[A]) = list == list.reverse

println(isPalindrome(List(1, 2, 3)))
println(isPalindrome(List(1, 2, 1)))