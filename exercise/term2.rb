def exercise2
  answer = 0
  i = 1
  while i <= 1000
    if i % 2 == 0
      answer = answer + i
    end
    i = i + 1
  end
  p(answer)
end

def fizzbuzz(n)
  i = 1
  while i <= n
    if i % 3 == 0
      print("Fizz")
    end
    if i % 5 == 0
      print("Buzz")
    end
    if i % 3 != 0 && i % 5 != 0
      print(i)
    end
    i = i + 1
    puts ""
  end
end
