=begin
def preorder(tree)
  p (tree[0]) # 各部分木でやりたい処理
  if tree[0].start_with?("節")
    preorder(tree[1])
    preorder(tree[2])
  end
end

node1 = ["節1", ["節2", ["葉A"], ["葉B"]], ["節3", ["葉C"], ["葉D"]]]
=end

def node1
  ["節1", ["節2", ["葉A"], ["葉B"]], ["節3", ["葉C"], ["葉D"]]]
end

def exercise1
  ["節1", ["節2", ["葉A"], ["節3", ["葉B"], ["葉C"]]], ["葉D"]]
end

def exercise2(tree)
  if tree[0].start_with?("節")
    exercise2(tree[1])
    exercise2(tree[2])
  else
    p (tree[0])
  end
end

def exercise3(tree)
  if tree[0].start_with?("節")
    exercise3(tree[1])
    exercise3(tree[2])
  end
  p (tree[0])
end
