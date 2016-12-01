require "minruby"

def evaluate(tree, env)
  case tree[0]
  when "lit"
    tree[1]
  when "+"
    evaluate(tree[1]) + evaluate(tree[2])
  when "-"
    evaluate(tree[1]) - evaluate(tree[2])
  when "*"
    evaluate(tree[1]) * evaluate(tree[2])
  when "/"
    evaluate(tree[1]) / evaluate(tree[2])
  when "%"
    evaluate(tree[1]) % evaluate(tree[2])
  when "**"
    evaluate(tree[1]) ** evaluate(tree[2])
  when "<"
    evaluate(tree[1]) < evaluate(tree[2])
  when "<="
    evaluate(tree[1]) <= evaluate(tree[2])
  when "=="
    evaluate(tree[1]) == evaluate(tree[2])
  when ">="
    evaluate(tree[1]) >= evaluate(tree[2])
  when ">"
    evaluate(tree[1]) > evaluate(tree[2])
  when "func_call" # 仮の実装
    p(evaluate(tree[2]))
  when "stmts"
    i = 1
    last = nil
    while tree[i]
      last = evaluate(tree[i], env)
      i = i + 1
    end
    last
  when "var_assign"
    env[tree[1]] = evaluate(tree[2], env)
  when "var_ref"
    env[tree[1]]
  end
end

str = minruby_load()
tree = minruby_parse(str)
answer = evaluate(tree)
