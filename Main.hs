fatorial :: Int -> Int
fatorial x
  | x <= 1 = 1
  | otherwise = x * fatorial (x-1)

fatorial2 1 = 1
fatorial2 x = x * fatorial (x - 1)

bhsk a b c =
  let delta = b * b - (4 * a * c)
      form = ((-b) + sqrt delta) / (2 * a)
  in form

rightBhsk a b c = (x1, x2)
  where delta = b * b - (4 * a * c)
        form op = op (-b) (sqrt delta) / (2 * a)
        x1 = form (+)
        x2 = form (-)

f g x = x + g 1
z = f (*5)

headz [] = error "Empty list"
headz (x:_) = x

main = do
  print(fatorial 10)
  print(fatorial2 5)
  -- print f (*5) 3
  print(z 3)
  print(bhsk 1 12 (-13))
  print(rightBhsk 1 12 (-13))
  print(headz [6,2])
  print(headz [1,2,3])
  