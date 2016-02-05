a := 5; -- Predefined Variable -- Test SetStmt
print 2; => 2 -- Test IntExp
print a; => 5 -- Test VarExp
print if 5<10 then 2 else 4 fi; => 2 -- Test IfExp
print if 5>10 then 2 else 4 fi; => 4
print true and true; => True -- Test BoolOpExp
print true and false; => False
print true or false; => True
print 10<20; => True -- Test CompOpExp
print 10>20; => False
print 20<20; => False
print 10==20; => False
print 10*20; => 200 -- Test IntOpExp
print 20-10; => 10
print 10+20; => 30
add:= fn [x,y] x+y end; -- Test FunExp and SetStmt
print call add (1,2); => 3 -- Test AppExp and CallStmt
print (let [a 10 b 2] a+b end); => 12 -- Test LetExp
