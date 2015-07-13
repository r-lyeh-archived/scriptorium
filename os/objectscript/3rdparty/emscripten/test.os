print "testNumVal: ${testNumVal}"
print "testStrVal: ${testStrVal}"
print "testNullVal: ${testNullVal}"
print "testUndefVal: ${testUndefVal}"
print "testTrueVal: ${testTrueVal}"
print "testFalseVal: ${testFalseVal}"
print "testObjVal: ${testObjVal}"
print "testObjVal.user: ${testObjVal.user}"
print "testObjVal.item.town: ${testObjVal.item.town}"
print "testObjVal.item.func(1, 'two'): ${testObjVal.item.func(1, 'two', testObjVal.item)}"
print "testObjVal.unknown: ${testObjVal.unknown}"
print "testObjVal.unknown.unknown: ${testObjVal.unknown.unknown}"
print "Math.sin(11): ${Math.sin(11)}"
print "$(\"#run_value\").val(): "..$("#run_value").val(math.random(1,10)|0)

console.log(window)
