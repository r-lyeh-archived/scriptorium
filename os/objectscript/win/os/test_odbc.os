
var conn = ODBO("odbc", {
 dsn = "soci_test"
 })

 var stmt = conn.query("select * from tbl_user where id > :id", {
  id = 2
 })
 for(var i, row in stmt){
  print row
 }
 
 print "Test insert sql"
 conn.begin()
 conn.execute("insert into tbl_user (username, password, reg_date) values(:username, :password, :reg_date)", {
  // id = 10,
  reg_date = DateTime(2013, 04, 01, 10, 00),
  username = "user",
  password = "passs",
 })
 print "lastInsertId: ${conn.lastInsertId}"
 conn.commit()
