fannkuch = function(n)
{
  var p = [], q = [], s = [], sign = 1, maxflips = 0, sum = 0;
  var i;
  for(i=1; i<=n; i++) p[i] = q[i] = s[i] = i;
  for(;;){
    // Copy and flip.
    var q1 = p[1];				// Cache 1st element.
    if(q1 != 1){
      for(i=2; i<=n; i++) q[i] = p[i];		// Work on a copy.
      var flips = 1;
      for(;;){
		var qq = q[q1];
		if(qq == 1){				// ... until 1st element is 1.
		  sum = sum + sign*flips;
		  if(flips > maxflips){
			maxflips = flips;
		  } // New maximum?
		  break;
		}
		q[q1] = q1;
		if(q1 >= 4){
		  var i = 2, j = q1 - 1
		  for(;;){ var tmp = q[i]; q[i] = q[j]; q[j] = tmp; if(++i >= --j) break; }
		}
		q1 = qq; flips++;
      }
    }
    // Permute.
    if(sign == 1){
      var tmp = p[2]; p[2] = p[1]; p[1] = tmp; sign = -1;	// Rotate 1<-2.
    }else{
      var tmp = p[2]; p[2] = p[3]; p[3] = tmp; sign = 1;	// Rotate 1<-2 and 1<-2<-3.
      for(i = 3;; i++){
		// print "mark 4"
		var sx = s[i];
		if(sx != 1){ s[i] = sx-1; break; }
		if(i == n) return [sum, maxflips];	// Out of permutations.
		s[i] = i;
		// Rotate 1<-...<-i+1.
		var t = p[1]; for(var j = 1; j <= i; j++){ p[j] = p[j+1]; } p[i+1] = t;
      }
    }
  }
}

/*
function getTimeSec(){
 	var d = new Date();
	return (d.getTime() + d.getMilliseconds() / 1000.0) / 1000.0;
}

var n = 10;
var start_time = getTimeSec();
var r = fannkuch(n);
var sum = r[0], flips = r[1];
WScript.Echo(
	sum,"\n", 
	"Pfannkuchen(",n,") = ",flips,"\n",
	"time = ",(getTimeSec() - start_time),"\n"
)
*/
