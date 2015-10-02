<?php
function fib($num){
	if($num<2){
		return $num;
	} else {
		return (fib($num-1) + fib($num-2));
	}
}

echo fib(34) + "\n"; 
?>
