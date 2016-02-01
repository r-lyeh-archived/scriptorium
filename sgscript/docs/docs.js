
function sgsCC( el )
{
	var code = el.innerHTML;
	code += "\n";
	
	code = code.replace( "&quot;", "\uE000" );
	code = code.replace( "&#39;", "\uE001" );
	
	code = code.replace( /(\uE000(?:[^\uE000\\]|\\.)*\uE000)/g, '<span class="sgsSTR">$1</span>' );
	code = code.replace( /(\uE001(?:[^\uE001\\]|\\.)*\uE001)/g, '<span class="sgsSTR">$1</span>' );
	code = code.replace( /(^|[^0-9a-zA-Z_])(0b[0-1]+)/g, '$1<span class="sgsNUM">$2</span>' );
	code = code.replace( /(^|[^0-9a-zA-Z_])(0o[0-8]+)/g, '$1<span class="sgsNUM">$2</span>' );
	code = code.replace( /(^|[^0-9a-zA-Z_])(0x[0-9a-fA-F]+)/g, '$1<span class="sgsNUM">$2</span>' );
	code = code.replace( /(^|[^0-9a-zA-Z_])(-?[0-9]+(\.[0-9]+)?([eE][-+][0-9]+)?)/g, '$1<span class="sgsNUM">$2</span>' );
	code = code.replace( /(^|[^0-9a-zA-Z_])(if|else|do|while|for|foreach|break|continue|var|global|function|return|null|true|false|use)(\s|\b)/g, '$1<span class="sgsKEY">$2</span>$3' );
	code = code.replace( /(\(|\)|\[|\]|\{|\})/g, '<span class="sgsSPC">$1</span>' );
	code = code.replace( /(\/\/[^\n\r]*)([\r\n])/g, '<span class="sgsCOM">$1</span>$2' );
	code = code.replace( /(\/\*[\s\S]*?\*\/)/g, '<span class="sgsCOM">$1</span>' );
	
	code = code.replace( "\uE000", "&quot;" );
	code = code.replace( "\uE001", "&#39;" );
	
	code = code.replace(/^\s+|\s+$/g, '');
	el.innerHTML = code;
}

function sgsCCify( tag )
{
	var codes = document.getElementsByTagName( tag );
	for( var i = 0; i < codes.length; ++i )
	{
		sgsCC( codes[ i ] );
	}
}

window.onload = function()
{
	sgsCCify( 'CODE' );
	sgsCCify( 'PRE' );
	var ems = document.getElementsByTagName( "EM" );
	for( var i = 0; i < ems.length; ++i )
	{
		var icon = document.createElement( 'SPAN' );
		icon.setAttribute( "class", "notice" );
		icon.appendChild( document.createTextNode( "i" ) );
		ems[ i ].insertBefore( icon, ems[ i ].firstChild );
	}
}
