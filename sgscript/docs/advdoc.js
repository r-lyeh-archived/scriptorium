"use strict";

///////
// CORE LIB //
     /////////
function foreach_do( arr, fn )
{
	for( var i = 0; i < arr.length; ++i )
		fn( arr[ i ] );
}
function find( selector, startNode )
{
	return document.querySelector( selector, startNode );
}
function findAll( selector, startNode )
{
	return document.querySelectorAll( selector, startNode );
}
function findID( id )
{
	return document.getElementById( id );
}
function bind( nodes, type, func )
{
	if( nodes instanceof Array )
	{
		for( var i = 0; i < nodes.length; ++i )
			bind( nodes[ i ], type, func );
		return;
	}
	nodes.addEventListener( type, func );
}
function empty( node )
{
	node.innerHTML = "";
	return node;
}
function is_attrib( nm )
{
	return (
		nm == "class" ||
		nm == "style" ||
		nm == "id"
	);
}
function element( type, attribs, ch )
{
	var el = document.createElement( type );
	if( attribs != null )
	{
		for( var key in attribs )
		{
			if( is_attrib( key ) )
				el.setAttribute( key, attribs[ key ] );
			else
				el[ key ] = attribs[ key ];
		}
	}
	if( ch instanceof Array )
	{
		for( var i = 0; i < ch.length; ++i )
		{
			el.appendChild( ch[ i ] );
		}
	}
	return el;
}
function goto_anchor( a )
{
	window.location.href = "#" + a;
}
function text2html( text )
{
	return text
		.replace( "&", "&amp;")
		.replace( "<", "&lt;")
		.replace( ">", "&gt;")
		.replace( "\"", "&quot;")
		.replace( "'", "&#039;");
}
function html2text( html, addspaces )
{
	function repfn( match ){ return match + " "; }
	if( addspaces )
	{
		html = html.replace( /<\/\s*[a-zA-Z0-9]+\s*>/gi, repfn );
	}
	var tmp = document.createElement( "DIV" );
	tmp.innerHTML = html;
	return tmp.textContent;
}

/////
// SEARCH //
    ////////
window.srch_unnecessary_words =
[
	"and","or","if","else","for","but","when","is","not","it","this","to","the","in",
	"thus","be","that","of","does","doesn","t","a","with","its","can","also","the","there",
	"are","do","while","yes","no","maybe","any","than","then","isn"
];
function srch_necessary_word( word )
{
	if( word.length <= 2 )
		return false;
	return srch_unnecessary_words.indexOf( word ) == -1;
}
function srch_split_into_normalized_words( text )
{
	var text = text.replace( /[^a-zA-Z0-9_]+/, " " );
	var words = text.split( " " );
	var out = [];
	for( var i = 0; i < words.length; ++i )
	{
		var word = words[ i ].toLowerCase();
		if( word && srch_necessary_word( word ) && out.indexOf( word ) == -1 )
			out.push( word );
	}
	return out;
}
function srch_highlight( text, words )
{
	function replace( match )
	{
		return "<mark>" + match + "</mark>";
	}
	for( var i = 0; i < words.length; ++i )
	{
		text = text.replace( new RegExp( words[ i ], "gi" ), replace );
	}
	return text;
}

/////
// DOCS //
   ///////
function map_toc()
{
	var path_info = {};
	for( var i = 0; i < sgs_toc.length; ++i )
	{
		var item = sgs_toc[ i ];
		var path = item[0];
		var at = path.lastIndexOf( ":" );
		var parent_path = at != -1 ? path.substring( 0, at ) : null;
		var alias = at != -1 ? path.substring( at + 1 ) : path;
		var data = { path: path, alias: alias, title: item[1], parent: parent_path, ch: [] };
		path_info[ path ] = data;
		if( at != -1 )
		{
			path_info[ parent_path ].ch.push( path );
			// also add the end-only version
			path_info[ alias ] = data;
		}
	}
	window.path_info = path_info;
}
function preproc_links()
{
	var links = findAll( "#_data_ a" );
	for( var i = 0; i < links.length; ++i )
	{
		var link = links[ i ];
		var href = link.getAttribute( "href" );
		if( path_info[ href ] != null )
			link.setAttribute( "href", "#" + href );
		else if( href.indexOf( "docs://" ) == 0 )
		{
			var path = href.substring( 7 );
			var sat = path.indexOf( "/" );
			if( sat != -1 )
			{
				href = path.substring( 0, sat ) + ".docs.htm#" + path.substring( sat + 1 );
			}
			else
			{
				href = path + ".docs.htm";
			}
			link.setAttribute( "href", href );
			link.setAttribute( "target", "_blank" );
		}
	}
}
function doc_create_entry( path )
{
	var item = path_info[ path ];
	var entry = element( "entry", { textContent: item.title, id: "entry:" + path, path: item.alias } );
	if( item.ch.length )
	{
		var entries = [];
		for( var i = 0; i < item.ch.length; ++i )
			entries.push( doc_create_entry( item.ch[ i ] ) );
		return element( "entryset", { id: "entryset:" + path }, [entry, element( "entrych", null, entries )] );
	}
	return entry;
}
function doc_select_page( path )
{
	var info = path_info[ path ];
	path = info.path;
	foreach_do( findAll( "toc .active" ), function(e){ e.classList.remove( "active" ); } );
	var tocActiveEntry = findID( "entry:" + path );
	tocActiveEntry.classList.add( "active" );
	var title = find( "#view ptitle" );
	var cont = find( "#view pcont" );
	empty( title ).textContent = path_info[ path ].title;
	empty( cont ).appendChild( findID( path ).cloneNode(true) );
	
	// related links
	var rellinks = empty( find( "#view relatedlinks" ) );
	if( info.ch.length )
	{
		var entries = [];
		for( var i = 0; i < info.ch.length; ++i )
		{
			var subitem = path_info[ info.ch[ i ] ];
			entries.push( element( "li", {},
			[
				element( "a", { href: "#" + subitem.alias, textContent: subitem.title } )
			]));
		}
		rellinks.appendChild( element( "content", null,
		[
			element( "subtitle", { textContent: "In this section..." } ),
			element( "ul", null, entries ),
		]));
	}
	
	// breadcrumbs
	var breadcrumbs = empty( find( "#view breadcrumbs" ) );
	breadcrumbs.style.display = "";
	var bclist = [];
	var cur = info;
	while( cur )
	{
		bclist.unshift( element( "a", { href: "#" + cur.alias, textContent: cur.title } ) );
		cur = path_info[ cur.parent ];
	}
	for( var i = 0; i < bclist.length; ++i )
	{
		if( i )
			breadcrumbs.appendChild( element( "span", { "class": "sep", innerHTML: "&#187;" } ) );
		breadcrumbs.appendChild( bclist[ i ] );
	}
	
	// make sure the active TOC item is visible
	if( tocActiveEntry.scrollIntoViewIfNeeded )
		tocActiveEntry.scrollIntoViewIfNeeded();
	else
		tocActiveEntry.scrollIntoView();
	// make sure top of the page is visible
	find("#view").scrollTop = 0;
}
function _dbg( cont, text )
{
	cont.appendChild( element( "dbglog", { textContent: text } ) );
}
function doc_search( text )
{
	text = text.trim();
	find( "toc search input" ).value = text;
	
	var debug = false;
	if( text.indexOf( "/d:" ) == 0 )
	{
		debug = true;
		text = text.substring( 3 );
	}
	
	var breadcrumbs = empty( find( "#view breadcrumbs" ) );
	breadcrumbs.style.display = "none";
	var rellinks = empty( find( "#view relatedlinks" ) );
	
	var title = find( "#view ptitle" );
	var cont = find( "#view pcont" );
	empty( title ).textContent = "Search results for \"" + text + "\"";
	empty( cont );
	
	// search data
	var sd_words = sgs_searchindex.words;
	var sd_pages = sgs_searchindex.pages;
	var sd_firsttwo = sgs_searchindex.firsttwo;
	
	var words = srch_split_into_normalized_words( text );
	if( debug )
	{
		_dbg( cont, "Words: " + words.join( ", " ) );
	}
	
	// narrow down search list by shortcut features, compare words against page words
	var pages = {};
	var pwcnt = {};
	for( var i = 0; i < words.length; ++i )
	{
		var word = words[ i ];
		var first2 = word.substring( 0, 2 );
		// look up smaller word list by first two letters
		var widlist = sd_firsttwo[ first2 ];
		if( debug )
		{
			_dbg( cont, "Testing word: '" + word + "', First two letters: '" + first2 + "'" );
		}
		if( widlist != null )
		{
			if( debug )
			{
				_dbg( cont, "Matching word count: " + widlist.length );
				_dbg( cont, "Found words IDs: " + widlist.join( ", " ) );
			}
			for( var j = 0; j < widlist.length; ++j )
			{
				var worditem = sd_words[ widlist[ j ] ];
				// if neither word contains the other, it's not a match
				if( worditem[0].indexOf( word ) == -1 && word.indexOf( worditem[0] ) == -1 )
				{
					if( debug )
					{
						_dbg( cont, "NOT match with word '" + worditem[0] + "'" );
					}
					continue;
				}
				var divisor = 1 + Math.abs( word.length - worditem[0].length );
				if( debug )
				{
					_dbg( cont, "MATCH with word '" + worditem[0] + "', divisor: " + divisor );
				}
				var numpages = 0;
				for( var pid in worditem[1] )
				{
					numpages++;
					var pfactor = worditem[1][ pid ] / divisor;
					if( pages[ pid ] != null )
					{
						pages[ pid ] += pfactor;
						pwcnt[ pid ] += 1;
					}
					else
					{
						pages[ pid ] = pfactor;
						pwcnt[ pid ] = 1;
					}
				}
				if( debug )
				{
					_dbg( cont, "Pages added: " + numpages );
				}
			}
		}
	}
	
	// filter results that hit all search words
	var fpages = [];
	var wordcount = words.length;
	for( var pid in pwcnt )
	{
		// not all words were found
		if( wordcount > pwcnt[ pid ] )
		{
			if( debug )
			{
				_dbg( cont, "Dropped page " + pid + " because of insufficient word count (need: "
					+ wordcount + ", got: " + pwcnt[ pid ] + ")" );
			}
			continue;
		}
		fpages.push([ pid, pages[ pid ] ]);
	}
	// sort by similarity, most to least
	fpages.sort(function(a,b){ return b[1] - a[1]; });
	
	for( var i = 0; i < fpages.length; ++i )
	{
		var pid = fpages[ i ][0];
		var pageurl = sd_pages[ pid ];
		var info = path_info[ pageurl ];
		
		var title = info.title;
		var text = html2text( findID( info.path ).innerHTML, true );
		if( text.length > 300 )
		{
			text = text.substring( 0, 300 ) + "...";
		}
		
		// cleanup
		text = text.replace( /\s+/g, " " );
		
		// to html
		title = text2html( title );
		text = text2html( text );
		
		// highlight
		title = srch_highlight( title, words );
		text = srch_highlight( text, words );
		
		cont.appendChild( element( "searchresult", {},
		[
			element( "srttl", null, [element( "a", { "class": "title", href: "#" + pageurl, innerHTML: title } )] ),
			element( "desc", { innerHTML: text } ),
		]));
	}
}
function doc_create_toc()
{
	var entries = [];
	for( var key in window.path_info )
	{
		var item = window.path_info[ key ];
		if( !item.parent )
			entries.push( doc_create_entry( key ) );
	}
	var entrylist, searchinput;
	var out = element( "toc", null,
	[
		element( "header", null,
		[
			find( "logo" ),
			element( "subtitle", { textContent: find("title").textContent } ),
			element( "search", null,
			[
				searchinput = element( "input", { type: "text", placeholder: "Search..." } ),
			]),
		]),
		element( "cont", null,
		[
			entrylist = element( "entrylist", null, entries ),
		]),
	]);
	bind( entrylist, "click", function(e)
	{
		if( e.target.tagName == "ENTRY" )
		{
			goto_anchor( e.target.path );
		}
	});
	bind( searchinput, "keyup", function(e)
	{
		if( e.keyCode == 13 )
		{
			goto_anchor( "search:" + e.target.value );
		}
	});
	return out;
}
function doc_create_view()
{
	var cont;
	var out = element( "view", { id: "view" },
	[
		element( "breadcrumbs", { style: "display:none" } ),
		element( "ptitle" ),
		cont = element( "pcont", { innerHTML: "<introtext>Click on a topic to view its contents</introtext>" } ),
		element( "relatedlinks" ),
	]);
	return out;
}
function doc_onhash()
{
	var hash = window.location.hash.substring( 1 );
	if( hash.indexOf( "search:" ) == 0 )
	{
		doc_search( hash.substring( 7 ) );
	}
	else if( path_info[ hash ] != null )
	{
		doc_select_page( hash );
	}
	else
	{
		var loc = window.location.href, index = loc.indexOf('#');
		if( index != -1 )
			window.location = loc.substring( 0, index );
	}
}

bind( window, "load", function()
{
	map_toc();
	preproc_links();
	var frame = element( "docframe", null,
	[
		doc_create_toc(),
		doc_create_view(),
	]);
	empty( find("#_frame_") ).appendChild( frame );
	
	if( window.location.hash )
	{
		doc_onhash();
	}
});
bind( window, "hashchange", function()
{
	doc_onhash();
});
