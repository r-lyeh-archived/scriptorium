
#include <windows.h>
#include <sgscript.h>


#define APPNAME "SGSVM Executable Stub"


#define FILE_READ 1
#define FILE_WRITE 2
HANDLE CreateFileFast( const CHAR* name, DWORD rw, DWORD mode )
{
	DWORD access = 0;
	DWORD share = 0;
	if( rw & FILE_READ )
	{
		access |= GENERIC_READ;
		share |= FILE_SHARE_READ;
	}
	if( rw & FILE_WRITE )
	{
		access |= GENERIC_WRITE;
		share |= FILE_SHARE_WRITE;
	}
	return CreateFile( name, access, share, NULL, mode, FILE_ATTRIBUTE_NORMAL, NULL );
}


int main( int argc, char* argv[] )
{
	int i;
	BYTE buf[ 4096 ], *path, *data;
	DWORD read, written, scriptsize = 0;
	HANDLE fh;
	
	path = buf;
	
	/* open EXE file */
	read = GetModuleFileName( NULL, (CHAR*) buf, sizeof( buf ) );
	if( read >= sizeof( buf ) )
	{
		path = malloc( read + 1 );
		GetModuleFileName( NULL, (CHAR*) buf, sizeof( buf ) );
	}
	fh = CreateFileFast( (CHAR*) buf, FILE_READ, OPEN_EXISTING );
	if( path != buf )
		free( path );
	if( fh == INVALID_HANDLE_VALUE )
		return E_FAIL;
	SetFilePointer( fh, -4, NULL, FILE_END );
	ReadFile( fh, &scriptsize, sizeof( scriptsize ), &read, NULL );
	
	/* read the following data */
	if( scriptsize == 0 )
	{
		if( argc == 3 )
		{
			HANDLE hsgs, hout = CreateFileFast( argv[ 1 ], FILE_WRITE, CREATE_ALWAYS );
			if( !hout )
			{
				MessageBox( 0, "Could not open executable file for writing", APPNAME, MB_ICONERROR );
				CloseHandle( fh );
				return E_FAIL;
			}
			hsgs = CreateFileFast( argv[ 2 ], FILE_READ, OPEN_EXISTING );
			if( !hsgs )
			{
				MessageBox( 0, "Could not open script file for reading", APPNAME, MB_ICONERROR );
				CloseHandle( hout );
				CloseHandle( fh );
				return E_FAIL;
			}
			SetFilePointer( fh, 0, NULL, FILE_BEGIN );
			while( ReadFile( fh, buf, sizeof( buf ), &read, NULL ) && read )
				WriteFile( hout, buf, read, &written, NULL );
			while( ReadFile( hsgs, buf, sizeof( buf ), &read, NULL ) && read )
				WriteFile( hout, buf, read, &written, NULL );
			scriptsize = GetFileSize( hsgs, NULL );
			WriteFile( hout, &scriptsize, 4, &written, NULL );
			CloseHandle( fh );
			CloseHandle( hsgs );
			CloseHandle( hout );
			MessageBox( 0, "File saved!", APPNAME, MB_ICONINFORMATION );
			return S_OK;
		}
		else
		{
			const char* info = "To create an executable from .sgs"
				", run sgsexe <output-file.exe> <script-file.sgs>."
				"\n\nglobal 'argv' will be the array of arguments";
			MessageBox( 0, info, APPNAME, MB_ICONINFORMATION );
			CloseHandle( fh );
			return E_ABORT;
		}
	}
	
	data = malloc( scriptsize );
	SetFilePointer( fh, -4-(LONG)scriptsize, NULL, FILE_END );
	ReadFile( fh, data, scriptsize, &read, NULL );
	CloseHandle( fh );
	
	{
		SGS_CTX = sgs_CreateEngine();
		
		for( i = 0; i < argc; ++i )
			sgs_PushString( C, argv[ i ] );
		
		sgs_CreateArray( C, NULL, argc );
		sgs_SetGlobalByName( C, "argv", sgs_StackItem( C, -1 ) );
		sgs_Pop( C, 1 );
		
		sgs_SetGlobalByName( C, "argc", sgs_MakeInt( argc ) );
		
		sgs_ExecBuffer( C, (char*) data, scriptsize );
		
		sgs_DestroyEngine( C );
	}
	
	free( data );
	
	return 0;
}
