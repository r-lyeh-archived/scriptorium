// This file is distributed under GPLv3 licence
// Author: Gorelov Grigory (gorelov@grigory.info)
//
// Contacts and other info are on the WEB page:  grigory.info/MPFDParser


#ifndef _FIELD_H
#define	_FIELD_H

#include "Exception.h"
#ifndef FSTREAM_FILE
#include <stdio.h>
#else
#include <iostream>
#include <fstream>
#endif
#include <stdlib.h>
#include <string.h>
#include <sstream>

namespace MPFD {

    class Field {
    public:
        static const int TextType = 1, FileType = 2;

        Field();
        virtual ~Field();

        void SetType(int type);
        int GetType();

        void AcceptSomeData(char *data, long length);
		void FinishData();

        // File functions
        void SetUploadedFilesStorage(int where);
        void SetTempDir(std::string dir);

        void SetFileName(std::string name);
        std::string GetFileName();

        void SetFileContentType(std::string type);
        std::string GetFileMimeType();

        char * GetFileContent();
        unsigned long GetFileContentSize();

        std::string GetTempFileNameEx();

        // Text field operations
        std::string GetTextTypeContent();




    private:
        unsigned long FieldContentLength;

        int WhereToStoreUploadedFiles;

        std::string TempDir, TempFile;
        std::string FileContentType, FileName;

        int type;
        char * FieldContent;
#ifndef FSTREAM_FILE
		FILE * file;
#else
        std::ofstream file;
#endif
    };
}
#endif	/* _FIELD_H */

