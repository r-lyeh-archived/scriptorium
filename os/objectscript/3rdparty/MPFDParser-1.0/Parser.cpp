// This file is distributed under GPLv3 licence
// Author: Gorelov Grigory (gorelov@grigory.info)
//
// Contacts and other info are on the WEB page:  grigory.info/MPFDParser


#include "Parser.h"

std::map<std::string, MPFD::Field *> MPFD::Parser::GetFieldsMap() {
    return Fields;
}

MPFD::Parser::Parser() {
    DataCollector = NULL;
    DataCollectorLength = 0;
	IsExternalDataBuffer = false;
    _HeadersOfTheFieldAreProcessed = false;
    CurrentStatus = Status_LookingForStartingBoundary;

    MaxDataCollectorLength = 16 * 1024 * 1024; // 16 Mb default data collector size.

    SetUploadedFilesStorage(StoreUploadedFilesInFilesystem);
}

MPFD::Parser::~Parser() {
    std::map<std::string, Field *>::iterator it;
    for (it = Fields.begin(); it != Fields.end(); it++) {
        delete it->second;
    }

    if (DataCollector && !IsExternalDataBuffer) {
        delete DataCollector;
    }
}

void MPFD::Parser::SetContentType(const std::string type) {
    if (type.find("multipart/form-data;") != 0) {
        throw MPFD::Exception(std::string("Content type is not \"multipart/form-data\"\nIt is \"") + type + std::string("\""));
    }


    int bp = type.find("boundary=");
	if (bp == (int)std::string::npos) {
        throw MPFD::Exception(std::string("Cannot find boundary in Content-type: \"") + type + std::string("\""));
    }

    Boundary = std::string("--") + type.substr(bp + 9, type.length() - bp);
}

void MPFD::Parser::AcceptSomeData(const char *data, const long length) {
    if (Boundary.length() > 0) {
		if(IsExternalDataBuffer){
			throw MPFD::Exception("Accepting data, but external data was used.");
		}
        // Append data to existing accumulator
        if (DataCollector == NULL) {
            DataCollector = new char[length];
            memcpy(DataCollector, data, length);
            DataCollectorLength = length;
        } else {
            DataCollector = (char*) realloc(DataCollector, DataCollectorLength + length);
            memcpy(DataCollector + DataCollectorLength, data, length);
            DataCollectorLength += length;
        }

        if (DataCollectorLength > MaxDataCollectorLength) {
            throw Exception("Maximum data collector length reached.");
        }

        _ProcessData();
    } else {
        throw MPFD::Exception("Accepting data, but content type was not set.");
    }
}

void MPFD::Parser::SetExternalDataBuffer(const char *data, const long length)
{
    if (Boundary.length() > 0) {
		if(!IsExternalDataBuffer && DataCollectorLength > 0){
			throw MPFD::Exception("Accepting data, but internal data was used.");
		}
		DataCollector = (char*)data;
		DataCollectorLength = length;
        _ProcessData();
    } else {
        throw MPFD::Exception("Accepting data, but content type was not set.");
    }
}

void MPFD::Parser::FinishData()
{
	std::map<std::string, MPFD::Field *>& fields = this->Fields;
	std::map<std::string, MPFD::Field *>::iterator it;
	for(it = fields.begin(); it != fields.end(); it++){
		MPFD::Field * field = fields[it->first];
		field->FinishData();
	}
}

void MPFD::Parser::_ProcessData() {
    // If some data left after truncate, process it right now.
    // Do not wait for AcceptSomeData called again
    bool NeedToRepeat;

    do {
        NeedToRepeat = false;
        switch (CurrentStatus) {
            case Status_LookingForStartingBoundary:
                if (FindStartingBoundaryAndTruncData()) {
                    CurrentStatus = Status_ProcessingHeaders;
                    NeedToRepeat = true;
                }
                break;

            case Status_ProcessingHeaders:
                if (WaitForHeadersEndAndParseThem()) {
                    CurrentStatus = Status_ProcessingContentOfTheField;
                    NeedToRepeat = true;
                }
                break;

            case Status_ProcessingContentOfTheField:
                if (ProcessContentOfTheField()) {
                    CurrentStatus = Status_LookingForStartingBoundary;
                    NeedToRepeat = true;
                }
                break;
        }
    } while (NeedToRepeat);
}

bool MPFD::Parser::ProcessContentOfTheField() {
    long BoundaryPosition = BoundaryPositionInDataCollector();
    long DataLengthToSendToField;
    if (BoundaryPosition >= 0) {
        // 2 is the \r\n before boundary we do not need them
        DataLengthToSendToField = BoundaryPosition - 2;
    } else {
        // We need to save +2 chars for \r\n chars before boundary
        DataLengthToSendToField = DataCollectorLength - (Boundary.length() + 2);
    }

    if (DataLengthToSendToField > 0) {
        Fields[ProcessingFieldName]->AcceptSomeData(DataCollector, DataLengthToSendToField);
        TruncateDataCollectorFromTheBeginning(DataLengthToSendToField);
    }

    if (BoundaryPosition >= 0) {
        CurrentStatus = Status_LookingForStartingBoundary;
        return true;
    } else {
        return false;
    }
}

bool MPFD::Parser::WaitForHeadersEndAndParseThem() {
    for (int i = 0; i < DataCollectorLength - 3; i++) {
        if ((DataCollector[i] == 13) && (DataCollector[i + 1] == 10) && (DataCollector[i + 2] == 13) && (DataCollector[i + 3] == 10)) {
            long headers_length = i;
            char *headers = new char[headers_length + 1];
            memset(headers, 0, headers_length + 1);
            memcpy(headers, DataCollector, headers_length);

            _ParseHeaders(std::string(headers));

            TruncateDataCollectorFromTheBeginning(i + 4);

            delete headers;

            return true;
        }
    }
    return false;
}

void MPFD::Parser::SetUploadedFilesStorage(int where) {
    WhereToStoreUploadedFiles = where;
}

void MPFD::Parser::SetTempDirForFileUpload(std::string dir) {
    TempDirForFileUpload = dir;
}

void MPFD::Parser::_ParseHeaders(std::string headers) {
    // Check if it is form data
    if (headers.find("Content-Disposition: form-data;") == std::string::npos) {
        throw Exception(std::string("Accepted headers of field does not contain \"Content-Disposition: form-data;\"\nThe headers are: \"") + headers + std::string("\""));
    }

    // Find name
    int name_pos = headers.find("name=\"");
    if (name_pos == (int)std::string::npos) {
        throw Exception(std::string("Accepted headers of field does not contain \"name=\".\nThe headers are: \"") + headers + std::string("\""));
    } else {
        int name_end_pos = headers.find("\"", name_pos + 6);
        if (name_end_pos == (int)std::string::npos) {
            throw Exception(std::string("Cannot find closing quote of \"name=\" attribute.\nThe headers are: \"") + headers + std::string("\""));
        } else {
            ProcessingFieldName = headers.substr(name_pos + 6, name_end_pos - (name_pos + 6));
            Fields[ProcessingFieldName] = new Field();
        }


        // find filename if exists
        int filename_pos = headers.find("filename=\"");
        if (filename_pos == (int)std::string::npos) {
            Fields[ProcessingFieldName]->SetType(Field::TextType);
        } else {
            Fields[ProcessingFieldName]->SetType(Field::FileType);
            Fields[ProcessingFieldName]->SetTempDir(TempDirForFileUpload);
            Fields[ProcessingFieldName]->SetUploadedFilesStorage(WhereToStoreUploadedFiles);

            int filename_end_pos = headers.find("\"", filename_pos + 10);
            if (filename_end_pos == (int)std::string::npos) {
                throw Exception(std::string("Cannot find closing quote of \"filename=\" attribute.\nThe headers are: \"") + headers + std::string("\""));
            } else {
                std::string filename = headers.substr(filename_pos + 10, filename_end_pos - (filename_pos + 10));
                Fields[ProcessingFieldName]->SetFileName(filename);
            }

            // find Content-Type if exists
            int content_type_pos = headers.find("Content-Type: ");
            if (content_type_pos != (int)std::string::npos) {
                int content_type_end_pos = 0;
                for (int i = content_type_pos + 14; (i < (int)headers.length()) && (!content_type_end_pos); i++) {
                    if ((headers[i] == ' ') || (headers[i] == 10) || (headers[i] == 13)) {
                        content_type_end_pos = i - 1;
                    }
                }
                std::string content_type = headers.substr(content_type_pos + 14, content_type_end_pos - (content_type_pos + 14));
                Fields[ProcessingFieldName]->SetFileContentType(content_type);
            }


        }

    }


}

void MPFD::Parser::SetMaxCollectedDataLength(long max) {
    MaxDataCollectorLength = max;
}

void MPFD::Parser::TruncateDataCollectorFromTheBeginning(long n) {
    long TruncatedDataCollectorLength = DataCollectorLength - n;

    char *tmp = DataCollector;

    DataCollector = new char[TruncatedDataCollectorLength];
    memcpy(DataCollector, tmp + n, TruncatedDataCollectorLength);

    DataCollectorLength = TruncatedDataCollectorLength;

    delete tmp;

}

long MPFD::Parser::BoundaryPositionInDataCollector() {
    const char *b = Boundary.c_str();
    int bl = Boundary.length();
    if (DataCollectorLength >= bl) {
        bool found = false;
        for (int i = 0; (i <= DataCollectorLength - bl) && (!found); i++) {
            found = true;
            for (int j = 0; (j < bl) && (found); j++) {
                if (DataCollector[i + j] != b[j]) {
                    found = false;
                }
            }

            if (found) {
                return i;
            }
        }
    }
    return -1;

}

bool MPFD::Parser::FindStartingBoundaryAndTruncData() {
    long n = BoundaryPositionInDataCollector();
    if (n >= 0) {
        TruncateDataCollectorFromTheBeginning(n + Boundary.length());
        return true;
    } else {
        return false;
    }
}