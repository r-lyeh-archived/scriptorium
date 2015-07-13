// This file is distributed under GPLv3 licence
// Author: Gorelov Grigory (gorelov@grigory.info)
//
// Contacts and other info are on the WEB page:  grigory.info/MPFDParser


#include "Exception.h"

MPFD::Exception::Exception(std::string error) {
    Error = error;
}

MPFD::Exception::Exception(const MPFD::Exception& orig) {
    Error = orig.Error;
}

MPFD::Exception::~Exception() {

}

std::string MPFD::Exception::GetError() {
    return Error;
}