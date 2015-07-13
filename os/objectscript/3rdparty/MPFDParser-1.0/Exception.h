// This file is distributed under GPLv3 licence
// Author: Gorelov Grigory (gorelov@grigory.info)
//
// Contacts and other info are on the WEB page:  grigory.info/MPFDParser


#ifndef _EXCEPTION_H
#define	_EXCEPTION_H

#include <string>
// #include <iostream>
#include <errno.h>


namespace MPFD {

    class Exception {
    public:
        Exception(std::string error);
        Exception(const Exception& orig);
        virtual ~Exception();
        
        std::string GetError();
     
    private:
        std::string Error;

    };
}

#endif	/* _EXCEPTION_H */

