//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#if defined(_WIN32) || defined(WIN32)
#include <windows.h>
#undef ERROR
#include <Rcpp.h>
// Adapated From http://banderlogi.blogspot.com/2011/06/enum-drive-letters-attached-for-usb.html
bool IsUsbDevice( std::string letter )
{
  wchar_t volumeAccessPath[] = L"\\\\.\\X:";
  volumeAccessPath[4] = (letter.c_str())[0];
 
  HANDLE deviceHandle = CreateFileW(volumeAccessPath,
                                    0,                // no access to the drive
                                    FILE_SHARE_READ | // share mode
                                    FILE_SHARE_WRITE,
                                    NULL,             // default security attributes
                                    OPEN_EXISTING,    // disposition
                                    0,                // file attributes
                                    NULL);            // do not copy file attributes
 
  // setup query
  STORAGE_PROPERTY_QUERY query;
  memset(&query, 0, sizeof(query));
  query.PropertyId = StorageDeviceProperty;
  query.QueryType = PropertyStandardQuery;
   
  // issue query
  DWORD bytes;
  STORAGE_DEVICE_DESCRIPTOR devd;
  STORAGE_BUS_TYPE busType = BusTypeUnknown;
 
  if (DeviceIoControl(deviceHandle,
                      IOCTL_STORAGE_QUERY_PROPERTY,
                      &query, sizeof(query),
                      &devd, sizeof(devd),
                      &bytes, NULL))
    {
      busType = devd.BusType;
    }
  else
    {
      CloseHandle(deviceHandle);
      // Rcpp::warning("Failed to define bus type for some drives.");
      return false;
    }
   
  CloseHandle(deviceHandle);
 
  return BusTypeUsb == busType;
}
#else
#include <Rcpp.h>
#endif

//[[Rcpp::export]]
bool removableDrive(std::string driveRoot)
{
#if defined(_WIN32) || defined(WIN32)
  UINT driveType = GetDriveType(driveRoot.c_str());
  bool ret = (DRIVE_REMOVABLE == driveType || DRIVE_CDROM == driveType || DRIVE_NO_ROOT_DIR == driveType || DRIVE_REMOTE == driveType || DRIVE_RAMDISK == driveType);
  if (driveType == DRIVE_FIXED){
    if (IsUsbDevice(driveRoot)){
      return true;
    } else {
      return false;
    }
  }
  return ret;
#else
  return false;
#endif
}
