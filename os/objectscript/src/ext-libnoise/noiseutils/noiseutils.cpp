// noiseutils.cpp
//
// Copyright (C) 2003-2005 Jason Bevins
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.
//
// This library is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
// License (COPYING.txt) for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// The developer's email is jlbezigvins@gmzigail.com (for great email, take
// off every 'zig'.)
//

#include <fstream>

#include "../noise/interp.h"
#include "../noise/mathconsts.h"

#include "noiseutils.h"

using namespace noise;
using namespace noise::model;
using namespace noise::module;

// Bitmap header size.
const int BMP_HEADER_SIZE = 54;

// Direction of the light source, in compass degrees (0 = north, 90 = east,
// 180 = south, 270 = east)
const double DEFAULT_LIGHT_AZIMUTH = 45.0;

// Amount of contrast between light and dark areas.
const double DEFAULT_LIGHT_CONTRAST  =  1.0;

// Elevation of the light source above the horizon, in degrees (0 = on
// horizon, 90 = directly overhead)
const double DEFAULT_LIGHT_ELEVATION = 45.0;

//////////////////////////////////////////////////////////////////////////////
// Miscellaneous functions

namespace noise
{

  namespace utils
  {

    // Performs linear interpolation between two 8-bit channel values.
    inline noise::uint8 BlendChannel (const uint8 channel0,
      const uint8 channel1, float alpha)
    {
      float c0 = (float)channel0 / 255.0f;
      float c1 = (float)channel1 / 255.0f;
      return (noise::uint8)(((c1 * alpha) + (c0 * (1.0f - alpha))) * 255.0f);
    }

    // Performs linear interpolation between two colors and stores the result
    // in out.
    inline void LinearInterpColor (const Color& color0, const Color& color1,
      float alpha, Color& out)
    {
      out.alpha = BlendChannel (color0.alpha, color1.alpha, alpha);
      out.blue  = BlendChannel (color0.blue , color1.blue , alpha);
      out.green = BlendChannel (color0.green, color1.green, alpha);
      out.red   = BlendChannel (color0.red  , color1.red  , alpha);
    }

    // Unpacks a floating-point value into four bytes.  This function is
    // specific to Intel machines.  A portable version will come soon (I
    // hope.)
    inline noise::uint8* UnpackFloat (noise::uint8* bytes, float value)
    {
      noise::uint8* pBytes = (noise::uint8*)(&value);
      bytes[0] = *pBytes++;
      bytes[1] = *pBytes++;
      bytes[2] = *pBytes++;
      bytes[3] = *pBytes++;
      return bytes;
    }

    // Unpacks a 16-bit integer value into two bytes in little endian format.
    inline noise::uint8* UnpackLittle16 (noise::uint8* bytes,
      noise::uint16 integer)
    {
      bytes[0] = (noise::uint8)((integer & 0x00ff)      );
      bytes[1] = (noise::uint8)((integer & 0xff00) >> 8 );
      return bytes;
    }

    // Unpacks a 32-bit integer value into four bytes in little endian format.
    inline noise::uint8* UnpackLittle32 (noise::uint8* bytes,
      noise::uint32 integer)
    {
      bytes[0] = (noise::uint8)((integer & 0x000000ff)      );
      bytes[1] = (noise::uint8)((integer & 0x0000ff00) >> 8 );
      bytes[2] = (noise::uint8)((integer & 0x00ff0000) >> 16);
      bytes[3] = (noise::uint8)((integer & 0xff000000) >> 24);
      return bytes;
    }

  }

}

using namespace noise;

using namespace noise::utils;

//////////////////////////////////////////////////////////////////////////////
// GradientColor class

GradientColor::GradientColor ()
{
  m_pGradientPoints = NULL;
}

GradientColor::~GradientColor ()
{
  delete[] m_pGradientPoints;
}

void GradientColor::AddGradientPoint (double gradientPos,
  const Color& gradientColor)
{
  // Find the insertion point for the new gradient point and insert the new
  // gradient point at that insertion point.  The gradient point array will
  // remain sorted by gradient position.
  int insertionPos = FindInsertionPos (gradientPos);
  InsertAtPos (insertionPos, gradientPos, gradientColor);
}

void GradientColor::Clear ()
{
  delete[] m_pGradientPoints;
  m_pGradientPoints = NULL;
  m_gradientPointCount = 0;
}

int GradientColor::FindInsertionPos (double gradientPos)
{
  int insertionPos;
  for (insertionPos = 0; insertionPos < m_gradientPointCount;
    insertionPos++) {
    if (gradientPos < m_pGradientPoints[insertionPos].pos) {
      // We found the array index in which to insert the new gradient point.
      // Exit now.
      break;
    } else if (gradientPos == m_pGradientPoints[insertionPos].pos) {
      // Each gradient point is required to contain a unique gradient
      // position, so throw an exception.
      throw noise::ExceptionInvalidParam ();
    }
  }
  return insertionPos;
}

const Color& GradientColor::GetColor (double gradientPos) const
{
  assert (m_gradientPointCount >= 2);

  // Find the first element in the gradient point array that has a gradient
  // position larger than the gradient position passed to this method.
  int indexPos;
  for (indexPos = 0; indexPos < m_gradientPointCount; indexPos++) {
    if (gradientPos < m_pGradientPoints[indexPos].pos) {
      break;
    }
  }

  // Find the two nearest gradient points so that we can perform linear
  // interpolation on the color.
  int index0 = ClampValue (indexPos - 1, 0, m_gradientPointCount - 1);
  int index1 = ClampValue (indexPos    , 0, m_gradientPointCount - 1);

  // If some gradient points are missing (which occurs if the gradient
  // position passed to this method is greater than the largest gradient
  // position or less than the smallest gradient position in the array), get
  // the corresponding gradient color of the nearest gradient point and exit
  // now.
  if (index0 == index1) {
    m_workingColor = m_pGradientPoints[index1].color;
    return m_workingColor;
  }
  
  // Compute the alpha value used for linear interpolation.
  double input0 = m_pGradientPoints[index0].pos;
  double input1 = m_pGradientPoints[index1].pos;
  double alpha = (gradientPos - input0) / (input1 - input0);

  // Now perform the linear interpolation given the alpha value.
  const Color& color0 = m_pGradientPoints[index0].color;
  const Color& color1 = m_pGradientPoints[index1].color;
  LinearInterpColor (color0, color1, (float)alpha, m_workingColor);
  return m_workingColor;
}

void GradientColor::InsertAtPos (int insertionPos, double gradientPos,
  const Color& gradientColor)
{
  // Make room for the new gradient point at the specified insertion position
  // within the gradient point array.  The insertion position is determined by
  // the gradient point's position; the gradient points must be sorted by
  // gradient position within that array.
  GradientPoint* newGradientPoints;
  newGradientPoints = new GradientPoint[m_gradientPointCount + 1];
  for (int i = 0; i < m_gradientPointCount; i++) {
    if (i < insertionPos) {
      newGradientPoints[i] = m_pGradientPoints[i];
    } else {
      newGradientPoints[i + 1] = m_pGradientPoints[i];
    }
  }
  delete[] m_pGradientPoints;
  m_pGradientPoints = newGradientPoints;
  ++m_gradientPointCount;

  // Now that we've made room for the new gradient point within the array, add
  // the new gradient point.
  m_pGradientPoints[insertionPos].pos = gradientPos ;
  m_pGradientPoints[insertionPos].color = gradientColor;
}

//////////////////////////////////////////////////////////////////////////////
// NoiseMap class

NoiseMap::NoiseMap ()
{
  InitObj ();
}

NoiseMap::NoiseMap (int width, int height)
{
  InitObj ();
  SetSize (width, height);
}

NoiseMap::NoiseMap (const NoiseMap& rhs)
{
  InitObj ();
  CopyNoiseMap (rhs);
}

NoiseMap::~NoiseMap ()
{
  delete[] m_pNoiseMap;
}

NoiseMap& NoiseMap::operator= (const NoiseMap& rhs)
{
  CopyNoiseMap (rhs);

  return *this;
}

void NoiseMap::Clear (float value)
{
  if (m_pNoiseMap != NULL) {
    for (int y = 0; y < m_height; y++) {
      float* pDest = GetSlabPtr (0, y);
      for (int x = 0; x < m_width; x++) {
        *pDest++ = value;
      }
    }
  }
}

void NoiseMap::CopyNoiseMap (const NoiseMap& source)
{
  // Resize the noise map buffer, then copy the slabs from the source noise
  // map buffer to this noise map buffer.
  SetSize (source.GetWidth (), source.GetHeight ());
  for (int y = 0; y < source.GetHeight (); y++) {
    const float* pSource = source.GetConstSlabPtr (0, y);
    float* pDest = GetSlabPtr (0, y);
    memcpy (pDest, pSource, (size_t)source.GetWidth () * sizeof (float));
  }

  // Copy the border value as well.
  m_borderValue = source.m_borderValue;
}

void NoiseMap::DeleteNoiseMapAndReset ()
{
  delete[] m_pNoiseMap;
  InitObj ();
}

float NoiseMap::GetValue (int x, int y) const
{
  if (m_pNoiseMap != NULL) {
    if (x >= 0 && x < m_width && y >= 0 && y < m_height) {
      return *(GetConstSlabPtr (x, y));
    }
  }
  // The coordinates specified are outside the noise map.  Return the border
  // value.
  return m_borderValue;
}

void NoiseMap::InitObj ()
{
  m_pNoiseMap = NULL;
  m_height    = 0;
  m_width     = 0;
  m_stride    = 0;
  m_memUsed   = 0;
  m_borderValue = 0.0;
}

void NoiseMap::ReclaimMem ()
{
  size_t newMemUsage = CalcMinMemUsage (m_width, m_height);
  if (m_memUsed > newMemUsage) {
    // There is wasted memory.  Create the smallest buffer that can fit the
    // data and copy the data to it.
    float* pNewNoiseMap = NULL;
    try {
      pNewNoiseMap = new float[newMemUsage];
    }
    catch (...) {
      throw noise::ExceptionOutOfMemory ();
    }
    memcpy (pNewNoiseMap, m_pNoiseMap, newMemUsage * sizeof (float));
    delete[] m_pNoiseMap;
    m_pNoiseMap = pNewNoiseMap;
    m_memUsed = newMemUsage;
  }
}

void NoiseMap::SetSize (int width, int height)
{
  if (width < 0 || height < 0
    || width > RASTER_MAX_WIDTH || height > RASTER_MAX_HEIGHT) {
    // Invalid width or height.
    throw noise::ExceptionInvalidParam ();
  } else if (width == 0 || height == 0) {
    // An empty noise map was specified.  Delete it and zero out the size
    // member variables.
    DeleteNoiseMapAndReset ();
  } else {
    // A new noise map size was specified.  Allocate a new noise map buffer
    // unless the current buffer is large enough for the new noise map (we
    // don't want costly reallocations going on.)
    size_t newMemUsage = CalcMinMemUsage (width, height);
    if (m_memUsed < newMemUsage) {
      // The new size is too big for the current noise map buffer.  We need to
      // reallocate.
      DeleteNoiseMapAndReset ();
      try {
        m_pNoiseMap = new float[newMemUsage];
      }
      catch (...) {
        throw noise::ExceptionOutOfMemory ();
      }
      m_memUsed = newMemUsage;
    }
    m_stride = (int)CalcStride (width);
    m_width  = width;
    m_height = height;
  }
}

void NoiseMap::SetValue (int x, int y, float value)
{
  if (m_pNoiseMap != NULL) {
    if (x >= 0 && x < m_width && y >= 0 && y < m_height) {
      *(GetSlabPtr (x, y)) = value;
    }
  }
}

void NoiseMap::TakeOwnership (NoiseMap& source)
{
  // Copy the values and the noise map buffer from the source noise map to
  // this noise map.  Now this noise map pwnz the source buffer.
  delete[] m_pNoiseMap;
  m_memUsed   = source.m_memUsed;
  m_height    = source.m_height;
  m_pNoiseMap = source.m_pNoiseMap;
  m_stride    = source.m_stride;
  m_width     = source.m_width;

  // Now that the source buffer is assigned to this noise map, reset the
  // source noise map object.
  source.InitObj ();
}

//////////////////////////////////////////////////////////////////////////////
// Image class

Image::Image ()
{
  InitObj ();
}

Image::Image (int width, int height)
{
  InitObj ();
  SetSize (width, height);
}

Image::Image (const Image& rhs)
{
  InitObj ();
  CopyImage (rhs);
}

Image::~Image ()
{
  delete[] m_pImage;
}

Image& Image::operator= (const Image& rhs)
{
  CopyImage (rhs);

  return *this;
}

void Image::Clear (const Color& value)
{
  if (m_pImage != NULL) {
    for (int y = 0; y < m_height; y++) {
      Color* pDest = GetSlabPtr (0, y);
      for (int x = 0; x < m_width; x++) {
        *pDest++ = value;
      }
    }
  }
}

void Image::CopyImage (const Image& source)
{
  // Resize the image buffer, then copy the slabs from the source image
  // buffer to this image buffer.
  SetSize (source.GetWidth (), source.GetHeight ());
  for (int y = 0; y < source.GetHeight (); y++) {
    const Color* pSource = source.GetConstSlabPtr (0, y);
    Color* pDest = GetSlabPtr (0, y);
    memcpy (pDest, pSource, (size_t)source.GetWidth () * sizeof (float));
  }

  // Copy the border value as well.
  m_borderValue = source.m_borderValue;
}

void Image::DeleteImageAndReset ()
{
  delete[] m_pImage;
  InitObj ();
}

Color Image::GetValue (int x, int y) const
{
  if (m_pImage != NULL) {
    if (x >= 0 && x < m_width && y >= 0 && y < m_height) {
      return *(GetConstSlabPtr (x, y));
    }
  }
  // The coordinates specified are outside the image.  Return the border
  // value.
  return m_borderValue;
}

void Image::InitObj ()
{
  m_pImage  = NULL;
  m_height  = 0;
  m_width   = 0;
  m_stride  = 0;
  m_memUsed = 0;
  m_borderValue = Color (0, 0, 0, 0);
}

void Image::ReclaimMem ()
{
  size_t newMemUsage = CalcMinMemUsage (m_width, m_height);
  if (m_memUsed > newMemUsage) {
    // There is wasted memory.  Create the smallest buffer that can fit the
    // data and copy the data to it.
    Color* pNewImage = NULL;
    try {
      pNewImage = new Color[newMemUsage];
    }
    catch (...) {
      throw noise::ExceptionOutOfMemory ();
    }
    memcpy (pNewImage, m_pImage, newMemUsage * sizeof (float));
    delete[] m_pImage;
    m_pImage = pNewImage;
    m_memUsed = newMemUsage;
  }
}

void Image::SetSize (int width, int height)
{
  if (width < 0 || height < 0
    || width > RASTER_MAX_WIDTH || height > RASTER_MAX_HEIGHT) {
    // Invalid width or height.
    throw noise::ExceptionInvalidParam ();
  } else if (width == 0 || height == 0) {
    // An empty image was specified.  Delete it and zero out the size member
    // variables.
    DeleteImageAndReset ();
  } else {
    // A new image size was specified.  Allocate a new image buffer unless
    // the current buffer is large enough for the new image (we don't want
    // costly reallocations going on.)
    size_t newMemUsage = CalcMinMemUsage (width, height);
    if (m_memUsed < newMemUsage) {
      // The new size is too big for the current image buffer.  We need to
      // reallocate.
      DeleteImageAndReset ();
      try {
        m_pImage = new Color[newMemUsage];
      }
      catch (...) {
        throw noise::ExceptionOutOfMemory ();
      }
      m_memUsed = newMemUsage;
    }
    m_stride = (int)CalcStride (width);
    m_width  = width;
    m_height = height;
  }
}

void Image::SetValue (int x, int y, const Color& value)
{
  if (m_pImage != NULL) {
    if (x >= 0 && x < m_width && y >= 0 && y < m_height) {
      *(GetSlabPtr (x, y)) = value;
    }
  }
}

void Image::TakeOwnership (Image& source)
{
  // Copy the values and the image buffer from the source image to this image.
  // Now this image pwnz the source buffer.
  delete[] m_pImage;
  m_memUsed = source.m_memUsed;
  m_height  = source.m_height;
  m_pImage  = source.m_pImage;
  m_stride  = source.m_stride;
  m_width   = source.m_width;

  // Now that the source buffer is assigned to this image, reset the source
  // image object.
  source.InitObj ();
}

/////////////////////////////////////////////////////////////////////////////
// WriterBMP class

int WriterBMP::CalcWidthByteCount (int width) const
{
  return ((width * 3) + 3) & ~0x03;
}

void WriterBMP::WriteDestFile ()
{
  if (m_pSourceImage == NULL) {
    throw noise::ExceptionInvalidParam ();
  }

  int width  = m_pSourceImage->GetWidth  ();
  int height = m_pSourceImage->GetHeight ();

  // The width of one line in the file must be aligned on a 4-byte boundary.
  int bufferSize = CalcWidthByteCount (width);
  int destSize   = bufferSize * height;

  // This buffer holds one horizontal line in the destination file.
  noise::uint8* pLineBuffer = NULL;

  // File object used to write the file.
  std::ofstream os;
  os.clear ();
  
  // Allocate a buffer to hold one horizontal line in the bitmap.
  try {
    pLineBuffer = new noise::uint8[bufferSize];
  }
  catch (...) {
    throw noise::ExceptionOutOfMemory ();
  }

  // Open the destination file.
  os.open (m_destFilename.c_str (), std::ios::out | std::ios::binary);
  if (os.fail () || os.bad ()) {
    delete[] pLineBuffer;
    throw noise::ExceptionUnknown ();
  }

  // Build the header.
  noise::uint8 d[4];
  os.write ("BM", 2);
  os.write ((char*)UnpackLittle32 (d, destSize + BMP_HEADER_SIZE), 4);
  os.write ("\0\0\0\0", 4);
  os.write ((char*)UnpackLittle32 (d, (noise::uint32)BMP_HEADER_SIZE), 4);
  os.write ((char*)UnpackLittle32 (d, 40), 4);   // Palette offset
  os.write ((char*)UnpackLittle32 (d, (noise::uint32)width ), 4);
  os.write ((char*)UnpackLittle32 (d, (noise::uint32)height), 4);
  os.write ((char*)UnpackLittle16 (d, 1 ), 2);   // Planes per pixel
  os.write ((char*)UnpackLittle16 (d, 24), 2);   // Bits per plane
  os.write ("\0\0\0\0", 4); // Compression (0 = none)
  os.write ((char*)UnpackLittle32 (d, (noise::uint32)destSize), 4);
  os.write ((char*)UnpackLittle32 (d, 2834), 4); // X pixels per meter
  os.write ((char*)UnpackLittle32 (d, 2834), 4); // Y pixels per meter
  os.write ("\0\0\0\0", 4);
  os.write ("\0\0\0\0", 4);
  if (os.fail () || os.bad ()) {
    os.clear ();
    os.close ();
    os.clear ();
    delete[] pLineBuffer;
    throw noise::ExceptionUnknown ();
  }

  // Build and write each horizontal line to the file.
  for (int y = 0; y < height; y++) {
    memset (pLineBuffer, 0, bufferSize);
    Color* pSource = m_pSourceImage->GetSlabPtr (y);
    noise::uint8* pDest   = pLineBuffer;
    for (int x = 0; x < width; x++) {
      *pDest++ = pSource->blue ;
      *pDest++ = pSource->green;
      *pDest++ = pSource->red  ;
      ++pSource;
    }
    os.write ((char*)pLineBuffer, (size_t)bufferSize);
    if (os.fail () || os.bad ()) {
      os.clear ();
      os.close ();
      os.clear ();
      delete[] pLineBuffer;
      throw noise::ExceptionUnknown ();
    }
  }

  os.close ();
  os.clear ();
  delete[] pLineBuffer;
}

/////////////////////////////////////////////////////////////////////////////
// WriterTER class

int WriterTER::CalcWidthByteCount (int width) const
{
  return (width * sizeof (int16));
}

void WriterTER::WriteDestFile ()
{
  if (m_pSourceNoiseMap == NULL) {
    throw noise::ExceptionInvalidParam ();
  }

  int width  = m_pSourceNoiseMap->GetWidth  ();
  int height = m_pSourceNoiseMap->GetHeight ();

  int bufferSize = CalcWidthByteCount (width);
  int destSize   = bufferSize * height;

  // This buffer holds one horizontal line in the destination file.
  noise::uint8* pLineBuffer = NULL;

  // File object used to write the file.
  std::ofstream os;
  os.clear ();

  // Allocate a buffer to hold one horizontal line in the height map.
  try {
    pLineBuffer = new noise::uint8[bufferSize];
  }
  catch (...) {
    throw noise::ExceptionOutOfMemory ();
  }

  // Open the destination file.
  os.open (m_destFilename.c_str (), std::ios::out | std::ios::binary);
  if (os.fail () || os.bad ()) {
    os.clear ();
    delete[] pLineBuffer;
    throw noise::ExceptionUnknown ();
  }

  // Build the header.
  noise::uint8 d[4];
  int16 heightScale = (int16)(floor (32768.0 / (double)m_metersPerPoint));
  os.write ("TERRAGENTERRAIN ", 16);
  os.write ("SIZE", 4);
  os.write ((char*)UnpackLittle16 (d, GetMin (width, height) - 1), 2);
  os.write ("\0\0", 2);
  os.write ("XPTS", 4);
  os.write ((char*)UnpackLittle16 (d, width), 2);
  os.write ("\0\0", 2);
  os.write ("YPTS", 4);
  os.write ((char*)UnpackLittle16 (d, height), 2);
  os.write ("\0\0", 2);
  os.write ("SCAL", 4);
  os.write ((char*)UnpackFloat (d, m_metersPerPoint), 4);
  os.write ((char*)UnpackFloat (d, m_metersPerPoint), 4);
  os.write ((char*)UnpackFloat (d, m_metersPerPoint), 4);
  os.write ("ALTW", 4);
  os.write ((char*)UnpackLittle16 (d, heightScale), 2);
  os.write ("\0\0", 2);
  if (os.fail () || os.bad ()) {
    os.clear ();
    os.close ();
    os.clear ();
    delete[] pLineBuffer;
    throw noise::ExceptionUnknown ();
  }

  // Build and write each horizontal line to the file.
  for (int y = 0; y < height; y++) {
    float* pSource = m_pSourceNoiseMap->GetSlabPtr (y);
    noise::uint8* pDest   = pLineBuffer;
    for (int x = 0; x < width; x++) {
      int16 scaledHeight = (int16)(floor (*pSource * 2.0));
      UnpackLittle16 (pDest, scaledHeight);
      pDest += 2;
      ++pSource;
    }
    os.write ((char*)pLineBuffer, (size_t)bufferSize);
    if (os.fail () || os.bad ()) {
      os.clear ();
      os.close ();
      os.clear ();
      delete[] pLineBuffer;
      throw noise::ExceptionUnknown ();
    }
  }

  os.close ();
  os.clear ();
  delete[] pLineBuffer;
}

/////////////////////////////////////////////////////////////////////////////
// NoiseMapBuilder class

NoiseMapBuilder::NoiseMapBuilder ():
  m_pCallback (NULL),
  m_userParam (NULL),
  m_destHeight (0),
  m_destWidth  (0),
  m_pDestNoiseMap (NULL),
  m_pSourceModule (NULL)
{
}

NoiseMapBuilder::~NoiseMapBuilder()
{
}

void NoiseMapBuilder::SetCallback (NoiseMapCallback pCallback, void * userParam)
{
  m_pCallback = pCallback;
  m_userParam = userParam;
}

NoiseMapCallback NoiseMapBuilder::GetCallback()
{
	return m_pCallback;
}

void * NoiseMapBuilder::GetCallbackParam()
{
	return m_userParam;
}

/////////////////////////////////////////////////////////////////////////////
// NoiseMapBuilderCylinder class

NoiseMapBuilderCylinder::NoiseMapBuilderCylinder ():
  m_lowerAngleBound  (0.0),
  m_lowerHeightBound (0.0),
  m_upperAngleBound  (0.0),
  m_upperHeightBound (0.0)
{
}

void NoiseMapBuilderCylinder::Build ()
{
  if ( m_upperAngleBound <= m_lowerAngleBound
    || m_upperHeightBound <= m_lowerHeightBound
    || m_destWidth <= 0
    || m_destHeight <= 0
    || m_pSourceModule == NULL
    || m_pDestNoiseMap == NULL) {
    throw noise::ExceptionInvalidParam ();
  }

  // Resize the destination noise map so that it can store the new output
  // values from the source model.
  m_pDestNoiseMap->SetSize (m_destWidth, m_destHeight);

  // Create the cylinder model.
  model::Cylinder cylinderModel;
  cylinderModel.SetModule (*m_pSourceModule);

  double angleExtent  = m_upperAngleBound  - m_lowerAngleBound ;
  double heightExtent = m_upperHeightBound - m_lowerHeightBound;
  double xDelta = angleExtent  / (double)m_destWidth ;
  double yDelta = heightExtent / (double)m_destHeight;
  double curAngle  = m_lowerAngleBound ;
  double curHeight = m_lowerHeightBound;

  // Fill every point in the noise map with the output values from the model.
  for (int y = 0; y < m_destHeight; y++) {
    float* pDest = m_pDestNoiseMap->GetSlabPtr (y);
    curAngle = m_lowerAngleBound;
    for (int x = 0; x < m_destWidth; x++) {
      float curValue = (float)cylinderModel.GetValue (curAngle, curHeight);
      *pDest++ = curValue;
      curAngle += xDelta;
    }
    curHeight += yDelta;
    if (m_pCallback != NULL) {
      m_pCallback (y, this, m_userParam);
    }
  }
}

/////////////////////////////////////////////////////////////////////////////
// NoiseMapBuilderPlane class

NoiseMapBuilderPlane::NoiseMapBuilderPlane ():
  m_isSeamlessEnabled (false),
  m_lowerXBound  (0.0),
  m_lowerZBound  (0.0),
  m_upperXBound  (0.0),
  m_upperZBound  (0.0)
{
}

void NoiseMapBuilderPlane::Build ()
{
  if ( m_upperXBound <= m_lowerXBound
    || m_upperZBound <= m_lowerZBound
    || m_destWidth <= 0
    || m_destHeight <= 0
    || m_pSourceModule == NULL
    || m_pDestNoiseMap == NULL) {
    throw noise::ExceptionInvalidParam ();
  }

  // Resize the destination noise map so that it can store the new output
  // values from the source model.
  m_pDestNoiseMap->SetSize (m_destWidth, m_destHeight);

  // Create the plane model.
  model::Plane planeModel;
  planeModel.SetModule (*m_pSourceModule);

  double xExtent = m_upperXBound - m_lowerXBound;
  double zExtent = m_upperZBound - m_lowerZBound;
  double xDelta  = xExtent / (double)m_destWidth ;
  double zDelta  = zExtent / (double)m_destHeight;
  double xCur    = m_lowerXBound;
  double zCur    = m_lowerZBound;

  // Fill every point in the noise map with the output values from the model.
  for (int z = 0; z < m_destHeight; z++) {
    float* pDest = m_pDestNoiseMap->GetSlabPtr (z);
    xCur = m_lowerXBound;
    for (int x = 0; x < m_destWidth; x++) {
      float finalValue;
      if (!m_isSeamlessEnabled) {
        finalValue = planeModel.GetValue (xCur, zCur);
      } else {
        double swValue, seValue, nwValue, neValue;
        swValue = planeModel.GetValue (xCur          , zCur          );
        seValue = planeModel.GetValue (xCur + xExtent, zCur          );
        nwValue = planeModel.GetValue (xCur          , zCur + zExtent);
        neValue = planeModel.GetValue (xCur + xExtent, zCur + zExtent);
        double xBlend = 1.0 - ((xCur - m_lowerXBound) / xExtent);
        double zBlend = 1.0 - ((zCur - m_lowerZBound) / zExtent);
        double z0 = LinearInterp (swValue, seValue, xBlend);
        double z1 = LinearInterp (nwValue, neValue, xBlend);
        finalValue = (float)LinearInterp (z0, z1, zBlend);
      }
      *pDest++ = finalValue;
      xCur += xDelta;
    }
    zCur += zDelta;
    if (m_pCallback != NULL) {
      m_pCallback (z, this, m_userParam);
    }
  }
}

/////////////////////////////////////////////////////////////////////////////
// NoiseMapBuilderSphere class

NoiseMapBuilderSphere::NoiseMapBuilderSphere ():
  m_eastLonBound  (0.0),
  m_northLatBound (0.0),
  m_southLatBound (0.0),
  m_westLonBound  (0.0)
{
}

void NoiseMapBuilderSphere::Build ()
{
  if ( m_eastLonBound <= m_westLonBound
    || m_northLatBound <= m_southLatBound
    || m_destWidth <= 0
    || m_destHeight <= 0
    || m_pSourceModule == NULL
    || m_pDestNoiseMap == NULL) {
    throw noise::ExceptionInvalidParam ();
  }

  // Resize the destination noise map so that it can store the new output
  // values from the source model.
  m_pDestNoiseMap->SetSize (m_destWidth, m_destHeight);

  // Create the plane model.
  model::Sphere sphereModel;
  sphereModel.SetModule (*m_pSourceModule);

  double lonExtent = m_eastLonBound  - m_westLonBound ;
  double latExtent = m_northLatBound - m_southLatBound;
  double xDelta = lonExtent / (double)m_destWidth ;
  double yDelta = latExtent / (double)m_destHeight;
  double curLon = m_westLonBound ;
  double curLat = m_southLatBound;

  // Fill every point in the noise map with the output values from the model.
  for (int y = 0; y < m_destHeight; y++) {
    float* pDest = m_pDestNoiseMap->GetSlabPtr (y);
    curLon = m_westLonBound;
    for (int x = 0; x < m_destWidth; x++) {
      float curValue = (float)sphereModel.GetValue (curLat, curLon);
      *pDest++ = curValue;
      curLon += xDelta;
    }
    curLat += yDelta;
    if (m_pCallback != NULL) {
      m_pCallback (y, this, m_userParam);
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// RendererImage class

RendererImage::RendererImage ():
  m_isLightEnabled    (false),
  m_isWrapEnabled     (false),
  m_lightAzimuth      (45.0),
  m_lightBrightness   (1.0),
  m_lightColor        (255, 255, 255, 255),
  m_lightContrast     (1.0),
  m_lightElev         (45.0),
  m_lightIntensity    (1.0),
  m_pBackgroundImage  (NULL),
  m_pDestImage        (NULL),
  m_pSourceNoiseMap   (NULL),
  m_recalcLightValues (true)
{
  BuildGrayscaleGradient ();
};

void RendererImage::AddGradientPoint (double gradientPos,
  const Color& gradientColor)
{
  m_gradient.AddGradientPoint (gradientPos, gradientColor);
}

void RendererImage::BuildGrayscaleGradient ()
{
  ClearGradient ();
  m_gradient.AddGradientPoint (-1.0, Color (  0,   0,   0, 255));
  m_gradient.AddGradientPoint ( 1.0, Color (255, 255, 255, 255));
}

void RendererImage::BuildTerrainGradient ()
{
  ClearGradient ();
  m_gradient.AddGradientPoint (-1.00, Color (  0,   0, 128, 255));
  m_gradient.AddGradientPoint (-0.20, Color ( 32,  64, 128, 255));
  m_gradient.AddGradientPoint (-0.04, Color ( 64,  96, 192, 255));
  m_gradient.AddGradientPoint (-0.02, Color (192, 192, 128, 255));
  m_gradient.AddGradientPoint ( 0.00, Color (  0, 192,   0, 255));
  m_gradient.AddGradientPoint ( 0.25, Color (192, 192,   0, 255));
  m_gradient.AddGradientPoint ( 0.50, Color (160,  96,  64, 255));
  m_gradient.AddGradientPoint ( 0.75, Color (128, 255, 255, 255));
  m_gradient.AddGradientPoint ( 1.00, Color (255, 255, 255, 255));
}

Color RendererImage::CalcDestColor (const Color& sourceColor,
  const Color& backgroundColor, double lightValue) const
{
  double sourceRed   = (double)sourceColor.red   / 255.0;
  double sourceGreen = (double)sourceColor.green / 255.0;
  double sourceBlue  = (double)sourceColor.blue  / 255.0;
  double sourceAlpha = (double)sourceColor.alpha / 255.0;
  double backgroundRed   = (double)backgroundColor.red   / 255.0;
  double backgroundGreen = (double)backgroundColor.green / 255.0;
  double backgroundBlue  = (double)backgroundColor.blue  / 255.0;

  // First, blend the source color to the background color using the alpha
  // of the source color.
  double red   = LinearInterp (backgroundRed,   sourceRed  , sourceAlpha);
  double green = LinearInterp (backgroundGreen, sourceGreen, sourceAlpha);
  double blue  = LinearInterp (backgroundBlue,  sourceBlue , sourceAlpha);

  if (m_isLightEnabled) {

    // Now calculate the light color.
    double lightRed   = lightValue * (double)m_lightColor.red   / 255.0;
    double lightGreen = lightValue * (double)m_lightColor.green / 255.0;
    double lightBlue  = lightValue * (double)m_lightColor.blue  / 255.0;

    // Apply the light color to the new color.
    red   *= lightRed  ;
    green *= lightGreen;
    blue  *= lightBlue ;
  }

  // Clamp the color channels to the (0..1) range.
  red   = (red   < 0.0)? 0.0: red  ;
  red   = (red   > 1.0)? 1.0: red  ;
  green = (green < 0.0)? 0.0: green;
  green = (green > 1.0)? 1.0: green;
  blue  = (blue  < 0.0)? 0.0: blue ;
  blue  = (blue  > 1.0)? 1.0: blue ;

  // Rescale the color channels to the noise::uint8 (0..255) range and return
  // the new color.
  Color newColor (
    (noise::uint8)((noise::uint)(red   * 255.0) & 0xff),
    (noise::uint8)((noise::uint)(green * 255.0) & 0xff),
    (noise::uint8)((noise::uint)(blue  * 255.0) & 0xff),
    GetMax (sourceColor.alpha, backgroundColor.alpha));
  return newColor;
}

double RendererImage::CalcLightIntensity (double center, double left,
  double right, double down, double up) const
{
  // Recalculate the sine and cosine of the various light values if
  // necessary so it does not have to be calculated each time this method is
  // called.
  if (m_recalcLightValues) {
    m_cosAzimuth = cos (m_lightAzimuth * DEG_TO_RAD);
    m_sinAzimuth = sin (m_lightAzimuth * DEG_TO_RAD);
    m_cosElev    = cos (m_lightElev    * DEG_TO_RAD);
    m_sinElev    = sin (m_lightElev    * DEG_TO_RAD);
    m_recalcLightValues = false;
  }

  // Now do the lighting calculations.
  const double I_MAX = 1.0;
  double io = I_MAX * SQRT_2 * m_sinElev / 2.0;
  double ix = (I_MAX - io) * m_lightContrast * SQRT_2 * m_cosElev
    * m_cosAzimuth;
  double iy = (I_MAX - io) * m_lightContrast * SQRT_2 * m_cosElev
    * m_sinAzimuth; 
  double intensity = (ix * (left - right) + iy * (down - up) + io);
  if (intensity < 0.0) {
    intensity = 0.0;
  }
  return intensity;
}

void RendererImage::ClearGradient ()
{
  m_gradient.Clear ();
}

void RendererImage::Render ()
{
  if ( m_pSourceNoiseMap == NULL
    || m_pDestImage == NULL
    || m_pSourceNoiseMap->GetWidth  () <= 0
    || m_pSourceNoiseMap->GetHeight () <= 0
    || m_gradient.GetGradientPointCount () < 2) {
    throw noise::ExceptionInvalidParam ();
  }

  int width  = m_pSourceNoiseMap->GetWidth  ();
  int height = m_pSourceNoiseMap->GetHeight ();

  // If a background image was provided, make sure it is the same size the
  // source noise map.
  if (m_pBackgroundImage != NULL) {
    if ( m_pBackgroundImage->GetWidth  () != width
      || m_pBackgroundImage->GetHeight () != height) {
      throw noise::ExceptionInvalidParam ();
    }
  }

  // Create the destination image.  It is safe to reuse it if this is also the
  // background image.
  if (m_pDestImage != m_pBackgroundImage) {
    m_pDestImage->SetSize (width, height);
  }

  for (int y = 0; y < height; y++) {
    const Color* pBackground = NULL;
    if (m_pBackgroundImage != NULL) {
      pBackground = m_pBackgroundImage->GetConstSlabPtr (y);
    }
    const float* pSource = m_pSourceNoiseMap->GetConstSlabPtr (y);
    Color* pDest = m_pDestImage->GetSlabPtr (y);
    for (int x = 0; x < width; x++) {

      // Get the color based on the value at the current point in the noise
      // map.
      Color destColor = m_gradient.GetColor (*pSource);

      // If lighting is enabled, calculate the light intensity based on the
      // rate of change at the current point in the noise map.
      double lightIntensity;
      if (m_isLightEnabled) {

        // Calculate the positions of the current point's four-neighbors.
        int xLeftOffset, xRightOffset;
        int yUpOffset  , yDownOffset ;
        if (m_isWrapEnabled) {
          if (x == 0) {
            xLeftOffset  = (int)width - 1;
            xRightOffset = 1;
          } else if (x == (int)width - 1) {
            xLeftOffset  = -1;
            xRightOffset = -((int)width - 1);
          } else {
            xLeftOffset  = -1;
            xRightOffset = 1;
          }
          if (y == 0) {
            yDownOffset = (int)height - 1;
            yUpOffset   = 1;
          } else if (y == (int)height - 1) {
            yDownOffset = -1;
            yUpOffset   = -((int)height - 1);
          } else {
            yDownOffset = -1;
            yUpOffset   = 1;
          }
        } else {
          if (x == 0) {
            xLeftOffset  = 0;
            xRightOffset = 1;
          } else if (x == (int)width - 1) {
            xLeftOffset  = -1;
            xRightOffset = 0;
          } else {
            xLeftOffset  = -1;
            xRightOffset = 1;
          }
          if (y == 0) {
            yDownOffset = 0;
            yUpOffset   = 1;
          } else if (y == (int)height - 1) {
            yDownOffset = -1;
            yUpOffset   = 0;
          } else {
            yDownOffset = -1;
            yUpOffset   = 1;
          }
        }
        yDownOffset *= m_pSourceNoiseMap->GetStride ();
        yUpOffset   *= m_pSourceNoiseMap->GetStride ();

        // Get the noise value of the current point in the source noise map
        // and the noise values of its four-neighbors.
        double nc = (double)(*pSource);
        double nl = (double)(*(pSource + xLeftOffset ));
        double nr = (double)(*(pSource + xRightOffset));
        double nd = (double)(*(pSource + yDownOffset ));
        double nu = (double)(*(pSource + yUpOffset   ));

        // Now we can calculate the lighting intensity.
        lightIntensity = CalcLightIntensity (nc, nl, nr, nd, nu);
        lightIntensity *= m_lightBrightness;

      } else {

        // These values will apply no lighting to the destination image.
        lightIntensity = 1.0;
      }

      // Get the current background color from the background image.
      Color backgroundColor (255, 255, 255, 255);
      if (m_pBackgroundImage != NULL) {
        backgroundColor = *pBackground;
      }

      // Blend the destination color, background color, and the light
      // intensity together, then update the destination image with that
      // color.
      *pDest = CalcDestColor (destColor, backgroundColor, lightIntensity);

      // Go to the next point.
      ++pSource;
      ++pDest;
      if (m_pBackgroundImage != NULL) {
        ++pBackground;
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// RendererNormalMap class

RendererNormalMap::RendererNormalMap ():
  m_bumpHeight      (1.0),
  m_isWrapEnabled   (false),
  m_pDestImage      (NULL),
  m_pSourceNoiseMap (NULL)
{
};

Color RendererNormalMap::CalcNormalColor (double nc, double nr, double nu,
  double bumpHeight) const
{
  // Calculate the surface normal.
  nc *= bumpHeight;
  nr *= bumpHeight;
  nu *= bumpHeight;
  double ncr = (nc - nr);
  double ncu = (nc - nu);
  double d = sqrt ((ncu * ncu) + (ncr * ncr) + 1);
  double vxc = (nc - nr) / d;
  double vyc = (nc - nu) / d;
  double vzc = 1.0 / d;

  // Map the normal range from the (-1.0 .. +1.0) range to the (0 .. 255)
  // range.
  noise::uint8 xc, yc, zc;
  xc = (noise::uint8)((noise::uint)((floor)((vxc + 1.0) * 127.5)) & 0xff);
  yc = (noise::uint8)((noise::uint)((floor)((vyc + 1.0) * 127.5)) & 0xff);
  zc = (noise::uint8)((noise::uint)((floor)((vzc + 1.0) * 127.5)) & 0xff);

  return Color (xc, yc, zc, 0);
}

void RendererNormalMap::Render ()
{
  if ( m_pSourceNoiseMap == NULL
    || m_pDestImage == NULL
    || m_pSourceNoiseMap->GetWidth  () <= 0
    || m_pSourceNoiseMap->GetHeight () <= 0) {
    throw noise::ExceptionInvalidParam ();
  }

  int width  = m_pSourceNoiseMap->GetWidth  ();
  int height = m_pSourceNoiseMap->GetHeight ();

  // Create the destination image.
  m_pDestImage->SetSize (width, height);

  for (int y = 0; y < height; y++) {
    const float* pSource = m_pSourceNoiseMap->GetConstSlabPtr (y);
    Color* pDest = m_pDestImage->GetSlabPtr (y);
    for (int x = 0; x < width; x++) {

      // Calculate the positions of the current point's right and up
      // neighbors.
      int xRightOffset, yUpOffset;
      if (m_isWrapEnabled) {
        if (x == (int)width - 1) {
          xRightOffset = -((int)width - 1);
        } else {
          xRightOffset = 1;
        }
        if (y == (int)height - 1) {
          yUpOffset = -((int)height - 1);
        } else {
          yUpOffset = 1;
        }
      } else {
        if (x == (int)width - 1) {
          xRightOffset = 0;
        } else {
          xRightOffset = 1;
        }
        if (y == (int)height - 1) {
          yUpOffset = 0;
        } else {
          yUpOffset = 1;
        }
      }
      yUpOffset *= m_pSourceNoiseMap->GetStride ();

      // Get the noise value of the current point in the source noise map
      // and the noise values of its right and up neighbors.
      double nc = (double)(*pSource);
      double nr = (double)(*(pSource + xRightOffset));
      double nu = (double)(*(pSource + yUpOffset   ));

      // Calculate the normal product.
      *pDest = CalcNormalColor (nc, nr, nu, m_bumpHeight);

      // Go to the next point.
      ++pSource;
      ++pDest;
    }
  }
}
