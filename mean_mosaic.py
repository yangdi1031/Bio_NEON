## try using the solution here: https://stackoverflow.com/questions/68760788/average-thousands-of-partially-overlapping-rasters-with-gdal 
## to mosaic multiple flightlines using an average of cells

from osgeo import gdal
import glob

def add_pixel_fn(filename: str) -> None:
    """inserts pixel-function into vrt file named 'filename'
    Args:
        filename (:obj:`string`): name of file, into which the function will be inserted
        resample_name (:obj:`string`): name of resampling method
    """

    header = """  <VRTRasterBand dataType="UInt16" band="1" subClass="VRTDerivedRasterBand">"""
    contents = """
    <PixelFunctionType>average</PixelFunctionType>
    <PixelFunctionLanguage>Python</PixelFunctionLanguage>
    <PixelFunctionCode><![CDATA[
import numpy as np

def average(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,raster_ysize, buf_radius, gt, **kwargs):
    div = np.zeros(in_ar[0].shape)
    for i in range(len(in_ar)):
        div += (in_ar[i] != 0)
    div[div == 0] = 1

    y = np.sum(in_ar, axis = 0, dtype = 'uint16')
    y = y / div
    
    np.clip(y,0,70000, out = out_ar)
]]>
    </PixelFunctionCode>"""

    lines = open(filename, 'r').readlines()
    lines[3] = header
    lines.insert(4, contents)
    open(filename, 'w').write("".join(lines))


out_name = "mean_mosaic"

tif_list = glob.glob(r"*BRDF.tif")
  
print(tif_list)

gdal.SetConfigOption('GDAL_VRT_ENABLE_PYTHON', 'YES')

gdal.BuildVRT(f'{out_name}.vrt', tif_list)

add_pixel_fn(f'{out_name}.vrt')

ds = gdal.Open(f'{out_name}.vrt')

translateoptions = gdal.TranslateOptions(gdal.ParseCommandLine("-co BIGTIFF=YES -co COMPRESS=DEFLATE -co TILED=YES"))

ds = gdal.Translate(f'{out_name}.tif', ds, options=translateoptions)

