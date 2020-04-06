######################################################################
# File to read an image in Python
######################################################################

from skimage import io # required module

image = io.imread('file name') # read the file

type(image) # type of object 

image.shape # size of image
