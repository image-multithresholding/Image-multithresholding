import unittest
import PythonCodes.library.thresholding_base as tbase
import PythonCodes.library.thresholding_candidates as tcand
import PythonCodes.library.hca as thca
import numpy as np
from skimage import io

class TestHCA(unittest.TestCase):

    def setUp(self):
        self.image01 = io.imread('Images/synthetic.jpg')
        self.assertEquals(self.image01.shape, (600, 600)) # Check test_image_01.jpg is 600x600px

    def test_valley(self):
        freq = tbase.image_histogram(self.image01)
        valleys = thca.valley_location(freq, 6)
        self.assertEquals(valleys[0], (2, 3))



if __name__ == '__main__':
    unittest.main()