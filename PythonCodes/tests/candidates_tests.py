import unittest
import PythonCodes.library.thresholding_base as tbase
import PythonCodes.library.thresholdsing_candidates as tcand
import numpy as np
from skimage import io

class TestThresholdedImage(unittest.TestCase):

    def setUp(self):
        self.image01 = io.imread('PythonCodes/tests/image_01.jpg')
        self.assertEquals(self.image01.shape, (600, 600)) # Check test_image_01.jpg is 600x600px

if __name__ == '__main__':
    unittest.main()