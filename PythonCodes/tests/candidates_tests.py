import unittest
import PythonCodes.library.thresholding_base as tbase
import PythonCodes.library.thresholding_candidates as tcand
import numpy as np
from skimage import io


class TestThresholdedImage(unittest.TestCase):

    def setUp(self):
        self.image01 = io.imread('PythonCodes/tests/image_01.jpg')
        # Check test_image_01.jpg is 600x600px
        self.assertEquals(self.image01.shape, (600, 600))

    def test_fom(self):
        fom_result = tcand.threshold_fom(self.image01, 2)
        self.assertEqual(fom_result, 129)

    def test_lra(self):
        pass


if __name__ == '__main__':
    unittest.main()
