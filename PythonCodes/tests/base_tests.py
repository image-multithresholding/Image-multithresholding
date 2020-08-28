import unittest
import PythonCodes.library.thresholding_base as tbase
import numpy as np
from skimage import io

class TestThresholdedImage(unittest.TestCase):

    def setUp(self):
        self.image01 = io.imread('PythonCodes/tests/image_01.jpg')
        self.assertEquals(self.image01.shape, (600, 600)) # Check test_image_01.jpg is 600x600px

    def test_arguments(self):
        with self.assertRaises(Exception):
            tbase.thresholded_image(self.image01, []) # Should have at least one threshold
        with self.assertRaises(Exception):
            tbase.thresholded_image(self.image01, 123) # Should receive a list of thresholds
        
    def test_number_of_classes(self):
        timage = tbase.thresholded_image(self.image01, [100])
        self.assertLessEqual(self.count_classes(timage), 2) # Check thresholded image has no more than 2 classes
        timage = tbase.thresholded_image(self.image01, [100, 200])
        self.assertLessEqual(self.count_classes(timage), 3) # Check thresholded image has no more than 3 classes

    def test_class_values(self):
        timage = tbase.thresholded_image(self.image01, [127]) # Has two classes, with means 64 (127 - 0 // 2) and 192 (255 - 127 // 2)
        classes = self.get_classes(timage)
        for value in [64, 192]:
            self.assertIn(value, classes)
        
    def count_classes(self, timage):
        gray_values = set()
        for value in timage.flat:
            gray_values.add(value)
        return len(gray_values)

    def get_classes(self, timage):
        gray_values = set()
        for value in timage.flat:
            gray_values.add(value)
        return gray_values
        
class TestPSNR(unittest.TestCase):

    def setUp(self):
        self.image01 = io.imread('PythonCodes/tests/image_01.jpg')
        self.thresholded01 = tbase.thresholded_image(self.image01, [127])
        self.PSNR = tbase.PSNR(self.image01, self.thresholded01)

    def test_return(self):
        self.assertEquals(type(self.PSNR), np.float64)
        self.assertGreaterEqual(self.PSNR, 0)

if __name__ == '__main__':
    unittest.main()